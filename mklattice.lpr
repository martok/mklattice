program mklattice;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  getopts,
  IniFiles, Math, Classes, SysUtils,
  uLattice, uAtomList, uStrInput, uAtomTypes, uLinAlg, uSScanf, uGetOpt
  { you can add units after this };

var
  AtomList:TAtomList;
  Lattices: TLatticeDescriptorList;
  OverallPlaces: int64;

const
  PresetFileName = 'presets.ini';
var
  LatticeType: string = '';           // BCC B2 DO3
  Elements: array of Byte;            // Element numbers by lattice place/sublattice
  Vacancies: array of Double;         // Sublattice vacancy densities (probability of a lattice place being empty)
  AntiSite: array of Double;          // Sublattice antisite densities (probability of a lattice place being occupied by an atom of the next (mod count) element)
  LatConst: double = 0.0;             // (fundamental) lattice constant a (not the effective lattice constant a' of complex crystals)
  LatticeCells: array[0..2] of integer = (1,1,1);          // number of cells in every direction
  Kink: double = 0.0;                 //
  TopPlaneCut: double = 0.0;          //
  Rotation: TMatrix3x3f;

{
  Carve out a kink in the top layer, keeping an area that is 0.5*SimBox x $Kink*SimBox x LatConst
}
procedure Carve_Kink(const LatConst: Double);
var
  bl, tr, trk: TSize3;
begin
  bl[0]:= 0;
  bl[1]:= 0;
  bl[2]:= AtomList.Cell[2] - LatConst * 0.7;

  tr[0]:= AtomList.Cell[0];
  tr[1]:= AtomList.Cell[1];
  tr[2]:= AtomList.Cell[2];

  trk[0]:= AtomList.Cell[0] * Kink;
  trk[1]:= AtomList.Cell[1] * Kink;
  trk[2]:= AtomList.Cell[2];

  WriteLn(ErrOutput, 'Kink boxes: (',bl[0],',',bl[1],',',bl[2],')->(',tr[0],',',tr[1],',',tr[2],')-(',trk[0],',',trk[1],',',trk[2],')');
  AtomList.RemoveAtoms(bl, tr, trk);
end;

procedure Carve_Top;
begin
  if TopPlaneCut <= 0 then
    exit;

  AtomList.RemoveAtoms(Size3(0,0,AtomList.Cell[2] - TopPlaneCut), AtomList.Cell);
  AtomList.Cell:= Size3(AtomList.Cell[0],AtomList.Cell[1],AtomList.Cell[2] - TopPlaneCut);
end;

procedure LoadLatticeTypes;
  function BuildDO3(descr: TLatticeDescriptor): TLatticeDescriptor;
  begin
    Result:= descr;
    descr.
      SetMatrix(IDENTITY_MATRIX * 2).
      SubLattice(0, [
        vecCreate(0.0,0.0,0.0), vecCreate(0.5,0.0,0.0), vecCreate(0.0,0.5,0.0),
        vecCreate(0.0,0.0,0.5), vecCreate(0.5,0.0,0.5), vecCreate(0.0,0.5,0.5),
        vecCreate(0.5,0.5,0.5)
      ]).
      SubLattice(0, [
        vecCreate(0.25,0.25,0.25), vecCreate(0.75,0.75,0.25),
        vecCreate(0.75,0.25,0.75), vecCreate(0.25,0.75,0.75)
      ]).
      SubLattice(1, [
        vecCreate(0.75,0.25,0.25), vecCreate(0.25,0.75,0.25),
        vecCreate(0.25,0.25,0.75), vecCreate(0.75,0.75,0.75)
      ]);
  end;

begin
  Lattices.Add(TLatticeDescriptor.Create('SC').
    SubLattice(0, [vecCreate(0,0,0)])
  );
  Lattices.Add(TLatticeDescriptor.Create('BCC').
    SubLattice(0, [vecCreate(0,0,0), vecCreate(0.5,0.5,0.5)])
  );
  Lattices.Add(TLatticeDescriptor.Create('FCC').
    SubLattice(0, [vecCreate(0,0,0), vecCreate(0.5,0.5,0), vecCreate(0.5,0,0.5), vecCreate(0,0.5,0.5)])
  );
  Lattices.Add(TLatticeDescriptor.Create('B2').
    SubLattice(0, [vecCreate(0,0,0)]).
    SubLattice(1, [vecCreate(0.5,0.5,0.5)])
  );
  Lattices.Add(BuildDO3(TLatticeDescriptor.Create('DO3')));
end;

procedure InitLatticeParameters;
var
  ld: TLatticeDescriptor;
  numEl: integer;
  ei: byte;
begin
  numEl:= -1;
  for ld in Lattices do
    if ld.Name = LatticeType then begin
      for ei in ld.Elements do
        if numEl < ei then
          numEl:= ei;
      break;
    end;
  if numEl < 0 then begin
    WriteLn(ErrOutput, 'Invalid lattice argument: ',OptArg);
    Halt(1);
  end;

  inc(numEl);
  SetLength(Elements, numEl);
  SetLength(Vacancies, numEl);
  SetLength(AntiSite, numEl);
end;

function SplitIndexedArg(const Arg: String; out Idx: integer; out param: string): boolean;
var
  i: integer;
begin
  i:= Pos(':',Arg);
  Result:= i > 0;
  if Result then begin
    Idx:= StrToInt(Copy(Arg, 1, i-1));
    param:= Copy(Arg, i+1, MaxInt);
  end else
    param:= Arg;
end;

function VacancyFilter(const AtomIndex: Integer; var AtomType: byte; const x,y,z: Single) : boolean;
begin
  Result:= (Random > Vacancies[AtomIndex]);
  if Result then begin
    if (Random < AntiSite[AtomIndex]) then begin
      AtomType:= Elements[(AtomIndex + 1) mod Length(Elements)];
    end;
  end;
end;

procedure Lattice_Execute;
var
  desc: TLatticeDescriptor;
begin
  for desc in Lattices do begin
    if desc.Name = LatticeType then begin
      TLatticeGenerator.Create(desc, LatConst).
        SetRotation(Rotation).
        InitLattice(LatticeCells[0], LatticeCells[1], LatticeCells[2]).
        SetElements(Elements).
        Filter(@VacancyFilter).
        ExportAtoms(AtomList, OverallPlaces).
        Free;
      exit;
    end;
  end;

  WriteLn(ErrOutput, 'Invalid lattice argument: ',OptArg);
  Halt(1);
end;

const
  OptionsLong: array[1..13] of TOption = (
   (Name: 'file'; Has_Arg: Required_Argument; Flag: nil; Value: 'o'),
   (Name: 'preset'; Has_Arg: Required_Argument; Flag: nil; Value: #0),
   (Name: 'lattice'; Has_Arg: Required_Argument; Flag: nil; Value: 'l'),
   (Name: 'latconst'; Has_Arg: Required_Argument; Flag: nil; Value: 'a'),
   (Name: 'cells'; Has_Arg: Required_Argument; Flag: nil; Value: 'c'),
   (Name: 'matrix'; Has_Arg: Required_Argument; Flag: nil; Value: 'm'),
   (Name: 'element'; Has_Arg: Required_Argument; Flag: nil; Value: 'e'),
   (Name: 'kink'; Has_Arg: Required_Argument; Flag: nil; Value: 'k'),
   (Name: 'top'; Has_Arg: Required_Argument; Flag: nil; Value: 't'),
   (Name: 'vac'; Has_Arg: Required_Argument; Flag: nil; Value: 'v'),
   (Name: 'anti'; Has_Arg: Required_Argument; Flag: nil; Value: 's'),
   (Name: 'help'; Has_Arg: No_Argument; Flag: nil; Value: 'h'),
   (Name: ''; Has_Arg: 0; Flag: nil; Value: #0)
  );
  OptionShort = '?hl:o:c:k:s:t:v:a:e:m:';

procedure ProcessOption(const opt: string; const OptArg: string);
  procedure ExecutePreset(const PresetName: string);
  var
    mi: TMemIniFile;
    cmd: TStringList;
    i: integer;
  begin
    if not FileExists(ExtractFilePath(ParamStr(0)) + PresetFileName) then begin
      WriteLn(ErrOutput, 'Preset file ',PresetFileName,' not found!');
      Halt(1);
    end;
    mi:= TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + PresetFileName);
    try
      cmd:= TStringList.Create;
      try
        mi.CaseSensitive:= false;
        if not mi.SectionExists(PresetName) then begin
          WriteLn(ErrOutput, 'Preset ',PresetName,' not found!');
          Halt(1);
        end;
        mi.ReadSectionValues(PresetName, cmd);
        for i:= 0 to cmd.Count-1 do
          if cmd.Names[i] > '' then
            ProcessOption(cmd.Names[i], cmd.ValueFromIndex[i]);
      finally
        FreeAndNil(cmd);
      end;
    finally
      FreeAndNil(mi);
    end;
  end;

var
  i: integer;
  p: string;
  x,y,z,a,m10,m11,m12,m20,m21,m22: Double;
begin
  case opt of
    'o': begin
      CloseFile(Output);
      AssignFile(Output, OptArg);
      Rewrite(Output);
    end;
    'preset': begin
      ExecutePreset(OptArg);
    end;
    'l': begin
      LatticeType:= UpperCase(OptArg);
      InitLatticeParameters;
    end;
    'a': begin
      LatConst:= StrToFloat(OptArg);
    end;
    'c': begin
      if utlSScanf(OptArg,'%d:%d:%d',[@LatticeCells[0],@LatticeCells[1],@LatticeCells[2]])<>0 then begin
        for i:= 0 to high(LatticeCells) do
          LatticeCells[i]:= StrToInt(OptArg);
      end;
    end;
    'm': begin
      if utlSScanf(OptArg,'identity',[])=0 then begin
        Rotation:= IDENTITY_MATRIX;
      end else
      if utlSScanf(OptArg,'rot:%f,%f,%f,%f',[@x,@y,@z,@a],NeutralFormatSettings)=0 then begin
        Rotation*= matRotation(vecCreate(x,y,z), degtorad(a));
      end else
      if utlSScanf(OptArg,'rotglob:%f,%f,%f,%f',[@x,@y,@z,@a],NeutralFormatSettings)=0 then begin
        Rotation*= matRotation(matInvert(Rotation)*vecCreate(x,y,z), degtorad(a));
      end else
      if utlSScanf(OptArg,'rotglob:%f,%f,%f',[@x,@y,@z],NeutralFormatSettings)=0 then begin
        Rotation*= matRotation(degtorad(x),degtorad(y),degtorad(z));
      end else
      if utlSScanf(OptArg,'bunge:%f,%f,%f',[@x,@y,@z],NeutralFormatSettings)=0 then begin
        Rotation*= matRotationBunge(degtorad(x),degtorad(y),degtorad(z));
      end else
      if utlSScanf(OptArg,'roe:%f,%f,%f',[@x,@y,@z],NeutralFormatSettings)=0 then begin
        Rotation*= matRotationRoe(degtorad(x),degtorad(y),degtorad(z));
      end else
      if utlSScanf(OptArg,'kocks:%f,%f,%f',[@x,@y,@z],NeutralFormatSettings)=0 then begin
        Rotation*= matRotationKocks(degtorad(x),degtorad(y),degtorad(z));
      end else
      if utlSScanf(OptArg,'load:%f,%f,%f,%f,%f,%f,%f,%f,%f',[@x,@y,@z,@m10,@m11,@m12,@m20,@m21,@m22],NeutralFormatSettings)=0 then begin
        Rotation*= matCreate(vecCreate(x,y,z),vecCreate(m10,m11,m12),vecCreate(m20,m21,m22));
      end else
      if utlSScanf(OptArg,'base:%f,%f,%f;%f,%f,%f;%f,%f,%f',[@x,@y,@z,@m10,@m11,@m12,@m20,@m21,@m22],NeutralFormatSettings)=0 then begin
        Rotation*= matTranspose(matCreate(vecNormalize(vecCreate(x,y,z)),vecNormalize(vecCreate(m10,m11,m12)),vecNormalize(vecCreate(m20,m21,m22))));
      end;
      WriteLn(ErrOutput, 'Using Matrix ',String(Rotation));
    end;
    'e': begin
      if SplitIndexedArg(OptArg, i, p) then begin
        if (i>=0) and (i < Length(Elements)) then
          Elements[i]:= StrToInt(p)
        else begin
          WriteLn(ErrOutput, 'Sublattice element index ',i,' out of bounds');
          Halt(1);
        end;
      end
      else
        for i:= 0 to high(Elements) do
          Elements[i]:= StrToInt(p);
    end;
    'k': begin
      Kink:= StrToFloat(OptArg);
    end;
    't': begin
      TopPlaneCut:= StrToFloat(OptArg);
    end;
    'v': begin
      if SplitIndexedArg(OptArg, i, p) then  begin
        if (i>=0) and (i < Length(Vacancies)) then
          Vacancies[i]:= StrToFloat(p)
        else begin
          WriteLn(ErrOutput, 'Sublattice vacancy index ',i,' out of bounds');
          Halt(1);
        end;
      end
      else
        for i:= 0 to high(Elements) do
          Vacancies[i]:= StrToFloat(p);
    end;
    's': begin
      if SplitIndexedArg(OptArg, i, p) then  begin
        if (i>=0) and (i < Length(AntiSite)) then
          AntiSite[i]:= StrToFloat(p)
        else begin
          WriteLn(ErrOutput, 'Sublattice antisite index ',i,' out of bounds');
          Halt(1);
        end;
      end
      else
        for i:= 0 to high(Elements) do
          AntiSite[i]:= StrToFloat(p);
    end;
    'h',
    '?': begin
      WriteLn('mklattice -l (B2|DO3) [-c cells] [-o file]');
      Halt(0);
    end;
  else
    WriteLn(ErrOutput, 'Unknown option: ', opt);
  end;
end;

begin
  LoadNeutralFormatSettings;
  Randomize;
  LoadAtomTypes;
  Lattices:= TLatticeDescriptorList.Create(true);
  try
    LoadLatticeTypes;

    OverallPlaces:= 0;
    AtomList:= TAtomList.Create;
    try
      Rotation:= IDENTITY_MATRIX;

      HandleAllOptions(OptionShort, @OptionsLong, @ProcessOption);

      Lattice_Execute;

      Carve_Top;
      if Kink > 0.0 then
        Carve_Kink(LatConst);

      AtomList.ExportAtoms('Generated by `'+cmdline+'` at ' + DateTimeToStr(Now), AtomTypes, OverallPlaces);
    finally
      FreeAndNil(AtomList);
    end;
  finally
    FreeAndNil(Lattices);
  end;
end.

