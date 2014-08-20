program mklattice;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  getopts,
  IniFiles,
  Classes, SysUtils, uLattice, uAtomList, uStrInput, uAtomTypes
  { you can add units after this };

var
  AtomList:TAtomList;
  OverallPlaces: int64;

const
  PresetFileName = 'presets.ini';
var
  LatticeType: string = '';           // BCC B2 DO3
  Elements: array of Byte;            // Element numbers by lattice place/sublattice
  Vacancies: array of Double;         // Sublattice vacancy densities (probability of a lattice place being empty)
  LatConst: double = 0.0;             // (fundamental) lattice constant a (not the effective lattice constant a' of complex crystals)
  LatticeCells: integer = 1;          // number of cells in every direction
  Kink: double = 0.0;                 //
  TopPlaneCut: double = 0.0;          //

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

procedure InitLatticeParameters;
var
  numEl: integer;
begin
  case LatticeType of
    'SC',
    'FCC',
    'BCC': begin
      numEl:= 1;
    end;
    'B2',
    'DO3': begin
      numEl:= 2;
    end;
  end;
  SetLength(Elements, numEl);
  SetLength(Vacancies, numEl);
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

procedure Lattice_SC;
begin
  TSubLatticeSC.Create(Elements[0], LatConst).
    InitLattice(LatticeCells, LatticeCells, LatticeCells).
    Place(0.0).
    ExportAtoms(AtomList, OverallPlaces).
    Free;
end;

procedure Lattice_BCC;
begin
  TSubLatticeSC.Create(Elements[0], LatConst).
    InitLattice(LatticeCells, LatticeCells, LatticeCells).
    Place(0.0).
    ExportAtoms(AtomList, OverallPlaces).
    Free;

  TSubLatticeSC.Create(Elements[0], LatConst).
    SetOffset(0.5, 0.5, 0.5).
    InitLattice(LatticeCells, LatticeCells, LatticeCells).
    Place(0.0).
    ExportAtoms(AtomList, OverallPlaces).
    Free;
end;

procedure Lattice_FCC;
const
  sides: array[0..3] of TSize3 = (
    (0.0, 0.0, 0.0),
    (0.5, 0.0, 0.5),
    (0.0, 0.5, 0.5),
    (0.5, 0.5, 0.0)
  );
var
  i: integer;
begin
  for i:= 0 to high(sides) do
    TSubLatticeSC.Create(Elements[0], LatConst).
      SetOffset(sides[i][0],sides[i][1],sides[i][2]).
      InitLattice(LatticeCells, LatticeCells, LatticeCells).
      Place(0.0).
      ExportAtoms(AtomList, OverallPlaces).
      Free;
end;

procedure Lattice_B2;
begin
  TSubLatticeSC.Create(Elements[0], LatConst).
    InitLattice(LatticeCells, LatticeCells, LatticeCells).
    Place(Vacancies[0]).
    ExportAtoms(AtomList, OverallPlaces).
    Free;

  TSubLatticeSC.Create(Elements[1], LatConst).
    SetOffset(0.5, 0.5, 0.5).
    InitLattice(LatticeCells, LatticeCells, LatticeCells).
    Place(Vacancies[1]).
    ExportAtoms(AtomList, OverallPlaces).
    Free;
end;

procedure Lattice_DO3;
const
  Alternate: array[boolean] of byte = (1,0);
var
  x,y,z,e: integer;
begin
  TSubLatticeSC.Create(Elements[0], LatConst).
    InitLattice(LatticeCells*2, LatticeCells*2, LatticeCells*2).
    Place(Vacancies[0]).
    ExportAtoms(AtomList, OverallPlaces).
    Free;

  for x:= 0 to 1 do
    for y:= 0 to 1 do
      for z:= 0 to 1 do begin
        e:= Alternate[(x+y+z) mod 2 = 0];
        TSubLatticeSC.Create(Elements[e], LatConst * 2).
          SetOffset(0.25 + 0.5*x, 0.25 + 0.5*y, 0.25 + 0.5*z).
          InitLattice(LatticeCells, LatticeCells, LatticeCells).
          Place(Vacancies[e]).
          ExportAtoms(AtomList, OverallPlaces).
          Free;
      end;
end;

const
  OptionsLong: array[1..11] of TOption = (
   (Name: 'file'; Has_Arg: Required_Argument; Flag: nil; Value: 'o'),
   (Name: 'preset'; Has_Arg: Required_Argument; Flag: nil; Value: #0),
   (Name: 'lattice'; Has_Arg: Required_Argument; Flag: nil; Value: 'l'),
   (Name: 'latconst'; Has_Arg: Required_Argument; Flag: nil; Value: 'a'),
   (Name: 'cells'; Has_Arg: Required_Argument; Flag: nil; Value: 'c'),
   (Name: 'element'; Has_Arg: Required_Argument; Flag: nil; Value: 'e'),
   (Name: 'kink'; Has_Arg: Required_Argument; Flag: nil; Value: 'k'),
   (Name: 'top'; Has_Arg: Required_Argument; Flag: nil; Value: 't'),
   (Name: 'vac'; Has_Arg: Required_Argument; Flag: nil; Value: 'v'),
   (Name: 'help'; Has_Arg: No_Argument; Flag: nil; Value: 'h'),
   (Name: ''; Has_Arg: 0; Flag: nil; Value: #0)
  );
  OptionShort = '?hl:o:c:k:t:v:a:e:';

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
      LatticeCells:= StrToInt(OptArg);
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
    'h',
    '?': begin
      WriteLn('mklattice -l (B2|DO3) [-c cells] [-o file]');
      Halt(0);
    end;
  else
    WriteLn(ErrOutput, 'Unknown option: ', opt);
  end;
end;

var
  optIndex: integer;
  opt: string;
begin
  LoadNeutralFormatSettings;
  Randomize;
  LoadAtomTypes;
  OverallPlaces:= 0;
  AtomList:= TAtomList.Create;
  try
    optIndex:= 0;
    while True do begin
      opt:= GetLongOpts(OptionShort, @OptionsLong[1], optIndex);
      if opt = EndOfOptions then
        break;
      if opt = #0 then
        opt:= OptionsLong[optIndex].Name;
      ProcessOption(opt, optArg);
    end;

    case LatticeType of
      'SC': Lattice_SC;
      'BCC': Lattice_BCC;
      'FCC': Lattice_FCC;
      'B2': Lattice_B2;
      'DO3': Lattice_DO3;
      else begin
        WriteLn(ErrOutput, 'Invalid lattice argument: ',OptArg);
        Halt(1);
      end;
    end;

    Carve_Top;
    if Kink > 0.0 then
      Carve_Kink(LatConst);

    AtomList.ExportAtoms('Generated by `'+cmdline+'` at ' + DateTimeToStr(Now), AtomTypes, OverallPlaces);
  finally
    FreeAndNil(AtomList);
  end;
end.

