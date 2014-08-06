program mklattice;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  getopts,
  Classes, SysUtils, uLattice, uAtomList, uStrInput, uAtomTypes
  { you can add units after this };

var
  AtomList:TAtomList;
  OverallPlaces: int64;

const
  LatConst_FeAl = 2.85133; // range 2.83..3.03, summary in Shu et al, J.Mater.Sci.Technol. Vol 17 No 6(2001), 601
  LatConst_W = 3.1;
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
    'BCC': begin
      numEl:= 1;
    end;
    'B2': begin
      numEl:= 2;
    end;
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
    Place(Vacancies[0]).
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
      case UpperCase(OptArg) of
        'FEAL-B2': begin
          ProcessOption('l','B2');
          ProcessOption('a',FloatToStr(LatConst_FeAl));
          ProcessOption('e','0:26');
          ProcessOption('e','1:13');
        end;
        'FEAL-DO3': begin
          ProcessOption('l','DO3');
          ProcessOption('a',FloatToStr(LatConst_FeAl));
          ProcessOption('e','0:26');
          ProcessOption('e','1:13');
        end;
      end;
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
      if SplitIndexedArg(OptArg, i, p) then
        Elements[i]:= StrToInt(p)
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
      if SplitIndexedArg(OptArg, i, p) then
        Vacancies[i]:= StrToFloat(p)
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
      'BCC': Lattice_BCC;
      'B2': Lattice_B2;
      'DO3': Lattice_DO3;
      else begin
        WriteLn(StdErr, 'Invalid lattice argument: ',OptArg);
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

