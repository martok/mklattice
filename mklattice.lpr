program mklattice;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  getopts,
  Classes, SysUtils, uLattice, uAtomList, uStrInput
  { you can add units after this };


const
  AtomTypes: array[0..2] of TAtomType = (
  //  (ID: 1; Weight: 55.845; Name: 'Fe'),
    (ID: 13; Weight: 26.981538; Name: 'Al'),
    (ID: 26; Weight: 55.845; Name: 'Fe'),
    (ID: 74; Weight: 183.84; Name: 'W')
  );

var
  AtomList:TAtomList;
  OverallPlaces: int64;

const
  LatConst_FeAl = 2.85133; // range 2.83..3.03, summary in Shu et al, J.Mater.Sci.Technol. Vol 17 No 6(2001), 601
  LatConst_W = 3.1;
var
  LatticeCells: integer = 1;
  LatticeType: string = '';
  Kink: double = 0.0;
  TopPlaneCut: double = 0.0;
  VacConcentration: double = 0.0;


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

procedure Lattice_W_bcc;
begin
  TSubLatticeSC.Create(74, LatConst_W).
    InitLattice(LatticeCells, LatticeCells, LatticeCells).
    Place(0.0).
    ExportAtoms(AtomList, OverallPlaces).
    Free;

  TSubLatticeSC.Create(74, LatConst_W).
    SetOffset(0.5, 0.5, 0.5).
    InitLattice(LatticeCells, LatticeCells, LatticeCells).
    Place(0.0).
    ExportAtoms(AtomList, OverallPlaces).
    Free;
end;

procedure Lattice_FeAl_B2;
begin
  TSubLatticeSC.Create(26, LatConst_FeAl).
    InitLattice(LatticeCells, LatticeCells, LatticeCells).
    Place(VacConcentration).
    ExportAtoms(AtomList, OverallPlaces).
    Free;

  TSubLatticeSC.Create(13, LatConst_FeAl).
    SetOffset(0.5, 0.5, 0.5).
    InitLattice(LatticeCells, LatticeCells, LatticeCells).
    Place(VacConcentration).
    ExportAtoms(AtomList, OverallPlaces).
    Free;

  Carve_Top;
  if Kink > 0.0 then
    Carve_Kink(LatConst_FeAl);
end;

procedure Lattice_FeAl_DO3;
const
  Alternate: array[boolean] of byte = (13, 26);
var
  x,y,z: integer;
begin
  TSubLatticeSC.Create(26, LatConst_FeAl).
    InitLattice(LatticeCells*2, LatticeCells*2, LatticeCells*2).
    Place(VacConcentration).
    ExportAtoms(AtomList, OverallPlaces).
    Free;

  for x:= 0 to 1 do
    for y:= 0 to 1 do
      for z:= 0 to 1 do begin
        TSubLatticeSC.Create(Alternate[(x+y+z) mod 2 = 0], LatConst_FeAl * 2).
          SetOffset(0.25 + 0.5*x, 0.25 + 0.5*y, 0.25 + 0.5*z).
          InitLattice(LatticeCells, LatticeCells, LatticeCells).
          Place(VacConcentration).
          ExportAtoms(AtomList, OverallPlaces).
          Free;
      end;

  Carve_Top;
  if Kink > 0.0 then
    Carve_Kink(LatConst_FeAl);
end;

const
  OptionsLong: array[0..7] of TOption = (
   (Name: 'lattice'; Has_Arg: Required_Argument; Flag: nil; Value: 'l'),
   (Name: 'file'; Has_Arg: Required_Argument; Flag: nil; Value: 'o'),
   (Name: 'cells'; Has_Arg: Required_Argument; Flag: nil; Value: 'c'),
   (Name: 'kink'; Has_Arg: Required_Argument; Flag: nil; Value: 'k'),
   (Name: 'top'; Has_Arg: Required_Argument; Flag: nil; Value: 't'),
   (Name: 'svac'; Has_Arg: Required_Argument; Flag: nil; Value: 'v'),
   (Name: 'help'; Has_Arg: No_Argument; Flag: nil; Value: 'h'),
   (Name: ''; Has_Arg: 0; Flag: nil; Value: #0)
  );
  OptionShort = '?hl:o:c:k:t:v:';

var
  optIndex: integer;


begin
  LoadNeutralFormatSettings;
  Randomize;
  OverallPlaces:= 0;
  AtomList:= TAtomList.Create;
  try
    optIndex:= 0;
    while True do begin
      case GetLongOpts(OptionShort, @OptionsLong, optIndex) of
        'l': LatticeType:= OptArg;
        'o': begin
          CloseFile(Output);
          AssignFile(Output, OptArg);
          Rewrite(Output);
        end;
        'c': begin
          LatticeCells:= StrToInt(OptArg);
        end;
        't': begin
          TopPlaneCut:= StrToFloat(OptArg);
        end;
        'k': begin
          Kink:= StrToFloat(OptArg);
        end;
        'v': begin
          VacConcentration:= StrToFloat(OptArg);
        end;
        'h',
        '?': begin
          WriteLn('mklattice -l (B2|DO3) [-c cells] [-o file]');
          Halt(0);
        end;
        EndOfOptions: break;
      end;
    end;

    case UpperCase(LatticeType) of
      'W_BCC': Lattice_W_bcc;
      'B2': Lattice_FeAl_B2;
      'DO3': Lattice_FeAl_DO3;
      else begin
        WriteLn(StdErr, 'Invalid lattice argument: ',OptArg);
        Halt(1);
      end;
    end;

    AtomList.ExportAtoms('Generated by `'+cmdline+'` at ' + DateTimeToStr(Now), AtomTypes, OverallPlaces);
  finally
    FreeAndNil(AtomList);
  end;
end.

