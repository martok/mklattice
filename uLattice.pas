unit uLattice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAtomList;

type
  TLatticeArray = packed array of packed array of packed array of boolean;
  TSubLatticeSC = class
    LatConst: Extended;
    AtomType: Byte;
    Offset: TSize3;
    Cell: TSize3;
    Places: TLatticeArray;
    constructor Create(aAtomType: Byte; aLatConst: Extended);
    // Set Position of first Atom
    function SetOffset(aX, aY, aZ: Extended): TSubLatticeSC;
    // Create a field so that realworld coordinates mX, mY, mZ form the upper corner
    function InitDimensions(mX, mY, mZ: Extended): TSubLatticeSC;
    // Create a field with a specific number of lattice nodes
    function InitLattice(mX, mY, mZ: Integer): TSubLatticeSC;

    function Place(const VacancyDensity: Single): TSubLatticeSC;
    function ExportAtoms(const List:TAtomList): TSubLatticeSC;
  end;

implementation

uses
  Math;

{ TSubLatticeSC }

constructor TSubLatticeSC.Create(aAtomType: Byte; aLatConst: Extended);
begin
  inherited Create;
  AtomType:= aAtomType;
  LatConst:= aLatConst;
  SetOffset(0,0,0);
  SetLength(Places,0,0,0);
end;

function TSubLatticeSC.SetOffset(aX, aY, aZ: Extended): TSubLatticeSC;
begin
  Result:= Self;
  Offset[0]:= aX;
  Offset[1]:= aY;
  Offset[2]:= aZ;
end;

function TSubLatticeSC.InitDimensions(mX, mY, mZ: Extended): TSubLatticeSC;
var
  ax,ay,az: Extended;
begin
  Result:= Self;
  ax:= mX - Offset[0];
  ay:= mY - Offset[1];
  az:= mZ - Offset[2];
  InitLattice(ceil(ax / LatConst), ceil(ay / LatConst), ceil(az / LatConst));
  Cell[0]:= mX;
  Cell[1]:= mY;
  Cell[2]:= mZ;
end;

function TSubLatticeSC.InitLattice(mX, mY, mZ: Integer): TSubLatticeSC;
begin
  Result:= Self;
  SetLength(Places, ceil(mX - Offset[0]), ceil(mY - Offset[1]), ceil(mZ - Offset[2]));
  Cell[0]:= mX * LatConst;
  Cell[1]:= mY * LatConst;
  Cell[2]:= mZ * LatConst;
end;

function TSubLatticeSC.Place(const VacancyDensity: Single): TSubLatticeSC;
var
  i,j,k: integer;
begin
  Result:= Self;
  for i:= 0 to high(Places) do
    for j:= 0 to high(Places[i]) do
      for k:= 0 to high(Places[i,j]) do
        Places[i,j,k]:= Random > VacancyDensity;
end;

function TSubLatticeSC.ExportAtoms(const List: TAtomList): TSubLatticeSC;
var
  i,j,k: integer;
begin
  Result:= Self;
  for i:= 0 to high(Places) do
    for j:= 0 to high(Places[i]) do
      for k:= 0 to high(Places[i,j]) do
        if Places[i,j,k] then begin
          List.Add(TAtomDef.Create(AtomType, (i + Offset[0]) * LatConst, (j + Offset[1]) * LatConst, (k + Offset[2]) * LatConst));
        end;
  List.MergeCell(Cell);
end;

end.

