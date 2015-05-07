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
    Dim: TGrid3;
    FilterFunction: TAtomFilter;
    FilterIndex: integer;
    constructor Create(aAtomType: Byte; aLatConst: Extended);
    // Set Position of first Atom
    function SetOffset(aX, aY, aZ: Extended): TSubLatticeSC;
    // Create a field so that realworld coordinates mX, mY, mZ form the upper corner
    function InitDimensions(mX, mY, mZ: Extended): TSubLatticeSC;
    // Create a field with a specific number of lattice nodes
    function InitLattice(mX, mY, mZ: Integer): TSubLatticeSC;

    function Filter(aFilterFunc: TAtomFilter; aIndex: integer): TSubLatticeSC;
    function ExportAtoms(const List: TAtomList; var PlaceCounter: int64): TSubLatticeSC;
  end;

implementation

uses
  Math;

function DefaultFilter(const {%H-}AtomIndex: Integer; var {%H-}AtomType: byte; const {%H-}x,{%H-}y,{%H-}z: Single) : boolean;
begin
  Result:= true;
end;

{ TSubLatticeSC }

constructor TSubLatticeSC.Create(aAtomType: Byte; aLatConst: Extended);
begin
  inherited Create;
  AtomType:= aAtomType;
  LatConst:= aLatConst;
  Filter(@DefaultFilter, 0);
  SetOffset(0,0,0);
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
  Dim[0]:= ceil(mX - Offset[0]);
  Dim[1]:= ceil(mY - Offset[1]);
  Dim[2]:= ceil(mZ - Offset[2]);
  Cell[0]:= mX * LatConst;
  Cell[1]:= mY * LatConst;
  Cell[2]:= mZ * LatConst;
end;

function TSubLatticeSC.Filter(aFilterFunc: TAtomFilter; aIndex: integer): TSubLatticeSC;
begin
  Result:= Self;
  FilterFunction:= aFilterFunc;
  FilterIndex:= aIndex;
end;

function TSubLatticeSC.ExportAtoms(const List: TAtomList; var PlaceCounter: int64): TSubLatticeSC;
var
  i,j,k: integer;
  x,y,z: Single;
  ia: Byte;
begin
  Result:= Self;
  for i:= 0 to Dim[0]-1 do
    for j:= 0 to Dim[1]-1 do
      for k:= 0 to Dim[2]-1 do begin
        inc(PlaceCounter);
        ia:= AtomType;
        x:= (i + Offset[0]) * LatConst;
        y:= (j + Offset[1]) * LatConst;
        z:= (k + Offset[2]) * LatConst;
        if FilterFunction(FilterIndex,ia,x,y,z) then begin
          List.Add(TAtomDef.Create(ia, x,y,z));
        end;
      end;
  List.MergeCell(Cell);
end;

end.

