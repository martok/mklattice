unit uLattice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAtomList, uLinAlg;

type
  TLatticeArray = packed array of packed array of packed array of boolean;
  TSubLatticeSC = class
    LatConst: Extended;
    AtomType: Byte;
    Offset: TSize3;
    Cell: TSize3;
    Dim: TGrid3;
    Rotation: TMatrix3x3f;
    FilterFunction: TAtomFilter;
    FilterIndex: integer;
    constructor Create(aAtomType: Byte; aLatConst: Extended);
    // Set Position of first Atom
    function SetOffset(aX, aY, aZ: Extended): TSubLatticeSC;
    // Create a field so that realworld coordinates mX, mY, mZ form the upper corner
    function InitDimensions(mX, mY, mZ: Extended): TSubLatticeSC;
    // Create a field with a specific number of lattice nodes
    function InitLattice(mX, mY, mZ: Integer): TSubLatticeSC;
    // Set rotation matrix
    function SetRotation(aMatrix: TMatrix3x3f): TSubLatticeSC;

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
  Rotation:= IDENTITY_MATRIX;
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

function TSubLatticeSC.SetRotation(aMatrix: TMatrix3x3f): TSubLatticeSC;
begin
  Result:= Self;
  Rotation:= aMatrix;
end;

function TSubLatticeSC.Filter(aFilterFunc: TAtomFilter; aIndex: integer): TSubLatticeSC;
begin
  Result:= Self;
  FilterFunction:= aFilterFunc;
  FilterIndex:= aIndex;
end;

function TSubLatticeSC.ExportAtoms(const List: TAtomList; var PlaceCounter: int64): TSubLatticeSC;
const
  pts: array[0..7] of array[0..2] of integer = (
         (0,0,0),
         (1,0,0),
         (0,0,1),
         (1,0,1),
         (0,1,0),
         (1,1,0),
         (0,1,1),
         (1,1,1)
       );

var
  xl,xh,yl,yh,zl,zh: integer;
  i,j,k: integer;
  m: TMatrix3x3f;
  v: TVector3f;
  ia: Byte;

  procedure minmax(v: single; var h,l: integer);
  begin
    if v > h then h:= ceil(v);
    if v < l then l:= floor(v);
  end;

  function CellRange(v: single; max: single): Boolean;
  begin
    Result:= IsZero(v) or ((v>=0) and (v < max));
  end;

begin
  Result:= Self;
  // Find bounding box in rotated csys
  m:= matInvert(Rotation);
  v:=  m * vecCreate(0,0,0);
  xl:= trunc(v[0]); xh:= trunc(v[0]);
  yl:= trunc(v[1]); yh:= trunc(v[1]);
  zl:= trunc(v[2]); zh:= trunc(v[2]);
  for i:= 1 to high(pts) do begin
    v:=  m * vecCreate(Dim[0] * pts[i][0], Dim[1] * pts[i][1], Dim[2] * pts[i][2]);
    minmax(v[0], xh, xl);
    minmax(v[1], yh, yl);
    minmax(v[2], zh, zl);
  end;

  for i:= xl to xh-1 do
    for j:= yl to yh-1 do
      for k:= zl to zh-1 do begin
        ia:= AtomType;
        v:= vecCreate(
              (i + Offset[0]) * LatConst,
              (j + Offset[1]) * LatConst,
              (k + Offset[2]) * LatConst);
        v:= Rotation * v;
        if CellRange(v[0], Cell[0]) and
           CellRange(v[1], Cell[1]) and
           CellRange(v[2], Cell[2]) then begin
          inc(PlaceCounter);
          if FilterFunction(FilterIndex,ia,v[0],v[1],v[2]) then
            List.Add(TAtomDef.Create(ia, v[0],v[1],v[2]));
        end;
      end;
  List.MergeCell(Cell);
end;

end.

