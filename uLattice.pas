unit uLattice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uAtomList, uLinAlg;

type
  TLatticeDescriptor = class
    Name: string;
    BoxMatrix: TMatrix3x3f;
    Atoms: array of array of TVector3f;   // index 0: atomtype, index 1: atoms of type
    Elements: array of byte;              // for each in Atoms: element index

    constructor Create(aName: string);
    function SetMatrix(aMatrix: TMatrix3x3f): TLatticeDescriptor;
    function SubLattice(aElementIndex: byte; aPositions: array of TVector3f): TLatticeDescriptor;
  end;
  TLatticeDescriptorList = specialize TFPGObjectList<TLatticeDescriptor>;
  TLatticeElements = array of byte;

  TLatticeGenerator = class
    Lattice: TLatticeDescriptor;
    Elements: TLatticeElements;
    LatConst: Double;
    Cell: TSize3;
    Dim: TGrid3;
    Rotation: TMatrix3x3f;
    FilterFunction: TAtomFilter;
    constructor Create(aLattice: TLatticeDescriptor; aLatConst: Double);
    // Create a field so that realworld coordinates mX, mY, mZ form the upper corner
    function InitDimensions(mX, mY, mZ: Extended): TLatticeGenerator;
    // Create a field with a specific number of lattice nodes
    function InitLattice(mX, mY, mZ: Integer): TLatticeGenerator;
    // Set rotation matrix
    function SetRotation(aMatrix: TMatrix3x3f): TLatticeGenerator;
    // Set elements in the same order as pointed to in descriptor
    function SetElements(aElements: TLatticeElements): TLatticeGenerator;

    function Filter(aFilterFunc: TAtomFilter): TLatticeGenerator;
    function ExportAtoms(const List: TAtomList; var PlaceCounter: int64): TLatticeGenerator;
  end;

implementation

uses
  Math;

function DefaultFilter(const {%H-}AtomIndex: Integer; var {%H-}AtomType: byte; const {%H-}x,{%H-}y,{%H-}z: Single) : boolean;
begin
  Result:= true;
end;

{ TLatticeDescriptor }

constructor TLatticeDescriptor.Create(aName: string);
begin
  inherited Create;
  Name:= aName;
  BoxMatrix:= IDENTITY_MATRIX;
  SetLength(Atoms, 0);
  SetLength(Elements,0);
end;

function TLatticeDescriptor.SetMatrix(aMatrix: TMatrix3x3f): TLatticeDescriptor;
begin
  Result:= Self;
  BoxMatrix:= aMatrix;
end;

function TLatticeDescriptor.SubLattice(aElementIndex: byte; aPositions: array of TVector3f): TLatticeDescriptor;
var
  l,i: integer;
begin
  Result:= Self;
  l:= Length(Atoms);
  SetLength(Atoms, l+1);
  SetLength(Atoms[l], Length(aPositions));
  for i:= 0 to high(aPositions) do
    Atoms[l][i]:= aPositions[i];
  SetLength(Elements, l+1);
  Elements[l]:= aElementIndex;
end;

{ TLatticeGenerator }

constructor TLatticeGenerator.Create(aLattice: TLatticeDescriptor; aLatConst: Double);
begin
  inherited Create;
  Lattice:= aLattice;
  LatConst:= aLatConst;
  Rotation:= IDENTITY_MATRIX;
  SetLength(Elements, 0);
  FilterFunction:= @DefaultFilter;
end;

function TLatticeGenerator.InitDimensions(mX, mY, mZ: Extended): TLatticeGenerator;
var
  ax,ay,az: Extended;
begin
  Result:= Self;
  ax:= mX;
  ay:= mY;
  az:= mZ;
  InitLattice(ceil(ax / LatConst), ceil(ay / LatConst), ceil(az / LatConst));
  Cell[0]:= mX;
  Cell[1]:= mY;
  Cell[2]:= mZ;
end;

function TLatticeGenerator.InitLattice(mX, mY, mZ: Integer): TLatticeGenerator;
begin
  Result:= Self;
  Dim[0]:= ceil(mX);
  Dim[1]:= ceil(mY);
  Dim[2]:= ceil(mZ);
  Cell[0]:= mX * LatConst;
  Cell[1]:= mY * LatConst;
  Cell[2]:= mZ * LatConst;
end;

function TLatticeGenerator.SetRotation(aMatrix: TMatrix3x3f): TLatticeGenerator;
begin
  Result:= Self;
  Rotation:= aMatrix;
end;

function TLatticeGenerator.SetElements(aElements: TLatticeElements): TLatticeGenerator;
begin
  Result:= Self;
  Elements:= aElements;
end;

function TLatticeGenerator.Filter(aFilterFunc: TAtomFilter): TLatticeGenerator;
begin
  Result:= Self;
  FilterFunction:= aFilterFunc;
end;

function TLatticeGenerator.ExportAtoms(const List: TAtomList; var PlaceCounter: int64): TLatticeGenerator;
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
  i,j,k,s,a: integer;
  realrot, m: TMatrix3x3f;
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

  realrot:= Rotation * Lattice.BoxMatrix;

  // Find bounding box in rotated csys
  m:= matInvert(realrot);
  v:= m * vecCreate(0,0,0);
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
        for s:= 0 to high(Lattice.Atoms) do begin
          for a:= 0 to high(Lattice.Atoms[s]) do begin
            ia:= Elements[Lattice.Elements[s]];
            v:= vecCreate(
                  (i + Lattice.Atoms[s,a,0]) * LatConst,
                  (j + Lattice.Atoms[s,a,1]) * LatConst,
                  (k + Lattice.Atoms[s,a,2]) * LatConst);
            v:= realrot * v;
            if CellRange(v[0], Cell[0]) and
               CellRange(v[1], Cell[1]) and
               CellRange(v[2], Cell[2]) then begin
              inc(PlaceCounter);
              if FilterFunction(a,ia,v[0],v[1],v[2]) then
                List.Add(TAtomDef.Create(ia, v[0],v[1],v[2]));
            end;
          end;
        end;
      end;
  List.MergeCell(Cell);
end;

end.

