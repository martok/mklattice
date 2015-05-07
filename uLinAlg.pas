unit uLinAlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TVector3f = array[0..2] of Single;

const
  NULL_VECTOR: TVector3f = (0,0,0);

function vecCreate(x,y,z: Single): TVector3f;
function vecMagnitude(v: TVector3f): Single;
function vecAngleBetween(a,b: TVector3f): Single;
function vecCross(a,b: TVector3f): TVector3f;
function vecNormalize(a: TVector3f): TVector3f;

function vecMaxComponent(a: TVector3f): Single;

operator-(a,b: TVector3f): TVector3f;
operator*(a,b: TVector3f): Single;
operator*(a: TVector3f; b: Single): TVector3f;

operator :=(a: TVector3f): string;

type
  TMatrix3x3f = array[0..2] of TVector3f;

const
  IDENTITY_MATRIX: TMatrix3x3f = ((1,0,0),(0,1,0),(0,0,1));

function matCreate(baseX, baseY, baseZ: TVector3f): TMatrix3x3f;
function matDeterminant(m: TMatrix3x3f): Single;
function matInvert(m:TMatrix3x3f): TMatrix3x3f;
function matRotation(axis: TVector3f; const angle: Double): TMatrix3x3f;

operator *(a: TMatrix3x3f; b: TVector3f): TVector3f;
operator *(a: TMatrix3x3f; b: TMatrix3x3f): TMatrix3x3f;

implementation

function vecCreate(x, y, z: Single): TVector3f;
begin
  Result[0]:= x;
  Result[1]:= y;
  Result[2]:= z;
end;

function vecMagnitude(v: TVector3f): Single;
begin
  Result:= sqrt(sqr(v[0])+sqr(v[1])+sqr(v[2]));
end;

function vecAngleBetween(a, b: TVector3f): Single;
var
  ctheta: Single;
begin
  ctheta:= EnsureRange((a * b) / (vecMagnitude(a) * vecMagnitude(b)), -1, 1);
  Result:= arccos(ctheta);
end;

function vecCross(a, b: TVector3f): TVector3f;
begin
  Result[0]:= a[1]*b[2] - a[2]*b[1];
  Result[1]:= a[2]*b[0] - a[0]*b[2];
  Result[2]:= a[0]*b[1] - a[1]*b[0];
end;

function vecNormalize(a: TVector3f): TVector3f;
begin
  Result:= a * (1/vecMagnitude(a));
end;

function vecMaxComponent(a: TVector3f): Single;
begin
  Result:= a[0];
  if Result < a[1] then
    Result:= a[1];
  if Result < a[2] then
    Result:= a[2];
end;

operator-(a, b: TVector3f): TVector3f;
begin
  Result[0]:= a[0] - b[0];
  Result[1]:= a[1] - b[1];
  Result[2]:= a[2] - b[2];
end;

operator*(a, b: TVector3f): single;
begin
  Result:= a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
end;

operator*(a: TVector3f; b: Single): TVector3f;
begin
  Result[0]:= a[0] * b;
  Result[1]:= a[1] * b;
  Result[2]:= a[2] * b;
end;

operator:=(a: TVector3f): string;
begin
  Result:= format('{%f, %f, %f}',[a[0], a[1], a[2]]);
end;

function matCreate(baseX, baseY, baseZ: TVector3f): TMatrix3x3f;
begin
  Result[0]:= baseX;
  Result[1]:= baseY;
  Result[2]:= baseZ;
end;

function matDeterminant(m: TMatrix3x3f): Single;
begin
  Result:=
    m[0,0] * m[1,1] * m[2,2] +
    m[1,0] * m[2,1] * m[0,2] +
    m[2,0] * m[0,1] * m[1,2] -
    m[2,0] * m[1,1] * m[0,2] -
    m[1,0] * m[0,1] * m[2,2] -
    m[0,0] * m[2,1] * m[1,2];
end;

function matInvert(m: TMatrix3x3f): TMatrix3x3f;
var
  idet: Single;
begin
  idet:= 1 / matDeterminant(m);
  Result:= matCreate(
    vecCreate(m[1,1] * m[2,2] - m[2,1] * m[1,2], m[2,1] * m[0,2] - m[0,1] * m[2,2], m[0,1] * m[1,2] - m[1,1] * m[0,2]) * idet,
    vecCreate(m[2,0] * m[1,2] - m[1,0] * m[2,2], m[0,0] * m[2,2] - m[2,0] * m[0,2], m[1,0] * m[0,2] - m[0,0] * m[1,2]) * idet,
    vecCreate(m[1,0] * m[2,1] - m[2,0] * m[1,1], m[2,0] * m[0,1] - m[0,0] * m[2,1], m[0,0] * m[1,1] - m[1,0] * m[0,1]) * idet
  );
end;

function matRotation(axis: TVector3f; const angle: Double): TMatrix3x3f;
var
  X, Y, Z, s, c: Double;
begin
  axis := vecNormalize(axis);
  X := axis[0];
  Y := axis[1];
  Z := axis[2];
  sincos(angle, s, c);
  result := IDENTITY_MATRIX;
  result[0] := vecCreate(
    SQR(X) + (1-SQR(X))*c,
    X*Y*(1-c) + Z*s,
    X*Z*(1-c) - Y*s);
  result[1] := vecCreate(
    X*Y*(1-c) - Z*s,
    SQR(Y) + (1-SQR(Y))*c,
    Y*Z*(1-c) + X*s);
  result[2] := vecCreate(
    X*Z*(1-c) + Y*s,
    Y*Z*(1-c) - X*s,
    SQR(Z) + (1-SQR(Z))*c);
end;
operator*(a: TMatrix3x3f; b: TVector3f): TVector3f;
begin
  Result[0]:= a[0,0] * b[0] + a[1,0] * b[1] + a[2,0] * b[2];
  Result[1]:= a[0,1] * b[0] + a[1,1] * b[1] + a[2,1] * b[2];
  Result[2]:= a[0,2] * b[0] + a[1,2] * b[1] + a[2,2] * b[2];
end;

operator*(a: TMatrix3x3f; b: TMatrix3x3f): TMatrix3x3f;
var
  x, y, i: Integer;
  sum: Single;
begin
  for x := 0 to 2 do begin
    for y := 0 to 2 do begin
      sum := 0;
      for i := 0 to 2 do
        sum := sum + a[i, y] * b[x, i];
      result[x, y] := sum;
    end;
  end;
end;

end.

