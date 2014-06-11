unit uAtomList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TAtomType = record
    ID: Byte;
    Weight: Extended;
    Name: String;
  end;

  TSize3 = array[0..3] of Extended;

  TAtomDef = class
    AtType: Byte;
    X,Y,Z: Extended;
    constructor Create(const aAtType: byte; const aX, aY, aZ: Extended);
  end;
  TAtomListBase = specialize TFPGObjectList<TAtomDef>;
  TAtomList = class(TAtomListBase)
  private
    fCell: array[0..3] of Extended;
  public
    procedure RemoveAtoms(const Min, Max, Keep: TSize3); overload;
    procedure RemoveAtoms(const Min, Max: TSize3); overload;
    procedure ExportAtoms(const Comment: String; const TypeTable: array of TAtomType);
    constructor Create;
    procedure MergeCell(const aCell: TSize3);
    property Cell: TSize3 read fCell write fCell;
  end;

function Size3(const a, b, c: Extended): TSize3;

implementation

uses
  Math;

function Size3(const a, b, c: Extended): TSize3;
begin
  Result[0]:= a;
  Result[1]:= b;
  Result[2]:= c;
end;

{ TAtomDef }

constructor TAtomDef.Create(const aAtType: byte; const aX, aY, aZ: Extended);
begin
  inherited Create;
  AtType:= aAtType;
  X:= aX;
  Y:= aY;
  Z:= aZ;
end;

{ TAtomList }

procedure TAtomList.RemoveAtoms(const Min, Max, Keep: TSize3);
var
  i: integer;
  a: TAtomDef;
  kept, deleted: Int64;
begin
  // remove atoms that lie between Min and Max but not between Min and Keep
  kept:= 0;
  deleted:= 0;
  for i:= Count - 1 downto 0 do begin
    a:= Items[i];
    if ((a.X >= Min[0]) and (a.Y >= Min[1]) and (a.Z >= Min[2]))  and
       ((a.X <= Max[0]) and (a.Y <= Max[1]) and (a.Z <= Max[2])) then begin
      if ((a.X <= Keep[0]) and (a.Y <= Keep[1]) and (a.Z <= Keep[2])) then
        inc(kept)
      else begin
        Delete(i);
        inc(deleted);
      end;
    end;
  end;
  WriteLn(StdErr, 'Removed ',deleted,' atoms, kept ',kept);
end;

procedure TAtomList.RemoveAtoms(const Min, Max: TSize3);
begin
  RemoveAtoms(Min, Max, Min);
end;

procedure TAtomList.ExportAtoms(const Comment: String; const TypeTable: array of TAtomType);
var
  i, k, m: integer;
  ix,iy,iz,ax,ay,az: Extended;
  p: TAtomDef;
  Masses: TStringList;
begin
  // Full path to find box extents and used atom types
  ix:= Infinity;
  iy:= Infinity;
  iz:= Infinity;
  ax:= NegInfinity;
  ay:= NegInfinity;
  az:= NegInfinity;
  Masses:= TStringList.Create;
  try
    for i:= 0 to Count-1 do begin
      p:= Items[i];
      // new type?
      m:= Masses.IndexOfName(IntToStr(p.AtType));
      if m < 0 then begin
        // not mapped yet, find out what this is and store index in TypeTable
        m:= -1;
        for k:= 0 to high(TypeTable) do
          if TypeTable[k].ID = p.AtType then begin
            // store atom type index back in m
            m:= Masses.Add(IntToStr(p.AtType) + Masses.NameValueSeparator + IntToStr(k));
            break;
          end;
        if m < 0 then begin
          WriteLn(StdErr, 'Undefined atom type ID: ',p.AtType);
          halt;
        end;
      end;
      p.AtType:= m;
      // compute new extents
      if p.X < ix then ix:= p.X;
      if p.X > ax then ax:= p.X;

      if p.Y < iy then iy:= p.Y;
      if p.Y > ay then ay:= p.Y;

      if p.z < iz then iz:= p.z;
      if p.z > az then az:= p.z;
    end;

    // Write Header
    WriteLn(Comment);
    WriteLn('');

    WriteLn(Count,' atoms');
    WriteLn(Masses.Count,' atom types');
    WriteLn('');

    WriteLn(ix, ' ', fCell[0], ' xlo xhi');
    WriteLn(iy, ' ', fCell[1], ' ylo yhi');
    WriteLn(iz, ' ', fCell[2], ' zlo zhi');
    WriteLn('');

    WriteLn('Masses');
    WriteLn('');
    for i:= 0 to Masses.Count-1 do begin
      m:= StrToInt(Masses.ValueFromIndex[i]);
      WriteLn(i+1, ' ', TypeTable[m].Weight);
    end;
    WriteLn('');
  finally
    FreeAndNil(Masses);
  end;

  WriteLn('Atoms # atomic');
  WriteLn('');
  for i:= 0 to Count-1 do
    with Items[i] do
      WriteLn(i+1, ' ', AtType+1, ' ', X, ' ', Y, ' ', Z);
  WriteLn('');

  // all at rest
  WriteLn('Velocities');
  WriteLn('');
  for i:= 0 to Count-1 do
    WriteLn(i+1, ' 0.0 0.0 0.0');
  WriteLn('');
end;

constructor TAtomList.Create;
begin
  inherited Create(true);
  fCell[0]:= 0;
  fCell[1]:= 0;
  fCell[2]:= 0;
end;

procedure TAtomList.MergeCell(const aCell: TSize3);
begin
  fCell[0]:= Max(fCell[0], aCell[0]);
  fCell[1]:= Max(fCell[1], aCell[1]);
  fCell[2]:= Max(fCell[2], aCell[2]);
end;

end.

