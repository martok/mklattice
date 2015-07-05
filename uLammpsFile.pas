unit uLammpsFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, uTimer, Math, uGZipStream;

type
  ELammpsError = class(Exception);

  TStringArr = array of string;
  PAtomRecord = ^TAtomRecord;
  TAtomRecord = record
    Fields: TStringArr;
    x,y,z: Single;
    NeighCell: integer;
  end;

  TAtomRefs = array of integer;

  TBoundingBox = record
    xlo,xhi,
    ylo,yhi,
    zlo,zhi: Single;
  end;

  PNeighborCell = ^TNeighborCell;
  TNeighborCell = record
    cx,cy,cz: integer;
    Atoms: TAtomRefs;
  end;
  TNeighborCells = array of TNeighborCell; // indexing is x + y * xcells + z * xcells*ycells;

  TLammpsFile = class
  private
    fTimestep: Int64;
    fPeriodicX: boolean;
    fPeriodicY: boolean;
    fPeriodicZ: boolean;
    fPeriodicDef: String;
  protected
    NeighXCells,NeighYCells,NeighZCells: integer;
    NeighborCells: TNeighborCells;

    procedure BBoxPrint;
    function NeighIndex(X,Y,Z: integer): integer; inline;
    procedure NeighborCellFromPos(const ax, ay, az: Single; out xi, yi, zi: integer);
  public
    FieldDefs: TStringList;
    Atoms: array of TAtomRecord;
    BBox: TBoundingBox;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadLAMMPSDumpFile(const aFileName: string);
    procedure LoadLAMMPSDataFile(const aFileName: string);
    procedure SaveLAMMPSDumpFile(const aFileName: string);
    function FieldIndex(const Fieldname: string; const CanCreate: boolean): integer;

    property Timestep: Int64 read fTimestep;
    property PeriodicDef: String read fPeriodicDef;
    property PeriodicX: boolean read fPeriodicX;
    property PeriodicY: boolean read fPeriodicY;
    property PeriodicZ: boolean read fPeriodicZ;

    procedure PrepareNeighborCells(NeighborCutoff: Single);
    function GetNeighbors(const atomID: Integer; const cutoff: Single): TAtomRefs;
  end;

implementation

uses
  uSScanf;

procedure SplitSpaces(s:string; out Result: TStringArr);
var
  sl: TStringList;
  i: integer;
begin
  sl:= TStringList.Create;
  try
    sl.Delimiter:=' ';
    sl.DelimitedText:= s;
    SetLength(Result, sl.Count);
    for i:= 0 to high(Result) do
      Result[i]:= sl[i];
  finally
    FreeAndNil(sl);
  end;
end;

{ TLammpsFile }

constructor TLammpsFile.Create;
begin
  inherited Create;
  FieldDefs:= TStringList.Create;
  Clear;
end;

destructor TLammpsFile.Destroy;
begin
  FreeAndNil(FieldDefs);
  inherited Destroy;
end;

procedure TLammpsFile.LoadLAMMPSDumpFile(const aFileName: string);
var
  ld: TextFile;
  item,l: String;
  lsp: TStringArr;
  i: integer;
  tmr: TTimer;
  fx,fy,fz,cmode: integer;

begin
  Clear;
  WriteLn('Reading file ', aFileName);

  tmr.Start;
  AssignFile(ld, aFileName);
  if AnsiEndsText('.gz', aFileName) then
    GZipInitFile(ld);
  Reset(ld);
  try
    while not EOF(ld) do begin
      ReadLn(ld, item);
      case item of
        'ITEM: TIMESTEP': begin
          // second timestep marker? done here
          if fTimestep >= 0 then
            exit;
          ReadLn(ld, l);
          fTimestep:= StrToInt64(l);
        end;
        'ITEM: NUMBER OF ATOMS': begin
          ReadLn(ld, l);
          SetLength(Atoms, StrToInt64(l));
        end;
      else
        if AnsiStartsStr('ITEM: BOX BOUNDS ', item) then begin
          SplitSpaces(item, lsp);
          fPeriodicDef:= lsp[3] + ' ' + lsp[4] + ' ' + lsp[5];
          fPeriodicX:= lsp[3] = 'pp';
          fPeriodicY:= lsp[4] = 'pp';
          fPeriodicZ:= lsp[5] = 'pp';
          ReadLn(ld, l);
          SplitSpaces(l, lsp);
          BBox.xlo:= StrToFloat(lsp[0]);
          BBox.xhi:= StrToFloat(lsp[1]);
          ReadLn(ld, l);
          SplitSpaces(l, lsp);
          BBox.ylo:= StrToFloat(lsp[0]);
          BBox.yhi:= StrToFloat(lsp[1]);
          ReadLn(ld, l);
          SplitSpaces(l, lsp);
          BBox.zlo:= StrToFloat(lsp[0]);
          BBox.zhi:= StrToFloat(lsp[1]);
          Writeln('Bounding box from file:');
          BBoxPrint;
        end else
        if AnsiStartsStr('ITEM: ATOMS ', item) then begin
          SplitSpaces(item, lsp);
          for i:= 2 to high(lsp) do
            FieldDefs.Add(lsp[i]);
          if FieldDefs.IndexOf('x') >=0 then begin
            cmode:= 0;
            fx:=  FieldDefs.IndexOf('x');
            fy:=  FieldDefs.IndexOf('y');
            fz:=  FieldDefs.IndexOf('z');
            if (fx<0) or (fy<0) or (fz<0) then
              raise ELammpsError.CreateFmt('All Coordinates must be specified in the same mode', []);
          end else
          if FieldDefs.IndexOf('xs') >=0 then begin
            cmode:= 1;
            fx:= FieldDefs.IndexOf('xs');
            fy:= FieldDefs.IndexOf('ys');
            fz:= FieldDefs.IndexOf('zs');
            if (fx<0) or (fy<0) or (fz<0) then
              raise ELammpsError.CreateFmt('All Coordinates must be specified in the same mode', []);
          end else
            raise ELammpsError.CreateFmt('Only the following coordinate modes are supported: x,xs', []);

          for i:= 0 to high(Atoms) do begin
            ReadLn(ld, l);
            With Atoms[i] do begin
              SplitSpaces(l, Fields);
              if Length(Fields) <> FieldDefs.Count then
                raise ELammpsError.CreateFmt('Atom data field count mismatch on atom %d', [i]);
              case cmode of
                0: begin
                  x:= StrToFloat(Fields[fx]);
                  y:= StrToFloat(Fields[fy]);
                  z:= StrToFloat(Fields[fz]);
                end;
                1: begin
                  x:= bbox.xlo + StrToFloat(Fields[fx]) * (BBox.xhi - BBox.xlo);
                  y:= bbox.ylo + StrToFloat(Fields[fy]) * (BBox.yhi - BBox.ylo);
                  z:= bbox.zlo + StrToFloat(Fields[fz]) * (BBox.zhi - BBox.zlo);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    Close(ld);
  end;
  tmr.Stop;
  tmr.OutputTime('File read time');
  WriteLn('Read ',Length(Atoms),' atoms from file.');
end;

procedure TLammpsFile.LoadLAMMPSDataFile(const aFileName: string);
var
  ld: TextFile;
  comment, item: String;
  k: int64;
  lsp: TStringArr;
  i, atypes: integer;
  tmr: TTimer;
  fx,fy,fz,ftype,fid: integer;
begin
  Clear;
  WriteLn('Reading file ', aFileName);

  tmr.Start;
  AssignFile(ld, aFileName);
  if AnsiEndsText('.gz', aFileName) then
    GZipInitFile(ld);
  Reset(ld);
  try
    // comment
    ReadLn(ld, comment);
    ReadLn(ld);
    // counts
    ReadLn(ld, item);
    if utlSScanf(item, '%D atoms',[@k]) <> 0 then
      raise ELammpsError.CreateFmt('Expected number of atoms', []);
    SetLength(Atoms, k);
    ReadLn(ld, item);
    if utlSScanf(item, '%d atom types',[@atypes]) <> 0 then
      raise ELammpsError.CreateFmt('Expected number of atom types', []);
    ReadLn(ld);
    // bounding box
    ReadLn(ld, item);
    SplitSpaces(item, lsp);
    BBox.xlo:= StrToFloat(lsp[0]);
    BBox.xhi:= StrToFloat(lsp[1]);
    ReadLn(ld, item);
    SplitSpaces(item, lsp);
    BBox.ylo:= StrToFloat(lsp[0]);
    BBox.yhi:= StrToFloat(lsp[1]);
    ReadLn(ld, item);
    SplitSpaces(item, lsp);
    BBox.zlo:= StrToFloat(lsp[0]);
    BBox.zhi:= StrToFloat(lsp[1]);
    Writeln('Bounding box from file:');
    BBoxPrint;
    ReadLn(ld);
    // masses
    Readln(ld, item);
    if item <> 'Masses' then
      raise ELammpsError.CreateFmt('Expected Masses section', []);
    ReadLn(ld);
    for i:= 1 to atypes do
      ReadLn(ld, item);
    ReadLn(ld);

    // atoms
    Readln(ld, item);
    if item <> 'Atoms # atomic' then
      raise ELammpsError.CreateFmt('Expected Atoms/atomic section', []);
    ReadLn(ld);

    fid:= FieldDefs.Add('id');
    ftype:= FieldDefs.Add('type');
    fx:= FieldDefs.Add('x');
    fy:= FieldDefs.Add('y');
    fz:= FieldDefs.Add('z');

    for i:= 0 to high(Atoms) do begin
      ReadLn(ld, item);
      With Atoms[i] do begin
        SplitSpaces(item, Fields);
        if Length(Fields) <> FieldDefs.Count then
          raise ELammpsError.CreateFmt('Atom data field count mismatch on atom %d', [i]);
        x:= StrToFloat(Fields[fx]);
        y:= StrToFloat(Fields[fy]);
        z:= StrToFloat(Fields[fz]);
      end;
    end;

    // skip velocities
  finally
    Close(ld);
  end;
  tmr.Stop;
  tmr.OutputTime('File read time');
  WriteLn('Read ',Length(Atoms),' atoms from file.');
end;

procedure TLammpsFile.SaveLAMMPSDumpFile(const aFileName: string);
var
  tmr: TTimer;
  ld: TextFile;
  i, j: integer;
  a: PAtomRecord;
begin
  WriteLn('Writing to file ', aFileName);

  tmr.Start;
  AssignFile(ld, aFileName);
  if AnsiEndsText('.gz', aFileName) then
    GZipInitFile(ld);
  Rewrite(ld);
  try
    WriteLn(ld, 'ITEM: TIMESTEP');
    WriteLn(ld, fTimestep);
    WriteLn(ld, 'ITEM: NUMBER OF ATOMS');
    WriteLn(ld, Length(Atoms));
    WriteLn(ld, 'ITEM: BOX BOUNDS '+fPeriodicDef);
    WriteLn(ld, FloatToStr(BBox.xlo), ' ', FloatToStr(bbox.xhi));
    WriteLn(ld, FloatToStr(BBox.ylo), ' ', FloatToStr(bbox.yhi));
    WriteLn(ld, FloatToStr(BBox.zlo), ' ', FloatToStr(bbox.zhi));
    Write(ld, 'ITEM: ATOMS');
    for i:= 0 to FieldDefs.Count - 1 do
      Write(ld, ' ', FieldDefs[i]);
    WriteLn(ld);
    for i:= 0 to high(Atoms) do begin
      a:= @Atoms[i];
      Write(ld,a^.Fields[0]);
      for j:= 1 to high(a^.Fields) do
        Write(ld,' ', a^.Fields[j]);
      WriteLn(ld);
    end;
  finally
    Close(ld);
  end;
  tmr.Stop;
  tmr.OutputTime('File write time');
  WriteLn('Wrote ',Length(Atoms),' atoms to file.');
end;

procedure TLammpsFile.Clear;
begin
  fTimestep:= -1;
  FieldDefs.Clear;
  SetLength(Atoms, 0);
  fPeriodicX:= false;
  fPeriodicY:= false;
  fPeriodicZ:= false;
  SetLength(NeighborCells,0);
  NeighXCells:= 0;
  NeighYCells:= 0;
  NeighZCells:= 0;
end;

procedure TLammpsFile.BBoxPrint;
begin
  WriteLn(' x ', BBox.xlo, ' ', bbox.xhi);
  WriteLn(' y ', BBox.ylo, ' ', bbox.yhi);
  WriteLn(' z ', BBox.zlo, ' ', bbox.zhi);
end;

procedure TLammpsFile.PrepareNeighborCells(NeighborCutoff: Single);
var
  tmr: TTimer;
  xi,yi,zi,i: integer;
  nc: PNeighborCell;
  ar: PAtomRecord;
begin
  tmr.Start;
  // estimate a cell size, but limit to what's reasonable memory-wise
  NeighXCells:= EnsureRange(Floor((BBox.xhi-BBox.xlo) / NeighborCutoff), 1, 100);
  NeighYCells:= EnsureRange(Floor((BBox.yhi-BBox.ylo) / NeighborCutoff), 1, 100);
  NeighZCells:= EnsureRange(Floor((BBox.zhi-BBox.zlo) / NeighborCutoff), 1, 100);

  // cache cell coordinates for later
  SetLength(NeighborCells,NeighXCells * NeighYCells * NeighZCells);
  for xi:= 0 to NeighXCells - 1 do
    for yi:= 0 to NeighYCells - 1 do
      for zi:= 0 to NeighZCells - 1 do begin
        with NeighborCells[NeighIndex(xi,yi,zi)] do begin
          cx:= xi;
          cy:= yi;
          cz:= zi;
          SetLength(Atoms, 0);
        end;
      end;

  // sort atoms into cells
  for i:= 0 to high(Atoms) do begin
    ar:= @Atoms[i];
    NeighborCellFromPos(ar^.x, ar^.y, ar^.z, xi, yi, zi);
    ar^.NeighCell:= NeighIndex(
      EnsureRange(xi, 0, NeighXCells-1),
      EnsureRange(yi, 0, NeighYCells-1),
      EnsureRange(zi, 0, NeighZCells-1)
    );

    nc:= @NeighborCells[ar^.NeighCell];
    SetLength(nc^.Atoms, Length(nc^.Atoms) + 1);
    nc^.Atoms[high(nc^.Atoms)]:= i;
  end;

  tmr.Stop;
  tmr.OutputTime('Neighbor prepare time');
end;

function TLammpsFile.NeighIndex(X, Y, Z: integer): integer;
begin
  Result:= X + Y * NeighXCells + Z * NeighXCells * NeighYCells;
end;

function TLammpsFile.GetNeighbors(const atomID: Integer; const cutoff: Single): TAtomRefs;
var
  rx,ry,rz,cosq: Single;
  dx,dy,dz,i: integer;
  clp: integer;
  CellList: array[0..3*3*3-1] of DWord;

  procedure MarkCell(mx,my,mz: integer);
  var
    ci: DWord;
    cx,cy,cz: integer;
  begin
    NeighborCellFromPos(rx + mx*cutoff, ry + my*cutoff, rz + mz*cutoff, cx, cy, cz);

    if (cx<0) or (cx >= NeighXCells) then begin
      if not fPeriodicX then exit;
      cx:= (cx + NeighXCells) mod NeighXCells;
    end;
    if (cy<0) or (cy >= NeighYCells) then begin
      if not fPeriodicY then exit;
      cy:= (cy + NeighYCells) mod NeighYCells;
    end;
    if (cz<0) or (cz >= NeighZCells) then begin
      if not fPeriodicZ then exit;
      cz:= (cz + NeighZCells) mod NeighZCells;
    end;
    ci:= NeighIndex(cx,cy,cz);
    if IndexDWord(CellList, clp, ci) < 0 then begin
      CellList[clp]:= ci;
      inc(clp);
    end;
  end;

var
  rlp: Integer;
  res: array[0..50] of record
    ID: Integer;
    DistSquared: Single;
  end;

  procedure ScanCell(cell: PNeighborCell);
  var
    i, ins, j: integer;
    at: PAtomRecord;
    d: Single;
  begin
    for i in cell^.Atoms do begin
      at:= @Atoms[i];

      d:= sqr(at^.x - rx) + sqr(at^.y - ry) + sqr(at^.z - rz);
      if d <= cosq then begin
        ins:= rlp;
        for j:= 0 to rlp-1 do begin
          if res[j].DistSquared>d then begin
            ins:= j;
            break;
          end;
        end;
        for j:= rlp downto ins+1 do
          res[j]:= res[j-1];
        res[ins].ID:= i;
        res[ins].DistSquared:= d;
        inc(rlp);
      end;
    end;
  end;

begin
  SetLength(Result, 0);
  if (atomID<0) or (atomID > High(Atoms)) then
    Exit;

  cosq:= sqr(cutoff);
  rx:= Atoms[atomID].x;
  ry:= Atoms[atomID].y;
  rz:= Atoms[atomID].z;

  // compute all possible cells that might have atoms inside our search sphere
  clp:= 0;
  for dx:= -1 to 1 do
    for dy:= -1 to 1 do
      for dz:= -1 to 1 do
        MarkCell(dx, dy, dz);

  // search cells we found for atoms inside the sphere
  rlp:= 0;
  for i:= 0 to clp-1 do
    ScanCell(@NeighborCells[CellList[i]]);

  // copy results to dynarray
  SetLength(Result, rlp);
  for i:= 0 to rlp-1 do
    Result[i]:= res[i].ID;
end;

function TLammpsFile.FieldIndex(const Fieldname: string; const CanCreate: boolean): integer;
var
  i: integer;
begin
  Result:= FieldDefs.IndexOf(Fieldname);
  if (Result < 0) and CanCreate then begin
    Result:= FieldDefs.Add(Fieldname);
    for i:= 0 to high(Atoms) do
      SetLength(Atoms[i].Fields, Result + 1);
  end;
end;

procedure TLammpsFile.NeighborCellFromPos(const ax, ay, az: Single; out xi, yi, zi: integer);
begin
  xi:= floor((ax - BBox.xlo) / (BBox.xhi - BBox.xlo) * NeighXCells);
  yi:= floor((ay - BBox.ylo) / (BBox.yhi - BBox.ylo) * NeighYCells);
  zi:= floor((az - BBox.zlo) / (BBox.zhi - BBox.zlo) * NeighZCells);
end;

end.

