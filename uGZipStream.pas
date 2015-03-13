unit uGZipStream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gzio, zbase;

type
  TGZipStream = class(TStream)
  private
    fFile: gzFile;
  protected
  public
    constructor Create(Path, Mode: string);
    destructor Destroy; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Flush(const Mode: Longint): LongInt;
  end;

procedure GZipInitFile(var T: TextFile);

implementation

Procedure GZipFileCloseFunc(Var t:TextRec);
Begin
  TGZipStream(t.Handle).Free;
  t.Handle:=UnusedHandle;
End;

Procedure GZipFileReadFunc(var t:TextRec);
Begin
  t.BufEnd:=TGZipStream(t.Handle).Read(t.Bufptr^,t.BufSize);
  t.BufPos:=0;
End;


Procedure GZipFileWriteFunc(var t:TextRec);
var
  i : longint;
Begin
  { prevent unecessary system call }
  if t.BufPos=0 then
    exit;
  i:=TGZipStream(t.Handle).Write(t.Bufptr^,t.BufPos);
  if i<>t.BufPos then
    InOutRes:=101;
  t.BufPos:=0;
End;

Procedure GZipFileOpenFunc(var t:TextRec);
var
  m: string;
  strm: TGZipStream;
Begin
  Case t.mode Of
    fmInput : m:= 'r';
    fmOutput : m:= 'w';
    fmAppend : m:= 'a';
  else
   begin
     InOutRes:=102;
     exit;
   end;
  End;
  strm:= TGZipStream.Create(PFileTextRecChar(@t.Name), m);
  t.Handle:= THandle(strm);
  t.CloseFunc:=@GZipFileCloseFunc;
  t.FlushFunc:=nil;
  if t.Mode=fmInput then
   t.InOutFunc:=@GZipFileReadFunc
  else
   begin
     t.InOutFunc:=@GZipFileWriteFunc;
     t.FlushFunc:=nil; // no flush func, use fully buffered IO
   end;
End;

procedure GZipInitFile(var T: TextFile);[IOCheck];
begin
  if TTextRec(T).Mode <> fmClosed then begin
    InOutRes:= 102;
    exit;
  end;
  TTextRec(T).openfunc:= @GZipFileOpenFunc;
end;

{ TGZipStream }

constructor TGZipStream.Create(Path, Mode: string);
begin
  inherited Create;
  fFile:= gzopen(Path, Mode);
  if not Assigned(fFile) then
    raise EStreamError.Create('Could not create gzip stream!');
end;

destructor TGZipStream.Destroy;
begin
  gzflush(fFile, Z_FINISH);
  gzclose(fFile);
  fFile:= nil;
  inherited Destroy;
end;

function TGZipStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result:= gzseek(fFile, Offset, Origin);
end;

function TGZipStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:= gzread(fFile, @Buffer, Count);
end;

function TGZipStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:= gzwrite(fFile, @Buffer, Count);
end;

function TGZipStream.Flush(const Mode: Longint): LongInt;
begin
  Result:= gzflush(fFile, Mode);
end;

end.

