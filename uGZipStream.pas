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
  end;

implementation

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

end.

