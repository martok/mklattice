unit uTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils;

type
  TTimer = object
    ref,last: Int64;
    procedure Start;
    procedure Stop;
    function DeltaSeconds: Single;
    procedure OutputTime(Title: string);
  end;

implementation

{ TTimer }

procedure TTimer.Start;
begin
  ref:= GetTickCount64;
end;

procedure TTimer.Stop;
begin
  last:= GetTickCount64;
end;

function TTimer.DeltaSeconds: Single;
begin
  Result:= (last-ref) / 1000;
end;

procedure TTimer.OutputTime(Title: string);
var
  d: Int64;
  h,m,s,ms: integer;
begin
  d:= last - ref;
  ms:= d mod 1000;
  d:= d div 1000;
  s:= d mod 60;
  d:= d div 60;
  m:= d mod 60;
  d:= d div 60;
  h:= d;
  WriteLn(Format('%s: %.2d:%.2d:%.2d.%.4d',[
    Title,
    h,m,s,ms
  ]));
end;

end.

