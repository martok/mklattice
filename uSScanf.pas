unit uSScanf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

function utlSScanf(const s: string; const fmt: string; const Pointers: array of Pointer): Integer;
function utlSScanf(const s: string; const fmt: string; const Pointers: array of Pointer; const FormatSettings: TFormatSettings): Integer;

implementation

function utlSScanf(const s: string; const fmt: string; const Pointers: array of Pointer): Integer;
begin
  Result:= utlSScanf(s, fmt, Pointers, DefaultFormatSettings);
end;

{
   0 on success (perfect match)
  +N if fmt was consumed without error, but there are characters left in s. Last parsed character is N
  -N if there was an error consuming pattern character N

  %s: string, must be terminated by a fixed string in fmt
  %d: integer
  %D: int64
}
function utlSScanf(const s: string; const fmt: string; const Pointers: array of Pointer; const FormatSettings: TFormatSettings): Integer;
var
  pt, fpos, spos:  integer;
  limitChar: Char;
  success: Boolean;

  function GetString: boolean;
  var
    tmp: string;
  begin
    tmp:= '';
    Result:= false;
    if limitChar = #0 then begin
      tmp:= Copy(s, spos, MaxInt);
      inc(spos, length(tmp));
      Result:= true;
    end else begin
      while spos <= Length(s) do begin
        if s[spos] = limitChar then begin
          dec(spos); // consume again
          Result:= true;
          break;
        end;
        tmp:= tmp + s[spos];
        inc(spos);
      end;
    end;
    PString(Pointers[pt])^:= tmp;
  end;

  function GetInteger(i64: boolean): Boolean;
  var
    tmp: string;
    v: int64;
  begin
    if limitChar = '%' then
      limitChar:= #0;
    tmp:= '';
    Result:= false;
    while spos <= Length(s) do begin
      if s[spos] = limitChar then begin
        dec(spos); // consume again
        break;
      end;
      tmp:= tmp + s[spos];
      if not TryStrToInt64(tmp, v) then begin
        dec(spos);
        SetLength(tmp, Length(tmp) - 1);
        Break;
      end;
      inc(spos);
    end;
    if TryStrToInt64(tmp, v) then begin
      if i64 then
        PInt64(Pointers[pt])^:= v
      else
        PInteger(Pointers[pt])^:= v;
      Result:= true;
    end;
  end;

begin
  pt:= 0;
  fpos:= 1;
  spos:= 1;
  while fpos <= length(fmt) do begin
    if spos > Length(s) then
      Exit(-fpos);

    case fmt[fpos] of
      '%': begin
        inc(fpos);
        if fpos > Length(fmt) then
          Exit(-fpos);
        if fpos < Length(fmt) then
          limitChar:= fmt[fpos+1]
        else
          limitChar:= #0;
        case fmt[fpos] of
          's': success:= GetString;
          'd': success:= GetInteger(false);
          'D': success:= GetInteger(True);
        else
          Exit(-fpos);
        end;
        if not success then
          Exit(-fpos);
        inc(pt);
      end
    else
      if fmt[fpos] <> s[spos] then
        Exit(-fpos);
    end;
    inc(fpos);
    inc(spos);
  end;
  if spos <= Length(s) then
    Exit(spos)
  else
    Exit(0);
end;

end.

