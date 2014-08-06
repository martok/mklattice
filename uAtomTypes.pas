unit uAtomTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAtomList;

var
  AtomTypes: array of TAtomType;

const
  ElementFileName = 'elements.tsv';

procedure LoadAtomTypes;

implementation

procedure LoadAtomTypes;
var
  atf,atl: TStringList;
  i: integer;
  at: TAtomType;
begin
  atf:= TStringList.Create;
  try
    atl:= TStringList.Create;
    try
      atl.Delimiter:= #9;
      atl.StrictDelimiter:= true;

      atf.LoadFromFile(ExtractFilePath(ParamStr(0)) + ElementFileName);
      SetLength(AtomTypes, atf.Count);
      for i:= 0 to atf.Count-1 do begin
        atl.DelimitedText:= atf[i];
        at.ID:= StrToInt(atl[0]);
        at.Name:= atl[1];
        at.Weight:= StrToFloat(atl[2]);

        AtomTypes[i]:= at;
      end;
    finally
      FreeAndNil(atl);
    end;
  finally
    FreeAndNil(atf);
  end;
  WriteLn(ErrOutput, 'Loaded ',Length(AtomTypes),' elements');
end;

end.

