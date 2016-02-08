program analysis;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  uStrInput, getopts, uGetOpt, uSScanf,
  uLammpsFile, uTimer, uLinAlg, Math;

var
  InputFile: string = '';
  ExpectedLatConst: Single = 2.86;
  InputFileType: (ftDump, ftData) = ftDump;
  analysesEnabled: set of (aeOIM) = [];
  oimRefSystem: TMatrix3x3f;

procedure Usage;
begin
  Writeln('Usage: ',ExtractFileName(ParamStr(0)), ' [options] [--] filename');
  Writeln('Options:');
  Writeln('  --help                        This text');
  Writeln('  --read -f DUMP|DATA           Input File Format, defautl: DUMP');
  Writeln('  --oim                         Enable Analysis: OIM');
  Writeln('  --reference -r m00,m01,m02,m11,...,m22');
  Writeln('                                Define reference coordinate system');
end;

const
  OptionsLong: array[1..5] of TOption = (
   (Name: 'help'; Has_Arg: No_Argument; Flag: nil; Value: 'h'),
   (Name: 'read'; Has_Arg: Required_Argument; Flag: nil; Value: 'f'),
   (Name: 'oim'; Has_Arg: No_Argument; Flag: nil; Value: #0),
   (Name: 'reference'; Has_Arg: Required_Argument; Flag: nil; Value: 'r'),
   (Name: ''; Has_Arg: 0; Flag: nil; Value: #0)
  );
  OptionShort = '?hf:r:';


procedure ProcessOption(const opt: string; const OptArg: string);
var
  a,b,c,d,e,f,g,h,j: Double;
begin
  case opt of
    'f': case LowerCase(OptArg) of
      'dump': InputFileType:= ftDump;
      'data': InputFileType:= ftData;
    else
      WriteLn(ErrOutput, 'Invalid input file type: ', OptArg);
      Halt(1);
    end;
    'oim': Include(analysesEnabled, aeOIM);
    'r': if utlSScanf(OptArg,'%f,%f,%f,%f,%f,%f,%f,%f,%f',[@a,@b,@c,@d,@e,@f,@g,@h,@j],NeutralFormatSettings) = 0 then
           oimRefSystem:= matCreate(
              vecCreate(a, b, c),
              vecCreate(d, e, f),
              vecCreate(g, h, j)
            );
    '?',
    'h': begin
      Usage;
      halt(0);
    end;
  end;
end;

procedure AnalysisTest(atoms: TLammpsFile);
var
  tmr: TTimer;
  ftest: Integer;
  i: integer;
  nn: TAtomRefs;
begin
  ftest:= atoms.FieldIndex('test',true);

  tmr.Start;
  for i:= 0 to High(atoms.Atoms) do begin
    nn:= atoms.GetNeighbors(i, 3);

    atoms.Atoms[i].Fields[ftest]:= IntToStr(Length(nn));
  end;
  tmr.Stop;
  tmr.OutputTime('Analysis time');
end;

{$I analysis_OIM.pas}


procedure ProcessFile(const filename: string);
var
  atoms: TLammpsFile;
  ex: String;
begin
  atoms:= TLammpsFile.Create;
  try
    case InputFileType of
      ftDump: atoms.LoadLAMMPSDumpFile(filename);
      ftData: atoms.LoadLAMMPSDataFile(filename);
    end;

    atoms.PrepareNeighborCells(ExpectedLatConst * 3);

    if aeOIM in analysesEnabled then begin
      WriteLn('Computing OIM...');
      AnalysisOIM(atoms, ExpectedLatConst, oimRefSystem);
      ex:= ExtractFileExt(filename);
      atoms.SaveLAMMPSDumpFile(ChangeFileExt(filename,'') + '.oim' + ex);
    end;
  finally
    FreeAndNil(atoms);
  end;
end;

var
  lastopt: Integer;
begin
  LoadNeutralFormatSettings;

  oimRefSystem:= IDENTITY_MATRIX;

  lastopt:= HandleAllOptions(OptionShort, @OptionsLong, @ProcessOption);
  InputFile:= ParamStr(lastopt);

  ProcessFile(InputFile);
end.

