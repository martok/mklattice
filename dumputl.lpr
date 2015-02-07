program dumputl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, getopts, Classes, uGZipStream;

var
  InputFile: string = '';
  ForceRemap: boolean = false;
  ScanningBuffer: Cardinal = 1024;
  ExtractFrames: array of Integer;
  CompressOutput: byte = 0;
  MergeOutput: boolean = false;
  OutputNameFormat: string = '%0:s.%2:.4d.dump';

const
  PATTERN = #10'ITEM: TIMESTEP'#10;
  MAPFILE_EXT = '.idx';
  MAPFILE_VER = 1;
var
  inpFile: THandle;
  PositionMap: array of record
    Timestep,
    ByteOffset, ByteLen: Int64;
  end;

procedure Usage;
begin
  Writeln('Usage: ',ExtractFileName(ParamStr(0)), ' [options] [--] filename');
  Writeln('Options:');
  Writeln('  --help                        This text');
  Writeln('  --remap -r                    Force recreation of frame map file');
  Writeln('  --merge -m                    Merge all output into one file instead of one per frame');
  Writeln('  --gzip -z [LEVEL]             Compress output, adds .gz to file name.');
  Writeln('                                Specify LEVEL 0 to disable compression.');
  Writeln('  --extract -x [FRAME|FROM-TO]  extract the specified frames, can be given more than once');
  Writeln('  --format [FORMATSTRING]       file name pattern, positional arguments:');
  Writeln('                                0: file name without extension');
  Writeln('                                1: file name without extension or path');
  Writeln('                                2: frame index');
  Writeln('                                3: timestep');
end;

const
  OptionsLong: array[1..7] of TOption = (
   (Name: 'help'; Has_Arg: No_Argument; Flag: nil; Value: 'h'),
   (Name: 'remap'; Has_Arg: No_Argument; Flag: nil; Value: 'r'),
   (Name: 'merge'; Has_Arg: No_Argument; Flag: nil; Value: 'm'),
   (Name: 'gzip'; Has_Arg: Optional_Argument; Flag: nil; Value: 'z'),
   (Name: 'extract'; Has_Arg: Required_Argument; Flag: nil; Value: 'x'),
   (Name: 'format'; Has_Arg: Required_Argument; Flag: nil; Value: 'f'),
   (Name: ''; Has_Arg: 0; Flag: nil; Value: #0)
  );
  OptionShort = '?hrmz::e:f:';


procedure AddExtract(Frame: Integer);
var
  i,insertat: integer;
begin
  insertat:= Length(ExtractFrames);
  for i:= 0 to high(ExtractFrames) do begin
    if ExtractFrames[i] = Frame then
      exit;
    if ExtractFrames[i] > Frame then begin
      insertat:= i;
      break;
    end;
  end;
  SetLength(ExtractFrames, Length(ExtractFrames) + 1);
  for i:= high(ExtractFrames) downto insertat + 1 do
    ExtractFrames[i]:= ExtractFrames[i-1];
  ExtractFrames[insertat]:= Frame;
end;

procedure ProcessParams;
var
  opt: string;
  optIndex: integer;
  a,b,i: integer;
begin
  while true do begin
    opt:= GetLongOpts(OptionShort, @OptionsLong[1], optIndex);
    if opt = EndOfOptions then
      break;
    if opt = #0 then
      opt:= OptionsLong[optIndex].Name;
    case opt of
      'r': ForceRemap:= true;
      'm': MergeOutput:= true;
      'z': begin
        if OptArg > '' then
          CompressOutput:= StrToInt(OptArg)
        else
          CompressOutput:= 5;
      end;
      'e': begin
        i:= Pos('-',OptArg);
        if i = 0 then begin
          AddExtract(StrToInt(OptArg))
        end else begin
          a:= StrToInt(Copy(OptArg, 1, i-1));
          b:= StrToInt(Copy(OptArg, i+1, Maxint));
          for i:= a to b do
            AddExtract(i);
        end;
      end;
      'f': begin
        OutputNameFormat:= OptArg;
      end;
      '?',
      'h': begin
        Usage;
        halt(0);
      end;
    end;
  end;
  optIndex:= OptInd;
  InputFile:= ParamStr(optIndex);
end;

procedure SaveMap;
var
  mapf: THandle;
  l: Cardinal;
  i: integer;
  sz: Int64;
begin
  mapf:= FileCreate(InputFile + MAPFILE_EXT);
  if mapf <> 0 then begin
    try
      l:= MAPFILE_VER;
      FileWrite(mapf, l, sizeof(l));
      sz:= FileSeek(inpFile, Int64(0), fsFromEnd);
      FileWrite(mapf, sz, sizeof(sz));
      l:= Length(PositionMap);
      FileWrite(mapf, l, sizeof(l));
      for i:= 0 to high(PositionMap) do
        FileWrite(mapf, PositionMap[i], sizeof(PositionMap[i]));
      WriteLn('Mapfile: saved ',l,' items');
    finally
      FileClose(mapf);
    end;
  end;
end;

procedure BuildMap;
var
  bufpos,o,pmc: Cardinal;
  buf,tsl: RawByteString;
  bufinfile, currpos, currts: int64;

  function ReadMoreAppend: Boolean;
  var
    r,
    ds,
    os: Cardinal;
  begin
    ds:= ScanningBuffer * 1024;
    os:= Length(buf);
    SetLength(buf, os + ds);
    r:= FileRead(inpFile, buf[os+1], ds);
    Setlength(buf, os + r);
    Result:= r > 0;
  end;

begin
  WriteLn('Mapfile: rebuilding file map');
  SetLength(PositionMap, 0);
  FileSeek(inpFile, 0, 0);
  bufinfile:= 0;

  buf:= #10;
  bufpos:= 1;
  repeat
    if not ReadMoreAppend then
      break; // EOF

    bufpos:= Pos(PATTERN, buf);
    while bufpos > 0 do begin
      o:= bufpos - 1 + Length(PATTERN);
      currpos:= bufinfile + bufpos - 1;
      inc(bufinfile, o);
      Delete(buf, 1, o);

      o:= Pos(#10, Buf);
      if o = 0 then
        ReadMoreAppend;
      o:= Pos(#10, Buf);
      tsl:= Copy(buf, 1, o-1);
      currts:= StrToInt64(tsl);

      pmc:= length(PositionMap);
      if pmc > 0 then
        PositionMap[pmc-1].ByteLen:= currpos-PositionMap[pmc-1].ByteOffset;
      SetLength(PositionMap, pmc+1);
      with PositionMap[pmc] do begin
        Timestep:= currts;
        ByteOffset:= currpos;
        ByteLen:= 0;
      end;
      WriteLn('Timestep: ', currts, ' @ ', currpos);

      bufpos:= Pos(PATTERN, buf);
    end;

    bufpos:= LastDelimiter(#10, buf);
    o:= bufpos - 1;
    inc(bufinfile, o);
    Delete(buf, 1, o);
  until false;
  pmc:= length(PositionMap);
  currpos:= FileSeek(inpFile, Int64(0), fsFromCurrent);
  if pmc > 0 then
    PositionMap[pmc-1].ByteLen:= currpos-PositionMap[pmc-1].ByteOffset;

  SaveMap;
end;

procedure TryLoadMap;
var
  mapf: THandle;
  l: Cardinal;
  i: integer;
  sz, expectsz: Int64;
begin
  if not ForceRemap then begin
    mapf:= FileOpen(InputFile + MAPFILE_EXT, fmOpenRead);
    if mapf <> 0 then begin
      try
        if (FileRead(mapf, l, sizeof(l)) = SizeOf(l)) and (l = MAPFILE_VER) then begin
          FileRead(mapf, sz, sizeof(sz));
          expectsz:= FileSeek(inpFile, Int64(0), fsFromEnd);
          if expectsz = sz then begin
            FileRead(mapf, l, sizeof(l));
            SetLength(PositionMap, l);
            WriteLn('Mapfile: loading ',l,' items');
            for i:= 0 to high(PositionMap) do
              FileRead(mapf, PositionMap[i], sizeof(PositionMap[i]));

            exit;
          end else
            WriteLn('Mapfile: valid, but file size differs');
        end;
      finally
        FileClose(mapf);
      end;
    end;
  end;
  // fallback
  BuildMap;
end;

procedure ExtractMarked;
var
  i,frame: integer;
  outf: TStream;
  Buf: RawByteString;

  procedure NextFile;
  var
    fn: string;
  begin
    fn:= Format(OutputNameFormat,[
      ChangeFileExt(InputFile, ''),
      ExtractFileName(ChangeFileExt(InputFile, '')),
      frame,
      PositionMap[frame].Timestep
    ]);
    if CompressOutput > 0 then
      fn += '.gz';

    if MergeOutput then begin
      if not Assigned(outf) then begin
        if CompressOutput>0 then
          outf:= TGZipStream.Create(fn, 'w' + IntToStr(CompressOutput))
        else
          outf:= TFileStream.Create(fn, fmCreate);
        WriteLn('Extract: opened merged output file ', fn);
      end;
      Write('Extract: frame ',frame,' ... ');
    end else begin
      Write('Extract: frame ',frame,' to ',fn, ' ... ');

      if CompressOutput>0 then
        outf:= TGZipStream.Create(fn, 'w' + IntToStr(CompressOutput))
      else
        outf:= TFileStream.Create(fn, fmCreate);
    end;
  end;

  procedure FileDone;
  begin
    if not MergeOutput then
      FreeAndNil(outf);
  end;

begin
  outf:= nil;
  for i:= 0 to high(ExtractFrames) do begin
    frame:= ExtractFrames[i];
    if (frame < 0) or (frame > high(PositionMap)) then begin
      WriteLn(StdErr, 'Extract: frame ',frame,' out of bounds!');
      continue;
    end;

    NextFile;
    try
      SetLength(Buf, PositionMap[frame].ByteLen);
      FileSeek(inpFile, PositionMap[frame].ByteOffset, fsFromBeginning);
      outf.WriteBuffer(buf[1], FileRead(inpFile, Buf[1], Length(Buf)));
    finally
      FileDone;
    end;
    WriteLn('OK');
  end;
  if MergeOutput then
    FreeAndNil(outf);
end;

begin
  if ParamCount = 0 then begin
    Usage;
    Halt(1);
  end;

  ProcessParams;

  if InputFile='' then begin
    WriteLn(StdErr, 'Error: no input file given');
    Halt(1);
  end;

  inpFile:= FileOpen(InputFile, fmOpenRead);
  if inpFile = 0 then begin
    WriteLn(StdErr, 'Error: can''t open input file');
    Halt(1);
  end;
  try
    TryLoadMap;

    ExtractMarked;
  finally
    FileClose(inpFile);
  end;
end.

