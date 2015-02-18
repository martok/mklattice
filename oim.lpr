program oim;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  uStrInput,
  uLammpsFile, uTimer, uLinAlg;


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

procedure AnalysisOIM(atoms: TLammpsFile; LatConst: Single; RefSystem: TMatrix3x3f);
const
  DEG_22 = 22/180*pi;
  DEG_45 = 45/180*pi;
var
  tmr: TTimer;
  fvx,fvy,fvz,
  fox,foy,foz: Integer;
  i, n: integer;
  nn: TAtomRefs;
  at: PAtomRecord;
  center,c,dir,nvec, axis,
  lxax, lyax, lzax,
  colx,coly,colz: TVector3f;
  d,avgd, llattice,minadiff, adiff: Single;
  mindiff: integer;
  proj: TMatrix3x3f;
begin
  tmr.Start;
  fvx:= atoms.FieldIndex('vecx',true);
  fvy:= atoms.FieldIndex('vecy',true);
  fvz:= atoms.FieldIndex('vecz',true);
  fox:= atoms.FieldIndex('oimx',true);
  foy:= atoms.FieldIndex('oimy',true);
  foz:= atoms.FieldIndex('oimz',true);

  colx:= vecNormalize(vecCreate(0,0,1));
  coly:= vecNormalize(vecCreate(1,0,1));
  colz:= vecNormalize(vecCreate(1,1,1));

  for i:= 0 to High(atoms.Atoms) do begin
    at:= @atoms.Atoms[i];
    center:= vecCreate(at^.x, at^.y, at^.z);

    nn:= atoms.GetNeighbors(i, LatConst * 1.1);

    nvec:= NULL_VECTOR;
    axis:= NULL_VECTOR;
    if Length(nn) = 15 then begin
      // compute local lattice constant from nearest neighbors (reflects lattice distortion)
      avgd:= 0;
      for n:= 1 to 8 do begin
        at:= @atoms.Atoms[nn[n]];
        c:= vecCreate(at^.x, at^.y, at^.z);
        dir:= c - center;
        d:= vecMagnitude(dir);
        avgd += d;
      end;
      avgd /= 8;

      llattice:= avgd / (sqrt(3) / 2);

      // compute second-neighbor distance and check latconst
      avgd:= 0;
      for n:= 9 to 14 do begin
        at:= @atoms.Atoms[nn[n]];
        c:= vecCreate(at^.x, at^.y, at^.z);
        dir:= c - center;
        d:= vecMagnitude(dir);
        avgd += d;
      end;
      avgd /= 6;

      // looks like BCC?
      if abs(llattice - avgd) < LatConst * 0.1 then begin

        // find first vector: axis that is most similar to (100)
        axis:= RefSystem[0];
        mindiff:= -1;
        minadiff:= 1000;
        for n:= 9 to 14 do begin
          at:= @atoms.Atoms[nn[n]];
          c:= vecCreate(at^.x, at^.y, at^.z);
          dir:= c - center;
          adiff:= vecAngleBetween(axis, dir);
          if (adiff < minadiff) and
             ((adiff < DEG_22) or (dir[1]>0)) then begin
            minadiff:= adiff;
            mindiff:= nn[n];
            lxax:= dir;
          end;
        end;
        if mindiff >= 0 then begin
          // choose axis
          lxax:= vecNormalize(lxax);

          // find second vector: axis that is most similar to (010) and perpendicular to the local (100)
          axis:= RefSystem[1];
          mindiff:= -1;
          minadiff:= 1000;
          for n:= 9 to 14 do begin
            at:= @atoms.Atoms[nn[n]];
            c:= vecCreate(at^.x, at^.y, at^.z);
            dir:= c - center;
            if abs(vecAngleBetween(dir,lxax) - pi/2) < 0.5 then begin
              adiff:= vecAngleBetween(axis, dir);
              if (adiff < minadiff) and
                 ((adiff < DEG_22) or (dir[2]>0)) then begin
                minadiff:= adiff;
                mindiff:= nn[n];
                lyax:= dir;
              end;
            end;
          end;
          // constrained to perpendicular vectors, so we may not have found one
          if mindiff >= 0 then begin
            // choose axis
            lyax:= vecNormalize(lyax);

            // compute z axis from right-hand system
            lzax:= vecCross(lxax, lyax);

            // we now have the image of the unit coordinate system in the current cells local rotation, turn it into a projection matrix
            proj:= matCreate(lxax, lyax, lzax);

            // find the distance between the projected reference axis and the 3 axis used for coloring
            axis:= proj * RefSystem[2];
            nvec[0]:= axis[2] - axis[1];
            nvec[1]:= axis[1] - axis[0];
            nvec[2]:= axis[0];
          end;
        end;
      end;
    end;

    atoms.Atoms[i].Fields[fvx]:= FloatToStr(axis[0]);
    atoms.Atoms[i].Fields[fvy]:= FloatToStr(axis[1]);
    atoms.Atoms[i].Fields[fvz]:= FloatToStr(axis[2]);

    atoms.Atoms[i].Fields[fox]:= FloatToStr(nvec[0]);
    atoms.Atoms[i].Fields[foy]:= FloatToStr(nvec[1]);
    atoms.Atoms[i].Fields[foz]:= FloatToStr(nvec[2]);
  end;
  tmr.Stop;
  tmr.OutputTime('Analysis time');
end;

procedure ProcessFile(const filename: string);
var
  atoms: TLammpsFile;
  refsys: TMatrix3x3f;
begin
  atoms:= TLammpsFile.Create;
  try
    atoms.LoadLAMMPSDumpFile(filename);
    atoms.PrepareNeighborCells(10);           {
    refsys:= matCreate(
      vecCreate(1, 0, 0),
      vecCreate(0, 1, 0),
      vecCreate(0, 0, 1)
    );
    }
    refsys:= matCreate(
      vecCreate(0, 0, 1),
      vecCreate(1, 0, 0),
      vecCreate(0, -1, 0)
    );
    AnalysisOIM(atoms, 2.86, refsys);
    atoms.SaveLAMMPSDumpFile(filename + '.out');
  finally
    FreeAndNil(atoms);
  end;
end;

begin
  LoadNeutralFormatSettings;

  if Paramcount <> 1 then begin
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' Filename');
    halt(1);
  end;

  ProcessFile(ParamStr(1));
end.

