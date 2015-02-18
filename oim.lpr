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

(*
   Implements the OIM Algorithm.
   Arguments:
     atoms      - obvious
     LatConst   - exptected lattice constant
     RefSystem  - Reference coordinate system.
                  Essentially, X and Y components span the cut plane and Z component is the normal vector
                  (pointing into material), although only Z is actually used in computation.

   Results are stored in 3 attributes 'oim[rgb]', defaulting to (0,0,0) on atoms where no result is found.
   Association is in the standard triangle of the IPF, being r = <100>, g = <011>, b = <111>
*)
procedure AnalysisOIM(atoms: TLammpsFile; LatConst: Single; RefSystem: TMatrix3x3f);
const
  DEG_22 = 22/180*pi;
  DEG_45 = 45/180*pi;
  DEG_90 = 90/180*pi;
var
  tmr: TTimer;
  fox,foy,foz: Integer;
  i, n, m: integer;
  nn: TAtomRefs;
  at: PAtomRecord;
  found: boolean;
  center,c,dir,nvec, axis,
  lxax, lyax, lzax: TVector3f;
  d,avgd, llattice: Single;
  proj: TMatrix3x3f;
begin
  tmr.Start;
  fox:= atoms.FieldIndex('oimr',true);
  foy:= atoms.FieldIndex('oimg',true);
  foz:= atoms.FieldIndex('oimb',true);

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

        // iterate over all 24 right-hand systems and find the one that results in a projection inside the standard triangle

        found:= false;
        // find first vector: axis that is most similar to (100)
        for n:= 9 to 14 do begin
          at:= @atoms.Atoms[nn[n]];
          c:= vecCreate(at^.x, at^.y, at^.z);
          dir:= c - center;
          lxax:= vecNormalize(dir);

          // find second vector: axis that is most similar to (010) and perpendicular to the local (100)
          for m:= 9 to 14 do begin
            at:= @atoms.Atoms[nn[m]];
            c:= vecCreate(at^.x, at^.y, at^.z);
            dir:= c - center;
            if abs(vecAngleBetween(dir,lxax) - DEG_90) < 0.5 then begin
              lyax:= vecNormalize(dir);

              // compute z axis from right-hand system
              lzax:= vecNormalize(vecCross(lxax, lyax));

              // we now have the image of the unit coordinate system in the current cells local rotation, turn it into a projection matrix
              proj:= matCreate(lxax, lyax, lzax);

              // compute projected reference axis
              axis:= matInvert(proj) * RefSystem[2];

              // decompose into r*(001),g*(011),b*(111) parts
              // {{r -> z - y, g -> y - x, b -> x}}
              nvec[0]:= axis[2] - axis[1];
              nvec[1]:= axis[1] - axis[0];
              nvec[2]:= axis[0];

              if (nvec[0] >= -0.1) and (nvec[1] >= -0.1) and (nvec[2] >= -0.1) then begin
                found:= true;
                break;
              end;
            end;
            if found then
              break;
          end; //for
          // did we find a system? good
          if found then
            break;
        end; // for

        if not found then
          nvec:= NULL_VECTOR;
      end;
    end;

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
    atoms.PrepareNeighborCells(10);
    refsys:= matCreate(
      vecCreate(1, 0, 0),
      vecCreate(0, 1, 0),
      vecCreate(0, 0, 1)
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

