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
  analysesEnabled: set of (aeOIM) = [];
  oimRefSystem: TMatrix3x3f;

procedure Usage;
begin
  Writeln('Usage: ',ExtractFileName(ParamStr(0)), ' [options] [--] filename');
  Writeln('Options:');
  Writeln('  --help                        This text');
  Writeln('  --oim                         Enable Analysis: OIM');
  Writeln('  --reference -r m00,m01,m02,m11,...,m22');
  Writeln('                                Define reference coordinate system');
end;

const
  OptionsLong: array[1..4] of TOption = (
   (Name: 'help'; Has_Arg: No_Argument; Flag: nil; Value: 'h'),
   (Name: 'oim'; Has_Arg: No_Argument; Flag: nil; Value: #0),
   (Name: 'reference'; Has_Arg: Required_Argument; Flag: nil; Value: 'r'),
   (Name: ''; Has_Arg: 0; Flag: nil; Value: #0)
  );
  OptionShort = '?hr:';


procedure ProcessOption(const opt: string; const OptArg: string);
var
  a,b,c,d,e,f,g,h,j: Double;
begin
  case opt of
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

(*
   Implements the OIM Algorithm.
   Arguments:
     atoms      - obvious
     LatConst   - exptected lattice constant
     RefSystem  - Reference coordinate system.
                  Essentially, X and Y components span the cut plane and Z component is the normal vector
                  (pointing away from material), although only Z is actually used in computation.

   Results are stored in 3 attributes 'oim[rgb]', defaulting to (0,0,0) on atoms where no result is found.
   Association is in the standard triangle of the IPF, being r = <100>, g = <011>, b = <111>
*)
procedure AnalysisOIM(atoms: TLammpsFile; LatConst: Single; RefSystem: TMatrix3x3f);
const
  DEG_22 = 22/180*pi;
  DEG_45 = 45/180*pi;
  DEG_90 = 90/180*pi;
  PERM_3 : array[0..5] of array[0..2] of integer = ((0, 1, 2), (0, 2, 1), (1, 0, 2), (1, 2, 0), (2, 0, 1), (2, 1, 0));
  FLOAT_EPS = 1E-6;
  COLOR_THRESH = -0.01;
var
  tmr: TTimer;
  fox,foy,foz,fstate: Integer;
  i, n, m, k: integer;
  nn: TAtomRefs;
  at: PAtomRecord;
  found: boolean;
  center,c,dir,nvec, axis,
  lxax, lyax, lzax, stereo, asort: TVector3f;
  d,avgd, llattice: Single;
  proj: TMatrix3x3f;
  state: integer;
begin
  tmr.Start;
  fox:= atoms.FieldIndex('oimr',true);
  foy:= atoms.FieldIndex('oimg',true);
  foz:= atoms.FieldIndex('oimb',true);
  fstate:= atoms.FieldIndex('state',true);

  for i:= 0 to High(atoms.Atoms) do begin
    at:= @atoms.Atoms[i];

    center:= vecCreate(at^.x, at^.y, at^.z);

    nn:= atoms.GetNeighbors(i, LatConst * 1.1);

    nvec:= NULL_VECTOR;
    axis:= NULL_VECTOR;
    state:= 0;
    if Length(nn) = 15 then begin
      state:= 10;
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
        state:= 20;

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
            if abs(vecAngleBetween(dir,lxax) - DEG_90) < 0.9 then begin
              state:= 30;
              lyax:= vecNormalize(dir);

              // compute z axis from right-hand system
              lzax:= vecNormalize(vecCross(lxax, lyax));

              // we now have the image of the unit coordinate system in the current cells local rotation, turn it into a projection matrix
              proj:= matCreate(lxax, lyax, lzax);

              // compute projected reference axis
              axis:= matInvert(proj) * RefSystem[2];

              axis:= vecNormalize(axis);

              // compute stereoscopic projection
              stereo[0]:= axis[0] / Max(1E-6,1 - axis[2]);
              stereo[1]:= axis[1] / Max(1E-6,1 - axis[2]);
              stereo[2]:= 0;

              // quick check for standard triangle
              if not (
                (stereo[0] >= -FLOAT_EPS) and (stereo[1] >= -FLOAT_EPS) and     // first quadrant
                (stereo[1] <= stereo[0] + FLOAT_EPS) and                        // below y = x
                (stereo[0] < 0.5)                                               // main triangle part
                ) then begin
                continue;
              end;
              state:= 40;

              // for symmetry reasons, we will always get Z negative (lower half of the unit sphere when doing the stereographic projection)
              // but can change that sign (or, mathematically, the sign of the X and Y components (symmetry of the 8 octants) and then invert the whole vector)
              axis[2] *= -1;

              // We now decompose into factors of <001>,<011> and <111>
              // Those are direction kinds, we work with just one set here. By symmetry, we would also get those as candidates
              // if we would also take left-hand coordinate systems, but for pure cubic lattices, we can just take permutations of the values we already have.

              for k:= 0 to high(PERM_3) do begin
                asort[0]:= axis[PERM_3[k,0]];
                asort[1]:= axis[PERM_3[k,1]];
                asort[2]:= axis[PERM_3[k,2]];


                // decompose into r*(001),g*(011),b*(111) parts
                // {{r -> z - y, g -> y - x, b -> x}}
                nvec[0]:= asort[2] - asort[1];
                nvec[1]:= asort[1] - asort[0];
                nvec[2]:= asort[0];

                if i = 111722 then begin
                  WriteLn('candidate: ', String(nvec), '        ', String(stereo), '        ', String(asort));
                end;

                if (i <> 111722) and (nvec[0] >= COLOR_THRESH) and (nvec[1] >= COLOR_THRESH) and (nvec[2] >= COLOR_THRESH) then begin
                  state:= 99;
                  found:= true;
                  break;
                end;
              end; // for
              if found then
                break;
            end;
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
    atoms.Atoms[i].Fields[fstate]:= IntToStr(state);
  end;
  tmr.Stop;
  tmr.OutputTime('Analysis time');
end;

procedure ProcessFile(const filename: string);
var
  atoms: TLammpsFile;
  ex: String;
begin
  atoms:= TLammpsFile.Create;
  try
    atoms.LoadLAMMPSDumpFile(filename);
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

