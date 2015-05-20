# Dump Index File Format

## Goal

LAMMPS dump files may contain multiple snapshots taken at different timestamps. Since finding the delimiter in a plain-text format requires scanning the whole file before being able to randomly access it, the format in this document is used to cache this information.

## File Naming Convention

The cache file for `<file>` MUST be named `<file>.idx`.

Example:
```
output.dump            -- the dump file
output.dump.idx        -- the index file
```

If a file conforming to this naming convention is found, it SHOULD be loaded, unless the user specified that no index file is to be read.

## File Contents

The first four octets specifiy the Format Version.

Defined is:
```
01:00:00:00           -- Version 1
```

### Version 1 File Contents

Integers are stored in little-endian (LSB first) byte order. Files that end prematurely MUST be rejected. An error MAY be reported to the user, or the file MAY be silently discarded and rebuild. Files that contain extra data after the nominal end MAY be accepted without further notice.

```
FileSize        | QWORD (8 octets)    | Referenced File Size
FrameCount      | DWORD (4 octets)    | Number of Frames in File
FrameStart[]    | FrameCount *        | Starting position of each frame, in file order.
                | QWORD (8 octets)    |
```
`FileSize` MUST be used to verify that the Index File and Dump File are matched.

Each `FrameStart` points to the beginning of a frame, that is, reading from this position will yield the string `ITEM: TIMESTEP`. The end of a frame is defined by the next `FrameStart` item or the file end if the frame is the last in the list.

