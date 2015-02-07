General
=======
Collection of tools to pre- and postprocess [LAMMPS](http://lammps.sandia.gov/) simulation
input and output files.

Built with FreePascal 2.7, may or may not compile with Delphi or other FPC versions.

mklattice
---------
Lattice data file generator.

Can be used to generate files to be imported using `read_data` according to a specified
lattice and composition. Purpose-built for working with simple lattices (sc,bcc,fcc) and
B2 and DO3 phases of FeAl, but can be extended to other crystal structures. Atom types
vacancies and anti-sites are configured as needed.

Built with FreePascal 2.7, may or may not compile with Delphi or other FPC versions.

dumputl
-------
Utility to split large LAMMPS dump files into handier parts, optionally compressing them using
gzip (.gz).

Can be used to create the single-frame file series that A. Stukowski's CrystalAnalysis-Tool requires
or to extract subranges from large dump files. Frame offsets are stored in a .idx Mapfile associated
with the input file to speed up any call after the first initial scan.

File name formatting uses normal Pascal [format strings](http://www.freepascal.org/docs-html/rtl/sysutils/format.html).

License
=======
```
Copyright (c) 2014-15, Sebastian Hütter (sebastian.huetter@st.ovgu.de)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
