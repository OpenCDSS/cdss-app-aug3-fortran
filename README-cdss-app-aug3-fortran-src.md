# dwr_aug3_scripts
AUG3 is a CDWR project that consists of an EXCEL interface that access three fortran program:
  aug4.for:  Pre-processor that generates input file for MODFLOW program
  dry.for: Post-processor that looks for and reports any dry cells in the MODFLOW output file
  GS3DRIVE.for: 1988 version of MODFLOW updated to provide stream depletion data file

##2017 Improvement to aug4.for and dry.for
small project for dwr to rewrite two aug3 gwbasic scripts (dry.bas, aug4.bas)from GWBASIC into fortran Previous dwr aug3 system requires gwbasic to run scripts like aug4.bas, dry.bas.  but gwbasic is an old 16 bit windows app and is problematic on modern windows computers.  Rewriting aug4.bas and dry.bas in a compilable computer language familiar to dwr staff will allow aug3 to be installed without needing gwbasic.  The fortran language was chosen because it is familiar to dwr staff, similar to basic in form, and still a relevant modern computer language with compiler implementations like GNU's gfortran. The versiom for aug4.for for this improvement is 0.81

##2018 Improvemnt to aug4.for
aug4.for was modified to generate input files for areas in the Laramie-Fox Hills and Arapahoe Aquifers.  In 2018 the model files for these aquifers within the Desjignated Basins were revised because of new geology.  If was also found that these revisions required that the pre-processor needed to expand the area used to generate input information.

