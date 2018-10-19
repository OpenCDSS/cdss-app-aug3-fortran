# cdss_aug3_scripts
AUG3 is a CDSS program that consists of an EXCEL interface that access these fortran program:
1. aug4.for:  Pre-processor that generates input files for MODFLOW program
1. aug4_db.for:  Pre-processor that gnerated input files for MODFLOW progrtam run only in locations within the Designated Basins 
1. dry.for: Post-processor that looks for and reports any dry cells in the MODFLOW output file
1. GS3DRIVE.for: 1983 version of MODFLOW revised in 1985, 1986 and 1990 by CDWR staff to provide stream depletion data file

## 2017 improvement to aug4.for and dry.for
small project for dwr to rewrite two aug3 gwbasic scripts (dry.bas, aug4.bas)from GWBASIC into fortran Previous dwr aug3 system requires gwbasic to run scripts like aug4.bas, dry.bas.  but gwbasic is an old 16 bit windows app and is problematic on modern windows computers.  Rewriting aug4.bas and dry.bas in a compilable computer language familiar to dwr staff will allow aug3 to be installed without needing gwbasic.  The fortran language was chosen because it is familiar to dwr staff, similar to basic in form, and still a relevant modern computer language with compiler implementations like GNU's gfortran. The versiom for aug4.for for this improvement is 0.81

## 2018 revision to aug4.for creating aug4_db.for
aug4.for was revised to generate input files for areas in the Laramie-Fox Hills and Arapahoe Aquifers in the Designated Basins.  In 2018 the model files for these aquifers within the Desjignated Basins were revised because of new geology.  If was also found that these revisions required that the pre-processor needed to expand the area used to generate input information. The verison was renamed aug4_db.for and was given version number 0.91 and this code should only be used for the Designated Basin Excel program. All other versions of the Excel Program should apply aug4.for version 0.81

