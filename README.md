# dwr_aug3_scripts
small project for dwr to rewrite two aug3 gwbasic scripts (dry.bas, aug4.bas) into fortran

current dwr aug3 system requires gwbasic to run scripts like aug4.bas, dry.bas.  but gwbasic is an old 16 bit windows app and is problematic on modern windows computers.  rewriting aug4.bas and dry.bas in a compilable computer language familiar to dwr staff will allow aug3 to be installed without needing gwbasic.
the fortran language was chosen because it is familiar to dwr staff, similar to basic in form, and still a relevant modern computer language with compiler implementations like GNU's gfortran

