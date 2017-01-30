      common /iblock2/ nlog,incli,outcli,njunk,
     1                 t9unit1,t9unit2,t1unit1,t1unit2
      integer nlog,incli,outcli,njunk,
     1        t9unit1,t9unit2,t1unit1,t1unit2
      common /cblock2/ fnlog, ver, vdate, tape74_base, tape74_ext,
     1                 junk_base, dirsep
      character(len=48) :: junk_base
      character(len=24) :: fnlog, tape74_base, tape74_ext
      character(len=12) :: vdate
      character(len=5) :: ver
      character(len=1) :: dirsep
      common /lblock2/ debug_cli, debug_log, versioncli, helpcli,
     1              nspcli, tslencli, ntscli, perlencli, tsmultcli
      logical debug_cli, debug_log, versioncli, helpcli,
     1              nspcli, tslencli, ntscli, perlencli, tsmultcli