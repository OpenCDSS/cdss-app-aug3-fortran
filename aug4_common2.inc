      common /iblock2/ nlog,incli,outcli,njunk
      integer nlog,incli,outcli,njunk
      common /cblock2/ fnlog, ver, vdate, tape74_base, tape74_ext,
     1                 junk_base
      character (len=48) junk_base
      character (len=24) fnlog, tape74_base, tape74_ext
      character (len=12) ver, vdate
      common /lblock2/ debug_cli, debug_log
      logical debug_cli, debug_log