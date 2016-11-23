c                            
c ************************************************************
c
      program aug4
c
      IMPLICIT NONE
c
c
c _________________________________________________________
c Program Description
c
c       AUG4.FOR - FORTRAN version of AUG4.BAS
c                  AUG4.BAS is a modified version of AUG3.BAS
c
c _________________________________________________________
c       Documentation               
c
c
c _________________________________________________________
c       Update History
c
c jhb 11/11/2016: original conversion of AUG4.BAS
c
c _________________________________________________________
c     common blocks
      common /cblock1/ AS,BS,CS,CCS
      character(len=32) AS,BS,CS,CCS
      common /iblock1/ X,Y,Z,IWIDE,NTS
      integer X,Y,Z,IWIDE,NTS
      common /rblock1/ QSS(1,1),SUMQSS(1,1),QNOM,DELT(1),FACT
      real QSS,SUMQSS,QNOM,DELT,FACT
      common /lblock1/ debug_cli, debug_log, runid_commandline
      logical debug_cli, debug_log, runid_commandline
      include 'aug4_common2.inc'
      !common /iblock2/ nlog,incli,outcli
      !integer nlog,incli,outcli
c _________________________________________________________
c     Local variables
c     command line arguments
      integer iargcount, command_line_len, status
      character(len=64) :: command_line
      character(len=32) :: arg, args
      dimension args(10)
c     tape74
      character(len=24) :: basename, extension
c     
      integer NSP
      character(len=32) shorts, dds, rns, rnns
      integer PERLEN
      real TSMULT
      dimension NTS(10),TSMULT(10),PERLEN(10)

      character fnlog*24,ver*12,vdate*12

      integer ioptio, iback


      data ver      /'version 0.0 '/
      data vdate    /'11/11/2016  '/
      data fnlog    /'aug4.log'/
      data nlog     /13/
      data outcli   /6/
      data incli    /5/
      data debug_cli    /.TRUE./
      data debug_log    /.TRUE./
c _________________________________________________________
c     set the run id
      data rnns    /'R1'/
c _________________________________________________________
c     set the tape74 parameters
      data basename    /'TAPE74'/
      data extension    /'.DAT'/
c _________________________________________________________
c     get and use the command line arguments
      ! gfortran method to get the command line arg strings
      ! fyi - there is a iargc function for f77 backward compat
      !CALL GET_COMMAND(command_line, command_line_len, status)
      !write(outcli,*) "command_line = ",command_line
      !write(outcli,*) "command_line_len = ",command_line_len
      !write(outcli,*) "status = ",status
      iargcount = 0
      do
        call get_command_argument(iargcount,arg)
        if (len_trim(arg) == 0) exit
        if (debug_cli) write(outcli,*) "arg ",iargcount," = ",arg
        if (debug_log) write(nlog,*) "arg ",iargcount," = ",arg
        ! use the first arg as the run id if the user provided one
        if (iargcount.eq.1) then
          runid_commandline = .TRUE.
          rnns = arg
        endif
        args(iargcount+1) = trim(arg)
        iargcount = iargcount + 1
      end do
c _________________________________________________________
c     if it exists, make sure tape74.dat (model output) gets renamed
      call tape74setup(basename,extension)
c _________________________________________________________
c     log file
      open(nlog,file=fnlog, status='unknown')
      write(nlog,200) ver, vdate, rnns
      write(outcli,200) ver, vdate, rnns
 200  format(
     1 72('_'),//
     2 '        AUG4                       '/
     3 '        State of Colorado - Denver Basin Aquifer Models '//
     4 '        Version: ',a12,/,
     5 '        Last revision date: ',a12,//
     6 '        Run id set to ',a32,//
     7 72('_'))     
c _________________________________________________________
c     option menu
c       use the same menu design as statemod (might as well, Thanks Ray Bennett!!)
c       ioptio is the selected option
c       if ioptio is 0, then it did not get set on the command line, so show the menu
c         and keep returning to the menu when complete, until exit/stop is selected
c         (set iback=1)
c       if ioptio is >0, then it got set on the command line, so don't show the menu
c         and do not go back
      ioptio = 0
      iback = 0
 100  if(ioptio.eq.0) then
        iback = 1
        Write(outcli,110)
        call flush(6)
 110    format(/,
     1           ' Option? ',
     2        //,'   [0] : STOP',
     3         /,'   [1] : One',
     4         /,'   [2] : Two',
     5         /,'   [3] : Three',
     6         /,'   [4] : Version')
c rrb 10/27/94 Additional Output
        write(outcli,*) ' '
        read (incli,*,err=165) ioptio
      endif
c _________________________________________________________
c     user selected exit/stop from menu
      if(ioptio.eq.0) goto 170
      if(ioptio.gt.4) goto 165
      GO TO (130,140,150,160) IOPTIO
c ______________________________________________________________________
c     Option One
 130  write(nlog, 132) 'Option One  '
      write(outcli, 132) 'Option One  '
 132  format(/,72('_'),//'  AUG4; Option selected = ', a12)
c      call optionone
      goto 166
c ______________________________________________________________________
c     Option Two
 140  write(nlog, 132) 'Option Two  '
      write(outcli, 132) 'Option Two  '
c      call optiontwo
      goto 166
c ______________________________________________________________________
c     Option Three
 150  write(nlog, 132) 'Option Three'
      write(outcli, 132) 'Option Three'
c      call optionthree
      goto 166
c ______________________________________________________________________
c     Option Four
 160  write(nlog, 132) 'Version     '
      write(outcli, 132) 'Option Four '
      write(outcli,200) ver, vdate
      goto 166
c ______________________________________________________________________
c     Invalid Option
 165  write(outcli,*) ' ** Invalid option, try again **'
      call flush(outcli)
      goto 166               
c _________________________________________________________
c     Go back to menu if in default mode
 166  if(iback.eq.1) then
        ioptio = 0
        goto 100  
      endif
c _________________________________________________________
c     exit the program      
 170  write(outcli,180) fnlog
      call flush(outcli)
 180  format(/,'  AUG4; See detailed messages in file: ',a24,/) 
 190  write(outcli,*) 'Stop 0'
      close(nlog)
      call flush(outcli)
      call exit(0)
      stop
      END


c 20 ON ERROR GOTO 4000
c 25 OPEN "I",#1,"TAPE74.DAT"
c 26 CLOSE#1:
c 27 PRINT "MODEL OUTPUT (TAPE74.DAT) HAS NOT BEEN RENAMED WITH RUN NUMBER"
c 28 INPUT "ENTER RUN NUMBER TO BE USED TO RENAME TAPE74.DAT  ";RNRN$
c 29 NAME "TAPE74.DAT" AS "TAPE74."+RNRN$
      subroutine tape74setup(basename,oldextension)
        character(len=24)  :: basename, oldextension
        !rename only if file basename+extension exists
        !if it does, ask for the new extension
        !then rename the file
        include 'aug4_common2.inc'
        ! local variables
        character(len=48) :: oldfilename, newfilename
        character(len=24) :: newextension
        logical file_exists

        oldfilename = trim(basename)//trim(oldextension)
        INQUIRE(FILE=trim(oldfilename), EXIST=file_exists)
        if (file_exists) then
          write(outcli,*)
     1'MODEL OUTPUT (TAPE74.DAT) HAS NOT BEEN RENAMED WITH A RUN NUMBER'
 200      write(outcli,*)
     1      'ENTER RUN NUMBER TO BE USED TO RENAME TAPE74.DAT  '
          read (incli,*,err=210) newextension
          newfilename = trim(basename)//'.'//trim(newextension)
          write(outcli,*) 'renaming ',trim(oldfilename),
     1      ' to ',trim(newfilename)
          call rename(trim(oldfilename),trim(newfilename))
          return
 210      write(outcli,*)'Invalid response.'
          goto 200
        endif
 299    return
      end


c     subroutine 3000
      subroutine printStreamDepletion(flg3)
        integer flg3
        !common blocks
        !common /cblock1/ AS,BS,CS,CCS
        !character(len=32) AS,BS,CS,CCS
        !common /iblock1/ X,Y,Z,IWIDE,NTS
        !integer X,Y,Z,IWIDE,NTS
        !common /rblock1/ QSS(1,1),SUMQSS(1,1),QNOM,DELT(1),FACT
        !real QSS,SUMQSS,QNOM,DELT,FACT
        !local variables
        logical fileOutput
        real sum
        if (flg3.eq.1) then
          fileOutput=.TRUE.
        else
          fileOutput=.FALSE.
        endif
        if (fileOutput) then
        endif
        return
      end
c 3000 ' ********* SUB TO PRINT STREAM SEG. DEPLETION ****************
c 3010 IF FLG3=1 GOTO 3030
c 3020 PRINT#3, TAB(1);A$;TAB(B);
c 3030 FOR Y=1 TO NTS
c 3040 IF FLG3=1 GOTO 3070
c 3050 PRINT#3, USING"##.#######";QSS(Y,Z);
c 3060 IF (Y MOD IWIDE)=0 THEN PRINT#3, TAB(B);
c 3070 SUMQSS(Y,Z)=SUMQSS(Y,Z)+QSS(Y,Z)
c 3080 NEXT Y
c 3090 IF FLG3=1 THEN RETURN
c 3100 PRINT#3, TAB(1);CC$;TAB(B);
c 3110 FOR Y=1 TO NTS
c 3120 PRINT#3, USING"######.###";QSS(Y,Z)*100/QNOM;
c 3130 IF (Y MOD IWIDE)=0 THEN PRINT#3, TAB(B);
c 3140 NEXT Y
c 3150 PRINT#3, TAB(1);B$;TAB(B);
c 3160 FOR Y=1 TO NTS
c 3170 PRINT#3, USING"#####.####";QSS(Y,Z)*DELT(Y)*FACT;
c 3180 IF (Y MOD IWIDE)=0 THEN PRINT#3, TAB(B);
c 3190 NEXT Y
c 3200 PRINT#3, TAB(1);C$;TAB(B);:SUM=0
c 3210 FOR Y=1 TO NTS
c 3220 SUM=SUM+QSS(Y,Z)*DELT(Y)*FACT
c 3230 PRINT#3, USING"#####.####";SUM;
c 3240 IF (Y MOD IWIDE)=0 THEN PRINT#3, TAB(B);
c 3250 NEXT Y
c 3260 RETURN
