c                            
c ************************************************************
c
        program aug4
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
c Dimensions, variable declarations and initialization via
c   data statements
c
      integer iargcount
      character(len=32) :: arg, args
      dimension args(10)
      character(len=24) :: basename, extension
      integer NSP
      character(len=32) SHORT, DD, RN
      integer NTS, PERLEN
      real TSMULT
      dimension NTS(10),TSMULT(10),PERLEN(10)

      character charvar*24,fnlog*24,ver*12,vdate*12
      character rnn*12

      dimension charvar(3)
      real realvar
      dimension realvar(3)
      integer int2var, int4var
      dimension int2var(3), int4var(3)
      integer intvar,nlog,incli,outcli
      dimension intvar(3)
      integer ioptio, iback


      data charvar  /'charvar one','charvar two','charvar three'/
      data ver      /'version 0.0 '/
      data vdate    /'11/11/2016  '/
      data fnlog    /'aug4.log'/
      data realvar  /1.111,2.222,3.333/
      data int2var  /1,2,3/
      data int4var  /11,22,33/
      data intvar   /111,222,333/
      data nlog     /13/
      data outcli   /6/
      data incli    /5/
c _________________________________________________________
c     set the run id
      data rnn    /'R1'/
c _________________________________________________________
c     get and use the command line arguments
      ! gfortran method to get the command line arg strings
      iargcount = 0
      do
        call get_command_argument(iarg,arg)
        if (len_trim(arg) == 0) exit
        args(iargcount+1) = trim(arg)
        iargcount = iargcount + 1
      end do
c _________________________________________________________
c     make sure tape74.dat has been renamed
c     with the run number as the extension
      basename="tape74"
      extension=".dat"
      call changefnext(basename,extension)
c _________________________________________________________
c     log file
      open(nlog,file=fnlog, status='unknown')
      write(nlog,200) ver, vdate
      write(outcli,200) ver, vdate
  200 format(
     1 72('_'),//
     2 '        AUG4                       '/
     3 '        State of Colorado - Denver Basin Aquifer Models '//
     4 '        Version: ',a12,/,
     5 '        Last revision date: ',a12,//
     6 '        Run number set to ''R1''',//
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
c
      ioptio = 0
      iback = 0
 100  if(ioptio.eq.0) then
        iback = 1
        Write(outcli,110)
        call flush(6)
  110   format(/,
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
  130 write(nlog, 132) 'Option One  '
      write(outcli, 132) 'Option One  '
  132 format(/,72('_'),//'  AUG4; Option selected = ', a12)
c      call optionone
      goto 166
c ______________________________________________________________________
c     Option Two
  140 write(nlog, 132) 'Option Two  '
      write(outcli, 132) 'Option Two  '
c      call optiontwo
      goto 166
c ______________________________________________________________________
c     Option Three
  150 write(nlog, 132) 'Option Three'
      write(outcli, 132) 'Option Three'
c      call optionthree
      goto 166
c ______________________________________________________________________
c     Option Four
  160 write(nlog, 132) 'Version     '
      write(outcli, 132) 'Option Four '
      write(outcli,200) ver, vdate
      goto 166
c ______________________________________________________________________
c     Invalid Option
  165 write(outcli,*) ' ** Invalid option, try again **'
      call flush(outcli)
      goto 166               
c _________________________________________________________
c     Go back to menu if in default mode
  166 if(iback.eq.1) then
        ioptio = 0
        goto 100  
      endif
c _________________________________________________________
c     exit the program      
  170 write(outcli,180) fnlog
      call flush(outcli)
  180 format(/,'  AUG4; See detailed messages in file: ',a24,/) 
  190 write(outcli,*) 'Stop 0'
      close(nlog)
      call flush(outcli)
      call exit(0)
      stop
      END

c     subroutine 3000
      subroutine printStreamDepletion(flg3)
        integer flg3
        !common blocks
        common /cblock1/ AS,BS,CS,CCS
        character(len=32) AS,BS,CS,CCS
        common /iblock1/ X,Y,Z,IWIDE,NTS
        integer X,Y,Z,IWIDE,NTS
        common /rblock1/ QSS(,),SUMQSS(,),QNOM,DELT(),FACT
        real QSS,SUMQSS,QNOM,DELT(),FACT
        !local variables
        logical fileOutput
        real sum
        if


        write(iprn,)
        return
      end

      subroutine changefnext(basename,oldextension)
        character(len=24)  :: basename, oldextension
        !only if file basename+extension exists
        !if it does, ask for the new extension
        !then rename the file
        character(len=48) :: oldfilename, newfilename
        character(len=24) :: newextension
        logical file_exists
        oldfilename = trim(basename)//trim(oldextension)
        INQUIRE(FILE=trim(oldfilename), EXIST=file_exists)
        if (file_exists) then
          newextension = ".R1"
          newfilename = trim(basename)//trim(newextension)
          call rename(trim(oldfilename),trim(newfilename))
        endif
        return
      end





3000 ' ********* SUB TO PRINT STREAM SEG. DEPLETION ****************
3010 IF FLG3=1 GOTO 3030
3020 PRINT#3, TAB(1);A$;TAB(B);
3030 FOR Y=1 TO NTS
3040 IF FLG3=1 GOTO 3070
3050 PRINT#3, USING"##.#######";QSS(Y,Z);
3060 IF (Y MOD IWIDE)=0 THEN PRINT#3, TAB(B);
3070 SUMQSS(Y,Z)=SUMQSS(Y,Z)+QSS(Y,Z)
3080 NEXT Y
3090 IF FLG3=1 THEN RETURN
3100 PRINT#3, TAB(1);CC$;TAB(B);
3110 FOR Y=1 TO NTS
3120 PRINT#3, USING"######.###";QSS(Y,Z)*100/QNOM;
3130 IF (Y MOD IWIDE)=0 THEN PRINT#3, TAB(B);
3140 NEXT Y
3150 PRINT#3, TAB(1);B$;TAB(B);
3160 FOR Y=1 TO NTS
3170 PRINT#3, USING"#####.####";QSS(Y,Z)*DELT(Y)*FACT;
3180 IF (Y MOD IWIDE)=0 THEN PRINT#3, TAB(B);
3190 NEXT Y
3200 PRINT#3, TAB(1);C$;TAB(B);:SUM=0
3210 FOR Y=1 TO NTS
3220 SUM=SUM+QSS(Y,Z)*DELT(Y)*FACT
3230 PRINT#3, USING"#####.####";SUM;
3240 IF (Y MOD IWIDE)=0 THEN PRINT#3, TAB(B);
3250 NEXT Y
3260 RETURN
