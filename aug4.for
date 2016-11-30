c _________________________________________________________
      program aug4
      IMPLICIT NONE
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
      !common /cblock1/ AS,BS,CS,CCS
      !character(len=32) AS,BS,CS,CCS
      !common /iblock1/ X,Y,Z,IWIDE
      !integer X,Y,Z,IWIDE
      !common /rblock1/ QSS(1,1),SUMQSS(1,1),QNOM,DELT(1),FACT
      !real QSS,SUMQSS,QNOM,DELT,FACT
c     common /lblock1/ 
c     logical
c     file i/o, program info, debug flags
      include 'aug4_common2.inc'
c     40 COMMON NTS(), TSMULT(), PERLEN(),NSP,SHORT$,DD1$,AQNAM$,RN$,OSELECT,RNN$
c       plus others
      include 'aug4_common3.inc'
c _________________________________________________________
c     Local variables
      integer ioptio, iback

      data ver        /'version 0.0 '/
      data vdate      /'11/11/2016  '/
      data fnlog      /'aug4.log'/
      data nlog       /13/
      data outcli     /6/
      data incli      /5/
      data debug_cli  /.TRUE./
      data debug_log  /.TRUE./
c _________________________________________________________
c     set the default run id
      data rnns      /'R1'/
c _________________________________________________________
c     set the tape74 name
      data tape74_base /'TAPE74'/
      data tape74_ext  /'.DAT'/
c _________________________________________________________
c     set the junk name
      data junk_base /'JUNK'/
c _________________________________________________________
c     set the simulation period default/max count
      data spselect  /2/
      data spcount   /10/
c _________________________________________________________
c     set the aquifer names
      data aqselect  /1/
      data aqcount   /6/
      data aqnams    /'LARAMIE-FOX HILLS       ',
     1                'LOWER ARAPAHOE          ',
     2                'UPPER ARAPAHOE          ',
     3                'DENVER                  ',
     4                'LOWER DAWSON            ',
     5                'UPPER DAWSON            ',
     6                'UPPER DAWSON',
     7                'NA',
     8                'NA',
     9                'NA'/
      data aqshorts/  'LF',
     1                'LA',
     2                'AR',
     3                'DE',
     4                'LD',
     5                'DA',
     6                'LW',
     7                'NA',
     8                'NA',
     9                'NA'/
      data sectionmin    /1/
      data sectionmax    /36/
      data townshipmin   /1/
      data townshipmax   /9/
      data rangemin      /60/
      data rangemax      /69/

c _________________________________________________________
      if (debug_log) write(nlog,*) "arg4 debug: nlog",nlog
      if (debug_log) write(nlog,*) "arg4 debug: incli",incli
      if (debug_log) write(nlog,*) "arg4 debug: outcli",outcli
      if (debug_log) write(nlog,*) "arg4 debug: debug_cli",debug_cli
      if (debug_log) write(nlog,*) "arg4 debug: debug_log",debug_log
      if (debug_cli) write(outcli,*) "arg4 debug: nlog",nlog
      if (debug_cli) write(outcli,*) "arg4 debug: incli",incli
      if (debug_cli) write(outcli,*) "arg4 debug: outcli",outcli
      if (debug_cli) write(outcli,*) "arg4 debug: debug_cli",debug_cli
      if (debug_cli) write(outcli,*) "arg4 debug: debug_log",debug_log
c _________________________________________________________
c     get and use the command line arguments
      call commandlinesetup
c _________________________________________________________
c     if it exists, make sure tape74.dat (model output) gets renamed
      call tape74setup
c _________________________________________________________
c     create a log file
      call logfilesetup
      write(nlog,300) ver, vdate, rnns
      write(outcli,300) ver, vdate, rnns
 300  format(
     1    72('_'),//
     2    '        AUG4                       '/
     3    '        State of Colorado - Denver Basin Aquifer Models '//
     4    '        Version: ',a12,/,
     5    '        Last revision date: ',a12,//
     6    '        Run id set to ',a32,//
     7    72('_'))
c _________________________________________________________
c     read the "junk" file containing run parameters
      call readjunkfile
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
      write(outcli, 300) ver, vdate, rnns
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


c _________________________________________________________
c     make sure tape74.dat output file gets renamed before continuing
c 20 ON ERROR GOTO 4000
c 25 OPEN "I",#1,"TAPE74.DAT"
c 26 CLOSE#1:
c 27 PRINT "MODEL OUTPUT (TAPE74.DAT) HAS NOT BEEN RENAMED WITH RUN NUMBER"
c 28 INPUT "ENTER RUN NUMBER TO BE USED TO RENAME TAPE74.DAT  ";RNRN$
c 29 NAME "TAPE74.DAT" AS "TAPE74."+RNRN$
      subroutine tape74setup()
      IMPLICIT NONE
        !rename only if file tape74_base+tape74__ext exists
        !if it does, ask for the new extension
        !then rename the file
        include 'aug4_common2.inc'
        ! local variables
        character(len=48) :: oldfilename, newfilename
        character(len=24) :: newextension
        logical file_exists

        if (debug_cli) then
          write(outcli,*)"arg4 debug: tape74setup: start"
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: tape74setup: start"
        endif
        oldfilename = trim(tape74_base)//trim(tape74_ext)
        INQUIRE(FILE=trim(oldfilename), EXIST=file_exists)
        if (file_exists) then
          if (debug_cli) then
            write(outcli,*)"arg4 debug: tape74setup: found file ",
     1                     trim(oldfilename)
          endif
          if (debug_log) then
            write(nlog,*)"arg4 debug: tape74setup: found file ",
     1                   trim(oldfilename)
          endif
          write(outcli,*)
     1      'MODEL OUTPUT (',
     2      trim(oldfilename),
     3      ') NEEDS TO BE RENAMED.'
 200      write(outcli,*)
     1      'ENTER NEW FILE NAME EXTENSION (E.G. RUN NUMBER): '
          read (incli,*,err=210) newextension
          newfilename = trim(tape74_base)//'.'//trim(newextension)
          write(outcli,*) 'renaming ',trim(oldfilename),
     1      ' to ',trim(newfilename)
          call rename(trim(oldfilename),trim(newfilename))
          if (debug_cli) then
            write(outcli,*)"arg4 debug: tape74setup: renamed to ",
     1                     trim(newfilename)
          endif
          if (debug_log) then
            write(nlog,*)"arg4 debug: tape74setup: renamed to ",
     1                   trim(newfilename)
          endif
          return
 210      write(outcli,*)'Invalid response.'
          goto 200
        endif
 299    return
      end
c _________________________________________________________
c     subroutine 3000
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
      subroutine printStreamDepletion(flg3)
      IMPLICIT NONE
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
c _________________________________________________________
c     get and use command line args
      subroutine commandlinesetup()
        IMPLICIT NONE
      ! gfortran method to get the command line arg strings
      ! note: if needed there is a iargc function for f77 backward compat
        include 'aug4_common2.inc'
        include 'aug4_common3.inc'
        ! local variables
        integer iargcount
        integer command_line_len, status
        character(len=64) :: command_line
        character(len=32) :: arg, args
        dimension args(10)
        logical runid_commandline
        if (debug_cli) then
          write(outcli,*)"arg4 debug: commandlinesetup: start"
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: commandlinesetup: start"
        endif
        CALL GET_COMMAND(command_line, command_line_len, status)
        if (debug_cli) then
          write(outcli,*)"arg4 debug: ", "command_line = ",command_line
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: ", "command_line = ",command_line
        endif
        !write(outcli,*) "command_line_len = ",command_line_len
        !write(outcli,*) "status = ",status
        iargcount = 0
        do
          call get_command_argument(iargcount,arg)
          if (len_trim(arg) == 0) exit
          if (debug_cli) then
            write(outcli,*)"arg4 debug: ", "arg ",iargcount," = ",arg
          endif
          if (debug_log) then
            write(nlog,*)"arg4 debug: ", "arg ",iargcount," = ",arg
          endif
          ! use the first arg as the run id if the user provided one
          if (iargcount.eq.1) then
            runid_commandline = .TRUE.
            rnns = arg
          endif
          args(iargcount+1) = trim(arg)
          iargcount = iargcount + 1
        end do
        return
      end
c _________________________________________________________
c     set up the log file
      subroutine logfilesetup()
        IMPLICIT NONE
        include 'aug4_common2.inc'
        if (debug_cli) then
          write(outcli,*)"arg4 debug: logfilesetup: start"
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: logfilesetup: start"
        endif
        open(nlog,file=fnlog, status='unknown')
        return
      end
c _________________________________________________________
c     read the "junk" file containing run parameters
!     50 OPEN "I",#1,"C:\AUG3\JUNK."+RNN$
!     60 INPUT#1,NSP
!     70 LINE INPUT#1,SHORT$
!     80 LINE INPUT#1,DD1$
!     85 LINE INPUT#1,RN$
!     90 FOR X=1 TO NSP
!     100 INPUT#1,NTS(X),TSMULT(X),PERLEN(X)
!     110 NEXT X
!     120 CLOSE#1
!     121 IF RN$<>RNN$ THEN GOSUB 5000:GOTO 50
      subroutine readjunkfile()
      IMPLICIT NONE
        ! if the file exists, read it
        ! if not, call subroutine to create it
        ! then try again
        include 'aug4_common2.inc'
        include 'aug4_common3.inc'
        ! local variables
        character(len=48) :: junkfilename
        character(len=127) :: fileline
        logical file_exists, runid_match
        integer i

        if (debug_cli) then
          write(outcli,*)"arg4 debug: readjunkfile: start"
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: readjunkfile: start"
        endif
        junkfilename = trim(junk_base)//'.'//rnns
 100    INQUIRE(FILE=trim(junkfilename), EXIST=file_exists)
        if (file_exists) then
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: readjunkfile: reading ",trim(junkfilename)
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: readjunkfile: reading ",trim(junkfilename)
          endif
          ! read it
          open(njunk,file=trim(junkfilename), status='old')
          read(njunk,*)nsp
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: readjunkfile: nsp ",nsp
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: readjunkfile: nsp ",nsp
          endif
          read(njunk,*)shorts
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: readjunkfile: shorts ",shorts
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: readjunkfile: shorts ",shorts
          endif
          read(njunk,'(A48)')dd1s
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: readjunkfile: dd1s ",dd1s
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: readjunkfile: dd1s ",dd1s
          endif
          read(njunk,*)rns
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: readjunkfile: rns ",rns
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: readjunkfile: rns ",rns
          endif
          do i=1,nsp
            read(njunk,*)nts(i),tsmult(i),perlen(i)
            if (debug_cli) then
              write(outcli,*)
     1        "arg4 debug: readjunkfile: i ",i
              write(outcli,*)
     1        "arg4 debug: readjunkfile: nts(i), tsmult(i), perlen(i)"
              write(outcli,*)
     1        "arg4 debug: readjunkfile: ",nts(i),tsmult(i),perlen(i)
            endif
            if (debug_log) then
              write(nlog,*)
     1        "arg4 debug: readjunkfile: i ",i
              write(nlog,*)
     1        "arg4 debug: readjunkfile: nts(i), tsmult(i), perlen(i)"
              write(nlog,*)
     1        "arg4 debug: readjunkfile: ",nts(i),tsmult(i),perlen(i)
            endif
          end do
          close(njunk)
          ! then compare run ids
          if (trim(rnns).eq.trim(rns)) then
            if (debug_cli) then
              write(outcli,*)
     1        "arg4 debug: readjunkfile: rnns = rns",rnns,rns
            endif
            if (debug_log) then
              write(nlog,*)
     1        "arg4 debug: readjunkfile: rnns = rns",rnns,rns
            endif
            ! done
          else
            ! erase and recreate junk file
            if (debug_cli) then
              write(outcli,*)
     1        "arg4 debug: readjunkfile: rnns <> rns",rnns,rns
            endif
            if (debug_log) then
              write(nlog,*)
     1        "arg4 debug: readjunkfile: rnns <> rns",rnns,rns
            endif
            close(njunk)
            if (debug_cli) then
              write(outcli,*)
     1        "arg4 debug: readjunkfile: recreating ",trim(junkfilename)
            endif
            if (debug_log) then
              write(nlog,*)
     1        "arg4 debug: readjunkfile: recreating ",trim(junkfilename)
            endif
            call rename(trim(junkfilename),trim(junkfilename)//".old")
            call createjunkfile
            goto 100
          endif
        else
          ! create junk file
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: readjunkfile: creating ",trim(junkfilename)
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: readjunkfile: creating ",trim(junkfilename)
          endif
          call createjunkfile
          goto 100
        endif
        return
      end
c _________________________________________________________
c     create a "junk" file containing run parameters
c     subroutine 5000
!     5000 REM  *************** CREATE COMMON VARIABLES AND WRITE TO JUNK.DAT *******
!     5001 CLS:PRINT "FILES RESIDENT ON THE APPOLO ARE CONFIGURED AS FOLLOWS WITH RESPECT TO TIME:"
!     5002 PRINT
!     5003 PRINT "STRESS PERIOD   No. OF TIME STEPS   TSMULT   LENGTH (years)"
!     5004 PRINT "-------------   -----------------   ------   --------------"
!     5005 PRINT "     1                 20            1.01         100"
!     5006 PRINT "     2                 60            1.01         300"
!     5007 PRINT
!     5008 PRINT "If you wish to simulate 2 stress periods as described above, press RETURN only  otherwise enter the number of stress periods to be simulated  ";
!     5009 INPUT "";NSP$
!     5010 REM INPUT "ENTER MODEL NAME (Ex:  DE10) (enter S for program to suggest model) ";SHORT$
!     5011 REM IF SHORT$="s" OR SHORT$="S" THEN GOSUB 6000
!     5012 GOSUB 6000
!     5020 DD1$="C:\AUG3\"
!     5021 IF NSP$="" THEN NSP=2 ELSE NSP=VAL(NSP$):GOTO 5040
!     5022 NTS(1)=20:NTS(2)=60
!     5023 TSMULT(1)=1.01:TSMULT(2)=1.01
!     5024 PERLEN(1)=100:PERLEN(2)=300
!     5025 GOTO 5110
!     5040 CLS:PRINT "REMEMBER FILES ON APOLLO ARE CONFIGURED FOR 2 STRESS PERIODS.  YOU WILL HAVE TO CHANGE TAPE1.DAT ON THE APOLLO.":PRINT
!     5041 FOR X=1 TO NSP 
!     5060 PRINT "FOR STRESS PERIOD #";X
!     5070 REM INPUT "ENTER NUMBER OF TIME STEPS";NTS(X)
!     5080 REM INPUT "ENTER TIME STEP MULTIPLIER";TSMULT(X)
!     5090 INPUT "ENTER LENGTH OF STRESS PERIOD (years) ";PERLEN(X) 
!     5092 NTS(X)=PERLEN(X)/5
!     5093 TSMULT(X)=1.0001
!     5095 IF PERLEN(X)>300 THEN BEEP:CLS:PRINT "LENGTH OF TIME STEP CANNOT EXCEED 300 YEARS.  (press RETURN to abort) ...";:INPUT "";N$:SYSTEM
!     5100 NEXT X
!     5110 OPEN "O",#1,"C:\AUG3\JUNK."+RNN$
!     5120 PRINT#1,NSP
!     5130 PRINT#1,SHORT$
!     5140 PRINT#1,DD1$
!     5145 PRINT#1,RNN$
!     5150 FOR X=1 TO NSP
!     5160 PRINT#1,NTS(X);TSMULT(X);PERLEN(X)
!     5165 NEXT X
!     5170 CLOSE#1
!     5171 L=LEN(SHORT$)
!     5172 IF L=3 THEN SDIR$=MID$(SHORT$,1,2)+"0"+MID$(SHORT$,3,1) ELSE SDIR$=SHORT$
!     5173 OPEN "I",#1,"C:\AUG3\"+SDIR$+"\TAPE1.SAV"
!     5174 OPEN "O",#2,"C:\AUG3\"+SDIR$+"\TAPE1.DAT"
!     5175 FOR X=1 TO 2
!     5176 LINE INPUT#1,A$
!     5177 PRINT#2,A$
!     5178 NEXT X
!     5179 INPUT#1,K1,K2,K3,K4,K5
!     5180 PRINT#2,USING"##########";K1;K2;K3;NSP;K5
!     5181 LINE INPUT#1,A$
!     5182 IF MID$(A$,1,10)<>"3155760000" THEN PRINT#2,A$:GOTO 5181
!     5183 FOR X=1 TO NSP
!     5184 PRINT#2,USING"##########";PERLEN(X)*1440*365.25*60;NTS(X);:PRINT#2,USING"#####.####";TSMULT(X)
!     5185 NEXT X
!     5186 CLOSE#1:CLOSE#2
!     5187 RETURN
      subroutine createjunkfile()
        IMPLICIT NONE
        include 'aug4_common2.inc'
        include 'aug4_common3.inc'
        ! local variables
        character(len=48) :: junkfilename, userinput
        !select model parameters
        if (debug_cli) then
          write(outcli,*)"arg4 debug: createjunkfile: start"
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: createjunkfile: start"
        endif
        write(outcli,72)
 72     format(72('_'),/)  
        write(outcli,*)"FILES RESIDENT ON THE APOLLO ARE CONFIGURED "//
     1    "AS FOLLOWS WITH RESPECT TO TIME:"
        write(outcli,*)
        write(outcli,*)
     1    "STRESS PERIOD   No. OF TIME STEPS   TSMULT   LENGTH (years)"
        write(outcli,*)
     1    "-------------   -----------------   ------   --------------"
        write(outcli,*)
     1    "     1                 20            1.01         100"
        write(outcli,*)
     1    "     2                 60            1.01         300"
 98     write(outcli,*)
        write(outcli,*)
     1    "If you wish to simulate 2 stress periods as described above,"
     2    //" enter 0."
        write(outcli,*)
     1   "Otherwise enter the number of stress periods to be simulated:"
        read (incli,*,err=99) spselect
        if (debug_cli) then
          write(outcli,*)"arg4 debug: createjunkfile: spselect",spselect
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: createjunkfile: spselect",spselect
        endif
        if (spselect.ge.0.and.spselect.le.spcount) then
          ! assign model after choosing an aquifer and well location
          call selectaquifer
          call selectwelllocation
          call assignmodel
          ! create the 'junk' data file for the model
          if(spselect.eq.0) then
            nsp=2
          else
            nsp=spselect
          endif
          junkfilename = trim(junk_base)//'.'//rnns
          open(njunk,file=trim(junkfilename), status='unknown')
          close(njunk)
          return
        endif
        ! nsp input error
 99     write(outcli,*)"Invalid input.  Must enter an integer."
        goto 98
      end
c _______________________________________________________
c     select the aquifer and well location  
c     subroutine 6000
!     6000 REM ********* SUB TO SELECT MODEL **********************
c _______________________________________________________
c     select the aquifer
!     6010 CLS
!     6020 RESTORE 1100
!     6030 PRINT "SELECT AQUIFER:"
!     6040 FOR X=1 TO 6
!     6050 READ AQNAM$,AQSHORT$
!     6060 PRINT STR$(X);")  ";AQNAM$
!     6070 NEXT X
!     6080 INPUT "       enter appropriate line number  ";OSEL
!     6090 RESTORE 1100
!     6100 FOR X=1 TO OSEL
!     6110 READ AQNAM$,AQSHORT$
!     6120 NEXT X
!     6130 CLS
      subroutine selectaquifer()
        IMPLICIT NONE
        include 'aug4_common2.inc'
        include 'aug4_common3.inc'
        ! local variables
        integer i

        if (debug_cli) then
          write(outcli,*)"arg4 debug: selectaquifer: start"
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: selectaquifer: start"
        endif
 101    write(outcli,*)
        write(outcli,*)"Denver Basin Aquifers: "
        do i=1,aqcount
          write(outcli,1001)i,aqnams(i),aqshorts(i)
 1001     format (I2,2x,A18,' (',A2,')')
        end do
        write(outcli,*)"Select an aquifer: "
        read (incli,*,err=99) aqselect
        if (aqselect.gt.0.and.aqselect.le.aqcount) then
          osel = aqselect
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: selectmodelandlocation: aqselect = ",aqselect
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: selectmodelandlocation: aqselect = ",aqselect
          endif
          aqshort = aqshorts(aqselect)
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: selectmodelandlocation: aqshorts(aqselect) = ",
     2      aqshorts(aqselect)
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: selectmodelandlocation: aqshorts(aqselect) = ",
     2      aqshorts(aqselect)
          endif
          aqnam = aqnams(aqselect)
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: selectmodelandlocation: aqnams(aqselect) = ",
     2      aqnams(aqselect)
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: selectmodelandlocation: aqnams(aqselect) = ",
     2      aqnams(aqselect)
          endif
          return
        endif
 99     write(outcli,*)"Invalid selection."
        write(outcli,*)"Enter a number between 1 and ",aqcount
        goto 101
      end
c _______________________________________________________
c     select the well location  
!     6140 INPUT "ENTER LOCATION (example:  3,5N,64)  ";SCTN,TWP$,RNG
!     6150 GOSUB 3270
      subroutine selectwelllocation()
        IMPLICIT NONE
        include 'aug4_common2.inc'
        include 'aug4_common3.inc'
        ! local variables
        logical locationok
        integer itownship
        character ctownship*1

        if (debug_cli) then
          write(outcli,*)"arg4 debug: selectwelllocation: start"
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: selectwelllocation: start"
        endif
 101    write(outcli,*)
        write(outcli,*)"Enter the well location (e.g. 13,5N,64): "
        read (incli,*,err=98) section, township, range
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: selectmodelandlocation: aqnams(aqselect) = ",
     2      aqnams(aqselect)
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: selectmodelandlocation: aqnams(aqselect) = ",
     2      aqnams(aqselect)
          endif
        if (section.gt.sectionmin.and.section.le.sectionmax) then
          if (range.ge.rangemin.and.range.le.rangemax) then
            read(township,'(I1)')itownship
            !read(township,'(1x,A1)')ctownship
            !itownship = township[1:1]
            ctownship = township(2:2)
           if(itownship.ge.townshipmin.and.itownship.le.townshipmax)then
              return
            else
              write(outcli,*)"Invalid range entered = ", range
              goto 99
            endif
          else
            write(outcli,*)"Invalid range entered = ", range
            goto 99
          endif
        else
          write(outcli,*)"Invalid section entered = ", section
          goto 99
        endif
 98     write(outcli,*)"Error reading location entry."
 99     write(outcli,*)
     1  "Enter the well location as a section, township and range "//
     2  "separated by commas."
        write(outcli,*)
     1  "For example: 13, 5N, 64"
        write(outcli,*)
     1  "The section must be an integer between 1 and 36"
        write(outcli,*)
     1  "The township must be an integer like 5 followed by N or S"
        write(outcli,*)
     1  "The range must be an integer like 64 (W is implied)"
        goto 101
      end
c _______________________________________________________
c     select a model based on the aquifer and well location
!     6160 ON OSEL GOTO 6170,6250,6270,6330,6360,6370
!     6170 IF I>95 AND J<53 THEN SHORT$="LF8"
!     6180 IF I<57 AND J<21 THEN SHORT$="LF6":GOTO 6380
!     6190 IF I<33 AND J<66 THEN SHORT$="LF1":GOTO 6380
!     6200 IF I<24 THEN GOTO 6380
!     6205 IF I<44 AND J>65 THEN SHORT$="LF2"
!     6210 IF I<87 AND J<39 THEN SHORT$="LF4"
!     6220 IF I>66 AND J>55 THEN SHORT$="LF3"
!     6240 GOTO 6380
!     6250 SHORT$="LA1"
!     6260 GOTO 6380
!     6270 IF I<66 AND J<38 THEN SHORT$="AR1":GOTO 6380
!     6280 IF I<72 AND J>37 THEN SHORT$="AR2":GOTO 6380
!     6290 IF I>83 AND J<47 THEN SHORT$="AR9":GOTO 6380
!     6300 IF J<38 THEN SHORT$="AR3":GOTO 6380
!     6310 IF I>77 THEN SHORT$="AR4"
!     6320 GOTO 6380
!     6330 IF I>82 THEN SHORT$="DE10":GOTO 6380
!     6340 IF J>35 THEN SHORT$="DE9" ELSE SHORT$="DE8"
!     6350 GOTO 6380
!     6360 SHORT$="LD2":GOTO 6380
!     6370 IF I<83 THEN SHORT$="DA1" ELSE SHORT$="DA2"
!     6380 IF SHORT$="S" OR SHORT$="s" THEN PRINT "ADEQUATE MODEL NOT AVAILABLE (press RETURN to continue...";:INPUT "";NUT$:SYSTEM
!     6390 PRINT SHORT$;" has been selected  (press RETURN to continue) ";:INPUT "";NUT$
!     6400 RETURN!
      subroutine assignmodel()
        IMPLICIT NONE
        include 'aug4_common2.inc'
        include 'aug4_common3.inc'
        ! local variables

        if (debug_cli) then
          write(outcli,*)"arg4 debug: assignmodel: start"
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: assignmodel: start"
        endif

        return
      end
c _______________________________________________________
c     check the well location
c     subroutine 3270
!     3270 REM   SUB TO GET LF COORDINATES FROM SEC.,T, and R (RETURNS I,J FROM SCTN,TWP$,RNG) (OTHER VARIABLES USED=AJ,NSI$,L,X)
!     3280 J=(70-RNG)*6-2
!     3290 RESTORE 3340
!     3300 FOR X=1 TO SCTN
!     3310 READ AJ
!     3320 NEXT X
!     3330 J=J+AJ
!     3340 DATA 6,5,4,3,2,1,1,2,3,4,5,6,6,5,4,3,2,1,1,2,3,4,5,6,6,5,4,3,2,1,1,2,3,4,5,6
!     3350 L=LEN(TWP$)
!     3360 NSI$=MID$(TWP$,L)
!     3370 I=VAL(MID$(TWP$,1,(L-1)))
!     3380 IF NSI$="N" GOTO 3450
!     3390 IF NSI$="n" GOTO 3450
!     3400 IF NSI$="S" GOTO 3430
!     3410 IF NSI$="s" GOTO 3430
!     3420 GOTO 3530
!     3430 I=I*6+24+INT(SCTN/6-.05)
!     3440 GOTO 3460
!     3450 I=(5-I)*6+INT(SCTN/6-.05)
!     3460 IF I<1 GOTO 3520
!     3470 IF I>120 GOTO 3520
!     3480 IF J<1 GOTO 3520
!     3490 IF J>84 GOTO 3520
!     3500 CODE%=(I-1)*84+J
!     3510 RETURN
!     3520 CLS
!     3530 PRINT "YOU HAVE SELECTED A LOCATION (";SCTN;TWP$;RNG;") OUTSIDE THE BASIN.  TRY AGAIN!!":END
!     3540 INPUT "INPUT SECTION, TOWNSHIP, and RANGE                                              (example: Sec. 4, T.2N., R.63W. would be input as 4,2N,63 )     ";SCTN,TWP$,RNG
!     3550 GOTO 3280
!     3560 RETURN
      subroutine checkwelllocation()
        IMPLICIT NONE
        include 'aug4_common2.inc'
        include 'aug4_common3.inc'
        ! local variables

      end
