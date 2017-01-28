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

      data ver        /' 0.73'/
      data vdate      /'12/09/2016  '/
      data fnlog      /'aug4.log'/
      data nlog       /13/
      data outcli     /6/
      data incli      /5/
      data t9unit1    /14/
      data t9unit2    /15/
      data t1unit1    /16/
      data t1unit2    /17/
      data debug_cli  /.FALSE./ !turn this off in production
      data debug_log  /.FALSE./ !turn this off in production
      data versioncli /.FALSE./ ! this can get set if --version or -v is on the command line
      data helpcli    /.FALSE./ ! this can get set if --help or -h is on the command line
      !     5020 DD1$="C:\AUG3\"
c      data dirsep     /'/'/
c      data dd1s       /'/home/jim/workspaces/dwr_aug3_scripts/'/
      data dirsep     /'\'/
      data dd1s       /'c:\aug3\'/
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
!     5022 NTS(1)=20:NTS(2)=60
!     5023 TSMULT(1)=1.01:TSMULT(2)=1.01
!     5024 PERLEN(1)=100:PERLEN(2)=300
c _________________________________________________________
c     set the simulation period count default/max count
      data spselect  /2/
      data spcount   /10/
      data nsp       /2/
      data xnsp      /2/
      data xnspmin   /1/
      data xnspmax   /5/
      data nspcli    /.FALSE./
c _________________________________________________________
c     set the time step length (years)
      data itslen    /1/
      data itslmin   /1/
      data itslmax   /10/
      data tslen     /1,1,1,1,1,1,1,1,1,1/
c     data tslen     /5,5,5,5,5,5,5,5,5,5/
      data tslencli  /.FALSE./
c _________________________________________________________
c     set the number of time steps in each simulation period
      data nts       /100,300,0,0,0,0,0,0,0,0/
c      data nts       /20,60,0,0,0,0,0,0,0,0/
      data ntscli    /.FALSE./
c _________________________________________________________
c     set the length (years) of each simulation period
c       must = tslen*nts
      data perlen    /100,300,0,0,0,0,0,0,0,0/
      data perlencli /.FALSE./
c _________________________________________________________
c     set the time step multiplier default / cli flag
      data xtsmult   /1.0/
      data xtsmmin   /0.9/
      data xtsmmax   /1.1/
      data tsmult    /1.0,1.0,1.0,1.0,1.0,
     0                1.0,1.0,1.0,1.0,1.0/
      data tsmultcli /.FALSE./
c _________________________________________________________
c     set the aquifer names
      data aqselect  /1/ !default selection
      data aqcount   /6/ !modeled aquifer count
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
      data aqshorts  /'LF',
     1                'LA',
     2                'AR',
     3                'DE',
     4                'LD',
     5                'DA',
     6                'LW',
     7                'NA',
     8                'NA',
     9                'NA'/
c _________________________________________________________
c     set some parameter constraints
      data sectionmin    /1/
      data sectionmax    /36/
      data townshipmin   /1/
      data townshipmax   /16/
      data rangemin      /56/
      data rangemax      /70/
c _________________________________________________________
c     section column index
      data sectioncolumn /6,5,4,3,2,1,
     1                    1,2,3,4,5,6,
     2                    6,5,4,3,2,1,
     3                    1,2,3,4,5,6,
     4                    6,5,4,3,2,1,
     5                    1,2,3,4,5,6/
c _________________________________________________________
c     tape9 data
!     310 RESTORE 330
!     320 READ TN$,NA$,A1$,A2$
!     330 DATA "9","WELL","  YIELD (gpm)"," + for pumping     WELL LOCATION"
      data tns   /"9"/
      data nas   /"WELL"/
      data a1s   /"  YIELD (gpm)"/
      data a2s   /" + for pumping     WELL LOCATION"/
!     2480 DATA "AR9",7,MONUMENT,KETTLE,COTTONWOOD,SHOOKS RUN,SAND,JIMMY CAMP,BLACK SQUIRREL,78,17,37,36
!     2490 DATA DE8,14,WEST PLUM,PLUM,LITTLE DRY,PLATTE,BEAR,CLEAR CREEK,CHERRY CREEK,SAND CREEK,1st CREEK,2nd CREEK,3rd CREEK,BEEBE SEEP,BOX ELDER,KIOWA,29,2,60,40
!     2495 DATA DE11,14,WEST PLUM,PLUM,LITTLE DRY,PLATTE,BEAR,CLEAR CREEK,CHERRY CREEK,SAND CREEK,1st CREEK,2nd CREEK,3rd CREEK,BEEBE SEEP,BOX ELDER,KIOWA,29,2,60,40
!     2500 DATA LD2,8,PLUM,LITTLE DRY,CHERRY CREEK,COAL CREEK,RUNNING CREEK,KIOWA,WOLF,COMANCHE,54,13,49,36
!     2510 DATA DE9,7,SAND CREEK,1st CREEK,3rd CREEK,BOX ELDER,LOST CREEK,KIOWA COMANCHE & WOLF,BIJOU,30,30,59,31
!     2520 DATA DE10,9,BIJOU,BIG SANDY,MONUMENT,COTTONWOOD,SHOOKS RUN,SAND CREEK,BLACK SQUIRREL,STEELS FORK,HORSE CREEK,77,15,33,48
!     2530 DATA CW1,10,MONUMENT,EAST PLUM - w,EAST PLUM - e,WEST CHERRY,EAST CHERRY,CHERRY,KIOWA,KETTLE,SAND,BIG SANDY,51,16,49,36
!     2540 DATA LA1,4,BIG DRY,S. PLATTE,BEEBE & BOX ELDER,LOST CREEK,19,2,25,47
!     2550 DATA LD3,8,PLUM,LITTLE DRY,CHERRY CREEK,COAL CREEK,RUNNING CREEK,KIOWA,WOLF,COMANCHE,54,13,49,36
!     2560 DATA AR1,7,JARRE,PLUM,BEAR,S. PLATTE,BEEBE,BOX ELDER,LOST CREEK,20,2,55,45
!     2570 DATA LF8,10,W. MONUMENT,DOUGLAS,MONUMENT,SHOOKS RUN,SAND,JIMMY CAMP,WILLIAMS,CHICO,BLACK SQUIRREL,POND,90,18,30,41
!     2580 DATA DA1,5,EAST PLUM,COAL CREEK,KIOWA CREEK,CHERRY CREEK,RUNNING CREEK,58,16,30,29
!     2590 DATA DA2,11,MONUMENT,EAST PLUM - w,EAST PLUM - e,WEST CHERRY,EAST CHERRY,CHERRY,KIOWA,KETTLE,SAND,BIG SANDY,BLACK SQUIRREL,78,17,25,30
!     2591 DATA "LF4",3,"BEAR","PLATTE","WEST PLUM",54,6,40,40
!     2592 DATA "LF6",4,"PLATTE","BOULDER CREEK","CLEAR CREEK","BEAR CREEK",1,1,57,28
!     2593 DATA "LF1",4,"PLATTE","BOULDER CREEK","BOX ELDER","KIOWA-BIJOU",1,11,60,40
!     2594 DATA "AR3",8,"MONUMENT","KETTLE","COTTONWOOD","SHOOKS RUN","SAND","JIMMY CAMP","BLACK SQUIRREL","WEST PLUM",60,9,55,38
!     2595 DATA "AR4",8,"SAND","JIMMY CAMP","BLACK SQUIRREL","POND","STEELS FORK","HORSE CREEK","RUSH CREEK","BIG SANDY",72,29,43,45
!     2596 DATA "AR2",5,"BEEBE","BOX ELDER","LOST CREEK","KIOWA COMANCHE WOLF","BIJOU",20,29,58,41
!     2600 DATA "LF2",2,"KIOWA-BIJOU","SAN ARROYO",18,47,34,34
!     2610 DATA "LF3",8,"POND","STEELS FORK","HORSE","RUSH","BIG SANDY","LITTLE HORSE","ADOBE","MUSTANG",60,50,60,35
!     2611 DATA "AR10",8,"MONUMENT","KETTLE","COTTONWOOD","SHOOKS RUN","SAND","JIMMY CAMP","BLACK SQUIRREL","WEST PLUM",60,9,55,38
      data modelcountmax /22/
      data rivercountmax /15/
      data modelshorts / "AR9",
     1                   "DE8",
     2                  "DE11",
     3                   "LD2",
     4                   "DE9",
     5                  "DE10",
     6                   "CW1",
     7                   "LA1",
     8                   "LD3",
     9                   "AR1",
     z                   "LF8",
     1                   "DA1",
     2                   "DA2",
     3                   "LF4",
     4                   "LF6",
     5                   "LF1",
     6                   "AR3",
     7                   "AR4",
     8                   "AR2",
     9                   "LF2",
     z                   "LF3",
     1                  "AR10"/
      data tnrs        / 7,
     1                  14,
     2                  14,
     3                   8,
     4                   7,
     5                   9,
     6                  10,
     7                   4,
     8                   8,
     9                   7,
     z                  10,
     1                   5,
     2                  11,
     3                   3,
     4                   4,
     5                   4,
     6                   8,
     7                   8,
     8                   5,
     9                   2,
     z                   8,
     1                   8/
      data imins       /78,
     1                  29,
     2                  29,
     3                  54,
     4                  30,
     5                  77,
     6                  51,
     7                  19,
     8                  54,
     9                  20,
     z                  90,
     1                  58,
     2                  78,
     3                  54,
     4                   1,
     5                   1,
     6                  60,
     7                  72,
     8                  20,
     9                  18,
     z                  60,
     1                  60/
      data jmins       /17,
     1                   2,
     2                   2,
     3                  13,
     4                  30,
     5                  15,
     6                  16,
     7                   2,
     8                  13,
     9                   2,
     z                  18,
     1                  16,
     2                  17,
     3                   6,
     4                   1,
     5                  11,
     6                   9,
     7                  29,
     8                  29,
     9                  47,
     z                  50,
     1                   9/
      data nrows       /37,
     1                  60,
     2                  60,
     3                  49,
     4                  59,
     5                  33,
     6                  49,
     7                  25,
     8                  49,
     9                  55,
     z                  30,
     1                  30,
     2                  25,
     3                  40,
     4                  57,
     5                  60,
     6                  55,
     7                  43,
     8                  58,
     9                  34,
     z                  60,
     1                  55/
      data ncols       /36,
     1                  40,
     2                  40,
     3                  36,
     4                  31,
     5                  48,
     6                  36,
     7                  47,
     8                  36,
     9                  45,
     z                  41,
     1                  29,
     2                  30,
     3                  40,
     4                  28,
     5                  40,
     6                  38,
     7                  45,
     8                  41,
     9                  34,
     z                  35,
     1                  38/

      data rivnames /"MONUMENT","KETTLE","COTTONWOOD","SHOOKS RUN",
     1 "SAND","JIMMY CAMP","BLACK SQUIRREL","","","","","","","","",
     2"WEST PLUM","PLUM","LITTLE DRY","PLATTE","BEAR","CLEAR CREEK",
     2"CHERRY CREEK","SAND CREEK","1st CREEK","2nd CREEK","3rd CREEK",
     2"BEEBE SEEP","BOX ELDER","KIOWA","",
     3"WEST PLUM","PLUM","LITTLE DRY","PLATTE","BEAR","CLEAR CREEK",
     3"CHERRY CREEK","SAND CREEK","1st CREEK","2nd CREEK","3rd CREEK",
     3"BEEBE SEEP","BOX ELDER","KIOWA","",
     4"PLUM","LITTLE DRY","CHERRY CREEK","COAL CREEK","RUNNING CREEK",
     4"KIOWA","WOLF","COMANCHE","","","","","","","",
     5"SAND CREEK","1st CREEK","3rd CREEK","BOX ELDER","LOST CREEK",
     5"KIOWA COMANCHE & WOLF","BIJOU","","","","","","","","",
     6"BIJOU","BIG SANDY","MONUMENT","COTTONWOOD","SHOOKS RUN",
     6"SAND CREEK","BLACK SQUIRREL","STEELS FORK","HORSE CREEK",
     6"","","","","","",
     7"MONUMENT","EAST PLUM - w","EAST PLUM - e","WEST CHERRY",
     7"EAST CHERRY","CHERRY","KIOWA","KETTLE","SAND","BIG SANDY",
     7"","","","","",
     8"BIG DRY","S. PLATTE","BEEBE & BOX ELDER","LOST CREEK",
     8"","","","","","","","","","","",
     9"PLUM","LITTLE DRY","CHERRY CREEK","COAL CREEK","RUNNING CREEK",
     9"KIOWA","WOLF","COMANCHE","","","","","","","",
     z"JARRE","PLUM","BEAR","S. PLATTE","BEEBE","BOX ELDER",
     z"LOST CREEK","","","","","","","","",
     1"W. MONUMENT","DOUGLAS","MONUMENT","SHOOKS RUN","SAND",
     1"JIMMY CAMP","WILLIAMS","CHICO","BLACK SQUIRREL","POND",
     1"","","","","",
     2"EAST PLUM","COAL CREEK","KIOWA CREEK","CHERRY CREEK",
     2"RUNNING CREEK","","","","","","","","","","",
     3"MONUMENT","EAST PLUM - w","EAST PLUM - e","WEST CHERRY",
     3"EAST CHERRY","CHERRY","KIOWA","KETTLE","SAND","BIG SANDY",
     3"BLACK SQUIRREL","","","","",
     4"BEAR","PLATTE","WEST PLUM","","","","","","","","","","","","",
     5"PLATTE","BOULDER CREEK","CLEAR CREEK","BEAR CREEK",
     5"","","","","","","","","","","",
     6"PLATTE","BOULDER CREEK","BOX ELDER","KIOWA-BIJOU",
     6"","","","","","","","","","","",
     7"MONUMENT","KETTLE","COTTONWOOD","SHOOKS RUN","SAND","JIMMY CAMP",
     7"BLACK SQUIRREL","WEST PLUM","","","","","","","",
     8"SAND","JIMMY CAMP","BLACK SQUIRREL","POND","STEELS FORK",
     8"HORSE CREEK","RUSH CREEK","BIG SANDY","","","","","","","",
     9"BEEBE","BOX ELDER","LOST CREEK","KIOWA COMANCHE WOLF","BIJOU",
     9"","","","","","","","","","",
     z"KIOWA-BIJOU","SAN ARROYO","","","","","","","","","","","","","",
     1"POND","STEELS FORK","HORSE","RUSH","BIG SANDY","LITTLE HORSE",
     1"ADOBE","MUSTANG","","","","","","","",
     2"MONUMENT","KETTLE","COTTONWOOD","SHOOKS RUN","SAND","JIMMY CAMP",
     2"BLACK SQUIRREL","WEST PLUM","","","","","","",""/
      !write(outcli,*) "arg4 debug: rivnames(1,1)",rivnames(1,1)
      !write(outcli,*) "arg4 debug: rivnames(3,6)",rivnames(3,6)
      !write(outcli,*) "arg4 debug: rivnames(2,18)",rivnames(2,18)
      !write(outcli,*) "arg4 debug: rivnames(1,22)",rivnames(1,22)
      !write(outcli,*) "arg4 debug: rivnames(15,22)",rivnames(15,22)
      !stop
c _________________________________________________________
c     debugging output of flags and unit numbers
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
      if (versioncli) then
        write(outcli,300) ver, vdate, rnns
        stop
      endif
      if (helpcli) then
        write(outcli,400)
 400    format(
     1    72('_'),//
     2    '        AUG4'/
     3    '        help message line 1'/
     4    '        help message line 2'/
     5    '        help message line 3'//
     6    72('_'))
        stop
      endif
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
     4    '        Version: ',a5,/,
     5    '        Last revision date: ',a12,//
     6    '        Run id set to ',a8,//
     7    72('_'))
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
!     130 RESTORE 1100
!     140 READ AQNAM$,TEST$
!     150 IF TEST$<>MID$(SHORT$,1,2) THEN GOTO 140
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
!     230 CLS
!     240 REM PRINT "SELECT OPTION:";TAB(2);"1) CODE TAPE9.DAT (WELL LIST)";TAB(2);"2) PRINT REPORT";TAB(2);"3) PLOT GRID MAP";TAB(2);"4) PLOT GRID MAP WITH WELL LOCATION(S)";TAB(2);"5) PLOT STREAM DEPLETIONS";TAB(2);"6) STOP"
!     241 PRINT "SELECT OPTION:";TAB(2);"1) CODE TAPE9.DAT (WELL LIST)";TAB(2);"6) STOP"
!     250 INPUT "       enter appropriate line number  ";OSELECT
!     260 IF OSELECT=6 THEN SYSTEM
!     261 IF OSELECT=1 THEN GOTO 280
!     262 IF OSELECT=2 THEN GOTO 800
!     263 IF OSELECT=3 OR OSELECT=4 THEN CHAIN "C:AUG3MOD1.BAS"
!     264 IF OSELECT=5 THEN CHAIN "C:AUG3MOD2.BAS"
!     270 GOTO 50
      ioptio = 0
      iback = 0
 100  if(ioptio.eq.0) then
        iback = 1
        Write(outcli,110)
        call flush(6)
 110    format(/,
     1           ' Option? ',
     2        //,'   [1] : CODE TAPE9.DAT (WELL LIST)',
     3         /,'   [6] : STOP')
        write(outcli,*) ' '
        read (incli,*,err=165) ioptio
      endif
c _________________________________________________________
c     user selected options from menu
      select case (ioptio)
        case (1) ! build tape9 input file
          goto 130
        case (2) ! unused
          goto 140
        case (3) ! unused
          goto 150
        case (4) ! unused
          goto 160
        case (5) ! unused
          goto 160
        case (6) ! exit aug4
          goto 170
        case default ! invalid selection, try again
          goto 165
      end select

c ______________________________________________________________________
c     Option 1 - build tape9 input file
 130  continue
      if (debug_cli) then
        write(outcli,*)"arg4 debug: option 1 selected - tape9 build"
      endif
      if (debug_log) then
        write(nlog,*)"arg4 debug: option 1 selected - tape9 build"
      endif
      call createtape9
      goto 166
c ______________________________________________________________________
c     Option Two
 140  continue
      if (debug_cli) then
        write(outcli,*)"arg4 debug: option 2 selected - "
      endif
      if (debug_log) then
        write(nlog,*)"arg4 debug: option 2 selected - "
      endif
c      call optiontwo
      goto 166
c ______________________________________________________________________
c     Option Three
 150  continue
      if (debug_cli) then
        write(outcli,*)"arg4 debug: option 3 selected - "
      endif
      if (debug_log) then
        write(nlog,*)"arg4 debug: option 3 selected - "
      endif
c      call optionthree
      goto 166
c ______________________________________________________________________
c     Option Four
 160  continue
      if (debug_cli) then
        write(outcli,*)"arg4 debug: option 4 selected - version info"
      endif
      if (debug_log) then
        write(nlog,*)"arg4 debug: option 4 selected - version info"
      endif
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
 180  format(/,'  AUG4 exiting; Log file created: ',a24,/) 
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
        integer iargcount, i
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
          !and if it is not a command like --version or -v
          if (arg(1:1).eq.'-') then
            ! check for special one token commands
            ! these stop the program and just print out data
            if (trim(arg).eq.'--version') then
              versioncli = .TRUE.
              ! stop reading args and return - i.e. exit subroutine
              return
            endif
            if (trim(arg).eq.'-v') then
              versioncli = .TRUE.
              ! stop reading args and return - i.e. exit subroutine
              return
            endif
            if (trim(arg).eq.'--help') then
              helpcli = .TRUE.
              ! stop reading args and return - i.e. exit subroutine
              return
            endif
            if (trim(arg).eq.'-h') then
              helpcli = .TRUE.
              ! stop reading args and return - i.e. exit subroutine
              return
            endif
            ! check for args with assigned values,
            !   read and then move to next one
            !
            ! set the default time step multiplier
            if (arg(1:9)).eq.'--tsmult=') then
              read(arg(10:len(arg)),'(*)')xtsmult
              if(xtsmult.ge.xtsmmin.and.xtsmult.le.xtsmmax) then
                tsmultcli = .TRUE.
                do i=1,10
                  tsmult(i)=xtsmult
                end do
              endif
            endif
            ! set the default number of simulation periods
            ! note that nsp SHOULD be one of the args
            if (arg(1:6)).eq.'--nsp=') then
              read(arg(7:len(arg)),'(*)')insp
              if(insp.ge.inspmin.and.insp.le.inspmax) then
                nsp = insp
                nspcli = .TRUE.
              endif
            endif
            ! set the default time step length (years)
            ! note that nsp MUST be one of the args
            if (arg(1:8)).eq.'--tslen=') then
              read(arg(9:len(arg)),'(*)')itslen
              if(itslen.ge.itslmin.and.itslen.le.itslmax) then
                tslencli = .TRUE.
                do i=1,10
                  tslen(i)=itslen
                end do
              endif
            endif
            ! set the length (years) of each simulation period
            ! there should be multiple (nsp), comma delimited
            ! record the whole string and parse it later
            ! (why? because nsp might not have been read, yet)
            if (arg(1:9)).eq.'--perlen=') then
              cperlen=arg(10:len(arg))
              perlencli = .TRUE.
            endif
            ! set the number of timesteps of each simulation period
            ! there should be multiple (nsp), comma delimited
            ! record the whole string and parse it later
            ! (why? because nsp might not have been read, yet)
            if (arg(1:6)).eq.'--nts=') then
              cnts=arg(7:len(arg))
              ntscli = .TRUE.
            endif
          else
            ! assume it is a runid
            ! note if there are multiple args like this, the last one
            !   becomes the runid
            runid_commandline = .TRUE.
            rnns = arg
          endif
          args(iargcount+1) = trim(arg)
          iargcount = iargcount + 1
        end do
c       parse the perlen and nts arg strings
        if(ntscli)then
          do i=1,nsp
            read(cnts,'(*)',err=1111)ints
            nts(i)=ints
 1111       perlen(i)=nts(i)*tslen(i)
          end do
        else
          do i=1,nsp
            read(cperlen,'(*)',err=1112)iperlen
            perlen(i)=iperlen
 1112       nts(i)=perlen(i)/tslen(i)
          end do
        endif
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
        character(len=48) :: junkfilename, trimmed
        character(len=128) :: fileline
        logical file_exists, runid_match
        integer i

        if (debug_cli) then
          write(outcli,*)"arg4 debug: readjunkfile: start"
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: readjunkfile: start"
        endif
        junkfilename = trim(junk_base)//'.'//trim(rnns)
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
          read(njunk,'(A128)')fileline
          trimmed=trim(fileline)
          shorts=trimmed(1:2)
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: readjunkfile: trimmed,shorts ",trimmed,shorts
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: readjunkfile: trimmed,shorts ",trimmed,shorts
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
          ! done reading junk file
          close(njunk)
          ! compare the given run id against the one in the junk file
          if (trim(rnns).eq.trim(rns)) then
            ! they match, continue processing the junk file data
            if (debug_cli) then
              write(outcli,*)
     1        "arg4 debug: readjunkfile: match: rnns, rns = ",rnns,rns
            endif
            if (debug_log) then
              write(nlog,*)
     1        "arg4 debug: readjunkfile: match: rnns, rns = ",rnns,rns
            endif
            ! get the full aquifer name of the short aquifer code found in the file
            do i=1,aqcount
              if(shorts.eq.aqshorts(i)) then
                aqshort=aqshorts(i)
                aqnam=aqnams(i)
                if (debug_cli) then
                  write(outcli,*)
     1            "arg4 debug: readjunkfile: aqshort,aqname ",
     2            aqshort," ",aqnam
                endif
                if (debug_log) then
                  write(nlog,*)
     1            "arg4 debug: readjunkfile: aqshort,aqname ",
     2            aqshort," ",aqnam
                endif
                ! 
                return
              endif
            end do
            ! error getting here, no aq match, so rebuild it
            if (debug_cli) then
              write(outcli,*)
     1        "arg4 debug: readjunkfile: aq mismatch, shorts = ",shorts
            endif
            if (debug_log) then
              write(nlog,*)
     1        "arg4 debug: readjunkfile: aq mismatch, shorts = ",shorts
            endif
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
            !close(njunk)
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
        integer iperlen, sp, mdlidlen
        character(len=4) :: subdirname
        character(len=24) :: filename
        character(len=96) :: fullfilename
        character(len=128) :: fileline

        !select model parameters
        if (debug_cli) then
          write(outcli,*)"arg4 debug: createjunkfile: start"
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: createjunkfile: start"
        endif
        !get the model parameters and then choose a model
        call selectstressperiods
        call selectaquifer
        call selectwelllocation
        call assignmodel
        ! set up the data
!     5021 IF NSP$="" THEN NSP=2 ELSE NSP=VAL(NSP$):GOTO 5040
        if(spselect.eq.0) then
!     5022 NTS(1)=20:NTS(2)=60
!     5023 TSMULT(1)=1.01:TSMULT(2)=1.01
!     5024 PERLEN(1)=100:PERLEN(2)=300
          ! defaults (but not these per DWR instructions)
          !   are initialized above in DATA statements,
          ! note: they should have been RESET by command line args
          !    in the DOS batch file to match the particular AUG3 application!!!!
        else
          ! user enters the simulation period parameters
          nsp=spselect
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
          write(outcli,*)
     1   "REMEMBER FILES ARE CONFIGURED FOR 2 STRESS PERIODS."
c     1   "REMEMBER FILES ON APOLLO ARE CONFIGURED FOR 2 STRESS PERIODS."
          write(outcli,*)
     1    "  YOU WILL HAVE TO CHANGE TAPE1.DAT."
c     1    "  YOU WILL HAVE TO CHANGE TAPE1.DAT ON THE APOLLO."
          !loop through the nsp stress periods and get the stress period lengths
          do sp=1,nsp
 98         write(outcli,1001)sp
 1001      format("For stress period ",I2,", enter the length (years):")
            read(incli,*,err=99)iperlen
            if(iperlen.gt.300) then
              write(outcli,*)
     1        "Invalid input.  Year count can not exceed 300."
              goto 98
             endif
            if(iperlen.eq.0) then
              write(outcli,*)
     1        "Invalid input.  Year count can not equal 0."
              goto 98
            endif
            perlen(sp) = iperlen
            nts(sp) = perlen(sp) / tslen(sp) ! note this is integer division
c            tsmult(sp) = is already set
            goto 100
            ! iperlen input error
 99         write(outcli,*)
     1      "Invalid input.  Year count must be an integer."
            goto 98
 100        continue   
          end do
        endif
        ! create the 'junk' data file for the model
!     5110 OPEN "O",#1,"C:\AUG3\JUNK."+RNN$
!     5120 PRINT#1,NSP
!     5130 PRINT#1,SHORT$
!     5140 PRINT#1,DD1$
!     5145 PRINT#1,RNN$
!     5150 FOR X=1 TO NSP
!     5160 PRINT#1,NTS(X);TSMULT(X);PERLEN(X)
!     5165 NEXT X
!     5170 CLOSE#1
        junkfilename = trim(junk_base)//'.'//trim(rnns)
        open(njunk,file=trim(junkfilename), status='unknown')
        write(njunk,*)nsp
        write(njunk,*)modelshort
        write(njunk,*)dd1s
        write(njunk,*)rnns
        do sp=1,nsp
          write(njunk,*)nts(sp),tsmult(sp),perlen(sp)
        end do
        close(njunk)
!     make a copy of TAPE1.DAT
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
        mdlidlen = len(modelshort)
        select case (mdlidlen)
          case (3)
            subdirname = modelshort(1:2)//"0"//modelshort(3:3)
          case default
            subdirname = modelshort
        end select
        filename = "TAPE1.SAV"
        fullfilename = trim(dd1s)//subdirname//dirsep//trim(filename)
        open(t1unit1,file=trim(fullfilename), status='old')
        filename = "TAPE1.DAT"
        fullfilename = trim(dd1s)//subdirname//dirsep//trim(filename)
        open(t1unit2,file=trim(fullfilename), status='unknown')
        do
          read(t1unit1,'(A128)',end=500,err=500)fileline
          write(t1unit2,*,err=500)trim(fileline)
        end do
 500    continue
        close(t1unit1)
        close(t1unit2)
        return
      end
c _______________________________________________________
c     select the number of stress time periods, spselect  
!     5001 CLS:PRINT "FILES RESIDENT ON THE APPOLO ARE CONFIGURED AS FOLLOWS WITH RESPECT TO TIME:"
!     5002 PRINT
!     5003 PRINT "STRESS PERIOD   No. OF TIME STEPS   TSMULT   LENGTH (years)"
!     5004 PRINT "-------------   -----------------   ------   --------------"
!     5005 PRINT "     1                 20            1.01         100"
!     5006 PRINT "     2                 60            1.01         300"
!     5007 PRINT
!     5008 PRINT "If you wish to simulate 2 stress periods as described above, press RETURN only  otherwise enter the number of stress periods to be simulated  ";
!     5009 INPUT "";NSP$
      subroutine selectstressperiods()
        IMPLICIT NONE
        include 'aug4_common2.inc'
        include 'aug4_common3.inc'
        ! local variables

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
        integer i, lents
        logical locationok
        character(len=127) :: readline
        character(len=24) :: rawtownship, trimtownship

        if (debug_cli) then
          write(outcli,*)"arg4 debug: selectwelllocation: start"
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: selectwelllocation: start"
        endif
 101    write(outcli,*)
        write(outcli,*)"Enter the well location (e.g. 13,5N,64): "
        !read (incli,'(A127)',err=98) readline
        !read (readline,*,err=98) section, township, range
        read (incli,*,err=98) section, rawtownship, range
        township = adjustl(rawtownship)
        lents = len(trim(township))
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: selectwelllocation: section, township, range ",
     2      section, township, range
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: selectwelllocation: section, township, range ",
     2      section, township, range
          endif
        if (section.ge.sectionmin.and.section.le.sectionmax) then
          if (range.ge.rangemin.and.range.le.rangemax) then
            select case (len(trim(township)))
              case (2)
                read(township,'(I1)')itownship
                ctownship = township(2:2)
              case (3)
                read(township,'(I2)')itownship
                ctownship = township(3:3)
              case default
                goto 99
            end select
            if (debug_cli) then
              write(outcli,*)
     1        "arg4 debug: selectwelllocation: itownship, ctownship",
     2        itownship, ctownship
            endif
            if (debug_log) then
              write(nlog,*)
     1        "arg4 debug: selectwelllocation: itownship, ctownship",
     2        itownship, ctownship
            endif
           if(itownship.ge.townshipmin.and.itownship.le.townshipmax)then
              call checkwelllocation
              if (code.gt.0) then
                write(outcli,*)"Code =",code
                return
              else
                write(outcli,*)"Invalid location (",
     1          section, township, range,") Try again"
                if (debug_cli) then
                  write(outcli,*)"arg4 debug: selectwelllocation: ",
     1            "section,township,range,code,II,JJ",
     2            section,township,range,code,II,JJ
                endif
                if (debug_log) then
                  write(nlog,*)"arg4 debug: selectwelllocation: ",
     1            "section,township,range,code,II,JJ",
     2            section,township,range,code,II,JJ
                endif
                goto 99
              endif
            else
              write(outcli,*)"Invalid township (",township,") Try again"
              if (debug_cli) then
                write(outcli,*)
     1        "Invalid township entered = township,itownship,ctownship",
     2          township, itownship, ctownship
                write(outcli,*)"  townshipmin,townshipmax ",
     1          townshipmin, townshipmax
              endif
              if (debug_log) then
                write(nlog,*)
     1        "Invalid township entered = township,itownship,ctownship",
     2          township, itownship, ctownship
                write(nlog,*)"  townshipmin,townshipmax ",
     1          townshipmin, townshipmax
              endif
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
     1  "Enter the well location as a section, township and range "
        write(outcli,*)
     1  "separated by commas (or on separate lines)."
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
        character(len=99) :: nuts

        if (debug_cli) then
          write(outcli,*)"arg4 debug: assignmodel: start"
          write(outcli,*)"arg4 debug: assignmodel: II, JJ", II, JJ
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: assignmodel: start"
          write(nlog,*)"arg4 debug: assignmodel: II, JJ", II, JJ
        endif
        !osel = aqselect
!     6160 ON OSEL GOTO 6170,6250,6270,6330,6360,6370
        select case (aqselect)
          case (1) ! LARAMIE-FOX HILLS
!     6170 IF I>95 AND J<53 THEN SHORT$="LF8"
            if(II.gt.95.and.JJ.lt.53) then
              modelshort = "LF8"
!     6180 IF I<57 AND J<21 THEN SHORT$="LF6":GOTO 6380
            else if(II.lt.57.and.JJ.lt.21) then
              modelshort = "LF6"
!     6190 IF I<33 AND J<66 THEN SHORT$="LF1":GOTO 6380
            else if(II.lt.33.and.JJ.lt.66) then
              modelshort = "LF1"
!     6200 IF I<24 THEN GOTO 6380
            else if(II.lt.24) then
              modelshort = "NA"
!     6205 IF I<44 AND J>65 THEN SHORT$="LF2"
            else if(II.lt.44.and.JJ.gt.65) then
              modelshort = "LF2"
!     6210 IF I<87 AND J<39 THEN SHORT$="LF4"
            else if(II.lt.87.and.JJ.lt.39) then
              modelshort = "LF4"
!     6220 IF I>66 AND J>55 THEN SHORT$="LF3"
            else if(II.gt.66.and.JJ.gt.55) then
              modelshort = "LF3"
            else
              modelshort = "NA"
            end if
          case (2) ! LOWER ARAPAHOE
!     6250 SHORT$="LA1"
            modelshort = "LA1"
          case (3) ! UPPER ARAPAHOE
!     6270 IF I<66 AND J<38 THEN SHORT$="AR1":GOTO 6380
            if(II.lt.66.and.JJ.lt.38) then
              modelshort = "AR1"
!     6280 IF I<72 AND J>37 THEN SHORT$="AR2":GOTO 6380
            else if(II.lt.72.and.JJ.gt.37) then
              modelshort = "AR2"
!     6290 IF I>83 AND J<47 THEN SHORT$="AR9":GOTO 6380
            else if(II.gt.83.and.JJ.lt.47) then
              modelshort = "AR9"
!     6300 IF J<38 THEN SHORT$="AR3":GOTO 6380
            else if(JJ.lt.38) then
              modelshort = "AR3"
!     6310 IF I>77 THEN SHORT$="AR4"
            else if(II.gt.77) then
              modelshort = "AR4"
            end if
          case (4) ! DENVER
!     6330 IF I>82 THEN SHORT$="DE10":GOTO 6380
            if(II.gt.82) then
              modelshort = "DE10"
!     6340 IF J>35 THEN SHORT$="DE9" ELSE SHORT$="DE8"          
            else if(JJ.gt.35) then
              modelshort = "DE9"
            else
              modelshort = "DE8"
            end if
          case (5) ! LOWER DAWSON
!     6360 SHORT$="LD2":GOTO 6380
            modelshort = "LD2"
          case (6) ! UPPER DAWSON
!     6370 IF I<83 THEN SHORT$="DA1" ELSE SHORT$="DA2"
            if(II.lt.83) then
              modelshort = "DA1"
            else
              modelshort = "DA2"
            end if
          case default
            ! uh oh
            modelshort = "NA"
        end select
        if (modelshort.eq."NA") then
          if (debug_cli) then
            write(outcli,*)"arg4 debug: assignmodel: II, JJ", II, JJ
          endif
          if (debug_log) then
            write(nlog,*)"arg4 debug: assignmodel: II, JJ", II, JJ
            write(nlog,*)
     1      "ADEQUATE MODEL NOT AVAILABLE. exit AUG4"
          endif
          write(outcli,*)
     1    "ADEQUATE MODEL NOT AVAILABLE (press RETURN to exit AUG4...)"
          read(incli,*)nuts
          call exit(99)
        end if
          if (debug_cli) then
            write(outcli,*)
     1      "arg4 debug: assignmodel: modelshort",modelshort
          endif
          if (debug_log) then
            write(nlog,*)
     1      "arg4 debug: assignmodel: modelshort",modelshort
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
        integer AJ

        if (debug_cli) then
          write(outcli,*)"arg4 debug: checkwelllocation: start"
        endif
        if (debug_log) then
          write(outcli,*)"arg4 debug: checkwelllocation: start"
        endif
        JJ = (70-range)*6 - 2
        AJ = sectioncolumn(section)
        JJ = JJ+AJ
        if (debug_cli) then
          write(outcli,*)"arg4 debug: checkwelllocation: ",
     1    "range,JJ,AJ",range,JJ,AJ
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: checkwelllocation: ",
     1    "range,JJ,AJ",range,JJ,AJ
        endif
        select case (ctownship)
          case ("n","N")
!     3450 I=(5-I)*6+INT(SCTN/6-.05)
            !I is the itownship
            !note: fortran integer division
            II = (5-itownship)*6 + ((section-1)/6)
            if (debug_cli) then
              write(outcli,*)"arg4 debug: checkwelllocation: N II ",II
            endif
            if (debug_log) then
              write(nlog,*)"arg4 debug: checkwelllocation: N II ",II
            endif
          case ("s","S")
!     3430 I=I*6+24+INT(SCTN/6-.05)
            !I is the itownship
            !note: fortran integer division
            II = itownship*6 + 24 + ((section-1)/6)
            if (debug_cli) then
              write(outcli,*)"arg4 debug: checkwelllocation: S II ",II
            endif
            if (debug_log) then
              write(nlog,*)"arg4 debug: checkwelllocation: S II ",II
            endif
          case default
            !uh oh
            if (debug_cli) then
              write(outcli,*)"arg4 debug: checkwelllocation: default "
            endif
            if (debug_log) then
              write(nlog,*)"arg4 debug: checkwelllocation: default "
            endif
        end select
        if (II.lt.1.or.II.gt.120) then
          !uh oh
          code = 0
          write(outcli,1001)section, township, range
 1001     format("YOU HAVE SELECTED A LOCATION (",I2," ",A3," ",I2,
     1           ") OUTSIDE THE BASIN.  TRY AGAIN!!")
        else if (JJ.lt.1.or.JJ.gt.84) then
          !uh oh
          code = 0
          write(outcli,1001)section, township, range
        else
          !it's good!
          code = (II-1)*84 + JJ
        end if
        if (debug_cli) then
          write(outcli,*)"arg4 debug: checkwelllocation: ",
     1    "section,township,itownship,ctownship,range,code,II,JJ",
     2    section,township,itownship,ctownship,range,code,II,JJ
        endif
        if (debug_log) then
          write(nlog,*)"arg4 debug: checkwelllocation: ",
     1    "section,township,itownship,ctownship,range,code,II,JJ",
     2    section,township,itownship,ctownship,range,code,II,JJ
        endif
        return
      end
c _________________________________________________________
c     create tape9.dat (well) data file
!     280 REM        ASSEMBLE WELL PKG. ON TAPE9.DAT
!     290 REM ***********************************************************************
!     300 CLS
!     310 RESTORE 330
!     320 READ TN$,NA$,A1$,A2$
!     330 DATA "9","WELL","  YIELD (gpm)"," + for pumping     WELL LOCATION"
!     340 REM PRINT "SELECT UNITS FOR WELL YIELD:";TAB(2);"1) gpm";TAB(2);"2) cfs";TAB(2);"3) acre-feet/year"
!     350 REM INPUT "         enter appropriate line number    ";QSELECT
!     351 QSELECT=3
!     360 IF QSELECT=1 THEN FAC=-1/448
!     370 IF QSELECT=2 THEN FAC=-1:A1$="  YIELD (cfs)"
!     380 IF QSELECT=3 THEN FAC=-(5280^2)/(640*1440*60*365.25):A1$="YIELD (af/yr)"
!     400 RESTORE 2480
!     410 READ A$,TNR
!     420 FOR X=1 TO TNR
!     430 READ B$
!     440 NEXT X
!     450 READ IMIN,JMIN,NROW,NCOL
!     460 IF A$<>SHORT$ GOTO 410
!     470 A2$="  #   LAY     ROW       COL   "+A2$
!     480 NAM$="TAPE"+TN$+".DAT"
!     510 NAM$=DD1$+NAM$
!     520 OPEN "O",#1,NAM$
!     540 NUT$="N"
!     550 ICB=0
!     560 IF NUT$="Y" THEN ICB=-1
!     570 PRINT "ENTER MAX. NUMBER OF ";NA$;"'S   ";
!     580 INPUT "",MX
!     590 PRINT#1,USING"##########";MX;ICB
!     610 FOR XY=1 TO NSP
!     620 PRINT "ENTER NUMBER OF ";NA$;"'S FOR STRESS PERIOD #";XY:PRINT
!     630 INPUT "",ITMP
!     640 PRINT#1,USING"##########";ITMP
!     650 CLS
!     660 PRINT "WELL  ";A1$;"     LOCATION (Ex:  5,3N,64)"
!     670 FOR K=1 TO ITMP
!     680 PRINT K;TAB(10);
!     690 INPUT ;"",Q
!     700 PRINT TAB(25);
!     710 INPUT "";SCTN,TWP$,RNG
!     720 GOSUB 3270
!     730 I=I-IMIN+1
!     740 J=J-JMIN+1
!     741 PRINT I;" and"J;" has been selected for Row Number and Column Number"
!     742 PRINT 
!     750 PRINT#1,USING"##########";1;I;J;:PRINT#1,USING"###.######";Q*FAC;:PRINT#1,TAB(51);"SEC";SCTN;"T";TWP$;" R";MID$(STR$(RNG),2);"W"
!     760 NEXT K
!     770 NEXT XY
!     780 CLOSE#1
!     781 OPEN "I",#1,"TAPE9.DAT":OPEN "O",#2,"TAPE9."+RN$
!     782 LINE INPUT#1,A$
!     783 PRINT#2,A$
!     784 GOTO 782
!     790 GOTO 230
      subroutine createtape9()
      IMPLICIT NONE
        include 'aug4_common2.inc'
        include 'aug4_common3.inc'
        ! local variables
        integer qselect, aqlayer, i, j, modelcount, icb, mx, xy, k, itmp
        character(len=1) :: nuts
        character(len=24) :: rawtownship, trimtownship
        character(len=24) :: filename
        character(len=96) :: header, fullfilename
        character(len=128) :: fileline
        real q
        data aqlayer /1/

        write(outcli,*)
        write(outcli,*)"Tape 9 (well package) parameters "
 101    write(outcli,*)"  SELECT UNITS FOR WELL YIELD:"
        !write(outcli,*)"  1. gpm"
        !write(outcli,*)"  2. cfs"
        !write(outcli,*)"  3. acre-feet/year"
        write(outcli,*)"    AUG4 is configured to use acre-feet/year."
 102    continue
c        read (incli,*,err=199) qselect
        qselect = 3
        select case (qselect)
          case (1)
            fac = -1.0/448.0
          case (2)
            fac = -1.0
            a1s = "  YIELD (cfs)"
          case (3)
            fac = -(5280.0**2)/(640.0*1440.0*60.0*365.25)
            a1s = "YIELD (af/yr)"
          case default
 199        write(outcli,*)"    Invalid selection. Enter 1,2,3"
            goto 102
        end select
c       match model short name
        modelcount = 0
        imin = 0
        jmin = 0
        nrow = 0
        ncol = 0
        do i=1,modelcountmax
          if(modelshort.eq.modelshorts(i)) then
            modelcount = i
            imin = imins(i)
            jmin = jmins(i)
            nrow = nrows(i)
            ncol = ncols(i)
          end if
        end do
c       create the data file
        header = "  #   LAY     ROW       COL   "//a2s
        filename = "TAPE"//tns//".DAT"
        fullfilename = trim(dd1s)//trim(filename)
        open(t9unit1,file=trim(fullfilename), status='unknown')
        filename = "TAPE"//tns//"."//trim(rns)
        fullfilename = trim(dd1s)//trim(filename)
        open(t9unit2,file=trim(fullfilename), status='unknown')
        nuts = "N"
        icb = 0
        if (nuts.eq."Y") icb = -1
        write(outcli,*)
 201    write(outcli,*)"  ENTER MAX. NUMBER OF "//nas//"S:   "
 202    continue
        read (incli,*,err=299) mx
        if (mx.ge.1) then
          write(t9unit1,203)mx,icb
          write(t9unit2,203)mx,icb
 203      format(I10,I10)         
          goto 300
        endif
 299    write(outcli,*)
     1  "    Invalid value. Enter a number greater than 0:"
        goto 202
 300    continue
c       well pumping and locations
        do xy=1,nsp
          write(outcli,*)
 301      write(outcli,304)nas,xy
 304      format("  ENTER NUMBER OF ",A4,"S FOR STRESS PERIOD #",I3)
 302      continue
          read (incli,*,err=399) itmp
          if (itmp.ge.0) then
            write (t9unit1,303) itmp
            write (t9unit2,303) itmp
 303        format(I10)
            write(outcli,305)nas,a1s
 305        format(4X,"FOR EACH ",A4," ENTER ",A13,
     1             " AND LOCATION (Ex:  5,3N,64)")
            do k=1,itmp
 306          write(outcli,307)nas,k
 307          format(6X,A4,1X,I2,1X,"Q:")
              read (incli,*,err=398) q
              goto 309
 398          write(outcli,*)
     1        "    Invalid value. Enter a number:"
              goto 306
 309          continue

 310          write(outcli,311)nas,k
 311          format(6X,A4,1X,I2,1X,"LOCATION:")
              read (incli,*,err=397) section,rawtownship,range
              trimtownship=trim(rawtownship)
              township=trimtownship(1:2)
c_______________________________________________________________________              
c             check the values, then check the location

        if (section.ge.sectionmin.and.section.le.sectionmax) then
          if (range.ge.rangemin.and.range.le.rangemax) then
            read(township,'(I1)')itownship
            !read(township,'(1x,A1)')ctownship
            !itownship = township[1:1]
            ctownship = township(2:2)
            if (debug_cli) then
              write(outcli,*)
     1        "arg4 debug: createtape9: itownship, ctownship",
     2        itownship, ctownship
            endif
            if (debug_log) then
              write(nlog,*)
     1        "arg4 debug: createtape9: itownship, ctownship",
     2        itownship, ctownship
            endif
           if(itownship.ge.townshipmin.and.itownship.le.townshipmax)then
              call checkwelllocation
              if (code.gt.0) then
c               !!SUCCESS!!
                if (debug_cli) then
                  write(outcli,*)"Code =",code
                endif
                if (debug_log) then
                  write(nlog,*)"Code =",code
                endif
c               730 I=I-IMIN+1
c               740 J=J-JMIN+1
                i = II - imin + 1
                j = JJ - jmin + 1
                write(outcli,313)i,j
 313            format(8x,"That location puts well in model row ",I4,
     1                     " and column ",I4)
!     750 PRINT#1,USING"##########";1;I;J;:PRINT#1,USING"###.######";Q*FAC;:PRINT#1,TAB(51);"SEC";SCTN;"T";TWP$;" R";MID$(STR$(RNG),2);"W"
                write (t9unit1,314)aqlayer,i,j,q*fac,
     1                                section,township,range
                write (t9unit2,314)aqlayer,i,j,q*fac,
     1                                section,township,range
 314      format(I10,I10,I10,F10.6,10X,"SEC",I3,1X,"T",A2,1X,"R",I2,"W")
                if (debug_cli) then
                  write (outcli,314)aqlayer,i,j,q*fac,
     1                                  section,township,range
                endif
                if (debug_log) then
                  write (nlog,314)aqlayer,i,j,q*fac,
     1                                  section,township,range
                endif
                goto 312
              else
                write(outcli,*)"Invalid location (",
     1          section, township, range,") Try again"
                if (debug_cli) then
                  write(outcli,*)"arg4 debug: createtape9: ",
     1            "section,township,range,code,II,JJ",
     2            section,township,range,code,II,JJ
                endif
                if (debug_log) then
                  write(nlog,*)"arg4 debug: createtape9: ",
     1            "section,township,range,code,II,JJ",
     2            section,township,range,code,II,JJ
                endif
                goto 310
              endif
            else
              write(outcli,*)"Invalid township (",township,") Try again"
              if (debug_cli) then
                write(outcli,*)
     1        "Invalid township entered = township,itownship,ctownship",
     2          township, itownship, ctownship
                write(outcli,*)"  townshipmin,townshipmax ",
     1          townshipmin, townshipmax
              endif
              if (debug_log) then
                write(nlog,*)
     1        "Invalid township entered = township,itownship,ctownship",
     2          township, itownship, ctownship
                write(nlog,*)"  townshipmin,townshipmax ",
     1          townshipmin, townshipmax
              endif
              goto 310
            endif
          else
            write(outcli,*)"Invalid range entered = ", range
            goto 310
          endif
        else
          write(outcli,*)"Invalid section entered = ", section
          goto 310
        endif
c_______________________________________________________________________
 397          write(outcli,*)
     1        "    Invalid location value. (Ex:  5,3N,64)"
              goto 310
 312          continue
            end do
            !finished
            goto 400
          endif
 399      write(outcli,*)
     1    "    Invalid value. Enter a number 0 or greater:"
          goto 302
 400      continue
        end do
        close (t9unit1)
        close (t9unit2)
        return
      end
