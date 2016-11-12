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
      character charvar*24,fnlog*24,ver*12,verdate*12
      dimension charvar(3)
      real realvar
      dimension realvar(3)
      integer int2var*2, int4var*4
      dimension int2var(3), int4var(3)
      integer intvar,nlog
      dimension intvar(3)
      integer ioptio*2, iback*2

      data charvar /'charvar one',
&                   'charvar two',
&                   'charvar three'/
      data ver     /'version 0.0 '/
      data verdate /'11/11/2016  '/
      data fnlog   /'aug4.log'/
      data realvar /1.111,
&                   2.222,
&                   3.333/
      data int2var /1,
&                   2,
&                   3/
      data int4var /11,
&                   22,
&                   33/
      data intvar  /111,
&                   222,
&                   333/
      data nlog    /13/
c
c _________________________________________________________
c     log file
      open(nlog,file=fnlog, status='unknown')
      write(nlog,200) ver, vdate
        write(6,200) ver, vdate
  200   format(
     1 72('_'),//
     1 '        StateMod                       '/
     1 '        State of Colorado - Water Supply Planning Model     '//
     1 '        Version: ',a8,/,
     1 '        Last revision date: ',a10,//
     1 72('_'))     
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
        Write(6,110)
        call flush(6)
  110   format(/,
     1           ' Option? ',
     4        //,'   [0] : STOP',
     1         /,'   [1] : One',
     2         /,'   [2] : Two',
     3         /,'   [3] : Three',
     4         /,'   [4] : Four')
c rrb 10/27/94 Additional Output
        write(6,*) ' '
        read (5,*,err=165) ioptio
      endif
c _________________________________________________________
c     user selected exit/stop from menu
      if(ioptio.eq.0) goto 170
      if(ioptio.gt.4) goto 165
      GO TO (130,140,150,160) IOPTIO
c ______________________________________________________________________
c     Option One
  130 write(nlog, 132) 'Option One  '
  132 format(/,72('_'),/'  AUG4; Option Specified = ', a12)
c      call optionone
      goto 166
c ______________________________________________________________________
c     Option Two
  140 write(nlog, 132) 'Option Two  '
c      call optiontwo
      goto 166
c ______________________________________________________________________
c     Option Three
  150 write(nlog, 132) 'Option Three'
c      call optionthree
      goto 166
c ______________________________________________________________________
c     Option Four
  160 write(nlog, 132) 'Option Four '
      write(6,200) ver, vdate
      goto 166
c ______________________________________________________________________
c     Invalid Option
  165 write(6,*) ' ** Invalid option, try again **'
      call flush(6)
      goto 166               
c _________________________________________________________
c     Go back to menu if in default mode
  166 if(iback.eq.1) then
        ioptio = 0
        goto 100  
      endif
c _________________________________________________________
c     exit the program      
  170 write(6,180) fnlog
      call flush(6)
  180 format('  AUG4; See detailed messages in file: ', a256) 
  190 write(6,*) 'Stop 0'
      call flush(6)
      call exit(0)
      stop 