!10 ON ERROR GOTO 1000
!20 OPEN "I",#1,"TAPE6.DAT"
!30 LINE INPUT#1,A$
!40 B=INSTR(A$,"DRY")
!50 IF B<>0 THEN PRINT A$:COUNT=COUNT+1
!60 GOTO 30
!1000 IF ERL=30 THEN PRINT "Found";COUNT;"occurances of DRY":SYSTEM
      program count_dry
      implicit none
      integer :: ierr
      integer, parameter :: iu = 20
      integer :: icount = 0
      integer i, nt
      character (len=*), parameter :: search_str = "DRY"
      character (len=*), parameter :: filename = "TAPE6.DAT"
      character (len=1000) :: text
      nt = LEN_TRIM(search_str)
      open (unit=iu,file=filename,action="read")
      do
         read (iu,"(a)",iostat=ierr) text ! read line into character variable
         if (ierr /= 0) exit
         DO i = 1,LEN(text)-nt+1
            IF (text(i:i+nt-1) == search_str(:nt)) icount = icount+1
         END DO
      end do
      print*,"Found ",icount," occurances of DRY. "//
     1       "Press ENTER to continue"
!      print*,"   (testing - remove this line)"
      read(*,*)
      end program count_dry
