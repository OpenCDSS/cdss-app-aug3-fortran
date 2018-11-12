c     ________________________________________________________________
c     count_dry
c     ________________________________________________________________
c AUG3 MODFLOW Groundwater Model Preprocessor
c AUG3 is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 2016-2018 Colorado Department of Natural Resources
c 
c AUG3 is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c     AUG3 is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with AUG3.  If not, see <https://www.gnu.org/licenses/>.
c     ________________________________________________________________
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
