c     _________________________________________________________________
c     aug4_common2
c     _________________________________________________________________
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
c     _________________________________________________________________
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
