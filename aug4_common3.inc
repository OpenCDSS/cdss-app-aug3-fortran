      common /cblock3/ shorts, dd1s, aqnam, aqnams(10), rns, rnns,
     1 aqshort, aqshorts(10), township
      character(len=48) dd1s
      character(len=24) shorts, aqnam, aqnams, rns, rnns, township
      character(len=2) aqshort, aqshorts
      common /iblock3/ nts(10), perlen(10), nsp, spcount, spselect,
     1 aqselect, osel, aqcount, section, range,
     2 sectionmin,sectionmax,townshipmin,townshipmax,rangemin,rangemax
      integer nts, perlen, nsp, spcount, spselect,
     1 aqselect, osel, aqcount, section, range,
     2 sectionmin,sectionmax,townshipmin,townshipmax,rangemin,rangemax
      common /rblock3/ tsmult(10)
      real tsmult
c     common /lblock3/ 
c     logical 
