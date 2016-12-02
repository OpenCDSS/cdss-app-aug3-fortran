      common /cblock3/ shorts, dd1s, aqnam, aqnams(10), rns, rnns,
     1 aqshort, aqshorts(10), township, modelshort, ctownship
      character(len=48) dd1s
      character(len=24) aqnam, aqnams, rns, rnns, township
      character(len=2) aqshort, shorts, aqshorts
      character(len=4) modelshort
      character(len=1) ctownship
      common /iblock3/ nts(10), perlen(10), nsp, spcount, spselect,
     1 aqselect, osel, aqcount, code, II, JJ,
     2 section, sectionmin, sectionmax, sectioncolumn(36),
     3 itownship, townshipmin, townshipmax,
     4 range, rangemin, rangemax
      integer nts, perlen, nsp, spcount, spselect,
     1 aqselect, osel, aqcount, code, II, JJ,
     2 section, sectionmin, sectionmax, sectioncolumn,
     3 itownship, townshipmin, townshipmax,
     4 range, rangemin, rangemax
      common /rblock3/ tsmult(10)
      real tsmult
c     common /lblock3/ 
c     logical


