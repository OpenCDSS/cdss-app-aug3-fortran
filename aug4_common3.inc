      common /cblock3/ shorts, dd1s, aqnam, aqnams(10), rns, rnns,
     1 aqshort, aqshorts(10), township, modelshort, ctownship,
     2 tns,nas,a1s,a2s,modelshorts(22),rivnames(15,22)
      character(len=48) dd1s
      character(len=24) aqnam, aqnams, rns, rnns, township, rivnames
      character(len=32) a2s
      character(len=13) a1s
      character(len=2) aqshort, shorts, aqshorts
      character(len=4) modelshort, modelshorts, nas
      character(len=1) ctownship, tns
      common /iblock3/ nts(10), perlen(10), nsp, spcount, spselect,
     1 aqselect, osel, aqcount, code, II, JJ,
     2 section, sectionmin, sectionmax, sectioncolumn(36),
     3 itownship, townshipmin, townshipmax,
     4 range, rangemin, rangemax,
     5 tnr(22), imin(22), jmin(22), nrow(22), ncol(22)
      integer nts, perlen, nsp, spcount, spselect,
     1 aqselect, osel, aqcount, code, II, JJ,
     2 section, sectionmin, sectionmax, sectioncolumn,
     3 itownship, townshipmin, townshipmax,
     4 range, rangemin, rangemax,
     5 tnr, imin, jmin, nrow, ncol
      common /rblock3/ tsmult(10), fac
      real tsmult, fac
c     common /lblock3/ 
c     logical


