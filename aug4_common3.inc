      common /cblock3/ shorts, dd1s, aqnam, aqnams(10), rns, rnns,
     1 aqshort, aqshorts(10), township, modelshort, ctownship,
     2 tns,nas,a1s,a2s,modelshorts(22),rivnames(15,22)
      character(len=48) :: dd1s
      character(len=24) :: aqnam, aqnams, rns, rnns, rivnames
      character(len=32) :: a2s
      character(len=13) :: a1s
      character(len=2) :: aqshort, shorts, aqshorts
      character(len=8) :: township
      character(len=4) :: modelshort, modelshorts, nas
      character(len=1) :: ctownship, tns
      common /iblock3/ nts(10), ints,
     0 tslen(10), itslen, itslmin, itslmax, perlen(10), iperlen,
     0 nsp, insp, inspmin, inspmax, spcount, spselect,
     1 aqselect, osel, aqcount, code, II, JJ,
     2 section, sectionmin, sectionmax, sectioncolumn(36),
     3 itownship, townshipmin, townshipmax,
     4 range, rangemin, rangemax,
     5 tnrs(22), imins(22), jmins(22), nrows(22), ncols(22),
     6 tnr, imin, jmin, nrow, ncol,
     7 modelcountmax, rivercountmax
      integer nts, ints,
     0 tslen, itslen, itslmin, itslmax, perlen, iperlen,
     0 nsp, insp, inspmin, inspmax, spcount, spselect,
     1 aqselect, osel, aqcount, code, II, JJ,
     2 section, sectionmin, sectionmax, sectioncolumn,
     3 itownship, townshipmin, townshipmax,
     4 range, rangemin, rangemax,
     5 tnrs, imins, jmins, nrows, ncols,
     6 tnr, imin, jmin, nrow, ncol,
     7 modelcountmax, rivercountmax
      common /rblock3/ tsmult(10),
     0             xtsmult, xtsmmin, xtsmmax, fac
      real tsmult, xtsmult, xtsmmin, xtsmmax, fac
c      common /lblock3/ 
c      logical 


