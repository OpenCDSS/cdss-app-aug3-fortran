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
     1 tslen(10), itslen, itslmin, itslmax, perlen(10), iperlen,
     2 nsp, insp, inspmin, inspmax, spcount, spselect,
     3 aqselect, osel, aqcount, code, II, JJ,
     4 section, sectionmin, sectionmax, sectioncolumn(36),
     5 itownship, townshipmin, townshipmax,
     6 range, rangemin, rangemax,
     7 tnrs(22), imins(22), jmins(22), nrows(22), ncols(22),
     8 tnr, imin, jmin, nrow, ncol,
     9 modelcountmax, rivercountmax
      integer nts, ints,
     1 tslen, itslen, itslmin, itslmax, perlen, iperlen,
     2 nsp, insp, inspmin, inspmax, spcount, spselect,
     3 aqselect, osel, aqcount, code, II, JJ,
     4 section, sectionmin, sectionmax, sectioncolumn,
     5 itownship, townshipmin, townshipmax,
     6 range, rangemin, rangemax,
     7 tnrs, imins, jmins, nrows, ncols,
     8 tnr, imin, jmin, nrow, ncol,
     9 modelcountmax, rivercountmax
      common /rblock3/ tsmult(10), xtsmult, xtsmmin, xtsmmax, fac,
     1             xnsp, xnspmin, xnspmax
      real tsmult, xtsmult, xtsmmin, xtsmmax, fac,
     1             xnsp, xnspmin, xnspmax
c      common /lblock3/ 
c      logical 


