C     ******************************************************************        

C     MAIN CODE FOR MODULAR MODEL --    6/1/83                                  
C          BY MICHAEL G. MCDONALD AND ARLEN W. HARBAUGH                         
C-----VERSION 1116 28DEC1983 MAIN1                                              
C     ******************************************************************        
C
C         following modifications made at Colo Div Water Resources:
C
C       1.  OPEN statements for VAX/VMS file handling  7/31/85  W.B.
C       2.  Errata from USGS  8/7/85 W.B.
C       3.  Changed ANAME to type CHARACTER*4 in subroutines:
C            U1DREL     U2DINT     U2DREL     8/7/85  W.B.
C           (to match type specification in CALLing routines)
C       4.  commented out CHARACTER*4 statements because VAX was
C           showing fatal error described as a possible parameter
C           inconsistency.      W.B. 8/15/85
C       5.  Changed OPEN statements to use STATUS='UNKNOWN', so
C           that either an existing file will be used, or a new
C           file will be created if none exists
C        6.  CHANGED SUBROUTINE SBAS1D TO WRITE DRAWDOWN TO UNIT
C            21 USING FORMAT 15F7.2.   DRS  1/15/86.
C        7.  CHANGED SUBROUTINES RIV1BD AND WEL1BD TO ALLOW CELL-
C            BY-CELL RIVER FLOWS TO BE WRITTEN TO UNIT 74
C            DRS   8/27/1986
C        9.  Changed river subroutines to accumulate cell by cell flows
C            into total for river segment. 08/02/90 DRS


C     ******************************************************************        
C
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      COMMON X(100000)                                                           
      DIMENSION HEADNG(32),VBNM(4,20),VBVL(4,20),IUNIT(24)                      
      DOUBLE PRECISION DUMMY                                                    
      EQUIVALENCE (DUMMY,X(1))                                                  
C     ------------------------------------------------------------------        
      CHARACTER*30 FNAME
      CHARACTER*4 DIRNAME

 1501 FORMAT (' Input model (example DE09), directory must have 4 charac
     1ters : ',$)
 1502 FORMAT (A4)
 1503 FORMAT (A30)

      WRITE (*,1501)
      READ (*,1502) DIRNAME




C                  
C         open all data input files      


      FNAME ='C:\AUG3\'//DIRNAME//'\TAPE1.DAT'
      OPEN(UNIT=1,FILE=FNAME,STATUS='UNKNOWN')                       

      FNAME ='C:\AUG3\'//DIRNAME//'\TAPE2.DAT'
      OPEN(UNIT=2,FILE=FNAME,STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\TAPE4.DAT'
      OPEN(UNIT=4,FILE=FNAME,STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\TAPE7.DAT'
      OPEN(UNIT=7,FILE=FNAME,STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\TAPE8.DAT'
      OPEN(UNIT=8,FILE=FNAME,STATUS='UNKNOWN')

      OPEN(UNIT=9,FILE='TAPE9.DAT',STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\TAPE10.DAT'
      OPEN(UNIT=10,FILE=FNAME,STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\TAPE11.DAT'
      OPEN(UNIT=11,FILE=FNAME,STATUS='UNKNOWN')

      OPEN(UNIT=12,FILE='TAPE12.DAT',STATUS='UNKNOWN')
      OPEN(UNIT=13,FILE='TAPE13.DAT',STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\TAPE14.DAT'
        OPEN(UNIT=14,FILE=FNAME,STATUS='UNKNOWN')

C         open all data output files

      FNAME ='TAPE20.DAT'
      OPEN(UNIT=20,FILE=FNAME,STATUS='UNKNOWN')

      FNAME ='TAPE21.DAT'
      OPEN(UNIT=21,FILE=FNAME,STATUS='UNKNOWN')

      FNAME ='TAPE22.DAT'
      OPEN(UNIT=22,FILE=FNAME,STATUS='UNKNOWN')

      FNAME ='TAPE23.DAT'
      OPEN(UNIT=23,FILE=FNAME,STATUS='UNKNOWN')

      FNAME ='TAPE25.DAT'
      OPEN(UNIT=25,FILE=FNAME,STATUS='UNKNOWN')
C   OPEN FILES CONTAINING ARRAYS

      FNAME ='C:\AUG3\'//DIRNAME//'\IBOUND.DAT'
        OPEN(UNIT=30,FILE=FNAME,STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\HEAD1.DAT'
        OPEN(UNIT=31,FILE=FNAME,STATUS='UNKNOWN')

C        OPEN(UNIT=32,FILE='HEAD2.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=33,FILE='HEAD3.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=34,FILE='HEAD4.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=26,FILE='VCONT12.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=27,FILE='VCONT23.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=28,FILE='VCONT34.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=40,FILE='GRND.DAT',STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\PSF1.DAT'
         OPEN(UNIT=41,FILE=FNAME,STATUS='UNKNOWN')

C        OPEN(UNIT=42,FILE='PSF2.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=43,FILE='PSF3.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=44,FILE='PSF4.DAT',STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\SSF1.DAT'
         OPEN(UNIT=46,FILE=FNAME,STATUS='UNKNOWN')

C        OPEN(UNIT=47,FILE='SSF2.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=48,FILE='SSF3.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=49,FILE='SSF4.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=51,FILE='T1.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=52,FILE='T2.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=53,FILE='T3.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=54,FILE='T4.DAT',STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\K1.DAT'
        OPEN(UNIT=56,FILE=FNAME,STATUS='UNKNOWN')

C        OPEN(UNIT=57,FILE='K2.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=57,FILE='K3.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=58,FILE='K4.DAT',STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\BOTTOM1.DAT'
        OPEN(UNIT=61,FILE=FNAME,STATUS='UNKNOWN')

C        OPEN(UNIT=62,FILE='BOTTOM2.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=63,FILE='BOTTOM3.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=64,FILE='BOTTOM4.DAT',STATUS='UNKNOWN')

      FNAME ='C:\AUG3\'//DIRNAME//'\TOP1.DAT'
        OPEN(UNIT=66,FILE=FNAME,STATUS='UNKNOWN')

C        OPEN(UNIT=67,FILE='TOP2.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=68,FILE='TOP3.DAT',STATUS='UNKNOWN')
C        OPEN(UNIT=69,FILE='TOP4.DAT',STATUS='UNKNOWN')
C         open standard output file
      OPEN(UNIT=6,FILE='TAPE6.DAT',STATUS='UNKNOWN')
        OPEN(UNIT=74,FILE='TAPE74.DAT',STATUS='UNKNOWN')
        REWIND 6
        REWIND 74
C
C                                                                               
C1------SET SIZE OF X ARRAY. REMEMBER TO REDIMENSION X.                         
      LENX=100000                                                                
C                                                                               
C2------ASSIGN BASIC INPUT UNIT AND PRINTER UNIT.                               
      INBAS=1                                                                   
      IOUT=6                                                                    
C                                                                               
C3------DEFINE PROBLEM__ROWS,COLUMNS,LAYERS,STRESS PERIODS,PACKAGES             
      CALL BAS1DF(ISUM,HEADNG,NPER,ITMUNI,TOTIM,NCOL,NROW,NLAY,                 
     1             NODES,INBAS,IOUT,IUNIT)                                      
C                                                                               
C4------ALLOCATE SPACE IN "X" ARRAY.                                            
      CALL BAS1AL(ISUM,LENX,LCHNEW,LCHOLD,LCIBOU,LCCR,LCCC,LCCV,                
     1              LCHCOF,LCRHS,LCDELR,LCDELC,LCSTRT,LCBUFF,LCIOFL,            
     2              INBAS,ISTRT,NCOL,NROW,NLAY,IOUT)                            
      IF(IUNIT(1).GT.0) CALL BCF1AL(ISUM,LENX,LCSC1,LCHY,                       
     1     LCBOT,LCTOP,LCSC2,LCTRPY,IUNIT(1),ISS,                               
     2     NCOL,NROW,NLAY,IOUT,IBCFCB)                                          
      IF(IUNIT(2).GT.0) CALL WEL1AL(ISUM,LENX,LCWELL,MXWELL,NWEL,               
     1                 IUNIT(2),IOUT,IWELCB)                                    
      IF(IUNIT(3).GT.0) CALL DRN1AL(ISUM,LENX,LCDRAI,NDRAIN,MXDRN,              
     1                 IUNIT(3),IOUT,IDRNCB)                                    
      IF(IUNIT(8).GT.0) CALL RCH1AL(ISUM,LENX,LCIRCH,LCRECH,NRCHOP,             
     1            NCOL,NROW,IUNIT(8),IOUT,IRCHCB)                               
      IF(IUNIT(5).GT.0) CALL EVT1AL(ISUM,LENX,LCIEVT,LCEVTR,LCEXDP,             
     1            LCSURF,NCOL,NROW,NEVTOP,IUNIT(5),IOUT,IEVTCB)                 
      IF(IUNIT(4).GT.0) CALL RIV1AL(ISUM,LENX,LCRIVR,MXRIVR,NRIVER,             
     1            IUNIT(4),IOUT,IRIVCB,MRSEG)                                         
      IF(IUNIT(7).GT.0) CALL GHB1AL(ISUM,LENX,LCBNDS,NBOUND,MXBND,              
     1            IUNIT(7),IOUT,IGHBCB)                                         
      IF(IUNIT(9).GT.0) CALL SIP1AL(ISUM,LENX,LCEL,LCFL,LCGL,LCV,               
     1          LCHDCG,LCLRCH,LCW,MXITER,NPARM,NCOL,NROW,NLAY,                  
     2          IUNIT(9),IOUT)                                                  
      IF(IUNIT(11).GT.0) CALL SOR1AL(ISUM,LENX,LCA,LCRES,LCHDCG,LCLRCH,         
     1           LCIEQP,MXITER,NCOL,NROW,NLAY,NSLICE,MBW,IUNIT(11),IOUT)        
C                                                                               
C5------IF THE "X" ARRAY IS NOT BIG ENOUGH THEN STOP.                           
      IF(ISUM-1.GT.LENX) STOP                                                   
C                                                                               
C6------READ AND PREPARE INFORMATION FOR ENTIRE SIMULATION.                     
      CALL BAS1RP(X(LCIBOU),X(LCHNEW),X(LCSTRT),X(LCHOLD),                      
     1       ISTRT,INBAS,HEADNG,NCOL,NROW,NLAY,NODES,VBVL,X(LCIOFL),            
     2       IUNIT(12),IHEDFM,IDDNFM,IHEDUN,IDDNUN,IOUT)                        
      IF(IUNIT(1).GT.0) CALL BCF1RP(X(LCIBOU),X(LCHNEW),X(LCSC1),               
     1          X(LCHY),X(LCCR),X(LCCC),X(LCCV),X(LCDELR),                      
     2           X(LCDELC),X(LCBOT),X(LCTOP),X(LCSC2),X(LCTRPY),                
     3           IUNIT(1),ISS,NCOL,NROW,NLAY,NODES,IOUT)                        
      IF(IUNIT(9).GT.0) CALL SIP1RP(NPARM,MXITER,ACCL,HCLOSE,X(LCW),            
     1          IUNIT(9),IPCALC,IPRSIP,IOUT)                                    
      IF(IUNIT(11).GT.0) CALL SOR1RP(MXITER,ACCL,HCLOSE,IUNIT(11),              
     1         IPRSOR,IOUT)                                                     
C                                                                               
C7------SIMULATE EACH STRESS PERIOD.                                            
      DO 300 KPER=1,NPER                                                        
C                                                                               
C7A-----READ STRESS PERIOD TIMING INFORMATION.                                  
      CALL BAS1ST(NSTP,DELT,TSMULT,PERTIM,KPER,INBAS,IOUT)                      
C                                                                               
C7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.                         
      IF(IUNIT(2).GT.0) CALL WEL1RP(X(LCWELL),NWEL,MXWELL,IUNIT(2),             
     1             IOUT)                                                        
      IF(IUNIT(3).GT.0) CALL DRN1RP(X(LCDRAI),NDRAIN,MXDRN,IUNIT(3),            
     1                 IOUT)                                                    
      IF(IUNIT(8).GT.0) CALL RCH1RP(NRCHOP,X(LCIRCH),X(LCRECH),                 
     1            X(LCDELR),X(LCDELC),NROW,NCOL,NLAY,IUNIT(8),IOUT)             
      IF(IUNIT(5).GT.0) CALL EVT1RP(NEVTOP,X(LCIEVT),X(LCEVTR),                 
     1            X(LCEXDP),X(LCSURF),X(LCDELR),X(LCDELC),NCOL,NROW,            
     1            NLAY,IUNIT(5),IOUT)                                           
      IF(IUNIT(4).GT.0) CALL RIV1RP(X(LCRIVR),NRIVER,MXRIVR,IUNIT(4),           
     1            IOUT)                                                         
      IF(IUNIT(7).GT.0) CALL GHB1RP(X(LCBNDS),NBOUND,MXBND,IUNIT(7),            
     1              IOUT)                                                       
C                                                                               
C7C-----SIMULATE EACH TIME STEP.                                                
      DO 200 KSTP=1,NSTP                                                        
C                                                                               
C7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW..                             
      CALL BAS1AD(DELT,TSMULT,TOTIM,PERTIM,X(LCHNEW),X(LCHOLD),KSTP,            
     1             NCOL,NROW,NLAY)                                              
C                                                                               
C7C2----ITERATIVELY FORMULATE AND SOLVE THE EQUATIONS.                          
      DO 100 KITER=1,MXITER                                                     
C                                                                               
C7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.                              
      CALL BAS1FM(X(LCHCOF),X(LCRHS),NCOL,NROW,NLAY,NODES)                      
      IF(IUNIT(1).GT.0) CALL BCF1FM(X(LCHCOF),X(LCRHS),X(LCHOLD),               
     1          X(LCSC1),X(LCHNEW),X(LCIBOU),X(LCCR),X(LCCC),X(LCCV),           
     2          X(LCHY),X(LCTRPY),X(LCBOT),X(LCTOP),X(LCSC2),                   
     3          X(LCDELR),X(LCDELC),DELT,ISS,KITER,KSTP,KPER,NCOL,              
     4          NROW,NLAY,IOUT)                                                 
      IF(IUNIT(2).GT.0) CALL WEL1FM(NWEL,MXWELL,X(LCRHS),X(LCWELL),             
     1           X(LCIBOU),NCOL,NROW,NLAY)                                      
      IF(IUNIT(3).GT.0) CALL DRN1FM(NDRAIN,MXDRN,X(LCDRAI),X(LCHNEW),           
     1         X(LCHCOF),X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY)                     
      IF(IUNIT(8).GT.0) CALL RCH1FM(NRCHOP,X(LCIRCH),X(LCRECH),                 
     1            X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY)                            
      IF(IUNIT(5).GT.0) CALL EVT1FM(NEVTOP,X(LCIEVT),X(LCEVTR),                 
     1            X(LCEXDP),X(LCSURF),X(LCRHS),X(LCHCOF),X(LCIBOU),             
     1            X(LCHNEW),NCOL,NROW,NLAY)                                     
      IF(IUNIT(4).GT.0) CALL RIV1FM(NRIVER,MXRIVR,X(LCRIVR),X(LCHNEW),          
     1            X(LCHCOF),X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY)                  
      IF(IUNIT(7).GT.0) CALL GHB1FM(NBOUND,MXBND,X(LCBNDS),X(LCHCOF),           
     1            X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY)                            
C                                                                               
C7C2B---MAKE ONE CUT AT AN APPROXIMATE SOLUTION.                                
      IF(IUNIT(9).GT.0) CALL SIP1AP(X(LCHNEW),X(LCIBOU),X(LCCR),X(LCCC),        
     1     X(LCCV),X(LCHCOF),X(LCRHS),X(LCEL),X(LCFL),X(LCGL),X(LCV),           
     2     X(LCW),X(LCHDCG),X(LCLRCH),NPARM,KITER,HCLOSE,ACCL,ICNVG,            
     3     KSTP,KPER,IPCALC,IPRSIP,MXITER,NSTP,NCOL,NROW,NLAY,NODES,            
     4     IOUT)                                                                
      IF(IUNIT(11).GT.0) CALL SOR1AP(X(LCHNEW),X(LCIBOU),X(LCCR),               
     1     X(LCCC),X(LCCV),X(LCHCOF),X(LCRHS),X(LCA),X(LCRES),X(LCIEQP),        
     2     X(LCHDCG),X(LCLRCH),KITER,HCLOSE,ACCL,ICNVG,KSTP,KPER,IPRSOR,        
     3     MXITER,NSTP,NCOL,NROW,NLAY,NSLICE,MBW,IOUT)                          
C                                                                               
C7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.                   
      IF(ICNVG.EQ.1) GO TO 110                                                  
  100 CONTINUE                                                                  
      KITER=MXITER                                                              
  110 CONTINUE                                                                  
C                                                                               
C7C3----DETERMINE WHICH OUTPUT IS NEEDED.                                       
      CALL BAS1OC(NSTP,KSTP,KPER,ISTRT,ICNVG,X(LCIOFL),NLAY,                    
     1  IBUDFL,ICBCFL,IHDDFL,IUNIT(12),IOUT)                                    
C                                                                               
C7C4----CALCULATE BUDGET TERMS. SAVE CELL-BY-CELL FLOW TERMS.                   
      MSUM=1                                                                    
      IF(IUNIT(1).GT.0) CALL BCF1BD(VBNM,VBVL,MSUM,X(LCHNEW),                   
     1     X(LCIBOU),X(LCHOLD),X(LCSC1),X(LCCR),X(LCCC),X(LCCV),                
     2     X(LCTOP),X(LCSC2),DELT,ISS,NCOL,NROW,NLAY,KSTP,KPER,                 
     3     IBCFCB,ICBCFL,X(LCBUFF),IOUT)                                        
      IF(IUNIT(2).GT.0) CALL WEL1BD(NWEL,MXWELL,VBNM,VBVL,MSUM,X(LCWELL)
     1     ,X(LCIBOU),DELT,NCOL,NROW,NLAY,KSTP,KPER,IWELCB,ICBCFL,               
     1     X(LCBUFF),IOUT)                                                      
      IF(IUNIT(3).GT.0) CALL DRN1BD(NDRAIN,MXDRN,VBNM,VBVL,MSUM,                
     1     X(LCDRAI),DELT,X(LCHNEW),NCOL,NROW,NLAY,X(LCIBOU),KSTP,KPER,         
     2     IDRNCB,ICBCFL,X(LCBUFF),IOUT)                                        
      IF(IUNIT(8).GT.0) CALL RCH1BD(NRCHOP,X(LCIRCH),X(LCRECH),                 
     1     X(LCIBOU),NROW,NCOL,NLAY,DELT,VBVL,VBNM,MSUM,KSTP,KPER,              
     2     IRCHCB,ICBCFL,X(LCBUFF),IOUT)                                        
      IF(IUNIT(5).GT.0) CALL EVT1BD(NEVTOP,X(LCIEVT),X(LCEVTR),                 
     1     X(LCEXDP),X(LCSURF),X(LCIBOU),X(LCHNEW),NCOL,NROW,NLAY,              
     2     DELT,VBVL,VBNM,MSUM,KSTP,KPER,IEVTCB,ICBCFL,X(LCBUFF),IOUT)          
      IF(IUNIT(4).GT.0) CALL RIV1BD(NRIVER,MXRIVR,X(LCRIVR),X(LCIBOU),          
     1     X(LCHNEW),NCOL,NROW,NLAY,DELT,VBVL,VBNM,MSUM,                        
     2     KSTP,KPER,IRIVCB,ICBCFL,X(LCBUFF),IOUT,MRSEG)                              
      IF(IUNIT(7).GT.0) CALL GHB1BD(NBOUND,MXBND,VBNM,VBVL,MSUM,                
     1     X(LCBNDS),DELT,X(LCHNEW),NCOL,NROW,NLAY,X(LCIBOU),KSTP,KPER,         
     2     IGHBCB,ICBCFL,X(LCBUFF),IOUT)                                        
C                                                                               
C7C5---PRINT AND OR SAVE HEADS AND DRAWDOWNS. PRINT OVERALL BUDGET.             
      CALL BAS1OT(X(LCHNEW),X(LCSTRT),ISTRT,X(LCBUFF),X(LCIOFL),                
     1        MSUM,X(LCIBOU),VBNM,VBVL,KSTP,KPER,DELT,                          
     2        PERTIM,TOTIM,ITMUNI,NCOL,NROW,NLAY,ICNVG,                         
     3     IHDDFL,IBUDFL,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IOUT)                      
C                                                                               
C7C6----IF ITERATION FAILED TO CONVERGE THEN STOP.                              
      IF(ICNVG.EQ.0) STOP                                                       
  200 CONTINUE                                                                  
  300 CONTINUE                                                                  
C                                                                               
C8------END PROGRAM                                                             
      STOP                                                                      
C                                                                               
      END                                                                       
      SUBROUTINE BAS1AD(DELT,TSMULT,TOTIM,PERTIM,HNEW,HOLD,KSTP,                
     1                  NCOL,NROW,NLAY)                                         
C                                                                               
C-----VERSION 1412 22FEB1982 BAS1AD                                             
C                                                                               
C     ******************************************************************        
C     ADVANCE TO NEXT TIME STEP                                                 
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
C                                                                               
      DIMENSION HNEW(NCOL,NROW,NLAY), HOLD(NCOL,NROW,NLAY)                      
C     ------------------------------------------------------------------        
C                                                                               
C1------IF NOT FIRST TIME STEP THEN CALCULATE TIME STEP LENGTH.                 
      IF(KSTP.NE.1) DELT=TSMULT*DELT                                            
C                                                                               
C2------ACCUMULATE ELAPSED TIME IN SIMULATION(TOTIM) AND IN THIS                
C2------STRESS PERIOD(PERTIM).                                                  
      TOTIM=TOTIM+DELT                                                          
      PERTIM=PERTIM+DELT                                                        
C                                                                               
C3------COPY HNEW TO HOLD.                                                      
      DO 10 K=1,NLAY                                                            
      DO 10 I=1,NROW                                                            
      DO 10 J=1,NCOL                                                            
   10 HOLD(J,I,K)=HNEW(J,I,K)                                                   
C                                                                               
C4------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE BAS1AL(ISUM,LENX,LCHNEW,LCHOLD,LCIBOU,LCCR,LCCC,LCCV,          
     1            LCHCOF,LCRHS,LCDELR,LCDELC,LCSTRT,LCBUFF,LCIOFL,INBAS,        
     1            ISTRT,NCOL,NROW,NLAY,IOUT)                                    
C-----VERSION 0927 08DEC1983 BAS1AL                                             
C     ******************************************************************        
C     ALLOCATE SPACE FOR BASIC MODEL ARRAYS                                     
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
C                                                                               
C1------PRINT A MESSAGE IDENTIFYING THE PACKAGE.                                
      WRITE(IOUT,1)INBAS                                                        
    1 FORMAT(1H0,'BAS1 -- BASIC MODEL PACKAGE, VERSION 1, 12/08/83',            
     2' INPUT READ FROM UNIT',I3)                                               
C                                                                               
C2------READ & PRINT FLAG IAPART (RHS & BUFFER SHARE SPACE?) AND                
C2------FLAG ISTRT (SHOULD STARTING HEADS BE SAVED FOR DRAWDOWN?)               
      READ(INBAS,2) IAPART,ISTRT                                                
    2 FORMAT(2I10)                                                              
      IF(IAPART.EQ.0) WRITE(IOUT,3)                                             
    3 FORMAT(1X,'ARRAYS RHS AND BUFF WILL SHARE MEMORY.')                       
      IF(ISTRT.NE.0) WRITE(IOUT,4)                                              
    4 FORMAT(1X,'START HEAD WILL BE SAVED')                                     
      IF(ISTRT.EQ.0) WRITE(IOUT,5)                                              
    5 FORMAT(1X,'START HEAD WILL NOT BE SAVED',                                 
     1      ' -- DRAWDOWN CANNOT BE CALCULATED')                                
C                                                                               
C3------STORE,IN ISOLD, LOCATION OF FIRST UNALLOCATED SPACE IN X.               
      ISOLD=ISUM                                                                
      NRCL=NROW*NCOL*NLAY                                                       
C                                                                               
C4------ALLOCATE SPACE FOR ARRAYS.                                              
      LCHNEW=ISUM                                                               
      ISUM=ISUM+2*NRCL                                                          
      LCHOLD=ISUM                                                               
      ISUM=ISUM+NRCL                                                            
      LCIBOU=ISUM                                                               
      ISUM=ISUM+NRCL                                                            
      LCCR=ISUM                                                                 
      ISUM=ISUM+NRCL                                                            
      LCCC=ISUM                                                                 
      ISUM=ISUM+NRCL                                                            
      LCCV=ISUM                                                                 
      ISUM=ISUM+NROW*NCOL*(NLAY-1)                                              
      LCHCOF=ISUM                                                               
      ISUM=ISUM+NRCL                                                            
      LCRHS=ISUM                                                                
      ISUM=ISUM+NRCL                                                            
      LCDELR=ISUM                                                               
      ISUM=ISUM+NCOL                                                            
      LCDELC=ISUM                                                               
      ISUM=ISUM+NROW                                                            
      LCIOFL=ISUM                                                               
      ISUM=ISUM+NLAY*4                                                          
C                                                                               
C5------IF BUFFER AND RHS SHARE SPACE THEN LCBUFF=LCRHS.                        
      LCBUFF=LCRHS                                                              
      IF(IAPART.EQ.0) GO TO 50                                                  
      LCBUFF=ISUM                                                               
      ISUM=ISUM+NRCL                                                            
C                                                                               
C6------IF STRT WILL BE SAVED THEN ALLOCATE SPACE.                              
   50 LCSTRT=ISUM                                                               
      IF(ISTRT.NE.0) ISUM=ISUM+NRCL                                             
      ISP=ISUM-ISOLD                                                            
C                                                                               
C7------PRINT AMOUNT OF SPACE USED.                                             
      WRITE(IOUT,6) ISP                                                         
    6 FORMAT(1X,I6,' ELEMENTS IN X ARRAY ARE USED BY BAS')                      
      ISUM1=ISUM-1                                                              
      WRITE(IOUT,7) ISUM1,LENX                                                  
    7 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED OUT OF',I7)                       
      IF(ISUM1.GT.LENX) WRITE(IOUT,8)                                           
    8 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')                  
C                                                                               
C                                                                               
C8------RETURN                                                                  
      RETURN                                                                    
C                                                                               
      END                                                                       
      SUBROUTINE BAS1DF(ISUM,HEADNG,NPER,ITMUNI,TOTIM,NCOL,NROW,                
     1       NLAY,NODES,INBAS,IOUT,IUNIT)                                       
C                                                                               
C-----VERSION 1128 28DEC1983 BAS1DF                                             
C     ******************************************************************        
C     DEFINE KEY MODEL PARAMETERS                                               
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION HEADNG(32),IUNIT(24)                                            
C     ------------------------------------------------------------------        
C                                                                               
C1------PRINT THE NAME OF THE PROGRAM.                                          
      WRITE(IOUT,1)                                                             
    1 FORMAT(1H1,20X,'U.S. GEOLOGICAL SURVEY MODULAR',                          
     1      ' FINITE-DIFFERENCE GROUND-WATER MODEL')                            
C                                                                               
C2------READ AND PRINT A HEADING.                                               
      READ(INBAS,2) HEADNG                                                      
    2 FORMAT(20A4)                                                              
      WRITE(IOUT,3) HEADNG                                                      
    3 FORMAT(1H0,32A4)                                                          
C                                                                               
C3------READ NUMBER OF LAYERS,ROWS,COLUMNS,STRESS PERIODS AND                   
C3------UNITS OF TIME CODE.                                                     
      READ(INBAS,4) NLAY,NROW,NCOL,NPER,ITMUNI                                  
    4 FORMAT(8I10)                                                              
C                                                                               
C4------PRINT # OF LAYERS, ROWS, COLUMNS AND STRESS PERIODS.                    
      WRITE(IOUT,5) NLAY,NROW,NCOL                                              
    5 FORMAT(1X,I4,' LAYERS',I10,' ROWS',I10,' COLUMNS')                        
      WRITE(IOUT,6) NPER                                                        
    6 FORMAT(1X,I3,' STRESS PERIOD(S) IN SIMULATION')                           
C                                                                               
C5------SELECT AND PRINT A MESSAGE SHOWING TIME UNITS.                          
      IF(ITMUNI.LT.0 .OR. ITMUNI.GT.5) ITMUNI=0                                 
      GO TO (10,20,30,40,50),ITMUNI                                             
      WRITE(IOUT,9)                                                             
    9 FORMAT(1X,'MODEL TIME UNITS ARE UNDEFINED')                               
      GO TO 100                                                                 
   10 WRITE(IOUT,11)                                                            
   11 FORMAT(1X,'MODEL TIME UNIT IS SECONDS')                                   
      GO TO 100                                                                 
   20 WRITE(IOUT,21)                                                            
   21 FORMAT(1X,'MODEL TIME UNIT IS MINUTES')                                   
      GO TO 100                                                                 
   30 WRITE(IOUT,31)                                                            
   31 FORMAT(1X,'MODEL TIME UNIT IS HOURS')                                     
      GO TO 100                                                                 
   40 WRITE(IOUT,41)                                                            
   41 FORMAT(1X,'MODEL TIME UNIT IS DAYS')                                      
      GO TO 100                                                                 
   50 WRITE(IOUT,51)                                                            
   51 FORMAT(1X,'MODEL TIME UNIT IS YEARS')                                     
C                                                                               
C6------READ & PRINT INPUT UNIT NUMBERS (IUNIT) FOR MAJOR OPTIONS.              
  100 READ(INBAS,101) IUNIT                                                     
  101 FORMAT(24I3)                                                              
      WRITE(IOUT,102) (I,I=1,24),IUNIT                                          
  102 FORMAT(1H0,'I/O UNITS:'/1X,'ELEMENT OF IUNIT:',24I3,                      
     1                       /1X,'        I/O UNIT:',24I3)                      
C                                                                               
C7------INITIALIZE TOAL ELAPSED TIME COUNTER STORAGE ARRAY COUNTER              
C7------AND CALCULATE NUMBER OF CELLS.                                          
      TOTIM=0.                                                                  
      ISUM=1                                                                    
      NODES=NCOL*NROW*NLAY                                                      
C                                                                               
C8------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE BAS1FM(HCOF,RHS,NCOL,NROW,NLAY,NODES)                          
C                                                                               
C                                                                               
C-----VERSION 1408 22FEB1982 BAS1FM                                             
C     ******************************************************************        
C     SET HCOF=RHS=0.                                                           
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION HCOF(NODES),RHS(NODES)                                          
C     ------------------------------------------------------------------        
C                                                                               
C1------FOR EACH CELL INITIALIZE HCOF AND RHS ACCUMULATORS.                     
      DO 100 I=1,NODES                                                          
      HCOF(I)=0.                                                                
      RHS(I)=0.                                                                 
  100 CONTINUE                                                                  
C                                                                               
C2------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE BAS1OC(NSTP,KSTP,KPER,ISTRT,ICNVG,IOFLG,NLAY,                  
     1      IBUDFL,ICBCFL,IHDDFL,INOC,IOUT)                                     
C                                                                               
C-----VERSION 0949 03NOV1982 BAS1OC                                             
C     ******************************************************************        
C     OUTPUT CONTROLLER FOR HEAD, DRAWDOWN, AND BUDGET                          
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION IOFLG(NLAY,4)                                                   
C     ------------------------------------------------------------------        
C                                                                               
C                                                                               
C1------TEST UNIT NUMBER (INOC (INOC=IUNIT(12))) TO SEE IF                      
C1------OUTPUT CONTROL IS ACTIVE.                                               
      IF(INOC.NE.0)GO TO 500                                                    
C                                                                               
C2------IF OUTPUT CONTROL IS INACTIVE THEN SET DEFAULTS AND RETURN.             
      IHDDFL=0                                                                  
      IF(ICNVG.EQ.0 .OR. KSTP.EQ.NSTP)IHDDFL=1                                  
      IBUDFL=0                                                                  
      IF(ICNVG.EQ.0 .OR. KSTP.EQ.NSTP)IBUDFL=1                                  
      ICBCFL=0                                                                  
      GO TO 1000                                                                
C                                                                               
C3-------READ AND PRINT OUTPUT FLAGS AND CODE FOR DEFINING IOFLG.               
  500 READ(INOC,1) INCODE,IHDDFL,IBUDFL,ICBCFL                                  
    1 FORMAT(4I10)                                                              
      WRITE(IOUT,3) IHDDFL,IBUDFL,ICBCFL                                        
    3 FORMAT(1H0,'HEAD/DRAWDOWN PRINTOUT FLAG =',I2,                            
     1    5X,'TOTAL BUDGET PRINTOUT FLAG =',I2,                                 
     2    5X,'CELL-BY-CELL FLOW TERM FLAG =',I2)                                
C                                                                               
C4------DECODE INCODE TO DETERMINE HOW TO SET FLAGS IN IOFLG.                   
      IF(INCODE) 100,200,300                                                    
C                                                                               
C5------USE IOFLG FROM LAST TIME STEP.                                          
  100 WRITE(IOUT,101)                                                           
  101 FORMAT(1H ,'REUSING PREVIOUS VALUES OF IOFLG')                            
      GO TO 600                                                                 
C                                                                               
C6------READ IOFLG FOR LAYER 1 AND ASSIGN SAME TO ALL LAYERS                    
  200 READ(INOC,201) (IOFLG(1,M),M=1,4)                                         
  201 FORMAT(4I10)                                                              
      DO 210 K=1,NLAY                                                           
      IOFLG(K,1)=IOFLG(1,1)                                                     
      IOFLG(K,2)=IOFLG(1,2)                                                     
      IOFLG(K,3)=IOFLG(1,3)                                                     
      IOFLG(K,4)=IOFLG(1,4)                                                     
  210 CONTINUE                                                                  
      WRITE(IOUT,211) (IOFLG(1,M),M=1,4)                                        
  211 FORMAT(1H0,'OUTPUT FLAGS FOR ALL LAYERS ARE THE SAME:'/                   
     1   1X,'  HEAD    DRAWDOWN  HEAD  DRAWDOWN'/                               
     2   1X,'PRINTOUT  PRINTOUT  SAVE    SAVE'/                                 
     3   1X,34('-')/1X,I5,I10,I8,I8)                                            
      GO TO 600                                                                 
C                                                                               
C7------READ IOFLG IN ENTIRETY                                                  
  300 READ(INOC,301) ((IOFLG(K,I),I=1,4),K=1,NLAY)                              
  301 FORMAT(4I10)                                                              
      WRITE(IOUT,302)                                                           
  302 FORMAT(1H0,'OUTPUT FLAGS FOR EACH LAYER:'/                                
     1   1X,'         HEAD    DRAWDOWN  HEAD  DRAWDOWN'/                        
     2   1X,'LAYER  PRINTOUT  PRINTOUT  SAVE    SAVE'/                          
     3   1X,41('-'))                                                            
      WRITE(IOUT,303) (K,(IOFLG(K,I),I=1,4),K=1,NLAY)                           
  303 FORMAT(1X,I4,I8,I10,I8,I8)                                                
C                                                                               
C8------THE LAST STEP IN A STRESS PERIOD AND STEPS WHERE ITERATIVE              
C8------PROCEDURE FAILED TO CONVERGE GET A VOLUMETRIC BUDGET.                   
  600 IF(ICNVG.EQ.0 .OR. KSTP.EQ.NSTP) IBUDFL=1                                 
C                                                                               
C9------RETURN                                                                  
 1000 RETURN                                                                    
      END                                                                       
      SUBROUTINE BAS1OT(HNEW,STRT,ISTRT,BUFF,IOFLG,MSUM,IBOUND,VBNM,            
     1  VBVL,KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,NCOL,NROW,NLAY,ICNVG,           
     2  IHDDFL,IBUDFL,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IOUT)                         
C-----VERSION 0951 03NOV1982 BAS1OT                                             
C     ******************************************************************        
C     OUTPUT TIME, VOLUMETRIC BUDGET, HEAD, AND DRAWDOWN                        
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
C                                                                               
      DIMENSION HNEW(NCOL,NROW,NLAY),STRT(NCOL,NROW,NLAY),                      
     1          VBNM(1),VBVL(1),IOFLG(NLAY,4),                                  
     2          IBOUND(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY)                     
C     ------------------------------------------------------------------        
C                                                                               
C1------CLEAR PRINTOUT FLAG (IPFLG)                                             
      IPFLG=0                                                                   
C                                                                               
C2------IF ITERATIVE PROCEDURE FAILED TO CONVERGE PRINT MESSAGE                 
      IF(ICNVG.EQ.0) WRITE(IOUT,1) KSTP,KPER                                    
    1 FORMAT(1H0,10X,'****FAILED TO CONVERGE IN TIME STEP',I3,                  
     1      ' OF STRESS PERIOD',I3,'****')                                      
C                                                                               
C3------IF HEAD AND DRAWDOWN FLAG (IHDDFL) IS SET WRITE HEAD AND                
C3------DRAWDOWN IN ACCORDANCE WITH FLAGS IN IOFLG.                             
      IF(IHDDFL.EQ.0) GO TO 100                                                 
C                                                                               
      CALL SBAS1H(HNEW,BUFF,IOFLG,KSTP,KPER,NCOL,NROW,                          
     1    NLAY,IOUT,IHEDFM,IHEDUN,IPFLG)                                        
      CALL SBAS1D(HNEW,BUFF,IOFLG,KSTP,KPER,NCOL,NROW,                          
     1    NLAY,IOUT,IDDNFM,IDDNUN,STRT,ISTRT,IBOUND,IPFLG)                      
C                                                                               
C4------PRINT TOTAL BUDGET IF REQUESTED                                         
  100 IF(IBUDFL.EQ.0) GO TO 120                                                 
      CALL SBAS1V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT)                                
      IPFLG=1                                                                   
C                                                                               
C5------END PRINTOUT WITH TIME SUMMARY AND FORM FEED IF ANY PRINTOUT            
C5------WILL BE PRODUCED.                                                       
  120 IF(IPFLG.EQ.0) RETURN                                                     
      CALL SBAS1T(KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,IOUT)                      
      WRITE(IOUT,101)                                                           
  101 FORMAT(1H1)                                                               
C                                                                               
C6------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE BAS1RP(IBOUND,HNEW,STRT,HOLD,ISTRT,INBAS,                      
     1    HEADNG,NCOL,NROW,NLAY,NODES,VBVL,IOFLG,INOC,IHEDFM,                   
     2    IDDNFM,IHEDUN,IDDNUN,IOUT)                                            
C-----VERSION 0956 03NOV1982 BAS1RP                                             
C     ******************************************************************        
C     READ AND INITIALIZE BASIC MODEL ARRAYS                                    
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW,HNOFLO                                              
C                                                                               
      DIMENSION HNEW(NODES),IBOUND(NODES),STRT(NODES),HOLD(NODES),              
     1             ANAME(6,2),VBVL(4,20),IOFLG(NLAY,4),HEADNG(32)               
C                                                                               
CCC   CHARACTER ANAME*4                                                         
      DATA ANAME(1,1),ANAME(2,1),ANAME(3,1),ANAME(4,1),ANAME(5,1),              
     1   ANAME(6,1) /'    ','    ','  BO','UNDA','RY A','RRAY'/                 
      DATA ANAME(1,2),ANAME(2,2),ANAME(3,2),ANAME(4,2),ANAME(5,2),              
     1   ANAME(6,2) /'    ','    ','    ','INIT','IAL ','HEAD'/                 
C     ------------------------------------------------------------------        
C                                                                               
C1------PRINT SIMULATION TITLE, CALCULATE # OF CELLS IN A LAYER.                
      WRITE(IOUT,1) HEADNG                                                      
    1 FORMAT(1H1,32A4)                                                          
      NCR=NCOL*NROW                                                             
C                                                                               
C2------READ BOUNDARY ARRAY(IBOUND) ONE LAYER AT A TIME.                        
      DO 100 K=1,NLAY                                                           
      LOC=1+(K-1)*NCR                                                           
      CALL U2DINT(IBOUND(LOC),ANAME(1,1),NROW,NCOL,K,INBAS,IOUT)                
  100 CONTINUE                                                                  
C                                                                               
C3------READ AND PRINT HEAD VALUE TO BE PRINTED FOR NO-FLOW CELLS.              
      READ(INBAS,2) TMP                                                         
    2 FORMAT(F10.0)                                                             
      HNOFLO=TMP                                                                
      WRITE(IOUT,3) TMP                                                         
    3 FORMAT(1H0,'AQUIFER HEAD WILL BE SET TO ',1PG11.5,                        
     1       ' AT ALL NO-FLOW NODES (IBOUND=0).')                               
C                                                                               
C4------READ STARTING HEADS.                                                    
      DO 300 K=1,NLAY                                                           
      LOC=1+(K-1)*NCR                                                           
      CALL U2DREL(HOLD(LOC),ANAME(1,2),NROW,NCOL,K,INBAS,IOUT)                  
  300 CONTINUE                                                                  
C                                                                               
C5------COPY INITIAL HEADS FROM HOLD TO HNEW.                                   
      DO 400 I=1,NODES                                                          
      HNEW(I)=HOLD(I)                                                           
      IF(IBOUND(I).EQ.0) HNEW(I)=HNOFLO                                         
  400 CONTINUE                                                                  
C                                                                               
C6------IF STARTING HEADS ARE TO BE SAVED THEN COPY HOLD TO STRT.               
      IF(ISTRT.EQ.0) GO TO 590                                                  
      DO 500 I=1,NODES                                                          
      STRT(I)=HOLD(I)                                                           
  500 CONTINUE                                                                  
C                                                                               
C7------INITIALIZE VOLUMETRIC BUDGET ACCUMULATORS TO ZERO.                      
  590 DO 600 I=1,20                                                             
      DO 600 J=1,4                                                              
      VBVL(J,I)=0.                                                              
  600 CONTINUE                                                                  
C                                                                               
C8------SET UP OUTPUT CONTROL.                                                  
      CALL SBAS1I(NLAY,ISTRT,IOFLG,INOC,IOUT,IHEDFM,                            
     1         IDDNFM,IHEDUN,IDDNUN)                                            
C                                                                               
C9------RETURN                                                                  
 1000 RETURN                                                                    
      END                                                                       
      SUBROUTINE BAS1ST(NSTP,DELT,TSMULT,PERTIM,KPER,INBAS,IOUT)                
C                                                                               
C                                                                               
C-----VERSION 1614 08SEP1982 BAS1ST                                             
C     ******************************************************************        
C     SETUP TIME PARAMETERS FOR NEW TIME PERIOD                                 
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
C                                                                               
C1------READ LENGTH OF STRESS PERIOD, NUMBER OF TIME STEPS AND.                 
C1------TIME STEP MULTIPLIER.                                                   
      READ (INBAS,1) PERLEN,NSTP,TSMULT                                         
    1 FORMAT(F10.0,I10,F10.0)                                                   
C                                                                               
C2------CALCULATE THE LENGTH OF THE FIRST TIME STEP.                            
C                                                                               
C2A-----ASSUME TIME STEP MULTIPLIER IS EQUAL TO ONE.                            
      DELT=PERLEN/FLOAT(NSTP)                                                   
C                                                                               
C2B-----IF TIME STEP MULTIPLIER IS NOT ONE THEN CALCULATE FIRST                 
C2B-----TERM OF GEOMETRIC PROGRESSION.                                          
      IF(TSMULT.NE.1.) DELT=PERLEN*(1.-TSMULT)/(1.-TSMULT**NSTP)                
C                                                                               
C3------PRINT TIMING INFORMATION.                                               
      WRITE (IOUT,2) KPER,PERLEN,NSTP,TSMULT,DELT                               
    2 FORMAT(1H1,51X,'STRESS PERIOD NO.',I4,', LENGTH =',G15.7/52X              
     1,46('-')//52X,'NUMBER OF TIME STEPS =',I6                                 
     2//53X,'MULTIPLIER FOR DELT =',F10.3                                       
     3//50X,'INITIAL TIME STEP SIZE =',G15.7)                                   
C                                                                               
C4------INITIALIZE PERTIM (ELAPSED TIME WITHIN STRESS PERIOD).                  
      PERTIM=0.                                                                 
C                                                                               
C5------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SBAS1D(HNEW,BUFF,IOFLG,KSTP,KPER,NCOL,NROW,                    
     1    NLAY,IOUT,IDDNFM,IDDNUN,STRT,ISTRT,IBOUND,IPFLG)                      
C                                                                               
C-----VERSION 1133 28DEC1983 SBAS1D                                             
C     *******************************************************                   
C     CALCULATE PRINT AND RECORD DRAWDOWNS                                      
C     *******************************************************                   
C                                                                               
C        SPECIFICATIONS                                                         
C     -------------------------------------------------------                   
      DOUBLE PRECISION HNEW                                                     
C                                                                               
      DIMENSION HNEW(NCOL,NROW,NLAY),IOFLG(NLAY,4),TEXT(4),                     
     1     BUFF(NCOL,NROW,NLAY),STRT(NCOL,NROW,NLAY),                           
     2     IBOUND(NCOL,NROW,NLAY)                                               
C                                                                               
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /'    ','    ','DRAW',               
     1       'DOWN'/                                                            
C     -------------------------------------------------------                   
C                                                                               
C1------FOR EACH LAYER CALCULATE DRAWDOWN IF PRINT OR RECORD                    
C1------IS REQUESTED                                                            
      DO 59 K=1,NLAY                                                            
C                                                                               
C2------IS DRAWDOWN NEEDED FRO THIS LAYER?                                      
      IF(IOFLG(K,2).EQ.0 .AND. IOFLG(K,4).EQ.0) GO TO 59                        
C                                                                               
C3------DRAWDOWN IS NEEDED. WERE STARTING HEADS SAVED?                          
      IF(ISTRT.NE.0) GO TO 53                                                   
C                                                                               
C4------STARTING HEADS WERE NOT SAVED. PRINT MESSAGE AND STOP.                  
      WRITE(IOUT,52)                                                            
   52 FORMAT(1H0,'CANNOT CALCULATE DRAWDOWN BECAUSE START',                     
     1   ' HEADS WERE NOT SAVED')                                               
      STOP                                                                      
C                                                                               
C5------CALCULATE DRAWDOWN FOR THE LAYER.                                       
   53 DO 58 I=1,NROW                                                            
      DO 58 J=1,NCOL                                                            
      HSING=HNEW(J,I,K)                                                         
      BUFF(J,I,K)=HSING                                                         
      IF(IBOUND(J,I,K).NE.0) BUFF(J,I,K)=STRT(J,I,K)-HSING                      
   58 CONTINUE                                                                  
   59 CONTINUE                                                                  
C                                                                               
C6------FOR EACH LAYER: DETERMINE IF DRAWDOWN SHOULD BE PRINTED.                
C6------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT DRAWDOWN.                     
      DO 69 K=1,NLAY                                                            
      IF(IOFLG(K,2).EQ.0) GO TO 69                                              
      IF(IDDNFM.LT.0) CALL ULAPRS(BUFF(1,1,K),TEXT(1),KSTP,KPER,                
                                                                                
     1             NCOL,NROW,K,-IDDNFM,IOUT)                                    
      IF(IDDNFM.GE.0) CALL ULAPRW(BUFF(1,1,K),TEXT(1),KSTP,KPER,                
     1             NCOL,NROW,K,IDDNFM,IOUT)                                     
      IPFLG=1                                                                   
   69 CONTINUE                                                                  
C                                                                               
C7------FOR EACH LAYER: DETERMINE IF DRAWDOWN SHOULD BE RECORDED.               
C7------IF SO THEN CALL ULASAV TO RECORD DRAWDOWN.                              
      IFIRST=1                                                                  
      IF(IDDNUN.LE.0) GO TO 80                                                  
      DO 79 K=1,NLAY                                                            
      IF(IOFLG(K,4).LE.0) GO TO 79                                              
      IF(IFIRST.EQ.1) WRITE(IOUT,74) IDDNUN,KSTP,KPER                           
   74 FORMAT(1H0,'DRAWDOWN WILL BE SAVED ON UNIT',I3,                           
     1    ' AT END OF TIME STEP',I3,', STRESS PERIOD',I3)         
      IF (IFIRST.EQ.1) WRITE(IDDNUN,81) KSTP,KPER
      IF (IFIRST.EQ.1) WRITE(IDDNUN,84) NROW,NCOL
   84 FORMAT(2I5)
   81 FORMAT(1HO,'DRAWDOWN FOR TIME STEP',I3,'STRESS PERIOD',I3)            
      IFIRST=0                                                                  
C      CALL ULASAV(BUFF(1,1,K),TEXT(1),KSTP,KPER,PERTIM,TOTIM,NCOL            
C     1              NROW,K,IDDNUN)
      DO 82 I=1,NROW
      WRITE(IDDNUN,83) (BUFF(J,I,K),J=1,NCOL)
   83 FORMAT(15F7.2)
   82 CONTINUE                                           
   79 CONTINUE                                                                  
C                                                                               
C8------RETURN                                                                  
   80 RETURN                                                                    
      END                                                                       
      SUBROUTINE SBAS1H(HNEW,BUFF,IOFLG,KSTP,KPER,NCOL,NROW,                    
     1    NLAY,IOUT,IHEDFM,IHEDUN,IPFLG)                                        
C                                                                               
C-----VERSION 1247 28DEC1983 SBAS1H                                             
C     *******************************************************                   
C     PRINT AND RECORD HEADS                                                    
C     *******************************************************                   
C                                                                               
C        SPECIFICATIONS                                                         
C     -------------------------------------------------------                   
      DOUBLE PRECISION HNEW                                                     
C                                                                               
      DIMENSION HNEW(NCOL,NROW,NLAY),IOFLG(NLAY,4),TEXT(4),                     
     1     BUFF(NCOL,NROW,NLAY)                                                 
C                                                                               
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /'    ','    ','    ',               
     1       'HEAD'/                                                            
C     -------------------------------------------------------                   
C                                                                               
C1------FOR EACH LAYER: PRINT HEAD IF REQUESTED.                                
      DO 39 K=1,NLAY                                                            
C                                                                               
C2------TEST IOFLG TO SEE IF HEAD SHOULD BE PRINTED.                            
      IF(IOFLG(K,1).EQ.0) GO TO 39                                              
      IPFLG=1                                                                   
C                                                                               
C3------COPY HEADS FOR THIS LAYER INTO BUFFER.                                  
      DO 32 I=1,NROW                                                            
      DO 32 J=1,NCOL                                                            
      BUFF(J,I,1)=HNEW(J,I,K)                                                   
   32 CONTINUE                                                                  
C                                                                               
C4------CALL UTILITY MODULE TO PRINT CONTENTS OF BUFFER.                        
      IF(IHEDFM.LT.0) CALL ULAPRS(BUFF,TEXT(1),KSTP,KPER,NCOL,NROW,K,           
     1          -IHEDFM,IOUT)                                                   
      IF(IHEDFM.GE.0) CALL ULAPRW(BUFF,TEXT(1),KSTP,KPER,NCOL,NROW,K,           
     1            IHEDFM,IOUT)                                                  
   39 CONTINUE                                                                  
C                                                                               
C5------IF UNIT FOR RECORDING HEADS <= 0: THEN RETURN.                          
      IF(IHEDUN.LE.0)GO TO 50                                                   
      IFIRST=1                                                                  
C                                                                               
C6------FOR EACH LAYER: RECORD HEAD IF REQUESTED.                               
      DO 49 K=1,NLAY                                                            
C                                                                               
C7------CHECK IOFLG TO SEE IF HEAD FOR THIS LAYER SHOULD BE RECORDED.           
      IF(IOFLG(K,3).LE.0) GO TO 49                                              
      IF(IFIRST.EQ.1) WRITE(IOUT,41) IHEDUN,KSTP,KPER                           
   41 FORMAT(1H0,'HEAD WILL BE SAVED ON UNIT',I3,' AT END OF TIME STEP',        
     1    I3,', STRESS PERIOD',I3)                                              
      IFIRST=0                                                                  
C                                                                               
C8------COPY HEADS FOR THIS LAYER INTO BUFFER.                                  
      DO 44 I=1,NROW                                                            
      DO 44 J=1,NCOL                                                            
      BUFF(J,I,1)=HNEW(J,I,K)                                                   
   44 CONTINUE                                                                  
C                                                                               
C9------RECORD CONTENTS OF BUFFER ON UNIT=IHEDUN                                
      CALL ULASAV(BUFF,TEXT(1),KSTP,KPER,PERTIM,TOTIM,NCOL,NROW,K,              
     1            IHEDUN)                                                       
   49 CONTINUE                                                                  
C                                                                               
C10-----RETURN                                                                  
   50 RETURN                                                                    
      END                                                                       
      SUBROUTINE SBAS1I(NLAY,ISTRT,IOFLG,INOC,IOUT,IHEDFM,                      
     1         IDDNFM,IHEDUN,IDDNUN)                                            
C                                                                               
C-----VERSION 1138 03NOV1982 SBAS1I                                             
C     **************************************************************            
C     SET UP OUTPUT CONTROL                                                     
C     **************************************************************            
C                                                                               
C         SPECIFICATIONS                                                        
C                                                                               
      DIMENSION IOFLG(NLAY,4)                                                   
C     ---------------------------------------------------------------           
C                                                                               
C1------TEST UNIT NUMBER FROM IUNIT (INOC) TO SEE IF OUTPUT                     
C1------CONTROL IS ACTIVE.                                                      
      IF(INOC.LE.0) GO TO 600                                                   
C                                                                               
C2------READ AND PRINT FORMATS FOR PRINTING AND UNIT NUMBERS FOR                
C2------RECORDING HEADS AND DRAWDOWN. THEN RETURN.                              
  500 READ (INOC,1)IHEDFM,IDDNFM,IHEDUN,IDDNUN                                  
    1 FORMAT (4I10)                                                             
      WRITE (IOUT,3)IHEDFM,IDDNFM                                               
    3 FORMAT (1H0,'HEAD PRINT FORMAT IS FORMAT NUMBER',I4,                      
     1        '    DRAWDOWN PRINT FORMAT IS FORMAT NUMBER',I4)                  
      WRITE (IOUT,4)IHEDUN,IDDNUN                                               
    4 FORMAT (1H0,'HEADS WILL BE SAVED ON UNIT',I3,                             
     1        '    DRAWDOWNS WILL BE SAVED ON UNIT',I3)                         
      WRITE(IOUT,561)                                                           
  561 FORMAT(1H0,'OUTPUT CONTROL IS SPECIFIED EVERY TIME STEP')                 
      GO TO 1000                                                                
C                                                                               
C3------OUTPUT CONTROL IS INACTIVE. PRINT A MESSAGE LISTING DEFAULTS.           
  600 WRITE(IOUT,641)                                                           
  641 FORMAT(1H0,'DEFAULT OUTPUT CONTROL -- THE FOLLOWING OUTPUT',              
     1      ' COMES AT THE END OF EACH STRESS PERIOD:')                         
      WRITE(IOUT,642)                                                           
  642 FORMAT(1X,'TOTAL VOLUMETRIC BUDGET')                                      
      WRITE(IOUT,643)                                                           
  643 FORMAT(1X,10X,'HEAD')                                                     
      IF(ISTRT.NE.0)WRITE(IOUT,644)                                             
  644 FORMAT(1X,10X,'DRAWDOWN')                                                 
C                                                                               
C4------SET THE FORMAT CODES EQUAL TO THE DEFAULT FORMAT.                       
      IHEDFM=0                                                                  
      IDDNFM=0                                                                  
C                                                                               
C5------SET DEFAULT FLAGS IN IOFLG SO THAT HEAD (AND DRAWDOWN) IS               
C5------PRINTED FOR EVERY LAYER.                                                
      ID=0                                                                      
      IF(ISTRT.NE.0) ID=1                                                       
  670 DO 680 K=1,NLAY                                                           
      IOFLG(K,1)=1                                                              
      IOFLG(K,2)=ID                                                             
      IOFLG(K,3)=0                                                              
      IOFLG(K,4)=0                                                              
  680 CONTINUE                                                                  
      GO TO 1000                                                                
C                                                                               
C6------RETURN                                                                  
1000  RETURN                                                                    
      END                                                                       
      SUBROUTINE SBAS1T(KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,IOUT)                
C                                                                               
C                                                                               
C-----VERSION 0837 09APR1982 SBAS1T                                             
C     ******************************************************************        
C     PRINT SIMULATION TIME                                                     
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
      WRITE(IOUT,199) KSTP,KPER                                                 
  199 FORMAT(1H0,///10X,'TIME SUMMARY AT END OF TIME STEP',I3,                  
     1     ' IN STRESS PERIOD',I3)                                              
C                                                                               
C1------USE TIME UNIT INDICATOR TO GET FACTOR TO CONVERT TO SECONDS.            
      CNV=0.                                                                    
      IF(ITMUNI.EQ.1) CNV=1.                                                    
      IF(ITMUNI.EQ.2) CNV=60.                                                   
      IF(ITMUNI.EQ.3) CNV=3600.                                                 
      IF(ITMUNI.EQ.4) CNV=86400.                                                
      IF(ITMUNI.EQ.5) CNV=31557600.                                             
C                                                                               
C2------IF FACTOR=0 THEN TIME UNITS ARE NON-STANDARD.                           
      IF(CNV.NE.0.) GO TO 100                                                   
C                                                                               
C2A-----PRINT TIMES IN NON-STANDARD TIME UNITS.                                 
      WRITE(IOUT,301) DELT,PERTIM,TOTIM                                         
  301 FORMAT(21X,'     TIME STEP LENGTH =',G15.6/                               
     1       21X,'   STRESS PERIOD TIME =',G15.6/                               
     2       21X,'TOTAL SIMULATION TIME =',G15.6)                               
C                                                                               
C2B-----RETURN                                                                  
      RETURN                                                                    
C                                                                               
C3------CALCULATE LENGTH OF TIME STEP & ELAPSED TIMES IN SECONDS.               
  100 DELSEC=CNV*DELT                                                           
      TOTSEC=CNV*TOTIM                                                          
      PERSEC=CNV*PERTIM                                                         
C                                                                               
C4------CALCULATE TIMES IN MINUTES,HOURS,DAYS AND YEARS.                        
      DELMN=DELSEC/60.                                                          
      DELHR=DELMN/60.                                                           
      DELDY=DELHR/24.                                                           
      DELYR=DELDY/365.25                                                        
      TOTMN=TOTSEC/60.                                                          
      TOTHR=TOTMN/60.                                                           
      TOTDY=TOTHR/24.                                                           
      TOTYR=TOTDY/365.25                                                        
      PERMN=PERSEC/60.                                                          
      PERHR=PERMN/60.                                                           
      PERDY=PERHR/24.                                                           
      PERYR=PERDY/365.25                                                        
C                                                                               
C5------PRINT TIME STEP LENGTH AND ELAPSED TIMES IN ALL TIME UNITS.             
      WRITE(IOUT,200)                                                           
  200 FORMAT(27X,'    SECONDS        MINUTES         HOURS',10X,                
     1    'DAYS           YEARS'/27X,75('-'))                                   
      WRITE (IOUT,201) DELSEC,DELMN,DELHR,DELDY,DELYR                           
  201 FORMAT(1X,'     TIME STEP LENGTH',5X,5G15.6)                              
      WRITE(IOUT,202) PERSEC,PERMN,PERHR,PERDY,PERYR                            
  202 FORMAT(1X,'   STRESS PERIOD TIME',5X,5G15.6)                              
      WRITE(IOUT,203) TOTSEC,TOTMN,TOTHR,TOTDY,TOTYR                            
  203 FORMAT(1X,'TOTAL SIMULATION TIME',5X,5G15.6)                              
C                                                                               
C6------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SBAS1V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT)                          
C                                                                               
C                                                                               
C-----VERSION 1153 03NOV1982 SBAS1V                                             
C     ******************************************************************        
C     PRINT VOLUMETRIC BUDGET                                                   
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
      DIMENSION VBNM(4,20),VBVL(4,20)                                           
C     ------------------------------------------------------------------        
C                                                                               
C1------DETERMINE NUMBER OF INDIVIDUAL BUDGET ENTRIES.                          
      MSUM1=MSUM-1                                                              
      IF(MSUM1.LE.0) RETURN                                                     
C                                                                               
C2------CLEAR RATE AND VOLUME ACCUMULATORS.                                     
      TOTRIN=0.                                                                 
      TOTROT=0.                                                                 
      TOTVIN=0.                                                                 
      TOTVOT=0.                                                                 
C                                                                               
C3------ADD RATES AND VOLUMES (IN AND OUT) TO ACCUMULATORS.                     
      DO 100 L=1,MSUM1                                                          
      TOTRIN=TOTRIN+VBVL(3,L)                                                   
      TOTROT=TOTROT+VBVL(4,L)                                                   
      TOTVIN=TOTVIN+VBVL(1,L)                                                   
      TOTVOT=TOTVOT+VBVL(2,L)                                                   
  100 CONTINUE                                                                  
C                                                                               
C4------PRINT TIME STEP NUMBER AND STRESS PERIOD NUMBER.                        
      WRITE(IOUT,260) KSTP,KPER                                                 
      WRITE(IOUT,265)                                                           
C                                                                               
C5------PRINT INDIVIDUAL INFLOW RATES AND VOLUMES AND THEIR TOTALS.             
      DO 200 L=1,MSUM1                                                          
      WRITE(IOUT,275) (VBNM(I,L),I=1,4),VBVL(1,L),(VBNM(I,L),I=1,4)             
     1,VBVL(3,L)                                                                
  200 CONTINUE                                                                  
      WRITE(IOUT,286) TOTVIN,TOTRIN                                             
C                                                                               
C6------PRINT INDIVIDUAL OUTFLOW RATES AND VOLUMES AND THEIR TOTALS.            
      WRITE(IOUT,287)                                                           
      DO 250 L=1,MSUM1                                                          
      WRITE(IOUT,275) (VBNM(I,L),I=1,4),VBVL(2,L),(VBNM(I,L),I=1,4)             
     1,VBVL(4,L)                                                                
  250 CONTINUE                                                                  
      WRITE(IOUT,298) TOTVOT,TOTROT                                             
C                                                                               
C7------CALCULATE THE DIFFERENCE BETWEEN INFLOW AND OUTFLOW.                    
C                                                                               
C7A-----CALCULATE DIFFERENCE BETWEEN RATE IN AND RATE OUT.                      
      DIFFR=TOTRIN-TOTROT                                                       
C                                                                               
C7B-----CALCULATE PERCENT DIFFERENCE BETWEEN RATE IN AND RATE OUT.              
      PDIFFR=100.*DIFFR/((TOTRIN+TOTROT)/2)                                     
C                                                                               
C7C-----CALCULATE DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.                  
      DIFFV=TOTVIN-TOTVOT                                                       
C                                                                               
C7D-----GET PERCENT DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.                
      PDIFFV=100.*DIFFV/((TOTVIN+TOTVOT)/2)                                     
C                                                                               
C8------PRINT DIFFERENCES AND PERCENT DIFFERENCES BETWEEN INPUT                 
C8------AND OUTPUT RATES AND VOLUMES.                                           
      WRITE(IOUT,299) DIFFV,DIFFR                                               
      WRITE(IOUT,300) PDIFFV,PDIFFR                                             
C                                                                               
C9------RETURN                                                                  
      RETURN                                                                    
C                                                                               
C    ---FORMATS                                                                 
C                                                                               
  260 FORMAT(1H0,///30X,'VOLUMETRIC BUDGET FOR ENTIRE MODEL AT END OF'          
     1,' TIME STEP',I3,' IN STRESS PERIOD',I3/30X,77('-'))                      
  265 FORMAT(1H0,19X,'CUMULATIVE VOLUMES',6X,'L**3',37X                         
     1,'RATES FOR THIS TIME STEP',6X,'L**3/T'/20X,18('-'),47X,24('-')           
     2//26X,'IN:',68X,'IN:'/26X,'---',68X,'---')                                
  275 FORMAT(1X,18X,4A4,' =',G14.5,39X,4A4,' =',G14.5)                          
  286 FORMAT(1H0,26X,'TOTAL IN =',G14.5,47X,'TOTAL IN ='                        
     1,G14.5)                                                                   
  287 FORMAT(1H0,24X,'OUT:',67X,'OUT:'/25X,4('-'),67X,4('-'))                   
  298 FORMAT(1H0,25X,'TOTAL OUT =',G14.5,46X,'TOTAL OUT ='                      
     1,G14.5)                                                                   
  299 FORMAT(1H0,26X,'IN - OUT =',G14.5,47X,'IN - OUT =',G14.5)                 
  300 FORMAT(1H0,15X,'PERCENT DISCREPANCY =',F20.2                              
     1,30X,'PERCENT DISCREPANCY =',F20.2,///)                                   
C                                                                               
      END                                                                       
      SUBROUTINE BCF1AL(ISUM,LENX,LCSC1,LCHY,LCBOT,                             
     1     LCTOP,LCSC2,LCTRPY,IN,ISS,NCOL,NROW,NLAY,IOUT,IBCFCB)                
C                                                                               
C-----VERSION 0931 08DEC1983 BCF1AL                                             
C                                                                               
C     ******************************************************************        
C     ALLOCATE ARRAY STORAGE FOR BLOCK-CENTERED FLOW PACKAGE                    
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      COMMON /FLWCOM/LAYCON(80)                                                 
C     ------------------------------------------------------------------        
C                                                                               
C1------IDENTIFY PACKAGE                                                        
      WRITE(IOUT,1)IN                                                           
    1 FORMAT(1H0,'BCF1 -- BLOCK-CENTERED FLOW PACKAGE, VERSION 1',              
     1', 12/08/83',' INPUT READ FROM UNIT',I3)                                  
C                                                                               
C2------READ AND PRINT ISS (STEADY-STATE FLAG) AND IBCFCB (FLAG FOR             
C2------PRINTING OR UNIT# FOR RECORDING CELL-BY-CELL FLOW TERMS)                
      READ(IN,2) ISS,IBCFCB                                                     
    2 FORMAT(2I10)                                                              
      IF(ISS.EQ.0) WRITE(IOUT,3)                                                
    3 FORMAT(1X,'TRANSIENT SIMULATION')                                         
      IF(ISS.NE.0) WRITE(IOUT,4)                                                
    4 FORMAT(1X,'STEADY-STATE SIMULATION')                                      
      IF(IBCFCB.GT.0) WRITE(IOUT,9) IBCFCB                                      
    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT',I3)               
      IF(IBCFCB.LT.0) WRITE(IOUT,88)                                            
   88 FORMAT(1X,'CONSTANT HEAD CELL-BY-CELL FLOWS WILL BE PRINTED')             
C                                                                               
C3------READ TYPE CODE FOR EACH LAYER AND COUNT TOPS AND BOTTOMS                
      IF(NLAY.LE.80) GO TO 50                                                   
      WRITE(IOUT,11)                                                            
   11 FORMAT(1H0,'YOU HAVE SPECIFIED MORE THAN 80 MODEL LAYERS'/1X,             
     1  'SPACE IS RESERVED FOR A MAXIMUM OF 80 LAYERS IN ARRAY LAYCON')         
      STOP                                                                      
C                                                                               
C3A-----READ LAYER TYPE CODES.                                                  
   50 READ(IN,51) (LAYCON(I),I=1,NLAY)                                          
   51 FORMAT(40I2)                                                              
C        BOTTOM IS READ FOR TYPES 1,3    TOP IS READ FOR TYPES 2,3              
      WRITE(IOUT,52)                                                            
   52 FORMAT(1X,5X,'LAYER  AQUIFER TYPE',/1X,5X,19('-'))                        
C                                                                               
C3B-----INITIALIZE TOP AND BOTTOM COUNTERS.                                     
      NBOT=0                                                                    
      NTOP=0                                                                    
C                                                                               
C3C------PRINT LAYER TYPE AND COUNT TOPS AND BOTTOMS NEEDED.                    
      DO 100 I=1,NLAY                                                           
C                                                                               
C3C1----PRINT LAYER NUMBER AND LAYER TYPE CODE.                                 
      L=LAYCON(I)                                                               
      WRITE(IOUT,7) I,L                                                         
    7 FORMAT(1X,I9,I10)                                                         
C                                                                               
C3C2----ONLY THE TOP LAYER CAN BE UNCONFINED(LAYCON=1).                         
      IF(L.NE.1 .OR. I.EQ.1) GO TO 70                                           
      WRITE(IOUT,8)                                                             
    8 FORMAT(1H0,'AQUIFER TYPE 1 IS ONLY ALLOWED IN TOP LAYER')                 
      STOP                                                                      
C                                                                               
C3C3----LAYER TYPES 1 AND 3 NEED A BOTTOM. ADD 1 TO KB.                         
   70 IF(L.EQ.1 .OR. L.EQ.3) NBOT=NBOT+1                                        
C                                                                               
C3C4----LAYER TYPES 2 AND 3 NEED A TOP. ADD 1 TO KT.                            
      IF(L.EQ.2 .OR. L.EQ.3) NTOP=NTOP+1                                        
  100 CONTINUE                                                                  
C                                                                               
C                                                                               
C                                                                               
C4------COMPUTE DIMENSIONS FOR ARRAYS.                                          
      NRC=NROW*NCOL                                                             
      ISIZ=NRC*NLAY                                                             
C                                                                               
C5------ALLOCATE SPACE FOR ARRAYS. IF RUN IS TRANSIENT(ISS=0)                   
C5------THEN SPACE MUST BE ALLOCATED FOR STORAGE.                               
      ISOLD=ISUM                                                                
      LCSC1=ISUM                                                                
      IF(ISS.EQ.0) ISUM=ISUM+ISIZ                                               
      LCSC2=ISUM                                                                
      IF(ISS.EQ.0) ISUM=ISUM+NRC*NTOP                                           
      LCTRPY=ISUM                                                               
      ISUM=ISUM+NLAY                                                            
      LCBOT=ISUM                                                                
      ISUM=ISUM+NRC*NBOT                                                        
      LCHY=ISUM                                                                 
      ISUM=ISUM+NRC*NBOT                                                        
      LCTOP=ISUM                                                                
      ISUM=ISUM+NRC*NTOP                                                        
C                                                                               
C6------PRINT THE AMOUNT OF SPACE USED BY THE BCF PACKAGE.                      
      ISP=ISUM-ISOLD                                                            
      WRITE(IOUT,101) ISP                                                       
  101 FORMAT(1X,I6,' ELEMENTS IN X ARRAY ARE USED BY BCF')                      
      ISUM1=ISUM-1                                                              
      WRITE(IOUT,102) ISUM1,LENX                                                
  102 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED OUT OF',I7)                       
      IF(ISUM1.GT.LENX) WRITE(IOUT,103)                                         
  103 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')                  
C                                                                               
C7------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE BCF1RP(IBOUND,HNEW,SC1,HY,CR,CC,CV,DELR,DELC,                  
     1        BOT,TOP,SC2,TRPY,IN,ISS,NCOL,NROW,NLAY,NODES,IOUT)                
C                                                                               
C-----VERSION 1003 03MAY1983 BCF1RP                                             
C                                                                               
C     ******************************************************************        
C     READ AND INITIALIZE DATA FOR BLOCK-CENTERED FLOW PACKAGE                  
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
C                                                                               
      DIMENSION HNEW(NODES),SC1(NODES),HY(NODES),CR(NODES),CC(NODES),           
     1          CV(NODES),ANAME(6,10),DELR(NCOL),DELC(NROW),BOT(NODES),         
     1          TOP(NODES),SC2(NODES),TRPY(NLAY),IBOUND(NODES)                  
C                                                                               
      COMMON /FLWCOM/LAYCON(80)                                                 
C                                                                               
      DATA ANAME(1,1),ANAME(2,1),ANAME(3,1),ANAME(4,1),ANAME(5,1),              
     1  ANAME(6,1) /4H    ,4HPRIM,4HARY ,4HSTOR,4HAGE ,4HCOEF /                 
      DATA ANAME(1,2),ANAME(2,2),ANAME(3,2),ANAME(4,2),ANAME(5,2),              
     1  ANAME(6,2) /4H    ,4HTRAN,4HSMIS,4H. AL,4HONG ,4HROWS /                 
      DATA ANAME(1,3),ANAME(2,3),ANAME(3,3),ANAME(4,3),ANAME(5,3),              
     1  ANAME(6,3) /4H   H,4HYD. ,4HCOND,4H. AL,4HONG ,4HROWS /                 
      DATA ANAME(1,4),ANAME(2,4),ANAME(3,4),ANAME(4,4),ANAME(5,4),              
     1  ANAME(6,4) /4HVERT,4H HYD,4H CON,4HD /T,4HHICK,4HNESS /                 
      DATA ANAME(1,5),ANAME(2,5),ANAME(3,5),ANAME(4,5),ANAME(5,5),              
     1  ANAME(6,5) /4H    ,4H    ,4H    ,4H    ,4H  BO,4HTTOM/                  
      DATA ANAME(1,6),ANAME(2,6),ANAME(3,6),ANAME(4,6),ANAME(5,6),              
     1  ANAME(6,6) /4H    ,4H    ,4H    ,4H    ,4H    ,4H TOP/                  
      DATA ANAME(1,7),ANAME(2,7),ANAME(3,7),ANAME(4,7),ANAME(5,7),              
     1  ANAME(6,7) /4H  SE,4HCOND,4HARY ,4HSTOR,4HAGE ,4HCOEF/                  
      DATA ANAME(1,8),ANAME(2,8),ANAME(3,8),ANAME(4,8),ANAME(5,8),              
     1  ANAME(6,8) /4HCOLU,4HMN T,4HO RO,4HW AN,4HISOT,4HROPY/                  
      DATA ANAME(1,9),ANAME(2,9),ANAME(3,9),ANAME(4,9),ANAME(5,9),              
     1  ANAME(6,9) /4H    ,4H    ,4H    ,4H    ,4H    ,4HDELR/                  
      DATA ANAME(1,10),ANAME(2,10),ANAME(3,10),ANAME(4,10),ANAME(5,10),         
     1  ANAME(6,10) /4H    ,4H    ,4H    ,4H    ,4H    ,4HDELC/                 
C     ------------------------------------------------------------------        
C                                                                               
C1------CALCULATE NUMBER OF NODES IN A LAYER AND READ TRPY,DELR,DELC            
      NIJ=NCOL*NROW                                                             
C                                                                               
      CALL U1DREL(TRPY,ANAME(1,8),NLAY,IN,IOUT)                                 
      CALL U1DREL(DELR,ANAME(1,9),NCOL,IN,IOUT)                                 
      CALL U1DREL(DELC,ANAME(1,10),NROW,IN,IOUT)                                
C                                                                               
C2------READ ALL PARAMETERS FOR EACH LAYER                                      
      KT=0                                                                      
      KB=0                                                                      
      DO 200 K=1,NLAY                                                           
C                                                                               
C2A-----FIND ADDRESS OF EACH LAYER IN THREE DIMENSION ARRAYS.                   
      IF(LAYCON(K).EQ.1 .OR. LAYCON(K).EQ.3) KB=KB+1                            
      IF(LAYCON(K).EQ.2 .OR. LAYCON(K).EQ.3) KT=KT+1                            
      LOC=1+(K-1)*NIJ                                                           
      LOCB=1+(KB-1)*NIJ                                                         
      LOCT=1+(KT-1)*NIJ                                                         
C                                                                               
C2B-----READ PRIMARY STORAGE COEFFICIENT INTO ARRAY SC1 IF TRANSIENT            
      IF(ISS.EQ.0)CALL U2DREL(SC1(LOC),ANAME(1,1),NROW,NCOL,K,IN,IOUT)          
C                                                                               
C2C-----READ TRANSMISSIVITY INTO ARRAY CC IF LAYER TYPE IS 0 OR 2               
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.1) GO TO 100                          
      CALL U2DREL(CC(LOC),ANAME(1,2),NROW,NCOL,K,IN,IOUT)                       
      GO TO 110                                                                 
C                                                                               
C2D-----READ HYDRAULIC CONDUCTIVITY(HY) AND BOTTOM ELEVATION(BOT)               
C2D-----IF LAYER TYPE IS 1 OR 3                                                 
  100 CALL U2DREL(HY(LOCB),ANAME(1,3),NROW,NCOL,K,IN,IOUT)                      
      CALL U2DREL(BOT(LOCB),ANAME(1,5),NROW,NCOL,K,IN,IOUT)                     
C                                                                               
C2E-----READ VERTICAL HYCOND/THICK INTO ARRAY CV IF NOT BOTTOM LAYER            
C2E----- READ AS HYCOND/THICKNESS -- CONVERTED TO CONDUCTANCE LATER             
  110 IF(K.EQ.NLAY) GO TO 120                                                   
      CALL U2DREL(CV(LOC),ANAME(1,4),NROW,NCOL,K,IN,IOUT)                       
C                                                                               
C2F-----READ SECONDARY STORAGE COEFFICIENT INTO ARRAY SC2 IF TRANSIENT          
C2F-----AND LAYER TYPE IS 2 OR 3                                                
  120 IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.2) GO TO 200                         
      IF(ISS.EQ.0)CALL U2DREL(SC2(LOCT),ANAME(1,7),NROW,NCOL,K,IN,IOUT)         
C                                                                               
C2G-----READ TOP ELEVATION(TOP) IF LAYER TYPE IS 2 OR 3                         
      CALL U2DREL(TOP(LOCT),ANAME(1,6),NROW,NCOL,K,IN,IOUT)                     
  200 CONTINUE                                                                  
C                                                                               
C3------PREPARE AND CHECK BCF DATA                                              
      CALL SBCF1N(HNEW,IBOUND,SC1,SC2,CR,CC,CV,HY,TRPY,DELR,DELC,ISS,           
     1         NCOL,NROW,NLAY,IOUT)                                             
C                                                                               
C4------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE BCF1FM(HCOF,RHS,HOLD,SC1,HNEW,IBOUND,CR,CC,CV,HY,TRPY,         
     1                BOT,TOP,SC2,DELR,DELC,DELT,ISS,KITER,KSTP,KPER,           
     2                NCOL,NROW,NLAY,IOUT)                                      
C-----VERSION 1001 03MAY1983 BCF1FM                                             
C                                                                               
C     ******************************************************************        
C     ADD LEAKAGE CORRECTION AND STORAGE TO HCOF AND RHS, AND CALCULATE         
C     CONDUCTANCE AS REQUIRED                                                   
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
C                                                                               
      DIMENSION HCOF(NCOL,NROW,NLAY),RHS(NCOL,NROW,NLAY),                       
     1    HOLD(NCOL,NROW,NLAY),SC1(NCOL,NROW,NLAY),HNEW(NCOL,NROW,NLAY),        
     2    IBOUND(NCOL,NROW,NLAY),CR(NCOL,NROW,NLAY),                            
     3    CC(NCOL,NROW,NLAY),CV(NCOL,NROW,NLAY),HY(NCOL,NROW,NLAY),             
     4    TRPY(NLAY),BOT(NCOL,NROW,NLAY),TOP(NCOL,NROW,NLAY),DELR(NCOL),        
     5    DELC(NROW),SC2(NCOL,NROW,NLAY)                                        
C                                                                               
      COMMON /FLWCOM/LAYCON(80)                                                 
C     ------------------------------------------------------------------        
      KB=0                                                                      
      KT=0                                                                      
C                                                                               
C1------FOR EACH LAYER: IF T VARIES CALCULATE HORIZONTAL CONDUCTANCES           
      DO 100 K=1,NLAY                                                           
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2) KT=KT+1                            
C                                                                               
C1A-----IF LAYER TYPE IS NOT 1 OR 3 THEN SKIP THIS LAYER.                       
      IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.1) GO TO 100                         
      KB=KB+1                                                                   
C                                                                               
C1B-----FOR LAYER TYPES 1 & 3 CALL SBCFH1 TO CALCULATE                          
C1B-----HORIZONTAL CONDUCTANCES.                                                
      CALL SBCF1H(HNEW,IBOUND,CR,CC,CV,HY,TRPY,DELR,DELC,BOT,TOP,               
     1         K,KB,KT,KITER,KSTP,KPER,NCOL,NROW,NLAY,IOUT)                     
  100 CONTINUE                                                                  
C                                                                               
C2------IF THE SIMULATION IS TRANSIENT ADD STORAGE TO HCOF AND RHS              
      IF(ISS.NE.0) GO TO 201                                                    
      TLED=1./DELT                                                              
      KT=0                                                                      
      DO 200 K=1,NLAY                                                           
C                                                                               
C3------SEE IF THIS LAYER IS CONVERTIBLE OR NON-CONVERTIBLE.                    
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2) GO TO 150                          
C4------NON-CONVERTIBLE LAYER, SO USE PRIMARY STORAGE                           
      DO 140 I=1,NROW                                                           
      DO 140 J=1,NCOL                                                           
      RHO=SC1(J,I,K)*TLED                                                       
      HCOF(J,I,K)=HCOF(J,I,K)-RHO                                               
      RHS(J,I,K)=RHS(J,I,K)-RHO*HOLD(J,I,K)                                     
  140 CONTINUE                                                                  
      GO TO 200                                                                 
C                                                                               
C5------A CONVERTIBLE LAYER, SO CHECK OLD AND NEW HEADS TO DETERMINE            
C5------WHEN TO USE PRIMARY AND SECONDARY STORAGE                               
  150 KT=KT+1                                                                   
      DO 180 I=1,NROW                                                           
      DO 180 J=1,NCOL                                                           
C                                                                               
C5A-----IF THE CELL IS EXTERNAL THEN SKIP IT.                                   
      IF(IBOUND(J,I,K).LE.0) GO TO 180                                          
      TP=TOP(J,I,KT)                                                            
      RHO2=SC2(J,I,KT)*TLED                                                     
      RHO1=SC1(J,I,K)*TLED                                                      
C                                                                               
C5B-----FIND STORAGE FACTOR AT START OF TIME STEP.                              
      SOLD=RHO2                                                                 
      IF(HOLD(J,I,K).GT.TP) SOLD=RHO1                                           
C                                                                               
C5C-----FIND STORAGE FACTOR AT END OF TIME STEP.                                
      HTMP=HNEW(J,I,K)                                                          
      SNEW=RHO2                                                                 
      IF(HTMP.GT.TP) SNEW=RHO1                                                  
C                                                                               
C5D-----ADD STORAGE TERMS TO RHS AND HCOF.                                      
      HCOF(J,I,K)=HCOF(J,I,K)-SNEW                                              
      RHS(J,I,K)=RHS(J,I,K) - SOLD*(HOLD(J,I,K)-TP) - SNEW*TP                   
C                                                                               
  180 CONTINUE                                                                  
C                                                                               
  200 CONTINUE                                                                  
C                                                                               
C6------FOR EACH LAYER DETERMINE IF CORRECTION TERMS ARE NEEDED FOR             
C6------FLOW DOWN INTO PARTIALLY SATURATED LAYERS.                              
  201 KT=0                                                                      
      DO 300 K=1,NLAY                                                           
C                                                                               
C7------SEE IF CORRECTION IS NEEDED FOR LEAKAGE FROM ABOVE.                     
      IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.2) GO TO 250                         
      KT=KT+1                                                                   
      IF(K.EQ.1) GO TO 250                                                      
C                                                                               
C7A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.                            
      DO 220 I=1,NROW                                                           
      DO 220 J=1,NCOL                                                           
C                                                                               
C7B-----IF THE CELL IS EXTERNAL(IBOUND<=0) THEN SKIP IT.                        
      IF(IBOUND(J,I,K).LE.0) GO TO 220                                          
      HTMP=HNEW(J,I,K)                                                          
C                                                                               
C7C-----IF HEAD IS ABOVE TOP THEN CORRECTION NOT NEEDED                         
      IF(HTMP.GE.TOP(J,I,KT)) GO TO 220                                         
C                                                                               
C7D-----WITH HEAD BELOW TOP ADD CORRECTION TERMS TO RHS AND HCOF.               
      RHS(J,I,K)=RHS(J,I,K) + CV(J,I,K-1)*TOP(J,I,KT)                           
      HCOF(J,I,K)=HCOF(J,I,K) + CV(J,I,K-1)                                     
  220 CONTINUE                                                                  
C                                                                               
C8------SEE IF THIS LAYER MAY NEED CORRECTION FOR LEAKAGE TO BELOW.             
  250 IF(K.EQ.NLAY) GO TO 300                                                   
      IF(LAYCON(K+1).NE.3 .AND. LAYCON(K+1).NE.2) GO TO 300                     
      KTT=KT+1                                                                  
C                                                                               
C8A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.                            
      DO 280 I=1,NROW                                                           
      DO 280 J=1,NCOL                                                           
C                                                                               
C8B-----IF CELL IS EXTERNAL (IBOUND<=0) THEN SKIP IT.                           
      IF(IBOUND(J,I,K).LE.0) GO TO 280                                          
C                                                                               
C8C-----IF HEAD IN THE LOWER CELL IS LESS THAN TOP ADD CORRECTION               
C8C-----TERM TO RHS.                                                            
      HTMP=HNEW(J,I,K+1)                                                        
      IF(HTMP.LT.TOP(J,I,KTT)) RHS(J,I,K)=RHS(J,I,K)                            
     1                        - CV(J,I,K)*(TOP(J,I,KTT)-HTMP)                   
  280 CONTINUE                                                                  
  300 CONTINUE                                                                  
C                                                                               
C9------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE BCF1BD(VBNM,VBVL,MSUM,HNEW,IBOUND,HOLD,SC1,CR,CC,CV,           
     1   TOP,SC2,DELT,ISS,NCOL,NROW,NLAY,KSTP,KPER,IBCFCB,                      
     2   ICBCFL,BUFF,IOUT)                                                      
C-----VERSION 1250 28DEC1983 BCF1BD                                             
C                                                                               
C     ******************************************************************        
C     COMPUTE BUDGET FLOW TERMS FOR BCF -- STORAGE, CONSTANT HEAD, AND          
C     FLOW ACROSS CELL WALLS                                                    
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
C                                                                               
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),                   
     1   HOLD(NCOL,NROW,NLAY), SC1(NCOL,NROW,NLAY),                             
     2   CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),                                
     3   CV(NCOL,NROW,NLAY), VBNM(4,20), VBVL(4,20),                            
     4   SC2(NCOL,NROW,NLAY),                                                   
     5   TOP(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY)                               
C                                                                               
      COMMON /FLWCOM/LAYCON(80)                                                 
C                                                                               
      DIMENSION TEXT(4)                                                         
C                                                                               
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /'    ','    ',' STO','RAGE'/        
C     ------------------------------------------------------------------        
C                                                                               
C1------INITIALIZE BUDGET ACCUMULATORS                                          
      STOIN=0.                                                                  
      STOUT=0.                                                                  
C                                                                               
C2------IF CELL-BY-CELL FLOWS ARE NEEDED THEN SET FLAG IBD.                     
      IBD=0                                                                     
      IF(ICBCFL.NE.0 .AND. IBCFCB.GT.0) IBD=1                                   
C                                                                               
C3------IF STEADY STATE THEN SKIP ALL STORAGE CALCULATIONS                      
      IF(ISS.NE.0) GO TO 305                                                    
C                                                                               
C4------IF CELL-BY-CELL FLOWS ARE NEEDED (IBD IS SET) CLEAR BUFFER              
      IF(IBD.EQ.0) GO TO 220                                                    
      DO 210 K=1,NLAY                                                           
      DO 210 I=1,NROW                                                           
      DO 210 J=1,NCOL                                                           
      BUFF(J,I,K)=0.                                                            
  210 CONTINUE                                                                  
C                                                                               
C5------RUN THROUGH EVERY CELL IN THE GRID                                      
  220 KT=0                                                                      
      DO 300 K=1,NLAY                                                           
      LC=LAYCON(K)                                                              
      IF(LC.EQ.3 .OR. LC.EQ.2) KT=KT+1                                          
      DO 300 I=1,NROW                                                           
      DO 300 J=1,NCOL                                                           
C                                                                               
C6------CALCULATE FLOW FROM STORAGE (VARIABLE HEAD CELLS ONLY)                  
      IF(IBOUND(J,I,K).LE.0) GO TO 300                                          
      HSING=HNEW(J,I,K)                                                         
C                                                                               
C6A----CHECK LAYER TYPE TO SEE IF ONE STORAGE FACTOR OR TWO                     
      IF(LC.NE.3 .AND. LC.NE.2) GO TO 285                                       
C                                                                               
C6B----TWO STORAGE FACTORS                                                      
      TP=TOP(J,I,KT)                                                            
      SYA=SC2(J,I,KT)                                                           
      SCFA=SC1(J,I,K)                                                           
      SOLD=SYA                                                                  
      IF(HOLD(J,I,K).GT.TP) SOLD=SCFA                                           
      SNEW=SYA                                                                  
      IF(HSING.GT.TP) SNEW=SCFA                                                 
      STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING                         
      GO TO 288                                                                 
C                                                                               
C6C----ONE STORAGE FACTOR                                                       
  285 SC=SC1(J,I,K)                                                             
      STRG=SC*HOLD(J,I,K) - SC*HSING                                            
C                                                                               
C7-----STORE CELL-BY-CELL FLOW IN BUFFER AND ADD TO ACCUMULATORS                
  288 IF(IBD.EQ.1) BUFF(J,I,K)=STRG/DELT                                        
      IF(STRG) 292,300,294                                                      
  292 STOUT=STOUT-STRG                                                          
      GO TO 300                                                                 
  294 STOIN=STOIN+STRG                                                          
C                                                                               
  300 CONTINUE                                                                  
C                                                                               
C8-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER                     
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,                                  
     1                       IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)                   
C                                                                               
C9------ADD TOTAL RATES AND VOLUMES TO VBVL & PUT TITLES IN VBNM                
  305 VBVL(1,MSUM)=VBVL(1,MSUM)+STOIN                                           
      VBVL(2,MSUM)=VBVL(2,MSUM)+STOUT                                           
      VBVL(3,MSUM)=STOIN/DELT                                                   
      VBVL(4,MSUM)=STOUT/DELT                                                   
      VBNM(1,MSUM)=TEXT(1)                                                      
      VBNM(2,MSUM)=TEXT(2)                                                      
      VBNM(3,MSUM)=TEXT(3)                                                      
      VBNM(4,MSUM)=TEXT(4)                                                      
      MSUM=MSUM+1                                                               
C                                                                               
C10-----CALCULATE FLOW FROM CONSTANT HEAD NODES                                 
      CALL SBCF1F(VBNM,VBVL,MSUM,HNEW,IBOUND,CR,CC,CV,TOP,DELT,                 
     1        NCOL,NROW,NLAY,KSTP,KPER,IBD,IBCFCB,ICBCFL,BUFF,IOUT)             
C                                                                               
C11-----CALCULATE AND SAVE FLOW ACROSS CELL BOUNDARIES IF C-B-C                 
C11-----FLOW TERMS ARE REQUESTED.                                               
      IF(IBD.NE.0) CALL SBCF1B(HNEW,IBOUND,CR,CC,CV,TOP,NCOL,NROW,NLAY,         
     1         KSTP,KPER,IBCFCB,BUFF,IOUT)                                      
C                                                                               
C12----RETURN                                                                   
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SBCF1C(CR,CC,TRPY,DELR,DELC,K,NCOL,NROW,NLAY)                  
C                                                                               
C                                                                               
C-----VERSION 1010 16NOV1982 SBCF1C                                             
C     ******************************************************************        
C     COMPUTE BRANCH CONDUCTANCE USING HARMONIC MEAN OF BLOCK                   
C     CONDUCTANCES -- BLOCK TRANSMISSIVITY IS IN CC UPON ENTRY                  
C     ******************************************************************        
C                                                                               
C      SPECIFICATIONS:                                                          
C     ------------------------------------------------------------------        
C                                                                               
      DIMENSION CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY)                          
     2   , TRPY(NLAY), DELR(NCOL), DELC(NROW)                                   
C                                                                               
C     ------------------------------------------------------------------        
      YX=TRPY(K)*2.                                                             
C                                                                               
C1------FOR EACH CELL CALCULATE BRANCH CONDUCTANCES FROM THAT CELL              
C1------TO THE ONE ON THE RIGHT AND THE IN FRONT.                               
      DO 40 I=1,NROW                                                            
      DO 40 J=1,NCOL                                                            
      T1=CC(J,I,K)                                                              
C                                                                               
C2------IF T=0 THEN SET CONDUCTANCE EQUAL TO 0. GO ON TO NEXT CELL.             
      IF(T1.NE.0.) GO TO 10                                                     
      CR(J,I,K)=0.                                                              
      GO TO 40                                                                  
C                                                                               
C3------IF THIS IS NOT THE LAST COLUMN(RIGHTMOST) THEN CALCULATE                
C3------BRANCH CONDUCTANCE IN THE ROW DIRECTION (CR) TO THE RIGHT.              
   10 IF(J.EQ.NCOL) GO TO 30                                                    
      T2=CC(J+1,I,K)                                                            
      CR(J,I,K)=2.*T2*T1*DELC(I)/(T1*DELR(J+1)+T2*DELR(J))                      
C                                                                               
C4------IF THIS IS NOT THE LAST ROW(FRONTMOST) THEN CALCULATE                   
C4------BRANCH CONDUCTANCE IN THE COLUMN DIRECTION (CC) TO THE FRONT.           
   30 IF(I.EQ.NROW) GO TO 40                                                    
      T2=CC(J,I+1,K)                                                            
      CC(J,I,K)=YX*T2*T1*DELR(J)/(T1*DELC(I+1)+T2*DELC(I))                      
   40 CONTINUE                                                                  
C                                                                               
C5------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SBCF1B(HNEW,IBOUND,CR,CC,CV,TOP,NCOL,NROW,NLAY,                
     1      KSTP,KPER,IBCFCB,BUFF,IOUT)                                         
C                                                                               
C-----VERSION 1004 03MAY1983 SBCF1B                                             
C                                                                               
C     ******************************************************************        
C     COMPUTE FLOW ACROSS EACH CELL WALL                                        
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW,HD                                                  
C                                                                               
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),                   
     1     CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),                              
     2     CV(NCOL,NROW,NLAY), TOP(NCOL,NROW,NLAY),                             
     3     BUFF(NCOL,NROW,NLAY)                                                 
C                                                                               
      COMMON /FLWCOM/LAYCON(80)                                                 
C                                                                               
      DIMENSION TEXT(12)                                                        
C                                                                               
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4),TEXT(5),TEXT(6),TEXT(7),             
     1   TEXT(8),TEXT(9),TEXT(10),TEXT(11),TEXT(12)                             
     2   /'FLOW',' RIG','HT F','ACE ',                                          
     2    'FLOW',' FRO','NT F','ACE ','FLOW',' LOW','ER F','ACE '/              
C     ------------------------------------------------------------------        
C                                                                               
      NCM1=NCOL-1                                                               
      IF(NCM1.LT.1) GO TO 405                                                   
C                                                                               
C1-----CLEAR THE BUFFER                                                         
      DO 310 K=1,NLAY                                                           
      DO 310 I=1,NROW                                                           
      DO 310 J=1,NCOL                                                           
      BUFF(J,I,K)=0.                                                            
  310 CONTINUE                                                                  
C                                                                               
C2-----FOR EACH CELL CALCULATE FLOW THRU RIGHT FACE & STORE IN BUFFER           
      DO 400 K=1,NLAY                                                           
      DO 400 I=1,NROW                                                           
      DO 400 J=1,NCM1                                                           
      IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J+1,I,K).LE.0)) GO TO 400           
      HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)                                           
      BUFF(J,I,K)=HDIFF*CR(J,I,K)                                               
  400 CONTINUE                                                                  
C                                                                               
C3-----RECORD CONTENTS OF BUFFER                                                
      CALL UBUDSV(KSTP,KPER,TEXT(1),IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)            
C                                                                               
C4-----CLEAR THE BUFFER                                                         
  405 NRM1=NROW-1                                                               
      IF(NRM1.LT.1) GO TO 505                                                   
      DO 410 K=1,NLAY                                                           
      DO 410 I=1,NROW                                                           
      DO 410 J=1,NCOL                                                           
      BUFF(J,I,K)=0.                                                            
  410 CONTINUE                                                                  
C                                                                               
C5-----FOR EACH CELL CALCULATE FLOW THRU FRONT FACE & STORE IN BUFFER           
      DO 500 K=1,NLAY                                                           
      DO 500 I=1,NRM1                                                           
      DO 500 J=1,NCOL                                                           
      IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J,I+1,K).LE.0)) GO TO 500           
      HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)                                           
      BUFF(J,I,K)=HDIFF*CC(J,I,K)                                               
  500 CONTINUE                                                                  
C                                                                               
C6-----RECORD CONTENTS OF BUFFER.                                               
      CALL UBUDSV(KSTP,KPER,TEXT(5),IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)            
  505 NLM1=NLAY-1                                                               
      IF(NLM1.LT.1) GO TO 1000                                                  
C                                                                               
C7-----CLEAR THE BUFFER                                                         
      DO 510 K=1,NLAY                                                           
      DO 510 I=1,NROW                                                           
      DO 510 J=1,NCOL                                                           
      BUFF(J,I,K)=0.                                                            
  510 CONTINUE                                                                  
C                                                                               
C8-----FOR EACH CELL CALCULATE FLOW THRU LOWER FACE & STORE IN BUFFER           
      KT=0                                                                      
      DO 600 K=1,NLM1                                                           
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2) KT=KT+1                            
      DO 600 I=1,NROW                                                           
      DO 600 J=1,NCOL                                                           
      IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J,I,K+1).LE.0)) GO TO 600           
      HD=HNEW(J,I,K+1)                                                          
      IF(LAYCON(K+1).NE.3 .AND. LAYCON(K+1).NE.2) GO TO 580                     
      TMP=HD                                                                    
      IF(TMP.LT.TOP(J,I,KT+1)) HD=TOP(J,I,KT+1)                                 
  580 HDIFF=HNEW(J,I,K)-HD                                                      
      BUFF(J,I,K)=HDIFF*CV(J,I,K)                                               
  600 CONTINUE                                                                  
C                                                                               
C9-----RECORD CONTENTS OF BUFFER.                                               
      CALL UBUDSV(KSTP,KPER,TEXT(9),IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)            
C                                                                               
C10----RETURN                                                                   
 1000 RETURN                                                                    
      END                                                                       
      SUBROUTINE SBCF1F(VBNM,VBVL,MSUM,HNEW,IBOUND,CR,CC,CV,                    
     1   TOP,DELT,NCOL,NROW,NLAY,KSTP,KPER,IBD,IBCFCB,ICBCFL,                   
     2   BUFF,IOUT)                                                             
C-----VERSION 1123 29MAY1983 SBCF1F                                             
C                                                                               
C     ******************************************************************        
C     COMPUTE FLOW FROM CONSTANT HEAD NODES                                     
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW,HD                                                  
C                                                                               
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),                   
     1     CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),                              
     2     CV(NCOL,NROW,NLAY), VBNM(4,20), VBVL(4,20),                          
     3     TOP(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY)                             
C                                                                               
      COMMON /FLWCOM/LAYCON(80)                                                 
C                                                                               
      DIMENSION TEXT(4)                                                         
C                                                                               
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /'   C','ONST','ANT ','HEAD'/        
C     ------------------------------------------------------------------        
C                                                                               
C1------CLEAR BUDGET ACCUMULATORS                                               
      CHIN=0.                                                                   
      CHOUT=0.                                                                  
C                                                                               
C2------CLEAR BUFFER IF CELL-BY-CELL FLOW TERM FLAG(IBD) IS SET                 
      IF(IBD.EQ.0) GO TO 8                                                      
      DO 5 K=1,NLAY                                                             
      DO 5 I=1,NROW                                                             
      DO 5 J=1,NCOL                                                             
      BUFF(J,I,K)=0.                                                            
    5 CONTINUE                                                                  
C                                                                               
C3------FOR EACH CELL IF IT IS CONSTANT HEAD COMPUTE FLOW ACROSS 6              
C3-----FACES.                                                                   
    8 KT=0                                                                      
      DO 200 K=1,NLAY                                                           
      LC=LAYCON(K)                                                              
      IF(LC.EQ.3 .OR. LC.EQ.2) KT=KT+1                                          
      DO 200 I=1,NROW                                                           
      DO 200 J=1,NCOL                                                           
C                                                                               
C4-----IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.               
      IF (IBOUND(J,I,K).GE.0)GO TO 200                                          
C                                                                               
C5-----CLEAR FIELDS FOR SIX FLOW RATES.                                         
      X1=0.                                                                     
      X2=0.                                                                     
      X3=0.                                                                     
      X4=0.                                                                     
      X5=0.                                                                     
      X6=0.                                                                     
C6-----FOR EACH FACE OF THE CELL CALCULATE FLOW THROUGH THAT FACE               
C6-----OUT OF THE CONSTANT HEAD CELL AND INTO THE FLOW DOMAIN.                  
C6-----COMMENTS 7-11 APPEAR ONLY IN THE SECTION HEADED BY COMMENT 6A            
C6-----BUT THEY APPLY IN A SIMILAR MANNER TO THE SECTIONS HEADED                
C6-----BY COMMENTS 6B-6F.                                                       
C                                                                               
C6A----CALCULATE FLOW THROUGH THE LEFT FACE                                     
C                                                                               
C7-----IF THERE IS NOT A VARIABLE HEAD CELL ON THE OTHER SIDE OF THIS           
C7-----FACE THEN GO ON TO THE NEXT FACE.                                        
      IF(J.EQ.1) GO TO 30                                                       
      IF(IBOUND(J-1,I,K).LE.0)GO TO 30                                          
      HDIFF=HNEW(J,I,K)-HNEW(J-1,I,K)                                           
C                                                                               
C8-----CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.                 
      X1=HDIFF*CR(J-1,I,K)                                                      
C                                                                               
C9-----TEST TO SEE IF FLOW IS POSITIVE OR NEGATIVE                              
      IF (X1) 10,30,20                                                          
C                                                                               
C10----IF NEGATIVE ADD TO CHOUT(FLOW OUT OF DOMAIN TO CONSTANT HEAD).           
   10 CHOUT=CHOUT-X1                                                            
      GO TO 30                                                                  
C                                                                               
C11----IF POSITIVE ADD TO CHIN(FLOW INTO DOMAIN FROM CONSTANT HEAD).            
   20 CHIN=CHIN+X1                                                              
C                                                                               
C6B----CALCULATE FLOW THROUGH THE RIGHT FACE                                    
   30 IF(J.EQ.NCOL) GO TO 60                                                    
      IF(IBOUND(J+1,I,K).LE.0) GO TO 60                                         
      HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)                                           
      X2=HDIFF*CR(J,I,K)                                                        
      IF(X2)40,60,50                                                            
   40 CHOUT=CHOUT-X2                                                            
      GO TO 60                                                                  
   50 CHIN=CHIN+X2                                                              
C                                                                               
C6C----CALCULATE FLOW THROUGH THE BACK FACE.                                    
   60 IF(I.EQ.1) GO TO 90                                                       
      IF (IBOUND(J,I-1,K).LE.0) GO TO 90                                        
      HDIFF=HNEW(J,I,K)-HNEW(J,I-1,K)                                           
      X3=HDIFF*CC(J,I-1,K)                                                      
      IF(X3) 70,90,80                                                           
   70 CHOUT=CHOUT-X3                                                            
      GO TO 90                                                                  
   80 CHIN=CHIN+X3                                                              
C                                                                               
C6D----CALCULATE FLOW THROUGH THE FRONT FACE.                                   
   90 IF(I.EQ.NROW) GO TO 120                                                   
      IF(IBOUND(J,I+1,K).LE.0) GO TO 120                                        
      HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)                                           
      X4=HDIFF*CC(J,I,K)                                                        
      IF (X4) 100,120,110                                                       
  100 CHOUT=CHOUT-X4                                                            
      GO TO 120                                                                 
  110 CHIN=CHIN+X4                                                              
C                                                                               
C6E----CALCULATE FLOW THROUGH THE UPPER FACE                                    
  120 IF(K.EQ.1) GO TO 150                                                      
      IF (IBOUND(J,I,K-1).LE.0) GO TO 150                                       
      HD=HNEW(J,I,K)                                                            
      IF(LC.NE.3 .AND. LC.NE.2) GO TO 122                                       
      TMP=HD                                                                    
      IF(TMP.LT.TOP(J,I,KT)) HD=TOP(J,I,KT)                                     
  122 HDIFF=HD-HNEW(J,I,K-1)                                                    
      X5=HDIFF*CV(J,I,K-1)                                                      
      IF(X5) 130,150,140                                                        
  130 CHOUT=CHOUT-X5                                                            
      GO TO 150                                                                 
  140 CHIN=CHIN+X5                                                              
C                                                                               
C6F----CALCULATE FLOW THROUGH THE LOWER FACE.                                   
  150 IF(K.EQ.NLAY) GO TO 180                                                   
      IF(IBOUND(J,I,K+1).LE.0) GO TO 180                                        
      HD=HNEW(J,I,K+1)                                                          
      IF(LAYCON(K+1).NE.3 .AND. LAYCON(K+1).NE.2) GO TO 152                     
      TMP=HD                                                                    
      IF(TMP.LT.TOP(J,I,KT+1)) HD=TOP(J,I,KT+1)                                 
  152 HDIFF=HNEW(J,I,K)-HD                                                      
      X6=HDIFF*CV(J,I,K)                                                        
      IF(X6) 160,180,170                                                        
  160 CHOUT=CHOUT-X6                                                            
      GO TO 180                                                                 
  170 CHIN=CHIN+X6                                                              
C                                                                               
C12-----SUM UP FLOWS THROUGH SIX SIDES OF CONSTANT HEAD CELL.                   
 180  RATE=X1+X2+X3+X4+X5+X6                                                    
C                                                                               
C13-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IBCFCB<0).                      
      IF(IBCFCB.LT.0.AND.ICBCFL.NE.0) WRITE(IOUT,900) (TEXT(N),N=1,4),          
     1    KPER,KSTP,K,I,J,RATE                                                  
  900 FORMAT(1H0,4A4,'   PERIOD',I3,'   STEP',I3,'   LAYER',I3,                 
     1    '   ROW',I4,'   COL',I4,'   RATE ',G15.7)                             
C                                                                               
C14----IF CELL-BY-CELL FLAG SET STORE SUM OF FLOWS FOR CELL IN BUFFER           
      IF(IBD.EQ.1) BUFF(J,I,K)=RATE                                             
C                                                                               
  200 CONTINUE                                                                  
C                                                                               
C15----IF CELL-BY-CELL FLAG SET THEN RECORD CONTENTS OF BUFFER                  
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT(1),                               
     1                   IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)                       
C                                                                               
C                                                                               
C16----SAVE TOTAL CONSTANT HEAD FLOWS AND VOLUMES IN VBVL TABLE                 
C16----FOR INCLUSION IN BUDGET. PUT LABELS IN VBNM TABLE.                       
      VBVL(1,MSUM)=VBVL(1,MSUM)+CHIN*DELT                                       
      VBVL(2,MSUM)=VBVL(2,MSUM)+CHOUT*DELT                                      
      VBVL(3,MSUM)=CHIN                                                         
      VBVL(4,MSUM)=CHOUT                                                        
C                                                                               
C     ---SETUP VOLUMETRIC BUDGET NAMES                                          
      VBNM(1,MSUM)=TEXT(1)                                                      
      VBNM(2,MSUM)=TEXT(2)                                                      
      VBNM(3,MSUM)=TEXT(3)                                                      
      VBNM(4,MSUM)=TEXT(4)                                                      
C                                                                               
      MSUM=MSUM+1                                                               
C                                                                               
C                                                                               
C17----RETURN                                                                   
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SBCF1H(HNEW,IBOUND,CR,CC,CV,HY,TRPY,DELR,DELC                  
     1,BOT,TOP,K,KB,KT,KITER,KSTP,KPER,NCOL,NROW,NLAY,IOUT)                     
C                                                                               
C-----VERSION 1006 03MAY1983 SBCF1H                                             
C                                                                               
C     ******************************************************************        
C     COMPUTE CONDUCTANCE FROM SATURATED THICKNESS AND HYDRAULIC                
C     CONDUCTIVITY                                                              
C     ******************************************************************        
C                                                                               
C      SPECIFICATIONS:                                                          
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
C                                                                               
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)                     
     1, CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY), CV(NCOL,NROW,NLAY)              
     2,HY(NCOL,NROW,NLAY), TRPY(NLAY), DELR(NCOL), DELC(NROW)                   
     3, BOT(NCOL,NROW,NLAY),TOP(NCOL,NROW,NLAY)                                 
C                                                                               
      COMMON /FLWCOM/LAYCON(80)                                                 
C     ------------------------------------------------------------------        
C                                                                               
C1------CALCULATE TRANSMISSIVITY AT EACH ACTIVE CELL. TRANSMISSIVITY            
C1------WILL BE STORED TEMPORARILY IN THE CC ARRAY.                             
      DO 200 I=1,NROW                                                           
      DO 200 J=1,NCOL                                                           
C                                                                               
C2------IF CELL IS INACTIVE THEN SET T=0 & MOVE ON TO NEXT CELL.                
      IF(IBOUND(J,I,K).NE.0) GO TO 10                                           
      CC(J,I,K)=0.                                                              
      GO TO 200                                                                 
C                                                                               
C3------CALCULATE SATURATED THICKNESS.                                          
   10 HD=HNEW(J,I,K)                                                            
      IF(LAYCON(K).EQ.1) GO TO 50                                               
      IF(HD.GT.TOP(J,I,KT)) HD=TOP(J,I,KT)                                      
   50 THCK=HD-BOT(J,I,KB)                                                       
C                                                                               
C4------CHECK TO SEE IF SATURATED THICKNESS IS GREATER THAN ZERO.               
      IF(THCK.LE.0.) GO TO 100                                                  
C                                                                               
C5------IF SATURATED THICKNESS>0 THEN T=K*THICKNESS.                            
      CC(J,I,K)=THCK*HY(J,I,KB)                                                 
      GO TO 200                                                                 
C                                                                               
C6------WHEN SATURATED THICKNESS < 0, PRINT A MESSAGE AND SET                   
C6------TRANSMISSIVITY, IBOUND, AND VERTICAL CONDUCTANCE =0                     
  100 WRITE(IOUT,150) J,I,K,KITER,KSTP,KPER                                     
  150 FORMAT(1H0,10('*'),'NODE',3I4,' (COL,ROW,LAYER) WENT DRY'                 
     1      ,' AT ITERATION =',I3,'  TIME STEP =',I3                            
     2      ,'  STRESS PERIOD =',I3)                                            
      HNEW(J,I,K)=1.E30                                                         
      CC(J,I,K)=0.                                                              
      IBOUND(J,I,K)=0                                                           
      IF(K.LT.NLAY) CV(J,I,K)=0.                                                
      IF(K.GT.1) CV(J,I,K-1)=0.                                                 
      GO TO 200                                                                 
  200 CONTINUE                                                                  
C                                                                               
C7------COMPUTE HORIZONTAL BRANCH CONDUCTANCES FROM TRANSMISSIVITY              
      CALL SBCF1C(CR,CC,TRPY,DELR,DELC,K,NCOL,NROW,NLAY)                        
C                                                                               
C8------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SBCF1N(HNEW,IBOUND,SC1,SC2,CR,CC,CV,HY,TRPY,DELR,DELC,         
     1         ISS,NCOL,NROW,NLAY,IOUT)                                         
C                                                                               
C-----VERSION 1007 03MAY1983 SBCF1N                                             
C                                                                               
C     ******************************************************************        
C     INITIALIZE AND CHECK BCF DATA                                             
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C                                                                               
      DOUBLE PRECISION HNEW,HCNV                                                
C                                                                               
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)                     
     1    ,SC1(NCOL,NROW,NLAY),CR(NCOL,NROW,NLAY)                               
     2    ,CC(NCOL,NROW,NLAY),CV(NCOL,NROW,NLAY)                                
     3    ,HY(NCOL,NROW,NLAY),TRPY(NLAY),DELR(NCOL),DELC(NROW)                  
     4    ,SC2(NCOL,NROW,NLAY)                                                  
C                                                                               
      COMMON /FLWCOM/LAYCON(80)                                                 
C     ------------------------------------------------------------------        
C                                                                               
C1------IF IBOUND=0, SET CV=0., CC=0., AND HY=0.                                
      KB=0                                                                      
      DO 30 K=1,NLAY                                                            
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.1) KB=KB+1                            
      DO 30 I=1,NROW                                                            
      DO 30 J=1,NCOL                                                            
      IF(IBOUND(J,I,K).NE.0) GO TO 30                                           
      IF(K.NE.NLAY) CV(J,I,K)=0.                                                
      IF(K.NE.1) CV(J,I,K-1)=0.                                                 
      CC(J,I,K)=0.                                                              
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.1) HY(J,I,KB)=0.                      
   30 CONTINUE                                                                  
C                                                                               
C2------INSURE THAT EACH ACTIVE CELL HAS AT LEAST ONE NON-ZERO                  
C2------TRANSMISSIVE PARAMETER. IF NOT, CONVERT CELL TO NOFLOW.                 
      HCNV=888.88                                                               
      KB=0                                                                      
      DO 60 K=1,NLAY                                                            
      IF(LAYCON(K).EQ.1 .OR. LAYCON(K).EQ.3) GO TO 55                           
C2A-----WHEN LAYER TYPE 0 OR 2, TRANSMISSIVITY OR CV MUST BE NONZERO            
      DO 54 I=1,NROW                                                            
      DO 54 J=1,NCOL                                                            
      IF(IBOUND(J,I,K).EQ.0) GO TO 54                                           
      IF(CC(J,I,K).NE.0.) GO TO 54                                              
      IF(K.EQ.NLAY) GO TO 51                                                    
      IF(CV(J,I,K).NE.0.) GO TO 54                                              
   51 IF(K.EQ.1) GO TO 53                                                       
      IF(CV(J,I,K-1).NE.0.) GO TO 54                                            
   53 IBOUND(J,I,K)=0                                                           
      HNEW(J,I,K)=HCNV                                                          
      WRITE(IOUT,52) K,I,J                                                      
   52 FORMAT(1X,'NODE (LAYER,ROW,COL)',3I4,                                     
     1      ' ELIMINATED BECAUSE ALL CONDUCTANCES TO NODE ARE 0')               
   54 CONTINUE                                                                  
      GO TO 60                                                                  
C                                                                               
C2B-----WHEN LAYER TYPE IS 1 OR 3, HY OR CV MUST BE NONZERO                     
   55 KB=KB+1                                                                   
      DO 59 I=1,NROW                                                            
      DO 59 J=1,NCOL                                                            
      IF(IBOUND(J,I,K).EQ.0) GO TO 59                                           
      IF(HY(J,I,KB).NE.0.) GO TO 59                                             
      IF(K.EQ.NLAY) GO TO 56                                                    
      IF(CV(J,I,K).NE.0.) GO TO 59                                              
   56 IF(K.EQ.1) GO TO 57                                                       
      IF(CV(J,I,K-1).NE.0.) GO TO 59                                            
   57 IBOUND(J,I,K)=0                                                           
      HNEW(J,I,K)=HCNV                                                          
      CC(J,I,K)=0.                                                              
      WRITE(IOUT,52) K,I,J                                                      
   59 CONTINUE                                                                  
   60 CONTINUE                                                                  
C                                                                               
C3------CALCULATE HOR. CONDUCTANCE(CR AND CC) FOR CONSTANT T LAYERS             
      DO 65 K=1,NLAY                                                            
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.1) GO TO 65                           
      CALL SBCF1C(CR,CC,TRPY,DELR,DELC,K,NCOL,NROW,NLAY)                        
   65 CONTINUE                                                                  
C                                                                               
C4------MULTIPLY VERTICAL LEAKANCE BY AREA TO MAKE CONDUCTANCE                  
      IF(NLAY.EQ.1) GO TO 69                                                    
      K1=NLAY-1                                                                 
      DO 68 K=1,K1                                                              
      DO 68 I=1,NROW                                                            
      DO 68 J=1,NCOL                                                            
      CV(J,I,K)=CV(J,I,K)*DELR(J)*DELC(I)                                       
   68 CONTINUE                                                                  
C                                                                               
C5------IF TRANSIENT MULTIPLY PRIMARY STORAGE FACTOR BY DELR &                  
C5------DELC TO GET PRIMARY STORAGE CAPACITY(SC1).                              
   69 IF(ISS.NE.0) GO TO 100                                                    
      KT=0                                                                      
      DO 80 K=1,NLAY                                                            
      DO 70 I=1,NROW                                                            
      DO 70 J=1,NCOL                                                            
      SC1(J,I,K)=SC1(J,I,K)*DELR(J)*DELC(I)                                     
   70 CONTINUE                                                                  
C                                                                               
C6------IF LAYER IS CONF/UNCONF MULTIPLY SECONDARY STORAGE FACTOR               
C6------BY DELR AND DELC TO GET SECONDARY STORAGE CAPACITY(SC2).                
      IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.2) GO TO 80                          
      KT=KT+1                                                                   
      DO 75 I=1,NROW                                                            
      DO 75 J=1,NCOL                                                            
      SC2(J,I,KT)=SC2(J,I,KT)*DELR(J)*DELC(I)                                   
   75 CONTINUE                                                                  
C                                                                               
   80 CONTINUE                                                                  
C                                                                               
C7------RETURN                                                                  
  100 RETURN                                                                    
      END                                                                       
      SUBROUTINE RIV1AL(ISUM,LENX,LCRIVR,MXRIVR,NRIVER,IN,IOUT,                 
     1                      IRIVCB,MRSEG)                                             
C                                                                               
C-----VERSION 0935 08DEC1983 RIV1AL                                             
C     ******************************************************************        
C     ALLOCATE ARRAY STORAGE FOR RIVERS                                         
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
C                                                                               
C1------IDENTIFY PACKAGE AND INITIALIZE NRIVER.                                 
      WRITE(IOUT,1)IN                                                           
    1 FORMAT(1H0,'RIV1 -- RIVER PACKAGE, VERSION 1,  12/08/83',                 
     2' INPUT READ FROM UNIT',I3)                                               
      NRIVER=0                                                                  
C                                                                               
C2------READ & PRINT MXRIVR & IRIVCB(UNIT OR FLAG FOR C-B-C FLOWS)              
      READ(IN,2)MXRIVR,IRIVCB,MRSEG                                                   
    2 FORMAT(3I10)                                                              
      WRITE(IOUT,3)MXRIVR                                                       
    3 FORMAT(1H ,'MAXIMUM OF',I5,' RIVER NODES')                                
      IF(IRIVCB.GT.0) WRITE(IOUT,9) IRIVCB                                      
    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT',I3)               
      IF(IRIVCB.LT.0) WRITE(IOUT,8)                                             
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED')                           
C                                                                               
C3------SET LCRIVR EQUAL TO ADDRESS OF FIRST UNUSED SPACE IN X.                 
      LCRIVR=ISUM                                                               
C                                                                               
C4------CALCULATE AMOUNT OF SPACE USED BY RIVER LIST.                           
      ISP=7*MXRIVR                                                              
      ISUM=ISUM+ISP                                                             
C                                                                               
C5------PRINT AMOUNT OF SPACE USED BY RIVER PACKAGE.                            
      WRITE (IOUT,4)ISP                                                         
    4 FORMAT(1X,I6,' ELEMENTS IN X ARRAY ARE USED FOR RIVERS')                  
      ISUM1=ISUM-1                                                              
      WRITE(IOUT,5)ISUM1,LENX                                                   
    5 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED OUT OF',I7)                       
      IF(ISUM1.GT.LENX) WRITE(IOUT,6)                                           
    6 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')                  
C                                                                               
C7------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE RIV1RP(RIVR,NRIVER,MXRIVR,IN,IOUT)                             
C                                                                               
C                                                                               
C-----VERSION 1319 25AUG1982 RIV1RP                                             
C     ******************************************************************        
C     READ RIVER HEAD, CONDUCTANCE AND BOTTOM ELEVATION                         
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
      DIMENSION RIVR(7,MXRIVR)                                                  
C     ------------------------------------------------------------------        
C                                                                               
C                                                                               
C1------READ ITMP(NUMBER OF RIVER REACHES OR FLAG TO REUSE DATA)                
      READ(IN,8)ITMP                                                            
    8 FORMAT(I10)                                                               
C                                                                               
C2------TEST ITMP.                                                              
      IF(ITMP.GE.0)GO TO 50                                                     
C                                                                               
C2A-----IF ITMP <0 THEN REUSE DATA FROM LAST STRESS PERIOD.                     
      WRITE(IOUT,7)                                                             
    7 FORMAT(1H0,'REUSING RIVER REACHES FROM LAST STRESS PERIOD')               
      GO TO 260                                                                 
C                                                                               
C3------IF ITMP=> ZERO THEN IT IS THE NUMBER OF RIVER REACHES                   
   50 NRIVER=ITMP                                                               
C                                                                               
C4------IF NRIVER>MXRIVR THEN STOP.                                             
      IF(NRIVER.LE.MXRIVR)GO TO 100                                             
      WRITE(IOUT,99)NRIVER,MXRIVR                                               
   99 FORMAT(1H0,'NRIVER(',I4,') IS GREATER THAN MXRIVR(',I4,')')               
C                                                                               
C4A-----ABNORMAL STOP.                                                          
      STOP                                                                      
C                                                                               
C5------PRINT NUMBER OF RIVER REACHES IN THIS STRESS PERIOD.                    
  100 WRITE(IOUT,1)NRIVER                                                       
    1 FORMAT(1H0,//1X,I5,' RIVER REACHES')                                      
C                                                                               
C6------IF THERE ARE NO RIVER REACHES THEN RETURN.                              
      IF(NRIVER.EQ.0) GO TO 260                                                 
C                                                                               
C7------READ AND PRINT DATA FOR EACH RIVER REACH.                               
      WRITE(IOUT,3)                                                             
    3 FORMAT(1H0,15X,'LAYER',5X,'ROW',5X,'COL   '                               
     1,'  STAGE     CONDUCTANCE   BOTTOM ELEVATION   RIVER REACH'               
     2/1X,15X,80('-'))                                                          
      DO 250 II=1,NRIVER                                                        
      READ(IN,4)K,I,J,RIVR(4,II),RIVR(5,II),RIVR(6,II),RIVR(7,II)                          
    4 FORMAT(3I10,4F10.0)                                                       
      WRITE(IOUT,5)K,I,J,RIVR(4,II),RIVR(5,II),RIVR(6,II),II                    
    5 FORMAT(1X,15X,I4,I9,I8,G13.4,G14.4,G19.4,I10)                             
      RIVR(1,II)=K                                                              
      RIVR(2,II)=I                                                              
      RIVR(3,II)=J                                                              
  250 CONTINUE                                                                  
C                                                                               
C8------RETURN                                                                  
  260 RETURN                                                                    
      END                                                                       
      SUBROUTINE RIV1FM(NRIVER,MXRIVR,RIVR,HNEW,HCOF,RHS,IBOUND,                
     1                  NCOL,NROW,NLAY)                                         
C                                                                               
C-----VERSION 0915 27AUG1982 RIV1FM                                             
C     ******************************************************************        
C     ADD RIVER TERMS TO RHS AND HCOF                                           
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
C                                                                               
      DOUBLE PRECISION HNEW                                                     
      DIMENSION RIVR(7,MXRIVR),HNEW(NCOL,NROW,NLAY),                            
     1          HCOF(NCOL,NROW,NLAY),RHS(NCOL,NROW,NLAY),                       
     2          IBOUND(NCOL,NROW,NLAY)                                          
C     ------------------------------------------------------------------        
C                                                                               
C                                                                               
C1------IF NRIVER<=0 THERE ARE NO RIVERS. RETURN.                               
      IF(NRIVER.LE.0)RETURN                                                     
C                                                                               
C2------PROCESS EACH CELL IN THE RIVER LIST.                                    
      DO 100 L=1,NRIVER                                                         
C                                                                               
C3------GET COLUMN, ROW, AND LAYER OF CELL CONTAINING REACH                     
      IL=RIVR(1,L)                                                              
      IR=RIVR(2,L)                                                              
      IC=RIVR(3,L)                                                              
C                                                                               
C4------IF THE CELL IS EXTERNAL SKIP IT.                                        
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 100                                        
C                                                                               
C5------SINCE THE CELL IS INTERNAL GET THE RIVER DATA.                          
      HRIV=RIVR(4,L)                                                            
      CRIV=RIVR(5,L)                                                            
      RBOT=RIVR(6,L)                                                            
      HHNEW=HNEW(IC,IR,IL)                                                      
C                                                                               
C6------COMPARE AQUIFER HEAD TO BOTTOM OF STREAM BED.                           
      IF(HHNEW.LE.RBOT)GO TO 96                                                 
C                                                                               
C7------SINCE HEAD>BOTTOM ADD TERMS TO RHS AND HCOF.                            
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-CRIV*HRIV                                     
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-CRIV                                        
      GO TO 100                                                                 
C                                                                               
C8------SINCE HEAD<BOTTOM ADD TERM ONLY TO RHS.                                 
   96 RHS(IC,IR,IL)=RHS(IC,IR,IL)-CRIV*(HRIV-RBOT)                              
  100 CONTINUE                                                                  
C                                                                               
C9------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE RIV1BD(NRIVER,MXRIVR,RIVR,IBOUND,HNEW,                         
     1      NCOL,NROW,NLAY,DELT,VBVL,VBNM,MSUM,KSTP,KPER,IRIVCB,                
     2      ICBCFL,BUFF,IOUT,MRSEG)                                                   
C-----VERSION 1256 28DEC1983 RIV1BD 
C     CHANGED FOR OUTPUT TO UNIT 74  DRS 8/27/1986                             
C     ******************************************************************        
C     CALCULATE VOLUMETRIC BUDGET FOR RIVERS                                    
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
      DIMENSION RIVR(7,MXRIVR),IBOUND(NCOL,NROW,NLAY),                          
     1          HNEW(NCOL,NROW,NLAY),VBVL(4,20),VBNM(4,20),                     
     2          BUFF(NCOL,NROW,NLAY),SEGFLOW(30)                                            
      DIMENSION TEXT(4)                                                         
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /'   R','IVER',' LEA','KAGE'/        
C     ------------------------------------------------------------------        
C                                                                               
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND                        
C1------ACCUMULATORS (RATIN AND RATOUT).                                        
      IBD=0                                                                     
      RATIN=0.                                                                  
      RATOUT=0.                                                                 

      DO 4 ISEG=1,MRSEG
    4 SEGFLOW(ISEG)  =  0.0


C                                                                               
C2------IF NO REACHES KEEP ZEROES IN ACCUMULATORS.                              
      IF(NRIVER.EQ.0)GO TO 200                                                  
C                                                                               
C3------TEST TO SEE IF CELL-BY-CELL FLOW TERMS ARE NEEDED.                      
      GO TO 10                               
C                                                                               
C3A-----CELL-BY-CELL FLOW TERMS ARE NEEDED SET IBD AND CLEAR BUFFER.            
      IBD=1                                                                     
      DO 5 IL=1,NLAY                                                            
      DO 5 IR=1,NROW                                                            
      DO 5 IC=1,NCOL                                                            
      BUFF(IC,IR,IL)=0.                                                         
    5 CONTINUE                                                                  
C                                                                               
C4------FOR EACH RIVER REACH ACCUMULATE RIVER FLOW (STEPS 5-15)                 
   10 DO 100 L=1,NRIVER                                                         
C                                                                               
C5------GET LAYER, ROW & COLUMN OF CELL CONTAINING REACH.                       
      IL=RIVR(1,L)                                                              
      IR=RIVR(2,L)                                                              
      IC=RIVR(3,L)
      ISEG = RIVR(7,L)                                                        
C                                                                               
C6------IF CELL IS EXTERNAL MOVE ON TO NEXT REACH.                              
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 100                                        
C                                                                               
C7------GET RIVER PARAMETERS FROM RIVER LIST.                                   
      HRIV=RIVR(4,L)                                                            
      CRIV=RIVR(5,L)                                                            
      RBOT=RIVR(6,L)                                                            
      HHNEW=HNEW(IC,IR,IL)                                                      
C                                                                               
C8------COMPARE HEAD IN AQUIFER TO BOTTOM OF RIVERBED.                          
C                                                                               
C9------AQUIFER HEAD > BOTTOM THEN RATE=CRIV*(HRIV-HNEW).                       
      IF(HHNEW.GT.RBOT)RATE=CRIV*(HRIV-HHNEW)                                   
C                                                                               
C10-----AQUIFER HEAD < BOTTOM THEN RATE=CRIV*(HRIV-RBOT)                        
      IF(HHNEW.LE.RBOT)RATE=CRIV*(HRIV-RBOT)                                    
C                                                                               
C11-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IRIVCB<0).                      
      IF(IRIVCB.LT.0.AND.ICBCFL.NE.0) WRITE(IOUT,900) (TEXT(N),N=1,4),          
     1    KPER,KSTP,L,IL,IR,IC,RATE                                             
  900 FORMAT(1H0,4A4,'   PERIOD',I3,'   STEP',I3,'   REACH',I4,                 
     1    '   LAYER',I3,'   ROW',I4,'   COL',I4,'   RATE',G15.7)                
C                                                                               
C12------ADD C-B-C FLOW TERMS TO SEGFLOW
C      IF(ICBCFL.NE.0) WRITE(74,950) IR,IC,RATE
      SEGFLOW(ISEG) = SEGFLOW(ISEG) + RATE
      IF(IBD.EQ.1) BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+RATE                           
C                                                                               
C13-----SEE IF FLOW IS INTO AQUIFER OR INTO RIVER.                              
      IF(RATE)94,100,96                                                         
C                                                                               
C14-----AQUIFER IS DISCHARGING TO RIVER SUBTRACT RATE FROM RATOUT.              
   94 RATOUT=RATOUT-RATE                                                        
      GO TO 100                                                                 
C                                                                               
C15-----AQUIFER IS RECHARGED FROM RIVER ADD RATE TO RATIN.                      
   96 RATIN=RATIN+RATE                                                          
  100 CONTINUE                                                                  
C                                                                               
C16-----IF C-B-C FLOW TERMS WILL BE SAVED CALL UBUDSV TO RECORD THEM.           
C      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IRIVCB,BUFF,NCOL,NROW,          
C     1                          NLAY,IOUT)                                   
C                                                                               
C17-----MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.                   
  200 VBVL(3,MSUM)=RATIN                                                        
      VBVL(4,MSUM)=RATOUT                                                       
      VBVL(1,MSUM)=VBVL(1,MSUM)+RATIN*DELT                                      
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT                                     
      VBNM(1,MSUM)=TEXT(1)                                                      
      VBNM(2,MSUM)=TEXT(2)                                                      
      VBNM(3,MSUM)=TEXT(3)                                                      
      VBNM(4,MSUM)=TEXT(4)                                                      
C                       
      IF(ICBCFL.EQ.0) GOTO 952
      DO 201 ISEG=1,MRSEG
  201 WRITE (74,951) (SEGFLOW(ISEG))
      WRITE(74,951) (RATIN-RATOUT)           
  951 FORMAT(1G15.4)                          
  952 CONTINUE
C18-----INCREMENT BUDGET TERM COUNTER                                           
      MSUM=MSUM+1                                                               
C                                                                               
C19-----RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE RCH1AL(ISUM,LENX,LCIRCH,LCRECH,NRCHOP,                         
     C                  NCOL,NROW,IN,IOUT,IRCHCB)                               
C                                                                               
C-----VERSION 0939 08DEC1983 RCH1AL                                             
C     ******************************************************************        
C     ALLOCATE ARRAY STORAGE FOR RECHARGE                                       
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
C                                                                               
C1------IDENTIFY PACKAGE.                                                       
      WRITE(IOUT,1)IN                                                           
    1 FORMAT(1H0,'RCH1 -- RECHARGE PACKAGE, VERSION 1, 12/08/83',               
     2' INPUT READ FROM UNIT',I3)                                               
C                                                                               
C2------READ NRCHOP AND IRCHCB.                                                 
      READ(IN,2)NRCHOP,IRCHCB                                                   
    2 FORMAT(2I10)                                                              
C                                                                               
C3------CHECK TO SEE THAT OPTION IS LEGAL.                                      
      IF(NRCHOP.GE.1.AND.NRCHOP.LE.3)GO TO 200                                  
C                                                                               
C3A-----IF ILLEGAL PRINT A MESSAGE AND ABORT SIMULATION                         
      WRITE(IOUT,8)                                                             
    8 FORMAT(1X,'ILLEGAL OPTION CODE. SIMULATION ABORTING')                     
      STOP                                                                      
C                                                                               
C4------IF OPTION IS LEGAL PRINT OPTION CODE.                                   
  200 IRK=ISUM                                                                  
      IF(NRCHOP.EQ.1) WRITE(IOUT,201)                                           
  201 FORMAT(1X,'OPTION 1 -- RECHARGE TO TOP LAYER')                            
      IF(NRCHOP.EQ.2) WRITE(IOUT,202)                                           
  202 FORMAT(1X,'OPTION 2 -- RECHARGE TO ONE SPECIFIED NODE IN EACH',           
     1        ' VERTICAL COLUMN')                                               
      IF(NRCHOP.EQ.3) WRITE(IOUT,203)                                           
  203 FORMAT(1X,'OPTION 3 -- RECHARGE TO HIGHEST ACTIVE NODE IN EACH',          
     1        ' VERTICAL COLUMN')                                               
C                                                                               
C5------IF CELL-BY-CELL FLOW  TERMS TO BE SAVED THEN PRINT UNIT #               
      IF(IRCHCB.GT.0) WRITE(IOUT,204) IRCHCB                                    
  204 FORMAT(1X,'CELL-BY-CELL FLOW TERMS WILL BE RECORDED ON UNIT',I3)          
C                                                                               
C6------ALLOCATE SPACE FOR THE RECHARGE ARRAY(RECH).                            
      LCRECH=ISUM                                                               
      ISUM=ISUM+NCOL*NROW                                                       
C                                                                               
C7------IF OPTION 2 THEN ALLOCATE SPACE FOR INDICATOR ARRAY(IRCH)               
      IF(NRCHOP.NE.2)GO TO 300                                                  
      LCIRCH=ISUM                                                               
      ISUM=ISUM+NCOL*NROW                                                       
C                                                                               
C8------CALCULATE AND PRINT AMOUNT OF SPACE USED BY RECHARGE.                   
  300 IRK=ISUM-IRK                                                              
      WRITE(IOUT,4)IRK                                                          
    4 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED FOR RECHARGE')                    
      ISUM1=ISUM-1                                                              
      WRITE(IOUT,5)ISUM1,LENX                                                   
    5 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED OUT OF',I7)                       
      IF(ISUM1.GT.LENX)WRITE(IOUT,6)                                            
    6 FORMAT(1X,'   ***X ARRAY MUST BE MADE LARGER***')                         
C                                                                               
C9------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE RCH1RP(NRCHOP,IRCH,RECH,DELR,DELC,NROW,NCOL,                   
     C                  NLAY,IN,IOUT)                                           
C                                                                               
C-----VERSION 1513 22DEC1982 RCH1RP                                             
C     ******************************************************************        
C     READ RECHARGE RATES                                                       
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION IRCH(NCOL,NROW),RECH(NCOL,NROW),                                
     1          ANAME(6,2),DELR(NCOL),DELC(NROW)                                
C                                                                               
      DATA ANAME(1,1),ANAME(2,1),ANAME(3,1),ANAME(4,1),ANAME(5,1),              
     1  ANAME(6,1) /'    ','RECH','ARGE',' LAY','ER I','NDEX'/                  
      DATA ANAME(1,2),ANAME(2,2),ANAME(3,2),ANAME(4,2),ANAME(5,2),              
     1  ANAME(6,2) /'    ','    ','    ','    ','RECH','ARGE'/                  
C     ------------------------------------------------------------------        
C                                                                               
C1------READ FLAGS SHOWING WHETHER DATA IS TO BE REUSED.                        
      READ(IN,4)INRECH,INIRCH                                                   
    4 FORMAT(2I10)                                                              
C                                                                               
C2------TEST INRECH TO SEE WHERE RECH IS COMING FROM.                           
      IF(INRECH.GE.0)GO TO 32                                                   
C                                                                               
C2A-----IF INRECH<0 THEN REUSE RECHARGE ARRAY FROM LAST STRESS PERIOD           
      WRITE(IOUT,3)                                                             
    3 FORMAT(1H0,'REUSING RECH FROM LAST STRESS PERIOD')                        
      GO TO 55                                                                  
C                                                                               
C3------IF INRECH=>0 THEN CALL U2DREL TO READ RECHARGE RATE.                    
   32 CALL U2DREL(RECH,ANAME(1,2),NROW,NCOL,0,IN,IOUT)                          
C                                                                               
C4------MULTIPLY RECHARGE RATE BY CELL AREA TO GET VOLUMETRIC RATE.             
      DO 50 IR=1,NROW                                                           
      DO 50 IC=1,NCOL                                                           
      RECH(IC,IR)=RECH(IC,IR)*DELR(IC)*DELC(IR)                                 
   50 CONTINUE                                                                  
C                                                                               
C5------IF NRCHOP=2 THEN A LAYER INDICATOR ARRAY IS NEEDED.                     
  55  IF (NRCHOP.NE.2)GO TO 60                                                  
C                                                                               
C6------IF INIRCH<0 THEN REUSE LAYER INDICATOR ARRAY.                           
      IF(INIRCH.GE.0)GO TO 58                                                   
      WRITE(IOUT,2)                                                             
    2 FORMAT(1H0,'REUSING IRCH FROM LAST STRESS PERIOD')                        
      GO TO 60                                                                  
C                                                                               
C7------IF INIRCH=>0 CALL U2DINT TO READ LAYER IND ARRAY(IRCH)                  
   58 CALL U2DINT(IRCH,ANAME(1,1),NROW,NCOL,0,IN,IOUT)                          
C                                                                               
C8------RETURN                                                                  
   60 RETURN                                                                    
      END                                                                       
      SUBROUTINE RCH1FM(NRCHOP,IRCH,RECH,RHS,IBOUND,NCOL,                       
     1                     NROW,NLAY)
C                                                                               
C-----VERSION 1518 22DEC1982 RCH1FM                                             
C     ******************************************************************        
C     ADD RECHARGE TO RHS                                                       
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION IRCH(NCOL,NROW),RECH(NCOL,NROW),                                
     1          RHS(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)                      
C     ------------------------------------------------------------------        
C                                                                               
C1------IF NRCHOP IS 1 RECHARGE IS IN TOP LAYER. LAYER INDEX IS 1.              
      IF(NRCHOP.NE.1) GO TO 15                                                  
C                                                                               
      DO 10 IR=1,NROW                                                           
      DO 10 IC=1,NCOL                                                           
C                                                                               
C1A-----IF CELL IS EXTERNAL THERE IS NO RECHARGE INTO IT.                       
      IF(IBOUND(IC,IR,1).LE.0)GO TO 10                                          
C                                                                               
C1B-----SUBTRACT RECHARGE RATE FROM RIGHT-HAND-SIDE.                            
      RHS(IC,IR,1)=RHS(IC,IR,1)-RECH(IC,IR)                                     
   10 CONTINUE                                                                  
      GO TO 100                                                                 
C                                                                               
C2------IF OPTION IS 2 THEN RECHARGE IS INTO LAYER IN INDICATOR ARRAY           
   15 IF(NRCHOP.NE.2)GO TO 25                                                   
      DO 20 IR=1,NROW                                                           
      DO 20 IC=1,NCOL                                                           
C                                                                               
C2A-----LAYER INDEX IS IN INDICATOR ARRAY.                                      
      IL=IRCH(IC,IR)                                                            
C                                                                               
C2B-----IF THE CELL IS EXTERNAL THERE IS NO RECHARGE INTO IT.                   
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 20                                         
C                                                                               
C2C-----SUBTRACT RECHARGE FROM RIGHT-HAND-SIDE.                                 
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-RECH(IC,IR)                                   
   20 CONTINUE                                                                  
      GO TO 100                                                                 
C                                                                               
C3------IF OPTION IS 3 RECHARGE IS INTO HIGHEST INTERNAL CELL.                  
   25 IF(NRCHOP.NE.3)GO TO 100                                                  
C        CANNOT PASS THROUGH CONSTANT HEAD NODE                                 
      DO 30 IR=1,NROW                                                           
      DO 30 IC=1,NCOL                                                           
      DO 28 IL=1,NLAY                                                           
C                                                                               
C3A-----IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION.           
      IF(IBOUND(IC,IR,IL).LT.0) GO TO 30                                        
C                                                                               
C3B-----IF CELL IS INACTIVE MOVE DOWN A LAYER.                                  
      IF (IBOUND(IC,IR,IL).EQ.0)GO TO 28                                        
C                                                                               
C3C-----SUBTRACT RECHARGE FROM RIGHT-HAND-SIDE.                                 
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-RECH(IC,IR)                                   
      GO TO 30                                                                  
   28 CONTINUE                                                                  
   30 CONTINUE                                                                  
  100 CONTINUE                                                                  
C                                                                               
C4------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE RCH1BD(NRCHOP,IRCH,RECH,IBOUND,NROW,NCOL,NLAY,                 
     1    DELT,VBVL,VBNM,MSUM,KSTP,KPER,IRCHCB,ICBCFL,BUFF,IOUT)                
C                                                                               
C-----VERSION 1533 22DEC1982 RCH1BD                                             
C     ******************************************************************        
C     CALCULATE VOLUMETRIC BUDGET FOR RECHARGE                                  
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION IRCH(NCOL,NROW),RECH(NCOL,NROW),                                
     1          IBOUND(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY),                    
     2          VBVL(4,20),VBNM(4,20)                                           
      DIMENSION TEXT(4)                                                         
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /'    ','    ','RECH','ARGE'/        
C     ------------------------------------------------------------------        
C                                                                               
C1------CLEAR THE RATE ACCUMULATORS.                                            
      RATIN=0.                                                                  
      RATOUT=0.                                                                 
C                                                                               
C2------IF CELL-BY-CELL FLOW TERMS WILL BE SAVED THEN CLEAR THE BUFFER.         
      IBD=0                                                                     
      IF(ICBCFL.EQ.0 .OR. IRCHCB.LE.0) GO TO 5                                  
      IBD=1                                                                     
      DO 2 IL=1,NLAY                                                            
      DO 2 IR=1,NROW                                                            
      DO 2 IC=1,NCOL                                                            
      BUFF(IC,IR,IL)=0.                                                         
    2 CONTINUE                                                                  
C                                                                               
C3------IF NRCHOP=1 RECH GOES INTO LAYER 1. PROCESS EACH HORIZONTAL             
C3------CELL LOCATION.                                                          
    5 IF(NRCHOP.NE.1) GO TO 15                                                  
C                                                                               
C     ---RECHARGE IS APPLIED TO TOP LAYER                                       
      DO 10 IR=1,NROW                                                           
      DO 10 IC=1,NCOL                                                           
C                                                                               
C3A-----IF CELL IS EXTERNAL THEN DO NOT DO BUDGET FOR IT.                       
      IF(IBOUND(IC,IR,1).LE.0)GO TO 10                                          
      Q=RECH(IC,IR)                                                             
C                                                                               
C3B-----IF CELL-BY-CELL FLOW TERMS WILL BE SAVED THEN ADD RECH TO BUFF          
      IF(IBD.EQ.1) BUFF(IC,IR,1)=Q                                              
C                                                                               
C3C-----IF RECH POSITIVE ADD IT TO RATIN ELSE ADD IT TO RATOUT.                 
      IF(Q) 8,10,7                                                              
    7 RATIN=RATIN+Q                                                             
      GO TO 10                                                                  
    8 RATOUT=RATOUT-Q                                                           
   10 CONTINUE                                                                  
      GO TO 100                                                                 
C                                                                               
C4------IF NRCHOP=2 RECH IS IN LAYER SHOWN IN INDICATOR ARRAY(IRCH).            
C4------PROCESS HORIZONTAL CELL LOCATIONS ONE AT A TIME.                        
   15 IF(NRCHOP.NE.2)GO TO 25                                                   
      DO 20 IR=1,NROW                                                           
      DO 20 IC=1,NCOL                                                           
C                                                                               
C4A-----GET LAYER INDEX FROM INDICATOR ARRAY(IRCH).                             
      IL=IRCH(IC,IR)                                                            
C                                                                               
C4B-----IF CELL IS EXTERNAL DO NOT CALCULATE BUDGET FOR IT.                     
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 20                                         
      Q=RECH(IC,IR)                                                             
C                                                                               
C4C-----IF C-B-C FLOW TERMS WILL BE SAVED THEN ADD RECHARGE TO BUFFER.          
      IF(IBD.EQ.1) BUFF(IC,IR,IL)=Q                                             
C                                                                               
C4D-----IF RECHARGE IS POSITIVE ADD TO RATIN ELSE ADD IT TO RATOUT.             
      IF(Q) 18,20,17                                                            
   17 RATIN=RATIN+Q                                                             
      GO TO 20                                                                  
   18 RATOUT=RATOUT-Q                                                           
   20 CONTINUE                                                                  
      GO TO 100                                                                 
C                                                                               
C5------IF OPTION=3 RECHARGE IS INTO HIGHEST INTERNAL CELL. IT WILL NOT         
C5------PASS THROUGH A CONSTANT HEAD CELL. PROCESS HORIZONTAL CELL              
C5------LOCATIONS ONE AT A TIME.                                                
   25 IF(NRCHOP.NE.3)GO TO 100                                                  
      DO 30 IR=1,NROW                                                           
      DO 30 IC=1,NCOL                                                           
      DO 28 IL=1,NLAY                                                           
C                                                                               
C5A-----IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION.           
      IF(IBOUND(IC,IR,IL).LT.0) GO TO 30                                        
C                                                                               
C5B-----IF CELL IS INACTIVE MOVE DOWN TO NEXT CELL.                             
      IF (IBOUND(IC,IR,IL).EQ.0)GO TO 28                                        
      Q=RECH(IC,IR)                                                             
C                                                                               
C5C-----IF C-B-C FLOW TERMS TO BE SAVED THEN ADD RECHARGE TO BUFFER.            
      IF(IBD.EQ.1) BUFF(IC,IR,IL)=Q                                             
C                                                                               
C5D-----IF RECH IS POSITIVE ADD IT TO RATIN ELSE ADD IT TO RATOUT.              
      IF(Q) 27,30,26                                                            
   26 RATIN=RATIN+Q                                                             
      GO TO 30                                                                  
   27 RATOUT=RATOUT-Q                                                           
      GO TO 30                                                                  
   28 CONTINUE                                                                  
   30 CONTINUE                                                                  
C                                                                               
  100 CONTINUE                                                                  
C                                                                               
C6------IF C-B-C FLOW TERMS TO BE SAVED CALL UBUDSV TO WRITE THEM.              
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IRCHCB,BUFF,NCOL,NROW,            
     1                          NLAY,IOUT)                                      
C                                                                               
C7------MOVE TOTAL RECHARGE RATE INTO VBVL FOR PRINTING BY BAS1OT.              
      VBVL(4,MSUM)=RATOUT                                                       
      VBVL(3,MSUM)=RATIN                                                        
C                                                                               
C8------ADD RECHARGE FOR TIME STEP TO RECHARGE ACCUMULATOR IN VBVL.             
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT                                     
      VBVL(1,MSUM)=VBVL(1,MSUM)+RATIN*DELT                                      
C                                                                               
C9------MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS_OT.             
      VBNM(1,MSUM)=TEXT(1)                                                      
      VBNM(2,MSUM)=TEXT(2)                                                      
      VBNM(3,MSUM)=TEXT(3)                                                      
      VBNM(4,MSUM)=TEXT(4)                                                      
C                                                                               
C10-----INCREMENT BUDGET TERM COUNTER.                                          
      MSUM=MSUM+1                                                               
C                                                                               
C11-----RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE WEL1AL(ISUM,LENX,LCWELL,MXWELL,NWELLS,IN,IOUT,                 
     1                       IWELCB)                                            
C                                                                               
C-----VERSION 0933 08DEC1983 WEL1AL                                             
C     ******************************************************************        
C     ALLOCATE ARRAY STORAGE FOR WELL PACKAGE                                   
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
C                                                                               
C1------IDENTIFY PACKAGE AND INITIALIZE NWELLS                                  
      WRITE(IOUT,1)IN                                                           
    1 FORMAT(1H0,'WEL1 -- WELL PACKAGE, VERSION 1, 12/08/83',                   
     2' INPUT READ FROM',I3)                                                    
      NWELLS=0                                                                  
C                                                                               
C2------READ MAX NUMBER OF WELLS AND                                            
C2------UNIT OR FLAG FOR CELL-BY-CELL FLOW TERMS.                               
      READ(IN,2) MXWELL,IWELCB                                                  
    2 FORMAT(2I10)                                                              
      WRITE(IOUT,3) MXWELL                                                      
    3 FORMAT(1H ,'MAXIMUM OF',I5,' WELLS')                                      
      IF(IWELCB.GT.0) WRITE(IOUT,9) IWELCB                                      
    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT',I3)               
      IF(IWELCB.LT.0) WRITE(IOUT,8)                                             
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')         
C                                                                               
C3------SET LCWELL EQUAL TO LOCATION OF WELL LIST IN X ARRAY.                   
      LCWELL=ISUM                                                               
C                                                                               
C4------ADD AMOUNT OF SPACE USED BY WELL LIST TO ISUM.                          
      ISP=4*MXWELL                                                              
      ISUM=ISUM+ISP                                                             
C                                                                               
C5------PRINT NUMBER OF SPACES IN X ARRAY USED BY WELL PACKAGE.                 
      WRITE(IOUT,4) ISP                                                         
    4 FORMAT(1X,I6,' ELEMENTS IN X ARRAY ARE USED FOR WELLS')                   
      ISUM1=ISUM-1                                                              
      WRITE(IOUT,5) ISUM1,LENX                                                  
    5 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED OUT OF',I7)                       
C                                                                               
C6------IF THERE ISN'T ENOUGH SPACE IN THE X ARRAY THEN PRINT                   
C6------A WARNING MESSAGE.                                                      
      IF(ISUM1.GT.LENX) WRITE(IOUT,6)                                           
    6 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')                  
C7------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE WEL1RP(WELL,NWELLS,MXWELL,IN,IOUT)                             
C                                                                               
C                                                                               
C-----VERSION 1544 22DEC1982 WEL1RP                                             
C     ******************************************************************        
C     READ NEW WELL LOCATIONS AND STRESS RATES                                  
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION WELL(4,MXWELL)                                                  
C     ------------------------------------------------------------------        
C                                                                               
C1------READ ITMP(NUMBER OF WELLS OR FLAG SAYING REUSE WELL DATA)               
      READ (IN,1) ITMP                                                          
    1 FORMAT(I10)                                                               
      IF(ITMP.GE.0) GO TO 50                                                    
C                                                                               
C1A-----IF ITMP LESS THAN ZERO REUSE DATA. PRINT MESSAGE AND RETURN.            
      WRITE(IOUT,6)                                                             
    6 FORMAT(1H0,'REUSING WELLS FROM LAST STRESS PERIOD')                       
      RETURN                                                                    
C                                                                               
C1B-----ITMP=>0.  SET NWELLS EQUAL TO ITMP.                                     
   50 NWELLS=ITMP                                                               
      IF(NWELLS.LE.MXWELL) GO TO 100                                            
C                                                                               
C2------NWELLS>MXWELL.  PRINT MESSAGE. STOP.                                    
      WRITE(IOUT,99) NWELLS,MXWELL                                              
   99 FORMAT(1H0,'NWELLS(',I4,') IS GREATER THAN MXWELL(',I4,')')               
      STOP                                                                      
C                                                                               
C3------PRINT NUMBER OF WELLS IN CURRENT STRESS PERIOD.                         
  100 WRITE (IOUT,2) NWELLS                                                     
    2 FORMAT(1H0,10X,I4,' WELLS')                                               
C                                                                               
C4------IF THERE ARE NO ACTIVE WELLS IN THIS STRESS PERIOD THEN RETURN          
      IF(NWELLS.EQ.0) GO TO 260                                                 
C                                                                               
C5------READ AND PRINT LAYER,ROW,COLUMN AND RECHARGE RATE.                      
      WRITE(IOUT,3)                                                             
    3 FORMAT(1H ,47X,'LAYER    ROW    COL    STRESS RATE   WELL NO.'/           
     1,48X,45('-'))                                                             
      DO 250 II=1,NWELLS                                                        
      READ (IN,4) K,I,J,Q                                                       
    4 FORMAT(3I10,F10.0)                                                        
      WRITE (IOUT,5) K,I,J,Q,II                                                 
    5 FORMAT(48X,I3,I8,I7,G16.5,I8)                                             
      WELL(1,II)=K                                                              
      WELL(2,II)=I                                                              
      WELL(3,II)=J                                                              
      WELL(4,II)=Q                                                              
  250 CONTINUE                                                                  
C                                                                               
C6------RETURN                                                                  
  260 RETURN                                                                    
      END                                                                       
      SUBROUTINE WEL1FM(NWELLS,MXWELL,RHS,WELL,IBOUND,                          
     1        NCOL,NROW,NLAY)                                              
C                                                                               
C-----VERSION 1001 26AUG1982 WEL1FM                                             
C                                                                               
C     ******************************************************************        
C     ADD WELL FLOW TO SOURCE TERM                                              
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION RHS(NCOL,NROW,NLAY),WELL(4,MXWELL),                             
     1            IBOUND(NCOL,NROW,NLAY)                                        
C     ------------------------------------------------------------------        
C1------IF NUMBER OF WELLS <= 0 THEN RETURN.                                    
      IF(NWELLS.LE.0) RETURN                                                    
C                                                                               
C2------PROCESS EACH WELL IN THE WELL LIST.                                     
      DO 100 L=1,NWELLS                                                         
      IR=WELL(2,L)                                                              
      IC=WELL(3,L)                                                              
      IL=WELL(1,L)                                                              
      Q=WELL(4,L)                                                               
C                                                                               
C2A-----IF THE CELL IS INACTIVE THEN BYPASS PROCESSING.                         
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100                                       
C                                                                               
C2B-----IF THE CELL IS VARIABLE HEAD THEN ADD RECHARGE RATE                     
C       TO THE RHS ACCUMULATOR.                                                 
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-Q                                             
  100 CONTINUE                                                                  
C                                                                               
C3------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE WEL1BD(NWELLS,MXWELL,VBNM,VBVL,MSUM,WELL,IBOUND,DELT,          
     1             NCOL,NROW,NLAY,KSTP,KPER,IWELCB,ICBCFL,BUFF,IOUT)            
C                                                                               
C-----VERSION 1449 20MAY1983 WEL1BD        
C     CHANGED TO ALLOW OUTPUT TO TAPE74   DRS  8/27/1986                       
C     ******************************************************************        
C     CALCULATE VOLUMETRIC BUDGET FOR WELLS                                     
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION VBNM(4,MSUM),VBVL(4,MSUM),WELL(4,MXWELL),                       
     1          IBOUND(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY)                     
      DIMENSION TEXT(4)                                                         
C                                                                               
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /'    ','    ','   W','ELLS'/        
C     ------------------------------------------------------------------        
C                                                                               
C1------CLEAR RATIN AND RATOUT ACCUMULATORS.                                    
      RATIN=0.                                                                  
      RATOUT=0.                                                                 
      IBD=0                                                                     
C                                                                               
C2------IF THERE ARE NO WELLS DO NOT ACCUMULATE FLOW                            
      IF(NWELLS.EQ.0) GO TO 200                                                 
C                                                                               
C3------TEST TO SEE IF CELL-BY-CELL FLOW TERMS WILL BE RECORDED.                
      GO TO 60                                 
C                                                                               
C4------IF CELL-BY-CELL FLOWS WILL BE SAVED THEN CLEAR THE BUFFER.              
      IBD=1                                                                     
      DO 50 IL=1,NLAY                                                           
      DO 50 IR=1,NROW                                                           
      DO 50 IC=1,NCOL                                                           
      BUFF(IC,IR,IL)=0.                                                         
   50 CONTINUE                                                                  
C                                                                               
C5------PROCESS WELLS ONE AT A TIME.                                            
   60 DO 100 L=1,NWELLS                                                         
      IR=WELL(2,L)                                                              
      IC=WELL(3,L)                                                              
      IL=WELL(1,L)                                                              
      Q=WELL(4,L)                                                               
C                                                                               
C5A-----IF THE CELL IS EXTERNAL IGNORE IT.                                      
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 100                                        
C                                                                               
C5B-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IWELCB<0).                      
      IF(IWELCB.LT.0.AND.ICBCFL.NE.0) WRITE(IOUT,900) (TEXT(N),N=1,4),          
     1    KPER,KSTP,L,IL,IR,IC,Q                                                
  900 FORMAT(1H0,4A4,'   PERIOD',I3,'   STEP',I3,'   WELL',I4,                  
     1    '   LAYER',I3,'   ROW ',I4,'   COL',I4,'   RATE',G15.7)               
C                                                                               
C5C-----IF CELL-BY-CELL FLOWS ARE TO BE SAVED THEN ADD THEM TO BUFFER.          
      IF(IBD.EQ.1) BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+Q                              
      IF(Q) 90,100,80                                                           
C                                                                               
C5D-----PUMPING RATE IS POSITIVE(RECHARGE). ADD IT TO RATIN.                    
   80 RATIN=RATIN+Q                                                             
      GO TO 100                                                                 
C                                                                               
C5E-----PUMPING RATE IS NEGATIVE(DISCHARGE). ADD IT TO RATOUT.                  
   90 RATOUT=RATOUT-Q                                                           
  100 CONTINUE                                          
C                                                                               
C6------IF CELL-BY-CELL FLOWS WILL BE SAVED CALL UBUDSV TO RECORD THEM          
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IWELCB,BUFF,NCOL,NROW,            
     1                          NLAY,IOUT)                                      
C                                                                               
C7------MOVE RATES INTO VBVL FOR PRINTING BY MODULE BAS1OT.                     
  200 VBVL(3,MSUM)=RATIN                                                        
      VBVL(4,MSUM)=RATOUT                                                     
      IF(ICBCFL.EQ.0) GOTO 960
      WRITE(74,950) RATOUT
  950 FORMAT(1G15.5)
  960 CONTINUE
C
C8------MOVE RATES TIMES TIME STEP LENGTH INTO VBVL ACCUMULATORS.               
      VBVL(1,MSUM)=VBVL(1,MSUM)+RATIN*DELT                                      
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT                                     
C                                                                               
C9------MOVE BUDGET TERM LABELS INTO VBNM FOR PRINTING.                         
      VBNM(1,MSUM)=TEXT(1)                                                      
      VBNM(2,MSUM)=TEXT(2)                                                      
      VBNM(3,MSUM)=TEXT(3)                                                      
      VBNM(4,MSUM)=TEXT(4)                                                      
C                                                                               
C10-----INCREMENT BUDGET TERM COUNTER(MSUM).                                    
      MSUM=MSUM+1                                                               
C                                                                               
C11-----RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DRN1AL(ISUM,LENX,LCDRAI,NDRAIN,MXDRN,IN,IOUT,                  
     1                           IDRNCB)                                        
C                                                                               
C-----VERSION 0956 08DEC1983 DRN1AL                                             
C     ******************************************************************        
C     ALLOCATE ARRAY STORAGE FOR DRAIN PACKAGE                                  
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
C                                                                               
C1------IDENTIFY PACKAGE AND INITIALIZE NDRAIN.                                 
      WRITE(IOUT,1)IN                                                           
    1 FORMAT(1H0,'DRN1 -- DRAIN PACKAGE, VERSION 1, 12/08/83',                  
     2' INPUT READ FROM UNIT',I3)                                               
      NDRAIN=0                                                                  
C                                                                               
C2------READ & PRINT MXDRN & IDRNCB(UNIT & FLAG FOR CELL-BY-CELL FLOW)          
      READ(IN,2) MXDRN,IDRNCB                                                   
    2 FORMAT(2I10)                                                              
      WRITE(IOUT,3) MXDRN                                                       
    3 FORMAT(1H ,'MAXIMUM OF',I5,' DRAINS')                                     
      IF(IDRNCB.GT.0) WRITE(IOUT,9) IDRNCB                                      
    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT',I3)               
      IF(IDRNCB.LT.0) WRITE(IOUT,8)                                             
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')         
C                                                                               
C3------SET LCDRAI EQUAL TO ADDRESS OF FIRST UNUSED SPACE IN X.                 
      LCDRAI=ISUM                                                               
C                                                                               
C4------CALCULATE AMOUNT OF SPACE USED BY THE DRAIN PACKAGE.                    
      ISP=5*MXDRN                                                               
      ISUM=ISUM+ISP                                                             
C                                                                               
C5------PRINT AMOUNT OF SPACE USED BY DRAIN PACKAGE.                            
      WRITE(IOUT,4) ISP                                                         
    4 FORMAT(1X,I6,' ELEMENTS IN X ARRAY ARE USED FOR DRAINS')                  
      ISUM1=ISUM-1                                                              
      WRITE(IOUT,5) ISUM1,LENX                                                  
    5 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED OUT OF',I7)                       
      IF(ISUM1.GT.LENX) WRITE(IOUT,6)                                           
    6 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')                  
C                                                                               
C6------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DRN1RP(DRAI,NDRAIN,MXDRN,IN,IOUT)                              
C                                                                               
C                                                                               
C-----VERSION 1603 25APR1983 DRN1RP                                             
C     ******************************************************************        
C     READ DRAIN LOCATIONS, ELEVATIONS, AND CONDUCTANCES                        
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION DRAI(5,MXDRN)                                                   
C     ------------------------------------------------------------------        
C                                                                               
C1------READ ITMP(NUMBER OF DRAIN CELLS OR FLAG TO REUSE DATA)                  
      READ(IN,8) ITMP                                                           
    8 FORMAT(I10)                                                               
C                                                                               
C2------TEST ITMP                                                               
      IF(ITMP.GE.0) GO TO 50                                                    
C                                                                               
C2A-----IF ITMP<0 THEN REUSE DATA FROM LAST STRESS PERIOD.                      
      WRITE(IOUT,7)                                                             
    7 FORMAT(1H0,'REUSING DRAINS FROM LAST STRESS PERIOD')                      
      RETURN                                                                    
C                                                                               
C3------IF ITMP=>0 THEN IT IS THE NUMBER OF DRAINS.                             
   50 NDRAIN=ITMP                                                               
      IF(NDRAIN.LE.MXDRN) GO TO 100                                             
C                                                                               
C4------IF NDRAIN>MXDRN THEN STOP                                               
      WRITE(IOUT,99) NDRAIN,MXDRN                                               
   99 FORMAT(1H0,'NDRAIN(',I4,') IS GREATER THAN MXDRN(',I4,')')                
      STOP                                                                      
C                                                                               
C5------PRINT NUMBER OF DRAINS IN THIS STRESS PERIOD.                           
  100 WRITE(IOUT,1) NDRAIN                                                      
    1 FORMAT(1H0,//1X,I5,' DRAINS')                                             
C                                                                               
C6------IF THERE ARE NO DRAINS THEN RETURN.                                     
      IF(NDRAIN.EQ.0) GO TO 260                                                 
C                                                                               
C7------READ AND PRINT DATA FOR EACH DRAIN.                                     
      WRITE(IOUT,3)                                                             
    3 FORMAT(1H0,15X,'LAYER',5X,'ROW',5X                                        
     1,'COL   ELEVATION   CONDUCTANCE   DRAIN NO.'/1X,15X,60('-'))              
      DO 250 II=1,NDRAIN                                                        
      READ (IN,4) K,I,J,DRAI(4,II),DRAI(5,II)                                   
    4 FORMAT(3I10,2F10.0)                                                       
      WRITE (IOUT,5) K,I,J,DRAI(4,II),DRAI(5,II),II                             
    5 FORMAT(1X,15X,I4,I9,I8,G13.4,G14.4,I8)                                    
      DRAI(1,II)=K                                                              
      DRAI(2,II)=I                                                              
      DRAI(3,II)=J                                                              
  250 CONTINUE                                                                  
C                                                                               
C8------RETURN                                                                  
  260 RETURN                                                                    
C                                                                               
      END                                                                       
      SUBROUTINE DRN1FM(NDRAIN,MXDRN,DRAI,HNEW,HCOF,RHS,IBOUND,                 
     1              NCOL,NROW,NLAY)                                        
C                                                                               
C-----VERSION 1638 25APR1983 DRN1FM                                             
C                                                                               
C     ******************************************************************        
C     ADD DRAIN FLOW TO SOURCE TERM                                             
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
C                                                                               
      DIMENSION DRAI(5,MXDRN),HNEW(NCOL,NROW,NLAY),                             
     1         RHS(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),                      
     1         HCOF(NCOL,NROW,NLAY)                                             
C     ------------------------------------------------------------------        
C                                                                               
C1------IF NDRAIN<=0 THERE ARE NO DRAINS. RETURN                                
      IF(NDRAIN.LE.0) RETURN                                                    
C                                                                               
C2------PROCESS EACH CELL IN THE DRAIN LIST                                     
      DO 100 L=1,NDRAIN                                                         
C                                                                               
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING DRAIN.                     
      IL=DRAI(1,L)                                                              
      IR=DRAI(2,L)                                                              
      IC=DRAI(3,L)                                                              
C                                                                               
C4-------IF THE CELL IS EXTERNAL SKIP IT.                                       
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100                                       
C                                                                               
C5-------IF THE CELL IS INTERNAL GET THE DRAIN DATA.                            
      EL=DRAI(4,L)                                                              
      HHNEW=HNEW(IC,IR,IL)                                                      
C                                                                               
C6------IF HEAD IS LOWER THAN DRAIN THEN SKIP THIS CELL.                        
      IF(HHNEW.LE.EL) GO TO 100                                                 
C                                                                               
C7------HEAD IS HIGHER THAN DRAIN. ADD TERMS TO RHS AND HCOF.                   
      C=DRAI(5,L)                                                               
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C                                           
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*EL                                          
  100 CONTINUE                                                                  
C                                                                               
C8------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DRN1BD(NDRAIN,MXDRN,VBNM,VBVL,MSUM,DRAI,DELT,HNEW,             
     1        NCOL,NROW,NLAY,IBOUND,KSTP,KPER,IDRNCB,ICBCFL,BUFF,IOUT)          
C                                                                               
C-----VERSION 1301 28DEC1983 DRN1BD                                             
C                                                                               
C     ******************************************************************        
C     CALCULATE VOLUMETRIC BUDGET FOR DRAINS                                    
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
C                                                                               
      DIMENSION VBNM(4,MSUM),VBVL(4,MSUM),DRAI(5,MXDRN),                        
     1           HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),                   
     2           BUFF(NCOL,NROW,NLAY)                                           
      DIMENSION TEXT(4)                                                         
C                                                                               
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /'    ','    ','  DR','AINS'/        
C     ------------------------------------------------------------------        
C                                                                               
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND                        
C1------ACCUMULATORS (RATIN AND RATOUT).                                        
      RATOUT=0.                                                                 
      IBD=0                                                                     
C                                                                               
C2------IF THERE ARE NO DRAINS THEN DO NOT ACCUMULATE DRAIN FLOW                
      IF(NDRAIN.LE.0) GO TO 200                                                 
C                                                                               
C3------TEST TO SEE IF CELL-BY-CELL FLOW TERMS ARE NEEDED.                      
      IF(ICBCFL.EQ.0 .OR. IDRNCB.LE.0) GO TO 60                                 
C                                                                               
C3B-----CELL-BY-CELL FLOW TERMS ARE NEEDED SET IBD AND CLEAR BUFFER.            
      IBD=1                                                                     
      DO 50 IL=1,NLAY                                                           
      DO 50 IR=1,NROW                                                           
      DO 50 IC=1,NCOL                                                           
      BUFF(IC,IR,IL)=0.                                                         
   50 CONTINUE                                                                  
C                                                                               
C4------FOR EACH DRAIN ACCUMULATE DRAIN FLOW                                    
   60 DO 100 L=1,NDRAIN                                                         
C                                                                               
C5------GET LAYER, ROW & COLUMN OF CELL CONTAINING REACH.                       
      IL=DRAI(1,L)                                                              
      IR=DRAI(2,L)                                                              
      IC=DRAI(3,L)                                                              
C                                                                               
C6------IF CELL IS EXTERNAL IGNORE IT.                                          
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100                                       
C                                                                               
C7------GET DRAIN PARAMETERS FROM DRAIN LIST.                                   
      EL=DRAI(4,L)                                                              
      C=DRAI(5,L)                                                               
      HHNEW=HNEW(IC,IR,IL)                                                      
C                                                                               
C8------IF HEAD LOWER THAN DRAIN THEN FORGET THIS CELL.                         
      IF(HHNEW.LE.EL) GO TO 100                                                 
C                                                                               
C9------HEAD HIGHER THAN DRAIN. CALCULATE Q=C*(EL-HHNEW) ADD Q TO RATOUT        
      Q=C*(EL-HHNEW)                                                            
      RATOUT=RATOUT-Q                                                           
C                                                                               
C10-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IDRNCB<0).                      
      IF(IDRNCB.LT.0.AND.ICBCFL.NE.0) WRITE(IOUT,900) (TEXT(N),N=1,4),          
     1    KPER,KSTP,L,IL,IR,IC,Q                                                
  900 FORMAT(1H0,4A4,'   PERIOD',I3,'   STEP',I3,'   DRAIN',I4,                 
     1    '   LAYER',I3,'   ROW',I4,'   COL',I4,'   RATE',G15.7)                
C                                                                               
C11-----IF C-B-C FLOW TERMS ARE TO BE SAVED THEN ADD Q TO BUFFER.               
      IF(IBD.EQ.1) BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+Q                              
  100 CONTINUE                                                                  
C                                                                               
C12-----IF C-B-C FLOW TERMS WILL BE SAVED CALL UBUDSV TO RECORD THEM.           
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IDRNCB,BUFF,NCOL,NROW,            
     1                          NLAY,IOUT)                                      
C                                                                               
C13-----MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.                   
  200 VBVL(3,MSUM)=0.                                                           
      VBVL(4,MSUM)=RATOUT                                                       
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT                                     
      VBNM(1,MSUM)=TEXT(1)                                                      
      VBNM(2,MSUM)=TEXT(2)                                                      
      VBNM(3,MSUM)=TEXT(3)                                                      
      VBNM(4,MSUM)=TEXT(4)                                                      
C                                                                               
C14-----INCREMENT BUDGET TERM COUNTER                                           
      MSUM=MSUM+1                                                               
C                                                                               
C15-----RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE EVT1AL(ISUM,LENX,LCIEVT,LCEVTR,LCEXDP,LCSURF,                  
     1                  NCOL,NROW,NEVTOP,IN,IOUT,IEVTCB)                        
C                                                                               
C-----VERSION 0943 08DEC1983 EVT1AL                                             
C     ******************************************************************        
C     ALLOCATE ARRAY STORAGE FOR EVAPOTRANSPIRATION                             
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
C                                                                               
C1------IDENTIFY PACKAGE.                                                       
      WRITE(IOUT,1)IN                                                           
    1 FORMAT(1H0,'EVT1 -- EVAPOTRANSPIRATION PACKAGE, VERSION 1,',              
     1     ' 12/08/83',' INPUT READ FROM UNIT',I3)                              
C                                                                               
C2------READ NEVTOP AND IEVTCB.                                                 
      READ(IN,3)NEVTOP,IEVTCB                                                   
    3 FORMAT(2I10)                                                              
C                                                                               
C3------CHECK TO SEE THAT ET OPTION IS LEGAL.                                   
      IF(NEVTOP.GE.1.AND.NEVTOP.LE.2)GO TO 200                                  
C                                                                               
C3A-----IF ILLEGAL PRINT A MESSAGE & ABORT SIMULATION.                          
      WRITE(IOUT,8)                                                             
    8 FORMAT(1X,'ILLEGAL ET OPTION CODE. SIMULATION ABORTING')                  
      STOP                                                                      
C                                                                               
C4------IF THE OPTION IS LEGAL THEN PRINT THE OPTION CODE.                      
  200 IF(NEVTOP.EQ.1) WRITE(IOUT,201)                                           
  201 FORMAT(1X,'OPTION 1 -- EVAPOTRANSPIRATION FROM TOP LAYER')                
      IF(NEVTOP.EQ.2) WRITE(IOUT,202)                                           
  202 FORMAT(1X,'OPTION 2 -- EVAPOTRANSPIRATION FROM ONE SPECIFIED',            
     1      ' NODE IN EACH VERTICAL COLUMN')                                    
      IRK=ISUM                                                                  
C                                                                               
C5------IF CELL-BY-CELL TERMS TO BE SAVED THEN PRINT UNIT NUMBER.               
      IF(IEVTCB.GT.0) WRITE(IOUT,203) IEVTCB                                    
  203 FORMAT(1X,'CELL-BY-CELL FLOW TERMS WILL BE SAVED ON UNIT',I3)             
C                                                                               
C6------ALLOCATE SPACE FOR THE ARRAYS EVTR, EXDP AND SURF.                      
      LCEVTR=ISUM                                                               
      ISUM=ISUM+NCOL*NROW                                                       
      LCEXDP=ISUM                                                               
      ISUM=ISUM+NCOL*NROW                                                       
      LCSURF=ISUM                                                               
      ISUM=ISUM+NCOL*NROW                                                       
C                                                                               
C7------IF OPTION 2 THEN ALLOCATE SPACE FOR THE INDICATOR ARRAY(IEVT)           
      IF(NEVTOP.NE.2)GO TO 300                                                  
      LCIEVT=ISUM                                                               
      ISUM=ISUM+NCOL*NROW                                                       
C                                                                               
C8------CALCULATE & PRINT AMOUNT OF SPACE USED BY ET PACKAGE.                   
  300 IRK=ISUM-IRK                                                              
      WRITE(IOUT,4)IRK                                                          
    4 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED FOR EVAPOTRANSPIRATION')          
      ISUM1=ISUM-1                                                              
      WRITE(IOUT,5)ISUM1,LENX                                                   
    5 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED OUT OF',I7)                       
      IF(ISUM1.GT.LENX)WRITE(IOUT,6)                                            
    6 FORMAT(1X,'   ***X ARRAY MUST BE MADE LARGER***')                         
C                                                                               
C9------RETURN.                                                                 
      RETURN                                                                    
      END                                                                       
      SUBROUTINE EVT1RP(NEVTOP,IEVT,EVTR,EXDP,SURF,DELR,DELC,                   
     1                  NCOL,NROW,NLAY,IN,IOUT)                                 
C                                                                               
C-----VERSION 1631 08FEB1983 EVT1RP                                             
C     ******************************************************************        
C     READ EVAPOTRANSPIRATION DATA                                              
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION IEVT(NCOL,NROW),EVTR(NCOL,NROW),EXDP(NCOL,NROW),                
     1          SURF(NCOL,NROW),ANAME(6,4),DELR(NCOL),DELC(NROW)                
C                                                                               
      DATA ANAME(1,1),ANAME(2,1),ANAME(3,1),ANAME(4,1),ANAME(5,1),              
     1  ANAME(6,1) /'    ','    ','  ET',' LAY','ER I','NDEX'/                  
      DATA ANAME(1,2),ANAME(2,2),ANAME(3,2),ANAME(4,2),ANAME(5,2),              
     1  ANAME(6,2) /'    ','    ','    ','  ET',' SUR','FACE'/                  
      DATA ANAME(1,3),ANAME(2,3),ANAME(3,3),ANAME(4,3),ANAME(5,3),              
     1  ANAME(6,3) /' EVA','POTR','ANSP','IRAT','ION ','RATE'/                  
      DATA ANAME(1,4),ANAME(2,4),ANAME(3,4),ANAME(4,4),ANAME(5,4),              
     1  ANAME(6,4) /'    ','    ','EXTI','NCTI','ON D','EPTH'/                  
C     ------------------------------------------------------------------        
C                                                                               
C1------READ FLAGS SHOWING WHETHER DATA IS TO BE REUSED.                        
      READ(IN,6)INSURF,INEVTR,INEXDP,INIEVT                                     
    6 FORMAT(4I10)                                                              
C                                                                               
C2------TEST INSURF TO SEE WHERE SURFACE ELEVATION COMES FROM.                  
      IF(INSURF.GE.0)GO TO 32                                                   
C                                                                               
C2A------IF INSURF<0 THEN REUSE SURFACE ARRAY FROM LAST STRESS PERIOD           
      WRITE(IOUT,3)                                                             
    3 FORMAT(1H0,'REUSING SURF FROM LAST STRESS PERIOD')                        
      GO TO 35                                                                  
C                                                                               
C3-------IF INSURF=>0 THEN CALL MODULE U2DREL TO READ SURFACE.                  
   32 CALL U2DREL(SURF,ANAME(1,2),NROW,NCOL,0,IN,IOUT)                          
C                                                                               
C4------TEST INEVTR TO SEE WHERE MAX ET RATE COMES FROM.                        
   35 IF(INEVTR.GE.0)GO TO 37                                                   
C                                                                               
C4A-----IF INEVTR<0 THEN REUSE MAX ET RATE.                                     
      WRITE(IOUT,4)                                                             
    4 FORMAT(1H0,'REUSING EVTR FROM LAST STRESS PERIOD')                        
      GO TO 45                                                                  
C                                                                               
C5------IF INEVTR=>0 CALL MODULE U2DREL TO READ MAX ET RATE.                    
   37 CALL U2DREL(EVTR,ANAME(1,3),NROW,NCOL,0,IN,IOUT)                          
C                                                                               
C6------MULTIPLY MAX ET RATE BY CELL AREA TO GET VOLUMETRIC RATE                
      DO 40 IR=1,NROW                                                           
      DO 40 IC=1,NCOL                                                           
      EVTR(IC,IR)=EVTR(IC,IR)*DELR(IC)*DELC(IR)                                 
   40 CONTINUE                                                                  
C                                                                               
C7------TEST INEXDP TO SEE WHERE EXTINCTION DEPTH COMES FROM                    
   45 IF(INEXDP.GE.0)GO TO 47                                                   
C                                                                               
C7A------IF INEXDP<0 REUSE EXTINCTION DEPTH FROM LAST STRESS PERIOD             
      WRITE(IOUT,5)                                                             
    5 FORMAT(1H0,'REUSING EXDP FROM LAST STRESS PERIOD')                        
      GO TO 48                                                                  
C                                                                               
C8-------IF INEXDP=>0 CALL MODULE U2DREL TO READ EXTINCTION DEPTH               
   47 CALL U2DREL(EXDP,ANAME(1,4),NROW,NCOL,0,IN,IOUT)                          
C                                                                               
C9------IF OPTION(NEVTOP) IS 2 THEN WE NEED AN INDICATOR ARRAY.                 
  48  IF(NEVTOP.NE.2)GO TO 50                                                   
C                                                                               
C10------IF INIEVT<0 THEN REUSE LAYER INDICATOR ARRAY.                          
      IF(INIEVT.GE.0)GO TO 49                                                   
      WRITE(IOUT,2)                                                             
    2 FORMAT(1H0,'REUSING IEVT FROM LAST STRESS PERIOD')                        
      GO TO 50                                                                  
C                                                                               
C11------IF INIEVT=>0 THEN CALL MODULE U2DINT TO READ INDICATOR ARRAY.          
   49 CALL U2DINT(IEVT,ANAME(1,1),NROW,NCOL,0,IN,IOUT)                          
C                                                                               
C12-----RETURN                                                                  
   50 RETURN                                                                    
      END                                                                       
      SUBROUTINE EVT1FM(NEVTOP,IEVT,EVTR,EXDP,SURF,RHS,HCOF,                    
     1                  IBOUND,HNEW,NCOL,NROW,NLAY)                        
C                                                                               
C-----VERSION 0835 10FEB1983 EVT1FM                                             
C     ******************************************************************        
C        ADD EVAPOTRANSPIRATION TO RHS AND HCOF                                 
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
      DIMENSION IEVT(NCOL,NROW),EVTR(NCOL,NROW),EXDP(NCOL,NROW),                
     1          SURF(NCOL,NROW),RHS(NCOL,NROW,NLAY),                            
     2          HCOF(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),                    
     3          HNEW(NCOL,NROW,NLAY)                                            
C     ------------------------------------------------------------------        
C                                                                               
C1------PROCESS EACH HORIZONTAL CELL LOCATION                                   
      DO 10 IR=1,NROW                                                           
      DO 10 IC=1,NCOL                                                           
C                                                                               
C2------SET THE LAYER INDEX EQUAL TO 1      .                                   
      IL=1                                                                      
C                                                                               
C3------IF OPTION 2 IS SPECIFIED THEN GET LAYER INDEX FROM IEVT ARRAY           
      IF(NEVTOP.EQ.2)IL=IEVT(IC,IR)                                             
C                                                                               
C4------IF THE CELL IS EXTERNAL IGNORE IT.                                      
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 10                                         
      C=EVTR(IC,IR)                                                             
      S=SURF(IC,IR)                                                             
      H=HNEW(IC,IR,IL)                                                          
C                                                                               
C5------IF AQUIFER HEAD IS GREATER THAN OR EQUAL TO SURF, ET IS CONSTANT        
      IF(H.LT.S) GO TO 5                                                        
C                                                                               
C5A-----SUBTRACT -EVTR FROM RHS                                                 
      RHS(IC,IR,IL)=RHS(IC,IR,IL) + C                                           
      GO TO 10                                                                  
C                                                                               
C6------IF DEPTH TO WATER>=EXTINCTION DEPTH THEN ET IS 0                        
    5 D=S-H                                                                     
      X=EXDP(IC,IR)                                                             
      IF(D.GE.X)GO TO 10                                                        
C                                                                               
C7------LINEAR RANGE. ADD ET TERMS TO BOTH RHS AND HCOF.                        
      RHS(IC,IR,IL)=RHS(IC,IR,IL)+C-C*S/X                                       
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C/X                                         
   10 CONTINUE                                                                  
C                                                                               
C8------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE EVT1BD(NEVTOP,IEVT,EVTR,EXDP,SURF,IBOUND,HNEW,                 
     1           NCOL,NROW,NLAY,DELT,VBVL,VBNM,MSUM,KSTP,KPER,                  
     2           IEVTCB,ICBCFL,BUFF,IOUT)                                       
C-----VERSION 1405 10FEB1983 EVT1BD                                             
C     ******************************************************************        
C     CALCULATE VOLUMETRIC BUDGET FOR EVAPOTRANSPIRATION                        
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
      DIMENSION IEVT(NCOL,NROW),EVTR(NCOL,NROW),EXDP(NCOL,NROW),                
     1          SURF(NCOL,NROW),IBOUND(NCOL,NROW,NLAY),                         
     2          VBVL(4,20),VBNM(4,20),HNEW(NCOL,NROW,NLAY),                     
     3          BUFF(NCOL,NROW,NLAY)                                            
      DIMENSION TEXT(4)                                                         
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /'    ','    ','    ','  ET'/        
C     ------------------------------------------------------------------        
C                                                                               
C1------CLEAR THE RATE ACCUMULATOR.                                             
      RATOUT=0                                                                  
C                                                                               
C2------IF CELL-BY-CELL FLOW TERMS WILL BE SAVED THEN CLEAR THE BUFFER.         
      IBD=0                                                                     
      IF(IEVTCB.LE.0 .OR. ICBCFL.EQ.0) GO TO 5                                  
      IBD=1                                                                     
      DO 4 IL=1,NLAY                                                            
      DO 4 IR=1,NROW                                                            
      DO 4 IC=1,NCOL                                                            
      BUFF(IC,IR,IL)=0.                                                         
    4 CONTINUE                                                                  
C                                                                               
C3------PROCESS EACH HORIZONTAL CELL LOCATION                                   
    5 DO 10 IR=1,NROW                                                           
      DO 10 IC=1,NCOL                                                           
C                                                                               
C4------SET THE LAYER INDEX EQUAL TO 1      .                                   
      IL=1                                                                      
C                                                                               
C5------IF OPTION 2 IS SPECIFIED THEN GET LAYER INDEX FROM IEVT ARRAY           
      IF(NEVTOP.EQ.2)IL=IEVT(IC,IR)                                             
C                                                                               
C6------IF CELL IS EXTERNAL THEN IGNORE IT.                                     
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 10                                         
      C=EVTR(IC,IR)                                                             
      S=SURF(IC,IR)                                                             
      H=HNEW(IC,IR,IL)                                                          
C                                                                               
C7------IF AQUIFER HEAD => SURF,SET Q=MAX ET RATE                               
      IF(H.LT.S) GO TO 7                                                        
      Q=-C                                                                      
      GO TO 9                                                                   
C                                                                               
C8------IF DEPTH=>EXTINCTION DEPTH, ET IS 0                                     
    7 X=EXDP(IC,IR)                                                             
      D=S-H                                                                     
      IF(D.GE.X)GO TO 10                                                        
C                                                                               
C9------LINEAR RANGE . Q=-EVTR(H-EXEL)/EXDP                                     
      Q=C*D/X-C                                                                 
C                                                                               
C10-----ACCUMULATE TOTAL FLOW RATE                                              
    9 RATOUT=RATOUT-Q                                                           
C                                                                               
C11-----IF CELL-BY-CELL FLOW TERMS TO BE SAVED THE ADD Q TO BUFFER.             
      IF(IBD.EQ.1) BUFF(IC,IR,IL)=Q                                             
   10 CONTINUE                                                                  
C                                                                               
C12-----IF C-B-C TO BE SAVED CALL MODULE UBUDSV TO RECORD THEM.                 
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IEVTCB,BUFF,NCOL,NROW,            
     1                          NLAY,IOUT)                                      
C                                                                               
C13-----MOVE TOTAL ET RATE INTO VBVL FOR PRINTING BY BAS1OT.                    
      VBVL(3,MSUM)=0.                                                           
      VBVL(4,MSUM)=RATOUT                                                       
C                                                                               
C14-----ADD ET(ET_RATE TIMES STEP LENGTH) TO VBVL                               
      VBVL(1,MSUM)=0.                                                           
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT                                     
C                                                                               
C15-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS1OT              
      VBNM(1,MSUM)=TEXT(1)                                                      
      VBNM(2,MSUM)=TEXT(2)                                                      
      VBNM(3,MSUM)=TEXT(3)                                                      
      VBNM(4,MSUM)=TEXT(4)                                                      
C                                                                               
C16-----INCREMENT BUDGET TERM COUNTER                                           
      MSUM=MSUM+1                                                               
C                                                                               
C17-----RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE GHB1AL(ISUM,LENX,LCBNDS,NBOUND,MXBND,IN,IOUT,                  
     1                       IGHBCB)                                            
C                                                                               
C-----VERSION 0940 08DEC1983 GHB1AL                                             
C     ******************************************************************        
C     ALLOCATE ARRAY STORAGE FOR HEAD-DEPENDENT BOUNDARIES                      
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
C                                                                               
C1------IDENTIFY PACKAGE AND INITIALIZE # OF GENERAL HEAD BOUNDS                
      WRITE(IOUT,1)IN                                                           
    1 FORMAT(1H0,'GHB1 -- GHB PACKAGE, VERSION 1, 12/08/83',                    
     2' INPUT READ FROM UNIT',I3)                                               
      NBOUND=0                                                                  
C                                                                               
C2------READ AND PRINT MXBND AND IGHBCB (MAX # OF BOUNDS AND UNIT               
C2------FOR CELL-BY-CELL FLOW TERMS FOR GHB)                                    
      READ(IN,2) MXBND,IGHBCB                                                   
    2 FORMAT(2I10)                                                              
      WRITE(IOUT,3) MXBND                                                       
    3 FORMAT(1H ,'MAXIMUM OF',I5,' HEAD-DEPENDENT BOUNDARY NODES')              
      IF(IGHBCB.GT.0) WRITE(IOUT,9) IGHBCB                                      
    9 FORMAT(1X,'CELL-BY-CELL FLOW WILL BE RECORDED ON UNIT',I3)                
      IF(IGHBCB.LT.0) WRITE(IOUT,8)                                             
    8 FORMAT(1X,'CELL-BY-CELL FLOW WILL BE PRINTED WHEN ICBCFL NOT 0')          
C                                                                               
C3------SET LCBNDS EQUAL TO ADDRESS OF FIRST UNUSED SPACE IN X.                 
      LCBNDS=ISUM                                                               
C                                                                               
C4------CALCULATE AMOUNT OF SPACE USED BY THE GENERAL HEAD LIST.                
      ISP=5*MXBND                                                               
      ISUM=ISUM+ISP                                                             
C                                                                               
C5------PRINT AMOUNT OF SPACE USED BY THE GHB PACKAGE                           
      WRITE(IOUT,4) ISP                                                         
    4 FORMAT(1X,I6,' ELEMENTS IN X ARRAY ARE USED FOR HEAD',                    
     1      '-DEPENDENT BOUNDARIES')                                            
      ISUM1=ISUM-1                                                              
      WRITE(IOUT,5) ISUM1,LENX                                                  
    5 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED OUT OF',I7)                       
      IF(ISUM1.GT.LENX) WRITE(IOUT,6)                                           
    6 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')                  
C                                                                               
C6------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE GHB1RP(BNDS,NBOUND,MXBND,IN,IOUT)                              
C                                                                               
C                                                                               
C-----VERSION 1651 02FEB1983 GHB1RP                                             
C     ******************************************************************        
C     READ DATA FOR GHB                                                         
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
      DIMENSION BNDS(5,MXBND)                                                   
C     ------------------------------------------------------------------        
C                                                                               
C1------READ ITMP(# OF GENERAL HEAD BOUNDS OR FLAG TO REUSE DATA.)              
      READ(IN,8) ITMP                                                           
    8 FORMAT(I10)                                                               
C                                                                               
C2------TEST ITMP                                                               
      IF(ITMP.GE.0) GO TO 50                                                    
C                                                                               
C2A-----IF ITMP<0 THEN REUSE DATA FROM LAST STRESS PERIOD                       
      WRITE(IOUT,7)                                                             
    7 FORMAT(1H0,'REUSING HEAD-DEPENDENT BOUNDS FROM LAST STRESS',              
     1      ' PERIOD')                                                          
      GO TO 260                                                                 
C                                                                               
C3------IF ITMP=>0 THEN IT IS THE # OF GENERAL HEAD BOUNDS.                     
   50 NBOUND=ITMP                                                               
C                                                                               
C4------IF MAX NUMBER OF BOUNDS IS EXCEEDED THEN STOP                           
      IF(NBOUND.LE.MXBND) GO TO 100                                             
      WRITE(IOUT,99) NBOUND,MXBND                                               
   99 FORMAT(1H0,'NBOUND(',I4,') IS GREATER THAN MXBND(',I4,')')                
C                                                                               
C4A-----ABNORMAL STOP                                                           
      STOP                                                                      
C                                                                               
C5------PRINT # OF GENERAL HEAD BOUNDS THIS STRESS PERIOD                       
  100 WRITE(IOUT,1) NBOUND                                                      
    1 FORMAT(1H0,//1X,I5,' HEAD-DEPENDENT BOUNDARY NODES')                      
C                                                                               
C6------IF THERE ARE NO GENERAL HEAD BOUNDS THEN RETURN.                        
      IF(NBOUND.EQ.0) GO TO 260                                                 
C                                                                               
C7------READ & PRINT DATA FOR EACH GENERAL HEAD BOUNDARY.                       
      WRITE(IOUT,3)                                                             
    3 FORMAT(1H0,15X,'LAYER',5X,'ROW',5X                                        
     1,'COL   ELEVATION   CONDUCTANCE   BOUND NO.'/1X,15X,60('-'))              
      DO 250 II=1,NBOUND                                                        
      READ (IN,4) K,I,J,BNDS(4,II),BNDS(5,II)                                   
    4 FORMAT(3I10,2F10.0)                                                       
      WRITE (IOUT,5) K,I,J,BNDS(4,II),BNDS(5,II),II                             
    5 FORMAT(1X,15X,I4,I9,I8,G13.4,G14.4,I8)                                    
      BNDS(1,II)=K                                                              
      BNDS(2,II)=I                                                              
      BNDS(3,II)=J                                                              
  250 CONTINUE                                                                  
C                                                                               
C8------RETURN                                                                  
  260 RETURN                                                                    
      END                                                                       
      SUBROUTINE GHB1FM(NBOUND,MXBND,BNDS,HCOF,RHS,IBOUND,                      
     1              NCOL,NROW,NLAY)                                        
C                                                                               
C-----VERSION 1605 02FEB1983 GHB1FM                                             
C     ******************************************************************        
C     ADD GHB TERMS TO RHS AND HCOF                                             
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
      DIMENSION BNDS(5,MXBND),HCOF(NCOL,NROW,NLAY),                             
     1         RHS(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)                       
C     ------------------------------------------------------------------        
C                                                                               
C1------IF NBOUND<=0 THEN THERE ARE NO GENERAL HEAD BOUNDS. RETURN.             
      IF(NBOUND.LE.0) RETURN                                                    
C                                                                               
C2------PROCESS EACH ENTRY IN THE GENERAL HEAD BOUND LIST (BNDS)                
      DO 100 L=1,NBOUND                                                         
C                                                                               
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY                   
      IL=BNDS(1,L)                                                              
      IR=BNDS(2,L)                                                              
      IC=BNDS(3,L)                                                              
C                                                                               
C4------IF THE CELL IS EXTERNAL THEN SKIP IT.                                   
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100                                       
C                                                                               
C5------SINCE THE CELL IS INTERNAL GET THE BOUNDARY DATA.                       
      HB=BNDS(4,L)                                                              
      C=BNDS(5,L)                                                               
C                                                                               
C6------ADD TERMS TO RHS AND HCOF                                               
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C                                           
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*HB                                          
  100 CONTINUE                                                                  
C                                                                               
C7------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE GHB1BD(NBOUND,MXBND,VBNM,VBVL,MSUM,BNDS,DELT,HNEW,             
     1   NCOL,NROW,NLAY,IBOUND,KSTP,KPER,IGHBCB,ICBCFL,BUFF,IOUT)               
C                                                                               
C-----VERSION 1152 20MAY1983 GHB1BD                                             
C     ******************************************************************        
C     CALCULATE VOLUMETRIC BUDGET FOR GHB                                       
C     ******************************************************************        
C                                                                               
C     SPECIFICATIONS:                                                           
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW                                                     
      DIMENSION VBNM(4,MSUM),VBVL(4,MSUM),BNDS(5,MXBND),                        
     1           HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),                   
     2           BUFF(NCOL,NROW,NLAY)                                           
      DIMENSION TEXT(4)                                                         
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /' HEA','D DE','P BO','UNDS'/        
C     ------------------------------------------------------------------        
C                                                                               
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND                        
C1------ACCUMULATORS (RATIN AND RATOUT)                                         
      IBD=0                                                                     
      RATOUT=0.                                                                 
      RATIN=0.                                                                  
C                                                                               
C2------IF NO BOUNDARIES THEN KEEP ZEROES IN ACCUMULATORS.                      
      IF(NBOUND.EQ.0) GO TO 200                                                 
C                                                                               
C3------TEST TO SEE IF CELL-BY-CELL FLOW TERMS ARE NEEDED.                      
      IF(ICBCFL.EQ.0 .OR. IGHBCB.LE.0) GO TO 10                                 
C                                                                               
C3A-----SINCE CELL-BY-CELL FLOW TERMS ARE NEEDED CLEAR BUFFER & SET             
C3A-----THE FLAG IBD.                                                           
      IBD=1                                                                     
      DO 5 IL=1,NLAY                                                            
      DO 5 IR=1,NROW                                                            
      DO 5 IC=1,NCOL                                                            
      BUFF(IC,IR,IL)=0.                                                         
    5 CONTINUE                                                                  
C                                                                               
C4------FOR EACH GENERAL HEAD BOUND ACCUMULATE FLOW INTO AQUIFER                
   10 DO 100 L=1,NBOUND                                                         
C                                                                               
C5------GET LAYER, ROW AND COLUMN OF EACH GENERAL HEAD BOUNDARY.                
      IL=BNDS(1,L)                                                              
      IR=BNDS(2,L)                                                              
      IC=BNDS(3,L)                                                              
C                                                                               
C6------IF CELL IS EXTERNAL THEN IGNORE IT.                                     
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100                                       
C                                                                               
C7------GET PARAMETERS FROM BOUNDARY LIST.                                      
      HHNEW=HNEW(IC,IR,IL)                                                      
      HB=BNDS(4,L)                                                              
      C=BNDS(5,L)                                                               
C                                                                               
C8------CALCULATE THE FOW RATE INTO THE CELL                                    
      RATE=C*(HB-HHNEW)                                                         
C                                                                               
C9------PRINT THE INDIVIDUAL RATES IF REQUESTED(IGHBCB<0).                      
      IF(IGHBCB.LT.0.AND.ICBCFL.NE.0) WRITE(IOUT,900) (TEXT(N),N=1,4),          
     1    KPER,KSTP,L,IL,IR,IC,RATE                                             
  900 FORMAT(1H0,4A4,'   PERIOD',I3,'   STEP',I3,'   BOUNDARY',I4,              
     1    '   LAYER',I3,'   ROW',I4,'   COL',I4,'   RATE',G15.7)                
C                                                                               
C10-----IF CELL-BY-CELL TERMS ARE TO BE SAVED THEN PUT RATE IN BUFFER           
      IF(IBD.EQ.1) BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+RATE                           
C                                                                               
C11-----SEE IF FLOW IS INTO AQUIFER OR OUT OF AQUIFER.                          
      IF(RATE)94,100,96                                                         
C                                                                               
C12------FLOW IS OUT OF AQUIFER SUBTRACT RATE FROM RATOUT                       
   94 RATOUT=RATOUT-RATE                                                        
      GO TO 100                                                                 
C                                                                               
C13-----FLOW IS INTO AQIFER ADD RATE TO RATIN                                   
   96 RATIN=RATIN+RATE                                                          
  100 CONTINUE                                                                  
C                                                                               
C14-----IF CELL-BY-CELL TERMS ARE TO BE SAVED THEN CALL                         
C14-----UTILITY MODULE UBUDSV                                                   
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IGHBCB,BUFF,NCOL,NROW,            
     1                          NLAY,IOUT)                                      
C                                                                               
C15-----MOVE RATES, VOLUMES AND LABELS INTO ARRAYS FOR PRINTING                 
  200 VBVL(3,MSUM)=RATIN                                                        
      VBVL(1,MSUM)=VBVL(1,MSUM)+RATIN*DELT                                      
      VBVL(4,MSUM)=RATOUT                                                       
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT                                     
      VBNM(1,MSUM)=TEXT(1)                                                      
      VBNM(2,MSUM)=TEXT(2)                                                      
      VBNM(3,MSUM)=TEXT(3)                                                      
      VBNM(4,MSUM)=TEXT(4)                                                      
C                                                                               
C16-----INCREMENT THE BUDGET TERM COUNTER                                       
      MSUM=MSUM+1                                                               
C                                                                               
C17-----RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SIP1AL(ISUM,LENX,LCEL,LCFL,LCGL,LCV,LCHDCG,LCLRCH,             
     1       LCW,MXITER,NPARM,NCOL,NROW,NLAY,IN,IOUT)                           
C                                                                               
C-----VERSION 1001 08DEC1983 SIP1AL                                             
C                                                                               
C     ******************************************************************        
C     ALLOCATE STORAGE IN THE X ARRAY FOR SIP ARRAYS                            
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
C                                                                               
C1------PRINT A MESSAGE IDENTIFYING SIP PACKAGE                                 
      WRITE(IOUT,1)IN                                                           
    1 FORMAT(1H0,'SIP1 -- STRONGLY IMPLICIT PROCEDURE SOLUTION PACKAGE'         
     1,', VERSION 1, 12/08/83',' INPUT READ FROM UNIT',I3)                      
C                                                                               
C2------READ AND PRINT MXITER AND NPARM                                         
      READ(IN,2) MXITER,NPARM                                                   
    2 FORMAT(2I10)                                                              
      WRITE(IOUT,3) MXITER,NPARM                                                
    3 FORMAT(1X,'MAXIMUM OF',I4,' ITERATIONS ALLOWED FOR CLOSURE'/              
     1       1X,I2,' ITERATION PARAMETERS')                                     
C                                                                               
C3------ALLOCATE SPACE FOR THE SIP ARRAYS                                       
      ISOLD=ISUM                                                                
      NRC=NROW*NCOL                                                             
      ISIZ=NRC*NLAY                                                             
      LCEL=ISUM                                                                 
      ISUM=ISUM+ISIZ                                                            
      LCFL=ISUM                                                                 
      ISUM=ISUM+ISIZ                                                            
      LCGL=ISUM                                                                 
      ISUM=ISUM+ISIZ                                                            
      LCV=ISUM                                                                  
      ISUM=ISUM+ISIZ                                                            
      LCHDCG=ISUM                                                               
      ISUM=ISUM+MXITER                                                          
      LCLRCH=ISUM                                                               
      ISUM=ISUM+3*MXITER                                                        
      LCW=ISUM                                                                  
      ISUM=ISUM+NPARM                                                           
C                                                                               
C4------CALCULATE AND PRINT THE SPACE USED IN THE X ARRAY                       
      ISP=ISUM-ISOLD                                                            
      WRITE(IOUT,4) ISP                                                         
    4 FORMAT(1X,I6,' ELEMENTS IN X ARRAY ARE USED BY SIP')                      
      ISUM1=ISUM-1                                                              
      WRITE(IOUT,5) ISUM1,LENX                                                  
    5 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED OUT OF',I7)                       
      IF(ISUM1.GT.LENX) WRITE(IOUT,6)                                           
    6 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')                  
C                                                                               
C5------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SIP1RP(NPARM,MXITER,ACCL,HCLOSE,W,IN,IPCALC,IPRSIP,            
     1                IOUT)                                                     
C                                                                               
C-----VERSION 0925 16DEC1982 SIP1RP                                             
C                                                                               
C     ******************************************************************        
C     READ DATA FOR SIP                                                         
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION W(NPARM)                                                        
C     ------------------------------------------------------------------        
C                                                                               
C1------READ ACCL,HCLOSE,WSEED,IPCALC,IPRSIP                                    
      READ(IN,1) ACCL,HCLOSE,IPCALC,WSEED,IPRSIP                                
    1 FORMAT(2F10.0,I10,F10.0,I10)                                              
      IF(ACCL.EQ.0.) ACCL=1.                                                    
C                                                                               
C2------PRINT DATA VALUES JUST READ                                             
      WRITE(IOUT,100)                                                           
  100 FORMAT(1H0,///57X,'SOLUTION BY THE STRONGLY IMPLICIT PROCEDURE'           
     1/57X,43('-'))                                                             
      WRITE(IOUT,115) MXITER                                                    
  115 FORMAT(1H0,47X,'MAXIMUM ITERATIONS ALLOWED FOR CLOSURE =',I9)             
      WRITE(IOUT,120) ACCL                                                      
  120 FORMAT(1H ,63X,'ACCELERATION PARAMETER =',G15.5)                          
      WRITE(IOUT,125) HCLOSE                                                    
  125 FORMAT(1H ,52X,'HEAD CHANGE CRITERION FOR CLOSURE =',E15.5)               
      IF(IPRSIP.LE.0)IPRSIP=999                                                 
      WRITE(IOUT,130) IPRSIP                                                    
  130 FORMAT(1H ,52X,'SIP HEAD CHANGE PRINTOUT INTERVAL =',I9)                  
C                                                                               
C3------CHECK IF SPECIFIED VALUE OF WSEED SHOULD BE USED OR IF                  
C3------SEED SHOULD BE CALCULATED                                               
      IF(IPCALC.EQ.0) GO TO 150                                                 
C                                                                               
C3A-----CALCULATE SEED & ITERATION PARAMETERS PRIOR TO 1ST ITERATION            
      WRITE(IOUT,140)                                                           
  140 FORMAT(1H0,52X,'CALCULATE ITERATION PARAMETERS FROM MODEL',               
     1' CALCULATED WSEED')                                                      
      GO TO 1000                                                                
C                                                                               
C3B-----USE SPECIFIED VALUE OF WSEED                                            
C3B-----CALCULATE AND PRINT ITERATION PARAMETERS                                
  150 P1=-1.                                                                    
      P2=NPARM-1                                                                
      DO 160 I=1,NPARM                                                          
      P1=P1+1.                                                                  
  160 W(I)=1.-WSEED**(P1/P2)                                                    
      WRITE(IOUT,161) NPARM,WSEED,(W(J),J=1,NPARM)                              
  161 FORMAT(1H0,/,I5,' ITERATION PARAMETERS CALCULATED FROM',                  
     1     ' SPECIFIED WSEED =',F11.8,' :'//(10X,6E15.7))                       
C                                                                               
C4------RETURN                                                                  
 1000 RETURN                                                                    
      END                                                                       
      SUBROUTINE SIP1AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,EL,FL,GL,V,               
     1      W,HDCG,LRCH,NPARM,KITER,HCLOSE,ACCL,ICNVG,KSTP,KPER,                
     2      IPCALC,IPRSIP,MXITER,NSTP,NCOL,NROW,NLAY,NODES,IOUT)                
C-----VERSION 1454 02DEC1982 SIP1AP                                             
C                                                                               
C     ******************************************************************        
C     SOLUTION BY THE STRONGLY IMPLICIT PROCEDURE -- 1 ITERATION                
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW,DITPAR,AC,HHCOF,RRHS,XI,DZERO,DONE,RES              
      DOUBLE PRECISION Z,B,D,E,F,H,S,AP,TP,CP,GP,UP,RP                          
      DOUBLE PRECISION ZHNEW,BHNEW,DHNEW,FHNEW,HHNEW,SHNEW                      
      DOUBLE PRECISION AL,BL,CL,DL,ELNCL,FLNCL,GLNCL                            
      DOUBLE PRECISION ELNRL,FLNRL,GLNRL,ELNLL,FLNLL,GLNLL                      
      DOUBLE PRECISION VNRL,VNCL,VNLL,ELXI,FLXI,GLXI,VN,HCFHNW                 
C                                                                               
      DIMENSION HNEW(NODES), IBOUND(NODES), CR(NODES), CC(NODES),               
     1  CV(NODES), HCOF(NODES), RHS(NODES), EL(NODES), FL(NODES),               
     2  GL(NODES), V(NODES), W(NPARM), HDCG(MXITER), LRCH(3,MXITER)             
C     ------------------------------------------------------------------        
C                                                                               
C1------CALCULATE ITERATION PARAMETERS IF FLAG IS SET.  THEN                    
C1------CLEAR THE FLAG SO THAT CALCULATION IS DONE ONLY ONCE.                   
      IF(IPCALC.NE.0)                                                           
     1     CALL SSIP1I(CR,CC,CV,IBOUND,NPARM,W,NCOL,NROW,NLAY,IOUT)             
      IPCALC=0                                                                  
C                                                                               
C2------ASSIGN VALUES TO FIELDS THAT ARE CONSTANT DURING AN ITERATION           
      DZERO=0.                                                                  
      DONE=1.                                                                   
      AC=ACCL                                                                   
      NRC=NROW*NCOL                                                             
      NTH=MOD(KITER-1,NPARM)+1                                                  
      DITPAR=W(NTH)                                                             
C                                                                               
C3------INITIALIZE VARIABLE THAT TRACKS MAXIMUM HEAD CHANGE DURING              
C3------THE ITERATION                                                           
      BIGG=0.                                                                   
C                                                                               
C4------CLEAR SIP WORK ARRAYS.                                                  
      DO 100 I=1,NODES                                                          
      EL(I)=0.                                                                  
      FL(I)=0.                                                                  
      GL(I)=0.                                                                  
  100 V(I)=0.                                                                   
C                                                                               
C5------SET NORMAL/REVERSE EQUATION ORDERING FLAG (1 OR -1) AND                 
C5------CALCULATE INDEXES DEPENDENT ON ORDERING                                 
      IDIR=1                                                                    
      IF(MOD(KITER,2).EQ.0)IDIR=-1                                              
      IDNRC=IDIR*NRC                                                            
      IDNCOL=IDIR*NCOL                                                          
C                                                                               
C6------STEP THROUGH CELLS CALCULATING INTERMEDIATE VECTOR V                    
C6------USING FORWARD SUBSTITUTION                                              
      DO 150 K=1,NLAY                                                           
      DO 150 I=1,NROW                                                           
      DO 150 J=1,NCOL                                                           
C                                                                               
C6A-----SET UP CURRENT CELL LOCATION INDEXES.  THESE ARE DEPENDENT              
C6A-----ON THE DIRECTION OF EQUATION ORDERING.                                  
      IF(IDIR.LE.0)GO TO 120                                                    
      II=I                                                                      
      JJ=J                                                                      
      KK=K                                                                      
      GO TO 122                                                                 
  120 II=NROW-I+1                                                               
      JJ=J                                                                      
      KK=NLAY-K+1                                                               
C                                                                               
C6B-----CALCULATE 1 DIMENSIONAL SUBSCRIPT OF CURRENT CELL AND                   
C6B-----SKIP CALCULATIONS IF CELL IS NOFLOW OR CONSTANT HEAD                    
  122 N=JJ+(II-1)*NCOL+(KK-1)*NRC                                               
      IF(IBOUND(N).LE.0)GO TO 150                                               
C                                                                               
C6C-----CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR LOCATING THE 6                   
C6C-----SURROUNDING CELLS                                                       
      NRN=N+IDNCOL                                                              
      NRL=N-IDNCOL                                                              
      NCN=N+1                                                                   
      NCL=N-1                                                                   
      NLN=N+IDNRC                                                               
      NLL=N-IDNRC                                                               
C                                                                               
C6D-----CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR CONDUCTANCE TO THE 6             
C6D-----SURROUNDING CELLS.  THESE DEPEND ON ORDERING OF EQUATIONS.              
      IF(IDIR.LE.0)GO TO 124                                                    
      NCF=N                                                                     
      NCD=NCL                                                                   
      NRB=NRL                                                                   
      NRH=N                                                                     
      NLS=N                                                                     
      NLZ=NLL                                                                   
      GO TO 126                                                                 
  124 NCF=N                                                                     
      NCD=NCL                                                                   
      NRB=N                                                                     
      NRH=NRN                                                                   
      NLS=NLN                                                                   
      NLZ=N                                                                     
C                                                                               
C6E-----ASSIGN VARIABLES IN MATRICES A & U INVOLVING ADJACENT CELLS             
C6E1----NEIGHBOR IS 1 ROW BACK                                                  
  126 B=DZERO                                                                   
      ELNRL=DZERO                                                               
      FLNRL=DZERO                                                               
      GLNRL=DZERO                                                               
      BHNEW=DZERO                                                               
      VNRL=DZERO                                                                
      IF(I.EQ.1) GO TO 128                                                      
      B=CC(NRB)                                                                 
      ELNRL=EL(NRL)                                                             
      FLNRL=FL(NRL)                                                             
      GLNRL=GL(NRL)                                                             
      BHNEW=B*HNEW(NRL)                                                         
      VNRL=V(NRL)                                                               
C                                                                               
C6E2----NEIGHBOR IS 1 ROW AHEAD                                                 
  128 H=DZERO                                                                   
      HHNEW=DZERO                                                               
      IF(I.EQ.NROW) GO TO 130                                                   
      H=CC(NRH)                                                                 
      HHNEW=H*HNEW(NRN)                                                         
C                                                                               
C6E3----NEIGHBOR IS 1 COLUMN BACK                                               
  130 D=DZERO                                                                   
      ELNCL=DZERO                                                               
      FLNCL=DZERO                                                               
      GLNCL=DZERO                                                               
      DHNEW=DZERO                                                               
      VNCL=DZERO                                                                
      IF(J.EQ.1) GO TO 132                                                      
      D=CR(NCD)                                                                 
      ELNCL=EL(NCL)                                                             
      FLNCL=FL(NCL)                                                             
      GLNCL=GL(NCL)                                                             
      DHNEW=D*HNEW(NCL)                                                         
      VNCL=V(NCL)                                                               
C                                                                               
C6E4----NEIGHBOR IS 1 COLUMN AHEAD                                              
  132 F=DZERO                                                                   
      FHNEW=DZERO                                                               
      IF(J.EQ.NCOL) GO TO 134                                                   
      F=CR(NCF)                                                                 
      FHNEW=F*HNEW(NCN)                                                         
C                                                                               
C6E5----NEIGHBOR IS 1 LAYER BEHIND                                              
  134 Z=DZERO                                                                   
      ELNLL=DZERO                                                               
      FLNLL=DZERO                                                               
      GLNLL=DZERO                                                               
      ZHNEW=DZERO                                                               
      VNLL=DZERO                                                                
      IF(K.EQ.1) GO TO 136                                                      
      Z=CV(NLZ)                                                                 
      ELNLL=EL(NLL)                                                             
      FLNLL=FL(NLL)                                                             
      GLNLL=GL(NLL)                                                             
      ZHNEW=Z*HNEW(NLL)                                                         
      VNLL=V(NLL)                                                               
C                                                                               
C6E6----NEIGHBOR IS 1 LAYER AHEAD                                               
  136 S=DZERO                                                                   
      SHNEW=DZERO                                                               
      IF(K.EQ.NLAY) GO TO 138                                                   
      S=CV(NLS)                                                                 
      SHNEW=S*HNEW(NLN)                                                         
C                                                                               
C6E7----CALCULATE THE NEGATIVE SUM OF ALL CONDUCTANCES TO NEIGHBORING           
C6E7----CELLS                                                                   
  138 E=-Z-B-D-F-H-S                                                            
C                                                                               
C6F-----CALCULATE COMPONENTS OF THE UPPER AND LOWER MATRICES, WHICH             
C6F-----ARE THE FACTORS OF MATRIX (A+B)                                         
      AL=Z/(DONE+DITPAR*(ELNLL+FLNLL))                                          
      BL=B/(DONE+DITPAR*(ELNRL+GLNRL))                                          
      CL=D/(DONE+DITPAR*(FLNCL+GLNCL))                                          
      AP=AL*ELNLL                                                               
      CP=BL*ELNRL                                                               
      GP=CL*FLNCL                                                               
      RP=CL*GLNCL                                                               
      TP=AL*FLNLL                                                               
      UP=BL*GLNRL                                                               
      HHCOF=HCOF(N)                                                             
      DL=E+HHCOF+DITPAR*(AP+TP+CP+GP+UP+RP)-AL*GLNLL-BL*FLNRL-CL*ELNCL          
      EL(N)=(F-DITPAR*(AP+CP))/DL                                               
      FL(N)=(H-DITPAR*(TP+GP))/DL                                               
      GL(N)=(S-DITPAR*(RP+UP))/DL                                               
C                                                                               
C6G-----CALCULATE THE RESIDUAL                                                  
      RRHS=RHS(N)                                                               
      HNW=HNEW(N)                                                               
      HCFHNW=HNW*HCOF(N)                                                        
      RES=RRHS-ZHNEW-BHNEW-DHNEW-E*HNEW(N)-HCFHNW-FHNEW-HHNEW-SHNEW             
C                                                                               
C6H-----CALCULATE THE INTERMEDIATE VECTOR V                                     
      V(N)=(AC*RES-AL*VNLL-BL*VNRL-CL*VNCL)/DL                                  
C                                                                               
  150 CONTINUE                                                                  
C                                                                               
C7------STEP THROUGH EACH CELL AND SOLVE FOR HEAD CHANGE BY BACK                
C7------SUBSTITUTION                                                            
      DO 160 K=1,NLAY                                                           
      DO 160 I=1,NROW                                                           
      DO 160 J=1,NCOL                                                           
C                                                                               
C7A-----SET UP CURRENT CELL LOCATION INDEXES.  THESE ARE DEPENDENT              
C7A-----ON THE DIRECTION OF EQUATION ORDERING.                                  
      IF(IDIR.LT.0) GO TO 152                                                   
      KK=NLAY-K+1                                                               
      II=NROW-I+1                                                               
      JJ=NCOL-J+1                                                               
      GO TO 154                                                                 
  152 KK=K                                                                      
      II=I                                                                      
      JJ=NCOL-J+1                                                               
C                                                                               
C7B-----CALCULATE 1 DIMENSIONAL SUBSCRIPT OF CURRENT CELL AND                   
C7B-----SKIP CALCULATIONS IF CELL IS NOFLOW OR CONSTANT HEAD                    
  154 N=JJ+(II-1)*NCOL+(KK-1)*NRC                                               
      IF(IBOUND(N).LE.0)GO TO 160                                               
C                                                                               
C7C-----CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR THE 3 NEIGHBORING CELLS          
C7C-----BEHIND (RELATIVE TO THE DIRECTION OF THE BACK SUBSTITUTION              
C7C-----ORDERING) THE CURRRENT CELL.                                            
      NC=N+1                                                                    
      NR=N+IDNCOL                                                               
      NL=N+IDNRC                                                                
C                                                                               
C7D-----BACK SUBSTITUTE, STORING HEAD CHANGE IN ARRAY V IN PLACE OF             
C7D-----INTERMEDIATE FORWARD SUBSTITUTION VALUES.                               
      ELXI=DZERO                                                                
      FLXI=DZERO                                                                
      GLXI=DZERO                                                                
      IF(JJ.NE.NCOL) ELXI=EL(N)*V(NC)                                           
      IF(I.NE.1) FLXI=FL(N)*V(NR)                                               
      IF(K.NE.1) GLXI=GL(N)*V(NL)                                               
      VN=V(N)                                                                   
      V(N)=VN-ELXI-FLXI-GLXI                                                    
C                                                                               
C7E-----GET THE ABSOLUTE HEAD CHANGE. IF IT IS MAX OVER GRID SO FAR.            
C7E-----THEN SAVE IT ALONG WITH CELL INDICES AND HEAD CHANGE.                   
      TCHK=ABS(V(N))                                                            
      IF (TCHK.LE.BIGG) GO TO 155                                               
      BIGG=TCHK                                                                 
      BIG=V(N)                                                                  
      IB=II                                                                     
      JB=JJ                                                                     
      KB=KK                                                                     
C                                                                               
C7F-----ADD HEAD CHANGE THIS ITERATION TO HEAD FROM THE PREVIOUS                
C7F-----ITERATION TO GET A NEW ESTIMATE OF HEAD.                                
  155 XI=V(N)                                                                   
      HNEW(N)=HNEW(N)+XI                                                        
C                                                                               
  160 CONTINUE                                                                  
C                                                                               
C8------STORE THE LARGEST ABSOLUTE HEAD CHANGE (THIS ITERATION) AND             
C8------AND ITS LOCATION.                                                       
      HDCG(KITER)=BIG                                                           
      LRCH(1,KITER)=KB                                                          
      LRCH(2,KITER)=IB                                                          
      LRCH(3,KITER)=JB                                                          
      ICNVG=0                                                                   
      IF(BIGG.LE.HCLOSE) ICNVG=1                                                
C                                                                               
C9------IF END OF TIME STEP, PRINT # OF ITERATIONS THIS STEP                    
      IF(ICNVG.EQ.0 .AND. KITER.NE.MXITER) GO TO 600                            
      IF(KSTP.EQ.1) WRITE(IOUT,500)                                             
  500 FORMAT(1H0)                                                               
      WRITE(IOUT,501) KITER,KSTP,KPER                                           
  501 FORMAT(1X,I5,' ITERATIONS FOR TIME STEP',I4,' IN STRESS PERIOD',          
     1        I3)                                                               
C                                                                               
C10-----PRINT HEAD CHANGE EACH ITERATION IF PRINTOUT INTERVAL IS REACHED        
      IF(ICNVG.EQ.0 .OR. KSTP.EQ.NSTP .OR. MOD(KSTP,IPRSIP).EQ.0)               
     1      CALL SSIP1P(HDCG,LRCH,KITER,KSTP,KPER,MXITER,IOUT)                  
C                                                                               
C11-----RETURN                                                                  
600   RETURN                                                                    
C                                                                               
      END                                                                       
      SUBROUTINE SSIP1I(CR,CC,CV,IBOUND,NPARM,W,NCOL,NROW,NLAY,                 
     1          IOUT)                                                           
C                                                                               
C-----VERSION 1743 26APR1983 SSIP1I                                             
C     ******************************************************************        
C     CALCULATE AN ITERATION PARAMETER SEED AND USE IT TO CALCULATE SIP         
C     ITERATION PARAMETERS                                                      
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY)                           
     1       ,CV(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),W(NPARM)                
C                                                                               
      DOUBLE PRECISION DWMIN,AVGSUM                                             
C     ------------------------------------------------------------------        
C                                                                               
C1------CALCULATE CONSTANTS AND INITIALIZE VARIABLES                            
      PIEPIE=9.869604                                                           
      R=NROW                                                                    
      C=NCOL                                                                    
      L=NLAY                                                                    
      CCOL=PIEPIE/(2.*C*C)                                                      
      CROW=PIEPIE/(2.*R*R)                                                      
      CLAY=PIEPIE/(2.*L*L)                                                      
      WMINMN=1.                                                                 
      AVGSUM=0.                                                                 
      NODES=0                                                                   
C                                                                               
C2------LOOP THROUGH ALL CELLS, CALCULATING A SEED FOR EACH CELL                
C2------THAT IS ACTIVE                                                          
      DO 100 K=1,NLAY                                                           
      DO 100 I=1,NROW                                                           
      DO 100 J=1,NCOL                                                           
      IF(IBOUND(J,I,K).LE.0) GO TO 100                                          
C                                                                               
C2A-----CONDUCTANCE FROM THIS CELL                                              
C2A-----TO EACH OF THE 6 ADJACENT CELLS                                         
      D=0.                                                                      
      IF(J.NE.1) D=CR(J-1,I,K)                                                  
      F=0.                                                                      
      IF(J.NE.NCOL) F=CR(J,I,K)                                                 
      B=0.                                                                      
      IF(I.NE.1) B=CC(J,I-1,K)                                                  
      H=0.                                                                      
      IF(I.NE.NROW) H=CC(J,I,K)                                                 
      Z=0.                                                                      
      IF(K.NE.1) Z=CV(J,I,K-1)                                                  
      S=0.                                                                      
      IF(K.NE.NLAY) S=CV(J,I,K)                                                 
C                                                                               
C2B-----FIND THE MAXIMUM AND MINIMUM OF THE 2 CONDUCTANCE COEFFICIENTS          
C2B-----IN EACH PRINCIPAL COORDINATE DIRECTION                                  
      DFMX=AMAX1(D,F)                                                           
      BHMX=AMAX1(B,H)                                                           
      ZSMX=AMAX1(Z,S)                                                           
      DFMN=AMIN1(D,F)                                                           
      BHMN=AMIN1(B,H)                                                           
      ZSMN=AMIN1(Z,S)                                                           
      IF(DFMN.EQ.0.) DFMN=DFMX                                                  
      IF(BHMN.EQ.0.) BHMN=BHMX                                                  
      IF(ZSMN.EQ.0.) ZSMN=ZSMX                                                  
C                                                                               
C2C-----CALCULATE A SEED IN EACH PRINCIPAL COORDINATE DIRECTION                 
      WCOL=1.                                                                   
      IF(DFMN.NE.0.) WCOL=CCOL/(1.+(BHMX+ZSMX)/DFMN)                            
      WROW=1.                                                                   
      IF(BHMN.NE.0.) WROW=CROW/(1.+(DFMX+ZSMX)/BHMN)                            
      WLAY=1.                                                                   
      IF(ZSMN.NE.0.) WLAY=CLAY/(1.+(DFMX+BHMX)/ZSMN)                            
C                                                                               
C2D-----SELECT THE CELL SEED, WHICH IS THE MINIMUM SEED OF THE 3.               
C2D-----SELECT THE MINIMUM SEED OVER THE WHOLE GRID.                            
      WMIN=AMIN1(WCOL,WROW,WLAY)                                                
      WMINMN=AMIN1(WMINMN,WMIN)                                                 
C                                                                               
C2E-----ADD THE CELL SEED TO THE ACCUMULATOR AVGSUM FOR USE                     
C2E-----IN GETTING THE AVERAGE SEED.                                            
      DWMIN=WMIN                                                                
      AVGSUM=AVGSUM+DWMIN                                                       
      NODES=NODES+1                                                             
C                                                                               
  100 CONTINUE                                                                  
C                                                                               
C3------CALCULATE THE AVERAGE SEED OF THE CELL SEEDS, AND PRINT                 
C3------THE AVERAGE AND MINIMUM SEEDS.                                          
      TMP=NODES                                                                 
      AVGMIN=AVGSUM                                                             
      AVGMIN=AVGMIN/TMP                                                         
      WRITE(IOUT,101) AVGMIN,WMINMN                                             
  101 FORMAT(1H0,'AVERAGE SEED =',F11.8/1X,'MINIMUM SEED =',F11.8)              
C                                                                               
C4------CALCULATE AND PRINT ITERATION PARAMETERS FROM THE AVERAGE SEED          
      P1=-1.                                                                    
      P2=NPARM-1                                                                
      DO 50 I=1,NPARM                                                           
      P1=P1+1.                                                                  
   50 W(I)=1.-AVGMIN**(P1/P2)                                                   
      WRITE(IOUT,150) NPARM,(W(J),J=1,NPARM)                                    
  150 FORMAT(1H0,/,I5,' ITERATION PARAMETERS CALCULATED FROM',                  
     1      ' AVERAGE SEED:'//(10X,6E15.7))                                     
C                                                                               
C5------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SSIP1P(HDCG,LRCH,KITER,KSTP,KPER,MXITER,IOUT)                  
C                                                                               
C                                                                               
C-----VERSION 1504 08DEC1982 SSIP1P                                             
C     ******************************************************************        
C     PRINT MAXIMUM HEAD CHANGE FOR EACH ITERATION DURING A TIME STEP           
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C                                                                               
      DIMENSION HDCG(MXITER), LRCH(3,MXITER)                                    
C     ------------------------------------------------------------------        
C                                                                               
      WRITE(IOUT,5)                                                             
    5 FORMAT(1H0,'MAXIMUM HEAD CHANGE FOR EACH ITERATION:'/                     
     1    1H0,5(' HEAD CHANGE LAYER,ROW,COL')/1X,132('-'))                      
      WRITE (IOUT,10) (HDCG(J),(LRCH(I,J),I=1,3),J=1,KITER)                     
   10 FORMAT((1X,5(G12.4,' (',I3,',',I3,',',I3,')')))                           
      WRITE(IOUT,11)                                                            
   11 FORMAT(1H0)                                                               
C                                                                               
      RETURN                                                                    
C                                                                               
      END                                                                       
      SUBROUTINE SOR1AL(ISUM,LENX,LCA,LCRES,LCHDCG,LCLRCH,LCIEQP,               
     1       MXITER,NCOL,NROW,NLAY,NSLICE,MBW,IN,IOUT)                          
C                                                                               
C-----VERSION 1003 08DEC1983 SOR1AL                                             
C     ******************************************************************        
C     ALLOCATE STORAGE FOR SOR ARRAYS                                           
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
C                                                                               
C1------PRINT A MESSAGE IDENTIFYING SOR PACKAGE                                 
      WRITE(IOUT,1)IN                                                           
    1 FORMAT(1H0,'SOR1 -- SLICE-SUCCESSIVE OVERRELAXATION PACKAGE'              
     1,', VERSION 1, 12/08/83',' INPUT READ FROM UNIT',I3)                      
C                                                                               
C2------READ AND PRINT MXITER (MAXIMUM # OF ITERATIONS)                         
      READ(IN,2) MXITER                                                         
    2 FORMAT(I10)                                                               
      WRITE(IOUT,3) MXITER                                                      
    3 FORMAT(1X,I5,' ITERATIONS ALLOWED FOR SOR CLOSURE')                       
C                                                                               
C3------ALLOCATE SPACE FOR THE SOR ARRAYS                                       
      ISOLD=ISUM                                                                
      NSLICE=NCOL*NLAY                                                          
      MBW=NLAY+1                                                                
      LCA=ISUM                                                                  
      ISUM=ISUM+NSLICE*MBW                                                      
      LCRES=ISUM                                                                
      ISUM=ISUM+NSLICE                                                          
      LCIEQP=ISUM                                                               
      ISUM=ISUM+NSLICE                                                          
      LCHDCG=ISUM                                                               
      ISUM=ISUM+MXITER                                                          
      LCLRCH=ISUM                                                               
      ISUM=ISUM+3*MXITER                                                        
      ISP=ISUM-ISOLD                                                            
C                                                                               
C4------CALCULATE AND PRINT THE SPACE USED IN THE X ARRAY                       
      WRITE(IOUT,4) ISP                                                         
    4 FORMAT(1X,I6,' ELEMENTS IN X ARRAY ARE USED BY SOR')                      
      ISUM1=ISUM-1                                                              
      WRITE(IOUT,5) ISUM1,LENX                                                  
    5 FORMAT(1X,I6,' ELEMENTS OF X ARRAY USED OUT OF',I7)                       
      IF(ISUM1.GT.LENX) WRITE(IOUT,6)                                           
    6 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')                  
C                                                                               
C5------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SOR1RP(MXITER,ACCL,HCLOSE,IN,IPRSOR,IOUT)                      
C                                                                               
C                                                                               
C-----VERSION 1005 16MAR1983 SOR1RP                                             
C     ******************************************************************        
C     READ PARAMETERS FOR SOR                                                   
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
C     ------------------------------------------------------------------        
C                                                                               
C1------READ THE ACCELERATION PARAMETER/RELAXATION FACTOR (ACCL) THE            
C1------CLOSURE CRITERION (HCLOSE) AND THE NUMBER OF TIME STEPS                 
C1------BETWEEN PRINTOUTS OF MAXIMUM HEAD CHANGES (IPRSOR).                     
      READ(IN,1) ACCL,HCLOSE,IPRSOR                                             
    1 FORMAT(2F10.0,I10)                                                        
      IF(ACCL.EQ.0.) ACCL=1.                                                    
      IF(IPRSOR.LT.1) IPRSOR=999                                                
C                                                                               
C2------PRINT ACCL, HCLOSE, IPRSOR                                              
      WRITE(IOUT,100)                                                           
  100 FORMAT(1H0,///57X,'SOLUTION BY SLICE-SUCCESSIVE OVERRELAXATION'           
     1/57X,43('-'))                                                             
      WRITE(IOUT,115) MXITER                                                    
  115 FORMAT(1H0,47X,'MAXIMUM ITERATIONS ALLOWED FOR CLOSURE =',I9)             
      WRITE(IOUT,120) ACCL                                                      
  120 FORMAT(1H ,63X,'ACCELERATION PARAMETER =',G15.5)                          
      WRITE(IOUT,125) HCLOSE                                                    
  125 FORMAT(1H ,52X,'HEAD CHANGE CRITERION FOR CLOSURE =',E15.5)               
      WRITE(IOUT,130) IPRSOR                                                    
  130 FORMAT(1H ,52X,'SOR HEAD CHANGE PRINTOUT INTERVAL =',I9)                  
C                                                                               
C3------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SOR1AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,A,RES,IEQPNT,             
     1      HDCG,LRCH,KITER,HCLOSE,ACCL,ICNVG,KSTP,KPER,                        
     2      IPRSOR,MXITER,NSTP,NCOL,NROW,NLAY,NSLICE,MBW,IOUT)                  
C-----VERSION 0936 09MAY1983 SOR1AP                                             
C     ******************************************************************        
C     SOLUTION BY SLICE-SUCCESSIVE OVERRELAXATION -- 1 ITERATION                
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DOUBLE PRECISION HNEW,DIFF,DP,EE,R,HCFHNW,HHCOF                           
C                                                                               
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),                   
     1   CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),                                
     1   CV(NCOL,NROW,NLAY), HCOF(NCOL,NROW,NLAY), RHS(NCOL,NROW,NLAY),         
     2   HDCG(MXITER), LRCH(3,MXITER),A(MBW,NSLICE),RES(NSLICE),                
     3   IEQPNT(NLAY,NCOL)                                                      
C     ------------------------------------------------------------------        
C                                                                               
C1------CALCULATE # OF ELEMENTS IN COMPRESSED MATRIX A AND                      
C1------INITIALIZE FIELDS TO SAVE LARGEST HEAD CHANGE.                          
      NA=MBW*NSLICE                                                             
      BIG=0.                                                                    
      ABSBIG=0.                                                                 
      IB=0                                                                      
      JB=0                                                                      
      KB=0                                                                      
C                                                                               
C2------PROCESS EACH SLICE                                                      
      DO 500 I=1,NROW                                                           
C                                                                               
C3------CLEAR A                                                                 
      DO 110 J=1,NSLICE                                                         
      DO 110 K=1,MBW                                                            
  110 A(K,J)=0.                                                                 
C                                                                               
C4------ASSIGN A SEQUENCE # TO EACH VARIABLE HEAD CELL.                         
      NEQT=0                                                                    
      DO 200 J=1,NCOL                                                           
      DO 200 K=1,NLAY                                                           
      IEQPNT(K,J)=0                                                             
      IF(IBOUND(J,I,K).LE.0) GO TO 200                                          
      NEQT=NEQT+1                                                               
      IEQPNT(K,J)=NEQT                                                          
  200 CONTINUE                                                                  
C                                                                               
C5------FOR EACH CELL LOAD MATRIX A AND VECTOR RES                              
      DO 300 J=1,NCOL                                                           
      DO 300 K=1,NLAY                                                           
C                                                                               
C5A-----IF SEQUENCE # IS 0 (CELL IS EXTERNAL) GO ON TO NEXT CELL                
      NEQ=IEQPNT(K,J)                                                           
      IF(NEQ.EQ.0) GO TO 300                                                    
C                                                                               
C5B-----INITIALIZE ACCUMULATORS EE AND R                                        
      EE=0.                                                                     
      R=RHS(J,I,K)                                                              
C                                                                               
C5C-----IF NODE TO LEFT SUBTRACT TERMS FROM EE AND R                            
      IF(J.EQ.1) GO TO 120                                                      
      DP=CR(J-1,I,K)                                                            
      R=R-DP*HNEW(J-1,I,K)                                                      
      EE=EE-DP                                                                  
C                                                                               
C5D-----IF NODE TO RIGHT SUBTRACT TERMS FROM EE & R, MOVE COND TO A             
  120 IF(J.EQ.NCOL) GO TO 125                                                   
      SP=CR(J,I,K)                                                              
      DP=SP                                                                     
      R=R-DP*HNEW(J+1,I,K)                                                      
      EE=EE-DP                                                                  
      NXT=IEQPNT(K,J+1)                                                         
      IF(NXT.GT.0) A(1+NXT-NEQ,NEQ)=SP                                          
C                                                                               
C5E-----IF NODE TO REAR SUBTRACT TERMS FROM EE AND R                            
  125 IF(I.EQ.1) GO TO 130                                                      
      DP=CC(J,I-1,K)                                                            
      R=R-DP*HNEW(J,I-1,K)                                                      
      EE=EE-DP                                                                  
C                                                                               
C5F-----IF NODE TO FRONT SUBTRACT TERMS FROM EE AND R                           
  130 IF(I.EQ.NROW) GO TO 132                                                   
      DP=CC(J,I,K)                                                              
      R=R-DP*HNEW(J,I+1,K)                                                      
      EE=EE-DP                                                                  
C                                                                               
C5G-----IF NODE ABOVE SUBTRACT TERMS FROM EE AND R                              
  132 IF(K.EQ.1) GO TO 134                                                      
      DP=CV(J,I,K-1)                                                            
      R=R-DP*HNEW(J,I,K-1)                                                      
      EE=EE-DP                                                                  
C                                                                               
C5H-----IF NODE BELOW SUBTRACT TERMS FROM EE & R AND MOVE COND TO A             
  134 IF(K.EQ.NLAY) GO TO 136                                                   
      SP=CV(J,I,K)                                                              
      DP=SP                                                                     
      R=R-DP*HNEW(J,I,K+1)                                                      
      EE=EE-DP                                                                  
      IF(IEQPNT(K+1,J).GT.0) A(2,NEQ)=SP                                        
C                                                                               
C5I-----MOVE EE INTO A, SUBTRACT EE TIMES LAST HEAD FROM R TO GET RES           
  136 HHCOF=HCOF(J,I,K)                                                         
      A(1,NEQ)=EE+HHCOF                                                         
      HNW=HNEW(J,I,K)                                                           
      HCFHNW=HNW*HCOF(J,I,K)                                                    
      RES(NEQ)=R-EE*HNEW(J,I,K)-HCFHNW                                          
  300 CONTINUE                                                                  
C                                                                               
C6------IF NO EQUATIONS GO TO NEXT SLICE, IF ONE EQUATION SOLVE                 
C6------DIRECTLY, IF 2 EQUATIONS CALL SSOR1B TO SOLVE FOR FIRST                 
C6------ESTIMATE OF HEAD CHANGE FOR THIS ITERATION.                             
      IF(NEQT.LT.1) GO TO 500                                                   
      IF(NEQT.EQ.1) RES(1)=RES(1)/A(1,1)                                        
      IF(NEQT.GE.2) CALL SSOR1B(A,RES,NEQT,NA,MBW)                              
C                                                                               
C7------FOR EACH CELL IN SLICE CALCULATE FINAL HEAD CHANGE THEN HEAD.           
      DO 400 J=1,NCOL                                                           
      DO 400 K=1,NLAY                                                           
      NEQ=IEQPNT(K,J)                                                           
      IF(NEQ.EQ.0) GO TO 400                                                    
C                                                                               
C7A-----MULTIPLY FIRST ESTIMATE OF HEAD CHANGE BY RELAX FACTOR TO               
C7A-----GET FINAL ESTIMATE OF HEAD CHANGE FOR THIS ITERATION.                   
      DH=RES(NEQ)*ACCL                                                          
      DIFF=DH                                                                   
C                                                                               
C7B-----ADD FINAL ESTIMATE TO HEAD FROM LAST ITERATION TO GET HEAD              
C7B-----FOR THIS ITERATION.                                                     
      HNEW(J,I,K)=HNEW(J,I,K)+DIFF                                              
C                                                                               
C7C-----SAVE FINAL HEAD CHANGE IF IT IS THE LARGEST                             
      ABSDH=ABS(DH)                                                             
      IF(ABSDH.LE.ABSBIG) GO TO 400                                             
      ABSBIG=ABSDH                                                              
      BIG=DH                                                                    
      IB=I                                                                      
      JB=J                                                                      
      KB=K                                                                      
  400 CONTINUE                                                                  
C                                                                               
C                                                                               
  500 CONTINUE                                                                  
C                                                                               
C8------SAVE LARGEST HEAD CHANGE FOR THIS ITERATION                             
      HDCG(KITER)=BIG                                                           
      LRCH(1,KITER)=KB                                                          
      LRCH(2,KITER)=IB                                                          
      LRCH(3,KITER)=JB                                                          
C                                                                               
C9------IF LARGEST HEAD CHANGE IS SMALLER THAN CLOSURE THEN SET                 
C9------CONVERGE FLAG (ICNVG) EQUAL TO 1.                                       
      ICNVG=0                                                                   
      IF(ABSBIG.LE.HCLOSE) ICNVG=1                                              
C                                                                               
C10-----IF NOT CONVERGED AND NOT EXCEDED ITERATIONS THEN RETURN                 
      IF(ICNVG.EQ.0 .AND. KITER.NE.MXITER) RETURN                               
      IF(KSTP.EQ.1) WRITE(IOUT,600)                                             
  600 FORMAT(1H0)                                                               
C                                                                               
C11-----PRINT NUMBER OF ITERATIONS                                              
      WRITE(IOUT,601) KITER,KSTP,KPER                                           
  601 FORMAT(1X,I5,' ITERATIONS FOR TIME STEP',I4,' IN STRESS PERIOD',          
     1        I3)                                                               
C                                                                               
C12-----IF FAILED TO CONVERGE OR LAST TIME STEP OR PRINTOUT                     
C12-----INTERVAL SPECIFIED BY USER IS HERE THEN PRINT MAXIMUM                   
C12-----HEAD CHANGES FOR EACH ITERATION.                                        
      IF(ICNVG.NE.0 .AND. KSTP.NE.NSTP .AND. MOD(KSTP,IPRSOR).NE.0)             
     1      GO TO 700                                                           
      WRITE(IOUT,5)                                                             
    5 FORMAT(1H0,'MAXIMUM HEAD CHANGE FOR EACH ITERATION:'/                     
     1    1H0,4('    HEAD CHANGE  LAYER,ROW,COL')/1X,120('-'))                  
      WRITE (IOUT,10) (HDCG(J),(LRCH(I,J),I=1,3),J=1,KITER)                     
   10 FORMAT((1X,4(4X,G12.4,' (',I3,',',I3,',',I3,')')))                        
      WRITE(IOUT,11)                                                            
   11 FORMAT(1H0)                                                               
C                                                                               
C13-----RETURN                                                                  
  700 RETURN                                                                    
C                                                                               
      END                                                                       
      SUBROUTINE SSOR1B(A,B,N,NA,MBW)                                           
C                                                                               
C                                                                               
C-----VERSION 1359 31MAR1983 SSOR1B                                             
C     ******************************************************************        
C     SOLVE A SYMMETRIC SET OF EQUATIONS                                        
C        A IS COEFFICIENT MATRIX IN COMPRESSED FORM                             
C        B IS RIGHT HAND SIDE AND IS REPLACED BY SOLUTION                       
C        N IS NUMBER OF EQUATIONS TO BE SOLVED                                  
C        MBW IS BANDWIDTH OF A                                                  
C        NA IS ONE-DIMENSION SIZE OF A                                          
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION A(NA),B(N)                                                      
C     ------------------------------------------------------------------        
C                                                                               
      NM1=N-1                                                                   
      MBW1=MBW-1                                                                
      ID=1-MBW                                                                  
C                                                                               
C1------SEQUENTIALLY USE EACH OF THE FIRST N-1 ROWS AS                          
C1------THE PIVOT ROW.                                                          
      DO 20 I=1,NM1                                                             
C                                                                               
C2------CALCULATE THE INVERSE OF THE PIVOT.                                     
      ID=ID+MBW                                                                 
      C1=1./A(ID)                                                               
      LD=ID                                                                     
      L=I                                                                       
C                                                                               
C3------FOR EACH ROW AFTER THE PIVOT ROW (THE TARGET ROW)                       
C3------ELIMINATE THE COLUMN CORRESPONDING TO THE PIVOT.                        
      DO 15 J=1,MBW1                                                            
      L=L+1                                                                     
      IF(L.GT.N) GO TO 20                                                       
      IB=ID+J                                                                   
C                                                                               
C4------CALCULATE THE FACTOR NEEDED TO ELIMINATE A TERM IN THE                  
C4------TARGET ROW.                                                             
      C=A(IB)*C1                                                                
      LD=LD+MBW                                                                 
      LB=LD-1                                                                   
C                                                                               
C5------MODIFY THE REST OF THE TERMS IN THE TARGET ROW.                         
      DO 10 K=J,MBW1                                                            
C                                                                               
C6------SUBTRACT THE FACTOR TIMES A TERM IN THE PIVOT ROW                       
C6------FROM THE CORRESPONDING COLUMN IN THE TARGET ROW.                        
      LB=LB+1                                                                   
      A(LB)=A(LB)-C*A(ID+K)                                                     
   10 CONTINUE                                                                  
C                                                                               
C7------MODIFY THE RIGHT SIDE OF THE EQUATION CORRESPONDING                     
C7------TO THE TARGET ROW.                                                      
      B(I+J)=B(I+J)-C*B(I)                                                      
   15 CONTINUE                                                                  
   20 CONTINUE                                                                  
      ID=ID+MBW                                                                 
C                                                                               
C8------SOLVE THE LAST EQUATION.                                                
      B(N)=B(N)/A(ID)                                                           
C                                                                               
C9------WORKING BACKWARDS SOLVE THE REST OF THE EQUATIONS.                      
      DO 70 I=1,NM1                                                             
      ID=ID-MBW                                                                 
C                                                                               
C10-----CLEAR THE ACCUMULATOR SUM.                                              
      SUM=0.0                                                                   
      L=N-I                                                                     
      MBW1M=MIN0(MBW1,I)                                                        
C                                                                               
C11-----ADD THE KNOWN TERMS IN EQUATION L TO SUM.                               
      DO 60 J=1,MBW1M                                                           
      SUM=SUM+A(ID+J)*B(L+J)                                                    
   60 CONTINUE                                                                  
C                                                                               
C12-----SOLVE FOR THE ONE UNKNOWN IN EQUATION L.                                
      B(L)=(B(L)-SUM)/A(ID)                                                     
   70 CONTINUE                                                                  
C                                                                               
C13-----RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE UBUDSV(KSTP,KPER,TEXT,IBDCHN,BUFF,NCOL,NROW,NLAY,IOUT)         
C                                                                               
C                                                                               
C-----VERSION 1305 28DEC1983 UBUDSV                                             
C     ******************************************************************        
C     RECORD CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT OF FLOW.                 
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION TEXT(4),BUFF(NCOL,NROW,NLAY)                                    
C     ------------------------------------------------------------------        
C                                                                               
C1------WRITE AN UNFORMATTED RECORD CONTAINING IDENTIFYING                      
C1------INFORMATION.                                                            
      WRITE(IOUT,1) TEXT,IBDCHN,KSTP,KPER                                       
    1 FORMAT(1X,'"',4A4,'" BUDGET VALUES WILL BE SAVED ON UNIT',I3,             
     1     ' AT END OF TIME STEP',I3,', STRESS PERIOD',I3)                      
C                                                                               
      WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,NLAY                               
C                                                                               
C2------WRITE AN UNFORMATTED RECORD CONTAINING VALUES FOR                       
C2------EACH CELL IN THE GRID. THE ARRAY IS DIMENSIONED                         
C2------(NCOL,NROW,NLAY)                                                        
      WRITE(IBDCHN) BUFF                                                        
C                                                                               
C3------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE UCOLNO(NLBL1,NLBL2,NSPACE,NCPL,NDIG,IOUT)                      
C                                                                               
C                                                                               
C-----VERSION 1446 20APR1983 UCOLNO                                             
C     ******************************************************************        
C     OUTPUT COLUMN NUMBERS ABOVE A MATRIX PRINTOUT                             
C        NLBL1 IS THE START COLUMN LABEL (NUMBER)                               
C        NLBL2 IS THE STOP COLUMN LABEL (NUMBER)                                
C        NSPACE IS NUMBER OF BLANK SPACES TO LEAVE AT START OF LINE             
C        NCPL IS NUMBER OF COLUMN NUMBERS PER LINE                              
C        NDIG IS NUMBER OF CHARACTERS IN EACH COLUMN FIELD                      
C        IOUT IS OUTPUT CHANNEL                                                 
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION BF(130),DG(10)                                                  
C                                                                               
      DATA DG(1),DG(2),DG(3),DG(4),DG(5),DG(6),DG(7),DG(8),DG(9),DG(10)/        
     1         4H0   ,4H1   ,4H2   ,4H3   ,4H4   ,4H5   ,4H6   ,                
     2         4H7   ,4H8   ,4H9   /                                            
      DATA DOT,SPACE/4H.   ,4H    /                                             
C     ------------------------------------------------------------------        
C                                                                               
C1------CALCULATE # OF COLUMNS TO BE PRINTED (NLBL), WIDTH                      
C1------OF A LINE (NTOT), NUMBER OF LINES (NWRAP).                              
      WRITE(IOUT,1)                                                             
    1 FORMAT(1X)                                                                
      NLBL=NLBL2-NLBL1+1                                                        
      N=NLBL                                                                    
      IF(NLBL.GT.NCPL) N=NCPL                                                   
      NTOT=NSPACE+N*NDIG                                                        
      IF(NTOT.GT.130) GO TO 50                                                  
      NWRAP=(NLBL-1)/NCPL + 1                                                   
      J1=NLBL1-NCPL                                                             
      J2=NLBL1-1                                                                
C                                                                               
C2------BUILD AND PRINT EACH LINE                                               
      DO 40 N=1,NWRAP                                                           
C                                                                               
C3------CLEAR THE BUFFER (BF).                                                  
      DO 20 I=1,130                                                             
      BF(I)=SPACE                                                               
   20 CONTINUE                                                                  
      NBF=NSPACE                                                                
C                                                                               
C4------DETERMINE FIRST (J1) AND LAST (J2) COLUMN # FOR THIS LINE.              
      J1=J1+NCPL                                                                
      J2=J2+NCPL                                                                
      IF(J2.GT.NLBL2) J2=NLBL2                                                  
C5------LOAD THE COLUMN #'S INTO THE BUFFER.                                    
      DO 30 J=J1,J2                                                             
      NBF=NBF+NDIG                                                              
      I2=J/10                                                                   
      I1=J-I2*10+1                                                              
      BF(NBF)=DG(I1)                                                            
      IF(I2.EQ.0) GO TO 30                                                      
      I3=I2/10                                                                  
      I2=I2-I3*10+1                                                             
      BF(NBF-1)=DG(I2)                                                          
      IF(I3.EQ.0) GO TO 30                                                      
      BF(NBF-2)=DG(I3+1)                                                        
   30 CONTINUE                                                                  
C                                                                               
C6------PRINT THE CONTENTS OF THE BUFFER (I.E. PRINT THE LINE).                 
      WRITE(IOUT,31) (BF(I),I=1,NBF)                                            
   31 FORMAT(1X,130A1)                                                          
C                                                                               
   40 CONTINUE                                                                  
C                                                                               
C7------PRINT A LINE OF DOTS (FOR ESTHETIC PURPOSES ONLY).                      
   50 NTOT=NTOT+5                                                               
      IF(NTOT.GT.130) NTOT=130                                                  
      WRITE(IOUT,51) (DOT,I=1,NTOT)                                             
   51 FORMAT(1X,130A1)                                                          
C                                                                               
C8------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE ULAPRS(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,IOUT)            
C                                                                               
C                                                                               
C-----VERSION 1448 20APR1983 ULAPRS                                             
C     ******************************************************************        
C     PRINT A 1 LAYER ARRAY IN STRIPS                                           
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION BUF(NCOL,NROW),TEXT(4)                                          
C     ------------------------------------------------------------------        
C                                                                               
C1------MAKE SURE THE FORMAT CODE (IP OR IPRN) IS BETWEEN 1                     
C1------AND 12.                                                                 
      IP=IPRN                                                                   
      IF(IP.LT.1 .OR. IP.GT.12) IP=12                                           
C                                                                               
C2------DETERMINE THE NUMBER OF VALUES (NCAP) PRINTED ON ONE LINE.              
      IF(IP.EQ.1) NCAP=11                                                       
      IF(IP.EQ.2) NCAP=9                                                        
      IF(IP.GT.2 .AND. IP.LT.7) NCAP=15                                         
      IF(IP.GT.6 .AND. IP.LT.12) NCAP=20                                        
      IF(IP.EQ.12) NCAP=10                                                      
C                                                                               
C3------CALCULATE THE NUMBER OF STRIPS (NSTRIP).                                
      NCPF=129/NCAP                                                             
      ISP=0                                                                     
      IF(NCAP.GT.12) ISP=3                                                      
      NSTRIP=(NCOL-1)/NCAP + 1                                                  
      J1=1-NCAP                                                                 
      J2=0                                                                      
C                                                                               
C4------LOOP THROUGH THE STRIPS.                                                
      DO 2000 N=1,NSTRIP                                                        
C                                                                               
C5------CALCULATE THE FIRST(J1) & THE LAST(J2) COLUMNS FOR THIS STRIP           
      J1=J1+NCAP                                                                
      J2=J2+NCAP                                                                
      IF(J2.GT.NCOL) J2=NCOL                                                    
C                                                                               
C6-------PRINT TITLE ON EACH STRIP                                              
      WRITE(IOUT,1) TEXT,ILAY,KSTP,KPER                                         
    1 FORMAT(1H1,10X,4A4,' IN LAYER',I3,' AT END OF TIME STEP',I3,              
     1     ' IN STRESS PERIOD',I3/11X,71('-'))                                  
C                                                                               
C7------PRINT COLUMN NUMBERS ABOVE THE STRIP                                    
      CALL UCOLNO(J1,J2,ISP,NCAP,NCPF,IOUT)                                     
C                                                                               
C8------LOOP THROUGH THE ROWS PRINTING COLS J1 THRU J2 WITH FORMAT IP           
      DO 1000 I=1,NROW                                                          
      GO TO(10,20,30,40,50,60,70,80,90,100,110,120), IP                         
C                                                                               
C------------FORMAT 10G10.3                                                     
   10 WRITE(IOUT,11) I,(BUF(J,I),J=J1,J2)                                       
   11 FORMAT(1H0,I3,2X,1PG10.3,10(1X,G10.3))                                    
      GO TO 1000                                                                
C                                                                               
C------------FORMAT 8G13.6                                                      
   20 WRITE(IOUT,21) I,(BUF(J,I),J=J1,J2)                                       
   21 FORMAT(1H0,I3,2X,1PG13.6,8(1X,G13.6))                                     
      GO TO 1000                                                                
C                                                                               
C------------FORMAT 15F7.1                                                      
   30 WRITE(IOUT,31) I,(BUF(J,I),J=J1,J2)                                       
   31 FORMAT(1H0,I3,1X,15(1X,F7.1))                                             
      GO TO 1000                                                                
C                                                                               
C------------FORMAT 15F7.2                                                      
   40 WRITE(IOUT,41) I,(BUF(J,I),J=J1,J2)                                       
   41 FORMAT(1H0,I3,1X,15(1X,F7.2))                                             
      GO TO 1000                                                                
C                                                                               
C------------FORMAT 15F7.3                                                      
   50 WRITE(IOUT,51) I,(BUF(J,I),J=J1,J2)                                       
   51 FORMAT(1H0,I3,1X,15(1X,F7.3))                                             
      GO TO 1000                                                                
C                                                                               
C------------FORMAT 15F7.4                                                      
   60 WRITE(IOUT,61) I,(BUF(J,I),J=J1,J2)                                       
   61 FORMAT(1H0,I3,1X,15(1X,F7.4))                                             
      GO TO 1000                                                                
C                                                                               
C------------FORMAT 20F5.0                                                      
   70 WRITE(IOUT,71) I,(BUF(J,I),J=J1,J2)                                       
   71 FORMAT(1H0,I3,1X,20(1X,F5.0))                                             
      GO TO 1000                                                                
C                                                                               
C------------FORMAT 20F5.1                                                      
   80 WRITE(IOUT,81) I,(BUF(J,I),J=J1,J2)                                       
   81 FORMAT(1H0,I3,1X,20(1X,F5.1))                                             
      GO TO 1000                                                                
C                                                                               
C------------FORMAT 20F5.2                                                      
   90 WRITE(IOUT,91) I,(BUF(J,I),J=J1,J2)                                       
   91 FORMAT(1H0,I3,1X,20(1X,F5.2))                                             
      GO TO 1000                                                                
C                                                                               
C------------FORMAT 20F5.3                                                      
  100 WRITE(IOUT,101) I,(BUF(J,I),J=J1,J2)                                      
  101 FORMAT(1H0,I3,1X,20(1X,F5.3))                                             
      GO TO 1000                                                                
C                                                                               
C------------FORMAT 20F5.4                                                      
  110 WRITE(IOUT,111) I,(BUF(J,I),J=J1,J2)                                      
  111 FORMAT(1H0,I3,1X,20(1X,F5.4))                                             
      GO TO 1000                                                                
C                                                                               
C------------FORMAT 9G11.4                                                      
  120 WRITE(IOUT,121) I,(BUF(J,I),J=J1,J2)                                      
  121 FORMAT(1H0,I3,2X,1PG11.4,9(1X,G11.4))                                     
C                                                                               
 1000 CONTINUE                                                                  
 2000 CONTINUE                                                                  
C                                                                               
C9------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE ULAPRW(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,IOUT)            
C                                                                               
C                                                                               
C-----VERSION 1245 04MAY1983 ULAPRW                                             
C     ******************************************************************        
C     PRINT 1 LAYER ARRAY                                                       
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION BUF(NCOL,NROW),TEXT(4)                                          
C     ------------------------------------------------------------------        
C                                                                               
C1------PRINT A HEADER                                                          
      IF(ILAY.LE.0) GO TO 5                                                     
      WRITE(IOUT,1) TEXT,ILAY,KSTP,KPER                                         
    1 FORMAT(1H1,10X,4A4,' IN LAYER',I3,' AT END OF TIME STEP',I3,              
     1     ' IN STRESS PERIOD',I3/11X,71('-'))                                  
C                                                                               
C2------MAKE SURE THE FORMAT CODE (IP OR IPRN) IS                               
C2------BETWEEN 1 AND 12.                                                       
    5 IP=IPRN                                                                   
      IF(IP.LT.1 .OR. IP.GT.12) IP=12                                           
C                                                                               
C3------CALL THE UTILITY MODULE UCOLNO TO PRINT COLUMN NUMBERS.                 
      IF(IP.EQ.1) CALL UCOLNO(1,NCOL,0,11,11,IOUT)                              
      IF(IP.EQ.2) CALL UCOLNO(1,NCOL,0,9,14,IOUT)                               
      IF(IP.GT.2 .AND. IP.LT.7) CALL UCOLNO(1,NCOL,3,15,8,IOUT)                 
      IF(IP.GT.6 .AND. IP.LT.12) CALL UCOLNO(1,NCOL,3,20,6,IOUT)                
      IF(IP.EQ.12) CALL UCOLNO(1,NCOL,0,10,12,IOUT)                             
C                                                                               
C4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.                
      DO 1000 I=1,NROW                                                          
      GO TO(10,20,30,40,50,60,70,80,90,100,110,120), IP                         
C                                                                               
C------------ FORMAT 11G10.3                                                    
   10 WRITE(IOUT,11) I,(BUF(J,I),J=1,NCOL)                                      
   11 FORMAT(1H0,I3,2X,1PG10.3,10(1X,G10.3)/(5X,11(1X,G10.3)))                  
      GO TO 1000                                                                
C                                                                               
C------------ FORMAT 9G13.6                                                     
   20 WRITE(IOUT,21) I,(BUF(J,I),J=1,NCOL)                                      
   21 FORMAT(1H0,I3,2X,1PG13.6,8(1X,G13.6)/(5X,9(1X,G13.6)))                    
      GO TO 1000                                                                
C                                                                               
C------------ FORMAT 15F7.1                                                     
   30 WRITE(IOUT,31) I,(BUF(J,I),J=1,NCOL)                                      
   31 FORMAT(1H0,I3,1X,15(1X,F7.1)/(5X,15(1X,F7.1)))                            
      GO TO 1000                                                                
C                                                                               
C------------ FORMAT 15F7.2                                                     
   40 WRITE(IOUT,41) I,(BUF(J,I),J=1,NCOL)                                      
   41 FORMAT(1H0,I3,1X,15(1X,F7.2)/(5X,15(1X,F7.2)))                            
      GO TO 1000                                                                
C                                                                               
C------------ FORMAT 15F7.3                                                     
   50 WRITE(IOUT,51) I,(BUF(J,I),J=1,NCOL)                                      
   51 FORMAT(1H0,I3,1X,15(1X,F7.3)/(5X,15(1X,F7.3)))                            
      GO TO 1000                                                                
C                                                                               
C------------ FORMAT 15F7.4                                                     
   60 WRITE(IOUT,61) I,(BUF(J,I),J=1,NCOL)                                      
   61 FORMAT(1H0,I3,1X,15(1X,F7.4)/(5X,15(1X,F7.4)))                            
      GO TO 1000                                                                
C                                                                               
C------------ FORMAT 20F5.0                                                     
   70 WRITE(IOUT,71) I,(BUF(J,I),J=1,NCOL)                                      
   71 FORMAT(1H0,I3,1X,20(1X,F5.0)/(5X,20(1X,F5.0)))                            
      GO TO 1000                                                                
C                                                                               
C------------ FORMAT 20F5.1                                                     
   80 WRITE(IOUT,81) I,(BUF(J,I),J=1,NCOL)                                      
   81 FORMAT(1H0,I3,1X,20(1X,F5.1)/(5X,20(1X,F5.1)))                            
      GO TO 1000                                                                
C                                                                               
C------------ FORMAT 20F5.2                                                     
   90 WRITE(IOUT,91) I,(BUF(J,I),J=1,NCOL)                                      
   91 FORMAT(1H0,I3,1X,20(1X,F5.2)/(5X,20(1X,F5.2)))                            
      GO TO 1000                                                                
C                                                                               
C------------ FORMAT 20F5.3                                                     
  100 WRITE(IOUT,101) I,(BUF(J,I),J=1,NCOL)                                     
  101 FORMAT(1H0,I3,1X,20(1X,F5.3)/(5X,20(1X,F5.3)))                            
      GO TO 1000                                                                
C                                                                               
C------------ FORMAT 20F5.4                                                     
  110 WRITE(IOUT,111) I,(BUF(J,I),J=1,NCOL)                                     
  111 FORMAT(1H0,I3,1X,20(1X,F5.4)/(5X,20(1X,F5.4)))                            
      GO TO 1000                                                                
C                                                                               
C------------ FORMAT 10G11.4                                                    
  120 WRITE(IOUT,121) I,(BUF(J,I),J=1,NCOL)                                     
  121 FORMAT(1H0,I3,2X,1PG11.4,9(1X,G11.4)/(5X,10(1X,G11.4)))                   
C                                                                               
 1000 CONTINUE                                                                  
C                                                                               
C5------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE ULASAV(BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,                   
     1                   NROW,ILAY,ICHN)                                        
C                                                                               
C-----VERSION 1445 20APR1983 ULASAV                                             
C     ******************************************************************        
C     SAVE 1 LAYER ARRAY ON DISK                                                
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
      DIMENSION BUF(NCOL,NROW),TEXT(4)                                          
C     ------------------------------------------------------------------        
C                                                                               
C1------WRITE AN UNFORMATTED RECORD CONTAINING IDENTIFYING                      
C1------INFORMATION.                                                            
      WRITE(ICHN) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY                    
C                                                                               
C2------WRITE AN UNFORMATTED RECORD CONTAINING ARRAY VALUES                     
C2------THE ARRAY IS DIMENSIONED (NCOL,NROW)                                    
      WRITE(ICHN) ((BUF(IC,IR),IC=1,NCOL),IR=1,NROW)                            
C                                                                               
C3------RETURN                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE U1DREL(A,ANAME,JJ,IN,IOUT)                                     
C                                                                               
C                                                                               
C-----VERSION 1436 20MAY1983 U1DREL                                             
C     ******************************************************************        
C     ROUTINE TO INPUT 1-D REAL DATA MATRICES                                   
C       A IS ARRAY TO INPUT                                                     
C       ANAME IS 24 CHARACTER DESCRIPTION OF A                                  
C       JJ IS NO. OF ELEMENTS                                                   
C       IN IS INPUT UNIT                                                        
C       IOUT IS OUTPUT UNIT                                                     
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
CCC   CHARACTER ANAME*4
      DIMENSION A(JJ),ANAME(6),FMTIN(5)                                         
C     ------------------------------------------------------------------        
C                                                                               
C1------READ ARRAY CONTROL RECORD.                                              
      READ (IN,1) LOCAT,CNSTNT,FMTIN,IPRN                                       
    1 FORMAT(I10,F10.0,5A4,I10)                                                 
C                                                                               
C2------USE LOCAT TO SEE WHERE ARRAY VALUES COME FROM.                          
      IF(LOCAT.GT.0) GO TO 90                                                   
C                                                                               
C3------IF LOCAT=0 THEN SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN            
      DO 80 J=1,JJ                                                              
   80 A(J)=CNSTNT                                                               
      WRITE(IOUT,3) ANAME,CNSTNT                                                
    3 FORMAT(1H0,52X,6A4,' =',G15.7)                                            
      RETURN                                                                    
C                                                                               
C4------IF LOCAT>0 THEN READ FORMATTED RECORDS USING FORMAT FMTIN.              
   90 WRITE(IOUT,5) ANAME,LOCAT,FMTIN                                           
    5 FORMAT(1H0,///30X,6A4,' WILL BE READ ON UNIT',I3,                         
     1         ' USING FORMAT: ',5A4/30X,79('-')/)                              
      READ (LOCAT,FMTIN) (A(J),J=1,JJ)                                          
C                                                                               
C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.                
      IF(CNSTNT.EQ.0.) GO TO 120                                                
      DO 100 J=1,JJ                                                             
  100 A(J)=A(J)*CNSTNT                                                          
C                                                                               
C6------IF PRINT CODE (IPRN) =>0 THEN PRINT ARRAY VALUES.                       
 120  IF(IPRN.LT.0) RETURN                                                      
      WRITE(IOUT,1001) (A(J),J=1,JJ)                                            
 1001 FORMAT((1X,1PG12.5,9(1X,G12.5)))                                          
      RETURN                                                                    
C                                                                               
C7------CONTINUE                                                                
      END                                                                       
      SUBROUTINE U2DINT(IA,ANAME,II,JJ,K,IN,IOUT)                               
C                                                                               
C                                                                               
C-----VERSION 1442 20APR1983 U2DINT                                             
C     ******************************************************************        
C     ROUTINE TO INPUT 2-D INTEGER DATA MATRICES                                
C       IA IS ARRAY TO INPUT                                                    
C       ANAME IS 24 CHARACTER DESCRIPTION OF IA                                 
C       II IS NO. OF ROWS                                                       
C       JJ IS NO. OF COLS                                                       
C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT UNLESS K IS 0)         
C       IN IS INPUT UNIT                                                        
C       IOUT IS OUTPUT UNIT                                                     
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
CCC   CHARACTER ANAME*4
      DIMENSION IA(JJ,II),ANAME(6),FMTIN(5)                                     
C     ------------------------------------------------------------------        
C                                                                               
C1------READ ARRAY CONTROL RECORD.                                              
      READ (IN,1) LOCAT,ICONST,FMTIN,IPRN                                       
    1 FORMAT(I10,I10,5A4,I10)                                                   
C                                                                               
C2------USE LOCAT TO SEE WHERE ARRAY VALUES COME FROM.                          
      IF(LOCAT) 200,50,90                                                       
C                                                                               
C3------IF LOCAT=0 THEN SET ALL ARRAY VALUES EQUAL TO ICONST. RETURN            
   50 DO 80 I=1,II                                                              
      DO 80 J=1,JJ                                                              
   80 IA(J,I)=ICONST                                                            
      IF(K.GT.0) WRITE(IOUT,2) ANAME,ICONST,K                                   
    2 FORMAT(1H0,52X,6A4,' =',I15,' FOR LAYER',I3)                              
      IF(K.LE.0) WRITE(IOUT,3) ANAME,ICONST                                     
    3 FORMAT(1H0,52X,6A4,' =',I15)                                              
      RETURN                                                                    
C                                                                               
C4------IF LOCAT>0 THEN READ FORMATTED RECORDS USING FORMAT FMTIN.              
   90 IF(K.GT.0) WRITE(IOUT,4) ANAME,K,LOCAT,FMTIN                              
    4 FORMAT(1H0,///30X,6A4,' FOR LAYER',I3,' WILL BE READ ON UNIT',            
     1       I3,' USING FORMAT: ',5A4/30X,96('-'))                              
      IF(K.LE.0) WRITE(IOUT,5) ANAME,LOCAT,FMTIN                                
    5 FORMAT(1H0,///30X,6A4,' WILL BE READ ON UNIT',                            
     1        I3,' USING FORMAT: ',5A4/30X,83('-'))                             
      DO 100 I=1,II                                                             
      READ (LOCAT,FMTIN) (IA(J,I),J=1,JJ)                                       
  100 CONTINUE                                                                  
      GO TO 300                                                                 
C                                                                               
C5------LOCAT<0 THEN READ UNFORMATTED RECORD CONTAINING ARRAY VALUES            
  200 LOCAT=-LOCAT                                                              
      IF(K.GT.0) WRITE(IOUT,201) ANAME,K,LOCAT                                  
  201 FORMAT(1H0,///30X,6A4,', LAYER',I3,                                       
     1    ' WILL BE READ UNFORMATTED ON UNIT',I3/30X,73('-'))                   
      IF(K.LE.0) WRITE(IOUT,202) ANAME,LOCAT                                    
  202 FORMAT(1H0,///30X,6A4,                                                    
     1    ' WILL BE READ UNFORMATTED ON UNIT',I3/30X,60('-'))                   
C                                                                               
C5A------READ AN UNFORMATTED DUMMY RECORD FIRST.                                
      READ(LOCAT)                                                               
      READ(LOCAT) IA                                                            
C                                                                               
C6------IF ICONST NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICONST.                
  300 IF(ICONST.EQ.0) GO TO 320                                                 
      DO 310 I=1,II                                                             
      DO 310 J=1,JJ                                                             
      IA(J,I)=IA(J,I)*ICONST                                                    
  310 CONTINUE                                                                  
C                                                                               
C7------IF PRINT CODE (IPRN) =>0 THEN PRINT ARRAY VALUES.                       
  320 IF(IPRN.LT.0) RETURN                                                      
      IF(IPRN.GT.5) IPRN=0                                                      
      IPRN=IPRN+1                                                               
C                                                                               
C8------PRINT COLUMN NUMBERS AT TOP OF PAGE.                                    
      IF(IPRN.EQ.1) CALL UCOLNO(1,JJ,0,10,12,IOUT)                              
      NL=125/IPRN/5*5                                                           
      IF(IPRN.GT.1) CALL UCOLNO(1,JJ,4,NL,IPRN,IOUT)                            
C                                                                               
C9------PRINT EACH ROW IN THE ARRAY.                                            
      DO 110 I=1,II                                                             
C                                                                               
C10-----SELECT THE FORMAT                                                       
      GO TO(101,102,103,104,105,106), IPRN                                      
C                                                                               
C----------------FORMAT 10I11                                                   
  101 WRITE(IOUT,1001) I,(IA(J,I),J=1,JJ)                                       
 1001 FORMAT(1H0,I3,2X,I11,9(1X,I11)/(5X,10(1X,I11)))                           
      GO TO 110                                                                 
C                                                                               
C----------------FORMAT 60I1                                                    
  102 WRITE(IOUT,1002) I,(IA(J,I),J=1,JJ)                                       
 1002 FORMAT(1H0,I3,1X,60(1X,I1)/(5X,60(1X,I1)))                                
      GO TO 110                                                                 
C                                                                               
C----------------FORMAT 40I2                                                    
  103 WRITE(IOUT,1003) I,(IA(J,I),J=1,JJ)                                       
 1003 FORMAT(1H0,I3,1X,40(1X,I2)/(5X,40(1X,I2)))                                
      GO TO 110                                                                 
C                                                                               
C----------------FORMAT 30I3                                                    
  104 WRITE(IOUT,1004) I,(IA(J,I),J=1,JJ)                                       
 1004 FORMAT(1H0,I3,1X,30(1X,I3)/(5X,30(1X,I3)))                                
      GO TO 110                                                                 
C                                                                               
C----------------FORMAT 25I4                                                    
  105 WRITE(IOUT,1005) I,(IA(J,I),J=1,JJ)                                       
 1005 FORMAT(1H0,I3,1X,25(1X,I4)/(5X,25(1X,I4)))                                
      GO TO 110                                                                 
C                                                                               
C----------------FORMAT 20I5                                                    
  106 WRITE(IOUT,1006) I,(IA(J,I),J=1,JJ)                                       
 1006 FORMAT(1H0,I3,1X,20(1X,I5)/(5X,20(1X,I5)))                                
  110 CONTINUE                                                                  
      RETURN                                                                    
C                                                                               
C11-----RETURN                                                                  
      END                                                                       
      SUBROUTINE U2DREL(A,ANAME,II,JJ,K,IN,IOUT)                                
C                                                                               
C                                                                               
C-----VERSION 1439 20APR1983 U2DREL                                             
C     ******************************************************************        
C     ROUTINE TO INPUT 2-D REAL DATA MATRICES                                   
C       A IS ARRAY TO INPUT                                                     
C       ANAME IS 24 CHARACTER DESCRIPTION OF A                                  
C       II IS NO. OF ROWS                                                       
C       JJ IS NO. OF COLS                                                       
C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT UNLESS K IS 0)         
C       IN IS INPUT UNIT                                                        
C       IOUT IS OUTPUT UNIT                                                     
C     ******************************************************************        
C                                                                               
C        SPECIFICATIONS:                                                        
C     ------------------------------------------------------------------        
CCC   CHARACTER ANAME*4
      DIMENSION A(JJ,II),ANAME(6),FMTIN(5)                                      
C     ------------------------------------------------------------------        
C                                                                               
C1------READ ARRAY CONTROL RECORD.                                              
      READ (IN,1) LOCAT,CNSTNT,FMTIN,IPRN                                       
    1 FORMAT(I10,F10.0,5A4,I10)                                                 
C                                                                               
C2------USE LOCAT TO SEE WHERE ARRAY VALUES COME FROM.                          
      IF(LOCAT) 200,50,90                                                       
C                                                                               
C3------IF LOCAT=0 THEN SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN            
   50 DO 80 I=1,II                                                              
      DO 80 J=1,JJ                                                              
   80 A(J,I)=CNSTNT                                                             
      IF(K.GT.0) WRITE(IOUT,2) ANAME,CNSTNT,K                                   
    2 FORMAT(1H0,52X,6A4,' =',G15.7,' FOR LAYER',I3)                            
      IF(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT                                     
    3 FORMAT(1H0,52X,6A4,' =',G15.7)                                            
      RETURN                                                                    
C                                                                               
C4------IF LOCAT>0 THEN READ FORMATTED RECORDS USING FORMAT FMTIN.              
   90 IF(K.GT.0) WRITE(IOUT,4) ANAME,K,LOCAT,FMTIN                              
    4 FORMAT(1H0,///30X,6A4,' FOR LAYER',I3,' WILL BE READ ON UNIT',            
     1       I3,' USING FORMAT: ',5A4/30X,96('-'))                              
      IF(K.LE.0) WRITE(IOUT,5) ANAME,LOCAT,FMTIN                                
    5 FORMAT(1H0,///30X,6A4,' WILL BE READ ON UNIT',                            
     1       I3,' USING FORMAT: ',5A4/30X,83('-'))                              
      DO 100 I=1,II                                                             
      READ (LOCAT,FMTIN) (A(J,I),J=1,JJ)                                        
  100 CONTINUE                                                                  
      GO TO 300                                                                 
C                                                                               
C5------LOCAT<0 THEN READ UNFORMATTED RECORD CONTAINING ARRAY VALUES            
  200 LOCAT=-LOCAT                                                              
      IF(K.GT.0) WRITE(IOUT,201) ANAME,K,LOCAT                                  
  201 FORMAT(1H0,///30X,6A4,', LAYER',I3,                                       
     1    ' WILL BE READ UNFORMATTED ON UNIT',I3/30X,73('-'))                   
      IF(K.LE.0) WRITE(IOUT,202) ANAME,LOCAT                                    
  202 FORMAT(1H0,///30X,                                                        
     1    ' WILL BE READ UNFORMATTED ON UNIT',I3/30X,60('-'))                   
C                                                                               
C5A------READ AN UNFORMATTED DUMMY RECORD FIRST.                                
      READ(LOCAT)                                                               
      READ(LOCAT) A                                                             
C                                                                               
C6------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.                
  300 IF(CNSTNT.EQ.0.) GO TO 320                                                
      DO 310 I=1,II                                                             
      DO 310 J=1,JJ                                                             
      A(J,I)=A(J,I)*CNSTNT                                                      
  310 CONTINUE                                                                  
C                                                                               
C7------IF PRINT CODE (IPRN) =>0 THEN PRINT ARRAY VALUES.                       
  320 IF(IPRN.LT.0) RETURN                                                      
      CALL ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT)                                
      RETURN                                                                    
C                                                                               
C8------RETURN                                                                  
      END                                                                       
                                                                                
