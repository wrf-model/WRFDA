      SUBROUTINE  W2GCONV(ETPG,PS,GTMP,GU,GV,GSPH)
      DIMENSION ID(5)
      DIMENSION A(KMAX),B(KMAX)
      DIMENSION LAG(MEND1,NEND1)
      DIMENSION IFAX(10),TRIGS(IMAX)
      DIMENSION PNM (MNWAV,JOUTHF),DPNM(MNWAV,JOUTHF),
     1          ESINCL(JOUT),ECOSCL(JOUT),GCOSCL(JMAXG)
C
      DIMENSION QDAT1(KMX2,MNWAV),QDAT2(KMX2,MNWAV),
     1          QPHIS(   2,MNWAV)
C
      DIMENSION EDAT1(IMX *JOUT*KLMAX),EDAT2(IMX *JOUT*KLMAX),
     1          EWRK (IMX *JOUT*KLMAX),GU   (IOUT*JOUT*KLMAX),
     2          GV   (IOUT*JOUT*KLMAX),GTMP (IOUT*JOUT*KLMAX),
     3          GSPH (IOUT*JOUT*KLMAX),GOMG (IOUT*JOUT*KLMAX),
     4          GPHI (IOUT*JOUT*KLMAX),GDIV (IOUT*JOUT*KLMAX),
     5          ETPG (IOUT*JOUT),
     6          PS   (IOUT*JOUT),PSX (IOUT*JOUT),PSY (IOUT*JOUT)
      DIMENSION GUS  (IMAXG,JMAXG)   ,GVS  (IMAXG,JMAXG),
     1          GTS  (IMAXG,JMAXG)   ,GTDS (IMAXG,JMAXG),
     2          GRRR (IMAXG,JMAXG)
      DIMENSION WK   (IOUT*KMAX*12)  ,WL   (IOUT*LMAX*2)
      DIMENSION PWRKS(IOUT*JOUT),IDA (IOUT*JOUT)
C
      DIMENSION PWRK (IOUT*JOUT*LMAX)
C
      REAL*8 GAUL(JMAXG),GAUW(JMAXG)
      REAL*8 PPNM(MNWAV),HHNM(MNWAV)
C
      EQUIVALENCE (EWRK,PWRK)
C
      DIMENSION KTOUT(82)
	  REAL,    POINTER :: TS12(:,:,:)
	  INTEGER, POINTER :: KTTS(:)
C
      DATA IT,JT,BS,FM/4,4,0.0,1.0E10/
      DATA ER,GASR,GRAV,GAMMA/6371.E03,287.04,9.8,5.0E-3/
      DATA ITSAV,IPSL/1,1/
      DATA RHMIN,TDMAX/1.0E-6,31.0/
      DATA KTOUT/82*999/
      DATA IWVHST,ITPGFL
     1    /     1,     3/
	  DATA NGH,NWU,NWV,NTM,NTD,NRR,NRV,NPV
     1    /  1,  2,  3,  4,  5,  6,  7,  8/
      DATA KTPART /-1/
      DATA KTOHK,IHKFL/42,99/

      NAMELIST /NAMOUT/ KTPART,KTOUT,ITSAV,KTOHK,ID_PROC
	  NAMELIST /NAMFIL/ IWVHST,ITPGFL
      NAMELIST /NAMDBG/ NOMTN,IDEBUG,IT,JT,IPSL
      NAMELIST /NAMSLP/ MINKTI
C------------------------------------------------------------------
C Check parameters
C------------------------------------------------------------------
	  IF (IGRD.NE.IOUT.OR.JGRD.NE.JOUT) THEN
		WRITE(*,*)'OUT GRID DOES NOT MATCH'
	  	WRITE(*,*)'IOUT=',IOUT,'JOUT=',JOUT
	  	WRITE(*,*)'IGRD=',IGRD,'JGRD=',JGRD
		STOP 9998
	  END IF
C#######################################################################
C  +++ COMPUTE AND READ IN CONSTANTS +++
C#######################################################################
      READ (95,NAMOUT)
      READ (95,NAMFIL)
      READ (95,NAMDBG)
      READ (95,NAMSLP)
      WRITE(96,NAMOUT)
      WRITE(96,NAMFIL)
      WRITE(96,NAMDBG)
      WRITE(96,NAMSLP)
C------------------------------------------------------------------
C Init variables
C------------------------------------------------------------------
	  NNMAX=12/MINKTI+1
	  ALLOCATE(KTTS(0:NNMAX))
	  ALLOCATE(TS12(IOUT,JOUT,0:NNMAX))
      DO N=0,NNMAX
     	 KTTS(N)=-999
	  END DO
C------------------------------------------------------------------
C CHECK DATE    SURFACE DATA FILE
C------------------------------------------------------------------
      IO=IOUT
      JO=JOUT
      IJOUT=IOUT*JOUT
      IJMAX=IMAX*JOUT
      IJKOUT=IOUT*JOUT*KMAX
      IJLOUT=IOUT*JOUT*LMAX
      GINV  =1./GRAV
C-----------------------------------------------------------------------
      CALL SETARY(LAG,MEND1,NEND1,JEND1)
C-----------------------------------------------------------------------
C  +++ SET UP FFT COEFFICIENTS +++
C-----------------------------------------------------------------------
      CALL RFFTIM(IMAX,TRIGS,IFAX)
C-----------------------------------------------------------------------
C  +++ SETUP GAUSSIAN LATITUDES +++
C-----------------------------------------------------------------------
CLSW  CALL GAUSS(GAUL,GAUW,JMAXG)
CLSW  COSCL(1:JMAXG)=GAUL(1:JMAXG)
C-----------------------------------------------------------------------
C  +++ SETUP ZNM ETC +++
C-----------------------------------------------------------------------
      CALL ZNME2PXX
     I(MEND1,MNWAV,JOUTHF,
     O PNM  ,DPNM ,
     W PPNM ,HHNM)
      PAI=4.0*ATAN(1.0)
      DPAI=PAI/FLOAT(JOUT-1)
      DO 130 J=1,JOUT
      	ESINCL(J)=SIN(DPAI*FLOAT(J-1))
      	ECOSCL(J)=COS(DPAI*FLOAT(J-1))
 130  CONTINUE
C-----------------------------------------------------------------------
C  +++ READ IN TOPOGRAPHY FILE +++
C-----------------------------------------------------------------------
        IF(ITPGFL.NE.0) THEN
          READ(ITPGFL) MDIM,DPHIX,IDIM,JDIM,JHMSPH
          DPHIX=DPHIX
          IDIM=IDIM
          JDIM=JDIM
          IF(MDIM.NE.MEND1.OR.JHMSPH.NE.0) THEN
            WRITE(96,901) MDIM,MEND1,JHMSPH
  901       FORMAT(1H ,'*** ERROR IN TOPOGRPHY FILE ***'/
     1                ,1X,'(MDIM,MEND1)=(',I3,',',I3,')'/
     2                ,1X,'JHMSPH      = ',I3/)
            STOP 3002
          END IF
          READ(ITPGFL) ((QDAT1(KKK,NNN),KKK=1,2),NNN=1,MNWAV)
          CLOSE (ITPGFL)
C
          CALL REOWV
     1     (QDAT1,QPHIS,MEND1,NEND1,JEND1,MNWAV,KMX2,2,0,0,2,LAG)
        ELSE
        END IF
C
        CALL W2G
     I   (MEND1,NEND1,JEND1,MNWAV,IMAX,JOUT,IMX,JOUTHF,   1,
     I    IFAX ,TRIGS,PNM  ,QPHIS,
     O    EDAT1,
     W    EWRK)
C
        CALL CUT(ETPG,IOUT,JOUT,   1,EDAT1,IMAX,INTVL)
        DO 160 IJ=1,IJOUT
        ETPG(IJ)=GINV*ETPG(IJ)
  160   CONTINUE
        IF(IDEBUG.GE.1) THEN
          CALL GOUT(ETPG,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'PHIS',1)
        END IF

C
C#######################################################################
C  +++ READ IN HISTORY FILE +++
C  +++ WAVE TO GRID CONVERSION START +++
C#######################################################################
C...IWVHST; WAVE  HISTORY      FILE
C...ISFCFL; GAUSS GRID SURFACE FILE
C...IGHIST; EQUAL LAT-LON GRID P-HISTORY FILE
C           IF IGHIST=0, NO FILE OUTPUT
C...IPGUES; EQUAL LAT-LON GRID   P-GUESS FILE
C           IF IPGUES=0, NO FILE OUTPUT
C...IXXXFL; GRID ETA-TEMPORARY FILE FOR XXX PASSED TO ETA-P CONVERSION
C           IF IXXXFL=0, NO XXX TEMPORARY FILE OUTPUT
C           XXX IS TMP,SPH,U,V,DIV,OMG
C
      ICOUNT=1
C
 2000 CONTINUE
C

      IF(KTOUT(ICOUNT).EQ.999.AND.KTOUT(1).NE.-999) GOTO 1001
      READ(IWVHST) ID,KT,ISTP,FSEC,NREC,                       
     1                      KM,(A(K),K=1,KM),(B(K),K=1,KM)              
      WRITE(96,666) ID,KT                                              
 666  FORMAT(1H ,'HISTORY FILE READ IN AT',6I5)                         
C     IF(KT.LE.0) THEN                                                  
C          WRITE(96,*)'KT=',KT
C     END IF                                                            
C                                                                       
      IF(KM.NE.KMAX) THEN                                               
          WRITE(96,902)                                                 
  902     FORMAT(1X ,'*** ERROR IN NO. OF ETALEV ***')                  
          STOP 3001                                                     
      END IF                                                           
!
      IF(KT.EQ.KTOUT(ICOUNT).OR.KTOUT(1).EQ.-999) THEN                  
        ICOUNT=ICOUNT+1                                                 
        WRITE(96,905) ID,KT                                             
  905   FORMAT(1H ,2X,'YEAR=',I4,4X,'MONTH=',I2,4X,'DAY=',I2,4X,'HOUR=',
     *          I2,4X,'WEEK=',I1,4X,'FCST=',I3)                         
C---------------------------------------------------------------------
C...SURFACE PRESSURE AND ITS GRADIENTS
C---------------------------------------------------------------------
        READ(IWVHST) (QDAT1(KMN,1),KMN=1,2*MNWAV)
      	WRITE(96,*) ID,KT,'qdat1'
C...IPXY=1;OUTPUT IS PS,D(PS)/DX,-D(PS)/DY
      	IPXY=1
        CALL W2GPXY
     I (MEND1,NEND1,JEND1,MNWAV,IMAX,JOUT,IMX,JOUTHF,
     I  IFAX ,TRIGS,PNM  ,DPNM ,IPXY,IPSL,QDAT1,
     O  EDAT1,
     W  EWRK)
        CALL CUT(PS  ,IOUT,JOUT,1,EDAT1(        1),IMAX,INTVL)
        CALL CUT(PSX ,IOUT,JOUT,1,EDAT1(  IJMAX+1),IMAX,INTVL)
        CALL CUT(PSY ,IOUT,JOUT,1,EDAT1(2*IJMAX+1),IMAX,INTVL)
        IF(IDEBUG.GE.1) THEN
          CALL GOUT(PS ,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'PSFC',1)
!         CALL GOUT(PSX,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'DPSX',1)
!         CALL GOUT(PSY,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'DPSY',1)
        END IF
C---------------------------------------------------------------------
C...TEMPERATURE
C---------------------------------------------------------------------
        READ (IWVHST) QDAT1
        CALL W2G
     I (MEND1,NEND1,JEND1,MNWAV,IMAX,JOUT,IMX,JOUTHF,KMAX,
     I  IFAX ,TRIGS,PNM  ,QDAT1,
     O  EDAT1,
     W  EWRK)
        CALL CUT(GTMP,IOUT,JOUT,KMAX,EDAT1,IMAX,INTVL)
        IF(IDEBUG.GE.1) THEN
           K=KMAX
          CALL GOUT(GTMP,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'ETMP',K)
        END IF
C----------------------------------------------------------------------
C... VORTICITY & DIVERGENCE
C----------------------------------------------------------------------
C...VORTICITY
        READ(IWVHST)  QDAT1
C...DIVERGENCE
        READ(IWVHST)  QDAT2
        CALL W2G
     I (MEND1,NEND1,JEND1,MNWAV,IMAX,JOUT,IMX,JOUTHF,KMAX,
     I  IFAX ,TRIGS,PNM  ,QDAT2,
     O  EDAT1,
     W  EWRK)
        CALL CUT(GDIV,IOUT,JOUT,KMAX,EDAT1,IMAX,INTVL)
        IF(IDEBUG.GE.5) THEN
          K=IDEBUG
          CALL GOUT(GDIV,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'EDIV',K)
        END IF
        CALL W2GUV
     I (MEND1,NEND1,JEND1 ,MNWAV,IMAX,JOUT,IMX  ,JOUTHF,KMAX,
     I  IFAX ,TRIGS,ESINCL,ER   ,PNM ,DPNM,QDAT1,QDAT2 ,
     O  EDAT1,EDAT2,
     W  EWRK)
        CALL CUT(GU  ,IOUT,JOUT,KMAX,EDAT1,IMAX,INTVL)
        CALL CUT(GV  ,IOUT,JOUT,KMAX,EDAT2,IMAX,INTVL)
        IF(IDEBUG.GE.1) THEN
          K=KMAX
          CALL GOUT(GU,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'EU  ',K)
          CALL GOUT(GV,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'EV  ',K)
        END IF
C----------------------------------------------------------------------
C...SPECIFIC HUMIDITY
C----------------------------------------------------------------------
        READ(IWVHST)  QDAT1
        CALL W2G
     I (MEND1,NEND1,JEND1,MNWAV,IMAX,JOUT,IMX,JOUTHF,KMAX,
     I  IFAX ,TRIGS,PNM  ,QDAT1,
     O  EDAT1,
     W  EWRK)
        CALL CUT(GSPH,IOUT,JOUT,KMAX,EDAT1,IMAX,INTVL)
        IF(IDEBUG.GE.1) THEN
          K=KMAX
          CALL GOUT(GSPH,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'ESPH',K)
        END IF
      ELSE
        READ(IWVHST)                                                    
        READ(IWVHST)                                                   
        READ(IWVHST)                                                    
        READ(IWVHST)                                                    
        READ(IWVHST)                                                    
        GO TO 2000                                                      
      END IF
C
      write(96,*)'kt, ktpart=',kt, ktpart
C
      GO TO 2000
C
 1001 CONTINUE
      RETURN
      END SUBROUTINE  W2GCONV
