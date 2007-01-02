      SUBROUTINE PREGSM1(PS,GT,GU,GV,GQ,PSB,GTB,GUB,GVB,GQB)   !shc start
      INTEGER IDATE(5), IDGES(5), IDSST(5)                                      
      CHARACTER*8 FILE, MODEL, RESL                                             
      CHARACTER*80 CINF(10)                                                     
      CHARACTER*4 TYPE, EXPR, KTUNIT, NPROD, NPROM, VCODD, VCODM                
      CHARACTER*4 LEVEL, ELEM                                                   
      CHARACTER*32 TITLE                                                        
      CHARACTER*16 UNIT                                                         
      CHARACTER*8 MDLINF(4)                                                     
      REAL        DTHPRO(7)                                                     
      INTEGER ITYP(2)                                                           
      CHARACTER*48 LABEL                                                        
      INTEGER JTINF(2)                                                          
	  CHARACTER*10	FROMUNPACK
	  INTEGER		IUNPACK
C                                                                               
      DIMENSION A(KMAX+1), B(KMAX+1), AAM(KMAX+1), BBM(KMAX+1)                  
      DIMENSION AGD(KMAX+1), BGD(KMAX+1), AGM(KMAX+1), BGM(KMAX+1)              
      DIMENSION GPHIS(IMAX*JMAX)                                                
      REAL, DIMENSION(IMAX,JMAX)    :: GAU
      REAL, DIMENSION(JMAX)         :: SINCLT,COSCLT,GW,DGW,DCOSCL,COLRAD,DY
      DIMENSION  PS  (IMAX,JMAX),  GRH (IMAX,JMAX,KMAX),  !shc 
     1       GZ  (IMAX,JMAX,KMAX), GT  (IMAX,JMAX,KMAX),                        
     2       GU  (IMAX,JMAX,KMAX), GV  (IMAX,JMAX,KMAX),                        
     3       GQ  (IMAX,JMAX,KMAX), AGT (IMAX,JMAX,KMAX),
     4       GCWC(IMAX,JMAX,KMAX), GCVR(IMAX,JMAX,KMAX),                        
     5       GUMB(IMAX,JMAX,KMAX),                                              
     6       GSST(IMAX,JMAX)     , GSNW(IMAX,JMAX)
      DIMENSION  PSB (IMAX,JMAX),  GRHB(IMAX,JMAX,KMAX),  !shc start 
     1       GQB (IMAX,JMAX,KMAX), GTB (IMAX,JMAX,KMAX),                        
     2       GUB (IMAX,JMAX,KMAX), GVB (IMAX,JMAX,KMAX)   !shc end

      DIMENSION VLG(IMAX,JMAX,KMAX)                                             
C     DIMENSION WRK1(IMAX,JMAX,KMAX), WRK2(IMAX,JMAX,KMAX),                     
      REAL * 8  WRK1(IMAX,JMAX,KMAX), WRK2(IMAX,JMAX,KMAX)                      
      DIMENSION WRK3(IMAX,JMAX,KMAX), WRK4(IMAX,JMAX,KMAX),                     
     2          WRK5(IMAX,JMAX,KMAX), WRK6(IMAX,JMAX,KMAX)                      
      CHARACTER*4 ALVL                                                          
      INTEGER*2 I2(IDIM*JDIM)                                                   
      REAL*8    WRK(IDIM,JDIM)                                                  
      DIMENSION SSTA(ISST*JSST), SEWA(ISNW,JSNW)                                
      DIMENSION COLRAD(JMAX), DY(JMAX), LY(JMAX)                                
      DIMENSION WORK(362,182),DP(4,IMAX,JMAX)                                   
      INTEGER*2 IP(2,IMAX,JMAX)                                                 
      REAL*8    GAUL(JMAX),GAUW(JMAX),COCOT(JMAX)                               
      COMMON/CTETEN/TABLE(25000)                                                
      COMMON/DTETEN/DTABLE(25000)                                               
      REAL*8 TABLE,DTABLE,RGSA,G                                                       
      real   rdum (imax,jmax,kmax)      !shcimsi
      DIMENSION RLAT(MAXJZ), ZDAT(MAXJZ,KMAX)                                   
C                                                                               
      NAMELIST /NAMFIL/ NALFL,NVPFL,NGSFL,NSSTFL,NSNWFL,NINFL,
     1				    KTLAG,IDCHCK,NDIGFL,NTPFL,NALOT,NRSFL
C     NAMELIST /HEADIN/ TYPE,FILE,KTUNIT,IDTYPE,
C    1                  IBACK,NNSP
C------------------------------------------------------------------------
C  NALFL : 3DOI INPUT FILE
C  NVPFL : VERTIAL LEVEL DEF. FILE
C  NTPFL : TOPO FILE
C  NALOT : 3DOI INPUT SAVE FILE
C  NRSFL : UNPACK INPUT FILE
C------------------------------------------------------------------------
      NAMELIST /NAMVER/ MODEL, RESL, EXPR, CINF                                 
C                                                                               
      DATA RHMIN/1.0E-3/                             
      DATA GRAV,ER,GASR,GAMMA/9.80665,6371.E3,287.04,0.0050/                    
      DATA TLAPS,QCONS,QMIN,KST,ITERMX/2.0E-3,2.5E-6,1.0E-10,10,3/              
C                                                                               
      DATA NALFL ,NGSFL ,NSSTFL,NSNWFL,NINFL ,NVPFL ,NALOT,NRSFL
     1    /     1,     2,    -1,    -1,    11,    21,   12,   -1/                           
      DATA KTLAG / 6/                                                           
      DATA IDCHCK/ 1/                                                           
      DATA A/0.00000000000D+00,0.00000000000D+00,0.00000000000D+00,
     &       0.00000000000D+00,1.546082500000000,5.614406590000000,
     &       12.42546270000000,21.63197330000000,32.59785460000000,
     &       44.61235050000000,57.01704410000000,69.26280210000000,
     &       80.92097470000000,91.66931150000001,101.2670900000000,
     &       109.5278170000000,116.2947540000000,121.4214780000000,
     &       124.7591550000000,126.1514430000000,125.4377290000000,
     &       122.4657440000000,117.1135710000000,109.3194430000000,
     &       99.11479190000000,86.65005490000000,72.19601440000000,
     &       56.09729000000000,38.66041560000000,19.99998470000000,
     &       0.00000000000D+00/
      DATA B/1.0000000000000,0.9889042970000000,0.9682830569999999,
     &       0.9399999980000000,0.9042294030000000,0.8613848090000000,
     &       0.8124753240000000,0.7589231130000000,0.7022829060000000,
     &       0.6440208549999999,0.5853865740000000,0.5273658630000000,
     &       0.4706876280000000,0.4158638720000000,0.3632441160000000,
     &       0.3130739930000000,0.2655510310000000,0.2208738920000000,
     &       0.1792818900000000,0.1410827640000000,0.1066635850000000,
     &       7.647979300000D-02,5.101471400000D-02,3.070007300000D-02,
     &       1.579232499999D-02,6.205350000000D-03,1.324939000000D-03,
     &       0.000000000000D+00,0.000000000000D+00,0.000000000000D+00,
     &       0.000000000000D+00/
C                                                                               
C   =================================================================           
C   >>>   READ ANAL TIME                                          <<<         
C   =================================================================           
          READ(94,'(I4,3I2)') (IDATE(I),I=1,4)
          IDATE(5)=0
C   =================================================================           
C   >>>   NAMELIST (NAMFIL)                                       <<<           
C   =================================================================           
      READ(95,NAMFIL) 
C     READ(95,HEADIN)
      WRITE(6,NAMFIL)                                                           
C     WRITE(6,HEADIN)                                                           
C   =================================================================
C   >>>   Select Input Source                                     <<<
C   =================================================================
!shc-wei start
c     CALL GETENV('FROMUNPACK',FROMUNPACK)
c     IF (LEN_TRIM(FROMUNPACK).EQ.0) THEN
c       IUNPACK=0
c     ELSE
c       READ(FROMUNPACK,'(I1)') IUNPACK
c     END IF
c     WRITE(6,*)'IUNPACK=',IUNPACK
!shc-wei end
C   =================================================================           
C   >>>   GENERATE GAUSSIAN LATITUDES                             <<<           
C   =================================================================           
      CALL GAUSS(GAUL,GAUW,JMAX)                                                
      DO 800 J=1,JMAX                                                           
      COLRAD(J)=ACOS(GAUL(J))                                                   
  800 CONTINUE                                                                  
      DO J=1,JMAXHF
*vdir nodep
        GW    (       J)=0.5*DGW   (J)
        GW    (JMAX+1-J)=0.5*DGW   (J)
        COSCLT(       J)=     DCOSCL(J)
        COSCLT(JMAX+1-J)=    -DCOSCL(J)
        SINCLT(       J)=SQRT(1.0-DCOSCL(J)**2)
        SINCLT(JMAX+1-J)=SQRT(1.0-DCOSCL(J)**2)
      END DO
      CALL ZMNLAT( RLAT, MAXJZ, COLRAD, JMAX )                                  
C                                                                               
C   =================================================================           
C   >>>   TETEN                                                   <<<           
C   =================================================================           
      ICE = 1                                                                   
      CALL TETEN(ICE)                                                           
C   =================================================================           
C   >>>  READ TOPO FILE
C   =================================================================           
          NTPFL = 3
c         go to 33333   ! shc For T63 only
 	  IF (NTPFL.GT.0) THEN
 	  	READ(NTPFL)NWV,DUM,IGRD,JGRD
 	  	IF ((IGRD.NE.IMAX).OR.(JGRD.NE.JMAX)) THEN
 			WRITE(*,*)' TOPO DIM DOES NOT MATCH'
 			WRITE(*,*)'IMAX=',IMAX,' IGRD=',IGRD
 			WRITE(*,*)'JMAX=',JMAX,' JGRD=',JGRD
 			STOP 9988
 	  	END IF
 	  	READ(NTPFL)
 	  	READ(NTPFL)
 	  	READ(NTPFL)GPHIS
 	  	WRITE(*,*)'GRID DISTANCE=',DUM
 	  END IF
C---------------------------------------------------------------------
C +++ CONVERT LAT/LON to GAUSS
C---------------------------------------------------------------------
33333        continue
c 	READ(NTPFL,'(10f10.3)')GPHIS      !shc For T63 only
C                                                !shc start
C   =================================================================           
C   >>>   PS, TEMP, Q -> RH
C   =================================================================           
      CALL RELHUM                                 
     I  (GT ,GQ ,PS ,IMAX,JMAX,KMAX,A,B, 
     O   GRH)                                    
      CALL RELHUM                                 
     I  (GTB ,GQB ,PSB ,IMAX,JMAX,KMAX,A,B, 
     O   GRHB)                                   !shc end
      PIHF = 3.141593*0.5                              !shc start
      DO 3739 K = 1,KMAX
      DO 3738 J = 1,JMAX
      DO 3737 I = 1,IMAX
        AANAL = GRH(I,J,K)-GRHB(I,J,K)                 !shc end
        AGES  = GRHB(I,J,K)                            !shc start
        IF    ( AGES.LE.0.0 .AND. AANAL.LE.0.0 ) THEN
           AANAL = 1.0E-6  ! \214\270\202\351\227]\222n\202\252\202\310\202\242
        ELSEIF( AGES.GE.1.0 .AND. AANAL.GE.0.0 ) THEN
           AANAL = 1.0     ! \221\235\202\246\202\351\227]\222n\202\252\202\310\202\242
        ELSE
          IF( AANAL.GT.0.0 ) THEN
            RES = 1.0-AGES  ! \213\226\227e\227\312
          ELSE                                         !shc end
            RES =    -AGES                             !shc start
          ENDIF
          IF( ABS(AANAL).LE.ABS(RES*0.5) ) THEN ! \224\274\225\252\210\310\211\272
            AANAL = AGES+AANAL    ! \202\273\202\314\202\334\202\334
          ELSE                                   
            AA = RES/PIHF*0.5                          !shc end
            XN = AANAL-RES*0.5                         !shc start
            AANAL        = AGES + 0.5*RES + AA*ATAN(XN/AA)
          ENDIF
        ENDIF
        AANAL        = DMAX1( AANAL, 1.0D-6 )  ! \215\305\217I\222\262\220\256
        AANAL        = DMIN1( AANAL, 1.0D0  )
        GRH(I,J,K) = AANAL
 3737 CONTINUE
 3738 CONTINUE
 3739 CONTINUE                                         !shc end

C   =================================================================           
C   >>>   PS, TEMP, Q -> Z
C   =================================================================           
      RGAS = 287.04
      G    = 9.80665
      CALL GPLHGT
     I  (PS,GT,GQ,GPHIS,IMAX,JMAX,KMAX,RGAS,G,A,B,
     I      1,JMAX,
     O   GZ)

CLSW   do k=1,22,3
CLSW    do j=1,jmax
CLSW      write(99,FMT='(10F12.5,1x)') (GZ(I,J,K),I=1,IMAX)
CLSW    enddo
CLSW   enddo
C  ==================================================================
C  >>> SAVE INPUT DATA
C  ==================================================================
        IF (NALOT.GT.0) THEN
            WRITE(NALOT)PS
            WRITE(NALOT)GZ
            WRITE(NALOT)GU
            WRITE(NALOT)GV
            WRITE(NALOT)GQ
            WRITE(NALOT)GT
        END IF
Crizvi      ELSE          ! START WITH UNPACK FILE
Crizvi        LARHM=20
Crizvi        READ(NRSFL)IDATE
Crizvi        READ(NRSFL)PS
Crizvi        READ(NRSFL)GZ
Crizvi        READ(NRSFL)GU
Crizvi        READ(NRSFL)GV
Crizvi        READ(NRSFL)GQ
Crizvi        READ(NRSFL)AGT
Crizvi
Crizvi      END IF ! READ ANAL FINISH
       print*,' Gaussian lats data size ',IMAX, JMAX, KMAX
        write(661,'(10f10.3)')PS
        write(661,'(10f10.3)')GU
        write(661,'(10f10.3)')GV
        write(661,'(10f10.3)')GT
        write(661,'(10f10.3)')GQ

CLSW      write(99,*) ' Gauss GT'
CLSW   do k=1,2
CLSW    do j=1,jmax
CLSW      write(99,FMT='(10F12.5,1x)') (GT(I,J,k),I=1,IMAX)
CLSW    enddo
CLSW   enddo
C---------------------------------------------------------------------
C      DO J = 1, JMAX
C        write(99,FMT='(10F12.5,1x)') (GAU(I,J),I=1,IMAX)
C      ENDDO

1000  CONTINUE

C   =================================================================           
C   >>>   NAMELIST (NAMVER)                                       <<<           
C   =================================================================           
      CINF(1)=' ';CINF(2)=' ';CINF(3)=' ';CINF(4)=' ';CINF(5)=' '               
      CINF(6)=' ';CINF(7)=' ';CINF(8)=' ';CINF(9)=' ';CINF(10)=' '              
      READ(95,NAMVER)                                                            
      WRITE(6,NAMVER)                                                           
C   =================================================================
      IF(NGSFL.GE.0) THEN
      CALL REDGES
     I(NGSFL ,IMAX  ,JMAX  ,KMAX  ,KTLAG ,IDATE ,IDCHCK,
     O IDGES ,AGD   ,BGD   ,AGM   ,BGM   ,GCWC  ,GCVR  ,GUMB  ,
     W I2    ,IDSST )
      ENDIF
C   =================================================================           
C   >>>   Z -> TV                                                 <<<           
C   =================================================================           
C     CALL CTIME( 4, 'ZE2TVE              ' )
C   >>> GT IS TV (OUTPUT)
      IF (NTPFL.LT.0) THEN
        CALL GH2TV(GZ, GT, PS, GPHIS, A, B,
     1          IMAX  , JMAX  , KMAX  ,WRK1  , WRK2  , WRK3  , WRK4)
      ELSE
        CALL ZE2TVE( GZ    , GT    , PS    , A     , B     ,
     I             IMAX  , JMAX  , KMAX  ,
     W             VLG   , WRK1  , WRK2  , WRK3  , WRK4  , WRK5  ,
     W             WRK6  )
C
CLSW      write(99,*) ' Z->TV'
CLSW   do k=1,2
CLSW    do j=1,jmax
CLSW      write(99,FMT='(10F12.5,1x)') (GT(I,J,K),I=1,IMAX)
CLSW    enddo
CLSW   enddo
      END IF
CLSW  CALL ZMNT( ZDAT, MAXJZ, KMAX, GT   , IMAX, JMAX )
CLSW  CALL OUTZ( ZDAT, MAXJZ, KMAX, 'TV  ',
CLSW 1            'TV                             ', 'K               ',
CLSW 2             0, RLAT, 'KMAX' )

C   =================================================================           
C   >>>   RH, TV -> Q, T                                          <<<           
C   =================================================================           
      IDX=1
      LARHM=20                 !shc start
      DO K=1,LARHM-1
      DO I=1,IMAX
      DO J=1,JMAX
       GQ(I,J,K)=GRH(I,J,K)
      ENDDO
      ENDDO
      ENDDO                    !shc end
      CALL CRH2SHA
     I(IMAX*JMAX, KMAX, PS, A, B, GRAV,GASR,TLAPS,QCONS,QMIN,KST,ITERMX,
     I IDX, LARHM,
     O GQ, GT)
C          write(99,*) ' after  RH, TV -> Q, T'
C       do k=1,2
C        do j=1,jmax
C          write(99,FMT='(10F12.5,1x)') (GT(I,J,K),I=1,IMAX)
C        enddo
C       enddo

C   *****************************************************************           
C   >>>   OUTPUT INITIAL VALUE                                    <<<           
C   *****************************************************************           
C   =================================================================           
C   >>>   HEADER                                                  <<<           
C   =================================================================           
      CALL WRTHED                                                               
     I(NINFL ,                                                                  
     I 'GVS1',IDATE ,'INITETA ',MODEL, RESL,                                    
     I EXPR  ,'HOUR',1     ,0     ,0     ,                                      
     I IMAX  ,JMAX  ,'GAUS',360.0/IMAX, REAL(JMAX),                             
     I 1.0   ,(JMAX+1)/2.0, 0.0   ,0.0   ,                                      
     I 'ETA ',KMAX  ,A     ,B     ,                                             
     I IMAX  ,JMAX  ,'GAUS',360.0/IMAX, REAL(JMAX),                             
     I 1.0   ,(JMAX+1)/2.0, 0.0   ,0.0   ,                                      
     I 'ETA ',KMAX  ,A     ,B     ,                                             
     I CINF  )                                                                  
C                                                                               
C   =================================================================           
C   >>>   PS                                                      <<<           
C   =================================================================           
      CALL MOVERD(PS, WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NINFL , IDATE , -1    , 'SURF', 'P   ',
     2 'P                               ', 'HPA             ',
     3 0     , 0     , WRK   , IMAX  , JMAX  , I2    )
C                                                                               
C   =================================================================           
C   >>>   U, V                                                    <<<           
C   =================================================================           
        DO 9030 K=1,KMAX
        CALL MOVERD(GU(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL , IDATE , -1    , ALVL  , 'U   ',
     2   'U                               ', 'M/S             ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9030   CONTINUE
        DO 9040 K=1,KMAX
        CALL MOVERD(GV(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL , IDATE , -1    , ALVL  , 'V   ',
     2   'V                               ', 'M/S             ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9040   CONTINUE
C   =================================================================           
C   >>>   T, Q                                                    <<<           
C   =================================================================           
        DO 9010 K=1,KMAX
        CALL MOVERD(GT(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL , IDATE , -1    , ALVL  , 'T   ',
     2   'T                               ', 'K               ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9010   CONTINUE
        DO 9020 K=1,KMAX
        CALL MOVERD(GQ(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL , IDATE , -1    , ALVL  , 'Q   ',
     2   'Q                               ', 'KG/KG           ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9020   CONTINUE
C   =================================================================           
C   >>>  SAVE INPUT FIELD FOR DIAG.
C   =================================================================           
      IF (NDIGFL.GT.0) THEN
        WRITE(NDIGFL)GT
        WRITE(NDIGFL)GQ
      END IF
C                                                                               
C   =================================================================           
C   >>>   CWC, CVR                                                <<<           
C   =================================================================           
      IF(NGSFL.GT.0) THEN
        DO 9050 K=1,KMAX
        CALL MOVERD(GCWC(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL , IDATE , -1    , ALVL  , 'CWC ',
     2   'CLOUD WATER CONTENT             ', 'KG/KG           ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9050   CONTINUE
        DO 9060 K=1,KMAX
        CALL MOVERD(GCVR(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL , IDATE , -1    , ALVL  , 'CVR ',
     2   'CLOUD COVER                     ', '-               ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9060   CONTINUE
C                                                                               
C   =================================================================           
C   >>>   UMB                                                     <<<           
C   =================================================================           
        DO 9070 K=1,KMAX
        CALL MOVERD(GUMB(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL , IDATE , -1    , ALVL  , 'UMB ',
     2   'UPWARD MASS FLUX AT CLOUD BASE  ', 'KG/S/M**2       ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9070   CONTINUE
      END IF   !NGSFL>0
C                                                                               
C   *****************************************************************           
C   >>>   SST ANOMALY                                             <<<           
C   *****************************************************************           
      IF( NSSTFL.NE.-1 ) THEN
      CALL GETTYP(NSSTFL,IOTYP)
C
      IF(IOTYP.EQ.1) THEN
C     CALL GVDFIR(NSSTFL,
C    1            IDSST,IBACK,IM,JM,MDLINF,DTHPRO,CINF,ITYP,IRTN)
C     WRITE(6,*) 'GVDFIR:IRTN=',IRTN
C     CALL GVDFNR(NSSTFL,IDSST,0,'SURF','SSTA',
C    1            LABEL,JTINF,SSTA,IRTN)
C     WRITE(6,*) 'GVDFNR:IRTN=',IRTN
      WRITE(*,*)' UNKNOWN IOTYP:1'
      STOP 9999
      ELSE IF(IOTYP.EQ.3) THEN
C   =================================================================           
C   >>>   HEADER                                                  <<<           
C   =================================================================           
      CALL REDHED
     I(NSSTFL,
     O TYPE  ,IDSST ,FILE  ,MODEL ,RESL  ,EXPR  ,KTUNIT,IDTYPE,
     O IBACK ,NNSP  ,
     O IMD   ,JMD   ,NPROD ,FLONID, FLATID,
     O XID   ,XJD   ,XLATD ,XLOND ,
     O VCODD ,KMD   ,A     ,B     ,
     O IMM   ,JMM   ,NPROM ,FLONIM, FLATIM,
     O XIM   ,XJM   ,XLATM ,XLONM ,
     O VCODM ,KMM   ,AAM   ,BBM   ,
     O CINF  )
C   =================================================================           
C   >>>   SST ANOMALLY                                            <<<           
C   =================================================================           
      DO 1 I=1,NNSP
        READ(NSSTFL)
    1 CONTINUE
 3001 CALL REDDAT
     I(NSSTFL,
     O IDSST , KT    ,
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,
     O SSTA  , IRTN  ,
     I ISST  , JSST  , 1     ,
     W BASE  , AMP   ,I2    )
      IF(ELEM.NE.'SSTA') GOTO 3001
      WRITE(6,*) '## ', TITLE, '(',UNIT,')'
      ENDIF
C
      WRITE(6,*) '## ', IDSST, KT
      IF( IDCHCK.EQ.1 ) THEN
        CALL CVDATE( IDGES, IDSST, 24 )
        IF( IDATE(1).NE.IDGES(1).OR.IDATE(2).NE.IDGES(2).OR.
     1      IDATE(3).NE.IDGES(3) ) THEN
          WRITE(6,*) 'SSTA : DATE CHECK ERROR'
          STOP 999
        ENDIF
      ENDIF
C
      CALL LT2GAU(SSTA,ISST,JSST,IMAX,JMAX,COLRAD,GSST,DY,LY)
      CALL MOVERD(GSST, WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NINFL , IDATE , -1    , 'SURF', 'SSTA',
     2 'SST ANOMALLY                    ', 'K               ',
     3 0     , 0     , WRK   , IMAX  , JMAX  , I2    )
      WRITE(6,*) '## SST ANOMALLY WAS WRITTEN'
C
      ENDIF
C                                                                               
C   *****************************************************************           
C   >>>   SNOW ANALYSIS                                           <<<           
C   *****************************************************************           
      IF( NSNWFL.NE.-1 ) THEN
C   =================================================================           
C   >>>   HEADER                                                  <<<           
C   =================================================================           
      CALL REDHED
     I(NSNWFL,
     O TYPE  ,IDSST ,FILE  ,MODEL ,RESL  ,EXPR  ,KTUNIT,IDTYPE,
     O IBACK ,NNSP  ,
     O IMD   ,JMD   ,NPROD ,FLONID, FLATID,
     O XID   ,XJD   ,XLATD ,XLOND ,
     O VCODD ,KMD   ,A     ,B     ,
     O IMM   ,JMM   ,NPROM ,FLONIM, FLATIM,
     O XIM   ,XJM   ,XLATM ,XLONM ,
     O VCODM ,KMM   ,AAM   ,BBM   ,
     O CINF  )
      DO 2 I=1,NNSP
        READ(NSNWFL)
    2 CONTINUE
C   =================================================================           
C   >>>   SNOW ANALYSIS                                           <<<           
C   =================================================================           
      CALL REDDAT
     I(NSNWFL,
     O IDSST , KT    ,
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,
     O SEWA  , IRTN  ,
     I ISNW  , JSNW  , 1     ,
     W BASE  , AMP   ,I2    )
      WRITE(6,*) '## ', TITLE, '(',UNIT,')'
      WRITE(6,*) '## ', IDSST, KT
      IF( IDCHCK.EQ.1 ) THEN
        CALL CVDATE( IDGES, IDSST, 24 )
        IF( IDATE(1).NE.IDGES(1).OR.IDATE(2).NE.IDGES(2).OR.
     1      IDATE(3).NE.IDGES(3) ) THEN
          WRITE(6,*) 'SNOW : DATE CHECK ERROR'
          STOP 999
        ENDIF
      ENDIF
C   -----                                                                       
      CALL SETWHT (IMAX,JMAX,DP,IP,GAUL,GAUW,COCOT)
      DO 100 J=1,180
      DO 100 I=1,360
      WORK(I+1,J+1)=SEWA(I,J)
  100 CONTINUE
      DO 200 J=1,180
      WORK(  1,J+1)=WORK(361,J+1)
      WORK(362,J+1)=WORK(  2,J+1)
  200 CONTINUE
      DO 300 I=1,362
      WORK(I,  1)=WORK(I,  2)
      WORK(I,182)=WORK(I,181)
  300 CONTINUE
      CALL INTERP(WORK,GSNW,IMAX,JMAX,DP,IP)
C
      DO 400 J=1,JMAX
        DO 410 I=1,IMAX
          GSNW(I,J)=GSNW(I,J)/100.0
  410   CONTINUE
  400 CONTINUE
C   -----
      CALL MOVERD(GSNW, WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NINFL , IDATE , -1    , 'SURF', 'SEW ',
     2 'SNOW EQUIVALENT WATER           ', 'M               ',
     3 0     , 0     , WRK   , IMAX  , JMAX  , I2    )
      WRITE(6,*) '## SNOW ANALYSIS WAS WRITTEN'
      ENDIF
C   =================================================================           
C   >>>   EOF                                                     <<<           
C   =================================================================           
      WRITE(6,*) '## PREGSM IS NORMAL ENDED'
9001  format(10e15.7)     !shcimsi start
      rdum=0.0
      write(904,9001) ((rdum(i,j,1),i=1,imax),j=1,jmax)
      write(904,9001) ((PS(i,j),i=1,imax),j=1,jmax)
      write(904,9001) ((rdum(i,j,1),i=1,imax),j=1,jmax)
      do k=1,kmax
       write(904,9001) ((GT(i,j,k),i=1,imax),j=1,jmax)
      enddo
      do k=1,kmax
       write(904,9001) ((GU(i,j,k),i=1,imax),j=1,jmax)
      enddo
      do k=1,kmax
       write(904,9001) ((GV(i,j,k),i=1,imax),j=1,jmax)
      enddo
      do k=1,kmax
       write(904,9001) ((GQ(i,j,k),i=1,imax),j=1,jmax)
      enddo
      do k=1,kmax
       write(904,9001) ((rdum(i,j,k),i=1,imax),j=1,jmax)
      enddo
      do k=1,kmax
       write(904,9001) ((rdum(i,j,k),i=1,imax),j=1,jmax)
      enddo                                              !shcimsi end
C                                                                               
      END SUBROUTINE PREGSM1      !shc end
