PROGRAM TSTRAD_RTTOV7
  !
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2002, EUMETSAT, All Rights Reserved.
  !
  !     *************************************************************
  !
  !     TEST PROGRAM FOR RTTOV SUITE.
  !          RTTOV VERSION 7
  !
  ! Description: This program is the test harness for RTTOV-7. There
  !               are 3 options one to test only the forward model
  !               (option = 0) one to test the full model ie TL/AD/K
  !               (option=1) and one to test the cloudy radiance
  !                output (option=2)
  !
  !
  ! To run this program you must have the following files
  ! either resident in the same directory or set up as a
  ! symbolic link:
  !   refprof.dat    --    reference profile
  !   prof.dat       --    input profile
  !   input.dat      --    file to select channels and surface emis
  !   rtcoef_platform_id_sensor.dat --  coefficient file
  !                                     (depends on which sensor)
  ! There are unix scripts available to set up the files above and
  ! run this program (e.g. tstrad_all.scr)
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date        Comment
  ! -------   ----        -------
  !           25/01/2002     Initial version (R. Saunders)
  !           01/05/2002     Updated for NOAA-17 (R. Saunders)
  !           29/03/2005     Add end of header (J. Cameron)
  !           29/03/2005     Add implicit none (J. Cameron)
  !
  ! Code Description:
  !   Language:          Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  Use parkind1, Only : jpim     ,jprb

  USE MOD_CPARAM, ONLY : &
       ! Imported Paramters:
       jpnsat ,&     ! Total max sats to be used
       jplev  ,&     ! No. of pressure levels
       jpnav  ,&     ! No. of profile variables
       jpnsav ,&     ! No. of surface air variables
       jpnssv ,&     ! No. of skin variables
       jpncv  ,&     ! No. of cloud variables
       jppf   ,&     ! Max no. profiles
       jpch   ,&     ! Max. no. of tovs channels
       jpchus ,&     ! Max. no. of channels used tovs
       jpchpf ,&     ! Max no. of profs * chans used
       rcnv          ! kg/kg--> ppmv

  IMPLICIT NONE

#include "rttvi.interface"
#include "rttov.interface"

  REAL(Kind=jprb):: PPRES(JPLEV),PAV(JPLEV,JPNAV,JPPF),PSAV(JPNSAV,JPPF)
  REAL(Kind=jprb):: PSSV(JPNSSV,JPPF),PCV(JPNCV,JPPF)
  REAL(Kind=jprb):: PEMIS(JPCHPF),PEMIN(JPCHPF)
  REAL(Kind=jprb):: PANGL(JPPF),PANGS(JPPF)
  REAL(Kind=jprb):: PRAD(JPCHPF),PTB(JPCHPF)
  INTEGER(Kind=jpim):: KSURF(JPPF),JERR(JPPF,JPNSAT),JERRTL(JPPF,JPNSAT)
  INTEGER(Kind=jpim):: KCHAN(JPCHPF),KPROF(JPCHPF)
  REAL(Kind=jprb):: UP(JPLEV),UTMX(JPLEV),UTMN(JPLEV),UQMX(JPLEV)
  REAL(Kind=jprb):: UQMN(JPLEV),UOMX(JPLEV),UOMN(JPLEV)
  !
  integer(Kind=jpim), parameter :: max_platform=13   ! max platform number
  integer(Kind=jpim), parameter :: max_instrument=26 ! max instrument number
  integer(Kind=jpim), dimension(jpnsat) :: platform   ! platform id
  integer(Kind=jpim), dimension(jpnsat) :: satellite  ! satellite id
  integer(Kind=jpim), dimension(jpnsat) :: instrument ! instrument id
  integer(Kind=jpim), dimension(jpnsat) :: numchans   ! No. of chans required for insrtrument
  integer(Kind=jpim), dimension(jpnsat) :: fileunit   ! Fileunit for coeffs. for insrtrument
  ! min and max satellite id for each platform
  integer(Kind=jpim), dimension(max_platform) :: max_satid
  integer(Kind=jpim), dimension(max_platform) :: min_satid

  ! min and max channel numbers for each instrument
  integer(Kind=jpim), dimension(0:max_instrument) :: max_channel

  INTEGER(Kind=jpim) :: IOOUT, NRTTOVID, NO_ID, IFULL, NPROF, NSURF
  INTEGER(Kind=jpim) :: IUE, NCH, ICH, I, II, IERR
  INTEGER(Kind=jpim) :: KPNSAT, KPLEV, KPCH, KPCHUS, KPNAV, KPNSAV, KPNSSV
  INTEGER(Kind=jpim) :: KPNCV, ICH1, KSAT, KNPF, KNCHPF, KNCHAN, KLENPF, KNAV
  INTEGER(Kind=jpim) :: KNSAV, KNSSV, KNCV, J, JCH, IREF, ILEV, IL, IUA
  INTEGER(Kind=jpim) :: JJM, IRA, JJ, JP, JL, NLEV, JOFF, NPRINT, JN, NP, KPPF
  REAL(Kind=jprb) :: D, AZANG, ZENANG

  data min_satid / 1, 8, 5, 8, 5, 2, 1, 1, 1, 1, 1, 1, 3/
  data max_satid /17,16, 7,12, 5, 2, 1, 2, 3, 1, 1, 1, 4/
  data max_channel / 20, 4, 3, 15, 5, 3, 7, 8, 8, 9, 24,&
       &  2378, 4, 16, 3, 5, 8461,14, 0, 0, 2, 8, 4, 18, 3, 2, 3/

  REAL(Kind=jprb) :: SURFEM(JPCH)
  INTEGER(Kind=jpim) :: ICHAN(jpnsat,JPCH)  ! Max of six series so far
  INTEGER(Kind=jpim) :: NCHAN(jpnsat)
  INTEGER(Kind=jpim) :: IVCH(JPCH,JPNSAT)
  LOGICAL :: LCLOUD
  !
  REAL(Kind=jprb), ALLOCATABLE :: RADOV(:,:),RADO(:)
  REAL(Kind=jprb), ALLOCATABLE :: TAU(:,:),TAUSFC(:)
  !
  DATA NLEV/JPLEV/
  !
  DATA IUA/1/,IOOUT/2/,IREF/55/,IUE/56/

  !- End of header --------------------------------------------------------

  ! Beginning of Routine.
  ! ---------------------
  PAV(:,:,:) = 0.
  PSAV(:,:) = 0.
  PSSV(:,:) = 0.
  PCV(:,:) = 0.
  PEMIS(:) = 0.
  PEMIN(:) = 0.
  numchans(:) = 0
  Fileunit(:) = 0
  IVCH(:,:) = 0

  OPEN(IOOUT,file='print.dat',status='unknown',form='formatted')

  ! For more than one satellite
  ! comment out the next line and uncomment the following two.

  NRTTOVID  = 1

  !      PRINT *, 'How many satellites do you want?'
  !      READ  *, NRTTOVID

  DO NO_ID = 1, NRTTOVID

     PRINT *, 'Which satellite platform do you want?'
     PRINT *, 'NOAA=1  DMSP=2    METEOSAT=3  GOES=4: '
     PRINT *, 'GMS=5   FY-2=6    TRMM=7      ERS=8: '
     PRINT *, 'EOS=9   METOP=10  ENVISAT=11  MSG=12   FY-1=13: '
     READ  *,  Platform(no_id)
     IF ( Platform(no_id) <= 0  .OR. &
     &    Platform(no_id) > max_platform)  STOP 'Platform number not allowed'

     PRINT *, 'Which satellite id do you want for this platform?'
     PRINT *, 'Noaaxx = xx  GOESyy = yy'
     READ  *,  satellite(no_id)

     if( satellite(no_id) < min_satid(Platform(no_id)) .or. &
          & satellite(no_id) > max_satid(Platform(no_id))    ) &
          & STOP 'Satellite id not allowed'

     PRINT *, 'Which instrument type do you want for this satellite?'
     print *, 'HIRS=0,  MSU=1,   SSU=2,   AMSU-A=3, AMSU-B=4, AVHRR=5'
     print *, 'SSMI=6,  VTPR1=7, VTPR2=8, TMI=9,    SSMIS=10, AIRS=11'
     print *, 'HSB=12,  MODIS=13,ATSR=14, MHS=15,   IASI=16,  MVIRI=20'
     print *, 'SEVIRI=21, GOES-imager=22  GOES-sounder=23 '
     print *, 'GMS imager=24     FY2-VISSR=25        FY1-mvisr=26'

     READ  *,  instrument(no_id)
     IF ( instrument(no_id) < 0 .OR. &
     &    instrument(no_id) > max_instrument)&
     &   STOP 'instrument number not allowed'

     PRINT *, ' Forward model only (0) or full gradient test (1)',&
     &  ' or full radiance output (2)?'
     READ  *, IFULL
     IF(IFULL.EQ.2)THEN
       LCLOUD =.TRUE.
     ELSE
       LCLOUD =.FALSE.
     ENDIF
     PRINT  *, ' Number of profiles to test code? '
     READ  *, NPROF
     NPROF = MIN(JPPF, NPROF)
     PRINT  *, ' Surface type (0=land, 1=sea, 2=ice/snow)? '
     READ  *, NSURF
     !
     !..SET UP CHANNEL NUMBERS
     !
     ! .. DEFAULT MAXIMUMS
     nchan(no_id) = max_channel(instrument(no_id))
     !
     OPEN (IUE,FILE='input.dat',status='old')
     READ(IUE,*)
     NCH = 0
     DO ICH = 1 , nchan(no_id)
        READ(IUE,*,END=909)I,II,SURFEM(ICH)
        IF(II.GT.0)THEN
           NCH = NCH + 1
           ICHAN(no_id,NCH) = I
!           IVCH(NCH,no_id) = ICH   ! Normally comment out use to test AIRS
        ENDIF                       ! channel selection option
     ENDDO
     !
     CLOSE(IUE)
     nchan(no_id) = MIN(max_channel(instrument(no_id)),NCH)
     write(6,*)' Number of channels selected = ',nchan(no_id)
     !

  END DO

  CALL RTTVI(IERR,KPPF,KPNSAT,KPLEV,KPCH,KPCHUS,KPNAV,KPNSAV,&
  &  KPNSSV,KPNCV,&
  &  NRTTOVID, platform, satellite, instrument, numchans, &
  &  UP,UTMN,UTMX,UQMN,UQMX,UOMN,&
  &  UOMX,IVCH,FileUnit)
  !

  IF (IERR.NE.0 ) THEN
     WRITE (IOOUT,*) 'RTTVI: IERR =',IERR
     STOP
  ENDIF
  IF (  KPNSAT.NE.JPNSAT .OR.&
     &   KPLEV.NE.JPLEV .OR. KPCH.NE.JPCH .OR. KPCHUS.NE.JPCHUS .OR.&
     &   KPNAV.NE.JPNAV .OR. KPNSAV.NE.JPNSAV .OR. KPNSSV.NE.JPNSSV&
     &   .OR. KPNCV.NE.JPNCV  ) THEN
     WRITE (IOOUT,*) 'Mismatch in cparam.h and tstrad parameters'
     STOP
  ENDIF
  !
  !  Check Channel numbers are correct
  !
  DO ICH = 1 , nchan(1)
     DO ICH1 = 1 , JPCH
        IF (ICHAN(1,ICH) .EQ. IVCH(ICH1,1))GO TO 202
        IF (ICH1 .EQ. JPCH)THEN
           PRINT *,' BAD CHANNEL NUMBER REQUESTED'
           PRINT *,' EDIT input.dat'
           STOP
        ENDIF
     ENDDO
202 ENDDO
  !
  !
  DO no_id = 1, NRTTOVID

           !         Convert from position in required list to KSAT pointer used
           !         in rttov coefficient arrays - for only one satellite KSAT = 1
           !         This reflects the order in which coefficients are loaded in RTTVI

           KSAT = no_id

           KNPF=NPROF
           KNCHPF=0
           KNCHAN=NCHAN(no_id)
           KLENPF=JPLEV
           KNAV = 3
           KNSAV = JPNSAV
           KNSSV = JPNSSV
           KNCV = JPNCV

           WRITE(6,*)'Zenith angle (degrees)?'
           READ(5,*)ZENANG
           WRITE(6,*)'Azimuth angle (degrees)?'
           READ(5,*)AZANG

           DO  J=1,NPROF
              PANGS(J)=0.
              KSURF(J)=NSURF
              PANGL(J) = ZENANG
              DO  JCH=1,NCHAN(no_id)
                 KNCHPF=KNCHPF+1
                 KCHAN(KNCHPF)=ICHAN(no_id,JCH)
                 KPROF(KNCHPF)=J
                 PEMIS(KNCHPF)=SURFEM(KCHAN(KNCHPF))
                 PEMIN(KNCHPF)=SURFEM(KCHAN(KNCHPF))
              end do
           end do
           !
           OPEN(unit=iref,file='refprof.dat',status='old')
           !
           ! Get 43 level pressures
           !
           READ(IREF,*)
           READ(IREF,*)
           DO  ILEV = 1 , jplev
              READ(IREF,*)IL,PPRES(ILEV),D,D,D,D
           end do
           CLOSE(IREF)
           !
           OPEN (IUA,FILE='prof.dat',status='old')
           !
           JJM = 0
           DO  IRA = 1 , 1+(JPLEV-1)/10
              JJ = 1+JJM
              JJM = MIN(JJ+9,JPLEV)
              READ(IUA,*)(PAV(J,1,1),J=JJ,JJM)
           end do
           JJM = 0
           DO  IRA = 1 , 1+(JPLEV-1)/10
              JJ = 1+JJM
              JJM = MIN(JJ+9,JPLEV)
              READ(IUA,*)(PAV(J,2,1),J=JJ,JJM)
           end do
           JJM = 0
           DO  IRA = 1 , 1+(JPLEV-1)/10
              JJ = 1+JJM
              JJM = MIN(JJ+9,JPLEV)
              READ(IUA,*)(PAV(J,3,1),J=JJ,JJM)
           end do
           JJM = 0
           DO  IRA = 1 , 1+(JPLEV-1)/10
              JJ = 1+JJM
              JJM = MIN(JJ+9,JPLEV)
              READ(IUA,*)(PAV(J,4,1),J=JJ,JJM)
           end do
           READ(IUA,*)(PSAV(JJ,1),JJ=1,JPNSAV)
           READ(IUA,*)(PSSV(JJ,1),JJ=1,JPNSSV)
           READ(IUA,*)(PCV(JJ,1),JJ=1,JPNCV)
           !
           CLOSE(IUA)
           !
           WRITE(IOOUT,*)' INPUT PROFILE'
           WRITE(IOOUT,444)(PAV(JJ,1,1),JJ=1,JPLEV)
           WRITE(IOOUT,444)(PAV(JJ,2,1),JJ=1,JPLEV)
           WRITE(IOOUT,444)(PAV(JJ,3,1),JJ=1,JPLEV)
           WRITE(IOOUT,444)(PAV(JJ,4,1),JJ=1,JPLEV)
           WRITE(IOOUT,444)(PSAV(JJ,1),JJ=1,JPNSAV)
           WRITE(IOOUT,444)(PSSV(JJ,1),JJ=1,JPNSSV)
           WRITE(IOOUT,444)(PCV(JJ,1),JJ=1,JPNCV)
           WRITE(IOOUT,*)' '
           !
           !  Convert lnq to q in kg/kg for profile
           !
           DO J = 1 , JPLEV
              PAV(J,2,1) = EXP(PAV(J,2,1))/1000.
           ENDDO
           PSAV(2,1) = EXP(PSAV(2,1))/1000.
           ! convert from ppmv to kg/kg
           DO J = 1 , JPLEV
              PAV(J,3,1) = PAV(J,3,1)/RCNV
           ENDDO
           !
           !.. Fill profile arrays with the 1 profile NPROF times
           DO  JP = 1 , NPROF
              !      IF(PSSV(1,1).LT.271.5)KSURF(JP)=2
              DO  JL = 1 , NLEV
                 PAV(JL,1,JP) = PAV(JL,1,1)
                 PAV(JL,2,JP) = PAV(JL,2,1)
                 PAV(JL,3,JP) = PAV(JL,3,1)
                 PAV(JL,4,JP) = PAV(JL,4,1)
              end do
              PSAV(1,JP) = PSAV(1,1)
              PSAV(2,JP) = PSAV(2,1)
              PSAV(3,JP) = PSAV(3,1)
              PSAV(4,JP) = PSAV(4,1)
              PSAV(5,JP) = PSAV(5,1)
              PSSV(1,JP) = PSSV(1,1)
              PSSV(2,JP) = PSSV(2,1)
              PSSV(3,JP) = PSSV(3,1)
              PSSV(4,JP) = PSSV(4,1)
              PSSV(5,JP) = PSSV(5,1)
              PSSV(6,JP) = PSSV(6,1)
              PCV(1,JP) = PCV(1,1)
              PCV(2,JP) = PCV(2,1)
           end do

           !
           WRITE(IOOUT,*)' NUMBER OF PROFILES PROCESSED=',NPROF
           WRITE(IOOUT,*)' '
           !
           WRITE(IOOUT,*)'CHANNELS PROCESSED:'
           WRITE(IOOUT,111) (ICHAN(no_id,J), J = 1,NCHAN(no_id))
           WRITE (IOOUT,*)' '
           WRITE(IOOUT,*)'INPUT SURFACE EMISSIVITIES '&
           &      ,'SAT =', satellite(no_id)
           JOFF=0
           WRITE(IOOUT,444) (PEMIS(J+JOFF),J=1,NCHAN(no_id))
           WRITE(IOOUT,*)' '
           !
           ALLOCATE(TAU(KNCHPF,JPLEV))
           ALLOCATE(TAUSFC(KNCHPF))
           ALLOCATE(RADOV(KNCHPF,2*JPLEV+2))
           ALLOCATE(RADO(KNCHPF))
           !
           !     PERFORM RADIATIVE TRANSFER CALCULATIONS
           CALL RTTOV(KNPF,KLENPF,PPRES,PANGL, &
           &     PANGS,KSURF,KSAT,KNCHPF,KCHAN,KPROF,&
           &     PAV,PSAV,PSSV,PCV,PEMIS,JERR,PRAD,PTB,RADOV,RADO,TAU,&
           &     TAUSFC,LCLOUD)
           !
           !     OUTPUT RESULTS
           !
           NPRINT = 1+ INT((NCHAN(no_id)-1)/10)
           DO  JN=1,NPROF
              WRITE(IOOUT,*)' -----------------'
              WRITE(IOOUT,*)' Profile number ',JN, 'Instrument ',&
              &                    instrument(no_id)
              WRITE(IOOUT,*)' -----------------'
              IF(JERR(JN,1).NE.0)WRITE(IOOUT,*)' RTTOV ERROR CODE=',JERR(JN,1)
              IF(JERR(JN,1).GE.20)STOP
              WRITE(IOOUT,*)' '
              JOFF=NCHAN(no_id)*(JN-1)
              WRITE(IOOUT,777)satellite(no_id),PANGL(JN),AZANG,KSURF(1)
              WRITE(IOOUT,222) (PTB(J+JOFF),J=1,NCHAN(no_id))
              WRITE(IOOUT,*)' '
              WRITE(IOOUT,*)'CALCULATED RADIANCES: SAT =', satellite(no_id)
              WRITE(IOOUT,222) (PRAD(J+JOFF),J=1,NCHAN(no_id))
              WRITE(IOOUT,*)' '
              WRITE(IOOUT,*)'CALCULATED OVERCAST RADIANCES: SAT =', satellite(no_id)
              WRITE(IOOUT,222) (RADO(J+JOFF),J=1,NCHAN(no_id))
              WRITE (IOOUT,*)' '
              WRITE(IOOUT,*)'CALCULATED SURFACE TO SPACE TRANSMITTANCE: S'&
              &      ,'AT =',satellite(no_id)
              WRITE(IOOUT,4444) (TAUSFC(J+JOFF),J=1,NCHAN(no_id))
              WRITE (IOOUT,*)' '
              WRITE(IOOUT,*)'CALCULATED SURFACE EMISSIVITIES '&
              &      ,'SAT =',satellite(no_id)
              WRITE(IOOUT,444) (PEMIS(J+JOFF),J=1,NCHAN(no_id))
              !
              !  Print  clear-sky radiance without reflection term and
              !  reflected clear-sky downwelling radiance
              !
              IF(IFULL.EQ.2 .AND. nchan(no_id) .LE. 20 )THEN
                WRITE (IOOUT,*)' '
                WRITE(IOOUT,*)'CALCULATED Clear-sky radiance without reflection term'&
              &      ,' SAT =',satellite(no_id)
                WRITE(IOOUT,444)(RADOV(J+JOFF,JPLEV*2+1),J=1,NCHAN(no_id))
                WRITE (IOOUT,*)' '
                WRITE (IOOUT,*)'CALCULATED Reflected clear-sky downwelling radiance'&
              &      ,' SAT =',satellite(no_id)
                WRITE(IOOUT,444)(RADOV(J+JOFF,JPLEV*2+2),J=1,NCHAN(no_id))
                WRITE (IOOUT,*)'CHANNELS '
                WRITE(IOOUT,111) (ICHAN(no_id,J), J = 1,NCHAN(no_id))
              ENDIF
!
              IF(JN.EQ.1 .AND. nchan(no_id) .LE. 20)THEN
                 DO  NP = 1 , NPRINT
                    WRITE (IOOUT,*)' '
                    WRITE (IOOUT,*)'Level to space transmittances for channels'
                    WRITE(IOOUT,1115)(ICHAN(no_id,J),&
                    &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,NCHAN(no_id)))
                    DO  ILEV = 1 , JPLEV
                       WRITE(IOOUT,4445)ILEV,(TAU(J+JOFF,ILEV),&
                       &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,NCHAN(no_id)))
                    end do
                    WRITE(IOOUT,1115)(ICHAN(no_id,J),&
                    &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,NCHAN(no_id)))
                 end do
              ENDIF
              !
              !  Print radiance upwelling arrays
              IF(JN.EQ.1.AND.IFULL.EQ.2 .AND. nchan(no_id) .LE. 20)THEN
                 DO  NP = 1 , NPRINT
                    WRITE (IOOUT,*)' '
                    WRITE (IOOUT,*)'Level to space upwelling radiances for channels'
                    WRITE(IOOUT,1115)(ICHAN(no_id,J),&
                    &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,NCHAN(no_id)))
                    DO  ILEV = 1 , JPLEV
                       WRITE(IOOUT,4446)ILEV,(RADOV(J+JOFF,ILEV),&
                       &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,NCHAN(no_id)))
                    end do
                    WRITE(IOOUT,1115)(ICHAN(no_id,J),&
                    &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,NCHAN(no_id)))
                 end do
              ENDIF
              !  Print radiance downwelling arrays
              IF(JN.EQ.1.AND.IFULL.EQ.2 .AND. nchan(no_id) .LE. 20)THEN
                 DO  NP = 1 , NPRINT
                    WRITE (IOOUT,*)' '
                    WRITE (IOOUT,*)'Level to space downwelling radiances for channels'
                    WRITE(IOOUT,1115)(ICHAN(no_id,J),&
                    &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,NCHAN(no_id)))
                    DO  ILEV = 1 , JPLEV
                       WRITE(IOOUT,4446)ILEV,(RADOV(J+JOFF,ILEV+JPLEV),&
                       &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,NCHAN(no_id)))
                    end do
                    WRITE(IOOUT,1115)(ICHAN(no_id,J),&
                    &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,NCHAN(no_id)))
                 end do
              ENDIF
           end do
           PRINT *,' FORWARD MODEL FINISHED'
           !
  ENDDO


  !
111 FORMAT(1X,10I8)
1115 FORMAT(3X,10I8)
2222 FORMAT(1X,10(1x,F8.6))
222 FORMAT(1X,10F8.2)
333 FORMAT(1X,I3,20I5)
3333 FORMAT(1X,I3,2I5)
444 FORMAT(1X,10F8.3)
4444 FORMAT(1X,10F8.4)
4445 FORMAT(1X,I2,10F8.4)
4446 FORMAT(1X,I2,10F8.3)
555 FORMAT(1X,10E8.2)
777 FORMAT(1X,'CALCULATED BRIGHTNESS TEMPERATURES: SAT =',I2,&
       &' ZENITH ANGLE=',F6.2, &
       &' AZIMUTH ANGLE=',F7.2,' SURFACE TYPE=',I2)
  STOP
909 PRINT *,' TOO FEW CHANNELS IN INPUT FILE '
  STOP
END PROGRAM TSTRAD_RTTOV7
