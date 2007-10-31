PROGRAM TSTRAD
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
  !          RTTOV VERSION 8
  !
  ! Description: This program is the test harness for RTTOV-8. There
  !               are 3 options:
  !               option = 0 to test forward model only
  !               option = 1 to test the full model ie TL/AD/K
  !               option = 2 to test the cloudy radiance output
  !
  ! To run this program you must have the following files
  ! either resident in the same directory or set up as a
  ! symbolic link:
  !   refprof.dat    --    reference profile
  !   prof.dat       --    input profile
  !   input.dat      --    file to select channels and surface emis
  !   rtcoef_platform_id_sensor.dat --  coefficient file to match
  !   the sensor you request in the input dialogue
  ! There are unix scripts available to set up the files above and
  ! run this program (e.g. tstrad_full.scr)
  ! The output is generated in a file called print.dat.
  ! This output can be compared with reference output generated
  ! by the code developers and included with the export package.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date        Comment
  ! -------   ----        -------
  !           25/01/2002     Initial version (R. Saunders)
  !           01/05/2002     Updated for NOAA-17 (R. Saunders)
  !           01/12/2002     New F90 code with structures (P Brunel A Smith)
  !           02/01/2003     Comments added (R Saunders)
  !           10/12/2003     Updated for polarimetric changes (S. English/R.Saunders)
  !           01/04/2004     Updated for chan setup routines (R.Saunders)
  !
  ! Code Description:
  !   Language:          Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  Use rttov_const, only :   &
     nplatforms          ,&
     ninst               ,&
     pi                  ,&
     errorstatus_fatal   ,&
     errorstatus_warning ,&
     errorstatus_success ,&
     platform_name       ,&
     sensor_id_mw        ,&
     inst_name,           &
     npolar_return,      &
     npolar_compute


  Use rttov_types, only : &
     rttov_coef     ,&
     profile_type   ,&
     transmission_Type   ,&
     radiance_type

  Use mod_tstrad
  !
  Use parkind1, Only : jpim     ,jprb
  Implicit None
  !
#include "rttov_errorreport.interface"
#include "rttov_setup.interface"
#include "rttov_setupchan.interface"
#include "rttov_setupindex.interface"
#include "rttov_errorhandling.interface"
#include "rttov_direct.interface"
!!#include "rttov_readcoeffs.interface"
!!#include "rttov_initcoeffs.interface"
#include "rttov_dealloc_coef.interface"
#include "tstrad_tl.interface"
#include "tstrad_ad.interface"
#include "tstrad_k.interface"
  !
  ! Parameter for WV conversion used in all tstrad suite
  Real(Kind=jprb), Parameter :: q_mixratio_to_ppmv  = 1.60771704e+6_JPRB
  !
    type( rttov_coef ), allocatable :: coef(:)         ! coefficients
    type(profile_type), allocatable :: profiles(:)
    type(transmission_type)   :: transmission
    type(radiance_type)  :: radiance
  !
  Integer(Kind=jpim), Allocatable :: instrument(:,:) ! instrument id
  Integer(Kind=jpim), Allocatable :: nchan(:,:) ! number of channels per instrument and profile
  Integer(Kind=jpim), Allocatable :: ifull(:)   ! full test (with TL,AD,K) per instrument
  Integer(Kind=jpim), Allocatable :: nprof(:)   ! number of profiles per instrument
  Integer(Kind=jpim), Allocatable :: nsurf(:)   ! surface id number per instrument
  Real(Kind=jprb), Allocatable :: surfem(:,:)   ! surface input emissivity per channel , instrument
  Integer(Kind=jpim), Allocatable :: ichan(:,:)   ! channel list per instrument
  Real(Kind=jprb), Allocatable :: surfem1(:)   ! surface input emissivity per channel for all profiles
  Integer(Kind=jpim), Allocatable :: ichan1(:)   ! channel list per instrument

  integer(Kind=jpim) :: nbtout
  integer(Kind=jpim) :: nfrequencies
  Integer(Kind=jpim) :: nchannels
  integer(Kind=jpim), Allocatable :: polarisations   (:,:)
  integer(Kind=jpim), Allocatable :: frequencies   (:)
  Integer(Kind=jpim), Allocatable :: channels   (:)
  Integer(Kind=jpim), Allocatable :: lprofiles  (:)
  Real(Kind=jprb),    Allocatable :: emissivity (:)
  Real(Kind=jprb),    Allocatable :: input_emissivity (:)
  logical,            Allocatable :: calcemis  (:)

  Integer(Kind=jpim)              :: coef_errorstatus      ! read coeffs error return code
  Integer(Kind=jpim), Allocatable :: rttov_errorstatus(:)  ! rttov error return code
  Integer(Kind=jpim), Allocatable :: setup_errorstatus(:)  ! setup return code

  ! min and max satellite id for each platform
  Integer(Kind=jpim), dimension(nplatforms) :: max_satid
  Integer(Kind=jpim), dimension(nplatforms) :: min_satid

  ! min and max channel numbers for each instrument

  integer(Kind=jpim), dimension(0:ninst-1) :: max_channel_old
  integer(Kind=jpim), dimension(0:ninst-1) :: max_channel_new
  integer(Kind=jpim), dimension(0:ninst-1) :: max_channel
  integer(Kind=jpim), parameter :: mxchn = 500    ! max number of channels per instruments allowed in one run

  ! polarisations to be computed and returned
  integer(Kind=jpim), Allocatable :: indexout(:)

  ! printing arrays
  real(Kind=jprb), Allocatable :: pr_radcld(:)
  real(Kind=jprb), Allocatable :: pr_trans(:)
  real(Kind=jprb), Allocatable :: pr_emis(:)
  real(Kind=jprb), Allocatable :: pr_trans_lev(:,:)
  real(Kind=jprb), Allocatable :: pr_upclr(:)
  real(Kind=jprb), Allocatable :: pr_dncld(:,:)
  real(Kind=jprb), Allocatable :: pr_refclr(:)
  real(Kind=jprb), Allocatable :: pr_ovcst(:,:)
  integer(Kind=jpim), dimension(1:mxchn) :: pr_pol

  data min_satid / 1, 8, 1, 8, 5, 2, 1, 1, 1, 1, 1, 1, 3, 2, 1, 1, 1, 1, 0, 0 /
  data max_satid /18,16, 7,12, 5, 3, 1, 2, 3, 1, 1, 2, 4, 2, 1, 1, 1, 1, 0, 0/
  data max_channel_old / 20,    4, 3, 15, 5, 3,    7, 8, 8, 9,&
                   & 24, 2378, 4, 16, 3, 5, 8461,14, 4,22,&
                   & 2,     8, 4, 18, 4, 3,    3,1000, 40, 22, &
                   & 5,  3000, 0,  0, 0/
  data max_channel_new / 20,    4, 3, 15, 5, 3,    4, 8, 8, 9,&
                   & 21, 2378, 4, 16, 3, 5, 8461,14, 4,22,&
                   & 2,     8, 4, 18, 4, 3,    3,1000, 40, 22, &
                   & 5,  3000, 0,  0, 0/

  Character (len=80) :: errMessage
  Character (len=6)  :: NameOfRoutine = 'tstrad'
  Character (len=3)  :: coeff_version = 'old'
  !
  Integer(Kind=jpim) :: Err_Unit        ! Logical error unit (<0 for default)
  Integer(Kind=jpim) :: verbosity_level ! (<0 for default)

  Integer(Kind=jpim) :: nrttovid     ! maximum number of instruments
  Integer(Kind=jpim) :: no_id        ! instrument loop index
  Integer(Kind=jpim) :: nlevels
  Integer(Kind=jpim) :: ios
  integer(Kind=jpim) :: i,pol_id,ich2
  integer(Kind=jpim) :: ichannels, ibtout
  Integer(Kind=jpim) :: j
  Integer(Kind=jpim) :: jjm, ira, jj
  integer(Kind=jpim) :: jch, jpol
  integer(Kind=jpim) :: jn, joff1, joff2, joff3
  Integer(Kind=jpim) :: nprint
  Integer(Kind=jpim) :: np, ilev
  Integer(Kind=jpim) :: n
  Integer(Kind=jpim) :: nch ! intermediate variable
  Integer(Kind=jpim) :: ich ! intermediate variable
  Integer(Kind=jpim) :: ii ! intermediate variable
  Integer(Kind=jpim) :: errorstatus
  Real(Kind=jprb) :: s
  Real(Kind=jprb) :: zenang
  Real(Kind=jprb) :: azang
  logical :: lcloud

  Integer(Kind=jpim) :: iua
  Integer(Kind=jpim) :: ioout
  Integer(Kind=jpim) :: iue

  ! Unit numbers for input/output
  DATA IUA/1/,IOOUT/2/,IUE/56/

  Integer(Kind=jpim) :: alloc_status(40)


  !- End of header --------------------------------------------------------


  errorstatus     = 0
  alloc_status(:) = 0

  !Initialise error management with default value for
  ! the error unit number and
  ! Fatal error message output
  Err_unit = -1
  verbosity_level = 1
  ! All error message output
  verbosity_level = 3
  call rttov_errorhandling(Err_unit, verbosity_level)

  ! Beginning of Routine.
  ! ---------------------

  OPEN(IOOUT,file='print.dat',status='unknown',form='formatted')

  ! For more than one satellite
  ! comment out the next line and uncomment the following two.

  NRTTOVID  = 1

  !      PRINT *, 'How many satellites do you want?'
  !      READ  *, NRTTOVID

  allocate (coef(nrttovid),stat= alloc_status(1))

  allocate (instrument(3,nrttovid),stat= alloc_status(2))
  allocate (ifull(nrttovid),stat= alloc_status(4))
  allocate (nprof(nrttovid),stat= alloc_status(5))
  allocate (nsurf(nrttovid),stat= alloc_status(6))

  !maximum number of channels allowed for one instrument is mxchn
  allocate (surfem(mxchn,nrttovid),stat= alloc_status(7))
  allocate (ichan (mxchn,nrttovid),stat= alloc_status(8))
  If( any(alloc_status /= 0) ) then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem allocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Stop
  End If

  surfem(:,:) = 0.0_JPRB
  ichan(:,:) = 0

  DO NO_ID = 1, NRTTOVID

     write(*,*)  'Which satellite platform do you want?'
     WRITE(*,'(4(2x,i3,2x,a8))') (i,platform_name(i), i = 1, nplatforms)
     READ  *,  Instrument(1,no_id)
     IF ( Instrument(1,no_id) <= 0  .OR. &
          &    Instrument(1,no_id) > nplatforms)  STOP 'Platform number not allowed'

     WRITE(*,*)  'Which satellite id do you want for this platform?'
     WRITE(*,*)  'Noaaxx = xx  GOESyy = yy'
     READ  *,  instrument(2,no_id)

     if( instrument(2,no_id) < min_satid(Instrument(1,no_id)) .or. &
          & instrument(2,no_id) > max_satid(Instrument(1,no_id))    ) &
          & STOP 'Satellite id not allowed'

     WRITE(*,*)  'Which instrument type do you want for this satellite?'
     write(*, '(4(2x,i3,2x,a8))') (i, inst_name(i),  i = 0, ninst-1)

     READ  *,  instrument(3,no_id)
     IF ( instrument(3,no_id) < 0 .OR. &
          &    instrument(3,no_id) > ninst-1)&
          &   STOP 'instrument number not allowed'

     WRITE(*,*) ' Forward model only (0) or full gradient test (1)',&
          &  ' or full radiance output (2)?'
     READ  *, IFULL(no_id)
     PRINT  *, ' Number of profiles to test code? '
     READ  *, NPROF(no_id)
     PRINT  *, ' Surface type (0=land, 1=sea, 2=ice/snow)? '
     READ  *, NSURF(no_id)
     !
     !..SET UP CHANNEL NUMBERS
     !
     ! .. DEFAULT MAXIMUMS
     if (coeff_version == 'old') max_channel(:)=max_channel_old(:)
     if (coeff_version == 'new') max_channel(:)=max_channel_new(:)
     allocate (nchan(nprof(no_id),nrttovid),stat= alloc_status(3))
     nchan(1:nprof(no_id),no_id) = max_channel(instrument(3,no_id))
     !
     ! Note that channels are the same for all instruments
     ! and all profiles because the filename is the same
     OPEN (IUE,FILE='input.dat',status='old')
     READ(IUE,*)
     NCH = 0
     DO ICH = 1 , nchan(1,no_id)
        READ(IUE,*,iostat=ios)I,II,S
        if(ios /= 0 ) then
           write (*,*) ' TOO FEW CHANNELS IN INPUT FILE '
           write (*,*) ' nchan(1,no_id),no_id  ',nchan(1,no_id),no_id
           stop
        endif
        IF(II.GT.0)THEN
           NCH = NCH + 1
           ICHAN(nch,no_id)  = I
           SURFEM(nch,no_id) = s
        ENDIF
     ENDDO
     !
     CLOSE(IUE)

     ! nchan(1,no_id) is now the real number of channels selected
     do j = 1 , nprof(no_id)
       nchan(j,no_id) = MIN(max_channel(instrument(3,no_id)),NCH)
     enddo
     write(6,*)' Number of channels selected = ',nchan(1,no_id)
     allocate (ichan1(nchan(1,no_id)),stat= alloc_status(8))
     ichan1(1:nchan(1,no_id)) = ichan(1:nchan(1,no_id),no_id)
     !
     !---------------------------------------------------------
     ! Beginning of rttov_readcoeffs test
     !---------------------------------------------------------
!!$     call rttov_readcoeffs  (coef_errorstatus, coef(no_id), instrument(:,no_id),&
!!$     call rttov_initcoeffs  (coef_errorstatus, coef(no_id))
!!$          &  channels = ichan(1:nchan(1,no_id) ,no_id) )
!!$
!!$     if(coef_errorstatus /= errorstatus_success ) then
!!$        write ( ioout, * ) 'rttov_readcoeffs fatal error'
!!$        stop
!!$     endif
!!$
!!$     if( any(coef(no_id)%ff_val_chn( 1 : coef(no_id)%fmv_chn ) /= 1 )) then
!!$        WRITE(*,*) ' some requested channels have bad validity parameter'
!!$        do i = 1, nchan(1,no_id)
!!$           write(*,*) i, coef(no_id)%ff_val_chn(i)
!!$        end do
!!$     endif
     !---------------------------------------------------------
     ! End of rttov_readcoeffs test
     !---------------------------------------------------------
  END DO

  !---------------------------------------------------------
  ! Beginning of rttov_setup test
  !---------------------------------------------------------
  allocate ( setup_errorstatus(nrttovid),stat= alloc_status(1))
  If( any(alloc_status /= 0) ) then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem allocation error for errorsetup")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Stop
  End If
  Call rttov_setup (&
     & setup_errorstatus, & ! out
     & Err_unit,          & ! in
     & verbosity_level,   & ! in
     & nrttovid,          & ! in
     & coef,              & ! out
     & instrument,        & ! in
     & ichan              ) ! in Optional

  if(any(setup_errorstatus(:) /= errorstatus_success ) ) then
     write ( ioout, * ) 'rttov_setup fatal error'
     stop
  endif

  deallocate( setup_errorstatus ,stat=alloc_status(1))
  If( any(alloc_status /= 0) ) then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem deallocation error for setup_errorstatus")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Stop
  End If

  DO no_id = 1, NRTTOVID
     if( any(coef(no_id)%ff_val_chn( : ) /= 1 )) then
      WRITE(*,*) ' some requested channels have bad validity parameter'
        do i = 1, nchan(1,no_id)
           write(*,*) i, coef(no_id)%ff_val_chn(i)
        end do
     endif
  End Do
  !---------------------------------------------------------
  ! End of rttov_setup test
  !---------------------------------------------------------
  !
  DO no_id = 1, NRTTOVID
     ! Set up various channel numbers required by RTTOV-8
     Call rttov_setupchan(nprof(no_id),nchan(1:nprof(no_id),no_id),coef(no_id),nfrequencies, &
     & nchannels,nbtout)

     ! total number of channels
     nlevels =  coef(no_id) % nlevels

     ! Memory allocation for RTTOV_Direct
     !-----------------------------------
     allocate( channels ( nfrequencies ) ,stat= alloc_status(1))
     allocate( rttov_errorstatus(nprof(no_id)),stat= alloc_status(1))
     allocate( profiles(nprof(no_id)),stat= alloc_status(2))
     allocate (surfem1(nchannels),stat= alloc_status(7))
     allocate( polarisations(nchannels,3),stat= alloc_status(1))
     allocate( frequencies(nbtout),stat= alloc_status(2))
     allocate( indexout(nbtout),stat= alloc_status(3))
     If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem allocation error")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Stop
     End If

     do j = 1, nprof(no_id)
        ! allocate model profiles atmospheric arrays with model levels dimension
        profiles(j) % nlevels =  coef(no_id) % nlevels
        allocate( profiles(j) % p  ( coef(no_id) % nlevels ) ,stat= alloc_status(4))
        allocate( profiles(j) % t  ( coef(no_id) % nlevels ) ,stat= alloc_status(5))
        allocate( profiles(j) % q  ( coef(no_id) % nlevels ) ,stat= alloc_status(6))
        allocate( profiles(j) % o3 ( coef(no_id) % nlevels ) ,stat= alloc_status(7))
        allocate( profiles(j) % clw( coef(no_id) % nlevels ) ,stat= alloc_status(8))
        profiles(j) % p(:) = coef(no_id) % ref_prfl_p(:)
        If( any(alloc_status /= 0) ) then
           errorstatus = errorstatus_fatal
           Write( errMessage, '( "mem allocation error")' )
           Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
           Stop
        End If
     end do

     ! number of channels per RTTOV call is only nchannels
     allocate( lprofiles  ( nfrequencies )    ,stat= alloc_status(9))
     allocate( emissivity ( nchannels )       ,stat= alloc_status(10))
     allocate( input_emissivity ( nchannels ) ,stat= alloc_status(11))
     allocate( calcemis  ( nchannels )        ,stat= alloc_status(12))

     ! allocate transmittance structure
     allocate( transmission % tau_surf      ( nchannels ) ,stat= alloc_status(13))
     allocate( transmission % tau_layer     ( coef(no_id) % nlevels, nchannels ) ,stat= alloc_status(14))
     allocate( transmission % od_singlelayer( coef(no_id) % nlevels, nchannels  ),stat= alloc_status(15))

     ! allocate radiance results arrays with number of channels
     allocate( radiance % clear    ( nchannels ) ,stat= alloc_status(19))
     allocate( radiance % cloudy   ( nchannels ) ,stat= alloc_status(20))
     allocate( radiance % total    ( nchannels ) ,stat= alloc_status(21))
     allocate( radiance % bt       ( nchannels ) ,stat= alloc_status(22))
     allocate( radiance % bt_clear ( nchannels ) ,stat= alloc_status(23))
     allocate( radiance % upclear  ( nchannels ) ,stat= alloc_status(24))
     allocate( radiance % dnclear  ( nchannels ) ,stat=alloc_status(25))
     allocate( radiance % reflclear( nchannels ) ,stat= alloc_status(26))
     allocate( radiance % overcast ( coef(no_id) % nlevels, nchannels ) ,stat= alloc_status(27))

     ! allocate the cloudy radiances with full size even if not used
     ! Save input values of emissivities for all calculations.
     allocate( radiance % downcld  ( coef(no_id) % nlevels, nchannels ) ,stat= alloc_status(28))
     allocate( radiance % out       ( nbtout ) ,stat= alloc_status(29))
     allocate( radiance % out_clear( nbtout ) ,stat= alloc_status(30))
     allocate( radiance % total_out( nbtout ) ,stat= alloc_status(31))
     allocate( radiance % clear_out( nbtout ) ,stat= alloc_status(32))

     Allocate(pr_radcld(nbtout) ,stat= alloc_status(33))
     Allocate(pr_trans(nbtout) ,stat= alloc_status(34))
     Allocate(pr_emis(nbtout) ,stat= alloc_status(35))
     Allocate(pr_trans_lev(coef(no_id) % nlevels,nbtout) ,stat= alloc_status(36))
     Allocate(pr_upclr(nbtout) ,stat= alloc_status(37))
     Allocate(pr_dncld(coef(no_id) % nlevels,nbtout) ,stat= alloc_status(38))
     Allocate(pr_refclr(nbtout) ,stat= alloc_status(39))
     Allocate(pr_ovcst(coef(no_id) % nlevels,nbtout) ,stat= alloc_status(40))

     If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem allocation error")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Stop
     End If

     WRITE(6,*)'Zenith angle (degrees)?'
     READ(5,*)ZENANG
     WRITE(6,*)'Azimuth angle (degrees)?'
     READ(5,*)AZANG

     WRITE(6,*)' Number of level =',NLEVELS
     ! Read profile ONE and fill other profiles with profile one
     OPEN (IUA,FILE='prof.dat',status='old')
     !
     JJM = 0
     DO  IRA = 1 , 1+(NLEVELS-1)/10
        JJ = 1+JJM
        JJM = MIN(JJ+9,NLEVELS)
        READ(IUA,*) (profiles(1) % t(J),J=JJ,JJM)
     end do
     JJM = 0
     DO  IRA = 1 , 1+(NLEVELS-1)/10
        JJ = 1+JJM
        JJM = MIN(JJ+9,NLEVELS)
        READ(IUA,*) (profiles(1) % q(J),J=JJ,JJM)
     end do
     JJM = 0
     DO  IRA = 1 , 1+(NLEVELS-1)/10
        JJ = 1+JJM
        JJM = MIN(JJ+9,NLEVELS)
        READ(IUA,*) (profiles(1) % o3(J),J=JJ,JJM)
     end do
     profiles(1) % ozone_data = .true.
     profiles(1) % co2_data   = .false.

     JJM = 0
     DO  IRA = 1 , 1+(NLEVELS-1)/10
        JJ = 1+JJM
        JJM = MIN(JJ+9,NLEVELS)
        READ(IUA,*) (profiles(1) % clw(J),J=JJ,JJM)
     end do
     ! check value of first level
     profiles(1) % clw_data = profiles(1) % clw(1) >= 0.0_JPRB

     READ(IUA,*) profiles(1) % s2m % t ,&
          & profiles(1) % s2m % q ,&
          & profiles(1) % s2m % p ,&
          & profiles(1) % s2m % u ,&
          & profiles(1) % s2m % v


     READ(IUA,*) profiles(1) % skin % t ,&
          & profiles(1) % skin % fastem

     READ(IUA,*) profiles(1) % ctp,&
          & profiles(1) % cfraction
     !
     CLOSE(IUA)
     !
     WRITE(IOOUT,*)' INPUT PROFILE'
     WRITE(IOOUT,444) (profiles(1) %   t(JJ) ,JJ=1,NLEVELS)
     WRITE(IOOUT,444) (profiles(1) %   q(JJ) ,JJ=1,NLEVELS)
     WRITE(IOOUT,444) (profiles(1) %  o3(JJ) ,JJ=1,NLEVELS)
     WRITE(IOOUT,444) (profiles(1) % clw(JJ) ,JJ=1,NLEVELS)
     WRITE(IOOUT,444) profiles(1) % s2m % t ,&
          & profiles(1) % s2m % q ,&
          & profiles(1) % s2m % p ,&
          & profiles(1) % s2m % u ,&
          & profiles(1) % s2m % v
     WRITE(IOOUT,444) profiles(1) % skin % t ,&
          & profiles(1) % skin % fastem
     WRITE(IOOUT,444) profiles(1) % ctp,&
          & profiles(1) % cfraction
     WRITE(IOOUT,*)' '

     !  Convert lnq to q in ppmv for profile
     !
     profiles(1) %    q(:) = (exp(profiles(1) %     q(:)) / 1000._JPRB) * q_mixratio_to_ppmv
     profiles(1) % s2m % q = (exp(profiles(1) %  s2m % q) / 1000._JPRB) * q_mixratio_to_ppmv

     ! Keep Ozone in ppmv

     ! viewing geometry
     profiles(1)  % zenangle      = ZENANG
     profiles(1)  % azangle       = AZANG
     ! surface type
     profiles(1)  % skin % surftype = nsurf(no_id)

     !.. Fill profile arrays with the 1 profile NPROF times
     DO  J = 1 , NPROF(no_id)
        profiles(j) % p(:)   = profiles(1) % p(:)
        profiles(j) % t(:)   = profiles(1) % t(:)
        profiles(j) % q(:)   = profiles(1) % q(:)
        profiles(j) % o3(:)  = profiles(1) % o3(:)
        profiles(j) % clw(:) = profiles(1) % clw(:)
        profiles(j) % s2m    = profiles(1) % s2m
        profiles(j) % skin   = profiles(1) % skin
        profiles(j) % ctp    = profiles(1) % ctp
        profiles(j) % cfraction   = profiles(1) % cfraction
        profiles(j) % ozone_data  = profiles(1) % ozone_data
        profiles(j) % co2_data    = profiles(1) % co2_data
        profiles(j) % clw_data    = profiles(1) % clw_data
        profiles(j) % zenangle    = profiles(1) % zenangle
        profiles(j) % azangle     = profiles(1) % azangle
     end do

     ! Build the list of channels/profiles indices
     surfem1(:) = 0.0_JPRB
     nch = 1
     do j = 1 , nprof(no_id)
       surfem1(nch:nch+nchan(j,no_id)-1) = surfem(1:nchan(1,no_id),no_id)  !Assume emissivities same as first profile
       nch = nch+nchan(j,no_id)
     enddo
     nch = 0
     Call rttov_setupindex (nchan(1:nprof(no_id),no_id),nprof(no_id),nfrequencies,nchannels,nbtout,coef(no_id),&
     & surfem1,lprofiles,channels,polarisations,emissivity)
     !
     nch = 0
     ibtout=0
     DO  J=1,NPROF(no_id)
        DO  JCH=1,NCHAN(1,no_id)
           nch = nch +1
           If( coef(no_id) % id_sensor /= sensor_id_mw) then
              frequencies(ibtout+1) = nch
              ibtout=ibtout+1
           End If
           If( coef(no_id) % id_sensor == sensor_id_mw) then
              pol_id = coef(no_id) % fastem_polar(jch) + 1
              Do i=1, npolar_return(pol_id)
                 frequencies(ibtout+i)=nch
              End Do
             ibtout=ibtout+npolar_return(pol_id)
           End If
        End Do
     End Do
     write(6,*)' nfreq=',nfrequencies,' nchannels=',nchannels,' nbtout=',nbtout
     !write(6,*)' Channels ',(channels(ich2),ich2=1,nfrequencies)
     !write(6,*)(polarisations(ich2,1),ich2=1,nchannels)
     !write(6,*)(polarisations(ich2,2),ich2=1,nchannels)
     !write(6,*)(polarisations(ich2,3),ich2=1,nchannels)

     ! save input values of emissivities for all calculations
     ! calculate emissivity where the input emissivity value is less than 0.01
     input_emissivity(:) = emissivity(:)
     calcemis(:) = emissivity(:) < 0.01_JPRB

     WRITE(IOOUT,*)' NUMBER OF PROFILES PROCESSED=',NPROF(no_id)
     WRITE(IOOUT,*)' '
     !
     WRITE(IOOUT,*)'CHANNELS PROCESSED:'
     WRITE(IOOUT,111) (ichan(J,no_id), J = 1,NCHAN(1,no_id))
     WRITE (IOOUT,*)' '
     WRITE(IOOUT,*)'INPUT SURFACE EMISSIVITIES '&
          &      ,'SAT =', instrument(2,no_id)
     JOFF1=0
     WRITE(IOOUT,444) (emissivity(J+JOFF1),J=1,NCHAN(1,no_id))
     WRITE(IOOUT,*)' '

     IF(IFULL(no_id).EQ.2)THEN
        LCLOUD =.TRUE.
     ELSE
        LCLOUD =.FALSE.
        radiance % downcld(:,:) = 0._JPRB
     ENDIF
     !     PERFORM RADIATIVE TRANSFER CALCULATIONS
     call rttov_direct(  &
          rttov_errorstatus,  & ! out
          nfrequencies, & ! in
          nchannels,    & ! in
          nbtout,       & ! in
          nprof(no_id), & ! in
          channels,     & ! in
          polarisations,& ! in
          lprofiles,    & ! in
          profiles,     & ! in
          coef(no_id),  & ! in
          lcloud,       & ! in
          calcemis,     & ! in
          emissivity,   & ! inout
          transmission, & ! out
          radiance  )     ! inout

     If ( any( rttov_errorstatus(:) == errorstatus_warning ) ) Then
        Do j = 1, nprof(no_id)
           If ( rttov_errorstatus(j) == errorstatus_warning ) Then
              write ( ioout, * ) 'rttov warning for profile',j
           End If
        End Do
     End If

     If ( any( rttov_errorstatus(:) == errorstatus_fatal ) ) Then
        Do j = 1, nprof(no_id)
           If ( rttov_errorstatus(j) == errorstatus_fatal ) Then
              write ( ioout, * ) 'rttov error for profile',j
           End If
        End Do
        Stop
     End If

     ! transfer data to printing arrays
     pr_pol(:) = 0
     pr_radcld(:) = 0.0_JPRB
     pr_trans(:) = 0.0_JPRB
     pr_emis(:) = 0.0_JPRB
     pr_trans_lev(:,:) = 0.0_JPRB
     pr_upclr(:) = 0.0_JPRB
     pr_dncld(:,:) = 0.0_JPRB
     pr_refclr(:) = 0.0_JPRB
     pr_ovcst(:,:) = 0.0_JPRB
     !
     do j = 1 , nchannels
       jpol = polarisations(j,2)
       if (nbtout == nchannels) then
          jpol = j
       endif
       pr_pol(jpol) = jpol
       pr_radcld(jpol) = radiance % cloudy(j)
       pr_trans(jpol) = Transmission % tau_surf(J)
       pr_emis(jpol) = emissivity(j)
       pr_upclr(jpol) = radiance % upclear(J)
       pr_refclr(jpol) = radiance % reflclear(J)
       do ilev = 1 , nlevels
         pr_trans_lev(ilev,jpol) = Transmission % tau_layer(ilev,J)
         pr_dncld(ilev,jpol) = radiance % downcld(ILEV,J)
         pr_ovcst(ilev,jpol) = radiance % overcast(ILEV,J)
       enddo
     enddo

     !     OUTPUT RESULTS
     !
     NPRINT = 1+ INT((nbtout-1)/(10*nprof(no_id)))
     DO  JN=1,NPROF(no_id)
        WRITE(IOOUT,*)' -----------------'
        WRITE(IOOUT,*)' Profile number ',JN, 'Instrument ',&
             &                    instrument(3,no_id)
        WRITE(IOOUT,*)' -----------------'
        WRITE(IOOUT,*)' '
!        JOFF=NCHAN(no_id)*(JN-1)
        JOFF1=nbtout/nprof(no_id)*(JN-1)
        JOFF2=nbtout/nprof(no_id)*(JN-1)
        JOFF3=nfrequencies/nprof(no_id)*(JN-1)
        WRITE(IOOUT,777)instrument(2,no_id), profiles(jn)%zenangle,profiles(jn)%azangle,profiles(jn)%skin%surftype
        WRITE(IOOUT,222) (radiance % out(J+JOFF1),J=1,nbtout/nprof(no_id))
        WRITE(IOOUT,*)' '
        WRITE(IOOUT,*)'CALCULATED RADIANCES: SAT =', instrument(2,no_id)
        WRITE(IOOUT,222) (radiance % total_out(J+JOFF1),J=1,nbtout/nprof(no_id))
        WRITE(IOOUT,*)' '
        WRITE(IOOUT,*)'CALCULATED OVERCAST RADIANCES: SAT =', instrument(2,no_id)
        WRITE(IOOUT,222) (pr_radcld(J+JOFF2),J=1,nbtout/nprof(no_id))
        WRITE (IOOUT,*)' '
        WRITE(IOOUT,*)'CALCULATED SURFACE TO SPACE TRANSMITTANCE: S'&
             &      ,'AT =',instrument(2,no_id)
        WRITE(IOOUT,4444) (pr_trans(J+JOFF2),J=1,nbtout/nprof(no_id))
        WRITE (IOOUT,*)' '
        WRITE(IOOUT,*)'CALCULATED SURFACE EMISSIVITIES '&
             &      ,'SAT =',instrument(2,no_id)
        WRITE(IOOUT,444) (pr_emis(J+JOFF2),J=1,nbtout/nprof(no_id))
        !
        !  Print  clear-sky radiance without reflection term and
        !  reflected clear-sky downwelling radiance
        !
        IF(IFULL(no_id) == 2 .AND. nchan(1,no_id) <= 20 )THEN
           WRITE (IOOUT,*)' '
           WRITE(IOOUT,*)'CALCULATED Clear-sky radiance without reflection term'&
                &      ,' SAT =',instrument(2,no_id)
           WRITE(IOOUT,444)(pr_upclr(J+JOFF2),J=1,nbtout/nprof(no_id))
           WRITE (IOOUT,*)' '
           WRITE (IOOUT,*)'CALCULATED Reflected clear-sky downwelling radiance'&
                &      ,' SAT =',instrument(2,no_id)
           WRITE(IOOUT,444)(pr_refclr(J+JOFF2),J=1,nbtout/nprof(no_id))
           WRITE (IOOUT,*)'CHANNELS '
           WRITE(IOOUT,111) (ichan(j,no_id), J = 1,nbtout/nprof(no_id))
        ENDIF
        !
        IF(JN.EQ.1 .AND. nchan(1,no_id) .LE. 20)THEN
           DO  NP = 1 , NPRINT
              WRITE (IOOUT,*)' '
              WRITE (IOOUT,*)'Level to space transmittances for channels'
!              WRITE(IOOUT,1115)(pr_pol(j),&
              WRITE(IOOUT,1115) (ICHAN(J,no_id),&
                   &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,nbtout/nprof(no_id)))
              DO  ILEV = 1 , NLEVELS
                 WRITE(IOOUT,4445)ILEV,(pr_trans_lev(ilev,J+JOFF2),&
                      &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,nbtout/nprof(no_id)))
              end do
              WRITE(IOOUT,1115) (ICHAN(J,no_id),&
                      &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,nbtout/nprof(no_id)))
           end do
        ENDIF
        !
        !  Print radiance upwelling arrays
        IF(JN==1 .AND. IFULL(no_id)==2 .AND. nchan(1,no_id)<=20)THEN
           DO  NP = 1 , NPRINT
              WRITE (IOOUT,*)' '
              WRITE (IOOUT,*)'Level to space upwelling radiances for channels'
              WRITE(IOOUT,1115) (ICHAN(J,no_id),&
                   &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,nbtout/nprof(no_id)))
              DO  ILEV = 1 , NLEVELS
                 WRITE(IOOUT,4446)ILEV,(pr_ovcst(ILEV,J+JOFF2),&
                      &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,nbtout/nprof(no_id)))
              end do
              WRITE(IOOUT,1115) (ICHAN(J,no_id),&
                   &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,nbtout/nprof(no_id)))
           end do
        ENDIF
        !  Print radiance downwelling arrays
        IF(JN==1 .AND. IFULL(no_id)==2 .AND. nchan(1,no_id)<=20)THEN
           DO  NP = 1 , NPRINT
              WRITE (IOOUT,*)' '
              WRITE (IOOUT,*)'Level to space downwelling radiances for channels'
              WRITE(IOOUT,1115) (ICHAN(J,no_id),&
                   &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,nbtout/nprof(no_id)))
              DO  ILEV = 1 , NLEVELS
                 WRITE(IOOUT,4446)ILEV,(pr_dncld(ILEV,J+JOFF2),&
                      &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,nbtout/nprof(no_id)))
              end do
              WRITE(IOOUT,1115) (ICHAN(J,no_id),&
                   &   J = 1+(NP-1)*10,MIN(10+(NP-1)*10,nbtout/nprof(no_id)))
           end do
        ENDIF
     end do
     WRITE(*,*) ' FORWARD MODEL FINISHED'
     !
     IF (IFULL(no_id).GE.1)THEN
        !
        !-----------------------------------------------------------
        ! Test tangent linear
        !-----------------------------------------------------------
        write(*,*) 'Tangent linear'

        call TSTRAD_TL(  &
             nfrequencies,   & ! in
             nchannels,      & ! in
             nbtout,         & ! in
             nprof(no_id),   & ! in
             channels,       & ! in
             polarisations,  & ! in
             lprofiles,      & ! in
             frequencies,    & ! in
             profiles,       & ! in
             coef(no_id),    & ! in
             lcloud,         & ! in
             calcemis,       & ! in
             input_emissivity) ! in

        write(*,*) 'Adjoint'

        call TSTRAD_AD(  &
             nfrequencies,   & ! in
             nchannels,      & ! in
             nbtout,         & ! in
             nprof(no_id),   & ! in
             channels,       & ! in
             polarisations,  & ! in
             frequencies,    & ! in
             lprofiles,      & ! in
             profiles,       & ! in
             coef(no_id),    & ! in
             lcloud,         & ! in
             calcemis,       & ! in
             input_emissivity) ! in

        write(*,*) 'K'

        call TSTRAD_K(  &
             nfrequencies,   & ! in
             nchannels,      & ! in
             nbtout,         & ! in
             nprof(no_id),   & ! in
             channels,       & ! in
             polarisations,  & ! in
             frequencies,    & ! in
             lprofiles,      & ! in
             profiles,       & ! in
             coef(no_id),    & ! in
             lcloud,         & ! in
             calcemis,       & ! in
             input_emissivity) ! in

     deallocate(xkbav   ,stat= alloc_status(1))
     deallocate(xkradovu,stat= alloc_status(2))
     deallocate(xkradovd,stat= alloc_status(3))
     deallocate(xkradov1,stat= alloc_status(4))
     deallocate(xkradov2,stat= alloc_status(5))
     deallocate(xkbsav  ,stat= alloc_status(6))
     deallocate(xkbem   ,stat= alloc_status(7))
     If( any(alloc_status /= 0) ) then
         errorstatus = errorstatus_fatal
         Write( errMessage, '( "mem deallocation error")' )
         Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
         Stop
     End If

     ENDIF

     do j = 1, nprof(no_id)
        ! deallocate model profiles atmospheric arrays
        deallocate( profiles(j) % p   ,stat=alloc_status(1))
        deallocate( profiles(j) % t   ,stat=alloc_status(2))
        deallocate( profiles(j) % q   ,stat=alloc_status(3))
        deallocate( profiles(j) % o3  ,stat=alloc_status(4))
        deallocate( profiles(j) % clw ,stat=alloc_status(5))
        If( any(alloc_status /= 0) ) then
           errorstatus = errorstatus_fatal
           Write( errMessage, '( "mem deallocation error")' )
           Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
           Stop
        End If
     end do
     deallocate( profiles,stat=alloc_status(1))

     ! number of channels per RTTOV call is only nchannels
     deallocate( channels   ,stat=alloc_status(2))
     deallocate( lprofiles  ,stat=alloc_status(3))
     deallocate( emissivity ,stat=alloc_status(4))
     deallocate( calcemis   ,stat=alloc_status(5))

     ! allocate transmittance structure
     deallocate( transmission % tau_surf      ,stat= alloc_status(6))
     deallocate( transmission % tau_layer     ,stat= alloc_status(7))
     deallocate( transmission % od_singlelayer,stat= alloc_status(8))

     ! allocate radiance results arrays with number of channels
     deallocate( radiance % clear    ,stat=alloc_status(9))
     deallocate( radiance % cloudy   ,stat=alloc_status(10))
     deallocate( radiance % total    ,stat=alloc_status(11))
     deallocate( radiance % bt       ,stat=alloc_status(12))
     deallocate( radiance % bt_clear ,stat=alloc_status(13))
     deallocate( radiance % upclear  ,stat=alloc_status(14))
     deallocate( radiance % dnclear  ,stat=alloc_status(15))
     deallocate( radiance % reflclear,stat=alloc_status(16))
     deallocate( radiance % overcast ,stat=alloc_status(17))
     deallocate( radiance % downcld  ,stat=alloc_status(18))
     deallocate( radiance % out       ,stat= alloc_status(19))
     deallocate( radiance % out_clear ,stat= alloc_status(20))
     deallocate( radiance % total_out ,stat= alloc_status(21))
     deallocate( radiance % clear_out ,stat= alloc_status(22))
     deallocate(pr_radcld ,stat= alloc_status(31))
     deallocate(pr_trans ,stat= alloc_status(32))
     deallocate(pr_emis ,stat= alloc_status(33))
     deallocate(pr_trans_lev ,stat= alloc_status(34))
     If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem deallocation error")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Stop
     End If
  ENDDO

  Do no_id = 1, nrttovid
     Call rttov_dealloc_coef (errorstatus, coef(no_id))
     If(errorstatus /= errorstatus_success) Then
        Write( errMessage, '( "deallocation error")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Endif
  End Do

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

END PROGRAM TSTRAD
