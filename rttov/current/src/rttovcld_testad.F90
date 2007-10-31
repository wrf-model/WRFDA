PROGRAM rttovcld_testad
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
  !
  ! Description:
  ! 1- read ECMWF profiles on model levels
  ! 2- interpolate the T, q, o3 profiles to the 43-level RTTOV grid
  ! 4- run rttovcld direct model
  ! 5- test TL and AD codes of the rttovcld package
  !
  ! Method:
  ! see comments in program
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date        Comment
  ! -------   ----        -------
  !           03/2001     Initial version (F. Chevallier)
  !           23/07/2001  Modified for use to test RTTOV-7 (R.Saunders)
  !           01/12/2002  New F90 code with structures (P Brunel A Smith)
  !           10/10/2003  Update cloud inputs + Further clean up (F. Chevallier)
  !
  ! Code Description:
  !   Language:          Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  !
  Use rttov_const, only :   &
       & errorstatus_fatal,   &
       & errorstatus_success, &
       & default_err_unit

  Use rttov_types, only : &
       & rttov_coef     ,&
       & profile_type   ,&
       & profile_cloud_type   ,&
       & radiance_cloud_type

  Use parkind1, Only : jpim     ,jprb
  IMPLICIT NONE
#include "rttov_errorhandling.interface"
#include "rttov_readcoeffs.interface"
#include "rttov_initcoeffs.interface"
!#include "rttov_cld.interface"
#include "rttov_intex.interface"
!#include "rttov_intext_prof.interface"

  ! Program arguments:

  ! Local parameters:
  Integer(Kind=jpim), parameter :: idim=100
  Integer(Kind=jpim), parameter :: nwp_levels=60

  type( rttov_coef )                    :: coef        ! (Only one instrument)
  type(profile_type), allocatable       :: profiles(:)
  type(profile_type), allocatable       :: input_profiles(:)
  type(profile_cloud_type), allocatable :: cld_profiles(:)
  type(radiance_cloud_type)             :: radiance
  Real(Kind=jprb),    Allocatable                  :: emissivity (:)

  ! Taylor test
  type(profile_type), allocatable       :: profiles2(:)
  type(profile_cloud_type), allocatable :: cld_profiles2(:)
  type(radiance_cloud_type)             :: radiance2
  Real(Kind=jprb),    Allocatable                  :: emissivity2 (:)

  ! TL arrays
  type(profile_type), allocatable       :: prof_inc(:)
  type(profile_cloud_type), allocatable :: cld_prof_inc(:)
  type(radiance_cloud_type)             :: radiance_tl
  Real(Kind=jprb),    Allocatable                  :: emissivity_inc (:)

  type(profile_type), allocatable       :: prof_inc2(:)
  type(profile_cloud_type), allocatable :: cld_prof_inc2(:)
  type(radiance_cloud_type)             :: radiance_tl2
  Real(Kind=jprb),    Allocatable                  :: emissivity_inc2 (:)

  ! AD arrays
  type(profile_type), allocatable       :: profiles_ad(:)
  type(profile_cloud_type), allocatable :: cld_profiles_ad(:)
  type(radiance_cloud_type)             :: radiance_inc
  Real(Kind=jprb),    Allocatable                  :: emissivity_ad (:)

  type(profile_type), allocatable       :: profiles_ad2(:)
  type(profile_cloud_type), allocatable :: cld_profiles_ad2(:)
  type(radiance_cloud_type)             :: radiance_inc2
  Real(Kind=jprb),    Allocatable                  :: emissivity_ad2 (:)

  ! Local arrays:
  Real(Kind=jprb), allocatable      :: emis(:)
  Integer(Kind=jpim), allocatable   :: lchan(:)

  Integer(Kind=jpim)              :: coef_errorstatus      ! read coeffs error return code
  Integer(Kind=jpim), Allocatable :: rttov_errorstatus(:)  ! rttov error return code

  Integer(Kind=jpim) :: nchannels
  Integer(Kind=jpim) :: nprofiles
  Integer(Kind=jpim), Allocatable :: channels   (:)
  Integer(Kind=jpim), Allocatable :: lprofiles  (:)
  Real(Kind=jprb),    Allocatable :: input_emissivity (:)
  Real(Kind=jprb),    Allocatable :: radiance_total_ref (:)
  logical, Allocatable :: calcemis  (:)


  Real(Kind=jprb), dimension(nwp_levels)     :: t, q, o3, co2, cc, clw, ciw
  !

  ! Local scalars:
  !Character (len=80) :: errMessage
  !Character (len=12) :: NameOfRoutine = 'main_testad '
  Integer(Kind=jpim) :: j
  Integer(Kind=jpim) :: ioff
  Integer(Kind=jpim) :: kinrad
  Real(Kind=jprb)    :: tbobs(7), rsatid
  Real(Kind=jprb)    :: st, t2m, q2m, psurf, u10, v10
  Real(Kind=jprb)    :: rlsm, rlon, rlat
  Real(Kind=jprb)    :: x, lambda, lambda0
  Real(Kind=jprb)    :: ratio(4)
  Integer(Kind=jpim) :: jdat
  Integer(Kind=jpim) :: iyyyy, iyyyymm, iyyyymmdd
  Integer(Kind=jpim) :: iyear, imonth, iday, itime
  Integer(Kind=jpim) :: iatm, katm, ichan
  Integer(Kind=jpim) :: iexp
  Integer(Kind=jpim) :: ioout, ioin
  Integer(Kind=jpim) :: isatid
  Integer(Kind=jpim) :: i, ii, nchan, kradip, kice
  Integer(Kind=jpim) :: lev
  Logical    :: switchrad  ! true if input is BT

  Integer(Kind=jpim) :: instrument(3) ! instrument triplet
  Real(Kind=jprb)    ::  zdelta1, zdelta2
  Real(Kind=jprb)    ::  z, eps

  Integer(Kind=jpim) :: Err_Unit        ! Logical error unit (<0 for default)
  Integer(Kind=jpim) :: verbosity_level ! (<0 for default)
  ! End of program arguments

  !-----End of header-----------------------------------------------------

  !Initialise error management with default value for
  ! the error unit number and
  ! Fatal error message output
  Err_unit = -1
  verbosity_level = 1
  call rttov_errorhandling(Err_unit, verbosity_level)

  !
  write(*,*) 'Radiances(1) or Tbs(2)?'
  read(*,*) kinrad
  switchrad = kinrad == 2

  !
  ! Set satellite configuration
  !      only one satellite processed
  instrument(1)=1  ! NOAA
  write(*,*) ' NOAA sat id?'
  read(*,*) isatid
  instrument(2)=isatid

  !
  ! Choose instrument
  !
  write(*,*) ' HIRS (0), MSU (1) or AMSUA (3)?'
  read(*,*) instrument(3)

  ! Read coef file
  call rttov_readcoeffs  (coef_errorstatus, coef, instrument)
  Call rttov_initcoeffs  (coef_errorstatus, coef)

  if(coef_errorstatus /= errorstatus_success ) then
     write ( ioout, * ) 'rttov_readcoeffs fatal error'
     stop
  endif

  if( any(coef%ff_val_chn( 1 : coef%fmv_chn ) /= 1 )) then
     WRITE(*,*) ' some requested channels have bad validity parameter'
     do i = 1, coef%fmv_chn
        write(*,*) i, coef%ff_val_chn(i)
     end do
  endif

  !
  ! If infrared, choose absorption parameterisation
  !
  if(coef%id_sensor == 1) then
    ! Choose ice particle radius parameterisation
    write(*,*) ' Ice particle radius from Ou and Liou (0), Wyser et al. (1), Boudala (2) or McFarquhar et al. (3)?'
    read(*,*) kradip

    ! Choose ice cristal shape
    write(*,*) ' Ice cristals: hexagonal columns (0) or aggregates (1)?'
    read(*,*) kice
  endif

  !
  ! Set list of channels and corresponding emissivities
  !   (process all channels and let RTTOV compute the emissivity)
  !
  allocate(lchan (coef%fmv_chn) )
  allocate(emis  (coef%fmv_chn) )
  nchan=coef%fmv_chn
  emis(:) = 0._JPRB
  do i = 1 , coef%fmv_chn
    lchan(i) = i
  enddo

  !
  ! Open files
  !
  ioin = 1
! open(ioin,file='profiles_fmt',form='formatted')
  open(ioin,file='file_tot',form='unformatted')
  ioout = 2
  open(ioout,file='print.dat',form='formatted')

  !
  ! Count number of profiles
  !
! do iatm = 1,idim
!    do i = 1,38
!       read(ioin,*,end=50)
!    enddo
! enddo
!50 continue
   nprofiles = iatm - 1
! write(ioout,*) 'This dataset is made of ',nprofiles,' ECMWF model profiles'
  rewind(ioin)
  nprofiles = 1

  !
  ! Initialisations and allocations
  ! NO allocation for CO2 profiles
  !
  nchannels = nprofiles * nchan
  Allocate( channels   ( nchannels ) )
  allocate( lprofiles  ( nchannels ) )
  allocate( emissivity ( nchannels ) )
  allocate( input_emissivity ( nchannels ) )
  allocate( calcemis  ( nchannels ) )

  allocate( rttov_errorstatus(nprofiles))

  ! Profiles on RTTOV pressure levels
  allocate( profiles(nprofiles))
  do j = 1, nprofiles
     ! allocate model profiles atmospheric arrays with model levels dimension
     profiles(j) % nlevels =  coef % nlevels
     allocate( profiles(j) % p  ( coef % nlevels ) )
     allocate( profiles(j) % t  ( coef % nlevels ) )
     allocate( profiles(j) % q  ( coef % nlevels ) )
     allocate( profiles(j) % o3 ( coef % nlevels ) )
     allocate( profiles(j) % clw( coef % nlevels ) )
     profiles(j) % p(:) = coef % ref_prfl_p(:)
  end do

  ! Profiles on NWP model pressure levels
  allocate(input_profiles(nprofiles))
  do j = 1, nprofiles
     ! allocate model profiles atmospheric arrays with model levels dimension
     input_profiles(j) % nlevels =  nwp_levels
     allocate( input_profiles(j) % p  ( nwp_levels ) )
     allocate( input_profiles(j) % t  ( nwp_levels ) )
     allocate( input_profiles(j) % q  ( nwp_levels ) )
     allocate( input_profiles(j) % o3 ( nwp_levels ) )
     allocate( input_profiles(j) % clw( nwp_levels ) )
  end do

  ! Cloud additional profiles
  allocate( cld_profiles(nprofiles))
  do j = 1, nprofiles
     ! allocate model profiles atmospheric arrays with model levels dimension
     cld_profiles(j) % nlevels =  nwp_levels
     allocate( cld_profiles(j) % p  ( nwp_levels ) )
     allocate( cld_profiles(j) % ph ( nwp_levels+1 ) )
     allocate( cld_profiles(j) % t  ( nwp_levels ) )
     allocate( cld_profiles(j) % cc ( nwp_levels ) )
     allocate( cld_profiles(j) % clw( nwp_levels ) )
     allocate( cld_profiles(j) % ciw( nwp_levels ) )
  end do

  ! allocate radiance results arrays with number of channels
  allocate( radiance % clear    ( nchannels ) )
  allocate( radiance % cloudy   ( nchannels ) )
  allocate( radiance % total    ( nchannels ) )
  allocate( radiance % bt       ( nchannels ) )
  allocate( radiance % bt_clear ( nchannels ) )
  allocate( radiance % upclear  ( nchannels ) )
  allocate( radiance % reflclear( nchannels ) )
  allocate( radiance % overcast ( nwp_levels, nchannels ) )
  allocate( radiance % downcld  ( nwp_levels, nchannels ) )
  allocate( radiance % cldemis  ( nwp_levels, nchannels ) )
  allocate( radiance % wtoa     ( nwp_levels, nchannels ) )
  allocate( radiance % wsurf    ( nwp_levels, nchannels ) )
  allocate( radiance % cs_wtoa  ( nchannels ) )
  allocate( radiance % cs_wsurf ( nchannels ) )
  allocate( radiance_total_ref  ( nchannels ) )



  !
  ! Read profile dataset
  !

  iatmloop : do katm = 1,1000 !!!!1000000000
  iatm = 1
!    read(ioin,'(i12)') jdat                ! date yyyymmddhh
!    read(ioin,'(10e16.6)') rlon, &        ! longitude (deg)
!         rlat, &        ! latitude (deg)
!         rlsm, &        ! land-sea mask (1=land)
!         st, &          ! surface temperature (K)
!         psurf, &       ! surface pressure (Pa)
!         t2m, &         ! 2-meter temperature (K)
!         q2m            ! 2-meter specific humidity (kg/kg)
!    read(ioin,'(10e16.6)') t   ! temperature (K)
!    read(ioin,'(10e16.6)') q   ! specific humidity (kg/kg)
!    read(ioin,'(10e16.6)') o3  ! specific ozone (kg/kg)
!    read(ioin,'(10e16.6)') cc  ! cloud cover
!    read(ioin,'(10e16.6)') clw ! liquid water (kg/kg)
!    read(ioin,'(10e16.6)') ciw ! ice water (kg/kg)

     read(ioin,end=10) rlon,         &! longitude (deg)
          & rlat,         &! latitude (deg)
          & rsatid,       &! latitude (deg)
          & tbobs,        &! latitude (deg)
          & rlsm,         &! land-sea mask (1=land)
          & st,           &! surface temperature (K)
          & psurf,        &! surface pressure (Pa)
          & u10,          &! surface pressure (Pa)
          & v10,          &! surface pressure (Pa)
          & t2m,          &! 2-meter temperature (K)
          & q2m,          &! 2-meter specific humidity (kg/kg)
          & t,            &! temperature (K)
          & q ,           &! specific humidity (kg/kg)
          & cc,           &! cloud cover
          & clw,          &! liquid water (kg/kg)
          & ciw            ! ice water (kg/kg)

          o3(:) = 1.e-7_JPRB
          q(:) = max(q(:),0._JPRB)
          clw(:) = max(clw(:),0._JPRB)
          ciw(:) = max(ciw(:),0._JPRB)
          psurf = psurf * 100._JPRB

     !*process date
     iyyyymmdd = jdat/100
     itime = jdat - iyyyymmdd*100
     iyyyymm = iyyyymmdd/100
     iday = iyyyymmdd - iyyyymm*100
     iyyyy = iyyyymm/100
     imonth = iyyyymm - iyyyy*100
     iyear = iyyyy

     !*get model vertical pressures from surface pressure (all Pa)
     call ec_p60l(                    &
           & psurf                    ,&
           & cld_profiles( iatm ) % p ,&
           & cld_profiles( iatm ) % ph )

     ! Convert to hPa
     cld_profiles( iatm ) % p(:)  = cld_profiles( iatm ) % p(:)  /100._JPRB
     cld_profiles( iatm ) % ph(:) = cld_profiles( iatm ) % ph(:) /100._JPRB

     ! Move to structures
!    input_profiles( iatm ) % p(:)    = cld_profiles( iatm ) % p(:)
!    input_profiles( iatm ) % t(:)    = t(:)
!    input_profiles( iatm ) % q(:)    = (q(:) / (1-q(:)))  * 1.60771704 *1e+06
!    input_profiles( iatm ) % o3(:)   = (o3(:) / (1-o3(:)))* 0.6034476  *1e+06
!    input_profiles( iatm ) % clw(:)  = clw(:)
!    input_profiles( iatm ) % s2m % p = psurf/100.
!    input_profiles( iatm ) % s2m % q = (q2m / (1-q2m)) * 1.60771704*1e+06
!    input_profiles( iatm ) % s2m % o = input_profiles( iatm ) % o3(nwp_levels)
!    input_profiles( iatm ) % s2m % t = t2m
!    input_profiles( iatm ) % s2m % u = 5.   ! constant for this run
!    input_profiles( iatm ) % s2m % v = 2.   ! constant for this run
!    input_profiles( iatm ) % skin % surftype = Int(1.0 - rlsm)
!    input_profiles( iatm ) % skin % t        = st
!    input_profiles( iatm ) % skin % fastem(:) = (/ 3.0, 5.0, 15.0, 0.1, 0.3 /)

!    input_profiles( iatm ) % ozone_data = .true.
!    input_profiles( iatm ) % co2_data   = .false.
!    input_profiles( iatm ) % clw_data   = .true.
!    input_profiles( iatm ) % zenangle   = 0.    ! Nadir view
!    input_profiles( iatm ) % ctp        = 500.  ! default value
!    input_profiles( iatm ) % cfraction  = 0.    ! default value
!
!    ! convert input profile to RTTOV pressure levels
!    call rttov_intext_prof(input_profiles( iatm ), profiles( iatm ) )
!    ! CLW is not interpolated, but profiles(iatm)%clw
!    ! has been allocated, so give 0. value for clw for security
!    profiles( iatm ) % clw_data   = .false.
!    profiles( iatm ) % clw(:) = 0.


     profiles( iatm ) % clw(:) = 0._JPRB   ! warning
     profiles( iatm ) % o3 (:) = 0._JPRB   ! warning
     profiles( iatm ) % s2m % p = psurf / 100._JPRB
     profiles( iatm ) % s2m % q = (q2m / (1-q2m)) * 1.60771704_JPRB*1e+06        ! ppmv
     profiles( iatm ) % s2m % o = 0._JPRB
     profiles( iatm ) % s2m % t = t2m
     profiles( iatm ) % s2m % u = u10
     profiles( iatm ) % s2m % v = v10
     profiles( iatm ) % skin % surftype = Int(1.0_JPRB - rlsm)
     profiles( iatm ) % skin % t        = st
     profiles( iatm ) % skin % fastem(:) = (/ 3.0_JPRB, 5.0_JPRB, 15.0_JPRB, 0.1_JPRB, 0.3_JPRB /)

     profiles( iatm ) % ozone_data = .false. !!!!WARNING
     profiles( iatm ) % co2_data   = .false.
     profiles( iatm ) % clw_data   = .false.
     profiles( iatm ) % zenangle   = 53.1_JPRB  ! SSM/I
     profiles( iatm ) % ctp        = 500._JPRB  ! default value
     profiles( iatm ) % cfraction  = 0._JPRB    ! default value

     cld_profiles( iatm ) % t(:)   = t(:)
     cld_profiles( iatm ) % cc(:)  = cc(:)
     cld_profiles( iatm ) % clw(:) = clw(:)
     cld_profiles( iatm ) % ciw(:) = ciw(:)
     cld_profiles( iatm ) % kice   = kice
     cld_profiles( iatm ) % kradip = kradip

     ! convert q to ppmv
     q(:)  = (q(:)  / (1- q(:)))  * 1.60771704_JPRB *1e+06

     ! convert input profile to RTTOV pressure levels
     call rttov_intex (                  &
         & nwp_levels,                     &
         & coef%nlevels,                   &
         & cld_profiles(iatm) % p,         &
         & profiles(iatm) % p,             &
         & t(:),                           &
         & profiles(iatm) % t)
     call rttov_intex (                  &
         & nwp_levels,                     &
         & coef%nlevels,                   &
         & cld_profiles(iatm) % p,         &
         & profiles(iatm) % p,             &
         & q(:),                           &
         & profiles(iatm) % q)


     ! Channel, porfile list and emissivity arrays
     do iatm = 1, nprofiles
        ioff = (iatm - 1) * nchan
        channels(1+ioff:nchan+ioff)   = lchan(1:nchan)
        lprofiles(1+ioff:nchan+ioff)  = iatm
        emissivity(1+ioff:nchan+ioff) = emis(1:nchan)
     End do

     calcemis(:)         = emissivity(:) < 0.01_JPRB
     input_emissivity(:) = emissivity(:)

     !
     ! Call RTTOV_CLD
     !

!    write(ioout,*)
!    write(ioout,*) 'Call to RTTOV_CLD'
!    write(ioout,*) '----------------'
!    write(ioout,*)

!    Call rttov_cld( &
!         & rttov_errorstatus,  &! out
!         & nchannels,    &! in
!         & nprofiles,    &! in
!         & channels,     &! in
!         & lprofiles,    &! in
!         & profiles,     &! inout  (to invalid clw absorption)
!         & cld_profiles, &! in
!         & coef,         &! in
!         & calcemis,     &! in
!         & emissivity,   &! inout
!         & radiance )     ! inout

     If ( any( rttov_errorstatus(:) == errorstatus_fatal ) ) Then
        Do iatm = 1, nprofiles
           If ( rttov_errorstatus(iatm) == errorstatus_fatal ) Then
              write ( ioout, * ) 'rttov_cld error for profile',iatm
           End If
        End Do
        Stop
     End If


     ! main output:
     !
     ! radiance%total     = cloud-affected radiances
     ! radiance%clear     = clear-sky radiances
     ! radiance%bt        = cloud-affected Tbs
     ! radiance%bt_clear  = clear-sky Tbs
     !

     radiance_total_ref(:) = radiance%total(:)

!    if (kinrad == 2) then
!       write(ioout,*) 'Channel  cloudy Tb    clear Tb'
!       do ichan = 1, nchannels

        write(ioout,'(30f7.2)')   (radiance%bt(ichan) ,ichan = 1, nchannels)

!               & radiance%bt_clear(ichan)
!       enddo
!    else
!       write(ioout,*) 'Channel cloudy Rad   clear Rad'
!       do ichan = 1, nchannels
!          write(ioout,'(i4,3x,30e12.4)')     &
!               & ichan                      ,&
!               & radiance%total(ichan)  ,&
!               & radiance%clear(ichan)
!       enddo
!    endif

  enddo iatmloop

10  continue
  close(ioin)

  Stop

CONTAINS
!
! ----------------------------------------
!
      SUBROUTINE EC_P60l(spres,pap,paph)
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
!     Description:
!     Computes the 60-level vertical pressure grid
!       associated to the input surface pressure
!     All pressures are in Pa

  Use parkind1, Only : jpim     ,jprb
      implicit none
  Integer(Kind=jpim), parameter    :: nlev=60
  Integer(Kind=jpim)               :: jk
  Real(Kind=jprb)          :: spres
  Real(Kind=jprb)          :: aam(nlev+1), bbm(nlev+1)
  Real(Kind=jprb)          :: pap(nlev), paph(nlev+1)

      data aam / &
          & 0.000000_JPRB,    20.000000_JPRB,    38.425343_JPRB, &
         & 63.647804_JPRB,    95.636963_JPRB,   134.483307_JPRB, &
        & 180.584351_JPRB,   234.779053_JPRB,   298.495789_JPRB, &
        & 373.971924_JPRB,   464.618134_JPRB,   575.651001_JPRB, &
        & 713.218079_JPRB,   883.660522_JPRB,  1094.834717_JPRB, &
       & 1356.474609_JPRB,  1680.640259_JPRB,  2082.273926_JPRB, &
       & 2579.888672_JPRB,  3196.421631_JPRB,  3960.291504_JPRB, &
       & 4906.708496_JPRB,  6018.019531_JPRB,  7306.631348_JPRB, &
       & 8765.053711_JPRB, 10376.126953_JPRB, 12077.446289_JPRB, &
      & 13775.325195_JPRB, 15379.805664_JPRB, 16819.474609_JPRB, &
      & 18045.183594_JPRB, 19027.695313_JPRB, 19755.109375_JPRB, &
      & 20222.205078_JPRB, 20429.863281_JPRB, 20384.480469_JPRB, &
      & 20097.402344_JPRB, 19584.330078_JPRB, 18864.750000_JPRB, &
      & 17961.357422_JPRB, 16899.468750_JPRB, 15706.447266_JPRB, &
      & 14411.124023_JPRB, 13043.218750_JPRB, 11632.758789_JPRB, &
      & 10209.500977_JPRB,  8802.356445_JPRB,  7438.803223_JPRB, &
       & 6144.314941_JPRB,  4941.778320_JPRB,  3850.913330_JPRB, &
       & 2887.696533_JPRB,  2063.779785_JPRB,  1385.912598_JPRB, &
        & 855.361755_JPRB,   467.333588_JPRB,   210.393890_JPRB, &
         & 65.889244_JPRB,     7.367743_JPRB,     0.000000_JPRB, &
          & 0.000000_JPRB &
              & /
      data bbm / &
      & 0.0000000000_JPRB, 0.0000000000_JPRB, 0.0000000000_JPRB, &
      & 0.0000000000_JPRB, 0.0000000000_JPRB, 0.0000000000_JPRB, &
      & 0.0000000000_JPRB, 0.0000000000_JPRB, 0.0000000000_JPRB, &
      & 0.0000000000_JPRB, 0.0000000000_JPRB, 0.0000000000_JPRB, &
      & 0.0000000000_JPRB, 0.0000000000_JPRB, 0.0000000000_JPRB, &
      & 0.0000000000_JPRB, 0.0000000000_JPRB, 0.0000000000_JPRB, &
      & 0.0000000000_JPRB, 0.0000000000_JPRB, 0.0000000000_JPRB, &
      & 0.0000000000_JPRB, 0.0000000000_JPRB, 0.0000000000_JPRB, &
      & 0.0000758235_JPRB, 0.0004613950_JPRB, 0.0018151561_JPRB, &
      & 0.0050811190_JPRB, 0.0111429105_JPRB, 0.0206778757_JPRB, &
      & 0.0341211632_JPRB, 0.0516904071_JPRB, 0.0735338330_JPRB, &
      & 0.0996746942_JPRB, 0.1300225109_JPRB, 0.1643843204_JPRB, &
      & 0.2024759352_JPRB, 0.2439331412_JPRB, 0.2883229554_JPRB, &
      & 0.3351548910_JPRB, 0.3838921487_JPRB, 0.4339629412_JPRB, &
      & 0.4847715795_JPRB, 0.5357099175_JPRB, 0.5861684084_JPRB, &
      & 0.6355474591_JPRB, 0.6832686067_JPRB, 0.7287858129_JPRB, &
      & 0.7715966105_JPRB, 0.8112534285_JPRB, 0.8473749161_JPRB, &
      & 0.8796569109_JPRB, 0.9078838825_JPRB, 0.9319403172_JPRB, &
      & 0.9518215060_JPRB, 0.9676452279_JPRB, 0.9796627164_JPRB, &
      & 0.9882701039_JPRB, 0.9940194488_JPRB, 0.9976301193_JPRB, &
      & 1.0000000000_JPRB &
              & /

      do jk=1,nlev+1
      paph(jk)=aam(jk)+bbm(jk)*spres
      end do
      do jk=1,nlev
      pap(jk)=0.5_JPRB*(paph(jk)+paph(jk+1))
      end do

      end subroutine ec_p60l

END PROGRAM rttovcld_testad
