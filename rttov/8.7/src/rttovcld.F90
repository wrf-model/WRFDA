!
!  Cloud package
!
SUBROUTINE RTTOVCLD &
     & (knch,knpf, klenpf, klevm,     &
      & ppres, pangl, pangs, ksurf, ksat, knchpf,&
      & kchan, kprof, pav, psav, pssv, pcvm, pap, paph,   &
      & pemis, ifail, prad, ptb, pradcld, ptbcld, tau, tausfc,  &
      & pradovm, pcldemis, pait, pais, &
      & nfrequencies, nchannels, nbtout)
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
! Description:
! to compute multi-channel radiances and brightness
! temperatures for many profiles in cloudy sky.
! Compatible with RTTOV8 library but only able to
! run with coefficients created on RTTOV7 43 pressure levels
!
! Method
! see Saunders et al QJ 1999  for the clear-sky part
! see comments for cloudy sky part
!
! Current Code Owner: SAF NWP
!
! History:
! Version   Date        Comment
! -------   ----        -------
!           03/2001     Initial version (F. Chevallier)
!           19/7/2001   Version for testing RTTOV-7 (R. Saunders)
!          01/12/2002   Keep compatibility with RTTOV8 (P Brunel)
!          09/01/2003   Add polarisation (S English)
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
     nplatforms          ,&
     ninst               ,&
     pi                  ,&
     errorstatus_fatal   ,&
     errorstatus_warning ,&
     errorstatus_success ,&
     platform_name       ,&
     sensor_id_mw        ,&
     inst_name,           &
     npolar_return,   &
     npolar_compute



  Use rttov_types, only : &
    & rttov_coef           ,&
    & geometry_Type        ,&
    & profile_type         ,&
    & profile_cloud_type   ,&
    & radiance_cloud_type

  USE MOD_CPARAM, ONLY : &
  ! Imported Scalar Variables with intent (in):
  & njpnsat  ,   &! Total max sats to be used
  & njplev   ,   &! No. of pressure levels
  & njpnav   ,   &! No. of profile variables
  & njpnsav  ,   &! No. of surface air variables
  & njpnssv  ,   &! No. of skin variables
  & njpncv   ,   &! No. of cloud variables
  & q_mixratio_to_ppmv  ,&
  & o3_mixratio_to_ppmv , &
  & coef

  Use parkind1, Only : jpim     ,jprb
  IMPLICIT NONE
#include "rttov_cld.interface"
#include "rttov_errorreport.interface"
#include "rttov_setupindex.interface"
!#include "rttov_setupchan.interface"

  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  Integer(Kind=jpim) , INTENT(in) :: knpf           ! Number of profiles
  Integer(Kind=jpim) , INTENT(in) :: knch(knpf)     ! Number of channels
  Integer(Kind=jpim) , INTENT(in) :: klenpf         ! Length of input  profile vectors
  Integer(Kind=jpim) , INTENT(in) :: ksat           ! Satellite index (see rttvi)
  Integer(Kind=jpim) , INTENT(in) :: knchpf         ! Number of output radiances
                                         !    (= channels used * profiles)
  Integer(Kind=jpim) , INTENT(in) :: klevm          ! Number of model(native) levels

  ! Array  arguments with intent(in):
  Integer(Kind=jpim) , INTENT(in) :: kchan(knchpf)  ! Channel indices
  Integer(Kind=jpim) , INTENT(in) :: kprof(knchpf)  ! Profiles indices
  Integer(Kind=jpim) , INTENT(in) :: ksurf(knpf)    ! Surface type index
  Real(Kind=jprb) , INTENT(in)    :: ppres(njplev)  ! Pressure levels (hpa) of
                                         !   atmospheric profile vectors


  Real(Kind=jprb) , INTENT(in)    :: pangl(knpf)    ! Satellite local zenith angle (deg)
  Real(Kind=jprb) , INTENT(in)    :: pangs(knpf)    ! Solar zenith angle at surface (deg)
  Real(Kind=jprb) , INTENT(in)    :: pav(njplev,njpnav,knpf)! Atmosp. profile variables
  Real(Kind=jprb) , INTENT(in)    :: psav(njpnsav,knpf)    ! Surface air variables
  Real(Kind=jprb) , INTENT(in)    :: pssv(njpnssv,knpf)    ! Surface skin variables
  Real(Kind=jprb) , INTENT(in)    :: pap(knpf,klevm)       ! Full-level model pressures (hPa) of
                                                !   atmospheric profile vectors
  Real(Kind=jprb) , INTENT(in)    :: paph(knpf,klevm+1)    ! Half-level model pressures (hPa) of
                                                !   atmospheric profile vectors
  Real(Kind=jprb) , INTENT(in)    :: pcvm(knpf,klevm,4)    ! Temperature and
                                                ! cloud variables on klevm layers
                                                ! 1 = temperature (K)
                                                ! 2 = cloud cover
                                                ! 3 = cloud liquid water (kg/kg)
                                                ! 4 = cloud ice water (kg/kg)

  ! Array  arguments with intent(inout):
  Real(Kind=jprb) , INTENT(inout) :: pemis(knchpf)         !  surface emissivities

  ! Scalar arguments with intent(out):

  ! Array  arguments with intent(out):
  Integer(Kind=jpim) , INTENT(out) :: ifail(knpf,njpnsat)  !  return flag
              !     0 = input profile OK
              ! 11-19 = outside profile limits
              !    11 =  temp profile
              !    12 =  specific humidity profile
              !    13 =  ozone profile
              !    14 =  surface temp profile
              !    15 =  surface specific humidity profile
              !    16 =  surface wind
              ! 20-29 = unphysical profile
              !    20 =  input pressure levels wrong
              !    21 =  temp profile
              !    22 =  specific humidity profile
              !    23 =  ozone profile
              !    24 =  surface temp profile
              !    25 =  surface specific humidity profile
              !    26 =  surface wind
              !    27 =  surface pressure

  Real(Kind=jprb) , INTENT(out) :: prad(knchpf)         ! clear-sky radiances (mw/cm-1/ster/sq.m)
  Real(Kind=jprb) , INTENT(out) :: ptb(knchpf)          ! clear-sky brightness temperatures (K)
  Real(Kind=jprb) , INTENT(out) :: pradcld(knchpf)      ! cloud-affected radiance
  Real(Kind=jprb) , INTENT(out) :: ptbcld(knchpf)       ! cloud-affected brightness temperature
  Real(Kind=jprb) , INTENT(out) :: tau(knchpf,njplev)  ! clear-sky transmittance from each
                                                !   standard pressure level
  Real(Kind=jprb) , INTENT(out) :: tausfc(knchpf)      ! clear-sky transmittance from surface
  Real(Kind=jprb) , INTENT(out) :: pradovm(knchpf,2*klevm+2)
              ! RT quantities for cloud computation (see def. of radov in rttov.f90)
              ! on native levels
  Real(Kind=jprb) , INTENT(out) :: pcldemis(knchpf,klevm) ! cloud emissivity
  Real(Kind=jprb) , INTENT(out) :: pait(knchpf,klevm+1)   ! toa weights of the cloud layers
  Real(Kind=jprb) , INTENT(out) :: pais(knchpf,klevm+1)   ! surface weights of the cloud layers

!


! Local scalars
!
  Integer(Kind=jpim) :: errorstatus(knpf)
  Integer(Kind=jpim) :: alloc_status(23)

  Character (len=80) :: errMessage
  Character (len=8)  :: NameOfRoutine = 'rttovcld'
!


! Local arrays
!

  Type( rttov_coef ), pointer :: coef_pointer         ! coefficients
  Type(profile_type),allocatable          :: profiles(:)
  Type(profile_cloud_type),allocatable    :: cld_profiles(:)
  Type(radiance_cloud_type)               :: cld_radiance

  Integer(Kind=jpim)              :: i,ich2 ,ibtout,jch,nch,j,jpol,ilev
  integer(Kind=jpim)              :: nbtout
  Integer(Kind=jpim)              :: nchannels         ! Number of internal radiances
  Integer(Kind=jpim)              :: nfrequencies         ! Number of output frequencies
  Integer(Kind=jpim), Allocatable :: polarisations   (:,:)
  Integer(Kind=jpim), Allocatable :: frequencies   (:)
  Integer(Kind=jpim), Allocatable :: channels   (:)
  Integer(Kind=jpim), Allocatable :: lprofiles   (:)
  Integer(Kind=jpim), Allocatable :: indexout    (:)


  Real(Kind=jprb)                 :: pol_id
  Real(Kind=jprb),    Allocatable :: emissivity (:)
  Real(Kind=jprb),    Allocatable :: input_emissivity (:)


  Logical, Allocatable :: calcemis  (:)

  Real(Kind=jprb), target :: p__p   (coef(ksat)%nlevels,knpf)
  Real(Kind=jprb), target :: p__t   (coef(ksat)%nlevels,knpf)
  Real(Kind=jprb), target :: p__q   (coef(ksat)%nlevels,knpf)
  Real(Kind=jprb), target :: p__o3  (coef(ksat)%nlevels,knpf)
  Real(Kind=jprb), target :: p__clw (coef(ksat)%nlevels,knpf)

  Real(Kind=jprb), target :: cp__p   (klevm,knpf)
  Real(Kind=jprb), target :: cp__ph   (klevm+1,knpf)
  Real(Kind=jprb), target :: cp__t   (klevm,knpf)
  Real(Kind=jprb), target :: cp__cc  (klevm,knpf)
  Real(Kind=jprb), target :: cp__clw (klevm,knpf)
  Real(Kind=jprb), target :: cp__ciw (klevm,knpf)


  !- End of header ------------------------------------------------------

  !
  !-----------------------------------------------------------------
  !*         0.   Initialisation
  !               --------------

  errorstatus(:)  = 0
  alloc_status(:) = 0

  coef_pointer => coef(ksat)

     ! Set up various channel numbers required by RTTOV-8
!    Call rttov_setupchan(knpf        ,knchpf      ,coef(ksat ),nfrequencies, &
!    & nchannels,nbtout)

     ! total number of channels
     ! Memory allocation for RTTOV_Direct
     !-----------------------------------
  allocate( channels ( nfrequencies ) ,stat= alloc_status(1))
  allocate( lprofiles ( nfrequencies ) ,stat= alloc_status(1))
  allocate( polarisations(nchannels,3),stat= alloc_status(3))
  allocate( frequencies(nbtout),stat= alloc_status(4))
  allocate( indexout(nbtout),stat= alloc_status(5))
  allocate( input_emissivity(nchannels),stat= alloc_status(6))
  allocate( emissivity(nchannels),stat= alloc_status(7))
  allocate( profiles(knpf)      ,stat= alloc_status(2))
  allocate( cld_profiles(knpf)      ,stat= alloc_status(2))

  If( any(alloc_status /= 0) ) then
     ifail(:,:) = 20
     Write( errMessage, '( "mem allocation error")' )
     errorstatus(1) = errorstatus_fatal
     Call Rttov_ErrorReport (errorstatus(1), errMessage, NameOfRoutine)
     Return
  End If




  Do j = 1, knpf
     ! allocate model profiles atmospheric arrays with model levels dimension
     profiles(j) % p   => p__p  (:,j)
     profiles(j) % t   => p__t  (:,j)
     profiles(j) % q   => p__q  (:,j)
     profiles(j) % o3  => p__o3 (:,j)
     profiles(j) % clw => p__clw(:,j)

     cld_profiles(j) % p   => cp__p    (:,j)
     cld_profiles(j) % ph  => cp__ph   (:,j)
     cld_profiles(j) % t   => cp__t    (:,j)
     cld_profiles(j) % cc  => cp__cc   (:,j)
     cld_profiles(j) % clw => cp__clw  (:,j)
     cld_profiles(j) % ciw => cp__ciw  (:,j)
  End Do

  Do j = 1, knpf

     profiles(j) % nlevels =  coef(ksat) % nlevels
     profiles(j) % p    (:) = ppres(:)
     profiles(j) % t    (:) = pav(:,1,j)
     profiles(j) % q    (:) = pav(:,2,j) * q_mixratio_to_ppmv
     profiles(j) % o3   (:) = pav(:,3,j) * o3_mixratio_to_ppmv
     profiles(j) % clw  (:) = pav(:,4,j)
     profiles(j) % ozone_data = .true.
     profiles(j) % co2_data   = .false.
     profiles(j) % clw_data   = .false.  ! No cloud calc inside opdep
     profiles(j) % s2m % t = psav(1,j)
     profiles(j) % s2m % q = psav(2,j) * q_mixratio_to_ppmv
     profiles(j) % s2m % p = psav(3,j)
     profiles(j) % s2m % u = psav(4,j)
     profiles(j) % s2m % v = psav(5,j)
     profiles(j) % skin % t       = pssv(1,j)
     profiles(j) % skin % fastem  = 0._JPRB
     profiles(j) % skin % surftype= ksurf(j)
     profiles(j) % ctp            = 500._JPRB     ! default value
     profiles(j) % cfraction      = 0._JPRB       ! default value
     profiles(j) % zenangle       = pangl(j)
     profiles(j) % azangle        = 0._JPRB


     cld_profiles(j) % nlevels =  klevm
     cld_profiles(j) % p    (:) = pap (j,:)
     cld_profiles(j) % ph   (:) = paph(j,:)
     cld_profiles(j) % t    (:) = pcvm(j,:,1)
     cld_profiles(j) % cc   (:) = pcvm(j,:,2)
     cld_profiles(j) % clw  (:) = pcvm(j,:,3)
     cld_profiles(j) % ciw  (:) = pcvm(j,:,4)
     cld_profiles(j) % kice   = 1   ! choose ice cristals = aggregates
     cld_profiles(j) % kradip = 3   ! choose McFarquhar et al. for ice particle radius

  End Do

  allocate( calcemis  ( nchannels ) ,stat= alloc_status(1))

    ! allocate radiance results arrays with number of channels
  allocate( cld_radiance % clear    ( nchannels ) ,stat= alloc_status(3))
  allocate( cld_radiance % cloudy   ( nchannels ) ,stat= alloc_status(4))
  allocate( cld_radiance % total    ( nchannels ) ,stat= alloc_status(5))
  allocate( cld_radiance % bt       ( nchannels ) ,stat= alloc_status(6))
  allocate( cld_radiance % bt_clear ( nchannels ) ,stat= alloc_status(7))
  allocate( cld_radiance % out       ( nbtout )   ,stat= alloc_status(6))
  allocate( cld_radiance % out_clear ( nbtout )   ,stat= alloc_status(7))
  allocate( cld_radiance % clear_out ( nbtout )   ,stat= alloc_status(7))
  allocate( cld_radiance % total_out ( nbtout )   ,stat= alloc_status(7))
  allocate( cld_radiance % upclear  ( nchannels ) ,stat= alloc_status(8))
  allocate( cld_radiance % dnclear  ( nchannels ) ,stat= alloc_status(17))
  allocate( cld_radiance % reflclear( nchannels ) ,stat= alloc_status(9))
  allocate( cld_radiance % overcast ( klevm, nchannels ) ,stat= alloc_status(10))
  allocate( cld_radiance % downcld  ( klevm, nchannels ) ,stat= alloc_status(11))
  allocate( cld_radiance % cldemis  ( klevm, nchannels ) ,stat= alloc_status(12))
  allocate( cld_radiance % wtoa     ( klevm, nchannels ) ,stat= alloc_status(13))
  allocate( cld_radiance % wsurf    ( klevm, nchannels ) ,stat= alloc_status(14))
  allocate( cld_radiance % cs_wtoa  ( nchannels ) ,stat= alloc_status(15))
  allocate( cld_radiance % cs_wsurf ( nchannels ) ,stat= alloc_status(16))
  If( any(alloc_status /= 0) ) then
     ifail(:,:) = 20
     Write( errMessage, '( "mem allocation error")' )
     errorstatus(1) = errorstatus_fatal
     Call Rttov_ErrorReport (errorstatus(1), errMessage, NameOfRoutine)
     Return
  End If
  ! Build the list of channels/profiles indices
  input_emissivity(:) = pemis(:)
  Call rttov_setupindex (knch      ,knpf ,nfrequencies,nchannels,nbtout,coef(ksat ),&
   & input_emissivity,lprofiles,channels,polarisations,emissivity)
  !
  !save input values of emissivities for all calculations
  ! calculate emissivity where the input emissivity value is less than 0.01
  !  calcemis(:) = emissivity(:) < 0.01_JPRB

  Do j = 1,nchannels
     calcemis(j)=.false.
     if( emissivity(j)  > 1.5_JPRB )calcemis(j)=.true.
     if( emissivity(j)  < .01_JPRB )calcemis(j)=.true.
  End Do

     !
  Call rttov_cld( &
   & errorstatus,    &! out
   & nfrequencies,   &! in
   & nchannels,      &! in
   & nbtout,         &! in
   & knpf,           &! in
   & kchan,          &! in
   & polarisations,  &! in
   & kprof,          &! in
   & profiles,       &! inout  (to invalid clw absorption)
   & cld_profiles,   &! in
   & coef_pointer,   &! in
   & calcemis,       &! in
   & emissivity,     &! inout
   & cld_radiance )   ! inout

  tausfc(:) = -999._JPRB   ! output not available anymore
  tau(:,:)  = -999._JPRB   ! output not available anymore

  Do j = 1, knpf
     If( errorstatus(j) == errorstatus_fatal ) then
        ifail(j,:) = 20 ! unphysical profile
     Else If( errorstatus(j) == errorstatus_warning ) then
        ifail(j,:) = 11 ! outside profile limits
     Else
        ifail(j,:) = 0
     End If
  End Do

  ptbcld(:)  = cld_radiance % out(:)
  ptb(:)     = cld_radiance % out_clear(:)

  If( coef(ksat) % id_sensor == sensor_id_mw) then
     Do j=1,nchannels
        jpol = polarisations(j,2)

        prad(jpol)    = cld_radiance % clear(j)
        pradcld(jpol) = cld_radiance % total(j)

        pradovm(jpol,2*klevm+1)        = cld_radiance % upclear  (j)
        pradovm(jpol,2*klevm+2)        = cld_radiance % reflclear(j)
        pais(jpol,klevm+1)    = cld_radiance % cs_wsurf(j)
        pait(jpol,klevm+1)    = cld_radiance % cs_wtoa (j)

        Do ilev = 1 , klevm
           pradovm(jpol,ilev   )          = cld_radiance % overcast (ilev,j)
           pradovm(jpol,ilev +klevm)  = cld_radiance % downcld  (ilev,j)
           pcldemis(jpol,ilev)      = cld_radiance % cldemis (ilev,j)
           pais(jpol,ilev)    = cld_radiance % wsurf   (ilev,j)
           pait(jpol,ilev   )    = cld_radiance % wtoa    (ilev,j)
        End Do
     End Do
  Else
     Do j = 1, nfrequencies
        prad(j)    = cld_radiance % clear(j)
        pradcld(j) = cld_radiance % total(j)

        pradovm(j,2*klevm+1)        = cld_radiance % upclear  (j)
        pradovm(j,2*klevm+2)        = cld_radiance % reflclear(j)
        pais(j,klevm+1)    = cld_radiance % cs_wsurf(j)
        pait(j,klevm+1)    = cld_radiance % cs_wtoa (j)

        Do ilev = 1 , klevm
           pradovm(j,ilev   )          = cld_radiance % overcast (ilev,j)
           pradovm(j,ilev +klevm)  = cld_radiance % downcld  (ilev,j)
           pcldemis(j,ilev)      = cld_radiance % cldemis (ilev,j)
           pais(j,ilev)    = cld_radiance % wsurf   (ilev,j)
           pait(j,ilev   )    = cld_radiance % wtoa    (ilev,j)
        End Do
     End Do
  Endif

  ifail(:,:) = 0

  deallocate( calcemis ,stat= alloc_status(1))
  deallocate( cld_radiance % clear     ,stat= alloc_status(3))
  deallocate( cld_radiance % cloudy    ,stat= alloc_status(4))
  deallocate( cld_radiance % total     ,stat= alloc_status(5))
  deallocate( cld_radiance % bt        ,stat= alloc_status(6))
  deallocate( cld_radiance % bt_clear  ,stat= alloc_status(7))
  deallocate( cld_radiance % out        ,stat= alloc_status(6))
  deallocate( cld_radiance % out_clear  ,stat= alloc_status(7))
  deallocate( cld_radiance % clear_out  ,stat= alloc_status(7))
  deallocate( cld_radiance % total_out  ,stat= alloc_status(7))
  deallocate( cld_radiance % upclear   ,stat= alloc_status(8))
  deallocate( cld_radiance % dnclear   ,stat= alloc_status(17))
  deallocate( cld_radiance % reflclear ,stat= alloc_status(9))
  deallocate( cld_radiance % overcast  ,stat= alloc_status(10))
  deallocate( cld_radiance % downcld   ,stat= alloc_status(11))
  deallocate( cld_radiance % cldemis   ,stat= alloc_status(12))
  deallocate( cld_radiance % wtoa      ,stat= alloc_status(13))
  deallocate( cld_radiance % wsurf     ,stat= alloc_status(14))
  deallocate( cld_radiance % cs_wtoa  ,stat= alloc_status(15))
  deallocate( cld_radiance % cs_wsurf  ,stat= alloc_status(16))
  deallocate(polarisations  ,stat= alloc_status(17))
  deallocate(frequencies    ,stat= alloc_status(18))
  deallocate(channels       ,stat= alloc_status(19))
  deallocate(lprofiles      ,stat= alloc_status(20))
  deallocate(indexout       ,stat= alloc_status(21))
  deallocate(emissivity     ,stat= alloc_status(22))
  deallocate(input_emissivity,stat= alloc_status(23))
  deallocate(cld_profiles   ,stat= alloc_status(23))
  If( any(alloc_status /= 0) ) then
     ifail(:,:) = 20
     Write( errMessage, '( "mem allocation error")' )
     errorstatus(1) = errorstatus_fatal
     Call Rttov_ErrorReport (errorstatus(1), errMessage, NameOfRoutine)
     Return
  End If


END SUBROUTINE RTTOVCLD
