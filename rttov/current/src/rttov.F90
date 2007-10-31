!+ Fast radiative transfer model.
!
SUBROUTINE RTTOV &
     (knpf, klenpf, ppres, pangl, pangs, ksurf, ksat, knchpf,  &
     kchan, kprof, pav, psav, pssv, pcv, pemis, ifail, prad, ptb, radov, &
     rado, tau, tausfc, lcloud)
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
! to compute multi-channel level to space transmittances,
! top of atmosphere radiances and brightness
! temperatures for many profiles and Ir/Mw sensors.
! Compatible with RTTOV8 library but only able to
! run with coefficients created on RTTOV7 43 pressure levels
!
! Method
! see Matricardi et al QJ 2002
! see Saunders et al QJ 1999
! see: ECMWF Technical Memoranda 176/282/345 (Available from ECMWF)
!
! Current Code Owner: SAF NWP
!
! History:
! Version   Date        Comment
! -------   ----        -------
!          13/8/92.  For version 2.
!                    ksat added to argument list; ssu included;
!                    internal changes to move big arrays from commons to
!                    arguments and to introduce taskcommons
!           8/7/97   added ozone and extended water vapour in control vector
!        01/05/2000     F90 code
!        21/08/2000  Interface to rtint changed to include pref (surface reflectivity).
!                    (Stephen English)
!        31/01/2001  More cloud computations. stored in radov (F. Chevallier)
!        6/2/2001    pgrody and knav etc arrays removed from call (R Saunders)
!        18/01/2002  Thread safe (D.Salmond)
!        01/12/2002  Keep compatibility with RTTOV8 (P Brunel)
!
! Code Description:
!   Language:          Fortran 90.
!   Software Standards: "European Standards for Writing and
!     Documenting Exchangeable Fortran 90 Code".
!     31/01/2001  More cloud computations. stored in radov (F. Chevallier)
! Declarations:
! Modules used:
!
  Use rttov_const, only :   &
       errorstatus_warning ,&
       errorstatus_fatal   ,&
       sensor_id_mw        ,&
       npolar_return,       &
       npolar_compute

  Use rttov_types, only : &
    rttov_coef     ,&
    profile_type   ,&
    transmission_type  ,&
    radiance_type

  USE MOD_CPARAM, ONLY : &
  ! Imported Scalar Variables with intent (in):
  njpnsat  ,&   ! Total max sats to be used
  njplev   ,&   ! No. of pressure levels
  njpnav   ,&   ! No. of profile variables
  njpnsav  ,&   ! No. of surface air variables
  njpnssv  ,&   ! No. of skin variables
  njpncv   ,&   ! No. of cloud variables
  q_mixratio_to_ppmv  ,&
  o3_mixratio_to_ppmv ,&
  coef

  Use parkind1, Only : jpim     ,jprb
  IMPLICIT NONE

#include "rttov_errorreport.interface"
#include "rttov_direct.interface"

  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  Integer(Kind=jpim) , INTENT(in) :: knpf           ! Number of profiles
  Integer(Kind=jpim) , INTENT(in) :: klenpf         ! Length of input  profile vectors
  Integer(Kind=jpim) , INTENT(in) :: ksat           ! Satellite index (see rttvi)
  Integer(Kind=jpim) , INTENT(in) :: knchpf         ! Number of output frequencies
                                                    !    (= channels used * profiles)
  LOGICAL, INTENT(in) :: lcloud          ! switch for cloud computations

  ! Array  arguments with intent(in):
  Integer(Kind=jpim) , INTENT(in) :: kchan(knchpf)    ! Channel indices
  Integer(Kind=jpim) , INTENT(in) :: kprof(knchpf)    ! Profiles indices
  Integer(Kind=jpim) , INTENT(in) :: ksurf(knpf)      ! Surface type index
  Real(Kind=jprb) , INTENT(in)    :: ppres(njplev)    ! Pressure levels (hpa) of
                                         !   atmospheric profile vectors


  Real(Kind=jprb) , INTENT(in)    :: pangl(knpf)    ! Satellite local zenith angle (deg)
  Real(Kind=jprb) , INTENT(in)    :: pangs(knpf)    ! Solar zenith angle at surface (deg)
  Real(Kind=jprb) , INTENT(in)    :: pav(njplev,njpnav,knpf)! Atmosp. profile variables
  Real(Kind=jprb) , INTENT(in)    :: psav(njpnsav,knpf)    ! Surface air variables
  Real(Kind=jprb) , INTENT(in)    :: pssv(njpnssv,knpf)    ! Surface skin variables
  Real(Kind=jprb) , INTENT(in)    :: pcv(njpncv,knpf)      ! Cloud variables

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

  Real(Kind=jprb) , INTENT(out)    :: prad(knchpf)         ! radiances (mw/cm-1/ster/sq.m)
  Real(Kind=jprb) , INTENT(out)    :: ptb(knchpf)          ! brightness temperatures (K)
  Real(Kind=jprb) , INTENT(out)    :: rado(knchpf)         ! overcast radiance at given
                                                !   cloud top in mw/m2/sr/cm-1
  Real(Kind=jprb) , INTENT(out)    :: radov(knchpf,2*njplev+2)
    ! RT quantities for possible cloud computations outside RTTOV,
    !   in mw/m2/sr/cm-1 :
    ! radov (:,1:njplev)          : overcast radiance at given cloud top
    ! radov (:,njplev+1,2*njplev) : contribution to radiance of
    !                               downward cloud emission at given cloud top
    ! radov (:,2*njplev+1)        : clear-sky radiance without reflection term
    ! radov (:,2*njplev+2)        : reflected clear-sky downwelling radiance

  Real(Kind=jprb) , INTENT(out)    :: tau(knchpf,njplev)  ! transmittance from each
                                                !   standard pressure level
  Real(Kind=jprb) , INTENT(out)    :: tausfc(knchpf)      ! transmittance from surface



! Local arrays:
  Real(Kind=jprb)    :: panga(knpf)    ! Satellite local azimuth angle (deg)
  integer(Kind=jpim) :: nbtout
  integer(Kind=jpim) :: nfrequencies
  Integer(Kind=jpim) :: nchannels
  Integer(Kind=jpim) :: nprofiles
  integer(Kind=jpim), Allocatable :: polarisations   (:,:)
  !integer(Kind=jpim), Allocatable :: frequencies   (:)
  Integer(Kind=jpim), Allocatable :: channels   (:)
  Integer(Kind=jpim), Allocatable :: lprofiles  (:)
  Real(Kind=jprb),    Allocatable :: emissivity (:)

    type( rttov_coef ), pointer :: coef_pointer         ! coefficients
    type(profile_type)   :: profiles(knpf)
    type(radiance_type)  :: radiance
    type(transmission_type)  :: transmission

    logical, Allocatable :: calcemis  (:)

  Integer(Kind=jpim)   :: errorstatus(knpf)
  Integer(Kind=jpim)   :: alloc_status(22)
    Character (len=80) :: errMessage
    Character (len=6)  :: NameOfRoutine = 'rttov '
  Integer(Kind=jpim) :: j, jch, pol_id, ibtout, ichannels
  Integer(Kind=jpim) :: i, n, jpol
!- End of header ------------------------------------------------------

    errorstatus(:)  = 0
    alloc_status(:) = 0
  !  The terms "constant" and "variable" are employed here in the sense used
  !  in variational analysis, i.e. an input variable is a parameter with
  !  respect to which a gradient will be calculated in the associated
  !  tangent linear (TL) and adjoint (AD) routines.
  !
    coef_pointer => coef(ksat)

     If( coef_pointer % id_sensor /= sensor_id_mw) then
        nchannels     = knchpf
        nfrequencies  = knchpf
        nbtout        = knchpf
        nchannels     = knchpf
        nprofiles     = knpf
     End If
     If( coef_pointer % id_sensor == sensor_id_mw) then
        nfrequencies  = knchpf
        nprofiles     = knpf
        ichannels = 0
        ibtout = 0
        do  j = 1, nfrequencies
           pol_id = coef_pointer % fastem_polar(j) + 1
           ichannels=ichannels+npolar_compute(pol_id)
           ibtout=ibtout+npolar_return(pol_id)
        end do
        nchannels = ichannels
        nbtout = ibtout
     End If

     allocate( lprofiles ( nfrequencies ) ,stat= alloc_status(1))
     allocate( channels  ( nfrequencies ) ,stat= alloc_status(2))
     allocate( polarisations(nchannels,3) ,stat= alloc_status(3))
     allocate( emissivity ( nchannels )   ,stat= alloc_status(4))
     allocate( calcemis  ( nchannels )   ,stat= alloc_status(5))
     If( any(alloc_status /= 0) ) then
        ifail(:,:) = 20
        Write( errMessage, '( "mem allocation 1 error")' )
        Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
        Return
     End If

     If( coef_pointer % id_sensor /= sensor_id_mw) then
        lprofiles (:) = kprof(:)
        channels  (:) = kchan(:)
        polarisations(:,1) = (/ (i, i=1,knchpf) /)
        polarisations(:,2) = (/ (i, i=1,knchpf) /)
        polarisations(:,3) = 1
        emissivity(:) = pemis(polarisations(:,2))
     End If



     If( coef_pointer % id_sensor == sensor_id_mw) then
        lprofiles (:) = kprof(:)
        channels  (:) = kchan(:)
        ichannels = 0
        polarisations(:,:) = 0
        do  j = 1, nfrequencies
           jch = kchan(j)
           pol_id = coef_pointer % fastem_polar(jch) + 1
           polarisations(j,1) = ichannels+1
           polarisations(j,3) = npolar_compute(pol_id)
           Do n = ichannels+1, ichannels+npolar_compute(pol_id)
              polarisations(n,2)=j
           End Do
           ichannels=ichannels+npolar_compute(pol_id)
        end do
        emissivity(:) = pemis(polarisations(:,2))
     End If

!     write(6,*)' nfreq=',nfrequencies,' nchannels=',nchannels,' nbtout=',nbtout
!     write(6,*)' Channels ',(channels(i),i=1,nfrequencies)
!     write(6,*)(polarisations(i,1),i=1,nchannels)
!     write(6,*)(polarisations(i,2),i=1,nchannels)
!     write(6,*)(polarisations(i,3),i=1,nchannels)

     do j = 1, knpf
        ! allocate model profiles atmospheric arrays with model levels dimension
        profiles(j) % nlevels =  coef(ksat) % nlevels
        allocate( profiles(j) % p  ( coef(ksat) % nlevels ) ,stat= alloc_status(1))
        allocate( profiles(j) % t  ( coef(ksat) % nlevels ) ,stat= alloc_status(2))
        allocate( profiles(j) % q  ( coef(ksat) % nlevels ) ,stat= alloc_status(3))
        allocate( profiles(j) % o3 ( coef(ksat) % nlevels ) ,stat= alloc_status(4))
        allocate( profiles(j) % clw( coef(ksat) % nlevels ) ,stat= alloc_status(5))
        If( any(alloc_status /= 0) ) then
           ifail(:,:) = 20
           Write( errMessage, '( "mem allocation 2 error")' )
           Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
           Return
        End If

        profiles(j) % p    (:) = ppres(:)
        profiles(j) % t    (:) = pav(:,1,j)
        profiles(j) % q    (:) = pav(:,2,j) * q_mixratio_to_ppmv
        profiles(j) % o3   (:) = pav(:,3,j) * o3_mixratio_to_ppmv
        profiles(j) % clw  (:) = pav(:,4,j)
        profiles(j) % ozone_data = .true.
        profiles(j) % co2_data   = .false.
        profiles(j) % clw_data   = profiles(j) % clw(1) > 0.0_JPRB
        profiles(j) % s2m % t = psav(1,j)
        profiles(j) % s2m % q = psav(2,j) * q_mixratio_to_ppmv
        profiles(j) % s2m % p = psav(3,j)
        profiles(j) % s2m % u = psav(4,j)
        profiles(j) % s2m % v = psav(5,j)
        profiles(j) % skin % t       = pssv(1,j)
        profiles(j) % skin % fastem  = pssv(2:6,j)
        profiles(j) % skin % surftype= ksurf(j)
        profiles(j) % ctp            = pcv(1,j)
        profiles(j) % cfraction      = pcv(2,j)
        profiles(j) % zenangle       = pangl(j)
        profiles(j) % azangle        = 0.0_JPRB
     end do

     allocate( transmission % tau_surf (nchannels ) ,stat= alloc_status(2))
     allocate( transmission % tau_layer  (coef_pointer % nlevels , nchannels ) ,stat= alloc_status(3))
     allocate( transmission % od_singlelayer  (coef_pointer % nlevels , nchannels ) ,stat= alloc_status(4))
     ! allocate radiance results arrays with number of channels
     allocate( radiance % clear    ( nchannels ) ,stat= alloc_status(5))
     allocate( radiance % cloudy   ( nchannels ) ,stat= alloc_status(6))
     allocate( radiance % total    ( nchannels ) ,stat= alloc_status(7))
     allocate( radiance % bt       ( nchannels ) ,stat= alloc_status(8))
     allocate( radiance % bt_clear ( nchannels ) ,stat= alloc_status(9))
     allocate( radiance % upclear  ( nchannels ) ,stat= alloc_status(10))
     allocate( radiance % dnclear  ( nchannels ) ,stat= alloc_status(11))
     allocate( radiance % reflclear( nchannels ) ,stat= alloc_status(12))
     allocate( radiance % overcast ( coef_pointer % nlevels, nchannels ) ,stat= alloc_status(13))
     allocate( radiance % downcld  ( coef_pointer % nlevels, nchannels ) ,stat= alloc_status(14))
     allocate( radiance % out      ( nbtout ) ,stat= alloc_status(15))
     allocate( radiance % out_clear( nbtout ) ,stat= alloc_status(16))
     allocate( radiance % total_out( nbtout ) ,stat= alloc_status(17))
     allocate( radiance % clear_out( nbtout ) ,stat= alloc_status(18))
     If( any(alloc_status /= 0) ) then
        ifail(:,:) = 20
        Write( errMessage, '( "mem allocation 3 error")' )
        Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
        Return
     End If

     ! initialise downwelling radiance in case of
     ! "not cloudy" calculation condition
     radiance % downcld(:,:) = 0._JPRB

     where ( emissivity(:) < 0.001_JPRB )
        calcemis(:) = .true.
     elsewhere
        calcemis(:) = .false.
     endwhere


     call rttov_direct(  &
          errorstatus,  & ! out
          nfrequencies, & ! in
          nchannels,    & ! in
          nbtout,       & ! in
          nprofiles,    & ! in
          channels,     & ! in
          polarisations,& ! in
          lprofiles,    & ! in
          profiles,     & ! in
          coef_pointer, & ! in
          lcloud,       & ! in
          calcemis,     & ! in
          emissivity,   & ! inout
          transmission, & ! out
          radiance  ) ! inout

     do j = 1, knpf
        If( errorstatus(j) == errorstatus_fatal ) then
           ifail(j,:) = 20 ! unphysical profile
        Else If( errorstatus(j) == errorstatus_warning ) then
           ifail(j,:) = 11 ! outside profile limits
        Else
           ifail(j,:) = 0
        End If
     End Do

     !
     prad(:)  = radiance % total_out(:)   ! radiance
     ptb(:)   = radiance % out(:)     ! BT

     do j = 1 , nchannels
        jpol = polarisations(j,2)
        pemis(jpol)            = emissivity(j)
        tausfc(jpol)           = transmission % tau_surf(j)
        rado(jpol)             = radiance % cloudy(j)
        radov(jpol,2*njplev+1) = radiance % upclear  (j)
        radov(jpol,2*njplev+2) = radiance % reflclear(j)

        tau(jpol,:)                    = transmission % tau_layer(:,j)
        radov(jpol,1:njplev)           = radiance % overcast (:,j)
        radov(jpol,njplev+1:2*njplev)  = radiance % downcld  (:,j)

     enddo


     do j = 1, knpf
        deallocate( profiles(j) % p  ,stat= alloc_status(1))
        deallocate( profiles(j) % t  ,stat= alloc_status(2))
        deallocate( profiles(j) % q  ,stat= alloc_status(3))
        deallocate( profiles(j) % o3 ,stat= alloc_status(4))
        deallocate( profiles(j) % clw,stat= alloc_status(5))
        If( any(alloc_status /= 0) ) then
           ifail(:,:) = 20
           Write( errMessage, '( "mem deallocation error")' )
           Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
           Return
        End If
     end do

     deallocate( lprofiles     ,stat= alloc_status(1))
     deallocate( channels      ,stat= alloc_status(2))
     deallocate( polarisations ,stat= alloc_status(3))
     deallocate( emissivity    ,stat= alloc_status(4))
     deallocate( calcemis      ,stat= alloc_status(5))

     deallocate( transmission % tau_surf  ,stat= alloc_status(6))
     deallocate( transmission % tau_layer ,stat= alloc_status(7))
     deallocate( transmission % od_singlelayer ,stat= alloc_status(8))
     deallocate( radiance % clear    ,stat= alloc_status(9))
     deallocate( radiance % cloudy   ,stat= alloc_status(10))
     deallocate( radiance % total    ,stat= alloc_status(11))
     deallocate( radiance % bt       ,stat= alloc_status(12))
     deallocate( radiance % bt_clear ,stat= alloc_status(13))
     deallocate( radiance % upclear  ,stat= alloc_status(14))
     deallocate( radiance % dnclear  ,stat= alloc_status(15))
     deallocate( radiance % reflclear,stat= alloc_status(16))
     deallocate( radiance % overcast ,stat= alloc_status(17))
     deallocate( radiance % downcld  ,stat= alloc_status(18))
     deallocate( radiance % out      ,stat= alloc_status(19))
     deallocate( radiance % out_clear,stat= alloc_status(20))
     deallocate( radiance % total_out,stat= alloc_status(21))
     deallocate( radiance % clear_out,stat= alloc_status(22))
     If( any(alloc_status /= 0) ) then
        ifail(:,:) = 20
        Write( errMessage, '( "mem deallocation error")' )
        Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
        Return
     End If

  RETURN


END SUBROUTINE RTTOV
