Subroutine Rttov_cld_k  ( &
     & errorstatus,     &! out
     & nfrequencies,    &! in
     & nchannels,       &! in
     & nbtout,          &! in
     & nprofiles,       &! in
     & channels,        &! in
     & polarisations,   &! in
     & lprofiles,       &! in
     & profiles,        &! in
     & cld_profiles,    &! in
     & coef,            &! in
     & switchrad,       &! in
     & calcemis,        &! in
     & emissivity,      &! inout
     & profiles_k ,     &! inout
     & cld_profiles_k , &! inout
     & emissivity_k ,   &! inout
     & cld_radiance)     ! inout

  ! Description:
  ! to compute microwave multi-channel radiances and brightness
  ! temperatures for many profiles per call in a cloudy sky.
  !
  ! According to the argument switchrad the main input total or bt is used
  ! switchrad == true    bt is the input, brightness temperature
  ! switchrad == false   total is the input, radiance
  ! The AD outputs cld_profiles_ad, profiles_ad and emissivity_ad should be
  ! allocated and initialised before calling the subroutine
  !
  ! Copyright:
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2003, EUMETSAT, All Rights Reserved.
  !
  ! Method :
  ! ------
  ! According to the argument switchrad the main input total or bt is used
  ! switchrad == true    bt is the input, brightness temperature
  ! switchrad == false   total is the input, radiance
  !
  ! The K outputs cld_profiles_k , profiles_k  and emissivity_k  should be
  ! allocated and initialised before calling the subroutine
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       09/2003   Initial version (F. Chevallier) Note not based on RTTOV-7 code
  !  1.1       08/01/04  Added polarisation (S English)
  !  1.2       06/10/04  Change stop to return (J Cameron)
  !  1.3       29/03/05  Add end of header comment (J. Cameron)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  ! Declarations:

  Use rttov_const, Only :   &
       & sensor_id_mw        ,&
       & errorstatus_success ,&
       & errorstatus_fatal   ,&
       & overlap_scheme

  Use rttov_types, Only :    &
       & rttov_coef           ,&
       & geometry_Type        ,&
       & profile_Type         ,&
       & profile_cloud_Type   ,&
       & radiance_cloud_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_cld_ad.interface"
#include "rttov_errorreport.interface"

  !subroutine arguments:
  Integer(Kind=jpim),        Intent(in)    :: nfrequencies
  Integer(Kind=jpim),        Intent(in)    :: nchannels
  Integer(Kind=jpim),        Intent(in)    :: nbtout
  Integer(Kind=jpim),        Intent(in)    :: nprofiles
  Integer(Kind=jpim),        Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),        Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),        Intent(in)    :: lprofiles(nfrequencies)
  Logical,                   Intent(in)    :: switchrad  ! true if input is BT
  Type(profile_Type),        Intent(inout) :: profiles(nprofiles)
  Type(profile_cloud_Type),  Intent(in)    :: cld_profiles(nprofiles)
  Type(rttov_coef),          Intent(in)    :: coef
  Logical,                   Intent(in)    :: calcemis(nchannels)
  Real(Kind=jprb),           Intent(inout) :: emissivity(nchannels)
  Type(radiance_cloud_type), Intent(inout) :: cld_radiance! in because of meme allocation
  Type(profile_Type),        Intent(inout) :: profiles_k(nchannels)
  Type(profile_cloud_Type),  Intent(inout) :: cld_profiles_k(nchannels)
  Real(Kind=jprb),           Intent(inout) :: emissivity_k(nchannels)
  Integer(Kind=jpim),        Intent(out)   :: errorstatus(nprofiles)

  !local variables:
  Integer(Kind=jpim) :: i, j, jj, k, istart, istop, nchan, nprof ,nfreq
  Integer(Kind=jpim) :: kchan, kstart, kstop, kemis
  Integer(Kind=jpim) :: kbtout, jstart, jstop, jcount
  Integer(Kind=jpim)         :: kpol(nchannels/nprofiles,3)
  Integer(Kind=jpim) :: kchannels(nfrequencies/nprofiles)
  Logical            :: kcalcemis(nchannels/nprofiles)
  Real(Kind=jprb)    :: kemissivity(nchannels/nprofiles)
  Integer(Kind=jpim) :: freq
  Type(geometry_Type)   :: angles(nprofiles)    ! geometry angles
  Type(profile_Type)         :: profiles_ad(1)
  Type(profile_cloud_Type)   :: cld_profiles_ad(1)
  Type(radiance_cloud_type)  :: cld_rad
  Type(radiance_cloud_type)  :: cld_radiance_ad
  Real(Kind=jprb)            :: emissivity_ad(nchannels/nprofiles)
  Integer(Kind=jpim)         :: lprof(nfrequencies/nprofiles)
  Real(Kind=jprb)            :: total_ref(nchannels), bt_ref(nchannels)
  Character (len=80) :: errMessage
  Character (len=11) :: NameOfRoutine = 'rttov_cld_k'

  !- End of header --------------------------------------------------------

  errorstatus(:) = errorstatus_success

  nprof = 1
  nfreq = nfrequencies/nprofiles
  kbtout = nbtout/nprofiles
  kchan = nchannels/nprofiles

  Do j = 1, nprof
     profiles_ad(j) % nlevels =  coef % nlevels
     Allocate( profiles_ad(j) % p  ( coef % nlevels ) )
     Allocate( profiles_ad(j) % t  ( coef % nlevels ) )
     Allocate( profiles_ad(j) % q  ( coef % nlevels ) )
     Allocate( profiles_ad(j) % o3 ( coef % nlevels ) )
     Allocate( profiles_ad(j) % clw( coef % nlevels ) )
  End do
  Do j = 1, nprof
     cld_profiles_ad(j) % nlevels =  cld_profiles(1) % nlevels
     Allocate( cld_profiles_ad(j) % p  ( cld_profiles(1) % nlevels ) )
     Allocate( cld_profiles_ad(j) % ph ( cld_profiles(1) % nlevels+1 ) )
     Allocate( cld_profiles_ad(j) % t  ( cld_profiles(1) % nlevels ) )
     Allocate( cld_profiles_ad(j) % cc ( cld_profiles(1) % nlevels ) )
     Allocate( cld_profiles_ad(j) % clw( cld_profiles(1) % nlevels ) )
     Allocate( cld_profiles_ad(j) % ciw( cld_profiles(1) % nlevels ) )
  End do


  istop = 0
  kstop = 0
  jstop = 0
  lprof(:) = 1
  kpol(:,:) = 0
  Do i = 1, nprofiles

    istart  = istop + 1
    istop = istart + kchan - 1
    kstart = kstop + 1
    kstop = kstart + nfreq - 1
    nchan = kchan
    jstart = jstop + 1
    jstop = jstart + kbtout -1
    ! transfer arrays to single profile arrays
    kpol(1:nchan,:) = polarisations(1:nchan,:) ! Note assumes first array is same for every prof
    kchannels(1:nfreq) = channels(kstart:kstop)
    kemissivity(1:nchan) = emissivity(istart:istop)
    kcalcemis(1:nchan) = calcemis(istart:istop)

    ! Allocate structure with arrays dimensioned to nchan
    Allocate( cld_radiance_ad % clear    ( nchan ) )
    Allocate( cld_radiance_ad % clear_out( nchan ) )
    Allocate( cld_radiance_ad % cloudy   ( nchan ) )
    Allocate( cld_radiance_ad % total    ( nchan ) )
    Allocate( cld_radiance_ad % total_out( nchan ) )
    Allocate( cld_radiance_ad % bt       ( nchan ) )
    Allocate( cld_radiance_ad % bt_clear ( nchan ) )
    Allocate( cld_radiance_ad % out_clear( nchan ) )
    Allocate( cld_radiance_ad % out      ( nchan ) )
    Allocate( cld_radiance_ad % upclear  ( nchan ) )
    Allocate( cld_radiance_ad % reflclear( nchan ) )
    Allocate( cld_radiance_ad % overcast ( cld_profiles(1) % nlevels, nchan ) )
    Allocate( cld_radiance_ad % downcld  ( cld_profiles(1) % nlevels, nchan ) )
    Allocate( cld_radiance_ad % cldemis  ( cld_profiles(1) % nlevels, nchan ) )
    Allocate( cld_radiance_ad % wtoa     ( cld_profiles(1) % nlevels, nchan ) )
    Allocate( cld_radiance_ad % wsurf    ( cld_profiles(1) % nlevels, nchan ) )
    Allocate( cld_radiance_ad % cs_wtoa  ( nchan ) )
    Allocate( cld_radiance_ad % cs_wsurf ( nchan ) )
    Allocate( cld_rad % clear    ( nchan ) )
    Allocate( cld_rad % clear_out( nchan ) )
    Allocate( cld_rad % cloudy   ( nchan ) )
    Allocate( cld_rad % total    ( nchan ) )
    Allocate( cld_rad % total_out( nchan ) )
    Allocate( cld_rad % bt       ( nchan ) )
    Allocate( cld_rad % bt_clear ( nchan ) )
    Allocate( cld_rad % out      ( nchan ) )
    Allocate( cld_rad % out_clear( nchan ) )
    Allocate( cld_rad % upclear  ( nchan ) )
    Allocate( cld_rad % dnclear  ( nchan ) )
    Allocate( cld_rad % reflclear( nchan ) )
    Allocate( cld_rad % overcast ( cld_profiles(1) % nlevels, nchan ) )
    Allocate( cld_rad % downcld  ( cld_profiles(1) % nlevels, nchan ) )
    Allocate( cld_rad % cldemis  ( cld_profiles(1) % nlevels, nchan ) )
    Allocate( cld_rad % wtoa     ( cld_profiles(1) % nlevels, nchan ) )
    Allocate( cld_rad % wsurf    ( cld_profiles(1) % nlevels, nchan ) )
    Allocate( cld_rad % cs_wtoa  ( nchan ) )
    Allocate( cld_rad % cs_wsurf ( nchan ) )

    jcount = jstart-1
    kemis = istart
    Do j = istart, istop
      jcount = jcount + 1
      !
      ! Initialise AD input
      !
      cld_radiance_ad % clear    (:) = 0._JPRB
      cld_radiance_ad % clear_out(:) = 0._JPRB
      cld_radiance_ad % cloudy   (:) = 0._JPRB
      cld_radiance_ad % total    (:) = 0._JPRB
      cld_radiance_ad % total_out(:) = 0._JPRB
      cld_radiance_ad % bt       (:) = 0._JPRB
      cld_radiance_ad % bt_clear (:) = 0._JPRB
      cld_radiance_ad % out      (:) = 0._JPRB
      cld_radiance_ad % out_clear(:) = 0._JPRB
      cld_radiance_ad % upclear  (:) = 0._JPRB
      cld_radiance_ad % reflclear(:) = 0._JPRB
      cld_radiance_ad % overcast (:,:) = 0._JPRB
      cld_radiance_ad % downcld  (:,:) = 0._JPRB
      cld_radiance_ad % cldemis  (:,:) = 0._JPRB
      cld_radiance_ad % wtoa     (:,:) = 0._JPRB
      cld_radiance_ad % wsurf    (:,:) = 0._JPRB
      cld_radiance_ad % cs_wtoa  (:) = 0._JPRB
      cld_radiance_ad % cs_wsurf (:) = 0._JPRB
      If ( switchrad ) Then
        cld_radiance_ad % out    (j - istart + 1) = 1._JPRB
      Else
        cld_radiance_ad % total_out (j - istart + 1) = 1._JPRB
      Endif
      !
      ! Initialise AD output
      !
      profiles_ad(1) % ozone_Data = .False.  ! no meaning
      profiles_ad(1) % co2_Data   = .False.  ! no meaning
      profiles_ad(1) % clw_Data   = .False.  ! no meaning
      profiles_ad(1) % zenangle   = -1       ! no meaning

      ! increments for atmospheric variables
      profiles_ad(1) % p(:)   = 0._JPRB ! no AD on pressure levels
      profiles_ad(1) % t(:)   = 0._JPRB ! temperarure
      profiles_ad(1) % o3(:)  = 0._JPRB ! O3 ppmv
      profiles_ad(1) % clw(:) = 0._JPRB ! clw
      profiles_ad(1) % q(:)   = 0._JPRB ! WV

      ! increments for air surface variables
      profiles_ad(1) % s2m % t = 0._JPRB!  temperarure
      profiles_ad(1) % s2m % q = 0._JPRB !  WV
      profiles_ad(1) % s2m % o = 0._JPRB !  O3
      profiles_ad(1) % s2m % p = 0._JPRB!  pressure
      profiles_ad(1) % s2m % u = 0._JPRB!  wind components
      profiles_ad(1) % s2m % v = 0._JPRB!  wind components

      ! increments for skin variables
      profiles_ad(1) % skin % surftype = -1  ! no meaning
      profiles_ad(1) % skin % t        = 0._JPRB  ! on temperarure
      profiles_ad(1) % skin % fastem   = 0._JPRB

      ! increments for cloud variables
      profiles_ad(1) % ctp       = 0._JPRB  ! pressure
      profiles_ad(1) % cfraction = 0._JPRB  ! cloud fraction

      ! Cloud profiles
      cld_profiles_ad(1) % p  (:) = 0._JPRB
      cld_profiles_ad(1) % ph (:) = 0._JPRB
      cld_profiles_ad(1) % t  (:) = 0._JPRB
      cld_profiles_ad(1) % cc (:) = 0._JPRB
      cld_profiles_ad(1) % clw(:) = 0._JPRB
      cld_profiles_ad(1) % ciw(:) = 0._JPRB

      ! surface emissivity
      emissivity_ad(:) = 0._JPRB

      Call Rttov_cld_ad  ( &
         & errorstatus(i),       &! out
         & nfreq,                &! in
         & nchan,                &! in
         & kbtout,               &! in
         & nprof,                &! in
         & kchannels,            &! in
         & kpol,                 &! in
         & lprof,                &! in
         & profiles(i),          &! in
         & cld_profiles(i),      &! in
         & coef,                 &! in
         & switchrad,            &! in
         & kcalcemis,            &! in
         & kemissivity,          &! inout
         & profiles_ad,          &! inout
         & cld_profiles_ad,      &! inout
         & emissivity_ad,        &! inout
         & cld_rad,              &! inout
         & cld_radiance_ad )      ! inout

      If ( errorstatus(i) == errorstatus_fatal ) Then
         Write( errMessage, '( "error in rttov_cld_ad")' )
         Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
         Return
      End If

      !
      ! Save derivatives for that profile and that channel in K matrix
      !

      If (jcount <= jstop) Then
       ! increments for atmospheric variables
       profiles_k(jcount) % t(:)   = profiles_ad(1) % t(:)
       profiles_k(jcount) % o3(:)  = profiles_ad(1) % o3(:)
       profiles_k(jcount) % q(:)   = profiles_ad(1) % q(:)

       ! increments for air surface variables
       profiles_k(jcount) % s2m % t = profiles_ad(1) % s2m % t
       profiles_k(jcount) % s2m % q = profiles_ad(1) % s2m % q
       profiles_k(jcount) % s2m % o = profiles_ad(1) % s2m % o
       profiles_k(jcount) % s2m % p = profiles_ad(1) % s2m % p
       profiles_k(jcount) % s2m % u = profiles_ad(1) % s2m % u
       profiles_k(jcount) % s2m % v = profiles_ad(1) % s2m % v

       ! increments for skin variables
       profiles_k(jcount) % skin % t        = profiles_ad(1) % skin % t
       profiles_k(jcount) % skin % fastem   = profiles_ad(1) % skin % fastem

      ! Cloud profiles
       cld_profiles_k(jcount) % p  (:) = cld_profiles_ad(1) % p  (:)
       cld_profiles_k(jcount) % ph (:) = cld_profiles_ad(1) % ph (:)
       cld_profiles_k(jcount) % t  (:) = cld_profiles_ad(1) % t  (:)
       cld_profiles_k(jcount) % cc (:) = cld_profiles_ad(1) % cc (:)
       cld_profiles_k(jcount) % clw(:) = cld_profiles_ad(1) % clw(:)
       cld_profiles_k(jcount) % ciw(:) = cld_profiles_ad(1) % ciw(:)
      Endif

       ! increments for surface emissivity
       If ( kemis <= istop )Then
         jj = kpol(j-istart+1,2)
         Do k = 1 , kpol(jj,3)
           emissivity_k(kemis) = emissivity_ad(kemis-istart+1)
           kemis = kemis + 1
         Enddo
       Endif

       ! Save cloudy radiance/brightness temperature
       total_ref(istart:istop) = cld_rad % total (1:nchan)
       bt_ref   (istart:istop) = cld_rad % bt    (1:nchan)

    Enddo

    ! Deallocate structure with arrays dimensioned to nchan
    Deallocate( cld_radiance_ad % clear     )
    Deallocate( cld_radiance_ad % clear_out )
    Deallocate( cld_radiance_ad % cloudy    )
    Deallocate( cld_radiance_ad % total     )
    Deallocate( cld_radiance_ad % total_out )
    Deallocate( cld_radiance_ad % bt        )
    Deallocate( cld_radiance_ad % bt_clear  )
    Deallocate( cld_radiance_ad % out       )
    Deallocate( cld_radiance_ad % out_clear )
    Deallocate( cld_radiance_ad % upclear   )
    Deallocate( cld_radiance_ad % reflclear )
    Deallocate( cld_radiance_ad % overcast  )
    Deallocate( cld_radiance_ad % downcld   )
    Deallocate( cld_radiance_ad % cldemis   )
    Deallocate( cld_radiance_ad % wtoa      )
    Deallocate( cld_radiance_ad % wsurf     )
    Deallocate( cld_radiance_ad % cs_wtoa   )
    Deallocate( cld_radiance_ad % cs_wsurf  )
    Deallocate( cld_rad % clear     )
    Deallocate( cld_rad % clear_out )
    Deallocate( cld_rad % cloudy    )
    Deallocate( cld_rad % total     )
    Deallocate( cld_rad % total_out )
    Deallocate( cld_rad % bt        )
    Deallocate( cld_rad % bt_clear  )
    Deallocate( cld_rad % out       )
    Deallocate( cld_rad % out_clear )
    Deallocate( cld_rad % upclear   )
    Deallocate( cld_rad % dnclear   )
    Deallocate( cld_rad % reflclear )
    Deallocate( cld_rad % overcast  )
    Deallocate( cld_rad % downcld   )
    Deallocate( cld_rad % cldemis   )
    Deallocate( cld_rad % wtoa      )
    Deallocate( cld_rad % wsurf     )
    Deallocate( cld_rad % cs_wtoa   )
    Deallocate( cld_rad % cs_wsurf  )
  Enddo

  Do j = 1, nchan
    profiles_k(j) % p(:)   = 0._JPRB
    profiles_k(j) % clw(:) = 0._JPRB
    profiles_k(j) % ctp       = 0._JPRB
    profiles_k(j) % cfraction = 0._JPRB
  Enddo

  ! Get correct values of cloudy radiance/brightness temperature
  cld_radiance % total (:) = total_ref(:)
  cld_radiance % bt    (:) = bt_ref   (:)


  Do j = 1, nprof
     Deallocate( profiles_ad(j) % p  )
     Deallocate( profiles_ad(j) % t  )
     Deallocate( profiles_ad(j) % q  )
     Deallocate( profiles_ad(j) % o3 )
     Deallocate( profiles_ad(j) % clw)
  End do
  Do j = 1, nprof
     Deallocate( cld_profiles_ad(j) % p  )
     Deallocate( cld_profiles_ad(j) % ph )
     Deallocate( cld_profiles_ad(j) % t  )
     Deallocate( cld_profiles_ad(j) % cc )
     Deallocate( cld_profiles_ad(j) % clw)
     Deallocate( cld_profiles_ad(j) % ciw)
  End do


End Subroutine Rttov_cld_k
