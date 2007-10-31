Subroutine Rttov_cld_ad ( &
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
     & profiles_ad,     &! inout
     & cld_profiles_ad, &! inout
     & emissivity_ad,   &! inout
     & cld_radiance,    &! inout
     & cld_radiance_ad ) ! inout
  !
  ! Description:
  ! to compute multi-channel radiances and brightness
  ! temperatures for many profiles per call in a cloudy sky.
  ! Note that cld_radiance_ad can be used for all its structure elements
  ! In normal case the element total or bt is the only one initialised but
  ! for some particular cases like for rttov_cld_ad some other elements
  ! have been already init.
  ! According to the argument switchrad the main input total or bt is used
  ! switchrad == true    bt is the input, brightness temperature
  ! switchrad == false   total is the input, radiance
  !
  ! The AD outputs cld_profiles_ad, profiles_ad and emissivity_ad should be
  ! allocated and initialised before calling the subroutine
  !
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
  !    Copyright 2002, EUMETSAT, All Rights Reserved.
  !
  ! Method:
  ! See Chevallier, F., P. Bauer, G. A. Kelly, C. Jakob,
  ! and T. McNally, 2001 Model clouds over oceans as seen
  ! from space: comparison with HIRS/2 and MSU radiances.
  ! J. Climate 14 4216-4229.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       03/2001   Initial version (F. Chevallier)
  !  1.1     19/7/2001   Version for testing RTTOV-7 (R. Saunders)
  !  2.0    01/12/2002   New F90 code with structures (P Brunel A Smith)
  !  2.1    02/01/2002   Comments added (R Saunders)
  !  2.2    08/01/04     Added polarisation (S English)
  !  2.3    06/10/04     Add errorstatus to rttov_emiscld_ad call (J Cameron)
  !  2.4    29/03/2005   Add end of header comment (J. Cameron)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Parameters:

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
       & transmission_Type    ,&
       & radiance_Type        ,&
       & radiance_cloud_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None
#include "rttov_errorreport.interface"
#include "rttov_direct.interface"
#include "rttov_ad.interface"
#include "rttov_intex.interface"
#include "rttov_intex_ad.interface"
#include "rttov_emiscld.interface"
#include "rttov_emiscld_ad.interface"
#include "rttov_aitosu.interface"
#include "rttov_aitosu_ad.interface"
#include "rttov_calcbt.interface"
#include "rttov_calcbt_ad.interface"
#include "rttov_setgeometry.interface"
#include "rttov_calcpolarisation.interface"
#include "rttov_calcpolarisation_ad.interface"

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


  Type(profile_Type),        Intent(inout) :: profiles_ad(nprofiles)
  Type(profile_cloud_Type),  Intent(inout) :: cld_profiles_ad(nprofiles)
  Real(Kind=jprb),           Intent(inout) :: emissivity_ad(nchannels)
  Type(radiance_cloud_type), Intent(inout) :: cld_radiance_ad
  Integer(Kind=jpim),        Intent(out)   :: errorstatus(nprofiles)

  !local variables:
  Logical :: addcloud
  Integer(Kind=jpim) :: nwp_levels  ! number of levels for NWP profiles
  Integer(Kind=jpim) :: nrt_levels  ! number of levels for RTTOV_direct integration
  Integer(Kind=jpim) :: jl     ! loop indice
  Integer(Kind=jpim) :: jk     ! loop indice
  Integer(Kind=jpim) :: ipf    ! loop indice
  Integer(Kind=jpim) :: alloc_status(20)
  Integer(Kind=jpim) :: freq
  Type(geometry_Type)   :: angles(nprofiles)    ! geometry angles
  Character (len=80) :: errMessage
  Character (len=12) :: NameOfRoutine = 'rttov_cld_ad'

  Real(Kind=jprb)     :: null_press(coef%nlevels)
  Real(Kind=jprb)     :: total_ref(nchannels), bt_ref(nchannels)
  !Real(Kind=jprb)     :: tau_surf(nchannels)
  !Real(Kind=jprb)     :: tau_layer(coef%nlevels,nchannels)
  Type(radiance_Type) :: radiance
  Type(radiance_Type) :: radiance_ad
  Type(transmission_Type) :: transmission
  Type(transmission_Type) :: transmission_ad

  !- End of header --------------------------------------------------------

  errorstatus(:)  = errorstatus_success
  alloc_status(:) = 0

  ! allocate radiance results arrays with number of channels
  radiance % clear     => cld_radiance % clear
  radiance % clear_out => cld_radiance % clear_out
  radiance % cloudy    => cld_radiance % cloudy
  radiance % total     => cld_radiance % total
  radiance % total_out => cld_radiance % total_out
  radiance % bt        => cld_radiance % bt
  radiance % bt_clear  => cld_radiance % bt_clear
  radiance % out       => cld_radiance % out
  radiance % out_clear => cld_radiance % out_clear
  radiance % upclear   => cld_radiance % upclear
  radiance % dnclear   => cld_radiance % dnclear
  radiance % reflclear => cld_radiance % reflclear
  allocate( radiance % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(1))
  allocate( radiance % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(2))

 ! allocate transmission arrays
  Allocate( transmission % tau_surf      ( nchannels ) ,stat= alloc_status(3))
  Allocate( transmission % tau_layer     ( coef % nlevels, nchannels ) ,stat= alloc_status(4))
  Allocate( transmission % od_singlelayer( coef % nlevels, nchannels ) ,stat= alloc_status(5))

  If( any(alloc_status /= 0) ) then
     errorstatus(:) = errorstatus_fatal
     Write( errMessage, '( "mem allocation error")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  ! Repeat Direct Code
  !*         1.   Gas absorption

  addcloud = .true.

  ! No calculation of CLW absorption inside "classical" RTTOV
  if ( any(.not.profiles(:)%clw_data) ) then
     ! warning message
     profiles(:)%clw_data = .false.
  End If

  ! inside "classical" RTTOV profile should be considered clear
  if ( any(profiles(:)%cfraction > 0._JPRB) ) then
     ! warning message
     profiles(:)%cfraction = 0._JPRB
  End If
  Call rttov_direct( &
        & errorstatus,    &! out
        & nfrequencies,   &! in
        & nchannels,      &! in
        & nbtout,         &! in
        & nprofiles,      &! in
        & channels,       &! in
        & polarisations,  &! in
        & lprofiles,      &! in
        & profiles,       &! in
        & coef,           &! in
        & addcloud,       &! in
        & calcemis,       &! in
        & emissivity,     &! inout
        & transmission,   &! inout
        & radiance     )   ! inout
  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_direct")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  ! In order to make minimum changes inside the Cloud code
  ! we will pass some structure elements as arguments to the
  ! routines
  ! For example: number of levels   cld_profiles(1)%nlevels


  ! Be carefull that inside routines local arrays have a different
  ! shape as structures (see rttov_types)
  ! For RTTOV8 all arrays with channels and levels dimensions have the
  ! following shape (nlevels, nchannels). This is the reverse


  ! for local arrays of cloud routines.


  !*         2.   Interpolate cloud contribution to model levels
  ! compute arrays overcast, downcld of type cld_radiancedata
  nwp_levels   = cld_profiles(1) % nlevels
  nrt_levels   = profiles(1)     % nlevels

  DO jl = 1, nchannels
     freq = polarisations(jl,2)
     ipf = lprofiles(freq)
     call rttov_intex( &
        & nrt_levels,        &! in
        & nwp_levels,        &! in
        & profiles(ipf)     % p,      &! in
        & cld_profiles(ipf) % p,      &! in
        & radiance     % overcast(1:nrt_levels,jl),      &! in
        & cld_radiance % overcast(1:nwp_levels,jl)  )     ! inout

     call rttov_intex( &
        & nrt_levels,        &! in
        & nwp_levels,        &! in
        & profiles(ipf)     % p,      &! in
        & cld_profiles(ipf) % p,      &! in
        & radiance     % downcld(1:nrt_levels,jl),      &! in
        & cld_radiance % downcld(1:nwp_levels,jl)  )     ! inout
  End Do

  !*         3.   Calculate cloud emissivity
  call rttov_emiscld(  &
        & errorstatus,       &! out
        & nfrequencies,      &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & nwp_levels,        &! in
        & channels,          &! in
        & polarisations,     &! in
        & lprofiles,         &! in
        & profiles,          &! in  (surftype and zenangle)
        & coef,              &! in  (frequencies mw/ir/hi)
        & cld_profiles,      &! in
        & cld_radiance)       ! inout  (cldemis part only)
  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_emiscld")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  !*         4.   Compute the weights of the cloud layers
  !               ---------------------------------------
  call rttov_aitosu( &
        & nfrequencies,      &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & nwp_levels,        &! in
        & polarisations,     &! in
        & lprofiles,         &! in
        & overlap_scheme,    &! in
        & cld_profiles,      &! in  (cloud cover)
        & cld_radiance  )     ! inout  (cldemis input and
           ! cs_wtao, cs_wsurf, wtao, wsurf in output)

  !*         5.   Integrate *rt* equation.
  !               --------- ---- --------
  ! clear-sky contribution
  ! without the surface reflection
  cld_radiance % total (:) = cld_radiance % cs_wtoa(:) *&
        & cld_radiance % upclear (:)
  ! with the surface-reflected clear-sky downward radiance
  cld_radiance % total (:) = cld_radiance % total (:) +&
        & cld_radiance % cs_wsurf(:) *&
        & cld_radiance % cs_wtoa(:)  *&
        & cld_radiance % reflclear (:)

  !
  ! cloud contribution
  DO jk = 1, nwp_levels
     ! cloud upward emission
     cld_radiance % total (:) = cld_radiance % total (:) +&
           & cld_radiance % wtoa(jk,:)  *&
           & cld_radiance % overcast(jk,:)

     ! cloud downward emission, reflected at the surface
     cld_radiance % total (:) = cld_radiance % total (:) +&
           & cld_radiance % wsurf(jk,:)  *&
           & cld_radiance % cs_wtoa(:)   *&
           & cld_radiance % downcld(jk,:)
  END DO

  ! Remember that radiance struture is mainly pointing on cld_radiance
  ! so we can use radiance struture for conversion of radiance to brightness temperatue.
  Call rttov_calcbt( &
    &    nfrequencies,    &! in
    &    nchannels,       &! in
    &    channels,        &! in
    &    polarisations,   &! in
         & coef,            &! in
         & radiance        ) ! inout

  ! Save cloudy radiance/brightness temperature since they are overwritten in rttov_ad
  total_ref(:) = cld_radiance % total (:)
  bt_ref   (:) = cld_radiance % bt    (:)

  !-----------------------------------------------------------
  ! AD code
  !-----------------------------------------------------------

  ! Initialise AD variables
  ! allocate radiance AD results arrays with number of channels
  radiance_ad % clear     => cld_radiance_ad % clear
  radiance_ad % clear_out => cld_radiance_ad % clear_out
  radiance_ad % cloudy    => cld_radiance_ad % cloudy
  radiance_ad % total     => cld_radiance_ad % total
  radiance_ad % total_out => cld_radiance_ad % total_out
  radiance_ad % bt        => cld_radiance_ad % bt
  radiance_ad % out    => cld_radiance_ad % out
  radiance_ad % out_clear => cld_radiance_ad % out_clear
  radiance_ad % bt_clear  => cld_radiance_ad % bt_clear
  radiance_ad % upclear   => cld_radiance_ad % upclear
  radiance_ad % reflclear => cld_radiance_ad % reflclear
  allocate( radiance_ad % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(1))
  allocate( radiance_ad % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(2))
  allocate( transmission_ad % tau_surf      ( nchannels ) ,stat= alloc_status(3))
  allocate( transmission_ad % tau_layer     ( coef % nlevels, nchannels ) ,stat= alloc_status(4))
  allocate( transmission_ad % od_singlelayer( coef % nlevels, nchannels ) ,stat= alloc_status(5))

  If( any(alloc_status /= 0) ) then
     errorstatus(:) = errorstatus_fatal
     Write( errMessage, '( "mem allocation error")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If
  radiance_ad % overcast (:,:) = 0._JPRB
  radiance_ad % downcld  (:,:) = 0._JPRB
  transmission_ad % tau_surf       (:)   = 0._JPRB
  transmission_ad % tau_layer      (:,:) = 0._JPRB
  transmission_ad % od_singlelayer (:,:) = 0._JPRB

 If (coef % id_sensor == sensor_id_mw) Then
     Do ipf = 1, nprofiles
       Call rttov_setgeometry( &
          & profiles(ipf),   &! in
          & coef,            &! in
          & angles(ipf) )     ! out
     End Do
     !
     Call rttov_calcpolarisation( &
        & nfrequencies,      &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & angles,            &! in
        & channels,          &! in
        & polarisations,     &! in
        & lprofiles,         &! in
        & coef,              &! in
        & radiance        )   ! inout
     Call rttov_calcpolarisation_ad( &
        & nfrequencies,      &! in
        & nchannels,         &! in
        & nbtout,            &! in
        & profiles,          &! in
        & nprofiles,         &! in
        & angles,            &! in
        & channels,          &! in
        & polarisations,     &! in
        & lprofiles,         &! in
        & coef,              &! in
        & radiance_ad     )   ! inout
  Else
  radiance%out         = radiance%bt
        radiance%out_clear   = radiance%bt_clear
  radiance%total_out   = radiance%total
        radiance%clear_out   = radiance%clear
  radiance_ad%bt       = radiance_ad%out
        radiance_ad%bt_clear = radiance_ad%out_clear
  radiance_ad%total    = radiance_ad%total_out
        radiance_ad%clear    = radiance_ad%clear_out
  End If

! if input AD unit is temperature, convert it in radiance
  if ( switchrad )  then
     Call rttov_calcbt_ad( &
        & nfrequencies,      &! in
        & nchannels,         &! in
        & channels,          &! in
        & polarisations,     &! in
        & coef,              &! in
        & radiance,          &! in
        & radiance_ad     )   ! inout   output is only radiance_ad%total
  endif

  !*         5.   Integrate *rt* equation.
  !               --------- ---- --------
  ! cloud contribution
  DO jk = 1, nwp_levels
     ! cloud downward emission, reflected at the surface
     cld_radiance_ad%downcld(jk,:) = cld_radiance_ad%downcld(jk,:) +&
           & cld_radiance_ad % total (:) *&
           & cld_radiance %wsurf(jk,:) *&
           & cld_radiance %cs_wtoa(:)

     cld_radiance_ad%cs_wtoa(:)    = cld_radiance_ad%cs_wtoa(:)    +&
           & cld_radiance_ad % total (:) *&
           & cld_radiance %wsurf(jk,:)   *&
           & cld_radiance %downcld(jk,:)

     cld_radiance_ad%wsurf(jk,:)   = cld_radiance_ad%wsurf(jk,:)   +&
           & cld_radiance_ad % total (:) *&
           & cld_radiance %cs_wtoa(:)    *&
           & cld_radiance %downcld(jk,:)

     ! cloud upward emission
     cld_radiance_ad % wtoa(jk,:) = cld_radiance_ad % wtoa(jk,:) +&
           & cld_radiance_ad % total (:) *&
           & cld_radiance    % overcast(jk,:)

     cld_radiance_ad % overcast(jk,:) = cld_radiance_ad % overcast(jk,:) +&
           & cld_radiance_ad % total (:) *&
           & cld_radiance    % wtoa(jk,:)

  END DO

  ! with the surface-reflected clear-sky downward radiance
  cld_radiance_ad%cs_wsurf(:) = cld_radiance_ad%cs_wsurf(:) +&
        & cld_radiance_ad % total (:) *&
        & cld_radiance %cs_wtoa(:)    *&
        & cld_radiance %reflclear(:)

  cld_radiance_ad%cs_wtoa(:)  = cld_radiance_ad%cs_wtoa(:)  +&
        & cld_radiance_ad % total (:) *&
        & cld_radiance %cs_wsurf(:)   *&
        & cld_radiance %reflclear(:)

  cld_radiance_ad%reflclear(:) = cld_radiance_ad%reflclear(:) +&
        & cld_radiance_ad % total (:) *&
        & cld_radiance %cs_wsurf(:)   *&
        & cld_radiance %cs_wtoa(:)


  ! clear-sky contribution
  ! without the surface reflection
  cld_radiance_ad % cs_wtoa(:)  = cld_radiance_ad % cs_wtoa(:)  +&
        & cld_radiance_ad % total (:) *&
        & cld_radiance % upclear (:)

  cld_radiance_ad % upclear (:) = cld_radiance_ad % upclear (:) +&
        & cld_radiance_ad % total (:) *&
        & cld_radiance    % cs_wtoa(:)

  cld_radiance_ad % total (:) = 0._JPRB

  !*         4.   Compute the weights of the cloud layers
  !               ---------------------------------------
  call rttov_aitosu_ad( &
        & nfrequencies,      &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & nwp_levels,        &! in
        & polarisations,     &! in
        & lprofiles,         &! in
        & overlap_scheme,    &! in
        & cld_profiles,      &! in  (cloud cover)
        & cld_profiles_ad,   &! inout  (cloud cover updated)
        & cld_radiance ,     &! inout  (cldemis in input and
                              !   cs_wtao, cs_wsurf, wtao, wsurf in output are zeroed)
        & cld_radiance_ad  )  ! inout  (cldemis updated and
                              !   cs_wtao, cs_wsurf, wtao, wsurf in output are zeroed)


  !*         3.   Calculate cloud emissivity
  call rttov_emiscld_ad(  &
        & errorstatus,       &! out
        & nfrequencies,      &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & nwp_levels,        &! in
        & channels,          &! in
        & polarisations,     &! in
        & lprofiles,         &! in
        & profiles,          &! in  (surftype and zenangle)
        & coef,              &! in  (frequencies mw/ir/hi)
        & cld_profiles,      &! in
        & cld_profiles_ad,   &! inout
        & cld_radiance,      &! inout  (cldemis part only)
        & cld_radiance_ad)    ! inout  (cldemis part only)
  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_emiscld")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  !*         2.   Interpolate cloud contribution to model levels
  ! compute arrays overcast, downcld of type cld_radiancedata
  DO jl = 1, nchannels
     freq = polarisations(jl,2)
     ipf = lprofiles(freq)
     null_press(:) = 0._JPRB
     call rttov_intex_ad( &
        & nrt_levels,        &! in
        & nwp_levels,        &! in
        & null_press,        &! inout
        & cld_profiles_ad(ipf) % p,                     &! inout
        & radiance_ad     % downcld(1:nrt_levels,jl),   &! inout
        & cld_radiance_ad % downcld(1:nwp_levels,jl),   &! inout
        & profiles(ipf)     % p,                        &! in
        & cld_profiles(ipf) % p,                        &! in
        & radiance     % downcld(1:nrt_levels,jl),      &! in
        & cld_radiance % downcld(1:nwp_levels,jl)  )     ! out

     null_press(:) = 0._JPRB
     call rttov_intex_ad( &
        & nrt_levels,        &! in
        & nwp_levels,        &! in
        & null_press,        &! inout
        & cld_profiles_ad(ipf) % p,                      &! inout
        & radiance_ad     % overcast(1:nrt_levels,jl),   &! inout
        & cld_radiance_ad % overcast(1:nwp_levels,jl),   &! inout
        & profiles(ipf)     % p,                         &! in
        & cld_profiles(ipf) % p,                         &! in
        & radiance     % overcast(1:nrt_levels,jl),      &! in
        & cld_radiance % overcast(1:nwp_levels,jl)  )     ! out

  End Do


  ! Input of CLD_AD code are Bt increments for "total" (cloudy) and
  ! "clear" arrays
  ! Cloudy part has been considered above, now just consider the gas absorption
  ! and run rttov_ad in clear conditions
  ! Overcast arrays have been interpolated to rttov levels and are part on the
  ! input of rttov_ad. They are represented by structure elements
  ! upclear, reflclear, overcast and downcld
  if( switchrad ) then
     radiance_ad%out(:)    = radiance_ad%out_clear(:)
     radiance_ad%total(:) = 0._JPRB
     radiance_ad%cloudy(:)= 0._JPRB
     radiance_ad%clear(:) = 0._JPRB
     radiance_ad%out_clear(:)  = 0._JPRB
     radiance_ad%bt_clear(:)  = 0._JPRB
     radiance_ad%bt(:)  = 0._JPRB
  Else
     radiance_ad%total_out(:) = radiance_ad%clear_out(:)
     radiance_ad%out(:)    = 0._JPRB
     radiance_ad%cloudy(:)= 0._JPRB
     radiance_ad%clear(:) = 0._JPRB
     radiance_ad%out_clear(:)  = 0._JPRB
     radiance_ad%bt_clear(:)  = 0._JPRB
     radiance_ad%bt(:)  = 0._JPRB
  End If
  !*         1.   Gas absorption

  addcloud = .true.

  ! No calculation of CLW absorption inside "classical" RTTOV
  if ( any(.not.profiles(:)%clw_data) ) then
     ! warning message
     profiles(:)%clw_data = .false.
  End If

  Call rttov_ad( &
     & errorstatus,     &! out
     & nfrequencies,    &! in
     & nchannels,       &! in
     & nbtout,          &! in
     & nprofiles,       &! in
     & channels,        &! in
     & polarisations,   &! in
     & lprofiles,       &! in
     & profiles,        &! in
     & coef,            &! in
     & addcloud,        &! in
     & switchrad,       &! in
     & calcemis,        &! in
     & emissivity,      &! inout
     & profiles_ad,     &! inout
     & emissivity_ad,   &! inout
     & transmission,    &! inout
     & transmission_ad, &! inout
     & radiance,        &! inout
     & radiance_ad     ) ! inout

  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_ad")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  ! Get correct values of cloudy radiance/brightness temperature
  cld_radiance % total (:) = total_ref(:)
  cld_radiance % bt    (:) = bt_ref   (:)


  ! deallocate radiance structure for overcast and dowcld arrays
  deallocate( radiance % overcast    ,stat= alloc_status(1))
  deallocate( radiance % downcld     ,stat= alloc_status(2))
  deallocate( radiance_ad % overcast ,stat= alloc_status(3))
  deallocate( radiance_ad % downcld  ,stat= alloc_status(4))
  deallocate( transmission % tau_surf      ,stat= alloc_status(5))
  deallocate( transmission % tau_layer     ,stat= alloc_status(6))
  deallocate( transmission % od_singlelayer,stat= alloc_status(7))
  deallocate( transmission_ad % tau_surf      ,stat= alloc_status(8))
  deallocate( transmission_ad % tau_layer     ,stat= alloc_status(9))
  deallocate( transmission_ad % od_singlelayer,stat= alloc_status(10))

  If( any(alloc_status /= 0) ) then
     errorstatus(:) = errorstatus_fatal
     Write( errMessage, '( "mem deallocation error")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If


End Subroutine Rttov_cld_ad
