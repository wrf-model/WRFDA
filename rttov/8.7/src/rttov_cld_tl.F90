Subroutine Rttov_cld_tl ( &
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
     & calcemis,        &! in
     & emissivity,      &! inout
     & profiles_tl,     &! in
     & cld_profiles_tl, &! in
     & emissivity_tl,   &! inout
     & cld_radiance,    &! inout
     & cld_radiance_tl ) ! inout
  ! Description:
  ! to compute multi-channel radiances and brightness
  ! temperatures for many profiles per call in a cloudy sky.
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
  !  2.3    06/10/04     Add errorstatus to rttov_emiscld_tl call (J Cameron)
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
#include "rttov_tl.interface"
!  include "rttov_intradov_tl.interface"
#include "rttov_intex_tl.interface"
#include "rttov_emiscld_tl.interface"
#include "rttov_aitosu_tl.interface"
#include "rttov_errorreport.interface"
#include "rttov_calcbt.interface"
#include "rttov_calcbt_tl.interface"
#include "rttov_setgeometry.interface"
#include "rttov_calcpolarisation.interface"
#include "rttov_calcpolarisation_tl.interface"

  !subroutine arguments:
  Integer(Kind=jpim),        Intent(in)    :: nfrequencies
  Integer(Kind=jpim),        Intent(in)    :: nchannels
  Integer(Kind=jpim),        Intent(in)    :: nbtout
  Integer(Kind=jpim),        Intent(in)    :: nprofiles
  Integer(Kind=jpim),        Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),        Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),        Intent(in)    :: lprofiles(nfrequencies)
  Type(profile_Type),        Intent(inout) :: profiles(nprofiles)
  Type(profile_cloud_Type),  Intent(in)    :: cld_profiles(nprofiles)
  Type(rttov_coef),          Intent(in)    :: coef
  Logical,                   Intent(in)    :: calcemis(nchannels)
  Real(Kind=jprb),           Intent(inout) :: emissivity(nchannels)
  Type(radiance_cloud_type), Intent(inout) :: cld_radiance! in because of meme allocation


  Type(profile_Type),        Intent(in)    :: profiles_tl(nprofiles)
  Type(profile_cloud_Type),  Intent(in)    :: cld_profiles_tl(nprofiles)
  Real(Kind=jprb),           Intent(inout) :: emissivity_tl(nchannels)
  Type(radiance_cloud_type), Intent(inout) :: cld_radiance_tl ! in because of meme allocation
  Integer(Kind=jpim),        Intent(out)   :: errorstatus(nprofiles)

  !local variables:
  Logical :: addcloud
  Integer(Kind=jpim) :: nwp_levels  ! number of levels for NWP profiles
  Integer(Kind=jpim) :: nrt_levels  ! number of levels for RTTOV_direct integration
  Integer(Kind=jpim) :: jl     ! loop indice
  Integer(Kind=jpim) :: jk    ! loop indice
  Integer(Kind=jpim) :: ipf    ! loop indice
  Integer(Kind=jpim) :: alloc_status(20)
  Character (len=80) :: errMessage
  Character (len=12) :: NameOfRoutine = 'rttov_cld_tl'

  Real(Kind=jprb)    :: null_press(coef%nlevels)
  Integer(Kind=jpim) :: freq
  Type(geometry_Type):: angles(nprofiles)    ! geometry angles
  Type(radiance_Type) :: radiance
  Type(radiance_Type) :: radiance_tl
  Type(transmission_Type)    :: transmission
  Type(transmission_Type)    :: transmission_tl

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

  ! allocate radiance TL results arrays with number of channels
  radiance_tl % clear     => cld_radiance_tl % clear
  radiance_tl % clear_out => cld_radiance_tl % clear_out
  radiance_tl % cloudy    => cld_radiance_tl % cloudy
  radiance_tl % total     => cld_radiance_tl % total
  radiance_tl % total_out => cld_radiance_tl % total_out
  radiance_tl % bt        => cld_radiance_tl % bt
  radiance_tl % bt_clear  => cld_radiance_tl % bt_clear
  radiance_tl % out        => cld_radiance_tl % out
  radiance_tl % out_clear  => cld_radiance_tl % out_clear
  radiance_tl % upclear   => cld_radiance_tl % upclear
  radiance_tl % reflclear => cld_radiance_tl % reflclear
  allocate( radiance_tl % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(3))
  allocate( radiance_tl % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(4))

  ! allocate transmission arrays
  Allocate( transmission % tau_surf      ( nchannels ) ,stat= alloc_status(5))
  Allocate( transmission % tau_layer     ( coef % nlevels, nchannels ) ,stat= alloc_status(6))
  Allocate( transmission % od_singlelayer( coef % nlevels, nchannels ) ,stat= alloc_status(7))
  Allocate( transmission_tl % tau_surf      ( nchannels ) ,stat= alloc_status(8))
  Allocate( transmission_tl % tau_layer     ( coef % nlevels, nchannels ) ,stat= alloc_status(9))
  Allocate( transmission_tl % od_singlelayer( coef % nlevels, nchannels ) ,stat= alloc_status(10))

  If( any(alloc_status /= 0) ) then
     errorstatus(:) = errorstatus_fatal
     Write( errMessage, '( "mem allocation error")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If


  !*         1.   Gas absorption

  addcloud = .true.

  ! No calculation of CLW absorption inside "classical" RTTOV
  if ( any(.not.profiles(:)%clw_data) ) then
     ! warning message
     profiles(:)%clw_data = .false.
  End If

  Call rttov_tl( &
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
     & calcemis,        &! in
     & emissivity,      &! inout
     & profiles_tl,     &! in
     & emissivity_tl,   &! inout
     & transmission,    &! inout
     & transmission_tl, &! inout
     & radiance,        &! inout
     & radiance_tl     ) ! inout

  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_tl")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  !*         2.   Interpolate cloud contribution to model levels
  ! compute arrays overcast, downcld of type cld_radiancedata
  nwp_levels   = cld_profiles(1) % nlevels
  nrt_levels   = profiles(1)     % nlevels
  null_press(:) = 0._JPRB
  DO jl = 1, nchannels
     freq = polarisations(jl,2)
     ipf = lprofiles(freq)
     call rttov_intex_tl( &
        & nrt_levels,        &! in
        & nwp_levels,        &! in
        & null_press,        &! in
        & cld_profiles_tl(ipf) % p,                      &! in
        & radiance_tl     % overcast(1:nrt_levels,jl),   &! in
        & cld_radiance_tl % overcast(1:nwp_levels,jl),   &! inout
        & profiles(ipf)     % p,                         &! in
        & cld_profiles(ipf) % p,                         &! in
        & radiance     % overcast(1:nrt_levels,jl),      &! in
        & cld_radiance % overcast(1:nwp_levels,jl)  )     ! out

     call rttov_intex_tl( &
        & nrt_levels,        &! in
        & nwp_levels,        &! in
        & null_press,        &! in
        & cld_profiles_tl(ipf) % p,                     &! in
        & radiance_tl     % downcld(1:nrt_levels,jl),   &! in
        & cld_radiance_tl % downcld(1:nwp_levels,jl),   &! inout
        & profiles(ipf)     % p,                        &! in
        & cld_profiles(ipf) % p,                        &! in
        & radiance     % downcld(1:nrt_levels,jl),      &! in
        & cld_radiance % downcld(1:nwp_levels,jl)  )     ! out

  End Do

  !*         3.   Calculate cloud emissivity
  call rttov_emiscld_tl(  &
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
        & cld_profiles_tl,   &! in
        & cld_radiance,      &! inout  (cldemis part only)
        & cld_radiance_tl)    ! inout  (cldemis part only)
  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_emiscld_tl")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  !*         4.   Compute the weights of the cloud layers
  !               ---------------------------------------
  call rttov_aitosu_tl( &
        & nfrequencies,      &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & nwp_levels,        &! in
        & polarisations,     &! in
        & lprofiles,         &! in
        & overlap_scheme,    &! in
        & cld_profiles,      &! in  (cloud cover)
        & cld_profiles_tl,   &! in  (cloud cover)
        & cld_radiance ,     &! inout  (cldemis input and
        & cld_radiance_tl  )  ! inout   cs_wtao, cs_wsurf, wtao, wsurf in output)

  !*         5.   Integrate *rt* equation.
  !               --------- ---- --------
  ! clear-sky contribution
  ! without the surface reflection
  cld_radiance    % total (:) = cld_radiance    % cs_wtoa(:) * cld_radiance    % upclear (:)
  cld_radiance_tl % total (:) = cld_radiance_tl % cs_wtoa(:) * cld_radiance    % upclear (:) +&
                               & cld_radiance    % cs_wtoa(:) * cld_radiance_tl % upclear (:)

  ! with the surface-reflected clear-sky downward radiance
  cld_radiance    % total (:) = cld_radiance    % total (:) +&
        & cld_radiance % cs_wsurf(:) * cld_radiance % cs_wtoa(:) *&
        & cld_radiance % reflclear (:)
  cld_radiance_tl % total (:) = cld_radiance_tl % total (:) +&
     & cld_radiance_tl%cs_wsurf(:) * cld_radiance   %cs_wtoa(:) * cld_radiance  %reflclear(:) +&
     & cld_radiance   %cs_wsurf(:) * cld_radiance_tl%cs_wtoa(:) * cld_radiance  %reflclear(:) +&
     & cld_radiance   %cs_wsurf(:) * cld_radiance   %cs_wtoa(:) * cld_radiance_tl%reflclear(:)

  ! cloud contribution
  DO jk = 1, nwp_levels
     ! cloud upward emission
     cld_radiance    % total (:) = cld_radiance    % total (:) +&
           & cld_radiance % wtoa(jk,:) * cld_radiance % overcast(jk,:)
     cld_radiance_tl % total (:) = cld_radiance_tl % total (:)              +&
           & cld_radiance_tl % wtoa(jk,:) * cld_radiance    % overcast(jk,:) +&
           & cld_radiance    % wtoa(jk,:) * cld_radiance_tl % overcast(jk,:)

     ! cloud downward emission, reflected at the surface
     cld_radiance % total (:) = cld_radiance % total (:) +&
           & cld_radiance % wsurf(jk,:) * cld_radiance % cs_wtoa(:) *&
           & cld_radiance % downcld(jk,:)
     cld_radiance_tl % total (:) = cld_radiance_tl % total (:)        +&
         & cld_radiance_tl%wsurf(jk,:) * cld_radiance   %cs_wtoa(:) *&
         & cld_radiance   %downcld(jk,:)                               +&
         & cld_radiance   %wsurf(jk,:) * cld_radiance_tl%cs_wtoa(:) *&
         & cld_radiance   %downcld(jk,:)                               +&
         & cld_radiance   %wsurf(jk,:) * cld_radiance   %cs_wtoa(:) *&
         & cld_radiance_tl%downcld(jk,:)
  END DO

  ! Remember that radiance struture is mainly pointing on cld_radiance
  ! so we can use radiance struture for conversion of radiance to brightness temperatue.
  Call rttov_calcbt( &
    &    nfrequencies,    &! in
    &    nchannels,       &! in
    &    channels,        &! in
    &    polarisations,   &! in
         & coef,          &! in
         & radiance      ) ! inout
  Call rttov_calcbt_tl( &
     &    nfrequencies,   &! in
    &    nchannels,       &! in
    &    channels,        &! in
    &    polarisations,   &! in
         & coef,          &! in
         & radiance,      &! in
         & radiance_tl   ) ! inout

  If (coef % id_sensor == sensor_id_mw) Then
     Do ipf = 1, nprofiles
       Call rttov_setgeometry( &
          & profiles(ipf), &! in
          & coef,          &! in
          & angles(ipf) )   ! out
     End Do
     !
     Call rttov_calcpolarisation( &
        & nfrequencies,  &! in
        & nchannels,     &! in
        & nprofiles,     &! in
        & angles,        &! in
        & channels,      &! in
        & polarisations, &! in
        & lprofiles,     &! in
        & coef,          &! in
        & radiance      ) ! inout
     Call rttov_calcpolarisation_tl( &
        & nfrequencies,  &! in
        & nchannels,     &! in
        & nprofiles,     &! in
        & angles,        &! in
        & channels,      &! in
        & polarisations, &! in
        & lprofiles,     &! in
        & coef,          &! in
        & radiance_tl   ) ! inout
  Else
  radiance%out          = radiance%bt
        radiance%out_clear    = radiance%bt_clear
  radiance%total_out    = radiance%total
        radiance%clear_out    = radiance%clear
  radiance_tl%out       = radiance_tl%bt
        radiance_tl%out_clear = radiance_tl%bt_clear
  radiance_tl%total_out = radiance_tl%total
        radiance_tl%clear_out = radiance_tl%clear
  End If
  ! deallocate radiance structure for overcast and dowcld arrays
  deallocate( radiance % overcast    ,stat= alloc_status(1))
  deallocate( radiance % downcld     ,stat= alloc_status(2))
  deallocate( radiance_tl % overcast ,stat= alloc_status(3))
  deallocate( radiance_tl % downcld  ,stat= alloc_status(4))
  ! deallocate transmission structure
  deallocate( transmission % tau_surf      ,stat= alloc_status(5))
  deallocate( transmission % tau_layer     ,stat= alloc_status(6))
  deallocate( transmission % od_singlelayer,stat= alloc_status(7))
  deallocate( transmission_tl % tau_surf      ,stat= alloc_status(8))
  deallocate( transmission_tl % tau_layer     ,stat= alloc_status(9))
  deallocate( transmission_tl % od_singlelayer,stat= alloc_status(10))

  If( any(alloc_status /= 0) ) then
     errorstatus(:) = errorstatus_fatal
     Write( errMessage, '( "mem deallocation error")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If


End Subroutine Rttov_cld_tl
