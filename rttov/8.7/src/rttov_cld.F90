!
Subroutine rttov_cld( &
     & errorstatus,    &! out
     & nfrequencies,   &! in
     & nchannels,      &! in
     & nbtout,         &! in
     & nprofiles,      &! in
     & channels,       &! in
     & polarisations,  &! in
     & lprofiles,      &! in
     & profiles,       &! inout  (to invalid clw absorption)
     & cld_profiles,   &! in
     & coef,           &! in
     & calcemis,       &! in
     & emissivity,     &! inout
     & cld_radiance )   ! inout
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
  !  2.3    06/10/04     Add errorstatus to rttov_emiscld call (J Cameron)
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

  ! Imported Type Definitions:
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
#include "rttov_intex.interface"
#include "rttov_emiscld.interface"
#include "rttov_aitosu.interface"
#include "rttov_calcbt.interface"
#include "rttov_setgeometry.interface"
#include "rttov_calcpolarisation.interface"

  !subroutine arguments:
  Integer(Kind=jpim),        Intent(in)    :: nbtout  ! Number of output radiances
  Integer(Kind=jpim),        Intent(in)    :: nfrequencies  ! Number of output radiances
  Integer(Kind=jpim),        Intent(in)    :: nchannels  ! Number of output radiances
                                                           !  (= channels used * profiles)
  Integer(Kind=jpim),        Intent(in)    :: nprofiles  ! Number of profiles
  Integer(Kind=jpim),        Intent(in)    :: channels(nfrequencies)      ! Channel indices
  Integer(Kind=jpim),        Intent(in)    :: polarisations(nchannels,3)      ! Channel indices
  Integer(Kind=jpim),        Intent(in)    :: lprofiles(nfrequencies)     ! Profiles indices
  Type(profile_Type),        Intent(inout) :: profiles(nprofiles) ! Profiles on RTTOV levels
  Type(profile_cloud_Type),  Intent(in)    :: cld_profiles(nprofiles) ! Cloud profiles on NWP levels
  Type(rttov_coef),          Intent(in)    :: coef  ! Coefficients
  Logical,                   Intent(in)    :: calcemis(nchannels)  ! switch for emmissivity calc.
  Real(Kind=jprb),           Intent(inout) :: emissivity(nchannels) ! surface emmissivity
  Type(radiance_cloud_Type), Intent(inout) :: cld_radiance    ! radiances (mw/cm-1/ster/sq.m)
  Integer(Kind=jpim),        Intent(out)   :: errorstatus(nprofiles)  ! return flag

  !local variables:
  Logical :: addcloud
  Integer(Kind=jpim) :: nwp_levels  ! number of levels for NWP profiles
  Integer(Kind=jpim) :: nrt_levels  ! number of levels for RTTOV_direct integration
  Integer(Kind=jpim) :: jl     ! loop indice
  Integer(Kind=jpim) :: jk     ! loop indice
  Integer(Kind=jpim) :: ipf    ! loop indice
  Integer(Kind=jpim) :: alloc_status(10)
  Integer(Kind=jpim) :: freq
  Type(geometry_Type)   :: angles(nprofiles)    ! geometry angles

  Character (len=80) :: errMessage
  Character (len=10) :: NameOfRoutine = 'rttov_cld '

  Type(radiance_Type) :: radiance
  Type(transmission_Type)  :: transmission

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
  Allocate( radiance % overcast(coef % nlevels, nchannels) ,stat=alloc_status(1))
  Allocate( radiance % downcld (coef % nlevels, nchannels) ,stat=alloc_status(2))

  ! allocate transmission structure
  Allocate( transmission % tau_surf      ( nchannels ) ,stat= alloc_status(3))
  Allocate( transmission % tau_layer     ( coef % nlevels, nchannels ) ,stat= alloc_status(4))
  Allocate( transmission % od_singlelayer( coef % nlevels, nchannels ) ,stat= alloc_status(5))

  If( Any(alloc_status /= 0) ) Then
     errorstatus(:) = errorstatus_fatal
     Write( errMessage, '( "mem allocation error")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If


  !*         1.   Gas absorption

  addcloud = .True.

  ! No calculation of CLW absorption inside "classical" RTTOV
  If ( Any(.Not.profiles(:)%clw_Data) ) Then
     ! warning message
     profiles(:)%clw_Data = .False.
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


  !*  2.   Interpolate cloud contribution to model levels
  ! compute arrays overcast, downcld of type cld_radiancedata
  nwp_levels   = cld_profiles(1) % nlevels
  nrt_levels   = profiles(1)     % nlevels

  Do jl = 1, nchannels
     freq = polarisations(jl,2)
     ipf = lprofiles(freq)
     Call rttov_intex( &
           & nrt_levels,        &! in
           & nwp_levels,        &! in
           & profiles(ipf)     % p,      &! in
           & cld_profiles(ipf) % p,      &! in
           & radiance     % overcast(1:nrt_levels,jl),      &! in
           & cld_radiance % overcast(1:nwp_levels,jl)  )     ! inout

     Call rttov_intex( &
           & nrt_levels,        &! in
           & nwp_levels,        &! in
           & profiles(ipf)     % p,      &! in
           & cld_profiles(ipf) % p,      &! in
           & radiance     % downcld(1:nrt_levels,jl),      &! in
           & cld_radiance % downcld(1:nwp_levels,jl)  )     ! inout
  End Do

  !* 3.   Calculate cloud emissivity
  Call rttov_emiscld(  &
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

  !*  4.   Compute the weights of the cloud layers
  !   ---------------------------------------
  Call rttov_aitosu( &
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

  !*  5.   Integrate *rt* equation.
  !         --------- ---- --------
  ! clear-sky contribution
  ! without the surface reflection
  cld_radiance % total (:) = cld_radiance % cs_wtoa(:) * cld_radiance % upclear (:)
  ! with the surface-reflected clear-sky downward radiance
  cld_radiance % total (:) = cld_radiance % total (:) +&
        & cld_radiance % cs_wsurf(:) * cld_radiance % cs_wtoa(:) *&
        & cld_radiance % reflclear (:)
  !
  ! cloud contribution
  Do jk = 1, nwp_levels
     ! cloud upward emission
     cld_radiance % total (:) = cld_radiance % total (:) +&
           & cld_radiance % wtoa(jk,:) * cld_radiance % overcast(jk,:)

     ! cloud downward emission, reflected at the surface
     cld_radiance % total (:) = cld_radiance % total (:) +&
           & cld_radiance % wsurf(jk,:) * cld_radiance % cs_wtoa(:) *&
           & cld_radiance % downcld(jk,:)
  End Do

  ! Remember that radiance struture is mainly pointing on cld_radiance
  ! so we can use radiance struture for conversion of radiance to brightness temperatue.
  Call rttov_calcbt( &
       & nfrequencies,  &! in
       & nchannels,     &! in
       & channels,      &! in
       & polarisations, &! in
       & coef,          &! in
       & radiance      ) ! inout

  If (coef % id_sensor == sensor_id_mw) Then
     Do ipf = 1, nprofiles
       Call rttov_setgeometry( &
          & profiles(ipf),   &! in
          & coef,            &! in
          & angles(ipf) )     ! out
     End Do
     !
     Call rttov_calcpolarisation( &
        & nfrequencies,   &! in
        & nchannels,      &! in
        & nprofiles,      &! in
        & angles,         &! in
        & channels,       &! in
        & polarisations,  &! in
        & lprofiles,      &! in
        & coef,           &! in
        & radiance       ) ! inout
  Else
  radiance%out       = radiance%bt
        radiance%out_clear = radiance%bt_clear
  radiance%total_out = radiance%total
        radiance%clear_out = radiance%clear
  End If

  ! deallocate radiance structure for overcast and dowcld arrays
  Deallocate( radiance % overcast ,stat= alloc_status(1))
  Deallocate( radiance % downcld  ,stat= alloc_status(2))
  ! deallocate transmission structure
  Deallocate( transmission % tau_surf      ,stat= alloc_status(3))
  Deallocate( transmission % tau_layer     ,stat= alloc_status(4))
  Deallocate( transmission % od_singlelayer,stat= alloc_status(5))

  If( Any(alloc_status /= 0) ) Then
     errorstatus(:) = errorstatus_fatal
     Write( errMessage, '( "mem deallocation error")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If
!!$  nullify ( radiance % clear )
!!$  nullify ( radiance % cloudy  )
!!$  nullify ( radiance % total )
!!$  nullify ( radiance % bt )
!!$  nullify ( radiance % bt_clear )
!!$  nullify ( radiance % upclear )
!!$  nullify ( radiance % dnclear )
!!$  nullify ( radiance % reflclear )

  !

End Subroutine rttov_cld
