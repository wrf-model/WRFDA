!
Subroutine rttov_scatt_ad( & 
     & errorstatus,        &! out
     & nwp_levels,         &! in
     & nrt_levels,         &! in
     & nfrequencies,       &! in
     & nchannels,          &! in
     & nbtout,             &! in
     & nprofiles,          &! in
     & polarisations,      &! in
     & channels,           &! in
     & frequencies,        &! in
     & lprofiles,          &! in
     & lsprofiles,         &! in
     & profiles,           &! inout  
     & cld_profiles,       &! in
     & coef_rttov,         &! in
     & coef_scatt,         &! in
     & calcemiss,          &! in
     & emissivity_in,      &! inout
     & profiles_ad,        &! inout
     & cld_profiles_ad,    &! inout
     & emissivity_in_ad,   &! inout
     & cld_radiance,       &! inout
     & cld_radiance_ad)     ! inout 
     
  ! Description:
  ! AD of subroutine 
  ! to compute microwave multi-channel radiances and brightness
  ! temperatures for many profiles per call in a cloudy and/or rainy sky.
  !
  ! According to the argument switchrad the main input total or bt is used
  ! switchrad == true    bt is the input, brightness temperature
  ! switchrad == false   total is the input, radiance

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
  ! - Bauer, P., 2002: Microwave radiative transfer modeling in clouds and precipitation.
  !     Part I: Model description.
  !     NWP SAF Report No. NWPSAF-EC-TR-005, 21 pp.
  ! - Moreau, E., P. Bauer and F. Chevallier, 2002: Microwave radiative transfer modeling in clouds and precipitation.
  !     Part II: Model evaluation.
  !     NWP SAF Report No. NWPSAF-EC-TR-006, 27 pp.
  ! - Chevallier, F., and P. Bauer, 2003:
  !     Model rain and clouds over oceans:comparison with SSM/I observations. Mon. Wea. Rev., 131, 1240-1255.
  ! - Smith, E. A., P. Bauer, F. S. Marzano, C. D. Kummerow, D. McKague, A. Mugnai, G. Panegrossi, 2002:
  !     Intercomparison of microwave radiative transfer models for precipitating clouds.
  !     IEEE Trans. Geosci. Remote Sens. 40, 541-549.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !   1.0    09/2002      Initial version     (F. Chevallier)
  !   1.1    05/2003      RTTOV7.3 compatible (F. Chevallier)
  !   1.2    03/2004      Added polarimetry   (R. Saunders)
  !   1.3    08/2004      Polarimetry fixes   (U. O'Keeffe)
  !   1.4    11/2004      Clean-up            (P. Bauer)
  !   1.5    07/2005      Polarimetry fixes   (U. O'Keeffe)
  !   1.6    11/2005      Add errorstatus to iniscatt arguments and use a temporary
  !                       radiance type for the calcpolarisation call (J Cameron)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !   Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Parameters:
  
  Use rttov_const, Only :   &
       & sensor_id_mw        ,&
       & errorstatus_success ,&
       & errorstatus_fatal  

  Use rttov_types, Only :    &
       & rttov_coef           ,&
       & rttov_scatt_coef     ,&
       & geometry_Type        ,&
       & profile_Type         ,&
       & profile_cloud_Type   ,&
       & profile_scatt_aux    ,&
       & transmission_Type    ,&
       & radiance_Type        ,&
       & radiance_cloud_Type 

  Use parkind1, Only : jpim     ,jprb
  
  Implicit None

#include "rttov_direct.interface"
#include "rttov_iniscatt.interface"
#include "rttov_eddington.interface"
#include "rttov_ad.interface"
#include "rttov_iniscatt_ad.interface"
#include "rttov_eddington_ad.interface"
#include "rttov_errorreport.interface"
#include "rttov_calcpolarisation_ad.interface"

!* Subroutine arguments:
  Integer (Kind=jpim), Intent (in)  :: nwp_levels                              ! Number of levels
  Integer (Kind=jpim), Intent (in)  :: nrt_levels                              ! Number of levels
  Integer (Kind=jpim), Intent (in)  :: nprofiles                               ! Number of profiles
  Integer (Kind=jpim), Intent (in)  :: nfrequencies                            ! Number of frequencies  
  Integer (Kind=jpim), Intent (in)  :: nchannels                               ! Number of channels*profiles=radiances
  Integer (Kind=jpim), Intent (in)  :: nbtout                                  ! Number of output radiances
  Integer (Kind=jpim), Intent (in)  :: channels      (nfrequencies)            ! Channel indices
  Integer (Kind=jpim), Intent (in)  :: frequencies   (nchannels)               ! Frequency indices
  Integer (Kind=jpim), Intent (in)  :: polarisations (nchannels,3)             ! Polarisation indices
  Integer (Kind=jpim), Intent (in)  :: lprofiles     (nfrequencies)            ! Profile indices
  Integer (Kind=jpim), Intent (in)  :: lsprofiles    (nchannels)               ! Profile indices
  Integer (Kind=jpim), Intent (out) :: errorstatus   (nprofiles)               ! Error return flag

  Logical,             Intent (in)    :: calcemiss        (nchannels)          ! Switch for emmissivity calculation
  Real    (Kind=jprb), Intent (in)    :: emissivity_in    (nchannels)          ! Surface emmissivity 
  Real    (Kind=jprb), Intent (inout) :: emissivity_in_ad (nchannels)          ! Surface emmissivity 
  
  Type (profile_Type),        Intent (inout) :: profiles        (nprofiles)    ! Atmospheric profiles
  Type (profile_Type),        Intent (inout) :: profiles_ad     (nprofiles)    
  Type (rttov_coef),          Intent (in)    :: coef_rttov                     ! RTTOV Coefficients
  Type (rttov_scatt_coef),    Intent (in)    :: coef_scatt                     ! RTTOV_SCATT Coefficients
  Type (profile_cloud_Type),  Intent (in)    :: cld_profiles    (nprofiles)    ! Cloud profiles with NWP levels
  Type (profile_cloud_Type),  Intent (inout) :: cld_profiles_ad (nprofiles)   
  Type (radiance_cloud_Type), Intent (inout) :: cld_radiance                   ! Radiances
  Type (radiance_cloud_Type), Intent (inout) :: cld_radiance_ad          
   
  Integer (Kind=jpim), target :: sa__mclayer    (nchannels)
  Integer (Kind=jpim), target :: sa_ad__mclayer (nchannels)

  Real (kind=jprb), target :: r__clear_out (nbtout)
  Real (kind=jprb), target :: r__out       (nbtout)
  Real (kind=jprb), target :: r__out_clear (nbtout)
  Real (kind=jprb), target :: r__total_out (nbtout)
  Real (kind=jprb), target :: r__clear     (nchannels)
  Real (kind=jprb), target :: r__cloudy    (nchannels)
  Real (kind=jprb), target :: r__total     (nchannels)
  Real (kind=jprb), target :: r__bt        (nchannels)
  Real (kind=jprb), target :: r__bt_clear  (nchannels)
  Real (kind=jprb), target :: r__upclear   (nchannels)
  Real (kind=jprb), target :: r__dnclear   (nchannels)
  Real (kind=jprb), target :: r__reflclear (nchannels)
  Real (Kind=jprb), target :: r__overcast  (nrt_levels,nchannels)
  Real (Kind=jprb), target :: r__downcld   (nrt_levels,nchannels)

  Real (kind=jprb), target :: rz__clear_out (nbtout)
  Real (kind=jprb), target :: rz__out       (nbtout)
  Real (kind=jprb), target :: rz__out_clear (nbtout)
  Real (kind=jprb), target :: rz__total_out (nbtout)
  Real (kind=jprb), target :: rz__clear     (nchannels)
  Real (kind=jprb), target :: rz__cloudy    (nchannels)
  Real (kind=jprb), target :: rz__total     (nchannels)
  Real (kind=jprb), target :: rz__bt        (nchannels)
  Real (kind=jprb), target :: rz__bt_clear  (nchannels)
  Real (kind=jprb), target :: rz__upclear   (nchannels)
  Real (kind=jprb), target :: rz__dnclear   (nchannels)
  Real (kind=jprb), target :: rz__reflclear (nchannels)
  Real (Kind=jprb), target :: rz__overcast  (nrt_levels,nchannels)
  Real (Kind=jprb), target :: rz__downcld   (nrt_levels,nchannels)

  Real (kind=jprb), target :: r_ad__clear_out (nbtout)
  Real (kind=jprb), target :: r_ad__out       (nbtout)
  Real (kind=jprb), target :: r_ad__out_clear (nbtout)
  Real (kind=jprb), target :: r_ad__total_out (nbtout)
  Real (kind=jprb), target :: r_ad__clear     (nchannels)
  Real (kind=jprb), target :: r_ad__cloudy    (nchannels)
  Real (kind=jprb), target :: r_ad__total     (nchannels)
  Real (kind=jprb), target :: r_ad__bt        (nchannels)
  Real (kind=jprb), target :: r_ad__bt_clear  (nchannels)
  Real (kind=jprb), target :: r_ad__upclear   (nchannels)
  Real (kind=jprb), target :: r_ad__dnclear   (nchannels)
  Real (kind=jprb), target :: r_ad__reflclear (nchannels)
  Real (Kind=jprb), target :: r_ad__overcast  (nrt_levels,nchannels)
  Real (Kind=jprb), target :: r_ad__downcld   (nrt_levels,nchannels)
      
  Real (Kind=jprb), target :: t__tau_surf          (nchannels)
  Real (Kind=jprb), target :: t__tau_layer         (nrt_levels,nchannels)
  Real (Kind=jprb), target :: t__od_singlelayer    (nrt_levels,nchannels)
  Real (Kind=jprb), target :: t_ad__tau_surf       (nchannels)
  Real (Kind=jprb), target :: t_ad__tau_layer      (nrt_levels,nchannels)
  Real (Kind=jprb), target :: t_ad__od_singlelayer (nrt_levels,nchannels)
  
  Real (Kind=jprb), target :: sa__ccmax   (nprofiles)  
  Real (Kind=jprb), target :: sa__ems_bnd (nchannels)
  Real (Kind=jprb), target :: sa__ref_bnd (nchannels)
  Real (Kind=jprb), target :: sa__ems_cld (nchannels)
  Real (Kind=jprb), target :: sa__ref_cld (nchannels)
  
  Real (Kind=jprb), target :: sa__tbd (nprofiles,nwp_levels+1)
  
  Real (Kind=jprb), target :: sa__delta  (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa__tau    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa__ext    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa__ssa    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa__asm    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa__lambda (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa__h      (nchannels,nwp_levels)
  
  Real (Kind=jprb), target :: sa__b0     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa__b1     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa__bn     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa__dz     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa__clw    (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa__ciw    (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa__rain   (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa__sp     (nprofiles,nwp_levels)

  Real (Kind=jprb), target :: sa_ad__ccmax   (nprofiles)
  Real (Kind=jprb), target :: sa_ad__ems_bnd (nchannels)
  Real (Kind=jprb), target :: sa_ad__ref_bnd (nchannels)
  Real (Kind=jprb), target :: sa_ad__ems_cld (nchannels)
  Real (Kind=jprb), target :: sa_ad__ref_cld (nchannels)
  
  Real (Kind=jprb), target :: sa_ad__tbd (nprofiles,nwp_levels+1)
  
  Real (Kind=jprb), target :: sa_ad__delta  (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__tau    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__ext    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__ssa    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__asm    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__lambda (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__h      (nchannels,nwp_levels)
  
  Real (Kind=jprb), target :: sa_ad__b0     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__b1     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__bn     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__dz     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__clw    (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__ciw    (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__rain   (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_ad__sp     (nprofiles,nwp_levels)

!* Local variables:
  Logical             :: addcloud, switchrad
  Integer (Kind=jpim) :: iprof, ichan   
  Real    (Kind=jprb) :: emissivity    (nchannels)
  Real    (Kind=jprb) :: emissivity_ad (nchannels)
    
  Type (transmission_Type)   :: transmission, transmission_ad
  Type (geometry_Type)       :: angles (nprofiles)
  Type (profile_scatt_aux)   :: scatt_aux, scatt_aux_ad
  Type (radiance_Type)       :: radiance, radiance_ad
  Type (radiance_cloud_Type) :: zcld_radiance            
  Type (radiance_Type)       :: cld_radiance_tmp

  Character (len=80) :: errMessage
  Character (len=15) :: NameOfRoutine = 'rttov_scatt_ad '

  !- End of header --------------------------------------------------------
  
  errorstatus(:)  = errorstatus_success

  radiance % clear_out => r__clear_out
  radiance % out       => r__out
  radiance % out_clear => r__out_clear
  radiance % total_out => r__total_out
  radiance % clear     => r__clear
  radiance % cloudy    => r__cloudy
  radiance % total     => r__total
  radiance % bt        => r__bt
  radiance % bt_clear  => r__bt_clear
  radiance % upclear   => r__upclear
  radiance % dnclear   => r__dnclear
  radiance % reflclear => r__reflclear
  radiance % overcast  => r__overcast 
  radiance % downcld   => r__downcld 

  radiance_ad % clear_out => r_ad__clear_out
  radiance_ad % out       => r_ad__out
  radiance_ad % out_clear => r_ad__out_clear
  radiance_ad % total_out => r_ad__total_out
  radiance_ad % clear     => r_ad__clear
  radiance_ad % cloudy    => r_ad__cloudy
  radiance_ad % total     => r_ad__total
  radiance_ad % bt        => r_ad__bt
  radiance_ad % bt_clear  => r_ad__bt_clear
  radiance_ad % upclear   => r_ad__upclear
  radiance_ad % dnclear   => r_ad__dnclear
  radiance_ad % reflclear => r_ad__reflclear
  radiance_ad % overcast  => r_ad__overcast 
  radiance_ad % downcld   => r_ad__downcld 

  zcld_radiance % clear_out => rz__clear_out
  zcld_radiance % out       => rz__out
  zcld_radiance % out_clear => rz__out_clear
  zcld_radiance % total_out => rz__total_out
  zcld_radiance % clear     => rz__clear
  zcld_radiance % cloudy    => rz__cloudy
  zcld_radiance % total     => rz__total
  zcld_radiance % bt        => rz__bt
  zcld_radiance % bt_clear  => rz__bt_clear
  zcld_radiance % upclear   => rz__upclear
  zcld_radiance % dnclear   => rz__dnclear
  zcld_radiance % reflclear => rz__reflclear
  zcld_radiance % overcast  => rz__overcast 
  zcld_radiance % downcld   => rz__downcld 

  transmission % tau_surf       => t__tau_surf
  transmission % tau_layer      => t__tau_layer
  transmission % od_singlelayer => t__od_singlelayer

  transmission_ad % tau_surf       => t_ad__tau_surf
  transmission_ad % tau_layer      => t_ad__tau_layer
  transmission_ad % od_singlelayer => t_ad__od_singlelayer

  scatt_aux % ccmax    => sa__ccmax
  scatt_aux % ems_bnd  => sa__ems_bnd
  scatt_aux % ref_bnd  => sa__ref_bnd
  scatt_aux % ems_cld  => sa__ems_cld
  scatt_aux % ref_cld  => sa__ref_cld
  scatt_aux % tbd      => sa__tbd
  scatt_aux % mclayer  => sa__mclayer
  scatt_aux % delta    => sa__delta
  scatt_aux % tau      => sa__tau
  scatt_aux % ext      => sa__ext
  scatt_aux % ssa      => sa__ssa
  scatt_aux % asm      => sa__asm
  scatt_aux % lambda   => sa__lambda
  scatt_aux % h        => sa__h
  scatt_aux % b0       => sa__b0
  scatt_aux % b1       => sa__b1
  scatt_aux % bn       => sa__bn
  scatt_aux % dz       => sa__dz
  scatt_aux % clw      => sa__clw
  scatt_aux % ciw      => sa__ciw
  scatt_aux % rain     => sa__rain
  scatt_aux % sp       => sa__sp

  scatt_aux_ad % ccmax    => sa_ad__ccmax
  scatt_aux_ad % ems_bnd  => sa_ad__ems_bnd
  scatt_aux_ad % ref_bnd  => sa_ad__ref_bnd
  scatt_aux_ad % ems_cld  => sa_ad__ems_cld
  scatt_aux_ad % ref_cld  => sa_ad__ref_cld
  scatt_aux_ad % tbd      => sa_ad__tbd
  scatt_aux_ad % mclayer  => sa_ad__mclayer
  scatt_aux_ad % delta    => sa_ad__delta
  scatt_aux_ad % tau      => sa_ad__tau
  scatt_aux_ad % ext      => sa_ad__ext
  scatt_aux_ad % ssa      => sa_ad__ssa
  scatt_aux_ad % asm      => sa_ad__asm
  scatt_aux_ad % lambda   => sa_ad__lambda
  scatt_aux_ad % h        => sa_ad__h
  scatt_aux_ad % b0       => sa_ad__b0
  scatt_aux_ad % b1       => sa_ad__b1
  scatt_aux_ad % bn       => sa_ad__bn
  scatt_aux_ad % dz       => sa_ad__dz
  scatt_aux_ad % clw      => sa_ad__clw
  scatt_aux_ad % ciw      => sa_ad__ciw
  scatt_aux_ad % rain     => sa_ad__rain
  scatt_aux_ad % sp       => sa_ad__sp

!*         1.   Gas absorption
  switchrad = .true.   ! input to RTTOV is BT  
  addcloud  = .false.

  ! No calculation of CLW absorption inside "classical" RTTOV
  If ( Any(.Not. profiles (:) % clw_Data) ) Then
     ! warning message
     profiles (:) % clw_Data = .False.
  End If
  
  emissivity (:) = emissivity_in (:) 

  Call rttov_direct(          &
        & errorstatus,        &! out
        & nfrequencies,       &! in
        & nchannels,          &! in
        & nbtout,             &! in
        & nprofiles,          &! in
        & channels,           &! in
        & polarisations,      &! in
        & lprofiles,          &! in
        & profiles,           &! in
        & coef_rttov,         &! in
        & addcloud,           &! in
        & calcemiss,          &! in
        & emissivity,         &! inout
        & transmission,       &! inout
        & radiance     )       ! inout 

  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_direct")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  scatt_aux % ems_cld (:) = emissivity_in (:)      
  scatt_aux % ref_cld (:) = 1.0_JPRB - emissivity_in (:)    
      
!*  2.   Initialisations for Eddington
  Call rttov_iniscatt(       &
        & errorstatus,       &! out
        & nwp_levels,        &! in
        & nrt_levels,        &! in
        & nfrequencies,      &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & polarisations,     &! in
        & channels,          &! in
        & frequencies,       &! in
        & lprofiles,         &! in
        & lsprofiles,        &! in
        & profiles,          &! in  
        & cld_profiles,      &! in
        & coef_rttov,        &! in  
        & coef_scatt,        &! in  
        & transmission,      &! in
        & calcemiss,         &! in
        & angles,            &! out
        & scatt_aux   )       ! inout

  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_iniscatt")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

!* 3.   Eddington (in temperature space)
   Call rttov_eddington(     &
        & nwp_levels,        &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & lsprofiles,        &! in
        & angles,            &! in
        & profiles,          &! in  
        & cld_profiles,      &! in  
        & scatt_aux,         &! in
        & cld_radiance)       ! inout   

  zcld_radiance % bt (:) = cld_radiance % bt (:)
  
!*  4.   Combine clear and cloudy parts
  Do ichan = 1, nchannels
     iprof = lsprofiles (ichan)
     
     cld_radiance % total (ichan) = radiance % total (ichan)
     cld_radiance % clear (ichan) = radiance % clear (ichan)
     cld_radiance % bt_clear (ichan) = radiance % bt (ichan)
     cld_radiance % bt (ichan) = cld_radiance % bt (ichan) *             scatt_aux % ccmax (iprof) & 
                             & +     radiance % bt (ichan) * (1.0_JPRB - scatt_aux % ccmax (iprof))
  End Do
    
!* ADJOINT PART
  scatt_aux_ad % ccmax   (:)   = 0.0_JPRB
  scatt_aux_ad % ems_bnd (:)   = 0.0_JPRB
  scatt_aux_ad % ref_bnd (:)   = 0.0_JPRB
  scatt_aux_ad % ems_cld (:)   = 0.0_JPRB
  scatt_aux_ad % ref_cld (:)   = 0.0_JPRB
  scatt_aux_ad % tbd     (:,:) = 0.0_JPRB
  scatt_aux_ad % delta   (:,:) = 0.0_JPRB
  scatt_aux_ad % tau     (:,:) = 0.0_JPRB
  scatt_aux_ad % ext     (:,:) = 0.0_JPRB
  scatt_aux_ad % ssa     (:,:) = 0.0_JPRB
  scatt_aux_ad % asm     (:,:) = 0.0_JPRB
  scatt_aux_ad % lambda  (:,:) = 0.0_JPRB
  scatt_aux_ad % h       (:,:) = 0.0_JPRB
  scatt_aux_ad % b0      (:,:) = 0.0_JPRB
  scatt_aux_ad % b1      (:,:) = 0.0_JPRB
  scatt_aux_ad % bn      (:,:) = 0.0_JPRB
  scatt_aux_ad % dz      (:,:) = 0.0_JPRB
  scatt_aux_ad % clw     (:,:) = 0.0_JPRB
  scatt_aux_ad % ciw     (:,:) = 0.0_JPRB
  scatt_aux_ad % rain    (:,:) = 0.0_JPRB
  scatt_aux_ad % sp      (:,:) = 0.0_JPRB
  
  transmission_ad % tau_surf       (:)   = 0.0_JPRB
  transmission_ad % tau_layer      (:,:) = 0.0_JPRB
  transmission_ad % od_singlelayer (:,:) = 0.0_JPRB
    
  radiance_ad % bt (:) = 0.0_JPRB
  
  
  !
  !*  5.   Convert total polarisations length arrays to number of output channel length arrays
  !
  If (coef_rttov % id_sensor == sensor_id_mw) Then

     ! Point a temporary radiance type at cld_radiance_ad
     cld_radiance_tmp % clear     => cld_radiance_ad % clear
     cld_radiance_tmp % clear_out => cld_radiance_ad % clear_out
     cld_radiance_tmp % cloudy    => cld_radiance_ad % cloudy
     cld_radiance_tmp % total     => cld_radiance_ad % total
     cld_radiance_tmp % total_out => cld_radiance_ad % total_out
     cld_radiance_tmp % out       => cld_radiance_ad % out
     cld_radiance_tmp % out_clear => cld_radiance_ad % out_clear
     cld_radiance_tmp % bt        => cld_radiance_ad % bt
     cld_radiance_tmp % bt_clear  => cld_radiance_ad % bt_clear
     cld_radiance_tmp % upclear   => cld_radiance_ad % upclear
     cld_radiance_tmp % dnclear   => cld_radiance_ad % dnclear
     cld_radiance_tmp % reflclear => cld_radiance_ad % reflclear
     cld_radiance_tmp % overcast  => cld_radiance_ad % overcast
     cld_radiance_tmp % downcld   => cld_radiance_ad % downcld

     Call rttov_calcpolarisation_ad( &
       &  nfrequencies, & ! in
       &  nchannels,    & ! in
       &  nbtout,       & ! in
       &  profiles,     & ! in
       &  nprofiles,    & ! in
       &  angles,       & ! in
       &  channels,     & ! in
       &  polarisations,& ! in
       &  lprofiles,    & ! in
       &  coef_rttov,   & ! in
       &  cld_radiance_tmp       )      ! inout
   Else
         radiance_ad%bt       = radiance_ad%out
         radiance_ad%bt_clear = radiance_ad%out_clear
         cld_radiance_ad%bt       = cld_radiance_ad%out
         cld_radiance_ad%bt_clear = cld_radiance_ad%out_clear
   End If

!* 4.   Combine clear and cloudy parts  
  Do ichan = 1, nchannels
     iprof = lsprofiles (ichan)
       
     scatt_aux_ad % ccmax (iprof) = scatt_aux_ad % ccmax (iprof) & 
                                   & + (zcld_radiance % bt (ichan) - radiance % bt (ichan)) * cld_radiance_ad % bt (ichan)
     radiance_ad  % bt    (ichan) = radiance_ad  % bt    (ichan) & 
                                   & + (1.0_JPRB - scatt_aux % ccmax (iprof)) * cld_radiance_ad % bt (ichan)
     cld_radiance_ad % bt (ichan) = scatt_aux    % ccmax (iprof) * cld_radiance_ad % bt (ichan) 
  End Do
    
!* 3.   Eddington (in temperature space)
  Call rttov_eddington_ad(   &
        & nwp_levels,        &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & lsprofiles,        &! in
        & angles,            &! in
        & profiles,          &! in  
        & profiles_ad,       &! inout  
        & cld_profiles,      &! in  
        & scatt_aux,         &! in
        & scatt_aux_ad,      &! inout
        & cld_radiance,      &! inout  
        & cld_radiance_ad)    ! inout   

!*  2.   Initialisations for Eddington 
 
 
  Call rttov_iniscatt_ad(    &
        & errorstatus,       &! out
        & nwp_levels,        &! in
        & nrt_levels,        &! in
        & nfrequencies,      &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & polarisations,     &! in
        & channels,          &! in
        & frequencies,       &! in
        & lprofiles,        &! in
        & lsprofiles,        &! in
        & profiles,          &! in 
        & profiles_ad,       &! inout
        & cld_profiles,      &! in
        & cld_profiles_ad,   &! inout
        & coef_rttov,        &! in 
        & coef_scatt,        &! in  
        & transmission,      &! in
        & transmission_ad,   &! inout
        & calcemiss,         &! in
        & angles,            &! out
        & scatt_aux,         &! inout
        & scatt_aux_ad)       ! inout

  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_iniscatt_ad")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  radiance_ad % clear     (:)   = 0.0_JPRB
  radiance_ad % clear_out (:)   = 0.0_JPRB
  radiance_ad % cloudy    (:)   = 0.0_JPRB
  radiance_ad % total     (:)   = 0.0_JPRB
  radiance_ad % total_out (:)   = 0.0_JPRB
  radiance_ad % out       (:)   = 0.0_JPRB
  radiance_ad % out_clear (:)   = 0.0_JPRB
  radiance_ad % bt_clear  (:)   = 0.0_JPRB
  radiance_ad % upclear   (:)   = 0.0_JPRB
  radiance_ad % dnclear   (:)   = 0.0_JPRB
  radiance_ad % reflclear (:)   = 0.0_JPRB
  radiance_ad % overcast  (:,:) = 0.0_JPRB
  radiance_ad % downcld   (:,:) = 0.0_JPRB
 
!*         1.   Gas absorption
  emissivity_ad (:) = 0.0_JPRB
  
  Call rttov_ad(        &
     & errorstatus,     &! out
     & nfrequencies,    &! in
     & nchannels,       &! in
     & nbtout,          &! in
     & nprofiles,       &! in
     & channels,        &! in
     & polarisations,   &! in
     & lprofiles,       &! in
     & profiles,        &! in
     & coef_rttov,      &! in
     & addcloud,        &! in
     & switchrad,       &! in
     & calcemiss,       &! in
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
  
  emissivity_in_ad (:) = emissivity_in_ad (:) + emissivity_ad (:)
  emissivity_ad    (:) = 0.0_JPRB

  cld_radiance % bt (:) = zcld_radiance % bt (:)

End Subroutine rttov_scatt_ad
