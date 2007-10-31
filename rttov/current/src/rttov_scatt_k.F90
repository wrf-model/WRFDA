!
Subroutine rttov_scatt_k( & 
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
     & profiles_k,         &! inout
     & cld_profiles_k,     &! inout
     & emissivity_in_k,    &! inout
     & cld_radiance)        ! inout
    
  ! Description:
  ! AD of subroutine 
  ! to compute microwave multi-channel radiances and brightness
  ! temperatures for many profiles per call in a cloudy and/or rainy sky.
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
  !   1.3    08/2004      Polarimetry fixes   (U. O'Keefe)
  !   1.4    11/2004      Clean-up            (P. Bauer)
  !   1.5    02/2005      K code              (A. Collard)
  !   1.6    07/2005      Polarimetry fixes   (U. O'Keeffe)
  !   1.7    11/2005      Add errorstatus to iniscatt arguments and use a temporary
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
       & errorstatus_success ,&
       & errorstatus_fatal   ,&
       & sensor_id_mw

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
#include "rttov_k.interface"
#include "rttov_iniscatt_k.interface"
#include "rttov_eddington_k.interface"
#include "rttov_errorreport.interface"
#include "rttov_calcpolarisation_ad.interface"
#include "rttov_profout_k.interface"
#include "rttov_cld_profout_k.interface"

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
  Logical,             Intent (in)    :: calcemiss        (nchannels)          ! Surface emmissivity 
  Real    (Kind=jprb), Intent (in)    :: emissivity_in    (nchannels)          ! Surface emmissivity 
  Real    (Kind=jprb), Intent (inout) :: emissivity_in_k  (nchannels)            
  Type (profile_Type),        Intent (inout) :: profiles        (nprofiles)    ! Atmospheric profiles
  Type (profile_Type),        Intent (inout) :: profiles_k      (nbtout)    
  Type (rttov_coef),          Intent (in)    :: coef_rttov                     ! RTTOV Coefficients
  Type (rttov_scatt_coef),    Intent (in)    :: coef_scatt                     ! RTTOV_SCATT Coefficients
  Type (profile_cloud_Type),  Intent (in)    :: cld_profiles    (nprofiles)    ! Cloud profiles with NWP levels
  Type (profile_cloud_Type),  Intent (inout) :: cld_profiles_k  (nbtout)   
  Type (radiance_cloud_Type), Intent (inout) :: cld_radiance                   ! Radiances
   
  Integer (Kind=jpim), target :: sa__mclayer    (nchannels)
  Integer (Kind=jpim), target :: sa_k__mclayer (nchannels)

  Real (kind=jprb), target :: cld_r_k__clear_out (nbtout)
  Real (kind=jprb), target :: cld_r_k__out       (nbtout)
  Real (kind=jprb), target :: cld_r_k__out_clear (nbtout)
  Real (kind=jprb), target :: cld_r_k__total_out (nbtout)
  Real (kind=jprb), target :: cld_r_k__clear     (nchannels)
  Real (kind=jprb), target :: cld_r_k__cloudy    (nchannels)
  Real (kind=jprb), target :: cld_r_k__total     (nchannels)
  Real (kind=jprb), target :: cld_r_k__bt        (nchannels)
  Real (kind=jprb), target :: cld_r_k__bt_clear  (nchannels)
  Real (kind=jprb), target :: cld_r_k__upclear   (nchannels)
  Real (kind=jprb), target :: cld_r_k__dnclear   (nchannels)
  Real (kind=jprb), target :: cld_r_k__reflclear (nchannels)
  Real (Kind=jprb), target :: cld_r_k__overcast  (nrt_levels,nchannels)
  Real (Kind=jprb), target :: cld_r_k__downcld   (nrt_levels,nchannels)

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

  Real (kind=jprb), target :: r_k__clear_out (nbtout)
  Real (kind=jprb), target :: r_k__out       (nbtout)
  Real (kind=jprb), target :: r_k__out_clear (nbtout)
  Real (kind=jprb), target :: r_k__total_out (nbtout)
  Real (kind=jprb), target :: r_k__clear     (nchannels)
  Real (kind=jprb), target :: r_k__cloudy    (nchannels)
  Real (kind=jprb), target :: r_k__total     (nchannels)
  Real (kind=jprb), target :: r_k__bt        (nchannels)
  Real (kind=jprb), target :: r_k__bt_clear  (nchannels)
  Real (kind=jprb), target :: r_k__upclear   (nchannels)
  Real (kind=jprb), target :: r_k__dnclear   (nchannels)
  Real (kind=jprb), target :: r_k__reflclear (nchannels)
  Real (Kind=jprb), target :: r_k__overcast  (nrt_levels,nchannels)
  Real (Kind=jprb), target :: r_k__downcld   (nrt_levels,nchannels)
      
  Real (Kind=jprb), target :: t__tau_surf          (nchannels)
  Real (Kind=jprb), target :: t__tau_layer         (nrt_levels,nchannels)
  Real (Kind=jprb), target :: t__od_singlelayer    (nrt_levels,nchannels)
  Real (Kind=jprb), target :: t_k__tau_surf        (nchannels)
  Real (Kind=jprb), target :: t_k__tau_layer       (nrt_levels,nchannels)
  Real (Kind=jprb), target :: t_k__od_singlelayer  (nrt_levels,nchannels)
  
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

  Real (Kind=jprb), target :: sa_k__ccmax   (nchannels)
  Real (Kind=jprb), target :: sa_k__ems_bnd (nchannels)
  Real (Kind=jprb), target :: sa_k__ref_bnd (nchannels)
  Real (Kind=jprb), target :: sa_k__ems_cld (nchannels)
  Real (Kind=jprb), target :: sa_k__ref_cld (nchannels)
  
  Real (Kind=jprb), target :: sa_k__tbd (nchannels,nwp_levels+1)
  
  Real (Kind=jprb), target :: sa_k__delta  (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__tau    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__ext    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__ssa    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__asm    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__lambda (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__h      (nchannels,nwp_levels)
  
  Real (Kind=jprb), target :: sa_k__b0     (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__b1     (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__bn     (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__dz     (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__clw    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__ciw    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__rain   (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_k__sp     (nchannels,nwp_levels)

!* Local variables:
  Logical             :: addcloud, switchrad
  Integer (Kind=jpim) :: iprof, ichan, ilev   
  Real    (Kind=jprb) :: emissivity    (nchannels)
  Real    (Kind=jprb) :: emissivity_k (nchannels)
    
  Type (radiance_cloud_Type) :: cld_radiance_k          
  Type (transmission_Type)   :: transmission, transmission_k
  Type (geometry_Type)       :: angles (nprofiles)
  Type (profile_scatt_aux)   :: scatt_aux, scatt_aux_k
  Type (radiance_Type)       :: radiance, radiance_k
  Type (radiance_cloud_Type) :: zcld_radiance            
  Type (profile_Type)        :: profiles_k_all(nchannels)
  Type (profile_cloud_Type)  :: cld_profiles_k_all(nchannels)
  Type (radiance_Type)       :: cld_radiance_tmp

  Character (len=80) :: errMessage
  Character (len=15) :: NameOfRoutine = 'rttov_scatt_k '

  !- End of header --------------------------------------------------------
  
  errorstatus(:)  = errorstatus_success

  cld_radiance_k % clear_out => cld_r_k__clear_out
  cld_radiance_k % out       => cld_r_k__out
  cld_radiance_k % out_clear => cld_r_k__out_clear
  cld_radiance_k % total_out => cld_r_k__total_out
  cld_radiance_k % clear     => cld_r_k__clear
  cld_radiance_k % cloudy    => cld_r_k__cloudy
  cld_radiance_k % total     => cld_r_k__total
  cld_radiance_k % bt        => cld_r_k__bt
  cld_radiance_k % bt_clear  => cld_r_k__bt_clear
  cld_radiance_k % upclear   => cld_r_k__upclear
  cld_radiance_k % dnclear   => cld_r_k__dnclear
  cld_radiance_k % reflclear => cld_r_k__reflclear
  cld_radiance_k % overcast  => cld_r_k__overcast 
  cld_radiance_k % downcld   => cld_r_k__downcld 

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

  radiance_k % clear_out => r_k__clear_out
  radiance_k % out       => r_k__out
  radiance_k % out_clear => r_k__out_clear
  radiance_k % total_out => r_k__total_out
  radiance_k % clear     => r_k__clear
  radiance_k % cloudy    => r_k__cloudy
  radiance_k % total     => r_k__total
  radiance_k % bt        => r_k__bt
  radiance_k % bt_clear  => r_k__bt_clear
  radiance_k % upclear   => r_k__upclear
  radiance_k % dnclear   => r_k__dnclear
  radiance_k % reflclear => r_k__reflclear
  radiance_k % overcast  => r_k__overcast 
  radiance_k % downcld   => r_k__downcld 

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

  transmission_k % tau_surf       => t_k__tau_surf
  transmission_k % tau_layer      => t_k__tau_layer
  transmission_k % od_singlelayer => t_k__od_singlelayer

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

  scatt_aux_k % ccmax    => sa_k__ccmax
  scatt_aux_k % ems_bnd  => sa_k__ems_bnd
  scatt_aux_k % ref_bnd  => sa_k__ref_bnd
  scatt_aux_k % ems_cld  => sa_k__ems_cld
  scatt_aux_k % ref_cld  => sa_k__ref_cld
  scatt_aux_k % tbd      => sa_k__tbd
  scatt_aux_k % mclayer  => sa_k__mclayer
  scatt_aux_k % delta    => sa_k__delta
  scatt_aux_k % tau      => sa_k__tau
  scatt_aux_k % ext      => sa_k__ext
  scatt_aux_k % ssa      => sa_k__ssa
  scatt_aux_k % asm      => sa_k__asm
  scatt_aux_k % lambda   => sa_k__lambda
  scatt_aux_k % h        => sa_k__h
  scatt_aux_k % b0       => sa_k__b0
  scatt_aux_k % b1       => sa_k__b1
  scatt_aux_k % bn       => sa_k__bn
  scatt_aux_k % dz       => sa_k__dz
  scatt_aux_k % clw      => sa_k__clw
  scatt_aux_k % ciw      => sa_k__ciw
  scatt_aux_k % rain     => sa_k__rain
  scatt_aux_k % sp       => sa_k__sp

  Do ichan = 1, nchannels
     Allocate( profiles_k_all(ichan) % p(coef_rttov%nlevels ))
     Allocate( profiles_k_all(ichan) % t(coef_rttov%nlevels ))
     Allocate( profiles_k_all(ichan) % q(coef_rttov%nlevels ))
     Allocate( profiles_k_all(ichan) % o3(coef_rttov%nlevels ))
     Allocate( profiles_k_all(ichan) % co2(coef_rttov%nlevels ))
     Allocate( profiles_k_all(ichan) % clw(coef_rttov%nlevels ))
     Allocate( cld_profiles_k_all(ichan) % p(nwp_levels))
     Allocate( cld_profiles_k_all(ichan) % ph(nwp_levels+1))
     Allocate( cld_profiles_k_all(ichan) % t(nwp_levels))
     Allocate( cld_profiles_k_all(ichan) % q(nwp_levels))
     Allocate( cld_profiles_k_all(ichan) % cc(nwp_levels))
     Allocate( cld_profiles_k_all(ichan) % clw(nwp_levels))
     Allocate( cld_profiles_k_all(ichan) % ciw(nwp_levels))
     Allocate( cld_profiles_k_all(ichan) % rain(nwp_levels))
     Allocate( cld_profiles_k_all(ichan) % sp(nwp_levels))
     Do ilev = 1,coef_rttov%nlevels
        profiles_k_all(ichan) % clw (ilev) = 0.0_JPRB
        profiles_k_all(ichan) % co2 (ilev) = 0.0_JPRB
        profiles_k_all(ichan) % o3 (ilev) = 0.0_JPRB
        profiles_k_all(ichan) % t (ilev) = 0.0_JPRB
        profiles_k_all(ichan) % q (ilev) = 0.0_JPRB
        profiles_k_all(ichan) % p (ilev) = 0.0_JPRB
     Enddo
     profiles_k_all(ichan) % s2m % t  =0.0_JPRB
     profiles_k_all(ichan) % s2m % u  =0.0_JPRB
     profiles_k_all(ichan) % s2m % v  =0.0_JPRB
     profiles_k_all(ichan) % s2m % q  =0.0_JPRB
     profiles_k_all(ichan) % s2m % o  =0.0_JPRB
     profiles_k_all(ichan) % s2m % p  =0.0_JPRB
     profiles_k_all(ichan) % skin % t  =0.0_JPRB
     profiles_k_all(ichan) % skin % fastem(1)  =0.0_JPRB
     profiles_k_all(ichan) % skin % fastem(2)  =0.0_JPRB
     profiles_k_all(ichan) % skin % fastem(3)  =0.0_JPRB
     profiles_k_all(ichan) % skin % fastem(4)  =0.0_JPRB
     profiles_k_all(ichan) % skin % fastem(5)  =0.0_JPRB
     profiles_k_all(ichan) % ctp  =0.0_JPRB
     profiles_k_all(ichan) % cfraction  =0.0_JPRB
     profiles_k_all(ichan) % nlevels =  coef_rttov % nlevels
     ! The next five are initialised for completeness - they are not used.
     profiles_k_all(ichan) % zenangle = 0.0_JPRB
     profiles_k_all(ichan) % azangle = 0.0_JPRB
     profiles_k_all(ichan) % skin % surftype = 0_JPIM
     profiles_k_all(ichan) % ozone_data = .true.
     profiles_k_all(ichan) % co2_data = .true.
     profiles_k_all(ichan) % clw_data = .true.
     Do ilev = 1,nwp_levels
        cld_profiles_k_all(ichan) % p(ilev)=0.0_JPRB
        cld_profiles_k_all(ichan) % ph(ilev)=0.0_JPRB
        cld_profiles_k_all(ichan) % t(ilev)=0.0_JPRB
        cld_profiles_k_all(ichan) % q(ilev)=0.0_JPRB
        cld_profiles_k_all(ichan) % cc(ilev)=0.0_JPRB
        cld_profiles_k_all(ichan) % clw(ilev)=0.0_JPRB
        cld_profiles_k_all(ichan) % ciw(ilev)=0.0_JPRB
        cld_profiles_k_all(ichan) % rain(ilev)=0.0_JPRB
        cld_profiles_k_all(ichan) % sp(ilev)=0.0_JPRB
     Enddo
     cld_profiles_k_all(ichan) % ph(nwp_levels+1) =0.0_JPRB
     cld_profiles_k_all(ichan) % nlevels =  nwp_levels
     ! The next two are initialised for completeness - they are not used.
     cld_profiles_k_all(ichan) % kice   =  0_JPIM
     cld_profiles_k_all(ichan) % kradip =  0_JPIM
  END Do

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

! Initialise cld_radiance_k

  cld_radiance_k % clear(:)      = 0._JPRB
  cld_radiance_k % clear_out(:)  = 0._JPRB
  cld_radiance_k % out_clear(:)  = 0._JPRB
  cld_radiance_k % cloudy(:)     = 0._JPRB
  cld_radiance_k % bt_clear(:)   = 0._JPRB
  cld_radiance_k % upclear(:)    = 0._JPRB
  cld_radiance_k % reflclear(:)  = 0._JPRB
  cld_radiance_k % overcast(:,:) = 0._JPRB
  cld_radiance_k % downcld(:,:)  = 0._JPRB

!  cld_radiance_k % bt(:)         = 1._JPRB
  cld_radiance_k % bt(:)         = 0._JPRB
  cld_radiance_k % total(:)      = 0._JPRB
  cld_radiance_k % out(:)        = 1._JPRB
  cld_radiance_k % total_out(:)  = 0._JPRB


  scatt_aux_k % ccmax   (:)   = 0.0_JPRB
  scatt_aux_k % ems_bnd (:)   = 0.0_JPRB
  scatt_aux_k % ref_bnd (:)   = 0.0_JPRB
  scatt_aux_k % ems_cld (:)   = 0.0_JPRB
  scatt_aux_k % ref_cld (:)   = 0.0_JPRB
  scatt_aux_k % tbd     (:,:) = 0.0_JPRB
  scatt_aux_k % delta   (:,:) = 0.0_JPRB
  scatt_aux_k % tau     (:,:) = 0.0_JPRB
  scatt_aux_k % ext     (:,:) = 0.0_JPRB
  scatt_aux_k % ssa     (:,:) = 0.0_JPRB
  scatt_aux_k % asm     (:,:) = 0.0_JPRB
  scatt_aux_k % lambda  (:,:) = 0.0_JPRB
  scatt_aux_k % h       (:,:) = 0.0_JPRB
  scatt_aux_k % b0      (:,:) = 0.0_JPRB
  scatt_aux_k % b1      (:,:) = 0.0_JPRB
  scatt_aux_k % bn      (:,:) = 0.0_JPRB
  scatt_aux_k % dz      (:,:) = 0.0_JPRB
  scatt_aux_k % clw     (:,:) = 0.0_JPRB
  scatt_aux_k % ciw     (:,:) = 0.0_JPRB
  scatt_aux_k % rain    (:,:) = 0.0_JPRB
  scatt_aux_k % sp      (:,:) = 0.0_JPRB
  
  transmission_k % tau_surf       (:)   = 0.0_JPRB
  transmission_k % tau_layer      (:,:) = 0.0_JPRB
  transmission_k % od_singlelayer (:,:) = 0.0_JPRB
    
  radiance_k % bt (:) = 0.0_JPRB


  !
  !*  5.   Convert total polarisations length arrays to number of output channel length arrays
  !
  If (coef_rttov % id_sensor == sensor_id_mw) Then

    ! Point a temporary radiance type at cld_radiance_k
    cld_radiance_tmp % clear     => cld_radiance_k % clear
    cld_radiance_tmp % clear_out => cld_radiance_k % clear_out
    cld_radiance_tmp % cloudy    => cld_radiance_k % cloudy
    cld_radiance_tmp % total     => cld_radiance_k % total
    cld_radiance_tmp % total_out => cld_radiance_k % total_out
    cld_radiance_tmp % out       => cld_radiance_k % out
    cld_radiance_tmp % out_clear => cld_radiance_k % out_clear
    cld_radiance_tmp % bt        => cld_radiance_k % bt
    cld_radiance_tmp % bt_clear  => cld_radiance_k % bt_clear
    cld_radiance_tmp % upclear   => cld_radiance_k % upclear
    cld_radiance_tmp % dnclear   => cld_radiance_k % dnclear
    cld_radiance_tmp % reflclear => cld_radiance_k % reflclear
    cld_radiance_tmp % overcast  => cld_radiance_k % overcast
    cld_radiance_tmp % downcld   => cld_radiance_k % downcld

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
      &  cld_radiance_tmp      )      ! inout
  Else
        radiance_k%bt       = radiance_k%out
        radiance_k%bt_clear = radiance_k%out_clear
        cld_radiance_k%bt       = cld_radiance_k%out
        cld_radiance_k%bt_clear = cld_radiance_k%out_clear
  End If

!* 4.   Combine clear and cloudy parts  
  Do ichan = 1, nchannels
     iprof = lsprofiles (ichan)
       
     scatt_aux_k % ccmax (ichan) = scatt_aux_k % ccmax (ichan) + &
          (zcld_radiance % bt (ichan) - &
          radiance % bt (ichan)) * cld_radiance_k % bt (ichan)
     radiance_k  % bt    (ichan) = radiance_k  % bt    (ichan) + &
          (1.0_JPRB - scatt_aux % ccmax (iprof)) * cld_radiance_k % bt (ichan)
     cld_radiance_k % bt (ichan) = scatt_aux % ccmax (iprof) * &
          cld_radiance_k % bt (ichan) 
  End Do

!* 3.   Eddington (in temperature space)
  Call rttov_eddington_k(    &
        & nwp_levels,        &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & lsprofiles,        &! in
        & angles,            &! in
        & profiles,          &! in  
        & profiles_k_all,    &! inout  
        & cld_profiles,      &! in  
        & scatt_aux,         &! in
        & scatt_aux_k,       &! inout
        & cld_radiance,      &! inout  
        & cld_radiance_k)    ! inout   

!*  2.   Initialisations for Eddington  
  Call rttov_iniscatt_k(     &
        & errorstatus,       &! in
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
        & profiles_k_all,    &! inout
        & cld_profiles,      &! in
        & cld_profiles_k_all,&! inout
        & coef_rttov,        &! in 
        & coef_scatt,        &! in  
        & transmission,      &! in
        & transmission_k,    &! inout
        & calcemiss,         &! in
        & angles,            &! out
        & scatt_aux,         &! inout
        & scatt_aux_k)       ! inout   

  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_iniscatt_k")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  emissivity_in_k (:) = emissivity_in_k (:) - scatt_aux_k % ref_cld (:)
  scatt_aux_k % ref_cld (:) = 0.0_JPRB      
  emissivity_in_k (:) = emissivity_in_k (:) + scatt_aux_k % ems_cld (:)  
  scatt_aux_k % ems_cld (:) = 0.0_JPRB   
 
  radiance_k % clear     (:)   = 0.0_JPRB
  radiance_k % clear_out (:)   = 0.0_JPRB
  radiance_k % cloudy    (:)   = 0.0_JPRB
  radiance_k % total     (:)   = 0.0_JPRB
  radiance_k % total_out (:)   = 0.0_JPRB
  radiance_k % out       (:)   = 0.0_JPRB
  radiance_k % out_clear (:)   = 0.0_JPRB
  radiance_k % bt_clear  (:)   = 0.0_JPRB
  radiance_k % upclear   (:)   = 0.0_JPRB
  radiance_k % dnclear   (:)   = 0.0_JPRB
  radiance_k % reflclear (:)   = 0.0_JPRB
  radiance_k % overcast  (:,:) = 0.0_JPRB
  radiance_k % downcld   (:,:) = 0.0_JPRB
  
 
!*         1.   Gas absorption
  emissivity_k (:) = 0.0_JPRB
  
  Call rttov_k(         &
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
     & profiles_k_all,  &! inout
     & emissivity_k,    &! inout
     & transmission,    &! inout
     & transmission_k,  &! inout
     & radiance,        &! inout
     & radiance_k      ) ! inout 

  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_k")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If
  
  emissivity_in_k (:) = emissivity_in_k (:) + emissivity_k (:)
  emissivity_k    (:) = 0.0_JPRB

  cld_radiance % bt (:) = zcld_radiance % bt (:)


  If (coef_rttov % id_sensor == sensor_id_mw) Then
  ! We have K wrt all calculated TBs - but user wants K for
  ! instrument channels, so K code only requires an extra routine to modify
  ! output. In AD code we simply exclude unused channels. Note only required
  ! for microwave calculations.

     Call rttov_profout_k( &
            & nfrequencies,      &! in
            & nchannels,         &! in
            & nbtout,            &! in
            & nprofiles,         &! in
            & channels,          &! in
            & lprofiles,         &! in
            & polarisations,     &! in
            & coef_rttov,        &! in
            & angles,            &! in
            & profiles_k_all,    &! in
            & profiles_k)         ! Out
     Call rttov_cld_profout_k( &
            & nfrequencies,          &! in
            & nchannels,             &! in
            & nbtout,                &! in
            & nprofiles,             &! in
            & channels,              &! in
            & lprofiles,             &! in
            & polarisations,         &! in
            & coef_rttov,            &! in
            & angles,                &! in
            & cld_profiles_k_all,    &! in
            & cld_profiles_k)         ! Out
  Else
     !profiles_k  = profiles_k_all
     Do ichan = 1, nchannels
        profiles_k(ichan) % nlevels          = profiles_k_all(ichan) % nlevels
        profiles_k(ichan) % s2m % t          = profiles_k_all(ichan) % s2m % t
        profiles_k(ichan) % s2m % q          = profiles_k_all(ichan) % s2m % q
        profiles_k(ichan) % s2m % p          = profiles_k_all(ichan) % s2m % p
        profiles_k(ichan) % s2m % u          = profiles_k_all(ichan) % s2m % u
        profiles_k(ichan) % s2m % v          = profiles_k_all(ichan) % s2m % v
        profiles_k(ichan) % skin % t         = profiles_k_all(ichan) % skin % t
        profiles_k(ichan) % skin % fastem(1) = profiles_k_all(ichan) % skin % fastem(1)
        profiles_k(ichan) % skin % fastem(2) = profiles_k_all(ichan) % skin % fastem(2)
        profiles_k(ichan) % skin % fastem(3) = profiles_k_all(ichan) % skin % fastem(3)
        profiles_k(ichan) % skin % fastem(4) = profiles_k_all(ichan) % skin % fastem(4)
        profiles_k(ichan) % skin % fastem(5) = profiles_k_all(ichan) % skin % fastem(5)
        profiles_k(ichan) % ctp              = profiles_k_all(ichan) % ctp
        profiles_k(ichan) % cfraction        = profiles_k_all(ichan) % cfraction
        Do  ilev=1,coef_rttov%nlevels
           profiles_k(ichan) % t(ilev)         = profiles_k_all(ichan) % t(ilev)
           profiles_k(ichan) % q(ilev)         = profiles_k_all(ichan) % q(ilev)
           profiles_k(ichan) % o3(ilev)        = profiles_k_all(ichan) % o3(ilev)
           profiles_k(ichan) % clw(ilev)       = profiles_k_all(ichan) % clw(ilev)
        End Do
       cld_profiles_k(ichan) % nlevels        = cld_profiles_k_all(ichan) % nlevels
       Do  ilev=1,nwp_levels
          cld_profiles_k(ichan) % p(ilev)    = cld_profiles_k_all(ichan) % p(ilev)
          cld_profiles_k(ichan) % ph(ilev)   = cld_profiles_k_all(ichan) % ph(ilev)
          cld_profiles_k(ichan) % t(ilev)    = cld_profiles_k_all(ichan) % t(ilev)
          cld_profiles_k(ichan) % q(ilev)    = cld_profiles_k_all(ichan) % q(ilev)
          cld_profiles_k(ichan) % cc(ilev)   = cld_profiles_k_all(ichan) % cc(ilev)
          cld_profiles_k(ichan) % clw(ilev)  = cld_profiles_k_all(ichan) % clw(ilev)
          cld_profiles_k(ichan) % ciw(ilev)  = cld_profiles_k_all(ichan) % ciw(ilev)
          cld_profiles_k(ichan) % rain(ilev) = cld_profiles_k_all(ichan) % rain(ilev)
          cld_profiles_k(ichan) % sp(ilev)   = cld_profiles_k_all(ichan) % sp(ilev)
       enddo
       cld_profiles_k(ichan) % ph(nwp_levels+1)  = cld_profiles_k_all(ichan) % ph(nwp_levels+1)
     End Do
  End If


!   Deallocate memory

  Do ichan = 1, nchannels
     If( Associated(  profiles_k_all(ichan) % p )) Then
        Deallocate( profiles_k_all(ichan) % p)
        Deallocate( profiles_k_all(ichan) % t)
        Deallocate( profiles_k_all(ichan) % q)
        Deallocate( profiles_k_all(ichan) % o3)
        Deallocate( profiles_k_all(ichan) % co2)
        Deallocate( profiles_k_all(ichan) % clw)
     End If
     If( Associated(  cld_profiles_k_all(ichan) % p )) Then
        Deallocate( cld_profiles_k_all(ichan) % p)
        Deallocate( cld_profiles_k_all(ichan) % ph)
        Deallocate( cld_profiles_k_all(ichan) % t)
        Deallocate( cld_profiles_k_all(ichan) % q)
        Deallocate( cld_profiles_k_all(ichan) % cc)
        Deallocate( cld_profiles_k_all(ichan) % clw)
        Deallocate( cld_profiles_k_all(ichan) % ciw)
        Deallocate( cld_profiles_k_all(ichan) % rain)
        Deallocate( cld_profiles_k_all(ichan) % sp)
     End If
  End do



End Subroutine rttov_scatt_k



