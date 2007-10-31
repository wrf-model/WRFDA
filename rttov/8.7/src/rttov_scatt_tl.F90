!
Subroutine rttov_scatt_tl( &
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
     & profiles_tl,        &! in
     & cld_profiles_tl,    &! in
     & emissivity_in_tl,   &! inout
     & cld_radiance,       &! inout
     & cld_radiance_tl)     ! inout 
     
  ! Description:
  ! TL of subroutine 
  ! to compute microwave multi-channel radiances and brightness
  ! temperatures for many profiles per call in a cloudy and/or rainy sky.
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

#include "rttov_tl.interface"
#include "rttov_iniscatt_tl.interface"
#include "rttov_eddington_tl.interface"
#include "rttov_errorreport.interface"
#include "rttov_calcpolarisation.interface"
#include "rttov_calcpolarisation_tl.interface"

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

  Logical,             Intent (in)  :: calcemiss        (nchannels)            ! Switch for emmissivity calculation
  Real    (Kind=jprb), Intent (in)  :: emissivity_in    (nchannels)            ! Surface emmissivity 
  Real    (Kind=jprb), Intent (in)  :: emissivity_in_tl (nchannels)            ! Surface emmissivity 
  
  Type (profile_Type),        Intent (inout) :: profiles        (nprofiles)    ! Atmospheric profiles
  Type (profile_Type),        Intent (inout) :: profiles_tl     (nprofiles)    
  Type (rttov_coef),          Intent (in)    :: coef_rttov                     ! RTTOV Coefficients
  Type (rttov_scatt_coef),    Intent (in)    :: coef_scatt                     ! RTTOV_SCATT Coefficients
  Type (profile_cloud_Type),  Intent (in)    :: cld_profiles    (nprofiles)    ! Cloud profiles with NWP levels
  Type (profile_cloud_Type),  Intent (in)    :: cld_profiles_tl (nprofiles)   
  Type (radiance_cloud_Type), Intent (inout) :: cld_radiance                   ! Radiances
  Type (radiance_cloud_Type), Intent (inout) :: cld_radiance_tl          
 
  Integer (Kind=jpim), target :: sa__mclayer    (nchannels)
  Integer (Kind=jpim), target :: sa_tl__mclayer (nchannels)

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

  Real (kind=jprb), target :: r_tl__clear_out (nbtout)
  Real (kind=jprb), target :: r_tl__out       (nbtout)
  Real (kind=jprb), target :: r_tl__out_clear (nbtout)
  Real (kind=jprb), target :: r_tl__total_out (nbtout)
  Real (kind=jprb), target :: r_tl__clear     (nchannels)
  Real (kind=jprb), target :: r_tl__cloudy    (nchannels)
  Real (kind=jprb), target :: r_tl__total     (nchannels)
  Real (kind=jprb), target :: r_tl__bt        (nchannels)
  Real (kind=jprb), target :: r_tl__bt_clear  (nchannels)
  Real (kind=jprb), target :: r_tl__upclear   (nchannels)
  Real (kind=jprb), target :: r_tl__dnclear   (nchannels)
  Real (kind=jprb), target :: r_tl__reflclear (nchannels)
  Real (Kind=jprb), target :: r_tl__overcast  (nrt_levels,nchannels)
  Real (Kind=jprb), target :: r_tl__downcld   (nrt_levels,nchannels)
   
  Real (Kind=jprb), target :: t__tau_surf          (nchannels)
  Real (Kind=jprb), target :: t__tau_layer         (nrt_levels,nchannels)
  Real (Kind=jprb), target :: t__od_singlelayer    (nrt_levels,nchannels)
  Real (Kind=jprb), target :: t_tl__tau_surf       (nchannels)
  Real (Kind=jprb), target :: t_tl__tau_layer      (nrt_levels,nchannels)
  Real (Kind=jprb), target :: t_tl__od_singlelayer (nrt_levels,nchannels)
  
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

  Real (Kind=jprb), target :: sa_tl__ccmax   (nprofiles)
  Real (Kind=jprb), target :: sa_tl__ems_bnd (nchannels)
  Real (Kind=jprb), target :: sa_tl__ref_bnd (nchannels)
  Real (Kind=jprb), target :: sa_tl__ems_cld (nchannels)
  Real (Kind=jprb), target :: sa_tl__ref_cld (nchannels)
  
  Real (Kind=jprb), target :: sa_tl__tbd (nprofiles,nwp_levels+1)
  
  Real (Kind=jprb), target :: sa_tl__delta  (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__tau    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__ext    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__ssa    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__asm    (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__lambda (nchannels,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__h      (nchannels,nwp_levels)
  
  Real (Kind=jprb), target :: sa_tl__b0     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__b1     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__bn     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__dz     (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__clw    (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__ciw    (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__rain   (nprofiles,nwp_levels)
  Real (Kind=jprb), target :: sa_tl__sp     (nprofiles,nwp_levels)

!* Local variables:
  Logical             :: addcloud, switchrad
  Integer (Kind=jpim) :: iprof, ichan     
  Real    (Kind=jprb) :: emissivity    (nchannels)            
  Real    (Kind=jprb) :: emissivity_tl (nchannels)  
  
  Type (transmission_Type) :: transmission, transmission_tl
  Type (geometry_Type)     :: angles (nprofiles)
  Type (profile_scatt_aux) :: scatt_aux, scatt_aux_tl
  Type (radiance_Type)     :: radiance, radiance_tl
  Type (radiance_Type)     :: cld_radiance_tmp

  Character (len=80) :: errMessage
  Character (len=15) :: NameOfRoutine = 'rttov_scatt_tl '

  !- End of header --------------------------------------------------------
  
  errorstatus (:) = errorstatus_success

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

  radiance_tl % clear_out => r_tl__clear_out
  radiance_tl % out       => r_tl__out
  radiance_tl % out_clear => r_tl__out_clear
  radiance_tl % total_out => r_tl__total_out
  radiance_tl % clear     => r_tl__clear
  radiance_tl % cloudy    => r_tl__cloudy
  radiance_tl % total     => r_tl__total
  radiance_tl % bt        => r_tl__bt
  radiance_tl % bt_clear  => r_tl__bt_clear
  radiance_tl % upclear   => r_tl__upclear
  radiance_tl % dnclear   => r_tl__dnclear
  radiance_tl % reflclear => r_tl__reflclear
  radiance_tl % overcast  => r_tl__overcast 
  radiance_tl % downcld   => r_tl__downcld 

  transmission % tau_surf       => t__tau_surf
  transmission % tau_layer      => t__tau_layer
  transmission % od_singlelayer => t__od_singlelayer

  transmission_tl % tau_surf       => t_tl__tau_surf
  transmission_tl % tau_layer      => t_tl__tau_layer
  transmission_tl % od_singlelayer => t_tl__od_singlelayer

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

  scatt_aux_tl % ccmax    => sa_tl__ccmax
  scatt_aux_tl % ems_bnd  => sa_tl__ems_bnd
  scatt_aux_tl % ref_bnd  => sa_tl__ref_bnd
  scatt_aux_tl % ems_cld  => sa_tl__ems_cld
  scatt_aux_tl % ref_cld  => sa_tl__ref_cld
  scatt_aux_tl % tbd      => sa_tl__tbd
  scatt_aux_tl % mclayer  => sa_tl__mclayer
  scatt_aux_tl % delta    => sa_tl__delta
  scatt_aux_tl % tau      => sa_tl__tau
  scatt_aux_tl % ext      => sa_tl__ext
  scatt_aux_tl % ssa      => sa_tl__ssa
  scatt_aux_tl % asm      => sa_tl__asm
  scatt_aux_tl % lambda   => sa_tl__lambda
  scatt_aux_tl % h        => sa_tl__h
  scatt_aux_tl % b0       => sa_tl__b0
  scatt_aux_tl % b1       => sa_tl__b1
  scatt_aux_tl % bn       => sa_tl__bn
  scatt_aux_tl % dz       => sa_tl__dz
  scatt_aux_tl % clw      => sa_tl__clw
  scatt_aux_tl % ciw      => sa_tl__ciw
  scatt_aux_tl % rain     => sa_tl__rain
  scatt_aux_tl % sp       => sa_tl__sp

!*         1.   Gas absorption
  switchrad = .true.   ! input to RTTOV is BT  
  addcloud  = .false.
  
  ! No calculation of CLW absorption inside "classical" RTTOV
  If ( Any(.Not.profiles(:) % clw_Data) ) Then
     ! warning message
     profiles (:) % clw_Data = .False.
  End If
  
  emissivity    (:) = emissivity_in    (:)
  emissivity_tl (:) = emissivity_in_tl (:)

  Call rttov_tl(             &
     & errorstatus,          &! out
     & nfrequencies,         &! in
     & nchannels,            &! in
     & nbtout,               &! in
     & nprofiles,            &! in
     & channels,             &! in
     & polarisations,        &! in
     & lprofiles,            &! in
     & profiles,             &! in
     & coef_rttov,           &! in
     & addcloud,             &! in
     & calcemiss,            &! in
     & emissivity,           &! inout
     & profiles_tl,          &! in
     & emissivity_tl,        &! inout
     & transmission,         &! inout
     & transmission_tl,      &! inout
     & radiance,             &! inout
     & radiance_tl     )      ! inout
     
  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_tl")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If
 
  scatt_aux_tl % ems_cld (:) = emissivity_in_tl (:)
  scatt_aux    % ems_cld (:) = emissivity_in    (:)      
  scatt_aux_tl % ref_cld (:) = -1.0_JPRB * emissivity_in_tl (:)
  scatt_aux    % ref_cld (:) =  1.0_JPRB - emissivity_in    (:)  
      
!*  2.   Initialisations for Eddington
  Call rttov_iniscatt_tl(    &
        & errorstatus,       &! in
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
        & profiles_tl,       &! in 
        & cld_profiles,      &! in
        & cld_profiles_tl,   &! in
        & coef_rttov,        &! in  
        & coef_scatt,        &! in  
        & transmission,      &! in
        & transmission_tl,   &! in
	& calcemiss,         &! in
        & angles,            &! out
        & scatt_aux,         &! inout
        & scatt_aux_tl)       ! inout   

  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Write( errMessage, '( "error in rttov_iniscatt_tl")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

!* 3.   Eddington (in temperature space)
  Call rttov_eddington_tl(   &
        & nwp_levels,        &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & lsprofiles,        &! in
        & angles,            &! in
        & profiles,          &! in  
        & profiles_tl,       &! in  
        & cld_profiles,      &! in  
        & scatt_aux,         &! in
        & scatt_aux_tl,      &! in
        & cld_radiance,      &! inout  
        & cld_radiance_tl)    ! inout   	

!*  4.   Combine clear and cloudy parts
  Do ichan = 1, nchannels
     iprof = lsprofiles (ichan)
       
     cld_radiance_tl % total (ichan) = radiance_tl % total (ichan)
     cld_radiance % total (ichan) = radiance % total (ichan)
     cld_radiance_tl % clear (ichan) = radiance_tl % clear (ichan)
     cld_radiance % clear (ichan) = radiance % clear (ichan)
     cld_radiance_tl % bt_clear (ichan) = radiance_tl % bt (ichan)
     cld_radiance % bt_clear (ichan) = radiance % bt (ichan)
     cld_radiance_tl % bt (ichan) = cld_radiance_tl % bt (ichan) *             scatt_aux    % ccmax (iprof)  & 
                                & + cld_radiance    % bt (ichan) *             scatt_aux_tl % ccmax (iprof)  & 
				& +     radiance_tl % bt (ichan) * (1.0_JPRB - scatt_aux    % ccmax (iprof)) &
				& -     radiance    % bt (ichan) *             scatt_aux_tl % ccmax (iprof)

     cld_radiance    % bt (ichan) = cld_radiance    % bt (ichan) *             scatt_aux    % ccmax (iprof) & 
                                & +     radiance    % bt (ichan) * (1.0_JPRB - scatt_aux    % ccmax (iprof))     
  End Do



  !
  !*  5.   Convert total polarisations length arrays to number of output channel length arrays
  !
  If (coef_rttov % id_sensor == sensor_id_mw) Then

     ! Point a temporary radiance type at cld_radiance
     cld_radiance_tmp % clear     => cld_radiance % clear
     cld_radiance_tmp % clear_out => cld_radiance % clear_out
     cld_radiance_tmp % cloudy    => cld_radiance % cloudy
     cld_radiance_tmp % total     => cld_radiance % total
     cld_radiance_tmp % total_out => cld_radiance % total_out
     cld_radiance_tmp % out       => cld_radiance % out
     cld_radiance_tmp % out_clear => cld_radiance % out_clear
     cld_radiance_tmp % bt        => cld_radiance % bt
     cld_radiance_tmp % bt_clear  => cld_radiance % bt_clear
     cld_radiance_tmp % upclear   => cld_radiance % upclear
     cld_radiance_tmp % dnclear   => cld_radiance % dnclear
     cld_radiance_tmp % reflclear => cld_radiance % reflclear
     cld_radiance_tmp % overcast  => cld_radiance % overcast
     cld_radiance_tmp % downcld   => cld_radiance % downcld

     Call rttov_calcpolarisation( &
        & nfrequencies,       &! in
        & nchannels,          &! in
        & nprofiles,          &! in
        & angles,             &! in
        & channels,           &! in
        & polarisations,      &! in
        & lprofiles,          &! in
        & coef_rttov,         &! in
        & cld_radiance_tmp    )! inout

     ! Point a temporary radiance type at cld_radiance_tl
     cld_radiance_tmp % clear     => cld_radiance_tl % clear
     cld_radiance_tmp % clear_out => cld_radiance_tl % clear_out
     cld_radiance_tmp % cloudy    => cld_radiance_tl % cloudy
     cld_radiance_tmp % total     => cld_radiance_tl % total
     cld_radiance_tmp % total_out => cld_radiance_tl % total_out
     cld_radiance_tmp % out       => cld_radiance_tl % out
     cld_radiance_tmp % out_clear => cld_radiance_tl % out_clear
     cld_radiance_tmp % bt        => cld_radiance_tl % bt
     cld_radiance_tmp % bt_clear  => cld_radiance_tl % bt_clear
     cld_radiance_tmp % upclear   => cld_radiance_tl % upclear
     cld_radiance_tmp % dnclear   => cld_radiance_tl % dnclear
     cld_radiance_tmp % reflclear => cld_radiance_tl % reflclear
     cld_radiance_tmp % overcast  => cld_radiance_tl % overcast
     cld_radiance_tmp % downcld   => cld_radiance_tl % downcld

     Call rttov_calcpolarisation_tl( &
        & nfrequencies,   & ! in
        & nchannels,      & ! in
        & nprofiles,      & ! in
        & angles,         & ! in
        & channels,       & ! in
        & polarisations,  & ! in
        & lprofiles,      & ! in
        & coef_rttov,     & ! in
        & cld_radiance_tmp     ) ! inout
  Else
        radiance%out       = radiance%bt
        radiance%out_clear = radiance%bt_clear
        cld_radiance%out       = cld_radiance%bt
        cld_radiance%out_clear = cld_radiance%bt_clear
        radiance_tl%out        = radiance_tl%bt
        radiance_tl%out_clear  = radiance_tl%bt_clear
        cld_radiance_tl%out       = cld_radiance_tl%bt
        cld_radiance_tl%out_clear = cld_radiance_tl%bt_clear
  End If
End Subroutine rttov_scatt_tl
