!
Subroutine rttov_iniscatt_k (&
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
      & profiles_k,        &! inout
      & cld_profiles,      &! in 
      & cld_profiles_k,    &! inout 
      & coef_rttov,        &! in
      & coef_scatt,        &! in
      & transmission,      &! in
      & transmission_k,    &! inout
      & calcemiss,         &! in
      & angles,            &! out
      & scatt_aux,         &! inout
      & scatt_aux_k)        ! inout 

  !
  ! Description:
  ! AD of routine to
  ! Calculate some variables related to the input precipitation profile
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
  !   1.5    02/2005      K-code              (A. Collard)
  !   1.6    10/2005      Fixes for rttov8 indexing   (U. O'Keeffe)
  !   1.7    11/2005      Limit lines to 132 characters,
  !                       add errorstatus to arguments,
  !                       change stop to return. (J. Cameron) 
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !   Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:
    
  Use rttov_types, Only :    &
       & rttov_coef           ,&
       & rttov_scatt_coef     ,&
       & transmission_type    ,&
       & geometry_Type        ,&
       & profile_scatt_aux    ,&
       & profile_Type         ,&
       & profile_cloud_Type   

  Use rttov_const, Only:    &
       & errorstatus_success, &
       & errorstatus_fatal,   &
       & gravity,             &
       & pressure_top,        &
       & rgp,                 &
       & rm,                  &
       & rho_rain,            &
       & rho_snow,            &
       & ccthres 

  Use parkind1, Only : jpim     ,jprb
  
  Implicit None

#include "rttov_mieproc.interface"
#include "rttov_iniedd.interface"
#include "rttov_calcemis_mw.interface"
#include "rttov_mieproc_k.interface"
#include "rttov_iniedd_k.interface"
#include "rttov_calcemis_mw_k.interface"
#include "rttov_setgeometry.interface"
#include "rttov_errorreport.interface"
#include "rttov_intex.interface"
#include "rttov_intex_ad.interface"

!* Subroutine arguments:

  Integer (Kind=jpim), Intent (in) :: nwp_levels                              ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nrt_levels                              ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nprofiles                               ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: nfrequencies                            ! Number of frequencies  
  Integer (Kind=jpim), Intent (in) :: nchannels                               ! Number of channels*profiles=radiances
  Integer (Kind=jpim), Intent(out) :: errorstatus(nprofiles)                  ! Error return code
  Integer (Kind=jpim), Intent (in) :: channels      (nfrequencies)            ! Channel indices
  Integer (Kind=jpim), Intent (in) :: frequencies   (nchannels)               ! Frequency indices
  Integer (Kind=jpim), Intent (in) :: polarisations (nchannels,3)             ! Polarisation indices
  Integer (Kind=jpim), Intent (in) :: lprofiles     (nfrequencies)               ! Profile indices
  Integer (Kind=jpim), Intent (in) :: lsprofiles     (nchannels)               ! Profile indices
  Logical            , Intent (in) :: calcemiss     (nchannels)               ! Emissivity flags

  Type (profile_Type),        Intent (in)    :: profiles       (nprofiles)    ! Atmospheric profiles
  Type (profile_Type),        Intent (inout) :: profiles_k     (nchannels)    ! Atmospheric profiles
  Type (rttov_coef),          Intent (in)    :: coef_rttov                    ! RTTOV Coefficients
  Type (rttov_scatt_coef),    Intent (in)    :: coef_scatt                    ! RTTOV_SCATT Coefficients
  Type (profile_cloud_Type),  Intent (in)    :: cld_profiles    (nprofiles)   ! Cloud profiles with NWP levels
  Type (profile_cloud_Type),  Intent (inout) :: cld_profiles_k (nchannels)    ! Cloud profiles on NWP levels
  Type (transmission_Type),   Intent (in)    :: transmission                  ! Transmittances and optical depths
  Type (transmission_Type),   Intent (inout) :: transmission_k                ! Transmittances and optical depths
  Type (geometry_Type),       Intent (out)   :: angles          (nprofiles)   ! Zenith angles
  Type (profile_scatt_aux),   Intent (inout) :: scatt_aux                     ! Auxiliary profile variables
  Type (profile_scatt_aux),   Intent (inout) :: scatt_aux_k                   ! Auxiliary profile variables
 
!* Local variables
  Integer(Kind=jpim) :: freq
  Integer (Kind=jpim) :: ilayer, iprof, ichan, iccmax (nprofiles)
  Real    (Kind=jprb) :: p1, p2, pm, p1_k, p2_k, pm_k, dp2dz, de2mr, zccmax

  Real    (Kind=jprb), Dimension (nprofiles,nwp_levels)   :: presf            ! Pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nprofiles,nwp_levels+1) :: presfh           ! Half-level NWP pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nprofiles,nrt_levels)   :: presi            ! Half-level RTTOV pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nchannels,nwp_levels)   :: opd_nwp
  Real    (Kind=jprb), Dimension (nchannels,nrt_levels)   :: opd_nrt
  Real    (Kind=jprb), Dimension (nchannels)              :: zod_up_cld       ! Optical depth from top of the atmosphere 
  Real    (Kind=jprb), Dimension (nchannels)              :: emissivity       ! Surface emissivity
  Real    (Kind=jprb), Dimension (nchannels)              :: reflectivity     ! Surface reflectivity
  Real    (Kind=jprb), Dimension (nchannels,nwp_levels)   :: presf_k          ! Pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nchannels,nwp_levels+1) :: presfh_k         ! Half-level NWP pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nchannels,nrt_levels)   :: presi_k          ! Half-level RTTOV pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nchannels,nwp_levels)   :: opd_nwp_k
  Real    (Kind=jprb), Dimension (nchannels,nrt_levels)   :: opd_nrt_k
  Real    (Kind=jprb), Dimension (nchannels)              :: zod_up_cld_k    ! Optical depth from top of the atmosphere 
  Real    (Kind=jprb), Dimension (nchannels)              :: emissivity_k    ! Surface emissivity
  Real    (Kind=jprb), Dimension (nchannels)              :: reflectivity_k  ! Surface reflectivity
   
  Real    (Kind=jprb), Dimension (nchannels,nwp_levels)   :: ext_0
  Real    (Kind=jprb), Dimension (nchannels,nwp_levels)   :: ext_1, ssa_1, asm_1
  Real    (Kind=jprb), Dimension (nchannels,nwp_levels)   :: ext_2, ssa_2, asm_2
  Real    (Kind=jprb), Dimension (nchannels,nwp_levels)   :: ext_3, ssa_3, asm_3
  Real    (Kind=jprb), Dimension (nprofiles,nwp_levels)   :: clw_scale, ciw_scale, rain_scale, sp_scale

  Type (transmission_Type) :: transmissioncld                                 ! Clear+cloud transmittances with cloud
  Type (transmission_Type) :: transmissioncld_k                              ! Clear+cloud transmittances with cloud

  Character (len=80) :: errMessage
  Character (len=18) :: NameOfRoutine = 'rttov_iniscatt_k '

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  !- End of header --------------------------------------------------------

  errorstatus(:) = errorstatus_success

  allocate (transmissioncld    % tau_surf (nchannels))
  allocate (transmissioncld_k % tau_surf (nchannels))

  de2mr =  1.0E+05_JPRB * rm / rgp
  dp2dz = -1.0E-03_JPRB * rgp / gravity / rm 

  scatt_aux % ext (:,:) = 0.0_JPRB
  scatt_aux % ssa (:,:) = 0.0_JPRB
  scatt_aux % asm (:,:) = 0.0_JPRB

!* Security on user-defined pressures
  Do iprof = 1, nprofiles
     Do ilayer = 1, nwp_levels
        If (cld_profiles (iprof) % p (ilayer) >= pressure_top) Then
            presf (iprof,ilayer) = cld_profiles (iprof) % p (ilayer)
       else
            presf (iprof,ilayer) = pressure_top
       Endif
     Enddo
     Do ilayer = 1, nwp_levels + 1
        If (cld_profiles (iprof) % ph (ilayer) >= pressure_top ) Then
            presfh (iprof,ilayer) = cld_profiles (iprof) % ph (ilayer)
        else
            presfh (iprof,ilayer) = pressure_top
        Endif
     Enddo
  Enddo

!* Set up geometric variables
  Do iprof = 1, nprofiles
     Call rttov_setgeometry (profiles (iprof), coef_rttov, angles (iprof)) 
  End Do

!* Compute temperature at layer boundaries (K)
  Do iprof = 1, nprofiles
     scatt_aux % tbd (iprof,nwp_levels+1) = profiles     (iprof) % s2m % t
     scatt_aux % tbd (iprof,1)            = cld_profiles (iprof) % t(1)
  Enddo

  Do ilayer = 1, nwp_levels-1
     Do iprof = 1, nprofiles     
        p1 = presf  (iprof,ilayer+1)
        p2 = presf  (iprof,ilayer  )
        pm = presfh (iprof,ilayer+1)
	
        scatt_aux % tbd (iprof,ilayer+1) =  cld_profiles (iprof) % t (ilayer+1) &
                                       & + (cld_profiles (iprof) % t (ilayer)   & 
				       & -  cld_profiles (iprof) % t (ilayer+1)) &
                                       & / log(p2/p1) * log(pm/p1) 				    				       				       			       
     Enddo
  Enddo

!* Horizontal clear-sky fraction
  scatt_aux % clw   (:,:) = 0.0_JPRB
  scatt_aux % ciw   (:,:) = 0.0_JPRB
  scatt_aux % rain  (:,:) = 0.0_JPRB
  scatt_aux % sp    (:,:) = 0.0_JPRB
  scatt_aux % ccmax (:)   = 0.0_JPRB

  iccmax (:) = 0
     
  Do iprof = 1, nprofiles
     zccmax = 0.0_JPRB
     Do ilayer = 1, nwp_levels
        if (cld_profiles (iprof) % cc (ilayer) > zccmax) then
	    zccmax = cld_profiles (iprof) % cc (ilayer)
	    iccmax (iprof) = ilayer
        end if
     end do
     scatt_aux % ccmax (iprof) = zccmax
 
     If (scatt_aux % ccmax (iprof) > ccthres) Then
         clw_scale  (iprof,:) = cld_profiles (iprof) % clw  (:) / scatt_aux % ccmax (iprof)
         ciw_scale  (iprof,:) = cld_profiles (iprof) % ciw  (:) / scatt_aux % ccmax (iprof)
         rain_scale (iprof,:) = cld_profiles (iprof) % rain (:) / scatt_aux % ccmax (iprof)
         sp_scale   (iprof,:) = cld_profiles (iprof) % sp   (:) / scatt_aux % ccmax (iprof)
     else
         clw_scale  (iprof,:) = 0.0_JPRB
         ciw_scale  (iprof,:) = 0.0_JPRB
         rain_scale (iprof,:) = 0.0_JPRB
         sp_scale   (iprof,:) = 0.0_JPRB
     Endif
  Enddo

!* Nadir heights (km)
  Do ilayer = nwp_levels, 1, -1
     Do iprof = 1, nprofiles
        p1 = presfh (iprof,ilayer+1)
        p2 = presfh (iprof,ilayer  )
	
        If (p1 <= p2) then
           errorstatus (:) = errorstatus_fatal
           Write( errMessage, '( "iniscatt : problem with user-defined pressure layering")' )
           Call Rttov_ErrorReport (errorstatus(iprof), errMessage, NameOfRoutine)
           Return
        End If
	
        scatt_aux % dz (iprof,ilayer) = dp2dz * Log(p2/p1) * cld_profiles (iprof) % t (ilayer)	
     Enddo
  Enddo

!* Interpolate optical depths (at nadir and in hPa-1) to model levels
  Do ichan = 1, nchannels
     iprof = lsprofiles (ichan)

     opd_nrt (ichan,1) = transmission % od_singlelayer (1,ichan) / (profiles (iprof) % p (1) - pressure_top)
     presi   (iprof,1) = (profiles (iprof) % p (1) + pressure_top) / 2.0_JPRB
     
     Do ilayer = 2, nrt_levels
        opd_nrt (ichan,ilayer) = transmission % od_singlelayer (ilayer,ichan) &
                             & / (profiles (iprof) % p (ilayer) - profiles (iprof) % p (ilayer-1)) 
        presi   (iprof,ilayer) = (profiles (iprof) % p (ilayer) + profiles (iprof) % p (ilayer-1)) / 2.0_JPRB
     Enddo
     
     Call rttov_intex (nrt_levels, nwp_levels, presi (iprof,:), presf (iprof,:), opd_nrt (ichan,:), opd_nwp (ichan,:))		
  Enddo

!* Change units
  Do ilayer = 1, nwp_levels

!* Optical depths in km-1 and at nadir
     Do ichan = 1, nchannels
        iprof = lsprofiles (ichan)
     
        scatt_aux % ext (ichan,ilayer) = opd_nwp (ichan,ilayer) * (presfh (iprof,ilayer+1) - presfh (iprof,ilayer)) &
                                     & / scatt_aux % dz (iprof,ilayer) * angles (iprof) % coszen 
				     				     
        ext_0 (ichan,ilayer) = scatt_aux % ext (ichan,ilayer)		     

        if (scatt_aux % ext (ichan,ilayer) < 1.0E-10_JPRB) scatt_aux % ext (ichan,ilayer) = 1.0E-10_JPRB
     Enddo

!* Condensate from g/g to g/m^3
     Do iprof = 1, nprofiles
        scatt_aux % clw (iprof,ilayer) = clw_scale (iprof,ilayer) * presf (iprof,ilayer) * de2mr / cld_profiles (iprof) % t (ilayer)
        scatt_aux % ciw (iprof,ilayer) = ciw_scale (iprof,ilayer) * presf (iprof,ilayer) * de2mr / cld_profiles (iprof) % t (ilayer)
     
!* Rates from kg/m^2/s to g/m^3
        rain_scale (iprof,ilayer) =  rain_scale (iprof,ilayer) / rho_rain
        sp_scale   (iprof,ilayer) =  sp_scale   (iprof,ilayer) / rho_snow

        rain_scale (iprof,ilayer) =  rain_scale (iprof,ilayer) * 3600.0_JPRB 
        sp_scale   (iprof,ilayer) =  sp_scale   (iprof,ilayer) * 3600.0_JPRB

        if (rain_scale (iprof,ilayer) > 0.0_JPRB) scatt_aux % rain (iprof,ilayer) = &
          & (rain_scale (iprof,ilayer) * coef_scatt % conv_rain (1))**(coef_scatt % conv_rain (2)) 
        if (sp_scale   (iprof,ilayer) > 0.0_JPRB) scatt_aux % sp   (iprof,ilayer) = &
          & (sp_scale   (iprof,ilayer) * coef_scatt % conv_sp   (1))**(coef_scatt % conv_sp   (2))
      Enddo
 Enddo

!* Store clear-sky absorption/scattering parameters
  ext_1 (:,:) = scatt_aux % ext (:,:)
  ssa_1 (:,:) = scatt_aux % ssa (:,:)
  asm_1 (:,:) = scatt_aux % asm (:,:)

!* Cloud/rain absorption/scattering parameters
  Call rttov_mieproc (      &
       & nwp_levels,        &! in
       & nchannels,         &! in
       & nprofiles,         &! in
       & frequencies,       &! in
       & lsprofiles,         &! in
       & cld_profiles,      &! in
       & coef_rttov,        &! in
       & coef_scatt,        &! in
       & scatt_aux)          ! inout 
       
!* Store clear+cloud+rain absorption/scattering parameters
  ext_2 (:,:) = scatt_aux % ext (:,:)
  ssa_2 (:,:) = scatt_aux % ssa (:,:)
  asm_2 (:,:) = scatt_aux % asm (:,:)

!* Scattering parameters for Eddington RT
  Call rttov_iniedd(        &
       & nwp_levels,        &! in
       & nchannels ,        &! in
       & nprofiles ,        &! in
       & lsprofiles ,        &! in
       & angles    ,        &! in
       & coef_scatt,        &! in
       & scatt_aux)          ! inout 

!* Store delta-scaled clear+cloud+rain absorption/scattering parameters
  ext_3 (:,:) = scatt_aux % ext (:,:)
  ssa_3 (:,:) = scatt_aux % ssa (:,:)
  asm_3 (:,:) = scatt_aux % asm (:,:)

!* Surface emissivities
  zod_up_cld (:) = 0.0_JPRB
  
  Do ichan = 1, nchannels
     iprof = lsprofiles (ichan)
     
     Do ilayer = 1, nwp_levels     
        zod_up_cld (ichan) = zod_up_cld (ichan) + scatt_aux % ext (ichan,ilayer) * scatt_aux % dz (iprof,ilayer) 
     Enddo
     if (zod_up_cld (ichan) >= 30.0_JPRB) zod_up_cld (ichan) = 30.0_JPRB
     transmissioncld % tau_surf (ichan) = Exp(-1.0_JPRB * zod_up_cld (ichan) / angles (iprof) % coszen)
  Enddo
  
  Call rttov_calcemis_mw(      &
       & profiles,             &! in
       & angles,               &! in
       & coef_rttov,           &! in
       & nfrequencies,         &! in
       & nchannels,            &! in
       & nprofiles,            &! in
       & channels,             &! in
       & polarisations,        &! in
       & lprofiles,            &! in
       & transmissioncld,      &! in
       & calcemiss,            &! in
       & scatt_aux % ems_cld,  &! inout
       & scatt_aux % ref_cld,  &! out
       & errorstatus          ) ! inout 

!* Hemispheric emissivity (= Fastem's effective emissivity)
  scatt_aux % ems_bnd (:) = scatt_aux % ems_cld (:)
  scatt_aux % ref_bnd (:) = scatt_aux % ref_cld (:)

!* ADJOINT PART
!* Hemispheric emissivity (= Fastem's effective emissivity)
  scatt_aux_k % ems_cld (:) = scatt_aux_k % ems_cld (:) + scatt_aux_k % ems_bnd (:)
  scatt_aux_k % ems_bnd (:) = 0.0_JPRB
  
  scatt_aux_k % ref_cld (:) = scatt_aux_k % ref_cld (:) + scatt_aux_k % ref_bnd (:)  
  scatt_aux_k % ref_bnd (:) = 0.0_JPRB
  
  transmissioncld_k % tau_surf (:) = 0.0_JPRB
  
  Call rttov_calcemis_mw_k(           &
          & profiles,                  &! in
          & profiles_k,               &! inout
          & angles,                    &! in
          & coef_rttov,                &! in
          & nfrequencies,              &! in
          & nchannels,                 &! in
          & nprofiles,                 &! in
          & channels,                  &! in
          & polarisations,             &! in
          & lprofiles,                 &! in
          & transmissioncld    ,       &! in
          & transmissioncld_k,        &! in
          & calcemiss,                 &! in
          & scatt_aux_k % ems_cld,    &! inout
          & scatt_aux_k % ref_cld)     ! inout 

  scatt_aux_k % ems_cld (:) = 0.0_JPRB
  scatt_aux_k % ref_cld (:) = 0.0_JPRB
  
  zod_up_cld_k (:) = 0.0_JPRB
  
  Do ichan = 1, nchannels
     iprof = lsprofiles (ichan)
     
     zod_up_cld_k (ichan) = zod_up_cld_k (ichan) - transmissioncld_k % tau_surf (ichan) &
                         & * transmissioncld % tau_surf (ichan) / angles (iprof) % coszen
     transmissioncld_k % tau_surf (ichan) = 0.0_JPRB

     if (zod_up_cld (ichan) == 30.0_JPRB) zod_up_cld_k (ichan) = 0.0_JPRB

     Do ilayer = 1, nwp_levels
        iprof = lsprofiles (ichan)
     
        scatt_aux_k % ext (ichan,ilayer) = scatt_aux_k % ext (ichan,ilayer) + scatt_aux % dz  (iprof,ilayer) * zod_up_cld_k (ichan)
        scatt_aux_k % dz  (ichan,ilayer) = scatt_aux_k % dz  (ichan,ilayer) + scatt_aux % ext (ichan,ilayer) * zod_up_cld_k (ichan)
     Enddo
  Enddo
  zod_up_cld_k (:) = 0.0_JPRB

  scatt_aux % ext (:,:) = ext_2 (:,:) 
  scatt_aux % ssa (:,:) = ssa_2 (:,:) 
  scatt_aux % asm (:,:) = asm_2 (:,:) 

!* Scattering parameters for Eddington RT
  Call rttov_iniedd_k(     &
       & nwp_levels,        &! in
       & nchannels ,        &! in
       & nprofiles ,        &! in
       & lsprofiles ,        &! in
       & angles    ,        &! in
       & coef_scatt,        &! in
       & scatt_aux ,        &! inout
       & scatt_aux_k)       ! inout 
       
!* Cloud/rain absorption/scattering parameters
  scatt_aux % ext (:,:) = ext_1 (:,:) 
  scatt_aux % ssa (:,:) = ssa_1 (:,:) 
  scatt_aux % asm (:,:) = asm_1 (:,:) 

  Call rttov_mieproc_k (&
       & nwp_levels,        &! in
       & nchannels,         &! in
       & nprofiles,         &! in
       & frequencies,       &! in
       & lsprofiles,         &! in
       & cld_profiles,      &! in
       & cld_profiles_k,   &! in
       & coef_rttov,        &! in
       & coef_scatt,        &! in
       & scatt_aux,         &! inout
       & scatt_aux_k)       ! inout 
       

!* Change units
  presfh_k (:,:) = 0.0_JPRB
  presf_k  (:,:) = 0.0_JPRB
  
  Do ilayer = 1,nwp_levels
     Do ichan = 1, nchannels
        freq=polarisations(ichan, 2)
        iprof = lsprofiles(ichan)

!* Rates from kg/m^2/s to g/m^3
        if (sp_scale   (iprof,ilayer) > 0.0_JPRB) then 
            scatt_aux_k % sp   (ichan,ilayer) = scatt_aux_k % sp   (ichan,ilayer) &
                       & * (coef_scatt % conv_sp   (2)) * (sp_scale   (iprof,ilayer)**(coef_scatt % conv_sp   (2) - 1.0_JPRB)) &
                       & * (coef_scatt % conv_sp   (1))**(coef_scatt % conv_sp   (2)) 
	else
	    scatt_aux_k % sp   (ichan,ilayer) = 0.0_JPRB
	endif			

        if (rain_scale (iprof,ilayer) > 0.0_JPRB) then 
            scatt_aux_k % rain (ichan,ilayer) = scatt_aux_k % rain (ichan,ilayer) &
                       & * (coef_scatt % conv_rain (2)) * (rain_scale (iprof,ilayer)**(coef_scatt % conv_rain (2) - 1.0_JPRB)) &
                       & * (coef_scatt % conv_rain (1))**(coef_scatt % conv_rain (2)) 
	else
	   scatt_aux_k % rain (ichan,ilayer) = 0.0_JPRB
	endif			

        scatt_aux_k % sp   (ichan,ilayer) = scatt_aux_k % sp   (ichan,ilayer) * 3600.0_JPRB
        scatt_aux_k % rain (ichan,ilayer) = scatt_aux_k % rain (ichan,ilayer) * 3600.0_JPRB 

        scatt_aux_k % sp   (ichan,ilayer) = scatt_aux_k % sp   (ichan,ilayer) / rho_snow
        scatt_aux_k % rain (ichan,ilayer) = scatt_aux_k % rain (ichan,ilayer) / rho_rain

!* Condensate from g/g to g/m^3
        presf_k (ichan,ilayer) = presf_k (ichan,ilayer) + ciw_scale (iprof,ilayer) * de2mr & 
	                       & / cld_profiles (iprof) % t (ilayer) * scatt_aux_k % ciw (ichan,ilayer) 
        cld_profiles_k (ichan) % t (ilayer) =  cld_profiles_k (ichan) % t (ilayer) & 
	                       & - ciw_scale (iprof,ilayer) * presf (iprof,ilayer) * de2mr &
                               & / (cld_profiles    (iprof) % t (ilayer) * cld_profiles (iprof) % t (ilayer)) & 
			       & * scatt_aux_k % ciw (ichan,ilayer) 
        scatt_aux_k % ciw (ichan,ilayer) = presf(iprof,ilayer) * de2mr & 
	                       & / cld_profiles (iprof) % t (ilayer) * scatt_aux_k % ciw (ichan,ilayer) 

        presf_k (ichan,ilayer) = presf_k (ichan,ilayer) + clw_scale (iprof,ilayer) * de2mr & 
	                       & / cld_profiles (iprof) % t (ilayer) * scatt_aux_k % clw (ichan,ilayer) 	  
        cld_profiles_k (ichan) % t (ilayer) =  cld_profiles_k (ichan) % t (ilayer) & 
	                       & - clw_scale (iprof,ilayer) * presf (iprof,ilayer) * de2mr &
                               & / (cld_profiles    (iprof) % t (ilayer) * cld_profiles (iprof) % t (ilayer)) & 
			       & * scatt_aux_k % clw (ichan,ilayer) 	   
        scatt_aux_k % clw (ichan,ilayer) = presf (iprof,ilayer) * de2mr & 
	                       & / cld_profiles(iprof) % t (ilayer) * scatt_aux_k % clw (ichan,ilayer)
	  
     Enddo

!* Optical depths in km-1 and at nadir
     Do ichan = 1, nchannels
        iprof = lsprofiles (ichan)
     
!        scatt_aux % ext (ichan,ilayer) = opd_nwp (ichan,ilayer) * (presfh (iprof,ilayer+1) - presfh (iprof,ilayer)) &
!                                     & / scatt_aux % dz (iprof,ilayer) * angles (iprof) % coszen 
        If (ext_0 (ichan,ilayer) < 1.0E-10_JPRB) scatt_aux_k % ext (ichan,ilayer) = 0.0_JPRB

        opd_nwp_k (ichan,ilayer  ) = (presfh   (iprof,ilayer+1) - presfh  (iprof,ilayer)) / scatt_aux % dz (iprof,ilayer) &
                                  & * angles (iprof) % coszen * scatt_aux_k % ext (ichan,ilayer) 
        presfh_k  (ichan,ilayer+1) = presfh_k (ichan,ilayer+1) + opd_nwp (ichan,ilayer)  / scatt_aux % dz (iprof,ilayer) &
	                          & * angles (iprof) % coszen * scatt_aux_k % ext (ichan,ilayer) 
        presfh_k  (ichan,ilayer  ) = presfh_k (ichan,ilayer  ) - opd_nwp (ichan,ilayer)  / scatt_aux % dz (iprof,ilayer) &
	                          & * angles (iprof) % coszen * scatt_aux_k % ext (ichan,ilayer) 
        scatt_aux_k % dz (ichan,ilayer) = scatt_aux_k % dz (ichan,ilayer) - opd_nwp (ichan,ilayer) &
	                          & * (presfh (iprof,ilayer+1) - presfh (iprof,ilayer)) &
				  & * angles (iprof) % coszen / (scatt_aux % dz (iprof,ilayer) * scatt_aux % dz (iprof,ilayer)) &
				  & * scatt_aux_k % ext (ichan,ilayer) 
        scatt_aux_k % ext (ichan,ilayer) = 0.0_JPRB
     Enddo
  Enddo
 

!* Interpolate optical depths (at nadir and in hPa-1) to model levels
  opd_nrt_k (:,:) = 0.0_JPRB
  presi_k   (:,:) = 0.0_JPRB
  
  Do ichan = 1, nchannels
     iprof = lsprofiles (ichan)
    
     Call rttov_intex_ad (nrt_levels, nwp_levels, &
                        & presi_k (ichan,:), presf_k (ichan,:), opd_nrt_k (ichan,:), opd_nwp_k (ichan,:), &
                        & presi    (iprof,:), presf    (iprof,:), opd_nrt    (ichan,:), opd_nwp    (ichan,:)) 

     Do ilayer = nrt_levels, 2, -1
        transmission_k % od_singlelayer (ilayer,ichan) = transmission_k % od_singlelayer (ilayer,ichan) &
                          & + opd_nrt_k (ichan,ilayer) / (profiles (iprof) % p (ilayer) - profiles (iprof) % p (ilayer-1)) 
        opd_nrt_k (ichan,ilayer) = 0.0_JPRB			  
     Enddo
     transmission_k % od_singlelayer (1,ichan) = transmission_k % od_singlelayer (1,ichan) &
                   & + opd_nrt_k (ichan,1) / (profiles (iprof) % p (1) - pressure_top) 
     opd_nrt_k (ichan,1) = 0.0_JPRB			  
  Enddo
  
!* Nadir heights (km)
  Do ilayer = 1, nwp_levels
     Do ichan = 1, nchannels
        freq=polarisations(ichan, 2)
        iprof = lsprofiles(ichan)
        p1 = presfh (iprof,ilayer+1)
        p2 = presfh (iprof,ilayer  )

        p2_k =             dp2dz / p2 * cld_profiles (iprof) % t (ilayer) * scatt_aux_k % dz (ichan,ilayer)
        p1_k = -1.0_JPRB * dp2dz / p1 * cld_profiles (iprof) % t (ilayer) * scatt_aux_k % dz (ichan,ilayer)	
        cld_profiles_k (ichan) % t (ilayer) = cld_profiles_k (ichan) % t (ilayer) & 
	                                  & + dp2dz * Log(p2/p1) * scatt_aux_k % dz (ichan,ilayer) 
        scatt_aux_k % dz (ichan,ilayer) = 0.0_JPRB
	
        presfh_k (ichan,ilayer)   = presfh_k (ichan,ilayer)   + p2_k
        presfh_k (ichan,ilayer+1) = presfh_k (ichan,ilayer+1) + p1_k
     Enddo
  Enddo

!* Horizontal clear-sky fraction
  Do ichan = 1, nchannels
     freq=polarisations(ichan, 2)
     iprof = lsprofiles(ichan)
     If (scatt_aux % ccmax (iprof) > ccthres) Then
        cld_profiles_k (ichan) % clw  (:) = cld_profiles_k (ichan) % clw  (:) & 
	                                & + scatt_aux_k % clw  (ichan,:) / scatt_aux % ccmax (iprof)
        cld_profiles_k (ichan) % ciw  (:) = cld_profiles_k (ichan) % ciw  (:) & 
	                                & + scatt_aux_k % ciw  (ichan,:) / scatt_aux % ccmax (iprof)
        cld_profiles_k (ichan) % rain (:) = cld_profiles_k (ichan) % rain (:) & 
	                                & + scatt_aux_k % rain (ichan,:) / scatt_aux % ccmax (iprof)
        cld_profiles_k (ichan) % sp   (:) = cld_profiles_k (ichan) % sp   (:) & 
	                                & + scatt_aux_k % sp   (ichan,:) / scatt_aux % ccmax (iprof)

        Do ilayer = 1, nwp_levels
          scatt_aux_k % ccmax (ichan) = scatt_aux_k % ccmax (ichan) &
                & - (cld_profiles (iprof) % clw  (ilayer) * scatt_aux_k % clw  (ichan,ilayer) &
                & +  cld_profiles (iprof) % ciw  (ilayer) * scatt_aux_k % ciw  (ichan,ilayer) &
                & +  cld_profiles (iprof) % rain (ilayer) * scatt_aux_k % rain (ichan,ilayer) &
                & +  cld_profiles (iprof) % sp   (ilayer) * scatt_aux_k % sp   (ichan,ilayer)) &
                & / (scatt_aux % ccmax (iprof) * scatt_aux % ccmax (iprof))
        Enddo
     else
        scatt_aux_k % clw  (ichan,:) = 0.0_JPRB
        scatt_aux_k % ciw  (ichan,:) = 0.0_JPRB
        scatt_aux_k % rain (ichan,:) = 0.0_JPRB
        scatt_aux_k % sp   (ichan,:) = 0.0_JPRB
     Endif
     
     cld_profiles_k (ichan) % cc (iccmax (iprof)) = cld_profiles_k (ichan) % cc (iccmax (iprof)) + scatt_aux_k % ccmax (ichan)
     scatt_aux_k % ccmax (ichan) = 0.0_JPRB    
  Enddo
  
  scatt_aux_k % clw   (:,:) = 0.0_JPRB
  scatt_aux_k % ciw   (:,:) = 0.0_JPRB
  scatt_aux_k % rain  (:,:) = 0.0_JPRB
  scatt_aux_k % sp    (:,:) = 0.0_JPRB

!* Temperature at layer boundaries (K)
  Do ilayer = nwp_levels - 1, 1, -1
     Do ichan = 1, nchannels
        freq=polarisations(ichan, 2)
        iprof = lsprofiles(ichan)
        p1 = presf  (iprof,ilayer+1)
        p2 = presf  (iprof,ilayer  )
        pm = presfh (iprof,ilayer+1)
	
        cld_profiles_k (ichan) % t (ilayer+1) = cld_profiles_k (ichan) % t (ilayer+1) + scatt_aux_k % tbd (ichan,ilayer+1) &
                                             & - 1.0_JPRB / Log(p2/p1) * Log(pm/p1)     * scatt_aux_k % tbd (ichan,ilayer+1) 
        cld_profiles_k (ichan) % t (ilayer)   = cld_profiles_k (ichan) % t (ilayer) &
                                             & + 1.0_JPRB / Log(p2/p1) * Log(pm/p1)     * scatt_aux_k % tbd (ichan,ilayer+1) 
					     
        p1_k = (cld_profiles (iprof) % t (ilayer) - cld_profiles (iprof) % t (ilayer+1)) &
            & / (Log(p2/p1) * Log(p2/p1)) / p1 * Log(pm/p1) * scatt_aux_k % tbd (ichan,ilayer+1) &
            & - (cld_profiles (iprof) % t (ilayer) - cld_profiles (iprof) % t (ilayer+1)) &
            & /  Log(p2/p1) / p1 * scatt_aux_k % tbd (ichan,ilayer+1) 	    
        p2_k = -1.0_JPRB &
            & * (cld_profiles (iprof) % t (ilayer) - cld_profiles (iprof) % t (ilayer+1)) &
	    & *  Log(pm/p1) / (Log(p2/p1) * Log(p2/p1)) / p2 * scatt_aux_k % tbd (ichan,ilayer+1) 
        pm_k = (cld_profiles (iprof) % t (ilayer) - cld_profiles (iprof) % t (ilayer+1)) &
	    & /  Log(p2/p1) / pm * scatt_aux_k % tbd (ichan,ilayer+1) 
        scatt_aux_k % tbd (ichan,ilayer+1) = 0.0_JPRB
	
        presf_k  (ichan,ilayer+1) = presf_k  (ichan,ilayer+1) + p1_k
        presf_k  (ichan,ilayer  ) = presf_k  (ichan,ilayer  ) + p2_k
        presfh_k (ichan,ilayer+1) = presfh_k (ichan,ilayer+1) + pm_k	
     Enddo
  Enddo

  Do ichan = 1, nchannels
     freq=polarisations(ichan, 2)
     iprof = lsprofiles(ichan)
     profiles_k     (ichan) % s2m % t = profiles_k     (ichan) % s2m % t + scatt_aux_k % tbd (ichan,nwp_levels+1) 
     cld_profiles_k (ichan) % t (1)   = cld_profiles_k (ichan) % t (1)   + scatt_aux_k % tbd (ichan,1) 
  Enddo
  scatt_aux_k % tbd (:,:) = 0.0_JPRB

!* Security on user-defined pressures
  Do ichan = 1, nchannels
     freq=polarisations(ichan, 2)
     iprof = lsprofiles(ichan)
     Do ilayer = 1, nwp_levels
        If (cld_profiles    (iprof) % p (ilayer) >= pressure_top) &
	 &  cld_profiles_k (ichan) % p (ilayer) = cld_profiles_k (ichan) % p  (ilayer) + presf_k  (ichan,ilayer) 
	presf_k  (ichan,ilayer) = 0.0_JPRB
     Enddo
     Do ilayer = 1, nwp_levels + 1
        If (cld_profiles    (iprof) % ph (ilayer) >= pressure_top) &
	 &  cld_profiles_k (ichan) % ph (ilayer) = cld_profiles_k (ichan) % ph (ilayer) + presfh_k (ichan,ilayer) 
	presfh_k (ichan,ilayer) = 0.0_JPRB
     Enddo
  Enddo

  scatt_aux % ext (:,:) = ext_3 (:,:) 
  scatt_aux % ssa (:,:) = ssa_3 (:,:) 
  scatt_aux % asm (:,:) = asm_3 (:,:) 

!* Deallocate
  deallocate (transmissioncld    % tau_surf)
  deallocate (transmissioncld_k % tau_surf)

End Subroutine rttov_iniscatt_k
