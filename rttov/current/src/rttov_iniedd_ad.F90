Subroutine rttov_iniedd_ad (&
     & nwp_levels,    &! in
     & nchannels ,    &! in
     & nprofiles ,    &! in
     & lprofiles ,    &! in
     & angles ,       &! in
     & coef_scatt,    &! in
     & scatt_aux,     &! inout
     & scatt_aux_ad)   ! inout 

  ! Description:
  ! AD of routine
  ! to compute variables specific to Eddington approximation to RT
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
  !     NWP SAF Report No. NWPSAF-EC-TR-005, 27 pp.
  ! - Chevallier, F., and P. Bauer, 2003:
  !     Model rain and clouds over oceans: comparison with SSM/I observations.
  !     Mon. Wea. Rev., 131, 1240-1255.
  ! - Moreau, E., P. Bauer and F. Chevallier, 2002: Microwave radiative transfer modeling in clouds and precipitation.
  !     Part II: Model evaluation.
  !     NWP SAF Report No. NWPSAF-EC-TR-006, 27 pp.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       09/2002   Initial version     (P. Bauer, E. Moreau)
  !  1.1       05/2003   RTTOV7.3 compatible (F. Chevallier)
  !  1.2       03/2004   Added polarimetry   (R. Saunders)
  !  1.3       08/2004   Polarimetry fixes   (U. O'Keefe)
  !  1.4       11/2004   Clean-up            (P. Bauer)
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
       & geometry_Type        ,&
       & profile_cloud_Type   ,&
       & rttov_scatt_coef     ,&
       & profile_scatt_aux 

  Use parkind1, Only : jpim     ,jprb
  
  Implicit None

!* Subroutine arguments:
  Integer (Kind=jpim), Intent (in) :: nwp_levels                              ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nprofiles                               ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: nchannels                               ! Number of radiances
  Integer (Kind=jpim), Intent (in) :: lprofiles (nchannels)                   ! Profile indices

  Type (geometry_Type),     Intent (in)    :: angles (nprofiles)              ! Zenith angles
  Type (rttov_scatt_coef),  Intent (in)    :: coef_scatt                      ! RTTOV_SCATT Coefficients
  Type (profile_scatt_aux), Intent (inout) :: scatt_aux                       ! Auxiliary profile variables
  Type (profile_scatt_aux), Intent (inout) :: scatt_aux_ad                    ! Auxiliary profile variables

!* Local variables
  Real    (Kind=jprb), dimension (nchannels,nwp_levels)   :: ext_in, ssa_in, asm_in
  Real    (Kind=jprb), dimension (nwp_levels)             :: fac
  Integer (Kind=jpim) :: ilayer, iprof, ichan

  !- End of header --------------------------------------------------------

  scatt_aux % delta   = 0.0_JPRB
  scatt_aux % lambda  = 0.0_JPRB
  scatt_aux % h       = 0.0_JPRB
  scatt_aux % tau     = 1.0_JPRB
  
  scatt_aux % mclayer = 0

  ext_in (:,:) = scatt_aux % ext (:,:) 
  ssa_in (:,:) = scatt_aux % ssa (:,:) 
  asm_in (:,:) = scatt_aux % asm (:,:) 

!* Layer interface temperatures, lapse rates
  do ilayer = nwp_levels, 1, -1
     scatt_aux % b0 (:,ilayer) = scatt_aux % tbd (:,ilayer+1)
     scatt_aux % bn (:,ilayer) = scatt_aux % tbd (:,ilayer  )
     scatt_aux % b1 (:,ilayer) = (scatt_aux % bn (:,ilayer) - scatt_aux % b0 (:,ilayer)) / scatt_aux % dz (:,ilayer)

     scatt_aux_ad % bn (:,ilayer) = scatt_aux_ad % bn (:,ilayer)  + scatt_aux_ad % b1 (:,ilayer) /  scatt_aux % dz (:,ilayer)
     scatt_aux_ad % b0 (:,ilayer) = scatt_aux_ad % b0 (:,ilayer)  - scatt_aux_ad % b1 (:,ilayer) /  scatt_aux % dz (:,ilayer)
     scatt_aux_ad % dz (:,ilayer) = scatt_aux_ad % dz (:,ilayer)  - scatt_aux_ad % b1 (:,ilayer) * (scatt_aux % bn (:,ilayer) &
                                & - scatt_aux    % b0 (:,ilayer)) / (scatt_aux   % dz (:,ilayer) *  scatt_aux % dz (:,ilayer))
     scatt_aux_ad % b1 (:,ilayer) = 0.0_JPRB

     scatt_aux_ad % tbd (:,ilayer  ) = scatt_aux_ad % tbd (:,ilayer  ) + scatt_aux_ad % bn (:,ilayer)
     scatt_aux_ad % bn  (:,ilayer)   = 0.0_JPRB

     scatt_aux_ad % tbd (:,ilayer+1) = scatt_aux_ad % tbd (:,ilayer+1) + scatt_aux_ad % b0 (:,ilayer)
     scatt_aux_ad % b0  (:,ilayer)   = 0.0_JPRB
  end do
  
!* Delta-scaling
  do ichan = 1, nchannels
     iprof = lprofiles (ichan)
     
     scatt_aux % ext    (ichan,:) = (1.0_JPRB - scatt_aux % ssa (ichan,:) * scatt_aux % asm (ichan,:) &
                                & * scatt_aux % asm (ichan,:)) * scatt_aux % ext (ichan,:) 
     scatt_aux % ssa    (ichan,:) = (1.0_JPRB - scatt_aux % asm (ichan,:) * scatt_aux % asm (ichan,:)) &
                                & * scatt_aux % ssa (ichan,:) / (1.0_JPRB - scatt_aux % asm (ichan,:)  &
                                & * scatt_aux % asm (ichan,:) * scatt_aux % ssa (ichan,:)) 
     scatt_aux % asm    (ichan,:) = scatt_aux % asm (ichan,:) / (1.0_JPRB + scatt_aux % asm (ichan,:))
     
     scatt_aux % delta  (ichan,:) = (scatt_aux % ext (ichan,:) * scatt_aux % dz (iprof,:)) / angles (iprof) % coszen

     where (scatt_aux % delta (ichan,:) >= 30.0_JPRB) scatt_aux % delta (ichan,:) = 30.0_JPRB
    
     scatt_aux % tau    (ichan,:) = 1.0_JPRB / exp (scatt_aux % delta (ichan,:))

     scatt_aux % lambda (ichan,:) = sqrt (3.0_JPRB * scatt_aux % ext (ichan,:) * scatt_aux % ext (ichan,:) &
                                & * (1.0_JPRB - scatt_aux % ssa (ichan,:)) &
                                & * (1.0_JPRB - scatt_aux % ssa (ichan,:) * scatt_aux % asm (ichan,:))) 
     scatt_aux % h      (ichan,:) = 1.5_JPRB * scatt_aux % ext (ichan,:) * (1.0_JPRB - scatt_aux % ssa (ichan,:) &
                                & * scatt_aux % asm (ichan,:)) 

     where (scatt_aux    % h (ichan,:) < 0.00001_JPRB)
            scatt_aux    % h (ichan,:) = 0.00001_JPRB
            scatt_aux_ad % h (ichan,:) = 0.0_JPRB
     endwhere

!* Cloud top level index
     scatt_aux % mclayer (ichan) = nwp_levels - 2
     do ilayer = 1, nwp_levels - 2
        if (scatt_aux % ssa (ichan,ilayer) > 0.0_JPRB .and. ilayer < scatt_aux % mclayer (ichan)) &
	    scatt_aux % mclayer (ichan) = ilayer
     end do

!* h
     scatt_aux_ad % ext (ichan,:) = scatt_aux_ad % ext (ichan,:) + 1.5_JPRB * scatt_aux_ad % h (ichan,:) &
                                & * (1.0_JPRB - scatt_aux % ssa (ichan,:) * scatt_aux % asm (ichan,:)) 
     scatt_aux_ad % ssa (ichan,:) = scatt_aux_ad % ssa (ichan,:) - 1.5_JPRB * scatt_aux_ad % h (ichan,:) &
                                & * scatt_aux % ext (ichan,:) * scatt_aux % asm (ichan,:) 
     scatt_aux_ad % asm (ichan,:) = scatt_aux_ad % asm (ichan,:) - 1.5_JPRB * scatt_aux_ad % h (ichan,:) &
                                & * scatt_aux % ext (ichan,:) * scatt_aux % ssa (ichan,:) 
     scatt_aux_ad % h   (ichan,:) = 0.0_JPRB

!* lambda
     fac (:) = (1.0_JPRB / ( 2.0_JPRB * sqrt (3.0_JPRB * scatt_aux % ext (ichan,:) * scatt_aux % ext (ichan,:) &
           & * (1.0_JPRB - scatt_aux % ssa (ichan,:)) * (1.0_JPRB - scatt_aux % ssa (ichan,:) * scatt_aux % asm (ichan,:))))) 
     scatt_aux_ad % ext (ichan,:) = scatt_aux_ad % ext (ichan,:) + fac(:) * 6.0_JPRB * scatt_aux_ad % lambda  (ichan,:) &
                                & * scatt_aux % ext (ichan,:) *  (1.0_JPRB - scatt_aux % ssa (ichan,:)) &
                                & * (1.0_JPRB - scatt_aux % ssa (ichan,:) * scatt_aux % asm (ichan,:)) 
     scatt_aux_ad % ssa (ichan,:) = scatt_aux_ad % ssa (ichan,:) - fac (:) * 3.0_JPRB &
                                & * scatt_aux_ad % lambda  (ichan,:)  &
                                & * scatt_aux % ext (ichan,:) *  scatt_aux % ext (ichan,:) &
                                & * (1.0_JPRB + scatt_aux % asm (ichan,:) - 2.0_JPRB * scatt_aux % ssa (ichan,:) &
				& * scatt_aux % asm (ichan,:)) 
     scatt_aux_ad % asm (ichan,:) = scatt_aux_ad % asm (ichan,:) - fac(:) * 3.0_JPRB &
                                & * scatt_aux_ad % lambda  (ichan,:) &
                                & * scatt_aux % ext (ichan,:) *  scatt_aux % ext (ichan,:) &
                                & * (1.0_JPRB - scatt_aux % ssa (ichan,:)) * scatt_aux % ssa (ichan,:) 
     scatt_aux_ad % lambda (ichan,:) = 0.0_JPRB

!* tau
     scatt_aux_ad % delta (ichan,:) = scatt_aux_ad % delta (ichan,:) - scatt_aux_ad % tau (ichan,:) * scatt_aux % tau (ichan,:)
     scatt_aux_ad % tau   (ichan,:) = 0.0_JPRB
  
!* delta
     where (scatt_aux % delta (ichan,:) == 30.0_JPRB) scatt_aux_ad % delta (ichan,:) = 0.0_JPRB

     scatt_aux_ad % ext   (ichan,:) = scatt_aux_ad % ext (ichan,:) + scatt_aux_ad % delta (ichan,:) &
                                  & * scatt_aux % dz  (iprof,:) / angles (iprof) % coszen 
     scatt_aux_ad % dz    (iprof,:) = scatt_aux_ad % dz  (iprof,:) + scatt_aux_ad % delta (ichan,:) &
                                  & * scatt_aux % ext (ichan,:) / angles (iprof) % coszen 
     scatt_aux_ad % delta (ichan,:) = 0.0_JPRB

!* ext,ssa,asm
     scatt_aux_ad % asm (ichan,:) = scatt_aux_ad % asm (ichan,:) / (1.0_JPRB + asm_in (ichan,:)) / (1.0_JPRB + asm_in (ichan,:)) 
     fac  (:) = 1.0_JPRB - asm_in (ichan,:) * asm_in (ichan,:) * ssa_in (ichan,:)
     scatt_aux_ad % asm (ichan,:) = scatt_aux_ad % asm (ichan,:) - scatt_aux_ad % ssa (ichan,:) * (1.0_JPRB - ssa_in (ichan,:)) &
                                & * 2.0_JPRB * asm_in (ichan,:) * ssa_in (ichan,:) / fac (:) / fac (:) 
     scatt_aux_ad % ssa (ichan,:) = scatt_aux_ad % ssa (ichan,:) * (1.0_JPRB - asm_in (ichan,:) & 
                                & * asm_in(ichan,:)) / fac (:) / fac (:) 
     
     scatt_aux_ad % asm (ichan,:) = scatt_aux_ad % asm (ichan,:) - 2.0_JPRB * scatt_aux_ad % ext (ichan,:) * ext_in (ichan,:) &
                                & * asm_in (ichan,:) * ssa_in (ichan,:) 
     scatt_aux_ad % ssa (ichan,:) = scatt_aux_ad % ssa (ichan,:) - scatt_aux_ad % ext (ichan,:) * ext_in(ichan,:) &
                                & * asm_in (ichan,:) * asm_in (ichan,:) 
     scatt_aux_ad % ext (ichan,:) = scatt_aux_ad % ext (ichan,:) * (1.0_JPRB - ssa_in (ichan,:) & 
                                & * asm_in(ichan,:) * asm_in (ichan,:)) 
  end do

End subroutine rttov_iniedd_ad

