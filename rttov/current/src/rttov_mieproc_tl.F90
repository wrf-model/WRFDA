!
Subroutine rttov_mieproc_tl (&
     & nwp_levels,        &! in
     & nchannels,         &! in
     & nprofiles,         &! in
     & frequencies,       &! in
     & lprofiles,         &! in
     & cld_profiles,      &! in
     & cld_profiles_tl,   &! in
     & coef_rttov,        &! in
     & coef_scatt,        &! in
     & scatt_aux,         &! inout
     & scatt_aux_tl)       ! inout 
  !
  ! Description:
  ! Calculates scattering parameters from Mie tables
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
  !  1.2       11/2004   Clean-up            (P. Bauer)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !   Documenting Exchangeable Fortran 90 Code".
  !
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:
  
  Use rttov_types, Only :      &
       & rttov_coef           ,&
       & profile_scatt_aux    ,&
       & profile_cloud_Type   ,&
       & rttov_scatt_coef     

  Use parkind1, Only : jpim     ,jprb

  Implicit None

!* Subroutine arguments:
  Integer (Kind=jpim), Intent (in) :: nwp_levels                             ! Number of NWP levels
  Integer (Kind=jpim), Intent (in) :: nchannels                              ! Number of channels*profiles=radiances
  Integer (Kind=jpim), Intent (in) :: nprofiles                              ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: frequencies (nchannels)                ! Frequency indices
  Integer (Kind=jpim), Intent (in) :: lprofiles   (nchannels)                ! Profile indices

  Type (profile_cloud_Type),  Intent (in)    :: cld_profiles    (nprofiles)  ! Cloud profiles on NWP levels
  Type (profile_cloud_Type),  Intent (in)    :: cld_profiles_tl (nprofiles)  ! Cloud profiles on NWP levels
  Type (rttov_coef),          Intent (in)    :: coef_rttov                   ! RTTOV Coefficients
  Type (rttov_scatt_coef),    Intent (in)    :: coef_scatt                   ! RTTOV_SCATT Coefficients
  Type (profile_scatt_aux),   Intent (inout) :: scatt_aux                    ! Auxiliary profile variables
  Type (profile_scatt_aux),   Intent (inout) :: scatt_aux_tl                 ! Auxiliary profile variables

!* Local variables:
  Integer (Kind=jpim) :: iwc, itemp, itype, ichan, ifreq, iprof, ilayer
  Real    (Kind=jprb) :: wc   , temp   ,  kp   , ap   , gp   , s_k   , s_a   , s_g   , zln10     
  Real    (Kind=jprb) :: wc_tl, temp_tl,  kp_tl, ap_tl, gp_tl, s_k_tl, s_a_tl, s_g_tl  
  
  !- End of header --------------------------------------------------------
  
  zln10 = log (10.0_JPRB)

!* Loops over channels, levels, hydrometeor types
  nchan_loop: do ichan = 1, nchannels
     iprof = lprofiles   (ichan)     
     ifreq = frequencies (ichan)  
    
     nlayer_loop: do ilayer = 1, nwp_levels
        ntype_loop: do itype = 1, coef_scatt % nhydro - 1

           wc_tl = 0.0_JPRB
           wc    = 0.0_JPRB
	   
           select case (itype)
           case (1_jpim)
              if (scatt_aux % rain (iprof,ilayer) > 0.0_JPRB) then
                 wc    = coef_scatt % scale_water * log10 (scatt_aux % rain (iprof,ilayer)) - coef_scatt % offset_water
                 wc_tl = coef_scatt % scale_water * scatt_aux_tl % rain (iprof,ilayer) & 
		    & / (zln10 * scatt_aux % rain (iprof,ilayer))  
              endif
              temp = cld_profiles(iprof) % t(ilayer) - coef_scatt % offset_temp_rain
           case (2_jpim)
              if (scatt_aux % sp   (iprof,ilayer) > 0.0_JPRB) then
                 wc    = coef_scatt % scale_water * log10 (scatt_aux % sp   (iprof,ilayer))  - coef_scatt % offset_water
                 wc_tl = coef_scatt % scale_water * scatt_aux_tl % sp   (iprof,ilayer) & 
		    & / (zln10 * scatt_aux % sp   (iprof,ilayer))  
              endif
              temp = cld_profiles(iprof) % t(ilayer) - coef_scatt % offset_temp_sp
           case (3_jpim)
              if (scatt_aux % clw  (iprof,ilayer) > 0.0_JPRB) then
                 wc    = coef_scatt % scale_water * log10 (scatt_aux % clw   (iprof,ilayer)) - coef_scatt % offset_water
                 wc_tl = coef_scatt % scale_water * scatt_aux_tl % clw  (iprof,ilayer) & 
		     & / (zln10 * scatt_aux % clw  (iprof,ilayer))  
              endif
              temp = cld_profiles(iprof) % t(ilayer) - coef_scatt % offset_temp_liq
           case (4_jpim)
              if (scatt_aux % ciw  (iprof,ilayer) > 0.0_JPRB) then
                 wc    = coef_scatt % scale_water * log10 (scatt_aux % ciw   (iprof,ilayer)) - coef_scatt % offset_water
                 wc_tl = coef_scatt % scale_water * scatt_aux_tl % ciw  (iprof,ilayer) & 
		       & / (zln10 * scatt_aux % ciw  (iprof,ilayer))  
              endif
              temp = cld_profiles(iprof) % t(ilayer) - coef_scatt % offset_temp_ice
           end select
	   
!* nearest index for Mie-table: LWC/IWC
           iwc = floor (wc)
           if (iwc > coef_scatt % mwc - 1) iwc = coef_scatt % mwc - 1
                    
!* nearest index for Mie-table: T (w/o melting layer)
           itemp = anint (temp)
           if (itemp <                      1) itemp = 1
           if (itemp > coef_scatt % mtemp - 1) itemp = coef_scatt % mtemp - 1

           if (iwc >= 1) then
	      s_k   = coef_scatt % ext (ifreq,itype,itemp,iwc+1) - coef_scatt % ext (ifreq,itype,itemp,iwc)
	      s_a   = coef_scatt % ssa (ifreq,itype,itemp,iwc+1) - coef_scatt % ssa (ifreq,itype,itemp,iwc)
	      s_g   = coef_scatt % asp (ifreq,itype,itemp,iwc+1) - coef_scatt % asp (ifreq,itype,itemp,iwc)
	          	   
              kp    = coef_scatt % ext (ifreq,itype,itemp,iwc) + s_k * (wc - iwc)
              kp_tl = s_k * wc_tl
	      
              ap    = coef_scatt % ssa (ifreq,itype,itemp,iwc) + s_a * (wc - iwc)
              ap_tl = s_a * wc_tl
	      
              gp    = coef_scatt % asp (ifreq,itype,itemp,iwc) + s_g * (wc - iwc)
              gp_tl = s_g * wc_tl	      	      	      
           else
              kp    = 1.0E-10_JPRB                             
              ap    = 0.0_JPRB               
              gp    = 0.0_JPRB               
              kp_tl = 0.0_JPRB               
              ap_tl = 0.0_JPRB               
              gp_tl = 0.0_JPRB               
           endif
	   
           scatt_aux    % ext (ichan,ilayer) = scatt_aux    % ext (ichan,ilayer) + kp
           scatt_aux_tl % ext (ichan,ilayer) = scatt_aux_tl % ext (ichan,ilayer) + kp_tl
	   		
           scatt_aux    % ssa (ichan,ilayer) = scatt_aux    % ssa (ichan,ilayer) + kp    * ap		
           scatt_aux_tl % ssa (ichan,ilayer) = scatt_aux_tl % ssa (ichan,ilayer) + kp_tl * ap + kp * ap_tl
		
           scatt_aux    % asm (ichan,ilayer) = scatt_aux    % asm (ichan,ilayer) + kp    * ap * gp		
           scatt_aux_tl % asm (ichan,ilayer) = scatt_aux_tl % asm (ichan,ilayer) + kp_tl * ap * gp & 
	                                                                       & + kp * ap_tl * gp + kp * ap * gp_tl 
         enddo ntype_loop
     enddo nlayer_loop
  enddo nchan_loop

  do ilayer = 1, nwp_levels
     where (scatt_aux    % asm (:,ilayer) > 0.0_JPRB) 
            scatt_aux_tl % asm (:,ilayer) = scatt_aux_tl % asm (:,ilayer) / scatt_aux % ssa (:,ilayer) &
                                        & - scatt_aux_tl % ssa (:,ilayer) * scatt_aux % asm (:,ilayer) &
					& / scatt_aux    % ssa (:,ilayer) / scatt_aux % ssa (:,ilayer) 
            scatt_aux    % asm (:,ilayer) = scatt_aux    % asm (:,ilayer) / scatt_aux % ssa (:,ilayer)
     endwhere
     where (scatt_aux    % ssa (:,ilayer) > 0.0_JPRB) 
            scatt_aux_tl % ssa (:,ilayer) = (scatt_aux_tl % ssa (:,ilayer) * scatt_aux    % ext (:,ilayer) &
	                                & -  scatt_aux    % ssa (:,ilayer) * scatt_aux_tl % ext (:,ilayer)) &
                                        & / (scatt_aux    % ext (:,ilayer) * scatt_aux    % ext (:,ilayer)) 
            scatt_aux    % ssa (:,ilayer) =  scatt_aux    % ssa (:,ilayer) / scatt_aux    % ext (:,ilayer)
     endwhere
     where (scatt_aux % ext (:,ilayer) >= 20.0_JPRB) 
         scatt_aux_tl % ext (:,ilayer) =  0.0_JPRB
	 scatt_aux    % ext (:,ilayer) = 20.0_JPRB
     endwhere
  enddo

End subroutine rttov_mieproc_tl
