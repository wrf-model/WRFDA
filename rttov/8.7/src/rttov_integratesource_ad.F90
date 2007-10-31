!      
Subroutine rttov_integratesource_ad (&
     & nwp_levels,    &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & lprofiles,     &! in
     & angles,        &! in
     & scatt_aux,     &! in
     & scatt_aux_ad,  &! inout
     & dp,            &! in
     & dp_ad,         &! inout
     & dm,            &! in
     & dm_ad,         &! inout
     & j_do,          &! inout
     & j_do_ad,       &! inout
     & j_up,          &! inout
     & j_up_ad)        ! inout 

  ! Description:
  ! integrate source in Eddington
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
  !  1.0       09/2002   Initial version     (E. Moreau)
  !  1.1       05/2003   RTTOV7.3 compatible (F. Chevallier)
  !  1.2       03/2004   Added polarimetry   (R. Saunders)
  !  1.3       08/2004   Polarimetry fixes   (U. O'Keefe)
  !  1.4       11/2004   Clean-up            (P. Bauer)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:

  Use rttov_types, Only :    &
       & profile_scatt_aux    ,&
       & geometry_Type 

  Use parkind1, Only : jpim     ,jprb

  Implicit None

!* Subroutine arguments:
  Integer (Kind=jpim), Intent (in) :: nwp_levels                                 ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nprofiles                                  ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: nchannels                                  ! Number of channels*profiles=radiances
  Integer (Kind=jpim), Intent (in) :: lprofiles (nchannels)                      ! Profile indices

  Type (profile_scatt_aux), Intent (in)    :: scatt_aux                          ! Auxiliary profile variables for RTTOV_SCATT
  Type (profile_scatt_aux), Intent (inout) :: scatt_aux_ad                       ! Auxiliary profile variables for RTTOV_SCATT
  Type (geometry_Type),     Intent (in)    :: angles (nprofiles)                 ! Zenith angles  

  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nwp_levels) :: dp       ! D+ for boundary conditions
  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nwp_levels) :: dm       ! D- for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: j_do     ! Downward source terms 
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: j_up     ! Upward source terms
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: dp_ad    ! D+ for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: dm_ad    ! D- for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: j_do_ad  ! Downward source terms 
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: j_up_ad  ! Upward source terms

!* Local variables
  Real    (Kind=jprb), dimension (nwp_levels) :: ja1, jb1, jc1, jd1, aa, bb, cp, cm, ztmp
  Real    (Kind=jprb), dimension (nwp_levels) :: ja2, jb2, jc2, jd2
  Real    (Kind=jprb), dimension (nwp_levels) :: ja_ad, jb_ad, jc_ad, jd_ad, aa_ad, bb_ad, cp_ad, cm_ad, ztmp_ad
  Integer (Kind=jpim) :: iprof, ichan
  
  !- End of header --------------------------------------------------------

!* Channels * Profiles      
  do ichan = 1, nchannels
     iprof = lprofiles (ichan)

!* Reset      
     aa_ad (:) = 0.0_JPRB
     bb_ad (:) = 0.0_JPRB
     cp_ad (:) = 0.0_JPRB
     cm_ad (:) = 0.0_JPRB
     ja_ad (:) = 0.0_JPRB
     jb_ad (:) = 0.0_JPRB
     jc_ad (:) = 0.0_JPRB
     jd_ad (:) = 0.0_JPRB  

!* Coefficients
     aa (:) = scatt_aux % b0 (iprof,:) - 1.5_JPRB * scatt_aux % asm (ichan,:) * scatt_aux % ssa (ichan,:) &
          & * angles (iprof) % coszen * scatt_aux % b1 (iprof,:) / scatt_aux % h (ichan,:) 
     bb (:) = scatt_aux % b1 (iprof,:)
     cp (:) = dp (ichan,:) * scatt_aux % ssa (ichan,:) * (1.0_JPRB - 1.5_JPRB * scatt_aux % asm (ichan,:) &
          & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 
     cm (:) = dm (ichan,:) * scatt_aux % ssa (ichan,:) * (1.0_JPRB + 1.5_JPRB * scatt_aux % asm (ichan,:) &
          & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 

     ja1 (:) = 0.0_JPRB
     jb1 (:) = 0.0_JPRB
     jc1 (:) = 0.0_JPRB
     jd1 (:) = 0.0_JPRB
     
     ja2 (:) = 0.0_JPRB
     jb2 (:) = 0.0_JPRB
     jc2 (:) = 0.0_JPRB
     jd2 (:) = 0.0_JPRB

!* Downward radiance source terms    
     where (scatt_aux % ssa (ichan,:) > 1.0E-08_JPRB)    ! limit depends on mie tables 
!* FORWARD PART
        ja1  (:) = 1.0_JPRB - scatt_aux % tau (ichan,:)
        jb1  (:) = angles (iprof) % coszen / scatt_aux % ext (ichan,:) * (1.0_JPRB - scatt_aux % tau (ichan,:)) &
               & - scatt_aux % tau (ichan,:) * scatt_aux % dz (iprof,:) 

        ztmp (:) = exp (scatt_aux % dz (iprof,:) * (scatt_aux % lambda (ichan,:) - scatt_aux % ext (ichan,:) & 
	      & / angles (iprof) % coszen))
        jc1  (:) = scatt_aux % ext (ichan,:) &
              & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen - scatt_aux % ext (ichan,:)) * (ztmp(:) - 1.0_JPRB) 

        ztmp (:) = exp (scatt_aux % dz (iprof,:) * (scatt_aux % lambda (ichan,:) & 
	      & + scatt_aux % ext (ichan,:) / angles (iprof) % coszen))
        jd1  (:) = scatt_aux % ext (ichan,:) &
              & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen + scatt_aux % ext (ichan,:)) & 
	      & * (1.0_JPRB - 1.0_JPRB / ztmp(:)) 

        j_do (ichan,:) = ja1 (:) * aa (:) + jb1 (:) * bb (:) + jc1 (:) * cp (:) + jd1 (:) * cm (:)

!* Upward radiance source terms    
        ja2  (:) = 1.0_JPRB - scatt_aux % tau (ichan,:)
        jb2  (:) = scatt_aux % dz (iprof,:) - angles (iprof) % coszen / scatt_aux % ext (ichan,:) &
              & * (1.0_JPRB - scatt_aux % tau (ichan,:)) 

        ztmp (:) = exp (scatt_aux % dz (iprof,:) * scatt_aux % lambda (ichan,:))
        jc2  (:) = scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) + scatt_aux % lambda (ichan,:) &
               & * angles (iprof) % coszen) * (ztmp (:) - scatt_aux % tau (ichan,:)) 
        jd2  (:) = scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) - scatt_aux % lambda (ichan,:) &
               & * angles (iprof) % coszen) * (1.0_JPRB / ztmp (:) - scatt_aux % tau (ichan,:)) 

        j_up (ichan,:) = ja2 (:) * aa (:) + jb2 (:) * bb (:) + jc2 (:) * cp (:) + jd2 (:) * cm (:)    

!* ADJOINT PART
!* Upward radiance source terms    
        ja_ad (:) = ja_ad (:) + j_up_ad (ichan,:) * aa  (:)
        aa_ad (:) = aa_ad (:) + j_up_ad (ichan,:) * ja2 (:)
        jb_ad (:) = jb_ad (:) + j_up_ad (ichan,:) * bb  (:)
        bb_ad (:) = bb_ad (:) + j_up_ad (ichan,:) * jb2 (:)
        jc_ad (:) = jc_ad (:) + j_up_ad (ichan,:) * cp  (:)
        cp_ad (:) = cp_ad (:) + j_up_ad (ichan,:) * jc2 (:)
        jd_ad (:) = jd_ad (:) + j_up_ad (ichan,:) * cm  (:)
        cm_ad (:) = cm_ad (:) + j_up_ad (ichan,:) * jd2 (:)
        
        j_up_ad (ichan,:) = 0.0_JPRB

        scatt_aux_ad % ext (ichan,:) = scatt_aux_ad % ext (ichan,:) &
             & + jd_ad (:) / (scatt_aux % ext (ichan,:) - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & * (1.0_JPRB/ ztmp (:) - scatt_aux % tau (ichan,:)) &
             & * (1.0_JPRB - scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) & 
	     & - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen)) 
        scatt_aux_ad % lambda (ichan,:) = scatt_aux_ad % lambda (ichan,:) &
             & + jd_ad (:) * angles (iprof) % coszen * scatt_aux % ext (ichan,:) &
             & / (scatt_aux % ext (ichan,:) - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & / (scatt_aux % ext (ichan,:) - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & * (1.0_JPRB / ztmp (:) - scatt_aux % tau (ichan,:))  
        ztmp_ad (:) = -1.0_JPRB * jd_ad (:) / ztmp (:) / ztmp (:) * scatt_aux % ext (ichan,:) & 
	     & / (scatt_aux % ext (ichan,:) &
             & - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) 
        scatt_aux_ad % tau    (ichan,:) = scatt_aux_ad % tau    (ichan,:) &
             & - jd_ad (:) * scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) & 
	     & - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) 
        jd_ad (:) = 0.0_JPRB
        
        scatt_aux_ad % ext (ichan,:) = scatt_aux_ad % ext (ichan,:) &
             & + jc_ad (:) / (scatt_aux % ext (ichan,:) + scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & * (ztmp (:) - scatt_aux % tau (ichan,:)) &
             & * (1.0_JPRB - scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) & 
	     & + scatt_aux % lambda (ichan,:) * angles (iprof) % coszen)) 
        scatt_aux_ad % lambda (ichan,:) = scatt_aux_ad % lambda (ichan,:) &
             & - jc_ad (:) * angles (iprof) % coszen * scatt_aux % ext (ichan,:) &
             & / (scatt_aux % ext (ichan,:) + scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & / (scatt_aux % ext (ichan,:) + scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & * (ztmp (:) - scatt_aux % tau (ichan,:))  
        ztmp_ad (:) = ztmp_ad (:) + jc_ad (:) * scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) & 
	& + scatt_aux % lambda (ichan,:) &
             & * angles (iprof) % coszen) 
        scatt_aux_ad % tau (ichan,:) = scatt_aux_ad % tau (ichan,:) &
             & - jc_ad (:) * scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) & 
	     & + scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) 
        jc_ad   (:) = 0.0_JPRB
        
        scatt_aux_ad % dz     (iprof,:) = scatt_aux_ad % dz     (iprof,:) & 
	                                & + ztmp_ad (:) * scatt_aux % lambda (ichan,:) * ztmp (:)
        scatt_aux_ad % lambda (ichan,:) = scatt_aux_ad % lambda (ichan,:) & 
	                                & + ztmp_ad (:) * scatt_aux % dz     (iprof,:) * ztmp (:)
        ztmp_ad (:) = 0.0_JPRB

        scatt_aux_ad % dz  (iprof,:) = scatt_aux_ad % dz  (iprof,:) + jb_ad (:)
        scatt_aux_ad % ext (ichan,:) = scatt_aux_ad % ext (ichan,:) + jb_ad (:) & 
	                           & / scatt_aux % ext (ichan,:) / scatt_aux % ext (ichan,:) &
                                   & * angles (iprof) % coszen * (1.0_JPRB - scatt_aux % tau (ichan,:)) 
        scatt_aux_ad % tau (ichan,:) = scatt_aux_ad % tau (ichan,:) + jb_ad (:) & 
	                           & * angles (iprof) % coszen / scatt_aux % ext (ichan,:)
        jb_ad (:) = 0.0_JPRB

        scatt_aux_ad % tau (ichan,:)  = scatt_aux_ad % tau (ichan,:)  - ja_ad (:)
        ja_ad (:) = 0.0_JPRB

!* Downward radiance source terms    
        ja_ad (:) = ja_ad (:) + j_do_ad (ichan,:) * aa  (:)
        aa_ad (:) = aa_ad (:) + j_do_ad (ichan,:) * ja1 (:)
        jb_ad (:) = jb_ad (:) + j_do_ad (ichan,:) * bb  (:)
        bb_ad (:) = bb_ad (:) + j_do_ad (ichan,:) * jb1 (:)
        jc_ad (:) = jc_ad (:) + j_do_ad (ichan,:) * cp  (:)
        cp_ad (:) = cp_ad (:) + j_do_ad (ichan,:) * jc1 (:)
        jd_ad (:) = jd_ad (:) + j_do_ad (ichan,:) * cm  (:)
        cm_ad (:) = cm_ad (:) + j_do_ad (ichan,:) * jd1 (:)
        j_do_ad (ichan,:) = 0.0_JPRB

        ztmp (:) = exp (scatt_aux % dz (iprof,:) * (scatt_aux % lambda (ichan,:) & 
	         & + scatt_aux % ext (ichan,:) / angles (iprof) % coszen))

        scatt_aux_ad % ext    (ichan,:) = scatt_aux_ad % ext    (ichan,:) &
             & + jd_ad (:) / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen & 
	     & + scatt_aux % ext (ichan,:)) * (1.0_JPRB - 1.0_JPRB / ztmp (:)) &
             & * (1.0_JPRB - scatt_aux % ext (ichan,:) / (scatt_aux % lambda (ichan,:) & 
	     & * angles (iprof) % coszen + scatt_aux % ext (ichan,:))) 
        scatt_aux_ad % lambda (ichan,:) = scatt_aux_ad % lambda (ichan,:) &
             & - jd_ad (:) * angles (iprof) % coszen * scatt_aux % ext (ichan,:) * (1.0_JPRB - 1.0_JPRB / ztmp (:)) &
             & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen + scatt_aux % ext (ichan,:)) &
             & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen + scatt_aux % ext (ichan,:))  
        ztmp_ad (:) = jd_ad (:) * scatt_aux % ext (ichan,:) &
             & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen + scatt_aux % ext (ichan,:)) / ztmp (:) / ztmp (:) 
        jd_ad (:) = 0.0_JPRB
        
        scatt_aux_ad % dz     (iprof,:) = scatt_aux_ad % dz     (iprof,:) &
             & + ztmp_ad (:) * (scatt_aux % lambda (ichan,:) + scatt_aux % ext (ichan,:) / angles (iprof) % coszen) * ztmp (:) 
        scatt_aux_ad % lambda (ichan,:) = scatt_aux_ad % lambda (ichan,:) &
             & + ztmp_ad (:) * scatt_aux % dz (iprof,:) * ztmp (:) 
        scatt_aux_ad % ext    (ichan,:) = scatt_aux_ad % ext    (ichan,:) &
             & + ztmp_ad (:) * scatt_aux % dz (iprof,:) / angles (iprof) % coszen * ztmp (:) 
        ztmp_ad (:) = 0.0_JPRB

        ztmp(:) = exp (scatt_aux % dz (iprof,:) * (scatt_aux % lambda (ichan,:) & 
	     & - scatt_aux % ext (ichan,:) / angles (iprof) % coszen))

        scatt_aux_ad % ext    (ichan,:) = scatt_aux_ad % ext    (ichan,:) &
            & + jc_ad (:) / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen & 
	    & - scatt_aux % ext (ichan,:)) * (ztmp (:) - 1.0_JPRB) &
            & * (1.0_JPRB + scatt_aux % ext (ichan,:) / (scatt_aux % lambda (ichan,:) & 
	    & * angles (iprof) % coszen - scatt_aux % ext (ichan,:)) ) 
        scatt_aux_ad % lambda (ichan,:) = scatt_aux_ad % lambda (ichan,:) &
            & - jc_ad (:) * angles (iprof) % coszen * scatt_aux % ext (ichan,:) * (ztmp(:) - 1.0_JPRB) &
            & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen - scatt_aux % ext (ichan,:)) &
            & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen - scatt_aux % ext (ichan,:))  
        ztmp_ad (:) = jc_ad (:) * scatt_aux % ext (ichan,:) &
            & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen - scatt_aux % ext (ichan,:))  
        jc_ad (:) = 0.0_JPRB
        
        scatt_aux_ad % dz (iprof,:) = scatt_aux_ad % dz (iprof,:) &
            & + ztmp_ad (:) * (scatt_aux % lambda (ichan,:) - scatt_aux % ext (ichan,:) & 
	    & / angles (iprof) % coszen) * ztmp (:) 
        scatt_aux_ad % lambda(ichan,:) = scatt_aux_ad % lambda( ichan,:) &
            & + ztmp_ad (:) *  scatt_aux % dz     (iprof,:) * ztmp (:) 
        scatt_aux_ad % ext(ichan,:) = scatt_aux_ad % ext(ichan,:) &
            & - ztmp_ad (:) *  scatt_aux % dz     (iprof,:) / angles (iprof) % coszen * ztmp (:) 
        ztmp_ad (:) = 0.0_JPRB

        scatt_aux_ad % ext (ichan,:) = scatt_aux_ad % ext (ichan,:) &
            & - jb_ad (:) / scatt_aux % ext (ichan,:) / scatt_aux % ext (ichan,:) & 
	    & * angles (iprof) % coszen * (1.0_JPRB - scatt_aux % tau (ichan,:)) 
        scatt_aux_ad % tau(ichan,:) = scatt_aux_ad % tau(ichan,:)  &
            & - jb_ad (:) * (angles (iprof) % coszen / scatt_aux % ext (ichan,:) + scatt_aux % dz (iprof,:)) 
        scatt_aux_ad % dz (iprof,:) = scatt_aux_ad % dz (iprof,:) - jb_ad (:) * scatt_aux % tau (ichan,:)
        jb_ad (:) = 0.0_JPRB

        scatt_aux_ad % tau (ichan,:)  = scatt_aux_ad % tau (ichan,:) - ja_ad (:)
        ja_ad (:) = 0.0_JPRB       
     endwhere

     dm_ad (ichan,:) = dm_ad (ichan,:) + cm_ad (:) * scatt_aux % ssa (ichan,:) & 
                   & * (1.0_JPRB + 1.5_JPRB * scatt_aux % asm (ichan,:) &
                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 
                   
     scatt_aux_ad % ssa    (ichan,:) = scatt_aux_ad % ssa    (ichan,:) + cm_ad (:) * dm (ichan,:) & 
                                   & * (1.0_JPRB + 1.5_JPRB * scatt_aux % asm (ichan,:) &
                                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 
     scatt_aux_ad % asm    (ichan,:) = scatt_aux_ad % asm    (ichan,:) + cm_ad (:) * dm (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) * 1.5_JPRB &
                                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:) 
     scatt_aux_ad % lambda (ichan,:) = scatt_aux_ad % lambda (ichan,:) + cm_ad (:) * dm (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) &
                                   & * 1.5_JPRB * scatt_aux % asm (ichan,:) * angles (iprof) % coszen & 
				   & / scatt_aux % h      (ichan,:) 
     scatt_aux_ad % h      (ichan,:) = scatt_aux_ad % h      (ichan,:) - cm_ad (:) * dm (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) &
                                   & * 1.5_JPRB * scatt_aux % asm (ichan,:) * angles (iprof) % coszen  & 
				   & * scatt_aux % lambda (ichan,:) &
                                   & / scatt_aux % h (ichan,:) / scatt_aux % h (ichan,:) 
     cm_ad (:) = 0.0_JPRB        

     dp_ad (ichan,:) = dp_ad (ichan,:) + cp_ad (:) * scatt_aux % ssa (ichan,:) & 
                   & * (1.0_JPRB - 1.5_JPRB * scatt_aux % asm(ichan,:) &
                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 
                   
     scatt_aux_ad % ssa    (ichan,:) = scatt_aux_ad % ssa    (ichan,:) + cp_ad (:) * dp (ichan,:) & 
                                   & * (1.0_JPRB - 1.5_JPRB * scatt_aux % asm (ichan,:) &
                                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 
     scatt_aux_ad % asm    (ichan,:) = scatt_aux_ad % asm    (ichan,:) - cp_ad (:) * dp (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) * 1.5_JPRB &
                                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:) 
     scatt_aux_ad % lambda (ichan,:) = scatt_aux_ad % lambda (ichan,:) - cp_ad (:) * dp (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) * 1.5_JPRB * scatt_aux % asm (ichan,:) &
                                   & * angles (iprof) % coszen / scatt_aux % h      (ichan,:) 
     scatt_aux_ad % h      (ichan,:) = scatt_aux_ad % h      (ichan,:) + cp_ad (:) * dp (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) * 1.5_JPRB * scatt_aux % asm (ichan,:) &
                                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) & 
				   & / scatt_aux % h (ichan,:) / scatt_aux % h (ichan,:) 
     cp_ad (:) = 0.0_JPRB

     scatt_aux_ad % b1 (iprof,:) = scatt_aux_ad % b1 (iprof,:) + bb_ad (:)
     bb_ad (:) = 0.0_JPRB

     scatt_aux_ad % b0  (iprof,:) = scatt_aux_ad % b0  (iprof,:) + aa_ad (:)
     scatt_aux_ad % asm (ichan,:) = scatt_aux_ad % asm (ichan,:) - aa_ad (:) * 1.5_JPRB * scatt_aux % ssa (ichan,:) &
                                & * angles (iprof) % coszen * scatt_aux % b1 (iprof,:) / scatt_aux % h (ichan,:) 
     scatt_aux_ad % ssa (ichan,:) = scatt_aux_ad % ssa (ichan,:) - aa_ad (:) * 1.5_JPRB * scatt_aux % asm (ichan,:) &
                                & * angles (iprof) % coszen * scatt_aux % b1 (iprof,:) / scatt_aux % h (ichan,:) 
     scatt_aux_ad % b1  (iprof,:) = scatt_aux_ad % b1  (iprof,:) - aa_ad (:) * 1.5_JPRB * scatt_aux % asm (ichan,:) & 
                                & * scatt_aux % ssa (ichan,:) &
                                & * angles (iprof) % coszen / scatt_aux % h  (ichan,:) 
     scatt_aux_ad % h   (ichan,:) = scatt_aux_ad % h    (ichan,:) + aa_ad (:) * 1.5_JPRB * scatt_aux % asm (ichan,:) & 
                                & * scatt_aux % ssa (ichan,:) &
                                & * angles (iprof) % coszen * scatt_aux % b1 (iprof,:) / scatt_aux % h( ichan,:) & 
				& / scatt_aux % h (ichan,:) 
     aa_ad(:) = 0._JPRB
  end do

!* Reset      
  ja_ad (:) = 0.0_JPRB
  jb_ad (:) = 0.0_JPRB
  jc_ad (:) = 0.0_JPRB
  jd_ad (:) = 0.0_JPRB

End subroutine rttov_integratesource_ad
