!      
Subroutine rttov_integratesource_k (&
     & nwp_levels,    &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & lprofiles,     &! in
     & angles,        &! in
     & scatt_aux,     &! in
     & scatt_aux_k,   &! inout
     & dp,            &! in
     & dp_k,          &! inout
     & dm,            &! in
     & dm_k,          &! inout
     & j_do,          &! inout
     & j_do_k,        &! inout
     & j_up,          &! inout
     & j_up_k)         ! inout 

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
  !  1.5       02/2005   K-code              (A. Collard)
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
  Type (profile_scatt_aux), Intent (inout) :: scatt_aux_k                        ! Auxiliary profile variables for RTTOV_SCATT
  Type (geometry_Type),     Intent (in)    :: angles (nprofiles)                 ! Zenith angles  

  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nwp_levels) :: dp       ! D+ for boundary conditions
  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nwp_levels) :: dm       ! D- for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: j_do     ! Downward source terms 
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: j_up     ! Upward source terms
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: dp_k    ! D+ for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: dm_k    ! D- for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: j_do_k  ! Downward source terms 
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: j_up_k  ! Upward source terms

!* Local variables
  Real    (Kind=jprb), dimension (nwp_levels) :: ja1, jb1, jc1, jd1, aa, bb, cp, cm, ztmp
  Real    (Kind=jprb), dimension (nwp_levels) :: ja2, jb2, jc2, jd2
  Real    (Kind=jprb), dimension (nwp_levels) :: ja_k, jb_k, jc_k, jd_k, aa_k, bb_k, cp_k, cm_k, ztmp_k
  Integer (Kind=jpim) :: iprof, ichan
  
  !- End of header --------------------------------------------------------

!* Channels * Profiles      
  do ichan = 1, nchannels
     iprof = lprofiles (ichan)

!* Reset      
     aa_k (:) = 0.0_JPRB
     bb_k (:) = 0.0_JPRB
     cp_k (:) = 0.0_JPRB
     cm_k (:) = 0.0_JPRB
     ja_k (:) = 0.0_JPRB
     jb_k (:) = 0.0_JPRB
     jc_k (:) = 0.0_JPRB
     jd_k (:) = 0.0_JPRB  

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

        ztmp (:) = exp (scatt_aux % dz (iprof,:) * (scatt_aux % lambda (ichan,:) & 
	      & - scatt_aux % ext (ichan,:) / angles (iprof) % coszen))
        jc1  (:) = scatt_aux % ext (ichan,:) &
              & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen - scatt_aux % ext (ichan,:)) * (ztmp(:) - 1.0_JPRB) 

        ztmp (:) = exp (scatt_aux % dz (iprof,:) * (scatt_aux % lambda (ichan,:) & 
	     & + scatt_aux % ext (ichan,:) / angles (iprof) % coszen))
        jd1  (:) = scatt_aux % ext (ichan,:) &
              & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen & 
	      & + scatt_aux % ext (ichan,:)) * (1.0_JPRB - 1.0_JPRB / ztmp(:)) 

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
        ja_k (:) = ja_k (:) + j_up_k (ichan,:) * aa  (:)
        aa_k (:) = aa_k (:) + j_up_k (ichan,:) * ja2 (:)
        jb_k (:) = jb_k (:) + j_up_k (ichan,:) * bb  (:)
        bb_k (:) = bb_k (:) + j_up_k (ichan,:) * jb2 (:)
        jc_k (:) = jc_k (:) + j_up_k (ichan,:) * cp  (:)
        cp_k (:) = cp_k (:) + j_up_k (ichan,:) * jc2 (:)
        jd_k (:) = jd_k (:) + j_up_k (ichan,:) * cm  (:)
        cm_k (:) = cm_k (:) + j_up_k (ichan,:) * jd2 (:)
        
        j_up_k (ichan,:) = 0.0_JPRB

        scatt_aux_k % ext (ichan,:) = scatt_aux_k % ext (ichan,:) &
             & + jd_k (:) / (scatt_aux % ext (ichan,:) - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & * (1.0_JPRB/ ztmp (:) - scatt_aux % tau (ichan,:)) &
             & * (1.0_JPRB - scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) & 
	     & - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen)) 
        scatt_aux_k % lambda (ichan,:) = scatt_aux_k % lambda (ichan,:) &
             & + jd_k (:) * angles (iprof) % coszen * scatt_aux % ext (ichan,:) &
             & / (scatt_aux % ext (ichan,:) - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & / (scatt_aux % ext (ichan,:) - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & * (1.0_JPRB / ztmp (:) - scatt_aux % tau (ichan,:))  
        ztmp_k (:) = -1.0_JPRB * jd_k (:) / ztmp (:) / ztmp (:) * scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) &
             & - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) 
        scatt_aux_k % tau    (ichan,:) = scatt_aux_k % tau    (ichan,:) &
             & - jd_k (:) * scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) & 
	     & - scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) 
        jd_k (:) = 0.0_JPRB
        
        scatt_aux_k % ext (ichan,:) = scatt_aux_k % ext (ichan,:) &
             & + jc_k (:) / (scatt_aux % ext (ichan,:) + scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & * (ztmp (:) - scatt_aux % tau (ichan,:)) &
             & * (1.0_JPRB - scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) & 
	     & + scatt_aux % lambda (ichan,:) * angles (iprof) % coszen)) 
        scatt_aux_k % lambda (ichan,:) = scatt_aux_k % lambda (ichan,:) &
             & - jc_k (:) * angles (iprof) % coszen * scatt_aux % ext (ichan,:) &
             & / (scatt_aux % ext (ichan,:) + scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & / (scatt_aux % ext (ichan,:) + scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) &
             & * (ztmp (:) - scatt_aux % tau (ichan,:))  
        ztmp_k (:) = ztmp_k (:) + jc_k (:) * scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) & 
	     & + scatt_aux % lambda (ichan,:) &
             & * angles (iprof) % coszen) 
        scatt_aux_k % tau (ichan,:) = scatt_aux_k % tau (ichan,:) &
             & - jc_k (:) * scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) & 
	     & + scatt_aux % lambda (ichan,:) * angles (iprof) % coszen) 
        jc_k   (:) = 0.0_JPRB
        
        scatt_aux_k % dz     (ichan,:) = scatt_aux_k % dz     (ichan,:) + ztmp_k (:) * scatt_aux % lambda (ichan,:) * ztmp (:)
        scatt_aux_k % lambda (ichan,:) = scatt_aux_k % lambda (ichan,:) + ztmp_k (:) * scatt_aux % dz     (iprof,:) * ztmp (:)
        ztmp_k (:) = 0.0_JPRB

        scatt_aux_k % dz  (ichan,:) = scatt_aux_k % dz  (ichan,:) + jb_k (:)
        scatt_aux_k % ext (ichan,:) = scatt_aux_k % ext (ichan,:) + jb_k (:) / scatt_aux % ext (ichan,:) & 
	                           & / scatt_aux % ext (ichan,:) &
                                   & * angles (iprof) % coszen * (1.0_JPRB - scatt_aux % tau (ichan,:)) 
        scatt_aux_k % tau (ichan,:) = scatt_aux_k % tau (ichan,:) + jb_k (:) * angles (iprof) % coszen & 
	                           & / scatt_aux % ext (ichan,:)
        jb_k (:) = 0.0_JPRB

        scatt_aux_k % tau (ichan,:)  = scatt_aux_k % tau (ichan,:)  - ja_k (:)
        ja_k (:) = 0.0_JPRB

!* Downward radiance source terms    
        ja_k (:) = ja_k (:) + j_do_k (ichan,:) * aa  (:)
        aa_k (:) = aa_k (:) + j_do_k (ichan,:) * ja1 (:)
        jb_k (:) = jb_k (:) + j_do_k (ichan,:) * bb  (:)
        bb_k (:) = bb_k (:) + j_do_k (ichan,:) * jb1 (:)
        jc_k (:) = jc_k (:) + j_do_k (ichan,:) * cp  (:)
        cp_k (:) = cp_k (:) + j_do_k (ichan,:) * jc1 (:)
        jd_k (:) = jd_k (:) + j_do_k (ichan,:) * cm  (:)
        cm_k (:) = cm_k (:) + j_do_k (ichan,:) * jd1 (:)
        j_do_k (ichan,:) = 0.0_JPRB

        ztmp (:) = exp (scatt_aux % dz (iprof,:) * (scatt_aux % lambda (ichan,:) & 
	     & + scatt_aux % ext (ichan,:) / angles (iprof) % coszen))

        scatt_aux_k % ext    (ichan,:) = scatt_aux_k % ext    (ichan,:) &
             & + jd_k (:) / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen & 
	     & + scatt_aux % ext (ichan,:)) * (1.0_JPRB - 1.0_JPRB / ztmp (:)) &
             & * (1.0_JPRB - scatt_aux % ext (ichan,:) / (scatt_aux % lambda (ichan,:) & 
	     & * angles (iprof) % coszen + scatt_aux % ext (ichan,:))) 
        scatt_aux_k % lambda (ichan,:) = scatt_aux_k % lambda (ichan,:) &
             & - jd_k (:) * angles (iprof) % coszen * scatt_aux % ext (ichan,:) * (1.0_JPRB - 1.0_JPRB / ztmp (:)) &
             & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen + scatt_aux % ext (ichan,:)) &
             & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen + scatt_aux % ext (ichan,:))  
        ztmp_k (:) = jd_k (:) * scatt_aux % ext (ichan,:) &
             & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen + scatt_aux % ext (ichan,:)) / ztmp (:) / ztmp (:) 
        jd_k (:) = 0.0_JPRB
        
        scatt_aux_k % dz     (ichan,:) = scatt_aux_k % dz     (ichan,:) &
             & + ztmp_k (:) * (scatt_aux % lambda (ichan,:) + scatt_aux % ext (ichan,:) / angles (iprof) % coszen) * ztmp (:) 
        scatt_aux_k % lambda (ichan,:) = scatt_aux_k % lambda (ichan,:) &
             & + ztmp_k (:) * scatt_aux % dz (iprof,:) * ztmp (:) 
        scatt_aux_k % ext    (ichan,:) = scatt_aux_k % ext    (ichan,:) &
             & + ztmp_k (:) * scatt_aux % dz (iprof,:) / angles (iprof) % coszen * ztmp (:) 
        ztmp_k (:) = 0.0_JPRB

        ztmp(:) = exp (scatt_aux % dz (iprof,:) * (scatt_aux % lambda (ichan,:) & 
	    & - scatt_aux % ext (ichan,:) / angles (iprof) % coszen))

        scatt_aux_k % ext    (ichan,:) = scatt_aux_k % ext    (ichan,:) &
            & + jc_k (:) / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen & 
	    & - scatt_aux % ext (ichan,:)) * (ztmp (:) - 1.0_JPRB) &
            & * (1.0_JPRB + scatt_aux % ext (ichan,:) / (scatt_aux % lambda (ichan,:) & 
	    & * angles (iprof) % coszen - scatt_aux % ext (ichan,:)) ) 
        scatt_aux_k % lambda (ichan,:) = scatt_aux_k % lambda (ichan,:) &
            & - jc_k (:) * angles (iprof) % coszen * scatt_aux % ext (ichan,:) * (ztmp(:) - 1.0_JPRB) &
            & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen - scatt_aux % ext (ichan,:)) &
            & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen - scatt_aux % ext (ichan,:))  
        ztmp_k (:) = jc_k (:) * scatt_aux % ext (ichan,:) &
            & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen - scatt_aux % ext (ichan,:))  
        jc_k (:) = 0.0_JPRB
        
        scatt_aux_k % dz (ichan,:) = scatt_aux_k % dz (ichan,:) &
            & + ztmp_k (:) * (scatt_aux % lambda (ichan,:) - scatt_aux % ext (ichan,:) & 
	    & / angles (iprof) % coszen) * ztmp (:) 
        scatt_aux_k % lambda(ichan,:) = scatt_aux_k % lambda( ichan,:) &
            & + ztmp_k (:) *  scatt_aux % dz     (iprof,:) * ztmp (:) 
        scatt_aux_k % ext(ichan,:) = scatt_aux_k % ext(ichan,:) &
            & - ztmp_k (:) *  scatt_aux % dz     (iprof,:) / angles (iprof) % coszen * ztmp (:) 
        ztmp_k (:) = 0.0_JPRB

        scatt_aux_k % ext (ichan,:) = scatt_aux_k % ext (ichan,:) &
            & - jb_k (:) / scatt_aux % ext (ichan,:) / scatt_aux % ext (ichan,:) & 
	    & * angles (iprof) % coszen * (1.0_JPRB - scatt_aux % tau (ichan,:)) 
        scatt_aux_k % tau(ichan,:) = scatt_aux_k % tau(ichan,:)  &
            & - jb_k (:) * (angles (iprof) % coszen / scatt_aux % ext (ichan,:) + scatt_aux % dz (iprof,:)) 
        scatt_aux_k % dz (ichan,:) = scatt_aux_k % dz (ichan,:) - jb_k (:) * scatt_aux % tau (ichan,:)
        jb_k (:) = 0.0_JPRB

        scatt_aux_k % tau (ichan,:)  = scatt_aux_k % tau (ichan,:) - ja_k (:)
        ja_k (:) = 0.0_JPRB       
     endwhere

     dm_k (ichan,:) = dm_k (ichan,:) + cm_k (:) * scatt_aux % ssa (ichan,:) * (1.0_JPRB + 1.5_JPRB * scatt_aux % asm (ichan,:) &
                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 
                   
     scatt_aux_k % ssa    (ichan,:) = scatt_aux_k % ssa    (ichan,:) + cm_k (:) * dm (ichan,:) & 
                                   & * (1.0_JPRB + 1.5_JPRB * scatt_aux % asm (ichan,:) &
                                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) & 
				   & / scatt_aux % h (ichan,:)) 
     scatt_aux_k % asm    (ichan,:) = scatt_aux_k % asm    (ichan,:) + cm_k (:) * dm (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) * 1.5_JPRB &
                                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) & 
				   &/ scatt_aux % h (ichan,:) 
     scatt_aux_k % lambda (ichan,:) = scatt_aux_k % lambda (ichan,:) + cm_k (:) * dm (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) &
                                   & * 1.5_JPRB * scatt_aux % asm (ichan,:) * angles (iprof) % coszen & 
				   &/ scatt_aux % h      (ichan,:) 
     scatt_aux_k % h      (ichan,:) = scatt_aux_k % h      (ichan,:) - cm_k (:) * dm (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) &
                                   & * 1.5_JPRB * scatt_aux % asm (ichan,:) * angles (iprof) % coszen & 
				   & * scatt_aux % lambda (ichan,:) &
                                   & / scatt_aux % h (ichan,:) / scatt_aux % h (ichan,:) 
     cm_k (:) = 0.0_JPRB        

     dp_k (ichan,:) = dp_k (ichan,:) + cp_k (:) * scatt_aux % ssa (ichan,:) * (1.0_JPRB - 1.5_JPRB * scatt_aux % asm(ichan,:) &
                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 
                   
     scatt_aux_k % ssa    (ichan,:) = scatt_aux_k % ssa    (ichan,:) + cp_k (:) * dp (ichan,:) & 
                                   & * (1.0_JPRB - 1.5_JPRB * scatt_aux % asm (ichan,:) &
                                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) & 
				   & / scatt_aux % h (ichan,:)) 
     scatt_aux_k % asm    (ichan,:) = scatt_aux_k % asm    (ichan,:) - cp_k (:) * dp (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) * 1.5_JPRB &
                                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) & 
				   & / scatt_aux % h (ichan,:) 
     scatt_aux_k % lambda (ichan,:) = scatt_aux_k % lambda (ichan,:) - cp_k (:) * dp (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) * 1.5_JPRB * scatt_aux % asm (ichan,:) &
                                   & * angles (iprof) % coszen / scatt_aux % h      (ichan,:) 
     scatt_aux_k % h      (ichan,:) = scatt_aux_k % h      (ichan,:) + cp_k (:) * dp (ichan,:) & 
                                   & * scatt_aux % ssa (ichan,:) * 1.5_JPRB * scatt_aux % asm (ichan,:) &
                                   & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) & 
				   & / scatt_aux % h (ichan,:) / scatt_aux % h (ichan,:) 
     cp_k (:) = 0.0_JPRB

     scatt_aux_k % b1 (ichan,:) = scatt_aux_k % b1 (ichan,:) + bb_k (:)
     bb_k (:) = 0.0_JPRB

     scatt_aux_k % b0  (ichan,:) = scatt_aux_k % b0  (ichan,:) + aa_k (:)
     scatt_aux_k % asm (ichan,:) = scatt_aux_k % asm (ichan,:) - aa_k (:) * 1.5_JPRB * scatt_aux % ssa (ichan,:) &
                                & * angles (iprof) % coszen * scatt_aux % b1 (iprof,:) / scatt_aux % h (ichan,:) 
     scatt_aux_k % ssa (ichan,:) = scatt_aux_k % ssa (ichan,:) - aa_k (:) * 1.5_JPRB * scatt_aux % asm (ichan,:) &
                                & * angles (iprof) % coszen * scatt_aux % b1 (iprof,:) / scatt_aux % h (ichan,:) 
     scatt_aux_k % b1  (ichan,:) = scatt_aux_k % b1  (ichan,:) - aa_k (:) * 1.5_JPRB * scatt_aux % asm (ichan,:) & 
                                & * scatt_aux % ssa (ichan,:) &
                                & * angles (iprof) % coszen / scatt_aux % h  (ichan,:) 
     scatt_aux_k % h   (ichan,:) = scatt_aux_k % h    (ichan,:) + aa_k (:) * 1.5_JPRB * scatt_aux % asm (ichan,:) & 
                                & * scatt_aux % ssa (ichan,:) &
                                & * angles (iprof) % coszen * scatt_aux % b1 (iprof,:) / scatt_aux % h( ichan,:) & 
				& / scatt_aux % h (ichan,:) 
     aa_k(:) = 0._JPRB
  end do

!* Reset      
  ja_k (:) = 0.0_JPRB
  jb_k (:) = 0.0_JPRB
  jc_k (:) = 0.0_JPRB
  jd_k (:) = 0.0_JPRB

End subroutine rttov_integratesource_k
