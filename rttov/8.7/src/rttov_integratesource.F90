!      
Subroutine rttov_integratesource (&
     & nwp_levels,    &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & lprofiles,     &! in
     & angles,        &! in
     & scatt_aux,     &! in
     & dp,            &! in
     & dm,            &! in
     & j_do,          &! inout
     & j_up)           ! inout 

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
  !  1.0       09/2002   Initial version (P. Bauer, E. Moreau)
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
       & profile_scatt_aux    ,&
       & geometry_Type 

  Use parkind1, Only : jpim     ,jprb

  Implicit None

!* Subroutine arguments:
  Integer (Kind=jpim), Intent (in) :: nwp_levels                              ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nprofiles                               ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: nchannels                               ! Number of channels*profiles=radiances
  Integer (Kind=jpim), Intent (in) :: lprofiles (nchannels)                   ! Profile indices

  Type (profile_scatt_aux), Intent (in) :: scatt_aux                          ! Auxiliary profile variables for RTTOV_SCATT
  Type (geometry_Type),     Intent (in) :: angles (nprofiles)                 ! Zenith angles  

  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nwp_levels) :: dp    ! D+ for boundary conditions
  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nwp_levels) :: dm    ! D- for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: j_do  ! Downward source terms 
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nwp_levels) :: j_up  ! Upward source terms

!* Local variables
  Real    (Kind=jprb), dimension (nwp_levels) :: ja, jb, jc, jd, aa, bb, cp, cm, ztmp
  Integer (Kind=jpim) :: iprof, ichan
 
  !- End of header --------------------------------------------------------

!* Channels * Profiles      
  do ichan = 1, nchannels
     iprof = lprofiles (ichan)

!* Reset      
     ja (:) = 0.0_JPRB
     jb (:) = 0.0_JPRB
     jc (:) = 0.0_JPRB
     jd (:) = 0.0_JPRB

!* Coefficients
     aa (:) = scatt_aux % b0 (iprof,:) - 1.5_JPRB * scatt_aux % asm (ichan,:) * scatt_aux % ssa (ichan,:) &
          & * angles (iprof) % coszen * scatt_aux % b1 (iprof,:) / scatt_aux % h (ichan,:) 
     bb (:) = scatt_aux % b1 (iprof,:)
     cp (:) = dp (ichan,:) * scatt_aux % ssa (ichan,:) * (1.0_JPRB - 1.5_JPRB * scatt_aux % asm (ichan,:) &
          & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 
     cm (:) = dm (ichan,:) * scatt_aux % ssa (ichan,:) * (1.0_JPRB + 1.5_JPRB * scatt_aux % asm (ichan,:) &
          & * angles (iprof) % coszen * scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 

!* Downward radiance source terms    
     where (scatt_aux % ssa (ichan,:) > 1.0E-08_JPRB)    ! limit depends on mie tables 
        ja (:) = 1.0_JPRB - scatt_aux % tau (ichan,:)
        jb (:) = angles (iprof) % coszen / scatt_aux % ext (ichan,:) * (1.0_JPRB - scatt_aux % tau (ichan,:)) &
             & - scatt_aux % tau (ichan,:) * scatt_aux % dz (iprof,:) 

        ztmp (:) = exp (scatt_aux % dz (iprof,:) * (scatt_aux % lambda (ichan,:) &
	         & - scatt_aux % ext (ichan,:) / angles (iprof) % coszen))
        jc (:) = scatt_aux % ext (ichan,:) &
             & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen - scatt_aux % ext (ichan,:)) * (ztmp(:) - 1.0_JPRB) 

        ztmp (:) = exp (scatt_aux % dz (iprof,:) * (scatt_aux % lambda (ichan,:) &
	         & + scatt_aux % ext (ichan,:) / angles (iprof) % coszen))
        jd (:) = scatt_aux % ext (ichan,:) &
             & / (scatt_aux % lambda (ichan,:) * angles (iprof) % coszen &
	     & + scatt_aux % ext (ichan,:)) * (1.0_JPRB - 1.0_JPRB / ztmp(:)) 

        j_do (ichan,:) = ja (:) * aa (:) + jb (:) * bb (:) + jc (:) * cp (:) + jd (:) * cm (:)

!* Upward radiance source terms    
        ja (:) = 1.0_JPRB - scatt_aux % tau (ichan,:)
        jb (:) = scatt_aux % dz (iprof,:) - angles (iprof) % coszen / scatt_aux % ext (ichan,:) &
             & * (1.0_JPRB - scatt_aux % tau (ichan,:)) 

        ztmp (:) = exp (scatt_aux % dz (iprof,:) * scatt_aux % lambda (ichan,:))
        jc (:) = scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) + scatt_aux % lambda (ichan,:) &
             & * angles (iprof) % coszen) * (ztmp (:) - scatt_aux % tau (ichan,:)) 
        jd (:) = scatt_aux % ext (ichan,:) / (scatt_aux % ext (ichan,:) - scatt_aux % lambda (ichan,:) &
             & * angles (iprof) % coszen) * (1.0_JPRB / ztmp (:) - scatt_aux % tau (ichan,:)) 

        j_up (ichan,:) = ja (:) * aa (:) + jb (:) * bb (:) + jc (:) * cp (:) + jd (:) * cm (:)    
     end where
  end do 

End subroutine rttov_integratesource
