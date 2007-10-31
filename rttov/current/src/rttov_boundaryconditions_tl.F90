!
Subroutine rttov_boundaryconditions_tl (&
     & nwp_levels,    &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & lprofiles,     &! in
     & scatt_aux,     &! in
     & scatt_aux_tl,  &! in
     & profiles ,     &! in
     & profiles_tl ,  &! in
     & ftop,          &! in
     & ftop_tl,       &! in
     & dp,            &! out
     & dp_tl,         &! out
     & dm,            &! out
     & dm_tl)          ! out 


  ! Description:
  ! to compute boundary conditions for Eddington approximation to RT
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
  !  1.0       09/2002   Initial version      (E. Moreau)
  !  1.1       05/2003   RTTOV7.3 compatible  (F. Chevallier)
  !  1.2       03/2004   Included polarimetry (R. Saunders)
  !  1.3       11/2004   Clean-up             (P. Bauer)
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
       & profile_Type         ,&
       & profile_scatt_aux 

  Use parkind1, Only : jpim     ,jprb
  
  Implicit none

!* Subroutine arguments:
  Integer (Kind=jpim), Intent (in) :: nwp_levels                              ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nprofiles                               ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: nchannels                               ! Number of radiances
  Integer (Kind=jpim), Intent (in) :: lprofiles (nchannels)                   ! Profile indices

  Type (profile_scatt_aux), Intent (in)    :: scatt_aux                       ! Auxiliary profile variables for RTTOV_SCATT
  Type (profile_scatt_aux), Intent (in)    :: scatt_aux_tl                    ! Auxiliary profile variables for RTTOV_SCATT
  Type (profile_Type),      Intent (in)    :: profiles    (nprofiles)         ! Profiles on RTTOV levels
  Type (profile_Type),      Intent (in)    :: profiles_tl (nprofiles)         ! Profiles on RTTOV levels

  Real (Kind=jprb), Intent  (in), dimension (nchannels)            :: ftop
  Real (Kind=jprb), Intent  (in), dimension (nchannels)            :: ftop_tl
  Real (Kind=jprb), Intent (out), dimension (nchannels,nwp_levels) :: dp   , dm
  Real (Kind=jprb), Intent (out), dimension (nchannels,nwp_levels) :: dp_tl, dm_tl

!* Local variables
  Real    (Kind=jprb), dimension (nchannels,nwp_levels) :: lh_p   , lh_m   , bh
  Real    (Kind=jprb), dimension (nchannels,nwp_levels) :: lh_p_tl, lh_m_tl, bh_tl
  Real    (Kind=jprb), allocatable                      :: a    (:,:), b    (:), dx    (:)
  Real    (Kind=jprb), allocatable                      :: a_tl (:,:), b_tl (:), dx_tl (:)
  Real    (Kind=jprb)                                   :: ztmp, ztmp_tl
  Integer (Kind=jpim)                                   :: ilayer, jlayer, klayer, ilin, icol
  Integer (Kind=jpim)                                   :: ndim, iprof, ichan, jj, ii, mcly
  
!* Lapack/ESSL
  Real      (Kind=jprb), allocatable :: ab   (:,:)         
  Integer   (Kind=jpim), allocatable :: ipiv (:)          
  Integer   (Kind=jpim)              :: kl, ku, ldab, info, nrhs                   
  Character (len=1)                  :: trans                                                 
  Logical                            :: ll_essl
  
  !- End of header --------------------------------------------------------
!* Init Math related variables
  ll_essl = .false.
  
  kl = 2
  ku = 2
  if (ll_essl) then
     ldab = 2 * kl + ku + 16     
  else
     ldab = 2 * kl + ku + 1      
  endif
  trans = 'N'
  nrhs  = 1
  info  = 0

!* Reset      
  dp_tl (:,:) = 0.0_JPRB
  dm_tl (:,:) = 0.0_JPRB
  dp    (:,:) = 0.0_JPRB
  dm    (:,:) = 0.0_JPRB

!* Channels * Profiles      
  do ichan = 1, nchannels
     iprof = lprofiles (ichan)
     
     bh_tl (ichan,:) = scatt_aux_tl % b1 (iprof,:) / scatt_aux    % h (ichan,:) &
                   & - scatt_aux    % b1 (iprof,:) * scatt_aux_tl % h (ichan,:) &
                   & / scatt_aux    % h  (ichan,:) / scatt_aux    % h (ichan,:) 
     bh    (ichan,:) = scatt_aux    % b1 (iprof,:) / scatt_aux    % h (ichan,:)

     lh_p_tl (ichan,:) = scatt_aux_tl % lambda (ichan,:) / scatt_aux    % h (ichan,:) &
                     & - scatt_aux    % lambda (ichan,:) * scatt_aux_tl % h (ichan,:) &
		     & / scatt_aux    % h      (ichan,:) / scatt_aux    % h (ichan,:) 
     lh_p    (ichan,:) = (1.0_JPRB + scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 

     lh_m_tl (ichan,:) = - 1.0_JPRB * lh_p_tl (ichan,:)
     lh_m    (ichan,:) = (1.0_JPRB - scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:))

     mcly = scatt_aux % mclayer (ichan)
     ndim = 2 * (nwp_levels - mcly + 1)

     allocate (a_tl  (ndim,ndim))
     allocate (b_tl  (ndim     ))
     allocate (dx_tl (ndim     ))
     allocate (a     (ndim,ndim))
     allocate (b     (ndim     ))
     allocate (ab    (ldab,ndim))
     allocate (ipiv  (ndim     ))
     allocate (dx    (ndim     ))

     a_tl (:,:) = 0.0_JPRB
     b_tl (:  ) = 0.0_JPRB
     a    (:,:) = 0.0_JPRB
     b    (:  ) = 0.0_JPRB
     ab   (:,:) = 0.0_JPRB
     ipiv (:)   = 0

     do ilayer = 2, ndim - 2, 2
        jlayer = nwp_levels - ilayer / 2 + 1
        klayer = jlayer - 1

        ilin =  ilayer
        icol = (ilayer - 1)

        ztmp    = exp (scatt_aux    % lambda (ichan,jlayer) * scatt_aux    % dz (iprof,jlayer))
        ztmp_tl =     (scatt_aux_tl % lambda (ichan,jlayer) * scatt_aux    % dz (iprof,jlayer) + &
                     & scatt_aux    % lambda (ichan,jlayer) * scatt_aux_tl % dz (iprof,jlayer)) * ztmp 

!* From downward fluxes at i-th interface (@ level=dz for jlayer == level=0 for klayer)
        a_tl (ilin  ,icol  ) = lh_p_tl (ichan,jlayer) * ztmp + lh_p (ichan,jlayer) * ztmp_tl
        a    (ilin  ,icol  ) = lh_p    (ichan,jlayer) * ztmp

        a_tl (ilin  ,icol+1) = lh_m_tl (ichan,jlayer) / ztmp - lh_m (ichan,jlayer) * ztmp_tl / ztmp / ztmp
        a    (ilin  ,icol+1) = lh_m    (ichan,jlayer) / ztmp

        a_tl (ilin  ,icol+2) = -1.0_JPRB * lh_p_tl (ichan,klayer) 
        a    (ilin  ,icol+2) = -1.0_JPRB * lh_p    (ichan,klayer) 

        a_tl (ilin  ,icol+3) = -1.0_JPRB * lh_m_tl (ichan,klayer) 
        a    (ilin  ,icol+3) = -1.0_JPRB * lh_m    (ichan,klayer) 

        b_tl (ilin  ) = bh_tl (ichan,klayer) - bh_tl (ichan,jlayer)
        b    (ilin  ) = bh    (ichan,klayer) - bh    (ichan,jlayer)

!* From upward fluxes at i-th interface (@ level=dz for jlayer == level=0 for klayer)
        a_tl (ilin+1,icol  ) = lh_m_tl (ichan,jlayer) * ztmp + lh_m (ichan,jlayer) * ztmp_tl
        a    (ilin+1,icol  ) = lh_m    (ichan,jlayer) * ztmp

        a_tl (ilin+1,icol+1) = lh_p_tl (ichan,jlayer) / ztmp - lh_p (ichan,jlayer) * ztmp_tl / ztmp / ztmp
        a    (ilin+1,icol+1) = lh_p    (ichan,jlayer) / ztmp

        a_tl (ilin+1,icol+2) = -1.0_JPRB * lh_m_tl (ichan,klayer) 
        a    (ilin+1,icol+2) = -1.0_JPRB * lh_m    (ichan,klayer) 

        a_tl (ilin+1,icol+3) = -1.0_JPRB * lh_p_tl (ichan,klayer) 
        a    (ilin+1,icol+3) = -1.0_JPRB * lh_p    (ichan,klayer) 

        b_tl (ilin+1) = bh_tl (ichan,jlayer) - bh_tl (ichan,klayer)
        b    (ilin+1) = bh    (ichan,jlayer) - bh    (ichan,klayer)
     end do

!* From boundary conditions at bottom of the atmosphere with r_sfc=1-e_sfc
     ztmp    = (2.0_JPRB - scatt_aux % ems_bnd (ichan)) * scatt_aux % lambda (ichan,nwp_levels) / scatt_aux % h (ichan,nwp_levels)
     ztmp_tl =   -1.0_JPRB * scatt_aux_tl % ems_bnd (ichan)  * scatt_aux    % lambda (ichan,nwp_levels) &
             & / scatt_aux    % h (ichan,nwp_levels) &
             & + (2.0_JPRB - scatt_aux    % ems_bnd (ichan)) * scatt_aux_tl % lambda (ichan,nwp_levels) &
	     & / scatt_aux    % h (ichan,nwp_levels) &
             & - (2.0_JPRB - scatt_aux    % ems_bnd (ichan)) * scatt_aux    % lambda (ichan,nwp_levels) &
	     & * scatt_aux_tl % h (ichan,nwp_levels) &
             & / scatt_aux % h (ichan,nwp_levels) / scatt_aux % h (ichan,nwp_levels) 

     a_tl (1,1) = scatt_aux_tl % ems_bnd (ichan) - ztmp_tl
     a    (1,1) = scatt_aux    % ems_bnd (ichan) - ztmp

     a_tl (1,2) = scatt_aux_tl % ems_bnd (ichan) + ztmp_tl
     a    (1,2) = scatt_aux    % ems_bnd (ichan) + ztmp

     b_tl (1) = scatt_aux_tl % ems_bnd (ichan) * (profiles    (iprof) % skin % t - scatt_aux    % b0 (iprof,nwp_levels)) &
            & + scatt_aux    % ems_bnd (ichan) * (profiles_tl (iprof) % skin % t - scatt_aux_tl % b0 (iprof,nwp_levels)) &
            & - scatt_aux_tl % ems_bnd (ichan) * bh (ichan,nwp_levels) &
            & + (2.0_JPRB - scatt_aux % ems_bnd (ichan)) * bh_tl (ichan,nwp_levels) 
     b (1)    = scatt_aux % ems_bnd (ichan) * (profiles (iprof) % skin % t - scatt_aux % b0 (iprof,nwp_levels)) &
            & + (2.0_JPRB - scatt_aux % ems_bnd (ichan)) * bh    (ichan,nwp_levels)  

!* From boundary conditions at top of the atmosphere 
     ztmp    = exp (scatt_aux % lambda (ichan,mcly) * scatt_aux % dz (iprof,mcly))
     ztmp_tl = (scatt_aux_tl % lambda (ichan,mcly) * scatt_aux    % dz (iprof,mcly) &
            & + scatt_aux    % lambda (ichan,mcly) * scatt_aux_tl % dz (iprof,mcly)) * ztmp 

     a_tl (ndim,ndim-1) = lh_p_tl (ichan,mcly) * ztmp + lh_p (ichan,mcly) * ztmp_tl
     a    (ndim,ndim-1) = lh_p    (ichan,mcly) * ztmp

     a_tl (ndim,ndim  ) = lh_m_tl (ichan,mcly) / ztmp - lh_m (ichan,mcly) * ztmp_tl / ztmp / ztmp
     a    (ndim,ndim  ) = lh_m    (ichan,mcly) / ztmp

     b_tl (ndim) = ftop_tl (ichan) - scatt_aux_tl % bn (iprof,mcly) - bh_tl (ichan,mcly)
     b    (ndim) = ftop    (ichan) - scatt_aux    % bn (iprof,mcly) - bh    (ichan,mcly)

!* Solve equations A * DX = B, forward          
     do jj = 1, ndim
        do ii = max(1,jj-ku), min(ndim,jj+kl)
           ab (kl+ku+ii-jj+1,jj) = a (ii,jj)
        end do
     end do

!     if (ll_essl) then
!         call dgbf   (ab, ldab, ndim, kl, ku, ipiv)                            
!     else
         call dgbtrf (ndim, ndim, kl, ku, ab, ldab, ipiv, info)                  
!     endif
     if (info /= 0) write (*,*) ' DGBTRF boundary_conditions: ', info

     dx (:) = b (:)
!     if (ll_essl) then
!         call dgbs   (ab, ldab, ndim, kl, ku, ipiv, dx)                         
!     else
         call dgbtrs (trans, ndim, kl, ku, nrhs, ab, ldab, ipiv, dx, ndim, info)
!     endif
     if (info /= 0) write (*,*) ' DGBTRS boundary_conditions: ', info

!* Solve equations A * DX = B, tangent-linear          
     dx_tl (:) = b_tl (:) - matmul (a_tl, dx)

!     if (ll_essl) then
!         call dgbs   (ab, ldab, ndim, kl, ku, ipiv, dx_tl)                         
!     else
         call dgbtrs (trans, ndim, kl, ku, nrhs, ab, ldab, ipiv, dx_tl, ndim, info)
!     endif
     if (info /= 0) write (*,*) ' DGBTRS boundary_conditions: ', info
     
!* Decompose D+ and D-
     do ilayer = 2, ndim, 2
        jlayer = nwp_levels - ilayer / 2 + 1
	
        dp_tl (ichan,jlayer) = dx_tl (ilayer-1)
        dp    (ichan,jlayer) = dx    (ilayer-1)
        dm_tl (ichan,jlayer) = dx_tl (ilayer  )
        dm    (ichan,jlayer) = dx    (ilayer  )
     end do

     deallocate (a, b, dx, a_tl, b_tl, dx_tl, ab, ipiv)
  end do
 
End subroutine rttov_boundaryconditions_tl
