!
Subroutine rttov_boundaryconditions (&
     & nwp_levels,    &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & lprofiles,     &! in
     & scatt_aux,     &! in
     & profiles ,     &! in
     & ftop,          &! in
     & dp,            &! out
     & dm)             ! out 


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
       & profile_Type       ,&
       & profile_scatt_aux 

  Use parkind1, Only : jpim     ,jprb

  Implicit none

!* Subroutine arguments:
  Integer (Kind=jpim), Intent (in) :: nwp_levels                              ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nprofiles                               ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: nchannels                               ! Number of radiances
  Integer (Kind=jpim), Intent (in) :: lprofiles (nchannels)                   ! Profile indices
  
  Type (profile_scatt_aux), Intent (in) :: scatt_aux                          ! Auxiliary profile variables for RTTOV_SCATT
  Type (profile_Type)     , Intent (in) :: profiles (nprofiles)               ! Profiles on RTTOV levels
  
  Real (Kind=jprb), Intent  (in), dimension (nchannels)            :: ftop    ! Downward radiances at cloud top
  Real (Kind=jprb), Intent (out), dimension (nchannels,nwp_levels) :: dp, dm  ! Coefficients from boundary conditions

!* Local variables
  Real    (Kind=jprb), dimension (nchannels,nwp_levels) :: lh_p, lh_m, bh
  Real    (Kind=jprb), allocatable                      :: a (:,:), b (:), dx (:)
  Real    (Kind=jprb)                                   :: ztmp
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
  dp (:,:) = 0.0_JPRB
  dm (:,:) = 0.0_JPRB

!* Channels * Profiles      
  do ichan = 1, nchannels
     iprof = lprofiles (ichan)
     
     bh   (ichan,:) = scatt_aux % b1 (iprof,:) / scatt_aux % h (ichan,:)
     lh_p (ichan,:) = (1.0_JPRB + scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:)) 
     lh_m (ichan,:) = (1.0_JPRB - scatt_aux % lambda (ichan,:) / scatt_aux % h (ichan,:))

     mcly = scatt_aux % mclayer (ichan)

     ndim = 2 * (nwp_levels - mcly + 1)

     allocate (a    (ndim,ndim))
     allocate (b    (ndim     ))
     allocate (dx   (ndim     ))
     allocate (ab   (ldab,ndim))
     allocate (ipiv (ndim     ))

     a    (:,:) = 0.0_JPRB
     b    (:  ) = 0.0_JPRB
     ab   (:,:) = 0.0_JPRB
     ipiv (:)   = 0

     do ilayer = 2, ndim - 2, 2
        jlayer = nwp_levels - ilayer / 2 + 1
        klayer = jlayer - 1

        ilin = ilayer
        icol = ilayer - 1

        ztmp = exp (scatt_aux % lambda (ichan,jlayer) * scatt_aux % dz (iprof,jlayer))

!* From downward fluxes at i-th interface (@ level=dz for jlayer == level=0 for klayer)
        a (ilin  ,icol  ) =             lh_p (ichan,jlayer) * ztmp
        a (ilin  ,icol+1) =             lh_m (ichan,jlayer) / ztmp
        a (ilin  ,icol+2) = -1.0_JPRB * lh_p (ichan,klayer) 
        a (ilin  ,icol+3) = -1.0_JPRB * lh_m (ichan,klayer) 
	
        b (ilin  ) = bh (ichan,klayer) - bh (ichan,jlayer)

!* From upward fluxes at i-th interface (@ level=dz for jlayer == level=0 for klayer)
        a (ilin+1,icol  ) =             lh_m (ichan,jlayer) * ztmp
        a (ilin+1,icol+1) =             lh_p (ichan,jlayer) / ztmp
        a (ilin+1,icol+2) = -1.0_JPRB * lh_m (ichan,klayer) 
        a (ilin+1,icol+3) = -1.0_JPRB * lh_p (ichan,klayer) 

        b (ilin+1) = bh (ichan,jlayer) - bh (ichan,klayer)
     end do

!* From boundary conditions at bottom of the atmosphere with r_sfc=1-e_sfc
     ztmp = (2.0_JPRB - scatt_aux % ems_bnd (ichan)) * scatt_aux % lambda (ichan,nwp_levels) / scatt_aux % h (ichan,nwp_levels)

     a (1,1) = scatt_aux % ems_bnd (ichan) - ztmp
     a (1,2) = scatt_aux % ems_bnd (ichan) + ztmp

     b (1)   = scatt_aux % ems_bnd (ichan) * (profiles (iprof) % skin % t - scatt_aux % b0 (iprof,nwp_levels)) &
           & + (2.0_JPRB - scatt_aux % ems_bnd (ichan)) * bh (ichan,nwp_levels)  
     
!* From boundary conditions at top of the atmosphere 
     ztmp = exp (scatt_aux % lambda (ichan,mcly) * scatt_aux % dz (iprof,mcly))
     
     a (ndim,ndim-1) = lh_p (ichan,mcly) * ztmp
     a (ndim,ndim  ) = lh_m (ichan,mcly) / ztmp
     
     b (ndim) = ftop (ichan) - scatt_aux % bn (iprof,mcly) - bh (ichan,mcly)

!* Solve equations A * DX = B          
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

!* Decompose D+ and D-
     do ilayer = 2, ndim, 2
        jlayer = nwp_levels - ilayer / 2 + 1
	
        dp (ichan,jlayer) = dx (ilayer-1)
        dm (ichan,jlayer) = dx (ilayer  )
     end do

     deallocate (a, b, dx, ab, ipiv)
  end do
  
End subroutine rttov_boundaryconditions
