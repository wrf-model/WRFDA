!
Subroutine rttov_eddington_k ( & 
     & nwp_levels,        &! in
     & nchannels,         &! in
     & nprofiles,         &! in
     & lprofiles,         &! in
     & angles,            &! in
     & profiles,          &! in
     & profiles_k,        &! inout
     & cld_profiles,      &! in
     & scatt_aux,         &! in
     & scatt_aux_k,       &! inout
     & cld_radiance,      &! inout
     & cld_radiance_k)    ! inout 

  ! Description:
  ! AD of routine 
  ! to compute Eddington approximation to RT
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
  !  1.5       02/2005   K-code              (A. Collard)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !   Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:
  
  Use rttov_types, Only :      &
       & geometry_Type        ,&
       & profile_Type         ,&
       & profile_cloud_Type   ,&
       & profile_scatt_aux    ,&
       & radiance_cloud_Type 

  Use rttov_const, Only:       &
       & tcosmic 

  Use parkind1, Only : jpim     ,jprb
  
  Implicit None

#include "rttov_boundaryconditions.interface"
#include "rttov_integratesource.interface"
#include "rttov_boundaryconditions_k.interface"
#include "rttov_integratesource_k.interface"

!* Subroutine arguments:
  Integer (Kind=jpim), Intent (in) :: nwp_levels                              ! Number of NWP-levels
  Integer (Kind=jpim), Intent (in) :: nprofiles                               ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: nchannels                               ! Number of channels*profiles=radiances
  Integer (Kind=jpim), Intent (in) :: lprofiles (nchannels)                   ! Profile indices

  Type (geometry_Type),       Intent (in)    :: angles       (nprofiles)      ! Zenith angles
  Type (profile_Type),        Intent (in)    :: profiles     (nprofiles)      ! Profiles on RTTOV levels
  Type (profile_Type),        Intent (inout) :: profiles_k   (nchannels)      ! Profiles on RTTOV levels
  Type (profile_cloud_Type),  Intent (in)    :: cld_profiles (nprofiles)      ! Cloud profiles on NWP levels
  Type (profile_scatt_aux),   Intent (inout)    :: scatt_aux                     ! Auxiliary profile variables for RTTOV_SCATT
  Type (profile_scatt_aux),   Intent (inout) :: scatt_aux_k                  ! Auxiliary profile variables for RTTOV_SCATT
  Type (radiance_cloud_Type), Intent (inout) :: cld_radiance                  ! Radiances
  Type (radiance_cloud_Type), Intent (inout) :: cld_radiance_k               ! Radiances

!* Local variables
  Real (Kind=jprb), Dimension (nchannels,nwp_levels) :: dp                    ! D+ for boundary conditions
  Real (Kind=jprb), Dimension (nchannels,nwp_levels) :: dm                    ! D- for boundary conditions
  Real (Kind=jprb), Dimension (nchannels,nwp_levels) :: j_up                  ! Upward radiance source terms
  Real (Kind=jprb), Dimension (nchannels,nwp_levels) :: j_do                  ! Downward radiance source terms
  Real (Kind=jprb), Dimension (nchannels,nwp_levels) :: dp_k                 ! D+ for boundary conditions
  Real (Kind=jprb), Dimension (nchannels,nwp_levels) :: dm_k                 ! D- for boundary conditions
  Real (Kind=jprb), Dimension (nchannels,nwp_levels) :: j_up_k               ! Upward radiance source terms
  Real (Kind=jprb), Dimension (nchannels,nwp_levels) :: j_do_k               ! Downward radiance source terms


  Real (Kind=jprb), Dimension (nchannels,0:nwp_levels) :: irad_do1            ! Downward radiances 
  Real (Kind=jprb), Dimension (nchannels,0:nwp_levels) :: irad_do2            ! Downward radiances
  Real (Kind=jprb), Dimension (nchannels,nwp_levels+1) :: irad_up             ! Downward radiances 
  Real (Kind=jprb), Dimension (nchannels)              :: irad_sfc            ! Inward radiances at surface
  Real (Kind=jprb), Dimension (nchannels)              :: irad_space          ! Inward radiances from space
  Real (Kind=jprb), Dimension (nchannels,nwp_levels+1) :: tau_t               ! Transmittances integrated over all levels
  Real (Kind=jprb), Dimension (nchannels)              :: ftop, ftop_k       ! Downward radiances 
  Real (Kind=jprb), Dimension (nchannels,0:nwp_levels) :: irad_do2_k         ! Downward radiances 
  Real (Kind=jprb), Dimension (nchannels,0:nwp_levels) :: irad_do1_k         ! Downward radiances 
  Real (Kind=jprb), Dimension (nchannels,nwp_levels+1) :: irad_up_k          ! Upward radiances 
  Real (Kind=jprb), Dimension (nchannels)              :: irad_sfc_k         ! Inward radiances at surface
  Real (Kind=jprb), Dimension (nchannels)              :: irad_space_k       ! Inward radiances from space
  Real (Kind=jprb), Dimension (nchannels,nwp_levels+1) :: tau_t_k            ! Transmittances integrated over all levels

  Integer (Kind=jpim) :: ilayer, jlayer, iprof, ichan
  
  !- End of header --------------------------------------------------------

!* Channels * Profiles      
  do ichan = 1, nchannels
     iprof = lprofiles (ichan)

!* Top/bottom
     irad_sfc   (ichan) = scatt_aux % ems_cld (ichan) * profiles (iprof) % skin % t
     irad_space (ichan) = tcosmic

!* Clear-sky source terms
     j_up (ichan,:) = scatt_aux % b0 (iprof,:) * (1.0_JPRB - scatt_aux % tau (ichan,:)) 
     j_do (ichan,:) = scatt_aux % b0 (iprof,:) * (1.0_JPRB - scatt_aux % tau (ichan,:)) 

!* Downward radiance at cloud top
     irad_do1 (ichan,0) = irad_space (ichan)

     Do ilayer = 1, scatt_aux % mclayer (ichan) - 1
        irad_do1 (ichan,ilayer) = irad_do1 (ichan,ilayer-1) * scatt_aux % tau (ichan,ilayer) + j_do (ichan,ilayer)
     End do
  
     ftop (ichan) = irad_do1 (ichan,scatt_aux % mclayer (ichan) - 1)
  End do

  If (maxval(scatt_aux % mclayer) > 0) Then
!* Get D+, D- from boundary conditions

     Call rttov_boundaryconditions (&
      & nwp_levels,            &! in
      & nchannels,             &! in
      & nprofiles,             &! in
      & lprofiles,             &! in
      & scatt_aux,             &! in
      & profiles,              &! in
      & ftop,                  &! in
      & dp,                    &! out
      & dm)                     ! out 

!* Integrate radiance source terms
     Call rttov_integratesource (&
      & nwp_levels,            &! in
      & nchannels,             &! in
      & nprofiles,             &! in
      & lprofiles,             &! in
      & angles,                &! in
      & scatt_aux,             &! in
      & dp,                    &! in
      & dm,                    &! in
      & j_do,                  &! inout
      & j_up)                   ! inout 

  Endif

!* Integrate downward radiances/transmittance
  irad_do2 (:,0)            = irad_space (:)
  irad_up  (:,nwp_levels+1) = irad_sfc   (:)
  tau_t    (:,nwp_levels+1) = 1.0_JPRB
  
  Do ilayer = 1, nwp_levels
     jlayer = nwp_levels + 1 - ilayer
  
     irad_do2 (:,ilayer) = irad_do2 (:,ilayer-1) * scatt_aux % tau (:,ilayer) + j_do (:,ilayer)
     irad_up  (:,jlayer) = irad_up  (:,jlayer+1) * scatt_aux % tau (:,jlayer) + j_up (:,jlayer)
     
     tau_t    (:,jlayer) = tau_t    (:,jlayer+1) * scatt_aux % tau (:,jlayer)
  Enddo

  cld_radiance % bt (:) = irad_up (:,1) + scatt_aux % ref_cld (:) * irad_do2 (:,nwp_levels) * tau_t (:,1)

!* ADJOINT PART
  irad_up_k    (:,:) = 0.0_JPRB
  irad_do1_k   (:,:) = 0.0_JPRB
  irad_do2_k   (:,:) = 0.0_JPRB
  tau_t_k      (:,:) = 0.0_JPRB
  j_up_k       (:,:) = 0.0_JPRB
  j_do_k       (:,:) = 0.0_JPRB 
  irad_sfc_k   (:)   = 0.0_JPRB
  irad_space_k (:)   = 0.0_JPRB
  dp_k         (:,:) = 0.0_JPRB
  dm_k         (:,:) = 0.0_JPRB 
  ftop_k       (:)   = 0.0_JPRB 

!* Integrate downward radiances/transmittance
  irad_up_k (:,1) = irad_up_k (:,1) + cld_radiance_k % bt (:) 
  scatt_aux_k % ref_cld (:) = scatt_aux_k % ref_cld (:) + irad_do2 (:,nwp_levels) * tau_t (:,1) * cld_radiance_k % bt (:) 
  irad_do2_k (:,nwp_levels) = irad_do2_k (:,nwp_levels) + scatt_aux % ref_cld (:) * tau_t (:,1) * cld_radiance_k % bt (:)
  tau_t_k (:,1)             = tau_t_k (:,1)             + scatt_aux % ref_cld (:) & 
                             & * irad_do2 (:,nwp_levels) * cld_radiance_k % bt (:)
  cld_radiance_k % bt (:)   = 0.0_JPRB
  
  Do ilayer = nwp_levels, 1, -1
     jlayer = nwp_levels + 1 - ilayer
  
     tau_t_k           (:,jlayer+1) = tau_t_k (:,jlayer+1) + scatt_aux % tau (:,jlayer) * tau_t_k (:,jlayer)
     scatt_aux_k % tau (:,jlayer)   = scatt_aux_k % tau (:,jlayer) + tau_t (:,jlayer+1) * tau_t_k (:,jlayer)
     tau_t_k           (:,jlayer)   = 0.0_JPRB
     
     irad_up_k         (:,jlayer+1) = irad_up_k (:,jlayer+1) + scatt_aux % tau (:,jlayer) * irad_up_k (:,jlayer)
     scatt_aux_k % tau (:,jlayer)   = scatt_aux_k % tau (:,jlayer) + irad_up  (:,jlayer+1) * irad_up_k (:,jlayer)
     j_up_k            (:,jlayer)   = j_up_k (:,jlayer) + irad_up_k (:,jlayer)
     irad_up_k         (:,jlayer)   = 0.0_JPRB
     
     irad_do2_k        (:,ilayer-1) = irad_do2_k (:,ilayer-1) + scatt_aux % tau (:,ilayer) * irad_do2_k (:,ilayer)
     scatt_aux_k % tau (:,ilayer)   = scatt_aux_k % tau (:,ilayer) + irad_do2 (:,ilayer-1) * irad_do2_k (:,ilayer)
     j_do_k            (:,ilayer)   = j_do_k (:,ilayer) + irad_do2_k (:,ilayer)
     irad_do2_k        (:,ilayer)   = 0.0_JPRB     
  Enddo

  tau_t_k (:,nwp_levels+1) = 0.0_JPRB

  irad_sfc_k (:)              = irad_sfc_k (:) + irad_up_k (:,nwp_levels+1)
  irad_up_k  (:,nwp_levels+1) = 0.0_JPRB 
  
  irad_space_k (:)   = irad_space_k (:) + irad_do2_k (:,0) 
  irad_do2_k   (:,0) = 0.0_JPRB

  If (maxval(scatt_aux % mclayer) > 0) Then
!* Get D+, D- from boundary conditions

     Call rttov_integratesource_k (&
      & nwp_levels,            &! in
      & nchannels,             &! in
      & nprofiles,             &! in
      & lprofiles,             &! in
      & angles,                &! in
      & scatt_aux,             &! in
      & scatt_aux_k,           &! in
      & dp,                    &! in
      & dp_k,                  &! in
      & dm,                    &! in
      & dm_k,                  &! in
      & j_do,                  &! inout
      & j_do_k,                &! inout
      & j_up,                  &! inout
      & j_up_k)                 ! inout 

     Call rttov_boundaryconditions_k (&
      & nwp_levels,            &! in
      & nchannels,             &! in
      & nprofiles,             &! in
      & lprofiles,             &! in
      & scatt_aux,             &! in
      & scatt_aux_k,           &! in
      & profiles,              &! in
      & profiles_k ,           &! in
      & ftop,                  &! in
      & ftop_k,                &! in
      & dp,                    &! out
      & dp_k,                  &! out
      & dm,                    &! out
      & dm_k)                   ! out 
  Endif

!* Channels * Profiles      
  do ichan = 1, nchannels
     iprof = lprofiles (ichan)
     
!* Downward radiance at cloud top
     irad_do1_k (ichan,scatt_aux % mclayer (ichan) - 1) = irad_do1_k (ichan,scatt_aux % mclayer (ichan) - 1) + ftop_k (ichan)
     ftop_k (ichan) = 0.0_JPRB
     
     Do ilayer = scatt_aux % mclayer (ichan) - 1, 1, -1
        irad_do1_k        (ichan,ilayer-1) = irad_do1_k (ichan,ilayer-1) & 
	                                   &  + scatt_aux % tau (ichan,ilayer) * irad_do1_k (ichan,ilayer)
	scatt_aux_k % tau (ichan,ilayer)   = scatt_aux_k % tau (ichan,ilayer) & 
	                                   & + irad_do1 (ichan,ilayer-1) * irad_do1_k (ichan,ilayer)
	j_do_k            (ichan,ilayer)   = j_do_k (ichan,ilayer) + irad_do1_k (ichan,ilayer)
	irad_do1_k (ichan,ilayer)   = 0.0_JPRB
     End do
     
     irad_space_k (ichan) = irad_space_k (ichan) + irad_do1_k (ichan,0)
     irad_do1_k (ichan,0) = 0.0_JPRB         
     
!* Clear-sky source terms
     j_up_k (ichan,:) = j_up_k (ichan,:) + j_do_k (ichan,:)
     j_do_k (ichan,:) = 0.0_JPRB
     
     scatt_aux_k % b0  (ichan,:) = scatt_aux_k % b0  (ichan,:) + (1.0_JPRB - scatt_aux    % tau (ichan,:)) * j_up_k (ichan,:)
     scatt_aux_k % tau (ichan,:) = scatt_aux_k % tau (ichan,:) - scatt_aux % b0 (iprof,:) * j_up_k (ichan,:)
     j_up_k (ichan,:) = 0.0_JPRB
               
!* Top/bottom
     irad_space_k (ichan) = 0.0_JPRB
     
     scatt_aux_k % ems_cld (ichan) = scatt_aux_k % ems_cld (ichan) + profiles (iprof) % skin % t * irad_sfc_k (ichan)
     profiles_k (ichan) % skin % t = profiles_k (ichan) % skin % t + scatt_aux % ems_cld (ichan) * irad_sfc_k (ichan)
     irad_sfc_k (ichan) = 0.0_JPRB
  End do

End Subroutine rttov_eddington_k
