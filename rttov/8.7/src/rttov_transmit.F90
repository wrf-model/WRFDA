Subroutine rttov_transmit( &
     & nfrequencies, &! in
     & nchannels,    &! in
     & nprofiles,    &! in
     & nlevels,      &! in
     & channels,     &! in
     & polarisations, &! in
     & lprofiles,    &! in
     & predictors,   &! in
     & aux,          &! in
     & coef,         &! in
     & transmission, &! out
     & od_layer,     &! out
     & opdp_ref_freq) ! out 
  !
  ! Description:
  !  To calculate optical depths for a number of channels
  !  and profiles from every pressure level to space.
  ! To interpolate optical depths on to levels of radiative transfer model
  ! (which, at present, entails only surface transmittance, as
  ! other optical depths are on *rt* levels) and to convert
  ! optical depths to transmittances.
  !
  ! Code based on OPDEP and RTTAU from previous versions of RTTOV
  ! Only one profile per call
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
  !  1.0    01/12/2002  New F90 code with structures (P Brunel A Smith) 
  !  1.1    29/01/2003  Add WV Continuum and CO2 capability (P Brunel)
  !  1.2    04/12/2003  Optimisation (J Hague and D Salmond ECMWF)
  !  1.3    26/09/2003  Modified to allow for multiple polarisations (S English)
  !  1.4    17/08/2004    Bug fixed in setting transmission to 1 (S English)
  !  1.5    28/02/2005 Improved vectorisation (D Dent)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:

  ! Imported Parameters:
  Use rttov_const, Only: &
       & mwcldtop       ,&
       & sensor_id_mw   ,&
       & max_optical_depth 

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef     ,&
       & predictors_Type,&
       & transmission_Type  ,&
       & profile_aux 

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),               Intent(in)    :: nfrequencies            ! Number of frequencies
  Integer(Kind=jpim),               Intent(in)    :: nchannels               ! Number of output radiances
  Integer(Kind=jpim),               Intent(in)    :: nprofiles               ! Number of profiles
  Integer(Kind=jpim),               Intent(in)    :: nlevels                 ! Number of pressure levels
  Integer(Kind=jpim),               Intent(in)    :: channels(nfrequencies)     ! Channel indices
  Integer(Kind=jpim),               Intent(in)    :: polarisations(nchannels,3) ! polarisation indices
  Integer(Kind=jpim),               Intent(in)    :: lprofiles(nfrequencies)    ! Profiles indices
  Type(predictors_Type), Intent(in)    :: predictors( nprofiles ) ! Predictors
  Type(rttov_coef),      Intent(in)    :: coef                    ! Coefficients
  Type(transmission_Type),Intent(inout) :: transmission           ! Transmittances and single-layer od
  Type(profile_aux),     Intent(in)    :: aux( nprofiles )        ! auxillary profiles informations
  Real(Kind=jprb),                  Intent(out)   :: od_layer(nlevels,nchannels)  ! sat to layer optical depth
  Real(Kind=jprb),                  Intent(out)   :: opdp_ref_freq(nlevels,nfrequencies)  ! layer optical depth 
                                                                       !   before threshold

  !local variables:
  Real(Kind=jprb) :: opticaldepth(nlevels,nfrequencies)       ! raw layer optical depth 
  Real(Kind=jprb) :: od_surf(nfrequencies)                    ! sat to surface optical depth 
  Real(Kind=jprb) :: od_layer_freq(nlevels,nfrequencies)      ! sat to layer optical depth
  Real(Kind=jprb) :: od_singlelayer_freq(nlevels,nfrequencies)      ! sat to layer optical depth
  Real(Kind=jprb) :: opdp_ref(nlevels,nchannels)              ! layer optical depth 
  Real(Kind=jprb) :: tau_surf_freq(nfrequencies)              ! sat to surface transmission at each frequency 
  Real(Kind=jprb) :: tau_layer_freq(nlevels,nfrequencies)     ! sat to layer transmission 
  Real(Kind=jprb),  Pointer :: debye_prof(:,:)        ! pointer on Debye profiles
  Integer(Kind=jpim) :: lev, chan, j, prof, freq, kpol        ! loop variables

  ! cloud liquid water local variables
  Real(Kind=jprb) :: zf, zf_sq, z34_dif, z45_dif, z1_sq, z2_sq, z1_div, z2_div
  Real(Kind=jprb) :: z1_den, z2_den, zastar, z1_prod, z2_prod, z3_prod, z4_prod
  Real(Kind=jprb) :: zbstar, zbstar_sq, za2star, za2star_sq, zdiv, zgstar
  Real(Kind=jprb) :: z1f_sq_z1_sq, z2f_sq_z2_sq
  Integer(Kind=jpim) :: ii

  !- End of header --------------------------------------------------------


  !-----------------------------------------
  !1. calculate layer gaseous optical depths
  !-----------------------------------------

  !--------------------------
  !1.1 start with mixed gases
  !--------------------------
  opticaldepth(:,:)=0
  Do j = 1, nfrequencies
     chan = channels(j)
     prof = lprofiles(j)

     Do ii=1,coef % nmixed
       Do lev=1,nlevels
        opticaldepth(lev,j)= opticaldepth(lev,j) + &
         & coef%mixedgas(lev,chan,ii) * predictors( prof ) % mixedgas(ii,lev)  
       End Do
     End Do

  !--------------------
  !1.2 add water vapour
  !--------------------

     Do ii=1,coef % nwater
       Do lev=1,nlevels
        opticaldepth(lev,j)= opticaldepth(lev,j) + &
         & coef%watervapour(lev,chan,ii) * predictors( prof ) % watervapour(ii,lev)  
       End Do
     End Do

  !-------------
  !1.3 add ozone
  !-------------

     Do ii=1,coef % nozone
       Do lev=1,nlevels
        opticaldepth(lev,j)= opticaldepth(lev,j) + &
         & coef%ozone(lev,chan,ii) * predictors( prof ) % ozone(ii,lev)  
       End Do
     End Do

  !------------------------------
  !1.4 add Water Vapour Continuum
  !------------------------------

     Do ii=1,coef % nwvcont
       Do lev=1,nlevels
        opticaldepth(lev,j)= opticaldepth(lev,j) + &
              & coef%wvcont(lev,chan,ii) * predictors( prof ) % wvcont(ii,lev)  
       End Do
     End Do

  !-----------
  !1.5 add CO2
  !-----------

     Do ii=1,coef % nco2
       Do lev=1,nlevels
        opticaldepth(lev,j)= opticaldepth(lev,j) + &
             & coef%co2(lev,chan,ii) * predictors( prof ) % co2(ii,lev)  
       End Do
     End Do

  !--------------------
  !1.6 add liquid water (MW only)
  !--------------------

     If ( coef % id_sensor == sensor_id_mw ) Then
        debye_prof => aux(prof) % debye_prof
        If( predictors(prof) % ncloud >= 1 ) Then
           Do lev = mwcldtop, nlevels
              zf           = coef % frequency_ghz(chan)
              zf_sq        = zf*zf
              z1_sq        = debye_prof(1,lev) * debye_prof(1,lev)
              z2_sq        = debye_prof(2,lev) * debye_prof(2,lev)
              z34_dif      = debye_prof(3,lev) - debye_prof(4,lev)
              z45_dif      = debye_prof(4,lev) - debye_prof(5,lev)
              z1f_sq_z1_sq = zf_sq + z1_sq
              z2f_sq_z2_sq = zf_sq + z2_sq
              z1_div       = 1.0_JPRB / z1f_sq_z1_sq
              z2_div       = 1.0_JPRB / z2f_sq_z2_sq
              z1_den       = z34_dif * z1_div
              z2_den       = z45_dif * z2_div
              zastar       = debye_prof(3,lev) - zf_sq * (z1_den + z2_den)
              z1_prod      = z34_dif * debye_prof(1,lev)
              z2_prod      = z1_prod * z1_div
              z3_prod      = z45_dif * debye_prof(2,lev)
              z4_prod      = z3_prod * z2_div
              zbstar       = -zf * (z2_prod + z4_prod)
              zbstar_sq    = zbstar * zbstar
              za2star      = zastar + 2.0_JPRB
              za2star_sq   = za2star * za2star
              zdiv         = za2star_sq + zbstar_sq
              zgstar       = -3.0_JPRB * zbstar / zdiv
              opticaldepth(lev,j) = opticaldepth(lev,j) -&
                    & 1.5_JPRB * zf * zgstar * predictors ( prof ) % clw(lev) 
           End Do
        Endif
     End If
 End Do


  !----------------------------------------
  !2. Compute layer to space optical depths
  !----------------------------------------
  ! notes: apply gamma correction; check value is sensible and constrain
  ! if necessary.

  ! note that optical depth in the calculations is negative
  ! store optical depth in reference array for TL, AD and K calculations
  opdp_ref_freq(:,:) = opticaldepth(:,:) 
  Where( opticaldepth(:,:) > 0.0_JPRB )
     opticaldepth = 0.0_JPRB
  End Where

  Do j = 1, nfrequencies
     chan = channels(j)
     opticaldepth(:,j) = coef%ff_gam(chan) * opticaldepth(:,j)
  End Do
  od_singlelayer_freq(:,:) = - opticaldepth(:,:)

  od_layer_freq(1,:) = opticaldepth(1,:)
  Do lev = 2, nlevels
     od_layer_freq(lev,:) = od_layer_freq(lev-1,:) + opticaldepth(lev,:)
  End Do

  !-------------------------------------------
  !3. Convert optical depths to transmittances
  !-------------------------------------------

  ! On some computers when optical depth is too thick
  ! there is an underflow during the conversion in
  ! transmittances. In that case uncomment following line
  ! and the declaration statement of max_optical_depth
  od_layer_freq(:,:) = Max(od_layer_freq(:,:),-max_optical_depth)

  tau_layer_freq(:,:) = Exp(od_layer_freq(:,:))


  !-----------------------------------------------------
  !4. Compute optical depth and transmittance at surface
  !-----------------------------------------------------
  Do j = 1, nfrequencies
     prof       = lprofiles(j)
     od_surf(j) = od_layer_freq(aux(prof) % nearestlev_surf,j) + &
            & aux(prof) % pfraction_surf * &
           & (  od_layer_freq(aux(prof) % nearestlev_surf-1,j) - &
              & od_layer_freq(aux(prof) % nearestlev_surf  ,j)    ) 
  End Do

  tau_surf_freq(:) = Exp(od_surf(:))
  
  !-----------------------------------------------------
  !5. Store transmittances for other polarisations
  !-----------------------------------------------------

 transmission % tau_layer(:,:) = 1.0_JPRB
 transmission % od_singlelayer(:,:) = 1.0_JPRB
 od_layer(:,:) = 0.0_JPRB
 opdp_ref(:,:) = 0.0_JPRB
  Do j = 1, nchannels
     freq       = polarisations(j,2)               ! Frequency index
     kpol       = 1 + j - polarisations(freq,1)    ! Polarisation index
     prof       = lprofiles(freq)       ! Profile index
     transmission % tau_layer(:,j) = tau_layer_freq(:,freq)
     transmission % od_singlelayer(:,j) = od_singlelayer_freq(:,freq)
     od_layer(:,j) = od_layer_freq(:,freq) 
     opdp_ref(:,j) = opdp_ref_freq(:,freq) 
     transmission % tau_surf(j) = tau_surf_freq(freq) 
  End Do  

End Subroutine rttov_transmit
