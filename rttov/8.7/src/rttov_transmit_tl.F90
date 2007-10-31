Subroutine rttov_transmit_tl( &
     & nfrequencies, &! in
     & nchannels,    &! in
     & nprofiles,    &! in
     & nlevels,      &! in
     & channels,     &! in
     & polarisations, &! in
     & lprofiles,    &! in
     & predictors,   &! in
     & predictors_tl,   &! in
     & aux,             &! in
     & aux_tl,          &! in
     & coef,            &! in
     & od_layer,        &! in
     & opdp_ref,        &! in
     & transmission,    &! in
     & transmission_tl ) ! inout 
  ! Description:
  ! Tangent linear of rttov_transmit
  !  To calculate optical depths for a number of channels
  !  and profiles from every pressure level to space.
  ! To interpolate optical depths on to levels of radiative transfer model
  ! (which, at present, entails only surface transmittance, as
  ! other optical depths are on *rt* levels) and to convert
  ! optical depths to transmittances.
  !
  ! Code based on OPDEPTL and RTTAUTL from previous versions of RTTOV
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
  !  1.4    28/02/2005  Improved vectorisation (D Dent)  
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
       & sensor_id_mw   


  Use rttov_types, Only : &
       & rttov_coef     ,&
       & predictors_Type,&
       & transmission_Type  ,&
       & profile_aux 

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),  Intent(in)    :: nfrequencies
  Integer(Kind=jpim),  Intent(in)    :: nchannels
  Integer(Kind=jpim),  Intent(in)    :: nprofiles
  Integer(Kind=jpim),  Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),  Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),  Intent(in)    :: nlevels
  Integer(Kind=jpim),  Intent(in)    :: lprofiles(nfrequencies)
  Type(predictors_Type), Intent(in)    :: predictors( nprofiles )
  Type(predictors_Type), Intent(in)    :: predictors_tl( nprofiles )
  Type(rttov_coef)     , Intent(in)    :: coef
  Type(profile_aux)   ,  Intent(in)    :: aux( nprofiles )
  Type(profile_aux)   ,  Intent(in)    :: aux_tl( nprofiles )
  Real(Kind=jprb),       Intent(in)    :: od_layer(nlevels,nchannels)
  Real(Kind=jprb),       Intent(in)    :: opdp_ref(nlevels,nfrequencies)
  Type(transmission_Type),Intent(in)   :: transmission
  Type(transmission_Type),Intent(inout):: transmission_tl


  !local variables:
  Real(Kind=jprb) :: od_layer_tl(nlevels,nfrequencies)
  Real(Kind=jprb) :: opticaldepth_tl(nlevels,nfrequencies)
  Real(Kind=jprb) :: od_surf_tl(nfrequencies)
  Real(Kind=jprb) :: tau_surf_freq(nfrequencies)              ! sat to surface transmission at each frequency 
  Real(Kind=jprb) :: tau_layer_freq(nlevels,nfrequencies)     ! sat to layer transmission at each frequency
  Real(Kind=jprb) :: tau_surf_freq_tl(nfrequencies)              ! sat to surface transmission at each frequency 
  Real(Kind=jprb) :: tau_layer_freq_tl(nlevels,nfrequencies)     ! sat to layer transmission at each frequency
  Real(Kind=jprb) :: od_singlelayer_freq_tl(nlevels,nfrequencies)

  Real(Kind=jprb),  Pointer :: debye_prof(:,:)
  Real(Kind=jprb),  Pointer :: debye_prof_tl(:,:)
  Integer(Kind=jpim) :: lev,chan,j,freq,kpol
  Integer(Kind=jpim) :: prof

  ! cloud liquid water local variables
  Real(Kind=jprb) :: zf, zf_sq, z34_dif, z45_dif, z1_sq, z2_sq, z1_div, z2_div
  Real(Kind=jprb) :: z1_den, z2_den, zastar, z1_prod, z2_prod, z3_prod, z4_prod
  Real(Kind=jprb) :: zbstar, zbstar_sq, za2star, za2star_sq, zdiv, zgstar
  Real(Kind=jprb) :: z1f_sq_z1_sq, z2f_sq_z2_sq

  Real(Kind=jprb) :: z34_dif_tl, z45_dif_tl, z1_sq_tl, z2_sq_tl, z1_div_tl, z2_div_tl
  Real(Kind=jprb) :: z1_den_tl, z2_den_tl, zastar_tl, z1_prod_tl, z2_prod_tl, z3_prod_tl, z4_prod_tl
  Real(Kind=jprb) :: zbstar_tl, zbstar_sq_tl, za2star_tl, za2star_sq_tl, zdiv_tl, zgstar_tl
  Real(Kind=jprb) :: z1f_sq_z1_sq_tl, z2f_sq_z2_sq_tl
  Integer(Kind=jpim) :: ii

!- End of header --------------------------------------------------------

  !-----------------------------------------
  !1. calculate layer gaseous optical depths
  !-----------------------------------------

  !--------------------------
  !1.1 start with mixed gases
  !--------------------------

  Do j = 1,  nfrequencies
     chan = channels(j)
     prof = lprofiles(j)
     Do lev=1,nlevels
       opticaldepth_tl(lev,j)=0
     End Do
     Do ii=2,coef % nmixed
       Do lev=1,nlevels
        opticaldepth_tl(lev,j)= opticaldepth_tl(lev,j) + &
         & coef%mixedgas(lev,chan,ii) * predictors_tl( prof ) % mixedgas(ii,lev) 
       End Do
     End Do
  End Do

  !--------------------
  !1.2 add water vapour
  !--------------------

  Do j = 1,  nfrequencies
     chan = channels(j)
     prof = lprofiles(j)
     Do ii=1,coef % nwater
       Do lev=1,nlevels
        opticaldepth_tl(lev,j)= opticaldepth_tl(lev,j) + &
         & coef%watervapour(lev,chan,ii) * predictors_tl( prof ) % watervapour(ii,lev) 
       End Do
     End Do
  End Do

  !-------------
  !1.3 add ozone
  !-------------

  If ( coef%nozone > 0 ) Then
     Do j = 1,  nfrequencies
        chan = channels(j)
        prof = lprofiles(j)
        Do ii=1,coef % nozone
          Do lev=1,nlevels
           opticaldepth_tl(lev,j)= opticaldepth_tl(lev,j) + &
            & coef%ozone(lev,chan,ii) * predictors_tl( prof ) % ozone(ii,lev) 
          End Do
        End Do
     End Do
  End If

  !------------------------------
  !1.4 add Water Vapour Continuum
  !------------------------------

  If ( coef%nwvcont > 0 ) Then
     Do j = 1,  nfrequencies
        chan = channels(j)
        prof = lprofiles(j)
        Do ii=1,coef % nwvcont
          Do lev=1,nlevels
           opticaldepth_tl(lev,j)= opticaldepth_tl(lev,j) + &
                 & coef%wvcont(lev,chan,ii) * predictors_tl( prof ) % wvcont(ii,lev) 
          End Do
        End Do
     End Do
  End If

  !-----------
  !1.5 add CO2
  !-----------

  If ( coef%nco2 > 0 ) Then
     Do j = 1,  nfrequencies
        chan = channels(j)
        prof = lprofiles(j)
        Do ii=1,coef % nco2
          Do lev=1,nlevels
           opticaldepth_tl(lev,j)= opticaldepth_tl(lev,j) + &
                & coef%co2(lev,chan,ii) * predictors_tl( prof ) % co2(ii,lev) 
          End Do
        End Do
     End Do
  End If

  !--------------------
  !1.6 add liquid water (MW only)
  !--------------------
  If ( coef % id_sensor == sensor_id_mw ) Then
     Do j = 1,  nfrequencies
        chan = channels(j)
        prof = lprofiles(j)
        debye_prof    => aux(prof) % debye_prof
        debye_prof_tl => aux_tl(prof) % debye_prof
        If( predictors(prof) % ncloud >= 1 ) Then
           Do lev = mwcldtop, nlevels

              ! Repeat direct code
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


              ! Now compute tangent-linear code
              !zf_tl    = 0
              !zf_sq_tl = 0
              z1_sq_tl       = 2.0_JPRB * debye_prof(1,lev) * debye_prof_tl(1,lev)
              z2_sq_tl       = 2.0_JPRB * debye_prof(2,lev) * debye_prof_tl(2,lev)
              z34_dif_tl     = debye_prof_tl(3,lev) - debye_prof_tl(4,lev)
              z45_dif_tl     = debye_prof_tl(4,lev) - debye_prof_tl(5,lev)
              z1f_sq_z1_sq_tl  = z1_sq_tl
              z2f_sq_z2_sq_tl  = z2_sq_tl
              z1_div_tl      = -z1f_sq_z1_sq_tl / (z1f_sq_z1_sq * z1f_sq_z1_sq)
              z2_div_tl      = -z2f_sq_z2_sq_tl / (z2f_sq_z2_sq * z2f_sq_z2_sq)
              z1_den_tl      = z34_dif * z1_div_tl + z34_dif_tl * z1_div
              z2_den_tl      = z45_dif * z2_div_tl + z45_dif_tl * z2_div
              zastar_tl      = debye_prof_tl(3,lev) - zf_sq * (z1_den_tl + z2_den_tl)

              z1_prod_tl     = z34_dif_tl * debye_prof(1,lev)&
                    & + z34_dif*debye_prof_tl(1,lev) 
              z2_prod_tl     = z1_prod_tl * z1_div + z1_prod * z1_div_tl
              z3_prod_tl     = z45_dif_tl * debye_prof(2,lev)&
                    & + z45_dif * debye_prof_tl(2,lev) 
              z4_prod_tl     = z3_prod_tl * z2_div + z3_prod * z2_div_tl
              zbstar_tl      = -zf * (z2_prod_tl + z4_prod_tl)
              zbstar_sq_tl   = 2.0_JPRB * zbstar * zbstar_tl
              za2star_tl     = zastar_tl
              za2star_sq_tl  = 2.0_JPRB * za2star * za2star_tl
              zdiv_tl        = za2star_sq_tl + zbstar_sq_tl
              zgstar_tl      = -3.0_JPRB*(zbstar_tl * zdiv - zbstar * zdiv_tl) / (zdiv * zdiv)

              opticaldepth_tl(lev,j)= opticaldepth_tl(lev,j) &
                    & - 1.5_JPRB * zf * ( zgstar_tl * predictors    ( prof ) % clw(lev) + &
                                   & zgstar    * predictors_tl ( prof ) % clw(lev) ) 

           End Do
        Endif
     End Do
  End If


  !----------------------------------------
  !2. Compute layer to space optical depths
  !----------------------------------------
  ! notes: apply gamma correction; check value is sensible and constrain
  ! if necessary.

  ! note that optical depth in the calculations is negative
  Where( opdp_ref(:,:) > 0.0_JPRB )
     opticaldepth_tl = 0.0_JPRB
  End Where


  Do j = 1, nchannels
     freq       = polarisations(j,2)               ! Frequency index
     kpol       = 1 + j - polarisations(freq,1)    ! Polarisation index
     prof       = lprofiles(freq)       ! Profile index
     If (kpol <= 2) Then
      tau_layer_freq(:,freq) = transmission % tau_layer(:,j)      
        tau_surf_freq(freq)    = transmission % tau_surf(j)
     End If
  End Do   

  Do j = 1, nfrequencies
     chan = channels(j)
     opticaldepth_tl(:,j) = coef%ff_gam(chan) * opticaldepth_tl(:,j)
  End Do
  od_singlelayer_freq_tl(:,:) = - opticaldepth_tl(:,:)
  od_layer_tl(1,:) = opticaldepth_tl(1,:)
  Do lev = 2, nlevels
     od_layer_tl(lev,:) = od_layer_tl(lev-1,:) + opticaldepth_tl(lev,:)
  End Do

  !-------------------------------------------
  !3. Convert optical depths to transmittances
  !-------------------------------------------
  tau_layer_freq_tl(:,:) = od_layer_tl(:,:) * tau_layer_freq(:,:)

  !-----------------------------------------------------
  !4. Compute optical depth and transmittance at surface
  !-----------------------------------------------------

  Do j = 1, nfrequencies
     prof       = lprofiles(j)
     chan       = polarisations(j, 1)
     od_surf_tl(j) = od_layer_tl(aux(prof) % nearestlev_surf,j) &
           & +  aux_tl(prof) % pfraction_surf *                &
           & (  od_layer(aux(prof) % nearestlev_surf-1,chan) -    &
              & od_layer(aux(prof) % nearestlev_surf  ,chan)    ) &
           & +  aux(prof) % pfraction_surf *                   &
           & (  od_layer_tl(aux(prof) % nearestlev_surf-1,j) - &
              & od_layer_tl(aux(prof) % nearestlev_surf  ,j) ) 
  End Do
  tau_surf_freq_tl(:) = od_surf_tl(:) * tau_surf_freq(:)

  !-----------------------------------------------------
  !5. Store transmittances for other polarisations
  !-----------------------------------------------------

  Do j = 1, nchannels
    freq    = polarisations(j,2)    
    transmission_tl % tau_layer(:,j) = tau_layer_freq_tl(:,freq)
    transmission_tl % od_singlelayer(:,j) = od_singlelayer_freq_tl(:,freq)
    transmission_tl % tau_surf(j) = tau_surf_freq_tl(freq) 
  End Do 

End Subroutine rttov_transmit_tl

