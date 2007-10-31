Subroutine rttov_transmit_k( &
     & nfrequencies,   &! in
     & nchannels,      &! in
     & nprofiles,      &! in
     & nlevels,        &! in
     & polarisations,  &! in
     & channels,       &! in
     & lprofiles,      &! in
     & predictors,     &! in
     & predictors_k,   &! inout
     & aux,            &! in
     & aux_k,          &! inout
     & coef,           &! in
     & od_layer,       &! in
     & opdp_ref,       &! in
     & transmission,   &! in
     & transmission_k ) ! inout

! History:
! Version   Date     Comment
! -------   ----     -------
!  1.0
!  1.1    26/09/2003  Modified to allow for multiple polarisations (S English)
!  1.2    29/03/2005  Add end of header comment (J. Cameron)

! Adjoint variables
! input transmission_k% tau_surf and transmission_k% tau_layer set inside integrate_k
!
! input/output aux_k
!
! output predictors_k initialised inside rttov_k (need input
!    intent for memory allocation in calling routine)
!

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
  Integer(Kind=jpim),  Intent(in)    :: nlevels
  Integer(Kind=jpim),  Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),  Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),  Intent(in)    :: lprofiles(nfrequencies)
  Type(predictors_Type), Intent(in)    :: predictors( nprofiles )
  Type(predictors_Type), Intent(inout) :: predictors_k( nchannels )
  Type(rttov_coef),      Intent(in)    :: coef
  Type(profile_aux),     Intent(in)    :: aux( nprofiles )
  Type(profile_aux),     Intent(inout) :: aux_k( nchannels )
  Real(Kind=jprb),       Intent(in)    :: od_layer(nlevels,nchannels)
  Real(Kind=jprb),       Intent(in)    :: opdp_ref(nlevels,nfrequencies)
  Type(transmission_Type),Intent(in)   :: transmission
  Type(transmission_Type),Intent(inout):: transmission_k



  !local variables:
  Real(Kind=jprb) :: od_layer_k(nlevels,nchannels)
  Real(Kind=jprb) :: opticaldepth_k(nlevels,nchannels)
  Real(Kind=jprb) :: od_surf_k(nchannels)
  Real(Kind=jprb) :: opdp_ref_chan(nlevels,nchannels)
  Real(Kind=jprb),  Pointer :: debye_prof(:,:)
  Real(Kind=jprb),  Pointer :: debye_prof_k(:,:)
  Integer(Kind=jpim) :: lev,chan,j,freq
  Integer(Kind=jpim) :: prof, kpol


  ! cloud liquid water local variables
  Real(Kind=jprb) :: zf, zf_sq, z34_dif, z45_dif, z1_sq, z2_sq, z1_div, z2_div
  Real(Kind=jprb) :: z1_den, z2_den, zastar, z1_prod, z2_prod, z3_prod, z4_prod
  Real(Kind=jprb) :: zbstar, zbstar_sq, za2star, za2star_sq, zdiv, zgstar
  Real(Kind=jprb) :: z1f_sq_z1_sq, z2f_sq_z2_sq

  Real(Kind=jprb) :: z34_dif_k, z45_dif_k, z1_sq_k, z2_sq_k, z1_div_k, z2_div_k
  Real(Kind=jprb) :: z1_den_k, z2_den_k, zastar_k, z1_prod_k, z2_prod_k, z3_prod_k, z4_prod_k
  Real(Kind=jprb) :: zbstar_k, zbstar_sq_k, za2star_k, za2star_sq_k, zdiv_k, zgstar_k
  Real(Kind=jprb) :: z1f_sq_z1_sq_k, z2f_sq_z2_sq_k

!- End of header --------------------------------------------------------

  od_layer_k(:,:)     = 0._JPRB
  opticaldepth_k(:,:) = 0._JPRB

  !-----------------------------------------------------
  !4. Compute optical depth and transmittance at surface
  !-----------------------------------------------------

  od_surf_k(:) =  0.0_JPRB
  od_surf_k(:) =  transmission % tau_surf(:) * transmission_k% tau_surf(:)
  Do j = 1, nchannels
     freq=polarisations(j,2)
     prof       = lprofiles(freq)
     od_layer_k(aux(prof) % nearestlev_surf,j) = od_layer_k(aux(prof) % nearestlev_surf,j) +&
           & od_surf_k(j) * (1._JPRB - aux(prof) % pfraction_surf )
     od_layer_k(aux(prof) % nearestlev_surf-1,j) = od_layer_k(aux(prof) % nearestlev_surf-1,j) +&
           & od_surf_k(j) * aux(prof) % pfraction_surf
     aux_k(j) % pfraction_surf = aux_k(j) % pfraction_surf + od_surf_k(j) *&
           & (  od_layer(aux(prof) % nearestlev_surf-1,j) -    &
              & od_layer(aux(prof) % nearestlev_surf  ,j) )
     od_surf_k(j) = 0._JPRB
     opdp_ref_chan(:,j)=opdp_ref(:, freq)
  End Do

  !-------------------------------------------
  !3. Convert optical depths to transmittances
  !-------------------------------------------

  od_layer_k(:,:) = od_layer_k(:,:) + transmission_k% tau_layer(:,:) * transmission % tau_layer(:,:)
  !transmission_k% tau_layer(:,:) = 0.

  !----------------------------------------
  !2. Compute layer to space optical depths
  !----------------------------------------
  ! notes: apply gamma correction; check value is sensible and constrain
  ! if necessary.

  Do lev = nlevels, 2, -1
     od_layer_k(lev-1,:)   = od_layer_k(lev-1,:)    + od_layer_k(lev,:)
     opticaldepth_k(lev,:) = opticaldepth_k(lev,:)  + od_layer_k(lev,:)
     od_layer_k(lev,:)     = 0._JPRB
  End Do

  opticaldepth_k(1,:) = opticaldepth_k(1,:) + od_layer_k(1,:)

  opticaldepth_k(:,:) = opticaldepth_k(:,:) - transmission_k % od_singlelayer(:,:)
  transmission_k % od_singlelayer(:,:) = 0._JPRB
  Do j = 1, nchannels
     freq=polarisations(j,2)
     chan = channels(freq)
     opticaldepth_k(:,j) = coef%ff_gam(chan) * opticaldepth_k(:,j)
  End Do

  ! note that optical depth in the calculations is negative
  Where( opdp_ref_chan(:,:) > 0.0_JPRB )
     opticaldepth_k = 0.0_JPRB
  End Where

  !--------------------
  !1.6 add liquid water (MW only)
  !--------------------
  If ( coef % id_sensor == sensor_id_mw ) Then
     Do j = 1, nchannels
        freq = polarisations(j,2)
        chan = channels(freq)
        prof = lprofiles(freq)
        debye_prof    => aux(prof) % debye_prof
        debye_prof_k => aux_k(j) % debye_prof
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


              ! Now compute Adjoint code
              !opticaldepth_k(lev,j)= opticaldepth_k(lev,j)
              zgstar_k = opticaldepth_k(lev,j) *&
                    & (-1.5_JPRB * zf * predictors ( prof ) % clw(lev))
              predictors_k ( j ) % clw(lev) = predictors_k ( j ) % clw(lev) +&
                    & opticaldepth_k(lev,j) * (-1.5_JPRB * zf * zgstar)

              zbstar_k = -3.0_JPRB * zgstar_k / zdiv
              zdiv_k   =  3.0_JPRB * zgstar_k * zbstar / (zdiv*zdiv)
              !zgstar_k =  0.

              za2star_sq_k = zdiv_k
              zbstar_sq_k  = zdiv_k
              !zdiv_k       = 0.

              za2star_k    = 2.0_JPRB * za2star * za2star_sq_k
              !za2star_sq_k = 0.

              zastar_k     = za2star_k
              !za2star_k    = 0.

              zbstar_k     = zbstar_k + 2.0_JPRB * zbstar * zbstar_sq_k
              !zbstar_sq_k  = 0.

              z2_prod_k    = -zf * zbstar_k
              z4_prod_k    = -zf * zbstar_k
              !zbstar_k     = 0.

              z3_prod_k    = z2_div  * z4_prod_k
              z2_div_k     = z3_prod * z4_prod_k
              !z4_prod_k    = 0.

              z45_dif_k           = debye_prof(2,lev)             * z3_prod_k
              debye_prof_k(2,lev) = debye_prof_k(2,lev) + z45_dif* z3_prod_k
              !z3_prod_k           = 0.

              z1_prod_k    = z1_div  * z2_prod_k
              z1_div_k     = z1_prod * z2_prod_k
              !z2_prod_k    = 0.

              z34_dif_k           = debye_prof(1,lev)              * z1_prod_k
              debye_prof_k(1,lev) = debye_prof_k(1,lev) + z34_dif * z1_prod_k
              !z1_prod_k           = 0.

              debye_prof_k(3,lev) = debye_prof_k(3,lev) + zastar_k
              z1_den_k            =              -zf_sq  * zastar_k
              z2_den_k            =              -zf_sq  * zastar_k
              !zastar_k            = 0.

              z2_div_k     = z2_div_k  + z45_dif * z2_den_k
              z45_dif_k    = z45_dif_k + z2_div  * z2_den_k
              !z2_den_k     = 0.

              z1_div_k     = z1_div_k  + z34_dif * z1_den_k
              z34_dif_k    = z34_dif_k + z1_div  * z1_den_k
              !z1_den_k     = 0.

              z2f_sq_z2_sq_k = -z2_div_k / (z2f_sq_z2_sq * z2f_sq_z2_sq)
              !z2_div_k       = 0.

              z1f_sq_z1_sq_k = -z1_div_k / (z1f_sq_z1_sq * z1f_sq_z1_sq)
              !z1_div_k       = 0.

              z2_sq_k        = z2f_sq_z2_sq_k
              !z2f_sq_z2_sq_k = 0.

              z1_sq_k        = z1f_sq_z1_sq_k
              !z1f_sq_z1_sq_k = 0.

              debye_prof_k(4,lev) = debye_prof_k(4,lev) + z45_dif_k
              debye_prof_k(5,lev) = debye_prof_k(5,lev) - z45_dif_k
              !z45_dif_k           = 0.

              debye_prof_k(3,lev) = debye_prof_k(3,lev) + z34_dif_k
              debye_prof_k(4,lev) = debye_prof_k(4,lev) - z34_dif_k
              !z34_dif_k           = 0.

              debye_prof_k(2,lev) = debye_prof_k(2,lev) + z2_sq_k *&
                    & 2.0_JPRB * debye_prof(2,lev)
              !z2_sq_k             = 0.

              debye_prof_k(1,lev) = debye_prof_k(1,lev) + z1_sq_k *&
                    & 2.0_JPRB * debye_prof(1,lev)
              !z1_sq_k             = 0.

           End Do
        Endif
     End Do
  End If

  !-----------
  !1.5 add CO2
  !-----------

  If ( coef%nco2 > 0 ) Then
     Do j = 1, nchannels
        freq=polarisations(j,2)
        chan = channels(freq)
        Do lev=nlevels, 1, -1

           predictors_k( j ) % co2(:,lev) =&
                    & predictors_k( j ) % co2(:,lev) +&
                    & coef%co2(lev,chan,:) * opticaldepth_k(lev,j)

        End Do
     End Do
  End If

  !------------------------------
  !1.4 add Water Vapour Continuum
  !------------------------------

  If ( coef%nwvcont > 0 ) Then
     Do j = 1, nchannels
        freq=polarisations(j,2)
        chan = channels(freq)
        Do lev=nlevels, 1, -1

           predictors_k( j ) % wvcont(:,lev) =&
                    & predictors_k( j ) % wvcont(:,lev) +&
                    & coef%wvcont(lev,chan,:) * opticaldepth_k(lev,j)

        End Do
     End Do
  End If

  !-------------
  !1.3 add ozone
  !-------------

  If ( coef%nozone > 0 ) Then
     Do j = 1, nchannels
        freq=polarisations(j,2)
        chan = channels(freq)
        Do lev=nlevels, 1, -1

           predictors_k( j ) % ozone(:,lev) =&
                    & predictors_k( j ) % ozone(:,lev) +&
                    & coef%ozone(lev,chan,:) * opticaldepth_k(lev,j)

        End Do
     End Do
  End If

  !--------------------
  !1.2 add water vapour
  !--------------------

  Do j = 1, nchannels
     freq=polarisations(j,2)
     chan = channels(freq)
     Do lev=nlevels, 1, -1

        predictors_k( j ) % watervapour(:,lev) =&
                 & predictors_k( j ) % watervapour(:,lev) +   &
                 & coef%watervapour(lev,chan,:) * opticaldepth_k(lev,j)
     End Do
  End Do

  !--------------------------
  !1.1 start with mixed gases
  !--------------------------

  Do j = 1, nchannels
     freq=polarisations(j,2)
     chan = channels(freq)
     Do lev=nlevels, 1, -1

        predictors_k( j ) % mixedgas(:,lev) =&
                 & predictors_k( j ) % mixedgas(:,lev) +&
                 & coef%mixedgas(lev,chan,:) * opticaldepth_k(lev,j)
     End Do
  End Do

End Subroutine rttov_transmit_k
