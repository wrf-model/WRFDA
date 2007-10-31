!
Subroutine rttov_setpredictors_8_k( &
     & nfrequencies,  &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & nlevels,       &! in
     & angles,        &! in
     & polarisations, &! in
     & lprofiles,     &! in
     & profiles,      &! in
     & profiles_k,    &! inout
     & coef,          &! in
     & predictors,    &! in
     & predictors_k )  ! inout
  ! Description
  ! RTTOV-8 Model
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
  !    Copyright 2005, EUMETSAT, All Rights Reserved.
  !
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       22/06/2005  initial (P Brunel)
  !                        based on version 1.4 (29/03/05) of AD code
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Parameters:
  Use rttov_const, Only :   &
   &   gravity    ,&
   &   sensor_id_mw

  ! Imported Type Definitions:
  Use rttov_types, Only : &
   &   rttov_coef     ,&
   &   profile_Type   ,&
   &   geometry_Type  ,&
   &   predictors_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),    Intent(in)    :: nfrequencies            ! Number of frequencies
  Integer(Kind=jpim),    Intent(in)    :: nchannels               ! Number of output radiances
  Integer(Kind=jpim),    Intent(in)    :: nprofiles               ! Number of profiles
  Integer(Kind=jpim),    Intent(in)    :: nlevels                 ! Number of levels 
  Integer(Kind=jpim),    Intent(in)    :: polarisations(nchannels,3) ! polarisation indices
  Integer(Kind=jpim),    Intent(in)    :: lprofiles(nfrequencies)    ! Profiles indices

  Type(profile_Type),    Target, Intent(in)     :: profiles(nprofiles)
  Type(profile_Type),    Target, Intent(inout)  :: profiles_k(nchannels)
  Type(geometry_Type),   Target, Intent(in)     :: angles(nprofiles)  
  Type(predictors_Type), Target, Intent(in)     :: predictors(nprofiles)
  Type(predictors_Type), Target, Intent(inout)  :: predictors_k(nchannels)
  Type(rttov_coef),              Intent(in)     :: coef

  !local variables:

  Type(geometry_Type),   Pointer  :: geom
  Type(profile_Type),    Pointer  :: prof
  Type(profile_Type),    Pointer  :: prof_k
  Type(predictors_Type), Pointer  :: pred
  Type(predictors_Type), Pointer  :: pred_k

  Integer(Kind=jpim) :: level
  Integer(Kind=jpim) :: freq
  Integer(Kind=jpim) :: i          ! channel indice
  Integer(Kind=jpim) :: j          ! profile indice

  ! user profile
  Real(Kind=Jprb) :: t(nlevels, nprofiles)
  Real(Kind=Jprb) :: w(nlevels, nprofiles)
  Real(Kind=Jprb) :: o(nlevels, nprofiles)
  Real(Kind=Jprb) :: co2(nlevels, nprofiles)

  ! reference profile
  Real(Kind=Jprb) :: tr(nlevels, nprofiles)
  Real(Kind=Jprb) :: wr(nlevels, nprofiles)
  Real(Kind=Jprb) :: wwr(nlevels, nprofiles)

  ! user - reference
  Real(Kind=Jprb) :: dt(nlevels, nprofiles)
  Real(Kind=Jprb) :: dtabs(nlevels, nprofiles)

  ! pressure weighted
  Real(Kind=Jprb) :: tw(nlevels, nprofiles)

  ! intermediate variables
  Real(Kind=Jprb) :: sum1,sum2
  Real(Kind=Jprb) :: deltac(nlevels)
  Real(Kind=Jprb) :: sum2_ww(nlevels, nprofiles)
  Real(Kind=Jprb) :: sum2_wwr(nlevels, nprofiles)
  Real(Kind=Jprb) :: sum2_ow(nlevels, nprofiles)
  Real(Kind=Jprb) :: sum2_twr(nlevels, nprofiles)
  Real(Kind=Jprb) :: sum2_co2w(nlevels, nprofiles)
  Real(Kind=Jprb) :: tr_sq(nlevels, nprofiles)
  Real(Kind=Jprb) :: tr_sqrt(nlevels, nprofiles)
  Real(Kind=Jprb) :: tr_4(nlevels, nprofiles)

  ! K variables
  Real(Kind=Jprb) :: t_k(nlevels, nchannels)
  Real(Kind=Jprb) :: w_k(nlevels, nchannels)
  Real(Kind=Jprb) :: o_k(nlevels, nchannels)
  Real(Kind=Jprb) :: co2_k(nlevels, nchannels)

  Real(Kind=Jprb) :: tr_k(nlevels, nchannels)
  Real(Kind=Jprb) :: wr_k(nlevels, nchannels)
  Real(Kind=Jprb) :: or_k(nlevels, nchannels)
  Real(Kind=Jprb) :: wwr_k(nlevels, nchannels)
  Real(Kind=Jprb) :: co2r_k(nlevels, nchannels)
  Real(Kind=Jprb) :: twr_k(nlevels, nchannels)

  Real(Kind=Jprb) :: dt_k(nlevels, nchannels)
  Real(Kind=Jprb) :: dto_k(nlevels, nchannels)

  Real(Kind=Jprb) :: tw_k(nlevels, nchannels)
  Real(Kind=Jprb) :: ww_k(nlevels, nchannels)
  Real(Kind=Jprb) :: ow_k(nlevels, nchannels)
  Real(Kind=Jprb) :: co2w_k(nlevels, nchannels)

  Real(Kind=Jprb) :: sec_or_k(nlevels, nchannels)
  Real(Kind=Jprb) :: sec_wr_k(nlevels, nchannels)
  Real(Kind=Jprb) :: sec_wrwr_k(nlevels, nchannels)

  !- End of header --------------------------------------------------------

  nullify ( geom )
  nullify ( prof )
  nullify ( prof_k )
  nullify ( pred )
  nullify ( pred_k )

  !-------------------------------------------------------------------------------
  ! Recompute Direct variables
  !-------------------------------------------------------------------------------
  Do j = 1, nprofiles

     prof => profiles(j)

     !1) Profile layer quantities
     !-------------------------------------------------------------------------------
     t(1, j) = prof % t(1)
     t(2 : prof % nlevels, j) = ( prof % t(1 : prof % nlevels-1) + &
          &   prof % t(2 : prof % nlevels  ) ) / 2._JPRB

     w(1, j) = prof % q(1)
     w(2 : prof % nlevels, j) = ( prof % q(1 : prof % nlevels-1) + &
          &   prof % q(2 : prof % nlevels  ) ) / 2._JPRB

     If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
        o(1, j) = prof % o3(1)
        o(2 : prof % nlevels, j) = ( prof % o3(1 : prof % nlevels-1) + &
             &   prof % o3(2 : prof % nlevels  ) ) / 2._JPRB
     Else
        o( : , j ) = 0._JPRB
     Endif

     If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
        co2(1, j) = prof % co2(1)
        co2(2 : prof % nlevels, j) = ( prof % co2(1 : prof % nlevels-1) + &
             &   prof % co2(2 : prof % nlevels  ) ) / 2._JPRB
     Endif
     !------------------------------------------------------------------------------
     !2) calculate deviations from reference profile (layers)
     !------------------------------------------------------------------------------
     dt(:, j)  = t(:,j) - coef % tstar(:)
     dtabs(:,j) = Abs(dt(:,j))
     !------------------------------------------------------------------------------
     !3) calculate (profile / reference profile) ratios; tr wr or
     ! if no input O3 profile, set to reference value (or =1)
     !------------------------------------------------------------------------------
     tr(:,j) = t(:,j) / coef % tstar(:)
     tr_sq(:,j) = tr(:,j) * tr(:,j)
     tr_4(:,j) = tr_sq(:,j) * tr_sq(:,j)
     tr_sqrt(:,j) = Sqrt(tr(:,j))
     wr(:,j) = w(:,j) / coef % wstar(:)
     !-------------------------------------------------------------------
     ! 4. calculate profile / reference profile sums: tw wwr
     !--------------------------------------------------------------------
     tw(1,j) = 0._JPRB
     Do level = 2 , prof % nlevels
        tw( level, j) = tw( level-1, j) + coef % dpp( level ) * tr ( level -1, j)
     End Do
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     Do level = 1, prof % nlevels
        sum1 = sum1 + coef % dpp( level ) *      w       ( level, j)  *  t ( level, j)
        sum2 = sum2 + coef % dpp( level ) * coef % wstar ( level )  * coef % tstar ( level )
        sum2_wwr ( level, j) = sum2
        wwr(level, j) = sum1 / sum2
     End Do
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     Do level = 1, prof % nlevels
        sum1 = sum1 + coef % dpp( level ) *      w       ( level, j)
        sum2 = sum2 + coef % dpp( level ) * coef % wstar ( level )
        sum2_ww( level, j) = sum2
     End Do

     If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
        sum1 = 0._JPRB
        sum2 = 0._JPRB
        Do level = 1, prof % nlevels
           sum1 = sum1 + coef % dpp( level ) *      o       ( level, j)
           sum2 = sum2 + coef % dpp( level ) * coef % ostar ( level )
           sum2_ow( level, j) = sum2
        End Do
     Endif

     If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
        sum1 = 0._JPRB
        sum2 = 0._JPRB
        Do level = 1, prof % nlevels
           sum1 = sum1 + coef % dpp( level ) *     co2        ( level, j)
           sum2 = sum2 + coef % dpp( level ) * coef % co2star ( level )
           sum2_co2w ( level, j) = sum2
        End Do
        sum1 = 0._JPRB
        sum2 = 0._JPRB
        sum2_twr ( 1, j) =  0._JPRB
        Do level = 2, prof % nlevels
           sum1 = sum1 + coef % dpp( level ) *     t        ( level-1, j)
           sum2 = sum2 + coef % dpp( level ) * coef % tstar ( level-1 )
           sum2_twr ( level, j) =  sum2
        End Do
     Endif

  End Do

  !-------------------------------------------------------------------------
  ! Ajoint code
  !-------------------------------------------------------------------------
  w_k(:,:)       = 0._JPRB
  wr_k(:,:)      = 0._JPRB
  ww_k(:,:)      = 0._JPRB
  wwr_k(:,:)     = 0._JPRB
  sec_wr_k(:,:)  = 0._JPRB
  sec_wrwr_k(:,:)= 0._JPRB
  dt_k(:,:)      = 0._JPRB
  t_k(:,:)       = 0._JPRB
  tr_k(:,:)      = 0._JPRB
  tw_k(:,:)      = 0._JPRB

  !5.6 CO2
  !-------
  If ( coef % nco2 > 0 ) Then
     co2r_k(:,:)    = 0._JPRB
     co2w_k(:,:)    = 0._JPRB
     twr_k(:,:)     = 0._JPRB
     co2_k(:,:)     = 0._JPRB

     Do i = 1, nchannels
        freq=polarisations(i,2)
        j = lprofiles( freq )

        prof   => profiles(j) ! for test on CO2 data
        geom   => angles(j)
        pred   => predictors(j)
        ! prof_k => profiles_k(i)
        pred_k => predictors_k(i)

        If ( prof % co2_Data ) Then
           !
           twr_k(:,i) = twr_k(:,i) + pred_k % co2(10,:) * geom%seczen * tr_sqrt(:,j)
           tr_k(:,i)  = tr_k(:,i) + pred_k % co2(10,:) * 0.5_JPRB *  pred % co2(7,:) / tr_sqrt(:,j)
           twr_k(:,i) = twr_k(:,i) + pred_k % co2(9,:) * 3 * geom%seczen * pred % co2(9,:) &
                &  /  pred % co2(7,:)
           co2w_k(:,i) = co2w_k(:,i) + 2 * geom%seczen * pred_k % co2(8,:) * Sqrt(pred % co2(8,:))
           twr_k(:,i)  = twr_k(:,i) + pred_k % co2(7,:) * geom%seczen
           pred_k % co2(6,:) = 0._JPRB
           tr_k(:,i) = tr_k(:,i) + pred_k % co2(5,:)
           tr_k(:,i) = tr_k(:,i) + 2 * pred_k % co2(4,:) * pred % co2(3,:)
           tr_k(:,i) = tr_k(:,i) + pred_k % co2(3,:) * geom%seczen
           tr_k(:,i) = tr_k(:,i) + 2 * pred_k % co2(2,:) * pred % co2(5,:)
           co2r_k(:,i) = co2r_k(:,i) + pred_k % co2(1,:) * geom%seczen
        Endif ! CO2 data
     End Do ! channels
  End If ! coefs CO2

  !5.5 cloud
  !---------
  If ( coef % id_sensor == sensor_id_mw ) Then
     deltac(:) = 0.1820_JPRB * 100.0_JPRB * coef % dp(:) / (4.3429_JPRB * gravity)

     Do i = 1, nchannels
        freq=polarisations(i,2)
        j = lprofiles( freq )

        prof   => profiles(j)

        If ( prof % clw_Data ) Then

           geom   => angles(j)
           prof_k => profiles_k(i)
           pred_k => predictors_k(i)

           prof_k%clw(1:prof_k % nlevels-1) = prof_k%clw(1:prof_k % nlevels-1) +&
                & 0.5_JPRB * pred_k % clw(2:prof_k % nlevels) *&
                & deltac(2:prof_k % nlevels) * geom%seczen

           pred_k % clw(2:prof_k % nlevels) = 0.5_JPRB *  pred_k % clw(2:prof_k % nlevels)

           prof_k%clw(:) = prof_k%clw(:)  + pred_k % clw(:) *&
                & deltac(:) * geom%seczen

        End If
     End Do
  End If

  !5.4 ozone
  !---------
  If ( coef % nozone > 0 ) Then
     o_k(:,:)      = 0._JPRB
     or_k(:,:)     = 0._JPRB
     wr_k(:,:)     = 0._JPRB
     ow_k(:,:)     = 0._JPRB
     dto_k(:,:)    = 0._JPRB
     sec_or_k(:,:) = 0._JPRB

     Do i = 1, nchannels
        freq=polarisations(i,2)
        j = lprofiles( freq )

        ! prof   => profiles(j)      (use of variables derived from prof )
        geom   => angles(j)
        pred   => predictors(j)
        ! prof_k => profiles_k(i)     (not used for O3 )
        pred_k => predictors_k(i)

        ! One can pack all ow_k lines in one longer statement
        ! same for sec_or_k and dto_k
        ow_k(:,i) = ow_k(:,i) + pred_k % ozone(11,:) *&
             &  2 * geom%seczen * pred % ozone(10,:)

        ow_k(:,i) = ow_k(:,i) + pred_k % ozone(10,:) * geom%seczen

        sec_or_k(:,i) = sec_or_k(:,i) + pred_k % ozone(9,:) *&
             & Sqrt(pred % ozone(10,:))
        ow_k(:,i)    = ow_k(:,i)      + pred_k % ozone(9,:) *&
             & 0.5_JPRB * geom%seczen * pred % ozone(1,:) &
             &    / Sqrt(pred % ozone(10,:))

        ow_k(:,i) = ow_k(:,i) + pred_k % ozone(8,:) *&
             & pred % ozone(1,:)
        or_k(:,i) = or_k(:,i) + pred_k % ozone(8,:) *&
             & pred % ozone(10,:)

        sec_or_k(:,i) = sec_or_k(:,i) + pred_k % ozone(7,:) *&
             & 1.5_JPRB * pred % ozone(2,:) / pred % ozone(10,:)
        ow_k(:,i)     = ow_k(:,i)     - pred_k % ozone(7,:) *&
             & geom%seczen * pred % ozone(2,:)**3 / pred % ozone(10,:)**2


        or_k(:,i) =  or_k(:,i) + pred_k % ozone(6,:) *&
             & 2 * pred % ozone(8,:)
        ow_k(:,i) = ow_k(:,i)  + pred_k % ozone(6,:) *&
             & pred % ozone(4,:) / geom%seczen

        sec_or_k(:,i) = sec_or_k(:,i) + pred_k % ozone(5,:) *&
             & 0.5_JPRB * pred % ozone(3,:) /&
             &     ( pred % ozone(1,:) *pred % ozone(2,:))
        dto_k(:,i) = dto_k(:,i)       + pred_k % ozone(5,:) *&
             & pred % ozone(2,:)

        sec_or_k(:,i) = sec_or_k(:,i) + pred_k % ozone(4,:) *&
             & 2 * pred % ozone(1,:)

        sec_or_k(:,i) = sec_or_k(:,i) + pred_k % ozone(3,:) *&
             & pred % ozone(3,:) / pred % ozone(1,:)
        dto_k(:,i)    = dto_k(:,i)    + pred_k % ozone(3,:) *&
             & pred % ozone(1,:)

        sec_or_k(:,i) = sec_or_k(:,i) + pred_k % ozone(2,:) *&
             & 0.5_JPRB / pred % ozone(2,:)

        sec_or_k(:,i) = sec_or_k(:,i) + pred_k % ozone(1,:)

        or_k(:,i) = or_k(:,i) + sec_or_k(:,i) * geom%seczen

     End Do ! channels
  Endif ! Coef O3

  !5.3 Water Vapour Continuum based on RTIASI
  !------------------------------------------
  If ( coef % nwvcont > 0 ) Then

     Do i = 1, nchannels
        freq=polarisations(i,2)
        j = lprofiles( freq )

        ! prof   => profiles(j)   (use of variables derived from prof )
        ! geom   => angles(j)
        pred   => predictors(j)
        ! prof_k => profiles_k(i)
        pred_k => predictors_k(i)

        sec_wr_k(:,i) = sec_wr_k(:,i) + pred_k % wvcont(4,:) / tr_sq(:,j)
        tr_k(:,i) = tr_k(:,i) - 2 * pred_k % wvcont(4,:)* &
             & pred % watervapour(7,:) / (tr_sq(:,j) * tr(:,j))
        sec_wr_k(:,i) = sec_wr_k(:,i) + pred_k % wvcont(3,:) / tr(:,j)
        tr_k(:,i) = tr_k(:,i) - pred_k % wvcont(3,:) * &
             & pred % watervapour(7,:) / tr_sq(:,j)
        sec_wrwr_k(:,i) = sec_wrwr_k(:,i) + pred_k % wvcont(2,:) / tr_4(:,j)
        tr_k(:,i) = tr_k(:,i) - 4 * pred_k % wvcont(2,:) * &
             & pred % wvcont(1,:) / tr_4(:,j)
        sec_wrwr_k(:,i) = sec_wrwr_k(:,i) + pred_k % wvcont(1,:) / tr(:,j)
        tr_k(:,i) = tr_k(:,i) - pred_k % wvcont(1,:) * &
             & pred % wvcont(1,:) / tr(:,j)
     End Do ! channels
  Endif ! Coefs WV Cont
  !
  !5.2 water vapour based on RTIASI
  !--------------------------------
  Do i = 1, nchannels
     freq=polarisations(i,2)
     j = lprofiles( freq )

     ! prof   => profiles(j)    (use of variables derived from prof )
     geom   => angles(j)
     pred   => predictors(j)
     ! prof_k => profiles_k(i)
     pred_k => predictors_k(i)


     wr_k(:,i) = wr_k(:,i) + 1.5_JPRB * pred_k % watervapour(12,:) *&
          & pred % watervapour(5,:) / wwr(:,j)

     wwr_k(:,i) = wwr_k(:,i) - pred_k % watervapour(12,:) *&
          & pred % watervapour(5,:) * wr(:,j) / (wwr(:,j) * wwr(:,j))

     wr_k(:,i) = wr_k(:,i) + 2 * pred_k % watervapour(11,:) *&
          & pred % watervapour(7,:) / wwr(:,j)

     wwr_k(:,i) = wwr_k(:,i) - pred_k % watervapour(11,:) *&
          & pred % watervapour(1,:) / (geom%seczen * wwr(:,j) * wwr(:,j))

     dt_k(:,i) = dt_k(:,i) + pred_k % watervapour(10,:) *&
          & pred % watervapour(5,:)

     sec_wr_k(:,i) = sec_wr_k(:,i) + 0.5_JPRB * pred_k % watervapour(10,:) *&
          & dt(:,j) / pred % watervapour(5,:)

     sec_wr_k(:,i) = sec_wr_k(:,i) + pred_k % watervapour(9,:) * dtabs(:,j) * dt(:,j)
     dt_k(:,i) = dt_k(:,i) + 2 * pred_k % watervapour(9,:) *&
          &                    pred % watervapour(7,:) * dtabs(:,j)

     sec_wr_k(:,i) = sec_wr_k(:,i) + 3 * pred_k % watervapour(8,:) *&
          &  pred % watervapour(1,:)

     sec_wr_k(:,i) = sec_wr_k(:,i) + pred_k % watervapour(7,:)

     sec_wr_k(:,i) = sec_wr_k(:,i) + 0.25_JPRB * pred_k % watervapour(6,:) /&
          & pred % watervapour(6,:)**3

     sec_wr_k(:,i) = sec_wr_k(:,i) + 0.5_JPRB * pred_k % watervapour(5,:) /&
          & pred % watervapour(5,:)

     dt_k(:,i) = dt_k(:,i) + pred_k % watervapour(4,:) *&
          & pred % watervapour(7,:)

     sec_wr_k(:,i) = sec_wr_k(:,i) + pred_k % watervapour(4,:) * dt(:,j)

     ww_k(:,i) = ww_k(:,i) + 2 * pred_k % watervapour(3,:) * &
          & geom%seczen * pred % watervapour(2,:)

     ww_k(:,i) = ww_k(:,i) + pred_k % watervapour(2,:) * geom%seczen

     sec_wr_k(:,i) = sec_wr_k(:,i) + 2 * pred_k % watervapour(1,:) * &
          &  pred % watervapour(7,:)

     sec_wr_k(:,i) = sec_wr_k(:,i) + sec_wrwr_k(:,i)* wr(:,j)
     wr_k(:,i) = wr_k(:,i) + sec_wrwr_k(:,i) * pred % watervapour(7,:)
     wr_k(:,i) = wr_k(:,i) + sec_wr_k(:,i) * geom % seczen
  End Do ! channels

  !5.1 mixed gases
  !---------------

  Do i = 1, nchannels
     freq=polarisations(i,2)
     j = lprofiles( freq )

     ! prof   => profiles(j)   (use of variables derived from prof )
     geom   => angles(j)
     pred   => predictors(j)
     prof_k => profiles_k(i)
     pred_k => predictors_k(i)

     ! X10
     tw_k(2:prof_k % nlevels,i) = tw_k(2:prof_k % nlevels,i) +&
          & pred_k % mixedgas(10,2:prof_k % nlevels) *&
          &  0.25_JPRB * geom % seczen_sq &
          & / pred % mixedgas(10,2:prof_k % nlevels)**3
     ! X9
     ! X8
     tw_k(:,i) = tw_k(:,i) + pred_k % mixedgas(8,:) *&
          & geom % seczen / pred % mixedgas(5,:)
     tr_k(:,i) = tr_k(:,i) - pred_k % mixedgas(8,:) *&
          & pred % mixedgas(7,:) / pred % mixedgas(6,:)
     ! X7
     tw_k(:,i) = tw_k(:,i) + pred_k % mixedgas(7,:) *&
          &  geom % seczen
     ! X6
     tr_k(:,i) = tr_k(:,i) + pred_k % mixedgas(6,:) *&
          &  2._JPRB * tr(:,j)
     ! X5
     tr_k(:,i) = tr_k(:,i) + pred_k % mixedgas(5,:)
     ! X4
     tr_k(:,i) = tr_k(:,i) + pred_k % mixedgas(4,:) *&
          & 2._JPRB * pred % mixedgas(3,:)
     ! X3
     tr_k(:,i) = tr_k(:,i) + pred_k % mixedgas(3,:) *&
          & geom % seczen
     ! X2
     ! X1
  End Do ! channels

  Do i = 1, nchannels
     freq=polarisations(i,2)
     j = lprofiles( freq )

     prof   => profiles(j)
     geom   => angles(j)
     pred   => predictors(j)
     prof_k => profiles_k(i)
     pred_k => predictors_k(i)
     !-------------------------------------------------------------------
     !   calc adjoint of profile/reference sums
     !-------------------------------------------------------------------
     If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
        sum1 = 0._JPRB
        Do level = prof_k % nlevels, 1 , -1
           sum1 = sum1 + co2w_k(level, i) / sum2_co2w(level, j)
           co2_k( level, i) = co2_k( level, i) + sum1 * coef % dpp( level )
        End Do
        sum1 = 0._JPRB
        Do level = prof_k % nlevels , 2, -1
           sum1 = sum1 + twr_k(level,i) / sum2_twr(level,j)
           t_k( level-1, i) = t_k( level-1, i) + sum1 * coef % dpp( level )
        End Do
     Else
        t_k(:,i) = 0._JPRB
        co2_k(:,i) = 0._JPRB
     Endif
     !
     sum1 = 0._JPRB
     If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
        Do level = prof_k % nlevels, 1, -1
           sum1 = sum1 + ow_k ( level, i) / sum2_ow(level, j)
           o_k( level, i) = o_k( level, i) + sum1 * coef % dpp( level )
        End Do
     Else
        o_k(:,i) = 0._JPRB
     Endif
     !
     sum1 = 0._JPRB
     Do level = prof_k % nlevels, 1, -1
        sum1 = sum1 + wwr_k ( level, i) / sum2_wwr(level, j)
        w_k( level, i) = w_k( level, i) + sum1 * coef % dpp( level ) * t( level, j)
        t_k( level, i) = t_k( level, i) + sum1 * coef % dpp( level ) * w( level, j)
     End Do
     !
     sum1 = 0._JPRB
     Do level = prof_k % nlevels, 1, -1
        sum1 = sum1 + ww_k ( level, i) / sum2_ww(level, j)
        w_k( level, i) = w_k( level, i) + sum1 * coef % dpp( level )
     End Do

     Do level = prof_k % nlevels, 2, -1
        tw_k( level-1, i) = tw_k( level-1, i) + tw_k( level, i)
        tr_k( level-1, i) = tr_k( level-1, i) + tw_k( level, i) *&
             & coef % dpp( level )
     End Do
     !-------------------------------------------------------------------
     !   calc adjoint of profile deviations
     !-------------------------------------------------------------------
     If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
        co2_k(:,i) = co2_k(:,i) + co2r_k(:,i) / coef % co2star(:)
     Endif

     If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
        o_k(:,i) = o_k(:,i) + or_k(:,i) / coef % ostar(:)
     Endif

     w_k(:,i) = w_k(:,i) + wr_k(:,i) / coef % wstar(:)

     t_k(:,i) = t_k(:,i) + tr_k(:,i) / coef % tstar(:)


     If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
        t_k(:,i) = t_k(:,i) + dto_k(:,i)
     Endif

     t_k(:,i) = t_k(:,i) + dt_k(:,i)
     !-------------------------------------------------------------------
     !   calc adjoint of profile layer means
     !-------------------------------------------------------------------
     If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
        prof_k % co2(1 : prof_k % nlevels-1) = prof_k % co2(1 : prof_k % nlevels-1) +&
             & 0.5_JPRB *co2_k(2 : prof_k % nlevels, i)
        prof_k % co2(2 : prof_k % nlevels)   = prof_k % co2(2 : prof_k % nlevels) +&
             & 0.5_JPRB *co2_k(2 : prof_k % nlevels, i)
        prof_k % co2(1) = prof_k % co2(1) + co2_k(1,i)
     Endif

     If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
        prof_k % o3(1 : prof_k % nlevels-1) = prof_k % o3(1 : prof_k % nlevels-1) +&
             & 0.5_JPRB *o_k(2 : prof_k % nlevels, i)
        prof_k % o3(2 : prof_k % nlevels)   = prof_k % o3(2 : prof_k % nlevels) +&
             & 0.5_JPRB *o_k(2 : prof_k % nlevels, i)
        prof_k % o3(1) = prof_k % o3(1) + o_k(1,i)
     Endif

     prof_k % q(1 : prof_k % nlevels-1) = prof_k % q(1 : prof_k % nlevels-1) +&
          & 0.5_JPRB *w_k(2 : prof_k % nlevels, i)
     prof_k % q(2 : prof_k % nlevels)   = prof_k % q(2 : prof_k % nlevels) +&
          & 0.5_JPRB *w_k(2 : prof_k % nlevels, i)
     prof_k % q(1) = prof_k % q(1) + w_k(1,i)

     prof_k % t(1 : prof_k % nlevels-1) = prof_k % t(1 : prof_k % nlevels-1) +&
          & 0.5_JPRB *t_k(2 : prof_k % nlevels, i)
     prof_k % t(2 : prof_k % nlevels)   = prof_k % t(2 : prof_k % nlevels) +&
          & 0.5_JPRB *t_k(2 : prof_k % nlevels, i)
     prof_k % t(1) = prof_k % t(1) + t_k(1,i)

  End Do ! channels

  nullify ( geom )
  nullify ( prof )
  nullify ( prof_k )
  nullify ( pred )
  nullify ( pred_k )

End Subroutine rttov_setpredictors_8_k
