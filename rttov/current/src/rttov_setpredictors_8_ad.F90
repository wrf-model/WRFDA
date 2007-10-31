!
Subroutine rttov_setpredictors_8_ad( &
     prof,       & ! in
     prof_ad,    & ! inout
     geom,       & ! in
     coef,       & ! in
     predictors, & ! in
     predictors_ad ) ! inout
  ! Description
  ! RTTOV-8 Model
  ! AD of rttov_setpredictors_8
  ! To calculate and store the profile variables (predictors) required
  ! in subsequent transmittance calculations.
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
  ! see RTTOV7 science and validation report pages 18/19
  ! variable names are close to the documentation
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0   29/01/2003  Original - copy of RTTOV7 model (P Brunel)
  !                               as a template for RTTOV-8
  !  1.1   30/09/2003  Added predictors for wv line and continuum and CO2 (R Saunders)
  !  1.2   03/06/2004  Parkind parametrisation, correction of w_ad, t_ad calculation
  !                    simplify AD relatted to predictor 9 for WVL (P. Brunel)
  !  1.3   23/02/2005  Correction of Twr definition (P. Brunel)
  !  1.4   29/03/2005  Add end of header comment (J. Cameron)
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
  Type(profile_Type),    Intent(in)     :: prof
  Type(profile_Type),    Intent(inout)  :: prof_ad
  Type(rttov_coef),      Intent(in)     :: coef
  Type(geometry_Type),   Intent(in)     :: geom
  Type(predictors_Type), Intent(in)     :: predictors
  Type(predictors_Type), Intent(inout)  :: predictors_ad

  !local variables:
  Integer(Kind=jpim) :: level
  ! user profile
  Real(Kind=Jprb) :: t(prof % nlevels)
  Real(Kind=Jprb) :: w(prof % nlevels)
  Real(Kind=Jprb) :: o(prof % nlevels)
  Real(Kind=Jprb) :: co2(prof % nlevels)

  ! reference profile
  Real(Kind=Jprb) :: tr(prof % nlevels)
  Real(Kind=Jprb) :: wr(prof % nlevels)
  Real(Kind=Jprb) :: wwr(prof % nlevels)

  ! user - reference
  Real(Kind=Jprb) :: dt(prof % nlevels)
  Real(Kind=Jprb) :: dtabs(prof % nlevels)

  ! pressure weighted
  Real(Kind=Jprb) :: tw(prof % nlevels)

  ! intermediate variables
  Real(Kind=Jprb) :: sum1,sum2
  Real(Kind=Jprb) :: deltac(prof %nlevels)
  Real(Kind=Jprb) :: sum2_ww(prof %nlevels)
  Real(Kind=Jprb) :: sum2_wwr(prof %nlevels)
  Real(Kind=Jprb) :: sum2_ow(prof %nlevels)
  Real(Kind=Jprb) :: sum2_twr(prof %nlevels)
  Real(Kind=Jprb) :: sum2_co2w(prof %nlevels)
  Real(Kind=Jprb) :: tr_sq(prof % nlevels)
  Real(Kind=Jprb) :: tr_sqrt(prof % nlevels)
  Real(Kind=Jprb) :: tr_4(prof % nlevels)

  ! AD variables
  Real(Kind=Jprb) :: t_ad(prof % nlevels)
  Real(Kind=Jprb) :: w_ad(prof % nlevels)
  Real(Kind=Jprb) :: o_ad(prof % nlevels)
  Real(Kind=Jprb) :: co2_ad(prof % nlevels)

  Real(Kind=Jprb) :: tr_ad(prof % nlevels)
  Real(Kind=Jprb) :: wr_ad(prof % nlevels)
  Real(Kind=Jprb) :: or_ad(prof % nlevels)
  Real(Kind=Jprb) :: wwr_ad(prof % nlevels)
  Real(Kind=Jprb) :: co2r_ad(prof % nlevels)
  Real(Kind=Jprb) :: twr_ad(prof % nlevels)

  Real(Kind=Jprb) :: dt_ad(prof % nlevels)
  Real(Kind=Jprb) :: dto_ad(prof % nlevels)

  Real(Kind=Jprb) :: tw_ad(prof % nlevels)
  Real(Kind=Jprb) :: ww_ad(prof % nlevels)
  Real(Kind=Jprb) :: ow_ad(prof % nlevels)
  Real(Kind=Jprb) :: co2w_ad(prof % nlevels)

  Real(Kind=Jprb) :: sec_or_ad(prof %nlevels)
  Real(Kind=Jprb) :: sec_wr_ad(prof %nlevels)
  Real(Kind=Jprb) :: sec_wrwr_ad(prof %nlevels)

  !- End of header --------------------------------------------------------

  !-------------------------------------------------------------------------------
  ! Recompute Direct variables
  !-------------------------------------------------------------------------------
  !1) Profile layer quantities
  !-------------------------------------------------------------------------------
  t(1) = prof % t(1)
  t(2 : prof % nlevels ) = ( prof % t(1 : prof % nlevels-1) + &
       &   prof % t(2 : prof % nlevels  ) ) / 2._JPRB

  w(1) = prof % q(1)
  w(2 : prof % nlevels ) = ( prof % q(1 : prof % nlevels-1) + &
       &   prof % q(2 : prof % nlevels  ) ) / 2._JPRB

  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     o(1) = prof % o3(1)
     o(2 : prof % nlevels ) = ( prof % o3(1 : prof % nlevels-1) + &
          &   prof % o3(2 : prof % nlevels  ) ) / 2._JPRB
  Endif

  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
     co2(1) = prof % co2(1)
     co2(2 : prof % nlevels ) = ( prof % co2(1 : prof % nlevels-1) + &
          &   prof % co2(2 : prof % nlevels  ) ) / 2._JPRB
  Endif
  !------------------------------------------------------------------------------
  !2) calculate deviations from reference profile (layers)
  !------------------------------------------------------------------------------
  dt(:)  = t(:) - coef % tstar(:)
  dtabs(:) = Abs(dt(:))
  !------------------------------------------------------------------------------
  !3) calculate (profile / reference profile) ratios; tr wr or
  ! if no input O3 profile, set to reference value (or =1)
  !------------------------------------------------------------------------------
  tr(:) = t(:) / coef % tstar(:)
  tr_sq(:) = tr(:) * tr(:)
  tr_4(:) = tr_sq(:) * tr_sq(:)
  tr_sqrt(:) = Sqrt(tr(:))
  wr(:) = w(:) / coef % wstar(:)
  !-------------------------------------------------------------------
  ! 4. calculate profile / reference profile sums: tw wwr
  !--------------------------------------------------------------------
  tw(1) = 0.
  Do level = 2 , prof % nlevels
     tw( level ) = tw( level-1 ) + coef % dpp( level ) * tr ( level -1 )
  End Do
  sum1 = 0._JPRB
  sum2 = 0._JPRB
  Do level = 1, prof % nlevels
     sum1 = sum1 + coef % dpp( level ) *      w       ( level )  *  t ( level )
     sum2 = sum2 + coef % dpp( level ) * coef % wstar ( level )  * coef % tstar ( level )
     sum2_wwr ( level ) = sum2
     wwr(level) = sum1 / sum2
  End Do
  sum1 = 0._JPRB
  sum2 = 0._JPRB
  Do level = 1, prof % nlevels
     sum1 = sum1 + coef % dpp( level ) *      w       ( level )
     sum2 = sum2 + coef % dpp( level ) * coef % wstar ( level )
     sum2_ww( level ) = sum2
  End Do

  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     Do level = 1, prof % nlevels
        sum1 = sum1 + coef % dpp( level ) *      o       ( level )
        sum2 = sum2 + coef % dpp( level ) * coef % ostar ( level )
        sum2_ow( level ) = sum2
     End Do
  Endif

  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     Do level = 1, prof % nlevels
        sum1 = sum1 + coef % dpp( level ) *     co2        ( level )
        sum2 = sum2 + coef % dpp( level ) * coef % co2star ( level )
        sum2_co2w ( level ) = sum2
     End Do
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     sum2_twr ( 1 ) =  0._JPRB
     Do level = 2, prof % nlevels
        sum1 = sum1 + coef % dpp( level ) *     t        ( level-1 )
        sum2 = sum2 + coef % dpp( level ) * coef % tstar ( level-1 )
        sum2_twr ( level ) =  sum2
     End Do
  Endif
  !-------------------------------------------------------------------------
  ! Ajoint code
  !-------------------------------------------------------------------------
  w_ad(:)       = 0._JPRB
  wr_ad(:)      = 0._JPRB
  ww_ad(:)      = 0._JPRB
  wwr_ad(:)     = 0._JPRB
  sec_wr_ad(:)  = 0._JPRB
  sec_wrwr_ad(:)= 0._JPRB
  dt_ad(:)      = 0._JPRB
  t_ad(:)       = 0._JPRB
  tr_ad(:)      = 0._JPRB
  tw_ad(:)      = 0._JPRB

  !5.6 CO2
  !-------
  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
    co2r_ad(:)    = 0._JPRB
    co2w_ad(:)    = 0._JPRB
    twr_ad(:)     = 0._JPRB
    co2_ad(:)     = 0._JPRB
  !
    twr_ad(:) = twr_ad(:) + predictors_ad % co2(10,:) * geom%seczen * tr_sqrt(:)
    tr_ad(:) = tr_ad(:) + predictors_ad % co2(10,:) * 0.5_JPRB *  predictors % co2(7,:) / tr_sqrt(:)
    twr_ad(:) = twr_ad(:) + predictors_ad % co2(9,:) * 3 * geom%seczen * predictors % co2(9,:) &
         &  /  predictors % co2(7,:)
    co2w_ad(:) = co2w_ad(:) + 2 * geom%seczen * predictors_ad % co2(8,:) * Sqrt(predictors % co2(8,:))
    twr_ad(:) = twr_ad(:) + predictors_ad % co2(7,:) * geom%seczen
    predictors_ad % co2(6,:) = 0._JPRB
    tr_ad(:) = tr_ad(:) + predictors_ad % co2(5,:)
    tr_ad(:) = tr_ad(:) + 2 * predictors_ad % co2(4,:) * predictors % co2(3,:)
    tr_ad(:) = tr_ad(:) + predictors_ad % co2(3,:) * geom%seczen
    tr_ad(:) = tr_ad(:) + 2 * predictors_ad % co2(2,:) * predictors % co2(5,:)
    co2r_ad(:) = co2r_ad(:) + predictors_ad % co2(1,:) * geom%seczen
  Endif

  !5.5 cloud
  !---------
  If ( prof % clw_Data .And. coef % id_sensor == sensor_id_mw ) Then
     deltac(:) = 0.1820_JPRB * 100.0_JPRB * coef % dp(:) / (4.3429_JPRB * gravity)

     prof_ad%clw(1:prof_ad % nlevels-1) = prof_ad%clw(1:prof_ad % nlevels-1) +&
          & 0.5_JPRB * predictors_ad % clw(2:prof_ad % nlevels) *&
          & deltac(2:prof_ad % nlevels) * geom%seczen

     predictors_ad % clw(2:prof_ad % nlevels) = 0.5_JPRB *  predictors_ad % clw(2:prof_ad % nlevels)

     prof_ad%clw(:) = prof_ad%clw(:)  + predictors_ad % clw(:) *&
          & deltac(:) * geom%seczen

  Endif

  !5.4 ozone
  !---------
  If ( coef % nozone > 0 ) Then
     o_ad(:)      = 0._JPRB
     or_ad(:)     = 0._JPRB
     wr_ad(:)     = 0._JPRB
     ow_ad(:)     = 0._JPRB
     dto_ad(:)    = 0._JPRB
     sec_or_ad(:) = 0._JPRB

     ! One can pack all ow_ad lines in one longer statement
     ! same for sec_or_ad and dto_ad
     ow_ad(:) = ow_ad(:) + predictors_ad % ozone(11,:) *&
          &  2 * geom%seczen * predictors % ozone(10,:)

     ow_ad(:) = ow_ad(:) + predictors_ad % ozone(10,:) * geom%seczen

     sec_or_ad(:) = sec_or_ad(:) + predictors_ad % ozone(9,:) *&
          & Sqrt(predictors % ozone(10,:))
     ow_ad(:)    = ow_ad(:)      + predictors_ad % ozone(9,:) *&
          & 0.5_JPRB * geom%seczen * predictors % ozone(1,:) &
          &    / Sqrt(predictors % ozone(10,:))

     ow_ad(:) = ow_ad(:) + predictors_ad % ozone(8,:) *&
          & predictors % ozone(1,:)
     or_ad(:) = or_ad(:) + predictors_ad % ozone(8,:) *&
          & predictors % ozone(10,:)

     sec_or_ad(:) = sec_or_ad(:) + predictors_ad % ozone(7,:) *&
          & 1.5_JPRB * predictors % ozone(2,:) / predictors % ozone(10,:)
     ow_ad(:)     = ow_ad(:)     - predictors_ad % ozone(7,:) *&
          & geom%seczen * predictors % ozone(2,:)**3 / predictors % ozone(10,:)**2


     or_ad(:) =  or_ad(:) + predictors_ad % ozone(6,:) *&
          & 2 * predictors % ozone(8,:)
     ow_ad(:) = ow_ad(:)  + predictors_ad % ozone(6,:) *&
          & predictors % ozone(4,:) / geom%seczen

     sec_or_ad(:) = sec_or_ad(:) + predictors_ad % ozone(5,:) *&
          & 0.5_JPRB * predictors % ozone(3,:) /&
          &     ( predictors % ozone(1,:) *predictors % ozone(2,:))
     dto_ad(:) = dto_ad(:)       + predictors_ad % ozone(5,:) *&
          & predictors % ozone(2,:)

     sec_or_ad(:) = sec_or_ad(:) + predictors_ad % ozone(4,:) *&
          & 2 * predictors % ozone(1,:)

     sec_or_ad(:) = sec_or_ad(:) + predictors_ad % ozone(3,:) *&
          & predictors % ozone(3,:) / predictors % ozone(1,:)
     dto_ad(:)    = dto_ad(:)    + predictors_ad % ozone(3,:) *&
          & predictors % ozone(1,:)

     sec_or_ad(:) = sec_or_ad(:) + predictors_ad % ozone(2,:) *&
          & 0.5_JPRB / predictors % ozone(2,:)

     sec_or_ad(:) = sec_or_ad(:) + predictors_ad % ozone(1,:)

     or_ad(:) = or_ad(:) + sec_or_ad(:) * geom%seczen

  Endif

  !5.3 Water Vapour Continuum based on RTIASI
  !------------------------------------------
  If ( coef % nwvcont > 0 ) Then
    sec_wr_ad(:) = sec_wr_ad(:) + predictors_ad % wvcont(4,:) / tr_sq(:)
    tr_ad(:) = tr_ad(:) - 2 * predictors_ad % wvcont(4,:)* &
          & predictors % watervapour(7,:) / (tr_sq(:) * tr(:))
    sec_wr_ad(:) = sec_wr_ad(:) + predictors_ad % wvcont(3,:) / tr(:)
    tr_ad(:) = tr_ad(:) - predictors_ad % wvcont(3,:) * &
          & predictors % watervapour(7,:) / tr_sq(:)
    sec_wrwr_ad(:) = sec_wrwr_ad(:) + predictors_ad % wvcont(2,:) / tr_4(:)
    tr_ad(:) = tr_ad(:) - 4 * predictors_ad % wvcont(2,:) * &
          & predictors % wvcont(1,:) / tr_4(:)
    sec_wrwr_ad(:) = sec_wrwr_ad(:) + predictors_ad % wvcont(1,:) / tr(:)
    tr_ad(:) = tr_ad(:) - predictors_ad % wvcont(1,:) * &
          & predictors % wvcont(1,:) / tr(:)
  Endif
  !
  !5.2 water vapour based on RTIASI
  !--------------------------------
  wr_ad(:) = wr_ad(:) + 1.5_JPRB * predictors_ad % watervapour(12,:) *&
       & predictors % watervapour(5,:) / wwr(:)

  wwr_ad(:) = wwr_ad(:) - predictors_ad % watervapour(12,:) *&
    & predictors % watervapour(5,:) * wr / (wwr(:) * wwr(:))

  wr_ad(:) = wr_ad(:) + 2 * predictors_ad % watervapour(11,:) *&
       & predictors % watervapour(7,:) / wwr(:)

  wwr_ad(:) = wwr_ad(:) - predictors_ad % watervapour(11,:) *&
    & predictors % watervapour(1,:) / (geom%seczen * wwr(:) * wwr(:))

  dt_ad(:) = dt_ad(:) + predictors_ad % watervapour(10,:) *&
    & predictors % watervapour(5,:)

  sec_wr_ad(:) = sec_wr_ad(:) + 0.5_JPRB * predictors_ad % watervapour(10,:) *&
       & dt(:) / predictors % watervapour(5,:)

  sec_wr_ad(:) = sec_wr_ad(:) + predictors_ad % watervapour(9,:) * dtabs(:) * dt(:)
  dt_ad(:) = dt_ad(:) + 2 * predictors_ad % watervapour(9,:) *&
       &                    predictors % watervapour(7,:) * dtabs(:)

  sec_wr_ad(:) = sec_wr_ad(:) + 3 * predictors_ad % watervapour(8,:) *&
       &  predictors % watervapour(1,:)

  sec_wr_ad(:) = sec_wr_ad(:) + predictors_ad % watervapour(7,:)

  sec_wr_ad(:) = sec_wr_ad(:) + 0.25_JPRB * predictors_ad % watervapour(6,:) /&
       & predictors % watervapour(6,:)**3

  sec_wr_ad(:) = sec_wr_ad(:) + 0.5_JPRB * predictors_ad % watervapour(5,:) /&
       & predictors % watervapour(5,:)

  dt_ad(:) = dt_ad(:) + predictors_ad % watervapour(4,:) *&
    & predictors % watervapour(7,:)

  sec_wr_ad(:) = sec_wr_ad(:) + predictors_ad % watervapour(4,:) * dt(:)

  ww_ad(:) = ww_ad(:) + 2 * predictors_ad % watervapour(3,:) * &
       & geom%seczen * predictors % watervapour(2,:)

  ww_ad(:) = ww_ad(:) + predictors_ad % watervapour(2,:) * geom%seczen

  sec_wr_ad(:) = sec_wr_ad(:) + 2 * predictors_ad % watervapour(1,:) * &
       &  predictors % watervapour(7,:)

  sec_wr_ad(:) = sec_wr_ad(:) + sec_wrwr_ad(:)* wr(:)
  wr_ad(:) = wr_ad(:) + sec_wrwr_ad(:) * predictors % watervapour(7,:)
  wr_ad(:) = wr_ad(:) + sec_wr_ad(:) * geom % seczen

  !5.1 mixed gases
  !---------------


  ! X10
  tw_ad(2:prof_ad % nlevels) = tw_ad(2:prof_ad % nlevels) +&
       & predictors_ad % mixedgas(10,2:prof_ad % nlevels) *&
       &  0.25_JPRB * geom % seczen_sq &
       & / predictors % mixedgas(10,2:prof_ad % nlevels)**3
  ! X9
  ! X8
  tw_ad(:) = tw_ad(:) + predictors_ad % mixedgas(8,:) *&
       & geom % seczen / predictors % mixedgas(5,:)
  tr_ad(:) = tr_ad(:) - predictors_ad % mixedgas(8,:) *&
       & predictors % mixedgas(7,:) / predictors % mixedgas(6,:)
  ! X7
  tw_ad(:) = tw_ad(:) + predictors_ad % mixedgas(7,:) *&
       &  geom % seczen
  ! X6
  tr_ad(:) = tr_ad(:) + predictors_ad % mixedgas(6,:) *&
       &  2._JPRB * tr(:)
  ! X5
  tr_ad(:) = tr_ad(:) + predictors_ad % mixedgas(5,:)
  ! X4
  tr_ad(:) = tr_ad(:) + predictors_ad % mixedgas(4,:) *&
       & 2._JPRB * predictors % mixedgas(3,:)
  ! X3
  tr_ad(:) = tr_ad(:) + predictors_ad % mixedgas(3,:) *&
       & geom % seczen
  ! X2
  ! X1
  !-------------------------------------------------------------------
  !   calc adjoint of profile/reference sums
  !-------------------------------------------------------------------
  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
     sum1 = 0._JPRB
     Do level = prof_ad % nlevels, 1 , -1
        sum1 = sum1 + co2w_ad(level) / sum2_co2w(level)
        co2_ad( level ) = co2_ad( level ) + sum1 * coef % dpp( level )
     End Do
     sum1 = 0._JPRB
     Do level = prof_ad % nlevels , 2, -1
        sum1 = sum1 + twr_ad(level) / sum2_twr(level)
        t_ad( level-1 ) = t_ad( level-1 ) + sum1 * coef % dpp( level )
     End Do
  Else
    t_ad(:) = 0._JPRB
    co2_ad(:) = 0._JPRB
  Endif
 !
  sum1 = 0._JPRB
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     Do level = prof_ad % nlevels, 1, -1
        sum1 = sum1 + ow_ad ( level ) / sum2_ow(level)
        o_ad( level ) = o_ad( level ) + sum1 * coef % dpp( level )
     End Do
  Else
     o_ad(:) = 0._JPRB
  Endif
 !
  sum1 = 0._JPRB
  Do level = prof_ad % nlevels, 1, -1
     sum1 = sum1 + wwr_ad ( level ) / sum2_wwr(level)
     w_ad( level ) = w_ad( level ) + sum1 * coef % dpp( level ) * t( level )
     t_ad( level ) = t_ad( level ) + sum1 * coef % dpp( level ) * w( level )
  End Do
 !
  sum1 = 0._JPRB
  Do level = prof_ad % nlevels, 1, -1
     sum1 = sum1 + ww_ad ( level ) / sum2_ww(level)
     w_ad( level ) = w_ad( level ) + sum1 * coef % dpp( level )
  End Do

  Do level = prof_ad % nlevels, 2, -1
     tw_ad( level-1 ) = tw_ad( level-1 ) + tw_ad( level )
     tr_ad( level-1 ) = tr_ad( level-1 ) + tw_ad( level ) *&
          & coef % dpp( level )
  End Do
  !-------------------------------------------------------------------
  !   calc adjoint of profile deviations
  !-------------------------------------------------------------------
  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
     co2_ad(:) = co2_ad(:) + co2r_ad(:) / coef % co2star(:)
  Endif

  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     o_ad(:) = o_ad(:) + or_ad(:) / coef % ostar(:)
  Endif

  w_ad(:) = w_ad(:) + wr_ad(:) / coef % wstar(:)

  t_ad(:) = t_ad(:) + tr_ad(:) / coef % tstar(:)


  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     t_ad(:) = t_ad(:) + dto_ad(:)
  Endif

  t_ad(:) = t_ad(:) + dt_ad(:)
  !-------------------------------------------------------------------
  !   calc adjoint of profile layer means
  !-------------------------------------------------------------------
  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
     prof_ad % co2(1 : prof_ad % nlevels-1) = prof_ad % co2(1 : prof_ad % nlevels-1) +&
          & 0.5_JPRB *co2_ad(2 : prof_ad % nlevels )
     prof_ad % co2(2 : prof_ad % nlevels)   = prof_ad % co2(2 : prof_ad % nlevels) +&
          & 0.5_JPRB *co2_ad(2 : prof_ad % nlevels )
     prof_ad % co2(1) = prof_ad % co2(1) + co2_ad(1)
  Endif

  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     prof_ad % o3(1 : prof_ad % nlevels-1) = prof_ad % o3(1 : prof_ad % nlevels-1) +&
          & 0.5_JPRB *o_ad(2 : prof_ad % nlevels )
     prof_ad % o3(2 : prof_ad % nlevels)   = prof_ad % o3(2 : prof_ad % nlevels) +&
          & 0.5_JPRB *o_ad(2 : prof_ad % nlevels )
     prof_ad % o3(1) = prof_ad % o3(1) + o_ad(1)
  Endif

  prof_ad % q(1 : prof_ad % nlevels-1) = prof_ad % q(1 : prof_ad % nlevels-1) +&
       & 0.5_JPRB *w_ad(2 : prof_ad % nlevels )
  prof_ad % q(2 : prof_ad % nlevels)   = prof_ad % q(2 : prof_ad % nlevels) +&
       & 0.5_JPRB *w_ad(2 : prof_ad % nlevels )
  prof_ad % q(1) = prof_ad % q(1) + w_ad(1)

  prof_ad % t(1 : prof_ad % nlevels-1) = prof_ad % t(1 : prof_ad % nlevels-1) +&
       & 0.5_JPRB *t_ad(2 : prof_ad % nlevels )
  prof_ad % t(2 : prof_ad % nlevels)   = prof_ad % t(2 : prof_ad % nlevels) +&
       & 0.5_JPRB *t_ad(2 : prof_ad % nlevels )
  prof_ad % t(1) = prof_ad % t(1) + t_ad(1)

End Subroutine rttov_setpredictors_8_ad
