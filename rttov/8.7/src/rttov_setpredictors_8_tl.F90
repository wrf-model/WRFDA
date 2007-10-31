!
Subroutine rttov_setpredictors_8_tl( &
     prof,       & ! in
     prof_tl,    & ! in
     geom,       & ! in
     coef,       & ! in
     predictors, & ! in
     predictors_tl ) ! inout
  ! Description
  ! RTTOV-8 Model
  ! TL of rttov_setpredictors_8
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
  !  1.1   17/09/2003  Added predictors for wv line and continuum and CO2 (R Saunders)
  !  1.2   03/06/2004  Parkind parametrisation, correction of wwr_tl calculation
  !                    simplify TL of predictor 9 for WVL (P. Brunel)
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
    &  gravity    ,&
    &  sensor_id_mw

  ! Imported Type Definitions:
  Use rttov_types, Only : &
     & rttov_coef     ,&
     & profile_Type   ,&
     & geometry_Type  ,&
     & predictors_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Type(profile_Type),    Intent(in)  :: prof
  Type(profile_Type),    Intent(in)  :: prof_tl
  Type(rttov_coef),      Intent(in)  :: coef
  Type(geometry_Type),   Intent(in)  :: geom
  Type(predictors_Type), Intent(in)  :: predictors
  Type(predictors_Type), Intent(inout) :: predictors_tl ! in because of mem allocation

  !local variables:
  Integer(Kind=jpim) :: level
  ! user profile
  Real(Kind=Jprb) :: t(prof % nlevels)
  Real(Kind=Jprb) :: w(prof % nlevels)

  ! reference profile
  Real(Kind=Jprb) :: tr(prof % nlevels)
  Real(Kind=Jprb) :: wr(prof % nlevels)
  Real(Kind=Jprb) :: wwr(prof % nlevels)
  Real(Kind=Jprb) :: twr(prof % nlevels)

  ! user - reference
  Real(Kind=Jprb) :: dt(prof % nlevels)
  Real(Kind=Jprb) :: dtabs(prof % nlevels)

  ! pressure weighted
  Real(Kind=Jprb) :: tw(prof % nlevels)

  ! intermediate variables
  Real(Kind=Jprb) :: sum1,sum2
  Real(Kind=Jprb) :: deltac(prof %nlevels)
  Real(Kind=Jprb) :: tr_sq(prof % nlevels)
  Real(Kind=Jprb) :: tr_sqrt(prof % nlevels)
  Real(Kind=Jprb) :: tr_4(prof % nlevels)

  ! TL variables
  Real(Kind=Jprb) :: t_tl(prof % nlevels)
  Real(Kind=Jprb) :: w_tl(prof % nlevels)
  Real(Kind=Jprb) :: o_tl(prof % nlevels)
  Real(Kind=Jprb) :: co2_tl(prof % nlevels)

  Real(Kind=Jprb) :: tr_tl(prof % nlevels)
  Real(Kind=Jprb) :: wr_tl(prof % nlevels)
  Real(Kind=Jprb) :: wwr_tl(prof % nlevels)
  Real(Kind=Jprb) :: or_tl(prof % nlevels)
  Real(Kind=Jprb) :: co2r_tl(prof % nlevels)
  Real(Kind=Jprb) :: twr_tl(prof % nlevels)

  Real(Kind=Jprb) :: dt_tl(prof % nlevels)
  Real(Kind=Jprb) :: dto_tl(prof % nlevels)

  Real(Kind=Jprb) :: tw_tl(prof % nlevels)
  Real(Kind=Jprb) :: ww_tl(prof % nlevels)
  Real(Kind=Jprb) :: ow_tl(prof % nlevels)
  Real(Kind=Jprb) :: co2w_tl(prof % nlevels)

  Real(Kind=Jprb) :: sec_or_tl(prof %nlevels)
  Real(Kind=Jprb) :: sec_wr_tl(prof %nlevels)
  Real(Kind=Jprb) :: sec_wrwr_tl(prof %nlevels)

  !- End of header --------------------------------------------------------

  !-------------------------------------------------------------------------------
  ! Recompute direct variables
  !-------------------------------------------------------------------------------
  ! 1) profile layer mean quantities
  !------------------------------------------------------------------------------
  t(1) = prof % t(1)
  t(2 : prof % nlevels ) = ( prof % t(1 : prof % nlevels-1) + &
       &   prof % t(2 : prof % nlevels  ) ) / 2._JPRB

  w(1) = prof % q(1)
  w(2 : prof % nlevels ) = ( prof % q(1 : prof % nlevels-1) + &
       &   prof % q(2 : prof % nlevels  ) ) / 2._JPRB

  ! Direct value of o and co2 NOT needed for TL

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
  wr(:) = w(:) / coef % wstar(:)
  !-------------------------------------------------------------------
  ! 4. calculate profile / reference profile sums: tw wwr twr
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
     wwr ( level ) = sum1 / sum2
  End Do
  ! Direct value of ww NOT needed for TL
  ! Direct value of ow NOT needed for TL
  ! Direct value of co2w NOT needed for TL
  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     twr (1 ) =  0._JPRB
     Do level = 2, prof % nlevels
        sum1 = sum1 + coef % dpp( level ) *     t        ( level-1 )
        sum2 = sum2 + coef % dpp( level ) * coef % tstar ( level-1 )
        twr ( level ) = sum1 / sum2
     End Do
  Else
     twr(:) = 1._JPRB
  Endif
  !-------------------------------------------------------------------------------
  ! Now compute TL variables
  !-------------------------------------------------------------------------------
  ! 1) profile layer mean quantities
  !------------------------------------------------------------------------------
  t_tl(1) = prof_tl % t(1)
  t_tl(2 : prof_tl % nlevels ) = ( prof_tl % t(1 : prof_tl % nlevels-1) + &
       &   prof_tl % t(2 : prof_tl % nlevels  ) ) / 2._JPRB
  w_tl(1) = prof_tl % q(1)
  w_tl(2 : prof_tl % nlevels ) = ( prof_tl % q(1 : prof_tl % nlevels-1) + &
       &   prof_tl % q(2 : prof_tl % nlevels  ) ) / 2._JPRB
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     o_tl(1) = prof_tl % o3(1)
     o_tl(2 : prof_tl % nlevels ) = ( prof_tl % o3(1 : prof_tl % nlevels-1) + &
          &   prof_tl % o3(2 : prof_tl % nlevels  ) ) / 2._JPRB
  Endif
  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
    co2_tl(1) = prof_tl % co2(1)
    co2_tl(2 : prof_tl % nlevels ) = ( prof_tl % co2(1 : prof_tl % nlevels-1) + &
       &   prof_tl % co2(2 : prof_tl % nlevels  ) ) / 2._JPRB
  Endif
  !------------------------------------------------------------------------------
  !2) calculate deviations from reference profile (layers)
  !------------------------------------------------------------------------------
  dt_tl(:)  = t_tl(:)
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     dto_tl(:) = t_tl(:)
  Else
     dto_tl(:) = 0._JPRB
  Endif
  !------------------------------------------------------------------------------
  !3) calculate (profile / reference profile) ratios; tr_tl wr_tl or_tl
  !------------------------------------------------------------------------------
  tr_tl(:) = t_tl(:) / coef % tstar(:)
  wr_tl(:) = w_tl(:) / coef % wstar(:)
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     or_tl(:) = o_tl(:) / coef % ostar(:)
  Else
     or_tl(:) = 0._JPRB
  Endif
  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
     co2r_tl(:) = co2_tl(:) / coef % co2star(:)
  Else
     co2r_tl(:) = 0._JPRB
  Endif
  !-------------------------------------------------------------------------
  ! 4. calculate profile / reference profile sums: tw_tl ww_tl ow_tl co2w_tl
  !-------------------------------------------------------------------------
  tw_tl(1) = 0._JPRB
  Do level = 2 , prof_tl % nlevels
     tw_tl( level ) = tw_tl( level-1 ) + coef % dpp( level ) * tr_tl ( level -1 )
  End Do

  sum1 = 0._JPRB
  sum2 = 0._JPRB
  Do level = 1, prof_tl % nlevels
     sum1 = sum1 + coef % dpp( level ) *      w_tl    ( level )
     sum2 = sum2 + coef % dpp( level ) * coef % wstar ( level )
     ww_tl ( level ) = sum1 / sum2
  End Do
  sum1 = 0._JPRB
  sum2 = 0._JPRB
  Do level = 1, prof_tl % nlevels
     sum1 = sum1 + coef % dpp( level ) * ( w_tl  ( level )  *  t    ( level ) + &
          &                                w     ( level )  *  t_tl ( level ) )
     sum2 = sum2 + coef % dpp( level ) * coef % wstar ( level )  * coef % tstar ( level )
     wwr_tl ( level ) = sum1 / sum2
  End Do

  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     Do level = 1, prof_tl % nlevels
        sum1 = sum1 + coef % dpp( level ) *      o_tl    ( level )
        sum2 = sum2 + coef % dpp( level ) * coef % ostar ( level )
        ow_tl ( level ) = sum1 / sum2
     End Do
  Else
     ow_tl(:) = 0._JPRB
  Endif

  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     Do level = 1, prof_tl % nlevels
        sum1 = sum1 + coef % dpp( level ) *     co2_tl     ( level )
        sum2 = sum2 + coef % dpp( level ) * coef % co2star ( level )
        co2w_tl ( level ) = sum1 / sum2
     End Do
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     twr_tl (1 ) =  0._JPRB
    Do level = 2, prof_tl % nlevels
        sum1 = sum1 + coef % dpp( level ) *     t_tl     ( level-1 )
        sum2 = sum2 + coef % dpp( level ) * coef % tstar ( level-1 )
        twr_tl ( level ) = sum1 / sum2
     End Do
  Else
    co2w_tl(:) = 0._JPRB
    twr_tl(:) = 0._JPRB
  Endif
  ! End of TL profile calcs
  ! ATTENTION
  !  w_tl(:) = prof_tl % q(:)

  !5) set predictors for RTTOV-8 options
  !--

  !5.1 mixed gases
  !---

  predictors_tl % mixedgas(1,:)  = 0._JPRB
  predictors_tl % mixedgas(2,:)  = 0._JPRB
  predictors_tl % mixedgas(3,:)  = geom % seczen * tr_tl(:)
  predictors_tl % mixedgas(4,:)  = 2._JPRB * predictors % mixedgas(3,:) * tr_tl(:)
  ! or predictors_tl % mixedgas(4,:)  = 2. * geom % seczen * tr_tl(:) * tr(:)
  predictors_tl % mixedgas(5,:)  = tr_tl(:)
  predictors_tl % mixedgas(6,:)  = 2._JPRB * tr_tl(:) * tr(:)
  ! or predictors_tl % mixedgas(6,:)  = 2. * tr_tl(:) * predictors % mixedgas(5,:)
  predictors_tl % mixedgas(7,:)  = geom % seczen * tw_tl(:)
  predictors_tl % mixedgas(8,:)  =&
       &                geom % seczen * tw_tl(:) / predictors % mixedgas(5,:) &
       & - predictors % mixedgas(7,:) * tr_tl(:) / predictors % mixedgas(6,:)
  ! or predictors_tl % mixedgas(8,:)  = geom % seczen *&
  !     & ( tw_tl(:) / tr(:) - tw(:) * tr_tl(:) / tr(:)**2 )
  ! 9 and 10 may be removed after testing
  predictors_tl % mixedgas(9,:)  = 0._JPRB
  ! predictor 10 is always 0 for the first level
  predictors_tl % mixedgas(10,1) = 0._JPRB
  predictors_tl % mixedgas(10,2:prof_tl % nlevels) =&
       &  0.25_JPRB * geom % seczen_sq * tw_tl(2:prof_tl % nlevels)&
       & / predictors % mixedgas(10,2:prof_tl % nlevels)**3
  ! or predictors_tl % mixedgas(10,:) = 0.25 * geom % seczen_sqrt * tw_tl(:) / tw(:)**0.75


  !5.2 water vapour lines based on RTIASI
  !--------------------------------------

  sec_wr_tl(:)   = geom%seczen * wr_tl(:)
  sec_wrwr_tl(:)   = sec_wr_tl(:) * wr(:) + predictors % watervapour(7,:) * wr_tl(:)
  predictors_tl % watervapour(:,:)  = 0._JPRB

  predictors_tl % watervapour(1,:)  = 2 *  predictors % watervapour(7,:) * sec_wr_tl(:)

  predictors_tl % watervapour(2,:)  = geom%seczen * ww_tl(:)

  predictors_tl % watervapour(3,:)  =  2 * predictors % watervapour(2,:) *  predictors_tl % watervapour(2,:)

  predictors_tl % watervapour(4,:)  =  predictors % watervapour(7,:) * dt_tl(:) + sec_wr_tl(:) * dt(:)

  predictors_tl % watervapour(5,:)  = 0.5_JPRB * sec_wr_tl(:) /  predictors % watervapour(5,:)

  predictors_tl % watervapour(6,:)  = 0.25_JPRB * sec_wr_tl(:) /  predictors % watervapour(6,:)**3

  predictors_tl % watervapour(7,:)  =  sec_wr_tl(:)

  predictors_tl % watervapour(8,:)  = 3 * predictors % watervapour(1,:) * sec_wr_tl(:)
  ! NB can we sort this next one out?
  predictors_tl % watervapour(9,:)  =  &
       & dtabs(:) * &
       & (sec_wr_tl(:) * dt(:) + 2 * predictors % watervapour(7,:)  * dt_tl(:) )
!  Do level = 1, prof_tl % nlevels
!     predictors_tl % watervapour(9,level)  =  &
!       & Abs(dt(level)) * &
!       & (sec_wr_tl(level) * dt(level) + 2 * predictors % watervapour(7,level)  * dt_tl(level) )
!!$   If ( dt(level) >= 0. )Then
!!$     predictors_tl % watervapour(9,level)  =  sec_wr_tl(level) * dt(level) * dt(level) + &
!!$            & 2 * predictors % watervapour(7,level) * dt(level) * dt_tl(level)
!!$   Else
!!$     predictors_tl % watervapour(9,level)  =  sec_wr_tl(level) * dt(level) * dtabs(level) + &
!!$            & predictors % watervapour(7,level) * dtabs(level) * dt_tl(level) - &
!!$            & predictors % watervapour(7,level) * dt(level) * dt_tl(level)
!!$   Endif
!  End Do
  predictors_tl % watervapour(10,:)  = predictors % watervapour(5,:) * dt_tl(:) + 0.5_JPRB * dt(:) * sec_wr_tl(:)/ &
                                      &  predictors % watervapour(5,:)
  predictors_tl % watervapour(11,:)  = 2 * predictors % watervapour(7,:) * wr_tl(:)/wwr(:) - &
                                      & predictors % watervapour(1,:) * wwr_tl(:) / (geom%seczen*wwr(:)*wwr(:))

  predictors_tl % watervapour(12,:)  = 1.5_JPRB * predictors % watervapour(5,:) * wr_tl(:) / wwr(:) - &
                                      & predictors % watervapour(5,:) * wr * wwr_tl(:) / (wwr(:)*wwr(:))

  !predictors_tl % watervapour(12,:)  = 1.5 * sec_wr(:)**0.5 * wr_tl(:) / wwr(:) - &
  !                                    &  * wr * sec_wr(:)**0.5 * wwr_tl(:) / (wwr(:) * wwr(:))
  !
  !5.3 water vapour continuum transmittance based on RTIASI
  !--------------------------------------------------------
  !
  If ( coef % nwvcont > 0 ) Then
    tr_sq(:) = tr(:) * tr(:)
    tr_4(:) = tr_sq(:) * tr_sq(:)
    !predictors_tl % wvcont(:,:)  = 0._JPRB
    predictors_tl % wvcont(1,:)  = sec_wrwr_tl(:) / tr(:) -  predictors % wvcont(1,:) * tr_tl(:) / tr(:)
    predictors_tl % wvcont(2,:)  = sec_wrwr_tl(:) / tr_4(:) - 4 * predictors % wvcont(1,:) * tr_tl(:) / &
                                     & tr_4(:)
    predictors_tl % wvcont(3,:)  = sec_wr_tl(:) / tr(:) -  predictors % watervapour(7,:) * tr_tl(:) / tr_sq(:)
    predictors_tl % wvcont(4,:)  = sec_wr_tl(:) / tr_sq(:) - 2 * predictors % watervapour(7,:) * tr_tl(:) / &
                                     & (tr_sq(:)*tr(:))
  Endif
  !
  !5.4 ozone
  !---------

  If ( coef % nozone > 0 ) Then
     sec_or_tl(:) = geom%seczen * or_tl(:)

     predictors_tl % ozone(1,:)  = &
          & sec_or_tl(:)

     predictors_tl % ozone(2,:)  = &
          & 0.5_JPRB * sec_or_tl(:) / predictors % ozone(2,:)

     predictors_tl % ozone(3,:)  = &
          & sec_or_tl(:) * predictors % ozone(3,:) / predictors % ozone(1,:)&
          & + predictors % ozone(1,:) * dto_tl(:)

     predictors_tl % ozone(4,:)  = &
          & 2 * sec_or_tl(:) * predictors % ozone(1,:)

     predictors_tl % ozone(5,:)  = &
          & 0.5_JPRB * sec_or_tl(:) * predictors % ozone(3,:) /&
          &     ( predictors % ozone(1,:) *predictors % ozone(2,:))&
          & + predictors % ozone(2,:) * dto_tl(:)

     predictors_tl % ozone(6,:)  = &
          & 2 * predictors % ozone(8,:) * or_tl(:)&
          & + predictors % ozone(4,:) * ow_tl(:) / geom%seczen

     predictors_tl % ozone(7,:)  = &
          & 1.5_JPRB * sec_or_tl(:) * predictors % ozone(2,:) / predictors % ozone(10,:)&
          & - geom%seczen * ow_tl(:) * predictors % ozone(2,:)**3 / predictors % ozone(10,:)**2

     predictors_tl % ozone(8,:)  = &
          & predictors % ozone(10,:) * or_tl(:)&
          & + predictors % ozone(1,:) * ow_tl(:)

     predictors_tl % ozone(9,:)  = &
          & sec_or_tl(:) * Sqrt(predictors % ozone(10,:))&
          & + 0.5_JPRB * geom%seczen * ow_tl(:) * predictors % ozone(1,:)&
          &    / Sqrt(predictors % ozone(10,:))

     predictors_tl % ozone(10,:) = &
          & geom%seczen * ow_tl(:)

     predictors_tl % ozone(11,:) = &
          & 2 * geom%seczen * ow_tl(:) * predictors % ozone(10,:)

  Endif
  !
  !5.5 cloud
  !---------
  If ( prof % clw_Data .And. coef % id_sensor == sensor_id_mw ) Then
     deltac(:) = 0.1820_JPRB * 100.0_JPRB * coef % dp(:) / (4.3429_JPRB * gravity)

     predictors_tl % clw(:)    = deltac(:) * prof_tl%clw(:) * geom%seczen

     predictors_tl % clw(2:prof_tl % nlevels) =          &
          &  0.5_JPRB *                                       &
          & ( predictors_tl % clw(2:prof_tl % nlevels) + &
          &  deltac(2:prof_tl % nlevels) * prof_tl%clw(1:prof_tl % nlevels-1) * &
          &  geom%seczen )
  Else
     predictors_tl % ncloud   = 0
  Endif
  !
  !5.6 carbon dioxide transmittance based on RTIASI
  !-------------------------------------------------
  !
  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
     tr_sqrt(:) = Sqrt(tr(:))
     predictors_tl % co2(1,:)    = geom%seczen * co2r_tl(:)
     predictors_tl % co2(2,:)    = 2 * tr_tl(:) * predictors % co2(5,:)
     predictors_tl % co2(3,:)    = geom%seczen * tr_tl(:)
     predictors_tl % co2(4,:)    = 2 * tr_tl(:) * predictors % co2(3,:)
     predictors_tl % co2(5,:)    = tr_tl(:)
     predictors_tl % co2(6,:)    = 0._JPRB
     predictors_tl % co2(7,:)    = geom%seczen * twr_tl(:)
     predictors_tl % co2(8,:)    = 2 * geom%seczen * Sqrt( predictors % co2(8,:)) * co2w_tl(:)
     predictors_tl % co2(9,:)    = 3 * twr(:) * twr(:) * twr_tl(:)
     predictors_tl % co2(10,:)   = geom%seczen * tr_sqrt(:) * twr_tl(:) + &
                                   & 0.5_JPRB * predictors % co2(7,:) * tr_tl(:) / tr_sqrt(:)
  Endif
End Subroutine rttov_setpredictors_8_tl
