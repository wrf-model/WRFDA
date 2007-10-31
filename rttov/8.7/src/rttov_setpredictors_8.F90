!
Subroutine rttov_setpredictors_8( &
     prof,      & ! in
     geom,      & ! in
     coef,      & ! in
     predictors ) ! out
  ! Description
  ! RTTOV-8 Model
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
  !  1.1   11/09/2003  Added predictors for wv line and continuum and CO2 (R Saunders)
  !  1.2   03/06/2004  Parkind parametrisation (P. Brunel)
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
     & gravity        ,&
     & sensor_id_mw

  ! Imported Type Definitions:
  Use rttov_types, Only : &
     & rttov_coef     ,&
     & profile_Type   ,&
     & geometry_Type  ,&
     & predictors_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Type(profile_Type),    Intent(in)    :: prof       ! profile
  Type(rttov_coef),      Intent(in)    :: coef       ! coefficients
  Type(geometry_Type),   Intent(in)    :: geom       ! geometry
  Type(predictors_Type), Intent(inout) :: predictors ! predictors

  !local variables:
  Integer(Kind=jpim) :: level

  ! user profile
  Real(Kind=jprb) :: t(prof % nlevels)
  Real(Kind=jprb) :: w(prof % nlevels)
  Real(Kind=jprb) :: o(prof % nlevels)
  Real(Kind=jprb) :: co2(prof % nlevels)

  ! reference profile
  Real(Kind=Jprb) :: tr(prof % nlevels)
  Real(Kind=Jprb) :: wr(prof % nlevels)
  Real(Kind=Jprb) :: wwr(prof % nlevels)
  Real(Kind=Jprb) :: or(prof % nlevels)
  Real(Kind=Jprb) :: co2r(prof % nlevels)
  Real(Kind=Jprb) :: twr(prof % nlevels)

  ! user - reference
  Real(Kind=Jprb) :: dt(prof % nlevels)
  Real(Kind=Jprb) :: dto(prof % nlevels)
  Real(Kind=Jprb) :: dtabs(prof % nlevels)

  ! pressure weighted
  Real(Kind=Jprb) :: tw(prof % nlevels)
  Real(Kind=Jprb) :: ww(prof % nlevels)
  Real(Kind=Jprb) :: ow(prof % nlevels)
  Real(Kind=Jprb) :: co2w(prof % nlevels)

  ! intermediate variables
  Real(Kind=Jprb) :: sum1,sum2
  Real(Kind=Jprb) :: deltac(prof %nlevels)
  Real(Kind=Jprb) :: sec_or(prof %nlevels)
  Real(Kind=Jprb) :: sec_wr(prof %nlevels)
  Real(Kind=Jprb) :: sec_wrwr(prof %nlevels)
  Real(Kind=Jprb) :: tr_sq(prof %nlevels)

  !- End of header --------------------------------------------------------

  !-------------------------------------------------------------------------------
  ! 1 profile layer quantities
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
  ! 2 calculate deviations from reference profile (layers)
  ! if no input O3 profile, set to reference value (dto =0)
  !-----------------------------------------------------------------------------
  dt(:)  = t(:) - coef % tstar(:)
  dtabs(:) = Abs(dt(:))
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     dto(:) = t(:) - coef % to3star(:)
  Else
     dto(:) = 0._JPRB
  Endif
  !------------------------------------------------------------------------------
  ! 3 calculate (profile / reference profile) ratios; tr wr or co2r
  !------------------------------------------------------------------------------
  tr(:) = t(:) / coef % tstar(:)
  wr(:) = w(:) / coef % wstar(:)
  ! if no input O3 profile, set to reference value (or =1)
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     or(:) = o(:) / coef % ostar(:)
  Else
     or(:) = 1._JPRB
  Endif
  ! if no input CO2 profile, set to reference value (co2r=1)
  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
    co2r(:) = co2(:) / coef % co2star(:)
  Else
    co2r(:) = 1._JPRB
  Endif
  !------------------------------------------------------------------------------
  ! 4 calculate profile / reference profile sums: tw ww ow co2w twr
  !------------------------------------------------------------------------------
  tw(1) = 0._JPRB
  Do level = 2 , prof % nlevels
     tw( level ) = tw( level-1 ) + coef % dpp( level ) * tr ( level -1 )
  End Do

  sum1 = 0._JPRB
  sum2 = 0._JPRB
  Do level = 1, prof % nlevels
     sum1 = sum1 + coef % dpp( level ) *      w       ( level )
     sum2 = sum2 + coef % dpp( level ) * coef % wstar ( level )
     ww ( level ) = sum1 / sum2
  End Do
  sum1 = 0._JPRB
  sum2 = 0._JPRB
  Do level = 1, prof % nlevels
     sum1 = sum1 + coef % dpp( level ) *      w       ( level )  *  t ( level )
     sum2 = sum2 + coef % dpp( level ) * coef % wstar ( level )  * coef % tstar ( level )
     wwr ( level ) = sum1 / sum2
  End Do

  ! if no input O3 profile, set to reference value (ow =1)
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     Do level = 1, prof % nlevels
        sum1 = sum1 + coef % dpp( level ) *      o       ( level )
        sum2 = sum2 + coef % dpp( level ) * coef % ostar ( level )
        ow ( level ) = sum1 / sum2
     End Do
  Else
     ow(:) = 1._JPRB
  Endif

  ! if no input co2 profile, set to reference value (co2w=1 and twr=1)
  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     Do level = 1, prof % nlevels
        sum1 = sum1 + coef % dpp( level ) *     co2        ( level )
        sum2 = sum2 + coef % dpp( level ) * coef % co2star ( level )
        co2w ( level ) = sum1 / sum2
     End Do
     sum1 = 0._JPRB
     sum2 = 0._JPRB
     twr ( 1 ) = 0._JPRB
     Do level = 2, prof % nlevels
        sum1 = sum1 + coef % dpp( level ) *     t        ( level-1 )
        sum2 = sum2 + coef % dpp( level ) * coef % tstar ( level-1 )
        twr ( level ) = sum1 / sum2
     End Do
  Else
    co2w(:) = 1._JPRB
    twr(:) = 1._JPRB
  Endif

  !5) set predictors for RTTOV-8 options
  !--

  !5.1 mixed gases
  !---
  tr_sq(:) = tr(:) * tr(:)
  predictors % mixedgas(1,:)  = geom % seczen
  predictors % mixedgas(2,:)  = geom % seczen_sq
  predictors % mixedgas(3,:)  = geom % seczen * tr(:)
  predictors % mixedgas(4,:)  = geom % seczen * tr_sq(:)
  predictors % mixedgas(5,:)  = tr(:)
  predictors % mixedgas(6,:)  = tr_sq(:)
  predictors % mixedgas(7,:)  = geom % seczen * tw(:)
  predictors % mixedgas(8,:)  = geom % seczen * tw(:) / tr(:)
  ! these latter 2 predictors may be removed after testing
  predictors % mixedgas(9,:)  = geom % seczen_sqrt
  predictors % mixedgas(10,:) = geom % seczen_sqrt * tw(:)**0.25_JPRB

  !5.2 water vapour line transmittance based on RTIASI but with pred 9 removed
  !----------------

  sec_wr(:) = geom%seczen * wr(:)
  sec_wrwr(:) = sec_wr(:) * wr(:)
  !predictors % watervapour(:,:) = 0._JPRB
  predictors % watervapour(1,:)  = sec_wr(:) * sec_wr(:)
  predictors % watervapour(2,:)  = geom%seczen * ww(:)
  predictors % watervapour(3,:)  = ( geom%seczen * ww(:) )**2
  predictors % watervapour(4,:)  = sec_wr(:) * dt(:)
  predictors % watervapour(5,:)  = Sqrt( sec_wr(:) )
  predictors % watervapour(6,:)  = sec_wr(:)**0.25_JPRB
  predictors % watervapour(7,:)  = sec_wr(:)
  predictors % watervapour(8,:)  = sec_wr(:)**3
  predictors % watervapour(9,:)  = sec_wr(:) * dt(:) * dtabs(:)
  predictors % watervapour(10,:) = Sqrt(sec_wr(:)) * dt(:)
  predictors % watervapour(11,:) = sec_wrwr(:) / wwr(:)
  predictors % watervapour(12,:) = Sqrt(geom%seczen) * wr(:)**1.5_JPRB / wwr(:)

  !5.3 water vapour continuum transmittance based on RTIASI
  !----------------
  !
  If ( coef % nwvcont > 0 ) Then
    !predictors % wvcont(:,:)  = 0._JPRB
    predictors % wvcont(1,:)  = sec_wrwr(:) / tr(:)
    predictors % wvcont(2,:)  = sec_wrwr(:)  / (tr_sq(:)*tr_sq(:))
    predictors % wvcont(3,:)  = sec_wr(:) / tr(:)
    predictors % wvcont(4,:)  = sec_wr(:) / tr_sq(:)
  Endif

  !5.4 ozone
  !---------

  ! if no input O3 profile, variables or, ow and dto have been set
  ! to the reference profile values (1, 1, 0)
  If ( coef % nozone > 0 ) Then
     sec_or(:) = geom%seczen * or(:)
     predictors % ozone(1,:)  = sec_or(:)
     predictors % ozone(2,:)  = Sqrt( sec_or(:) )
     predictors % ozone(3,:)  = sec_or(:) * dto(:)
     predictors % ozone(4,:)  = sec_or(:) * sec_or(:)
     predictors % ozone(5,:)  = Sqrt(sec_or(:)) * dto(:)
     predictors % ozone(6,:)  = sec_or(:) * or(:) * ow (:)
     predictors % ozone(7,:)  = Sqrt( sec_or(:) ) * or(:) / ow(:)
     predictors % ozone(8,:)  = sec_or(:) * ow(:)
     predictors % ozone(9,:)  = sec_or(:) * Sqrt( geom%seczen * ow(:) )
     predictors % ozone(10,:) = geom%seczen * ow(:)
     predictors % ozone(11,:) = geom%seczen * ow(:) * geom%seczen * ow(:)
  Endif


  !5.5 cloud
  !---------
  If ( prof % clw_Data .And. coef % id_sensor == sensor_id_mw ) Then
     deltac(:) = 0.1820_JPRB * 100.0_JPRB * coef % dp(:) / (4.3429_JPRB * gravity)
     predictors % clw(:)         = deltac(:) * prof%clw(:) * geom%seczen
     predictors % clw(2:prof % nlevels) =          &
          &  0.5_JPRB *                                 &
          & ( predictors % clw(2:prof % nlevels) + &
          &  deltac(2:prof % nlevels) * prof%clw(1:prof % nlevels-1) * &
          &  geom%seczen )
     predictors % ncloud   = 1
  Endif

  !5.6 carbon diooxide transmittance based on RTIASI
  !-------------------------------------------------
  !
  If ( prof % co2_Data .And. coef % nco2 > 0 ) Then
     predictors % co2(1,:)    = geom%seczen * co2r(:)
     predictors % co2(2,:)    = tr_sq(:)
     predictors % co2(3,:)    = geom%seczen * tr(:)
     predictors % co2(4,:)    = geom%seczen * tr_sq(:)
     predictors % co2(5,:)    = tr(:)
     predictors % co2(6,:)    = geom%seczen
     predictors % co2(7,:)    = geom%seczen * twr(:)
     predictors % co2(8,:)    = (geom%seczen * co2w(:))**2
     predictors % co2(9,:)    = twr(:) * twr(:) * twr(:)
     predictors % co2(10,:)   = geom%seczen * twr(:) * Sqrt(tr(:))
  Endif

End Subroutine rttov_setpredictors_8
