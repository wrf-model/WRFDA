!
Subroutine rttov_setpredictors( &
     & prof,       &! in
     & geom,       &! in
     & coef,       &! in
     & predictors ) ! out
  ! Description
  ! To calculate and store the profile variables (predictors) required
  ! in subsequent transmittance calculations.
  ! Code based on PRFTAU from previous versions of RTTOV
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
  ! see RTTOV7 science and validation report pages 18/19
  ! variable names are close to the documentation
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1       04/12/2003  Optimisation (J Hague and D Salmond ECMWF)
  !  1.2       29/03/2005  Add end of header comment (J. Cameron)
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

  ! reference profile
  Real(Kind=jprb) :: tr(prof % nlevels)
  Real(Kind=jprb) :: wr(prof % nlevels)
  Real(Kind=jprb) :: or(prof % nlevels)

  ! user - reference
  Real(Kind=jprb) :: dt(prof % nlevels)
  Real(Kind=jprb) :: dto(prof % nlevels)

  ! pressure weighted
  Real(Kind=jprb) :: tw(prof % nlevels)
  Real(Kind=jprb) :: ww(prof % nlevels)
  Real(Kind=jprb) :: ow(prof % nlevels)

  ! intermediate variables
  Real(Kind=jprb) :: sum1,sum2
  Real(Kind=jprb) :: deltac(prof %nlevels)
  Real(Kind=jprb) :: sec_or(prof %nlevels)
  Real(Kind=jprb) :: sec_wr(prof %nlevels)

  !- End of header --------------------------------------------------------

  ! 1 profile layer quantities
  t(1) = prof % t(1)
  t(2 : prof % nlevels ) = ( prof % t(1 : prof % nlevels-1) + &
          & prof % t(2 : prof % nlevels  ) ) / 2

  w(1) = prof % q(1)
  w(2 : prof % nlevels ) = ( prof % q(1 : prof % nlevels-1) + &
          & prof % q(2 : prof % nlevels  ) ) / 2

  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     o(1) = prof % o3(1)
     o(2 : prof % nlevels ) = ( prof % o3(1 : prof % nlevels-1) + &
             & prof % o3(2 : prof % nlevels  ) ) / 2
  Endif

  ! 2 calculate deviations from reference profile (layers)
  ! if no input O3 profile, set to reference value (dto =0)
  dt(:)  = t(:) - coef % tstar(:)
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     dto(:) = t(:) - coef % to3star(:)
  Else
     dto(:) = 0._JPRB
  Endif

  ! 3 calculate (profile / reference profile) ratios; tr wr or
  tr(:) = t(:) / coef % tstar(:)
  wr(:) = w(:) / coef % wstar(:)
  ! if no input O3 profile, set to reference value (or =1)
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     or(:) = o(:) / coef % ostar(:)

  Else
     or(:) = 1._JPRB
  Endif

  ! 4 calculate profile / reference profile sums: tw ww ow
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

  ! for other minor gases do as for O3 for testing presence of
  ! coefficients and profile values
  !

  !5) set predictors
  !--

  !5.1 mixed gases
  !---

  Do level = 1, prof % nlevels
    predictors % mixedgas(1,level)  = geom % seczen
    predictors % mixedgas(2,level)  = geom % seczen_sq
    predictors % mixedgas(3,level)  = geom % seczen * tr(level)
    predictors % mixedgas(4,level)  = geom % seczen * tr(level) * tr(level)
    predictors % mixedgas(5,level)  = tr(level)
    predictors % mixedgas(6,level)  = tr(level) * tr(level)
    predictors % mixedgas(7,level)  = geom % seczen * tw(level)
    predictors % mixedgas(8,level)  = geom % seczen * tw(level) / tr(level)
    predictors % mixedgas(9,level)  = geom % seczen_sqrt
    predictors % mixedgas(10,level) = geom % seczen_sqrt * tw(level)**0.25_JPRB
  End Do

  !5.2 water vapour  ( numbers in right hand are predictor numbers
  ! in the reference document for RTTOV7 (science and validation report)
  !----------------

  Do level = 1, prof % nlevels
    sec_wr(level) = geom%seczen * wr(level)
    predictors % watervapour(1,level)  = sec_wr(level)                  !  7
    predictors % watervapour(2,level)  = Sqrt( sec_wr(level) )          !  5
    predictors % watervapour(3,level)  = sec_wr(level) * wr(level) / ww(level)  ! 12
    predictors % watervapour(4,level)  = sec_wr(level) * dt(level)              !  4
    predictors % watervapour(5,level)  = sec_wr(level) * sec_wr(level)  !  1
    predictors % watervapour(6,level)  = predictors % watervapour(2,level) * dt (level) ! 11
    predictors % watervapour(7,level)  = Sqrt( predictors % watervapour(2,level) )       ! 6
    predictors % watervapour(8,level)  = predictors % watervapour(2,level) * wr(level) / ww(level) ! 13
    predictors % watervapour(9,level)  = predictors % watervapour(5,level) * sec_wr(level) ! 8
    predictors % watervapour(10,level) = predictors % watervapour(9,level) * sec_wr(level) ! 9
    predictors % watervapour(11,level) = sec_wr(level) * dt(level) * Abs(dt(level)) ! 10
    predictors % watervapour(12,level) = ( geom%seczen * ww(level) )**4 ! 3
    predictors % watervapour(13,level) = ( geom%seczen * ww(level) )**2 ! 2
    predictors % watervapour(14,level) = sec_wr(level) * wr(level) / tr(level)  ! 14
    predictors % watervapour(15,level) = sec_wr(level) * wr(level) / tr(level)**4   ! 15
  End Do


  !5.3 ozone
  !---------

  ! if no input O3 profile, variables or, ow and dto have been set
  ! to the reference profile values (1, 1, 0)

  If ( coef % nozone > 0 ) Then
     Do level = 1, prof % nlevels
       sec_or(level) = geom%seczen * or(level)
       predictors % ozone(1,level)  = sec_or(level)
       predictors % ozone(2,level)  = Sqrt( sec_or(level) )
       predictors % ozone(3,level)  = sec_or(level) * dto(level)
       predictors % ozone(4,level)  = sec_or(level) * sec_or(level)
       predictors % ozone(5,level)  = predictors % ozone(2,level) * dto(level)
       predictors % ozone(6,level)  = sec_or(level) * or(level) * ow (level)
       predictors % ozone(7,level)  = predictors % ozone(2,level) * or(level) / ow(level)
       predictors % ozone(8,level)  = sec_or(level) * ow(level)
       predictors % ozone(9,level)  = sec_or(level) * Sqrt( geom%seczen * ow(level) )
       predictors % ozone(10,level) = geom%seczen * ow(level)
       predictors % ozone(11,level) = geom%seczen * ow(level) * geom%seczen * ow(level)
     End Do
  Endif


  !5.4 cloud
  !---------
  If ( prof % clw_Data .And. coef % id_sensor == sensor_id_mw ) Then
     deltac(:) = 0.1820_JPRB * 100.0_JPRB * coef % dp(:) / (4.3429_JPRB * gravity)
     predictors % clw(:)         = deltac(:) * prof%clw(:) * geom%seczen
     predictors % clw(2:prof % nlevels) =          &
            & 0.5_JPRB *                                 &
           & ( predictors % clw(2:prof % nlevels) + &
            & deltac(2:prof % nlevels) * prof%clw(1:prof % nlevels-1) * &
            & geom%seczen )
     predictors % ncloud   = 1
  Endif


End Subroutine rttov_setpredictors
