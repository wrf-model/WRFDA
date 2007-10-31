!
Subroutine rttov_setpredictors_ad( &
     & prof,        &! in
     & prof_ad,     &! inout
     & geom,        &! in
     & coef,        &! in
     & predictors,  &! in
     & predictors_ad ) ! inout
  ! Description
  ! RTTOV-7 Model
  ! AD of rttov_setpredictors
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
  Use rttov_const, Only :   &
       & gravity

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type   ,&
       & geometry_Type  ,&
       & predictors_Type

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
  Real(Kind=jprb) :: t(prof % nlevels)
  Real(Kind=jprb) :: w(prof % nlevels)
  Real(Kind=jprb) :: o(prof % nlevels)

  ! reference profile
  Real(Kind=jprb) :: tr(prof % nlevels)
  Real(Kind=jprb) :: wr(prof % nlevels)

  ! user - reference
  Real(Kind=jprb) :: dt(prof % nlevels)

  ! pressure weighted
  Real(Kind=jprb) :: tw(prof % nlevels)


  Real(Kind=jprb) :: sum1,sum2
  Real(Kind=jprb) :: deltac(prof %nlevels)
  Real(Kind=jprb) :: sec_wr(prof %nlevels)
  Real(Kind=jprb) :: sum2_ww(prof %nlevels)
  Real(Kind=jprb) :: sum2_ow(prof %nlevels)

  ! TL variables
  Real(Kind=jprb) :: t_ad(prof % nlevels)
  Real(Kind=jprb) :: w_ad(prof % nlevels)
  Real(Kind=jprb) :: o_ad(prof % nlevels)

  Real(Kind=jprb) :: tr_ad(prof % nlevels)
  Real(Kind=jprb) :: wr_ad(prof % nlevels)
  Real(Kind=jprb) :: or_ad(prof % nlevels)

  Real(Kind=jprb) :: dt_ad(prof % nlevels)
  Real(Kind=jprb) :: dto_ad(prof % nlevels)

  Real(Kind=jprb) :: tw_ad(prof % nlevels)
  Real(Kind=jprb) :: ww_ad(prof % nlevels)
  Real(Kind=jprb) :: ow_ad(prof % nlevels)


  Real(Kind=jprb) :: sec_or_ad(prof %nlevels)
  Real(Kind=jprb) :: sec_wr_ad(prof %nlevels)
  Real(Kind=jprb) :: zsqrt

  !- End of header --------------------------------------------------------

  ! profile layer quantities
  ! Direct variables
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


  !3) calculate deviations from reference profile (layers)
  ! if no input O3 profile, set to reference value (dto =0)
  ! Direct variables
  dt(:)  = t(:) - coef % tstar(:)

  !2) calculate (profile / reference profile) ratios; tr wr or
  ! if no input O3 profile, set to reference value (or =1)
  ! Direct variables
  tr(:) = t(:) / coef % tstar(:)
  wr(:) = w(:) / coef % wstar(:)

  ! calculate profile / reference profile sums: tw ww ow
  ! if no input O3 profile, set to reference value (ow =1)
  ! Direct variables
  tw(1) = 0._JPRB
  Do level = 2 , prof % nlevels
     tw( level ) = tw( level-1 ) + coef % dpp( level ) * tr ( level -1 )
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


  ! Ajoint code
  !-------------
  w_ad(:)      = 0._JPRB
  wr_ad(:)     = 0._JPRB
  ww_ad(:)     = 0._JPRB
  sec_wr_ad(:) = 0._JPRB
  dt_ad(:)     = 0._JPRB

  t_ad(:) = 0._JPRB
  tr_ad(:) = 0._JPRB
  tw_ad(:) = 0._JPRB

  !5.4 cloud
  !---------
  If ( prof % clw_Data ) Then
     deltac(:) = 0.1820_JPRB * 100.0_JPRB * coef % dp(:) / (4.3429_JPRB * gravity)

     prof_ad%clw(1:prof_ad % nlevels-1) = prof_ad%clw(1:prof_ad % nlevels-1) +&
           & 0.5_JPRB * predictors_ad % clw(2:prof_ad % nlevels) *&
           & deltac(2:prof_ad % nlevels) * geom%seczen

     predictors_ad % clw(2:prof_ad % nlevels) = 0.5_JPRB *  predictors_ad % clw(2:prof_ad % nlevels)

     prof_ad%clw(:) = prof_ad%clw(:)  + predictors_ad % clw(:) *&
           & deltac(:) * geom%seczen

  Endif

  !5.3 ozone
  !---------
  If ( coef % nozone > 0 ) Then
    Do level = 1,prof_ad % nlevels
      o_ad(level)      = 0._JPRB
      or_ad(level)     = 0._JPRB
      ow_ad(level)     = 0._JPRB
      dto_ad(level)    = 0._JPRB
      sec_or_ad(level) = 0._JPRB

      ! One can pack all ow_ad lines in one longer statement
      ! same for sec_or_ad and dto_ad
      ow_ad(level) = ow_ad(level) + predictors_ad % ozone(11,level) *&
             & 2 * geom%seczen * predictors % ozone(10,level)

      ow_ad(level) = ow_ad(level) + predictors_ad % ozone(10,level) * geom%seczen

      zsqrt=Sqrt(predictors % ozone(10,level))
      sec_or_ad(level) = sec_or_ad(level) + predictors_ad % ozone(9,level) *&
            & zsqrt
      ow_ad(level)    = ow_ad(level)      + predictors_ad % ozone(9,level) *&
            & 0.5_JPRB * geom%seczen * predictors % ozone(1,level) &
               & / zsqrt

      ow_ad(level) = ow_ad(level) + predictors_ad % ozone(8,level) *&
            & predictors % ozone(1,level)
      or_ad(level) = or_ad(level) + predictors_ad % ozone(8,level) *&
            & predictors % ozone(10,level)

      sec_or_ad(level) = sec_or_ad(level) + predictors_ad % ozone(7,level) *&
            & 1.5_JPRB * predictors % ozone(2,level) / predictors % ozone(10,level)
      ow_ad(level)     = ow_ad(level)     - predictors_ad % ozone(7,level) *&
            & geom%seczen * predictors % ozone(2,level)**3 / predictors % ozone(10,level)**2

      or_ad(level) =  or_ad(level) + predictors_ad % ozone(6,level) *&
            & 2 * predictors % ozone(8,level)
      ow_ad(level) = ow_ad(level)  + predictors_ad % ozone(6,level) *&
            & predictors % ozone(4,level) / geom%seczen

      sec_or_ad(level) = sec_or_ad(level) + predictors_ad % ozone(5,level) *&
            & 0.5_JPRB * predictors % ozone(3,level) /&
                & ( predictors % ozone(1,level) *predictors % ozone(2,level))
      dto_ad(level) = dto_ad(level)       + predictors_ad % ozone(5,level) *&
            & predictors % ozone(2,level)

      sec_or_ad(level) = sec_or_ad(level) + predictors_ad % ozone(4,level) *&
            & 2 * predictors % ozone(1,level)

      sec_or_ad(level) = sec_or_ad(level) + predictors_ad % ozone(3,level) *&
            & predictors % ozone(3,level) / predictors % ozone(1,level)
      dto_ad(level)    = dto_ad(level)    + predictors_ad % ozone(3,level) *&
            & predictors % ozone(1,level)

      sec_or_ad(level) = sec_or_ad(level) + predictors_ad % ozone(2,level) *&
            & 0.5_JPRB / predictors % ozone(2,level)

      sec_or_ad(level) = sec_or_ad(level) + predictors_ad % ozone(1,level)

      or_ad(level) = or_ad(level) + sec_or_ad(level) * geom%seczen
    End Do
  Endif

  !5.2 water vapour  ( numbers in right hand are predictor numbers
  ! in the reference document for RTTOV7 (science and validation report)
  !----------------

  Do level = 1,prof_ad % nlevels
    sec_wr(level)    = geom%seczen * wr(level)

    ! X15  (15)
    wr_ad(level) = wr_ad(level) + predictors_ad % watervapour(15,level) *&
          & predictors % watervapour(15,level) * geom%seczen  * &
          & 2 / predictors % watervapour(1,level)
    tr_ad(level) = tr_ad(level) - predictors_ad % watervapour(15,level) *&
          & predictors % watervapour(15,level) * geom%seczen  * &
            & 4 * predictors % watervapour(14,level)  / &
                & predictors % watervapour(5,level)

    ! X14  (14)
    sec_wr_ad(level) = sec_wr_ad(level) + predictors_ad % watervapour(14,level) *&
          & 2 * predictors % watervapour(14,level) / predictors % watervapour(1,level)
    tr_ad(level)     = tr_ad(level)     - predictors_ad % watervapour(14,level) *&
           & predictors % watervapour(14,level)**2 / (geom%seczen * wr(level) * wr(level))

    ! X13 (2)
    zsqrt=Sqrt( predictors % watervapour(13,level))
    ww_ad(level) =  ww_ad(level) + predictors_ad % watervapour(13,level) *&
          & 2 * geom%seczen * zsqrt

    ! X12 (3)
    ww_ad(level) =  ww_ad(level) + predictors_ad % watervapour(12,level) *&
          & 4 * geom%seczen * predictors % watervapour(12,level) &
          & / zsqrt

    ! X11 (10)
    sec_wr_ad(level) = sec_wr_ad(level) + predictors_ad % watervapour(11,level) *&
           & Abs(dt(level)) * dt(level)
    dt_ad(level) = dt_ad(level)         + predictors_ad % watervapour(11,level) *&
          & 2 * sec_wr(level) * Abs(dt(level))

    ! X10 (9)
    sec_wr_ad(level) = sec_wr_ad(level) + predictors_ad % watervapour(10,level) *&
          & 4 *  predictors % watervapour(9,level)

    ! X9 (8)
    sec_wr_ad(level) = sec_wr_ad(level) + predictors_ad % watervapour(9,level) *&
           & 3 * predictors % watervapour(5,level)

    ! X8 (13)
    wr_ad(level) = wr_ad(level) + predictors_ad % watervapour(8,level) *&
          & geom%seczen * predictors % watervapour(8,level) * &
           & 1.5_JPRB / predictors % watervapour(1,level)
    ww_ad(level) = ww_ad(level) - predictors_ad % watervapour(8,level) *&
          & geom%seczen * predictors % watervapour(8,level) / &
               & predictors % watervapour(13,level)**0.5_JPRB

    ! X7 (6)
    sec_wr_ad(level) = sec_wr_ad(level) + predictors_ad % watervapour(7,level) *&
          & 0.25_JPRB / predictors % watervapour(7,level)**3

    ! X6 (11)
    dt_ad(level)     = dt_ad(level)     + predictors_ad % watervapour(6,level) *&
          & predictors % watervapour(2,level)
    sec_wr_ad(level) = sec_wr_ad(level) + predictors_ad % watervapour(6,level) *&
          & 0.5_JPRB * predictors % watervapour(6,level) / predictors % watervapour(1,level)

    ! X5 (1)
    sec_wr_ad(level) = sec_wr_ad(level) + predictors_ad % watervapour(5,level) *&
          & 2 * predictors % watervapour(1,level)

    ! X4 (4)
    sec_wr_ad(level) = sec_wr_ad(level) + predictors_ad % watervapour(4,level) *&
           & predictors % watervapour(4,level) / predictors % watervapour(1,level)
    dt_ad(level)     = dt_ad(level)     + predictors_ad % watervapour(4,level) *&
          & predictors % watervapour(1,level)

    ! X3 (12)
    sec_wr_ad(level) = sec_wr_ad(level) + predictors_ad % watervapour(3,level) *&
          & 2 * predictors % watervapour(3,level) / predictors % watervapour(1,level)
    ww_ad(level)     = ww_ad(level)     - predictors_ad % watervapour(3,level) *&
          & predictors % watervapour(3,level)**2 * geom%seczen &
          & / predictors % watervapour(5,level)

    ! X2 (5)
    sec_wr_ad(level) = sec_wr_ad(level) + predictors_ad % watervapour(2,level) *&
          & 0.5_JPRB / predictors % watervapour(2,level)

    ! X1 (7)
    sec_wr_ad(level) = sec_wr_ad(level) + predictors_ad % watervapour(1,level)

    wr_ad(level) = wr_ad(level) + sec_wr_ad(level) * geom%seczen
  End Do

  !5.1 mixed gases
  !---------------

  ! X10
  tw_ad(2:prof_ad % nlevels) = tw_ad(2:prof_ad % nlevels) +&
        & predictors_ad % mixedgas(10,2:prof_ad % nlevels) *&
         & 0.25_JPRB * geom % seczen_sq &
        & / predictors % mixedgas(10,2:prof_ad % nlevels)**3

  ! X9
  ! X8
  Do level = 1,prof_ad % nlevels
    tw_ad(level) = tw_ad(level) + predictors_ad % mixedgas(8,level) *&
          & geom % seczen / predictors % mixedgas(5,level)
    tr_ad(level) = tr_ad(level) - predictors_ad % mixedgas(8,level) *&
          & predictors % mixedgas(7,level) / predictors % mixedgas(6,level)

    ! X7
    tw_ad(level) = tw_ad(level) + predictors_ad % mixedgas(7,level) *&
           & geom % seczen

    ! X6
    tr_ad(level) = tr_ad(level) + predictors_ad % mixedgas(6,level) *&
           & 2._JPRB * tr(level)

    ! X5
    tr_ad(level) = tr_ad(level) + predictors_ad % mixedgas(5,level)

    ! X4
    tr_ad(level) = tr_ad(level) + predictors_ad % mixedgas(4,level) *&
          & 2._JPRB * predictors % mixedgas(3,level)

    ! X3
    tr_ad(level) = tr_ad(level) + predictors_ad % mixedgas(3,level) *&
          & geom % seczen

    ! X2
    ! X1
  End Do

  sum1 = 0._JPRB
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     Do level = prof_ad % nlevels, 1, -1
        sum1 = sum1 + ow_ad ( level ) / sum2_ow(level)
        o_ad( level ) = o_ad( level ) + sum1 * coef % dpp( level )
     End Do
  Else
     o_ad(:) = 0._JPRB
  Endif

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


  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     o_ad(:) = o_ad(:) + or_ad(:) / coef % ostar(:)
  Endif

  w_ad(:) = w_ad(:) + wr_ad(:) / coef % wstar(:)

  t_ad(:) = t_ad(:) + tr_ad(:) / coef % tstar(:)


  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     t_ad(:) = t_ad(:) + dto_ad(:)
  Endif

  t_ad(:) = t_ad(:) + dt_ad(:)

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


End Subroutine rttov_setpredictors_ad
