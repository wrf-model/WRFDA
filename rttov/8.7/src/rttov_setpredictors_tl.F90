!
Subroutine rttov_setpredictors_tl( &
     & prof,        &! in
     & prof_tl,     &! in
     & geom,        &! in
     & coef,        &! in
     & predictors,  &! in
     & predictors_tl ) ! inout
  ! Description
  ! RTTOV-7 Model
  ! TL of rttov_setpredictors
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
  Type(profile_Type),    Intent(in)  :: prof
  Type(profile_Type),    Intent(in)  :: prof_tl
  Type(rttov_coef),      Intent(in)  :: coef
  Type(geometry_Type),   Intent(in)  :: geom
  Type(predictors_Type), Intent(in)  :: predictors
  Type(predictors_Type), Intent(inout) :: predictors_tl ! in because of mem allocation


  !local variables:
  Integer(Kind=jpim) :: level

  ! user profile
  Real(Kind=jprb) :: t(prof % nlevels)
  Real(Kind=jprb) :: w(prof % nlevels)

  ! reference profile
  Real(Kind=jprb) :: tr(prof % nlevels)
  Real(Kind=jprb) :: wr(prof % nlevels)

  ! user - reference
  Real(Kind=jprb) :: dt(prof % nlevels)

  ! pressure weighted
  Real(Kind=jprb) :: tw(prof % nlevels)


  ! intermediate variables
  Real(Kind=jprb) :: sum1,sum2
! Real(Kind=jprb) :: oz11,oz22
  Real(Kind=jprb) :: deltac(prof %nlevels)
  Real(Kind=jprb) :: sec_wr(prof %nlevels)

  ! TL variables
  Real(Kind=jprb) :: t_tl(prof % nlevels)
  Real(Kind=jprb) :: w_tl(prof % nlevels)
  Real(Kind=jprb) :: o_tl(prof % nlevels)

  Real(Kind=jprb) :: tr_tl(prof % nlevels)
  Real(Kind=jprb) :: wr_tl(prof % nlevels)
  Real(Kind=jprb) :: or_tl(prof % nlevels)

  Real(Kind=jprb) :: dt_tl(prof % nlevels)
  Real(Kind=jprb) :: dto_tl(prof % nlevels)

  Real(Kind=jprb) :: tw_tl(prof % nlevels)
  Real(Kind=jprb) :: ww_tl(prof % nlevels)
  Real(Kind=jprb) :: ow_tl(prof % nlevels)


  Real(Kind=jprb) :: sec_or_tl(prof %nlevels)
  Real(Kind=jprb) :: sec_wr_tl(prof %nlevels)
  Real(Kind=jprb) :: zsqrt, zrecip

  !- End of header --------------------------------------------------------

  ! profile layer quantities
  ! Direct variables
  t(1) = prof % t(1)
  t(2 : prof % nlevels ) = ( prof % t(1 : prof % nlevels-1) + &
          & prof % t(2 : prof % nlevels  ) ) / 2

  w(1) = prof % q(1)
  w(2 : prof % nlevels ) = ( prof % q(1 : prof % nlevels-1) + &
          & prof % q(2 : prof % nlevels  ) ) / 2

  ! Direct value of o NOT needed for TL

  ! TL variables
  t_tl(1) = prof_tl % t(1)
  t_tl(2 : prof_tl % nlevels ) = ( prof_tl % t(1 : prof_tl % nlevels-1) + &
          & prof_tl % t(2 : prof_tl % nlevels  ) ) / 2

  w_tl(1) = prof_tl % q(1)
  w_tl(2 : prof_tl % nlevels ) = ( prof_tl % q(1 : prof_tl % nlevels-1) + &
          & prof_tl % q(2 : prof_tl % nlevels  ) ) / 2

  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     o_tl(1) = prof_tl % o3(1)
     o_tl(2 : prof_tl % nlevels ) = ( prof_tl % o3(1 : prof_tl % nlevels-1) + &
             & prof_tl % o3(2 : prof_tl % nlevels  ) ) / 2
  Endif

  !3) calculate deviations from reference profile (layers)
  ! if no input O3 profile, set to reference value (dto =0)
  ! Direct variables
  dt(:)  = t(:) - coef % tstar(:)
  ! Direct value of dto NOT needed for TL

  ! TL variables
  dt_tl(:)  = t_tl(:)
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     dto_tl(:) = t_tl(:)
  Else
     dto_tl(:) = 0._JPRB
  Endif

  !2) calculate (profile / reference profile) ratios; tr wr or
  ! if no input O3 profile, set to reference value (or =1)
  ! Direct variables
  tr(:) = t(:) / coef % tstar(:)
  wr(:) = w(:) / coef % wstar(:)
  ! Direct value of or NOT needed for TL
  ! TL variables
  tr_tl(:) = t_tl(:) / coef % tstar(:)
  wr_tl(:) = w_tl(:) / coef % wstar(:)
  If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
     or_tl(:) = o_tl(:) / coef % ostar(:)
  Else
     or_tl(:) = 0._JPRB
  Endif

  ! calculate profile / reference profile sums: tw ww ow
  ! if no input O3 profile, set to reference value (ow =1)
  ! Direct variables
  tw(1) = 0._JPRB
  Do level = 2 , prof % nlevels
     tw( level ) = tw( level-1 ) + coef % dpp( level ) * tr ( level -1 )
  End Do

  ! Direct value of ww NOT needed for TL
  ! Direct value of ow NOT needed for TL


  ! TL variables
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




  ! ATTENTION
  !  w_tl(:) = prof_tl % q(:)


  !5) set predictors
  !--

  !5.1 mixed gases
  !---

  Do level = 1, prof_tl % nlevels
    predictors_tl % mixedgas(1,level)  = 0._JPRB
    predictors_tl % mixedgas(2,level)  = 0._JPRB
    predictors_tl % mixedgas(3,level)  = geom % seczen * tr_tl(level)
    predictors_tl % mixedgas(4,level)  = 2._JPRB * predictors % mixedgas(3,level) * tr_tl(level)
  ! or predictors_tl % mixedgas(4,level)  = 2. * geom % seczen * tr_tl(level) * tr(level)
    predictors_tl % mixedgas(5,level)  = tr_tl(level)
    predictors_tl % mixedgas(6,level)  = 2._JPRB * tr_tl(level) * tr(level)
  ! or predictors_tl % mixedgas(6,level)  = 2. * tr_tl(level) * predictors % mixedgas(5,level)
    predictors_tl % mixedgas(7,level)  = geom % seczen * tw_tl(level)
    predictors_tl % mixedgas(8,level)  =&
                     & geom % seczen * tw_tl(level) / predictors % mixedgas(5,level) &
      & - predictors % mixedgas(7,level) * tr_tl(level) / predictors % mixedgas(6,level)
    ! or predictors_tl % mixedgas(8,level)  = geom % seczen *&
    !     & ( tw_tl(level) / tr(level) - tw(level) * tr_tl(level) / tr(level)**2 )
      predictors_tl % mixedgas(9,level)  = 0._JPRB
    ! predictor 10 is always 0 for the first level
  End Do

  predictors_tl % mixedgas(10,1) = 0._JPRB
  predictors_tl % mixedgas(10,2:prof_tl % nlevels) =&
         & 0.25_JPRB * geom % seczen_sq * tw_tl(2:prof_tl % nlevels)&
        & / predictors % mixedgas(10,2:prof_tl % nlevels)**3
  ! or predictors_tl % mixedgas(10,level) = 0.25 * geom % seczen_sqrt * tw_tl(level) / tw(level)**0.75


  !5.2 water vapour  ( numbers in right hand are predictor numbers
  ! in the reference document for RTTOV7 (science and validation report)
  !----------------

  Do level = 1, prof_tl % nlevels
    sec_wr(level)    = geom%seczen * wr(level)
    sec_wr_tl(level) = geom%seczen * wr_tl(level)
    predictors_tl % watervapour(1,level)  = sec_wr_tl(level)          !  7

    predictors_tl % watervapour(2,level) = 0.5_JPRB * sec_wr_tl(level)/predictors % watervapour(2,level)!  5

    zrecip=1.0_JPRB/ predictors % watervapour(1,level)
    predictors_tl % watervapour(3,level)  =                      &! 12
        & 2 * predictors % watervapour(3,level) * sec_wr_tl(level) *zrecip &
        & - predictors % watervapour(3,level)**2 * geom%seczen * ww_tl(level) / predictors % watervapour(5,level)

    predictors_tl % watervapour(4,level)  =                      &! 4
        & sec_wr_tl(level) * predictors % watervapour(4,level) *zrecip &
        & + predictors % watervapour(1,level) * dt_tl(level)

    predictors_tl % watervapour(5,level)  =                      &! 1
        & 2 * predictors % watervapour(1,level) * sec_wr_tl(level)

    predictors_tl % watervapour(6,level)  =                      &! 11
        & predictors % watervapour(2,level) * dt_tl(level)&
        & + 0.5_JPRB * sec_wr_tl(level) * predictors % watervapour(6,level) *zrecip

    predictors_tl % watervapour(7,level)  =                      &! 6
        & 0.25_JPRB * sec_wr_tl(level) / predictors % watervapour(7,level)**3

    predictors_tl % watervapour(8,level)  =                      &! 13
        & geom%seczen * predictors % watervapour(8,level)      * &
        & ( 1.5_JPRB * wr_tl(level) *zrecip &
          & -  ww_tl(level) / predictors % watervapour(13,level)**0.5_JPRB)

    predictors_tl % watervapour(9,level)  =                      &! 8
        & 3 * sec_wr_tl(level) * predictors % watervapour(5,level)

    predictors_tl % watervapour(10,level)  =                      &! 9
        & 4 * sec_wr_tl(level) * predictors % watervapour(9,level)

    predictors_tl % watervapour(11,level)  =                      &! 10
        & Abs(dt(level)) * &
        & (sec_wr_tl(level) * dt(level) + 2 * sec_wr(level) * dt_tl(level) )

    zsqrt=Sqrt(predictors % watervapour(13,level))
    predictors_tl % watervapour(12,level)  =                      &! 3
        & 4 * geom%seczen * ww_tl(level) * predictors % watervapour(12,level) &
        & / zsqrt

    predictors_tl % watervapour(13,level)  =                      &! 2
        & 2 * geom%seczen * ww_tl(level) * zsqrt

    predictors_tl % watervapour(14,level)  =                      &! 14
        & 2 * predictors % watervapour(14,level) * sec_wr_tl(level) *zrecip &
        & - predictors % watervapour(14,level)**2 * tr_tl(level) / (geom%seczen * wr(level) * wr(level))

    predictors_tl % watervapour(15,level)  =                      &! 15
        & ( predictors % watervapour(15,level) * geom%seczen ) * &
        & ( 2 * wr_tl(level) *zrecip   - &
          & 4 * tr_tl(level) * predictors % watervapour(14,level)  / &
             & predictors % watervapour(5,level)             )
  End Do

  !5.3 ozone
  !---------

  If ( coef % nozone > 0 ) Then
    Do level = 1, prof_tl % nlevels
     sec_or_tl(level) = geom%seczen * or_tl(level)

     predictors_tl % ozone(1,level)  = &
           & sec_or_tl(level)

     predictors_tl % ozone(2,level)  = &
           & 0.5_JPRB * sec_or_tl(level) / predictors % ozone(2,level)

     predictors_tl % ozone(3,level)  = &
           & sec_or_tl(level) * predictors % ozone(3,level) / predictors % ozone(1,level)&
           & + predictors % ozone(1,level) * dto_tl(level)

     predictors_tl % ozone(4,level)  = &
           & 2 * sec_or_tl(level) * predictors % ozone(1,level)

!oz11=predictors % ozone(1,level)
!oz22=predictors % ozone(2,level)

     predictors_tl % ozone(5,level)  = &
           & 0.5_JPRB * sec_or_tl(level) * predictors % ozone(3,level) /&
               & ( predictors % ozone(1,level) *predictors % ozone(2,level))&
           & + predictors % ozone(2,level) * dto_tl(level)

     predictors_tl % ozone(6,level)  = &
           & 2 * predictors % ozone(8,level) * or_tl(level)&
           & + predictors % ozone(4,level) * ow_tl(level) / geom%seczen

     predictors_tl % ozone(7,level)  = &
           & 1.5_JPRB * sec_or_tl(level) * predictors % ozone(2,level) / predictors % ozone(10,level)&
           & - geom%seczen * ow_tl(level) * predictors % ozone(2,level)**3 / predictors % ozone(10,level)**2

     predictors_tl % ozone(8,level)  = &
           & predictors % ozone(10,level) * or_tl(level)&
           & + predictors % ozone(1,level) * ow_tl(level)

     zsqrt=Sqrt(predictors % ozone(10,level))
     predictors_tl % ozone(9,level)  = &
           & sec_or_tl(level) * zsqrt&
           & + 0.5_JPRB * geom%seczen * ow_tl(level) * predictors % ozone(1,level)&
              & / zsqrt

     predictors_tl % ozone(10,level) = &
           & geom%seczen * ow_tl(level)

     predictors_tl % ozone(11,level) = &
           & 2 * geom%seczen * ow_tl(level) * predictors % ozone(10,level)

    End Do
  Endif


  !5.4 cloud
  !---------
  If ( prof % clw_Data ) Then
     deltac(:) = 0.1820_JPRB * 100.0_JPRB * coef % dp(:) / (4.3429_JPRB * gravity)

     predictors_tl % clw(:)    = deltac(:) * prof_tl%clw(:) * geom%seczen

     predictors_tl % clw(2:prof_tl % nlevels) =          &
            & 0.5_JPRB *                                       &
           & ( predictors_tl % clw(2:prof_tl % nlevels) + &
            & deltac(2:prof_tl % nlevels) * prof_tl%clw(1:prof_tl % nlevels-1) * &
            & geom%seczen )
  Else
     predictors_tl % ncloud   = 0
  Endif


End Subroutine rttov_setpredictors_tl
