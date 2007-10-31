!
Subroutine rttov_setpredictors_k( &
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
  ! RTTOV-7 Model
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
  ! Method:
  ! see RTTOV7 science and validation report pages 18/19
  ! variable names are close to the documentation
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       22/06/2005  initial (P Brunel)
  !                        based on version 1.2 (29/03/05) of AD code
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
   &     gravity  ,&
   &   sensor_id_mw

  ! Imported Type Definitions:
  Use rttov_types, Only : &
   & rttov_coef     ,&
   & profile_Type   ,&
   & geometry_Type  ,&
   & predictors_Type

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
  Real(Kind=jprb) :: t(nlevels, nprofiles)
  Real(Kind=jprb) :: w(nlevels, nprofiles)
  Real(Kind=jprb) :: o(nlevels, nprofiles)

  ! reference profile
  Real(Kind=jprb) :: tr(nlevels, nprofiles)
  Real(Kind=jprb) :: wr(nlevels, nprofiles)

  ! user - reference
  Real(Kind=jprb) :: dt(nlevels, nprofiles)

  ! pressure weighted
  Real(Kind=jprb) :: tw(nlevels, nprofiles)


  Real(Kind=jprb) :: sum1,sum2
  Real(Kind=jprb) :: deltac(nlevels)
  Real(Kind=jprb) :: sec_wr(nlevels,  nprofiles)
  Real(Kind=jprb) :: sum2_ww(nlevels, nprofiles)
  Real(Kind=jprb) :: sum2_ow(nlevels, nprofiles)

  ! K variables
  Real(Kind=jprb) :: t_k(nlevels, nchannels)
  Real(Kind=jprb) :: w_k(nlevels, nchannels)
  Real(Kind=jprb) :: o_k(nlevels, nchannels)

  Real(Kind=jprb) :: tr_k(nlevels, nchannels)
  Real(Kind=jprb) :: wr_k(nlevels, nchannels)
  Real(Kind=jprb) :: or_k(nlevels, nchannels)

  Real(Kind=jprb) :: dt_k(nlevels, nchannels)
  Real(Kind=jprb) :: dto_k(nlevels, nchannels)

  Real(Kind=jprb) :: tw_k(nlevels, nchannels)
  Real(Kind=jprb) :: ww_k(nlevels, nchannels)
  Real(Kind=jprb) :: ow_k(nlevels, nchannels)


  Real(Kind=jprb) :: sec_or_k(nlevels, nchannels)
  Real(Kind=jprb) :: sec_wr_k(nlevels, nchannels)
  Real(Kind=jprb) :: zsqrt

  !- End of header --------------------------------------------------------

  nullify ( geom )
  nullify ( prof )
  nullify ( prof_k )
  nullify ( pred )
  nullify ( pred_k )

  !
  ! Keep use of prof%nlevels in the code instead of input argument nlevels
  ! This is to allow profiles on variable levels, for future version.
  !

  ! profile layer quantities
  ! Direct variables
  Do j = 1, nprofiles

     prof => profiles(j)

     t(1, j) = prof % t(1)
     t(2 : prof % nlevels, j ) = ( prof % t(1 : prof % nlevels-1) + &
          & prof % t(2 : prof % nlevels) ) / 2._JPRB

     w(1, j) = prof % q(1)
     w(2 : prof % nlevels, j ) = ( prof % q(1 : prof % nlevels-1) + &
          & prof % q(2 : prof % nlevels) ) / 2._JPRB

     If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
        o(1, j) = prof % o3(1)
        o(2 : prof % nlevels, j ) = ( prof % o3(1 : prof % nlevels-1) + &
             & prof % o3(2 : prof % nlevels) ) / 2._JPRB
     Else
        o( : , j ) = 0._JPRB
     Endif


     !3) calculate deviations from reference profile (layers)
     ! if no input O3 profile, set to reference value (dto =0)
     ! Direct variables
     dt(:, j)  = t(:, j) - coef % tstar(:)

     !2) calculate (profile / reference profile) ratios; tr wr or
     ! if no input O3 profile, set to reference value (or =1)
     ! Direct variables
     tr(:, j) = t(:, j) / coef % tstar(:)
     wr(:, j) = w(:, j) / coef % wstar(:)

     ! calculate profile / reference profile sums: tw ww ow
     ! if no input O3 profile, set to reference value (ow =1)
     ! Direct variables
     tw(1, j) = 0._JPRB
     Do level = 2 , prof % nlevels
        tw( level, j ) = tw( level-1, j ) + coef % dpp( level ) * tr ( level -1, j )
     End Do

     sum1 = 0._JPRB
     sum2 = 0._JPRB
     Do level = 1, prof % nlevels
        sum1 = sum1 + coef % dpp( level ) *      w       ( level, j )
        sum2 = sum2 + coef % dpp( level ) * coef % wstar ( level )
        sum2_ww( level, j ) = sum2
     End Do

     If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
        sum1 = 0._JPRB
        sum2 = 0._JPRB
        Do level = 1, prof % nlevels
           sum1 = sum1 + coef % dpp( level ) *      o       ( level, j )
           sum2 = sum2 + coef % dpp( level ) * coef % ostar ( level )
           sum2_ow( level, j ) = sum2
        End Do
     Else
        sum2_ow( :, j ) = 0._JPRB
     Endif

  End Do ! loop on profiles



  ! Ajoint code
  !-------------
  w_k(:,:)      = 0._JPRB
  wr_k(:,:)     = 0._JPRB
  ww_k(:,:)     = 0._JPRB
  sec_wr_k(:,:) = 0._JPRB
  dt_k(:,:)     = 0._JPRB

  t_k(:,:)  = 0._JPRB
  tr_k(:,:) = 0._JPRB
  tw_k(:,:) = 0._JPRB

  !5.4 cloud
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

        Endif
     End Do
  End If

  !5.3 ozone
  !---------
  If ( coef % nozone > 0 ) Then
     Do i = 1, nchannels
        freq=polarisations(i,2)
        j = lprofiles( freq )
        
        ! prof   => profiles(j)    (use of variables derived from prof )
        geom   => angles(j)
        pred   => predictors(j)
        prof_k => profiles_k(i)    ! only for loop on levels
        pred_k => predictors_k(i)

        Do level = 1,prof_k % nlevels

           o_k(level,i)      = 0._JPRB
           or_k(level,i)     = 0._JPRB
           ow_k(level,i)     = 0._JPRB
           dto_k(level,i)    = 0._JPRB
           sec_or_k(level,i) = 0._JPRB

           ! One can pack all ow_k lines in one longer statement
           ! same for sec_or_k and dto_k
           ow_k(level,i) = ow_k(level,i) + pred_k % ozone(11,level) *&
                & 2 * geom%seczen * pred % ozone(10,level)

           ow_k(level,i) = ow_k(level,i) + pred_k % ozone(10,level) * geom%seczen

           zsqrt=Sqrt(pred % ozone(10,level))
           sec_or_k(level,i) = sec_or_k(level,i) + pred_k % ozone(9,level) *&
                & zsqrt
           ow_k(level,i)    = ow_k(level,i)      + pred_k % ozone(9,level) *&
                & 0.5_JPRB * geom%seczen * pred % ozone(1,level) &
                & / zsqrt

           ow_k(level,i) = ow_k(level,i) + pred_k % ozone(8,level) *&
                & pred % ozone(1,level)
           or_k(level,i) = or_k(level,i) + pred_k % ozone(8,level) *&
                & pred % ozone(10,level)

           sec_or_k(level,i) = sec_or_k(level,i) + pred_k % ozone(7,level) *&
                & 1.5_JPRB * pred % ozone(2,level) / pred % ozone(10,level)
           ow_k(level,i)     = ow_k(level,i)     - pred_k % ozone(7,level) *&
                & geom%seczen * pred % ozone(2,level)**3 / pred % ozone(10,level)**2

           or_k(level,i) =  or_k(level,i) + pred_k % ozone(6,level) *&
                & 2 * pred % ozone(8,level)
           ow_k(level,i) = ow_k(level,i)  + pred_k % ozone(6,level) *&
                & pred % ozone(4,level) / geom%seczen

           sec_or_k(level,i) = sec_or_k(level,i) + pred_k % ozone(5,level) *&
                & 0.5_JPRB * pred % ozone(3,level) /&
                & ( pred % ozone(1,level) *pred % ozone(2,level))
           dto_k(level,i) = dto_k(level,i)       + pred_k % ozone(5,level) *&
                & pred % ozone(2,level)

           sec_or_k(level,i) = sec_or_k(level,i) + pred_k % ozone(4,level) *&
                & 2 * pred % ozone(1,level)

           sec_or_k(level,i) = sec_or_k(level,i) + pred_k % ozone(3,level) *&
                & pred % ozone(3,level) / pred % ozone(1,level)
           dto_k(level,i)    = dto_k(level,i)    + pred_k % ozone(3,level) *&
                & pred % ozone(1,level)

           sec_or_k(level,i) = sec_or_k(level,i) + pred_k % ozone(2,level) *&
                & 0.5_JPRB / pred % ozone(2,level)

           sec_or_k(level,i) = sec_or_k(level,i) + pred_k % ozone(1,level)

           or_k(level,i) = or_k(level,i) + sec_or_k(level,i) * geom%seczen
        End Do
     End Do
  Endif

  !5.2 water vapour  ( numbers in right hand are predictor numbers
  ! in the reference document for RTTOV7 (science and validation report)
  !----------------

  Do i = 1, nchannels
     freq=polarisations(i,2)
     j = lprofiles( freq )
     
     ! prof   => profiles(j)    (use of variables derived from prof )
     geom   => angles(j)
     pred   => predictors(j)
     prof_k => profiles_k(i)   ! only for loop on levels
     pred_k => predictors_k(i)

     Do level = 1,prof_k % nlevels

        sec_wr(level, j)    = geom%seczen * wr(level, j)

        ! X15  (15)
        wr_k(level,i) = wr_k(level,i) + pred_k % watervapour(15,level) *&
             & pred % watervapour(15,level) * geom%seczen  * &
             & 2 / pred % watervapour(1,level)
        tr_k(level,i) = tr_k(level,i) - pred_k % watervapour(15,level) *&
             & pred % watervapour(15,level) * geom%seczen  * &
             & 4 * pred % watervapour(14,level)  / &
             & pred % watervapour(5,level)

        ! X14  (14)
        sec_wr_k(level,i) = sec_wr_k(level,i) + pred_k % watervapour(14,level) *&
             & 2 * pred % watervapour(14,level) / pred % watervapour(1,level)
        tr_k(level,i)     = tr_k(level,i)     - pred_k % watervapour(14,level) *&
             & pred % watervapour(14,level)**2 / (geom%seczen * wr(level,j) * wr(level,j))

        ! X13 (2)
        zsqrt=Sqrt( pred % watervapour(13,level))
        ww_k(level,i) =  ww_k(level,i) + pred_k % watervapour(13,level) *&
             & 2 * geom%seczen * zsqrt

        ! X12 (3)
        ww_k(level,i) =  ww_k(level,i) + pred_k % watervapour(12,level) *&
             & 4 * geom%seczen * pred % watervapour(12,level) &
             & / zsqrt

        ! X11 (10)
        sec_wr_k(level,i) = sec_wr_k(level,i) + pred_k % watervapour(11,level) *&
             & Abs(dt(level,j)) * dt(level,j)
        dt_k(level,i) = dt_k(level,i)         + pred_k % watervapour(11,level) *&
             & 2 * sec_wr(level,j) * Abs(dt(level,j))

        ! X10 (9)
        sec_wr_k(level,i) = sec_wr_k(level,i) + pred_k % watervapour(10,level) *&
             & 4 *  pred % watervapour(9,level)

        ! X9 (8)
        sec_wr_k(level,i) = sec_wr_k(level,i) + pred_k % watervapour(9,level) *&
             & 3 * pred % watervapour(5,level)

        ! X8 (13)
        wr_k(level,i) = wr_k(level,i) + pred_k % watervapour(8,level) *&
             & geom%seczen * pred % watervapour(8,level) * &
             & 1.5_JPRB / pred % watervapour(1,level)
        ww_k(level,i) = ww_k(level,i) - pred_k % watervapour(8,level) *&
             & geom%seczen * pred % watervapour(8,level) / &
             & pred % watervapour(13,level)**0.5_JPRB

        ! X7 (6)
        sec_wr_k(level,i) = sec_wr_k(level,i) + pred_k % watervapour(7,level) *&
             & 0.25_JPRB / pred % watervapour(7,level)**3

        ! X6 (11)
        dt_k(level,i)     = dt_k(level,i)     + pred_k % watervapour(6,level) *&
             & pred % watervapour(2,level)
        sec_wr_k(level,i) = sec_wr_k(level,i) + pred_k % watervapour(6,level) *&
             & 0.5_JPRB * pred % watervapour(6,level) / pred % watervapour(1,level)

        ! X5 (1)
        sec_wr_k(level,i) = sec_wr_k(level,i) + pred_k % watervapour(5,level) *&
             & 2 * pred % watervapour(1,level)

        ! X4 (4)
        sec_wr_k(level,i) = sec_wr_k(level,i) + pred_k % watervapour(4,level) *&
             & pred % watervapour(4,level) / pred % watervapour(1,level)
        dt_k(level,i)     = dt_k(level,i)     + pred_k % watervapour(4,level) *&
             & pred % watervapour(1,level)

        ! X3 (12)
        sec_wr_k(level,i) = sec_wr_k(level,i) + pred_k % watervapour(3,level) *&
             & 2 * pred % watervapour(3,level) / pred % watervapour(1,level)
        ww_k(level,i)     = ww_k(level,i)     - pred_k % watervapour(3,level) *&
             & pred % watervapour(3,level)**2 * geom%seczen &
             & / pred % watervapour(5,level)

        ! X2 (5)
        sec_wr_k(level,i) = sec_wr_k(level,i) + pred_k % watervapour(2,level) *&
             & 0.5_JPRB / pred % watervapour(2,level)

        ! X1 (7)
        sec_wr_k(level,i) = sec_wr_k(level,i) + pred_k % watervapour(1,level)

        wr_k(level,i) = wr_k(level,i) + sec_wr_k(level,i) * geom%seczen
     End Do
  End Do

  !5.1 mixed gases
  !---------------

  ! X10
  Do i = 1, nchannels
     freq=polarisations(i,2)
     j = lprofiles( freq )

     geom   => angles(j)
     pred   => predictors(j)
     prof_k => profiles_k(i)
     pred_k => predictors_k(i)

     tw_k(2:prof_k % nlevels,i) = tw_k(2:prof_k % nlevels,i) +&
          & pred_k % mixedgas(10,2:prof_k % nlevels) *&
          & 0.25_JPRB * geom % seczen_sq &
          & / pred % mixedgas(10,2:prof_k % nlevels)**3
  End Do

  ! X9
  ! X8
  Do i = 1, nchannels
     freq=polarisations(i,2)
     j = lprofiles( freq )
     
     ! prof   => profiles(j)   (use of variables derived from prof )
     geom   => angles(j)
     pred   => predictors(j)
     prof_k => profiles_k(i)
     pred_k => predictors_k(i)

     Do level = 1,prof_k % nlevels

        tw_k(level,i) = tw_k(level,i) + pred_k % mixedgas(8,level) *&
             & geom % seczen / pred % mixedgas(5,level)
        tr_k(level,i) = tr_k(level,i) - pred_k % mixedgas(8,level) *&
             & pred % mixedgas(7,level) / pred % mixedgas(6,level)

        ! X7
        tw_k(level,i) = tw_k(level,i) + pred_k % mixedgas(7,level) *&
             & geom % seczen

        ! X6
        tr_k(level,i) = tr_k(level,i) + pred_k % mixedgas(6,level) *&
             & 2._JPRB * tr(level,j)

        ! X5
        tr_k(level,i) = tr_k(level,i) + pred_k % mixedgas(5,level)

        ! X4
        tr_k(level,i) = tr_k(level,i) + pred_k % mixedgas(4,level) *&
             & 2._JPRB * pred % mixedgas(3,level)

        ! X3
        tr_k(level,i) = tr_k(level,i) + pred_k % mixedgas(3,level) *&
             & geom % seczen

        ! X2
        ! X1
     End Do
  End Do

  Do i = 1, nchannels
     freq=polarisations(i,2)
     j = lprofiles( freq )

     prof   => profiles(j)
     geom   => angles(j)
     pred   => predictors(j)
     prof_k => profiles_k(i)
     pred_k => predictors_k(i)

     sum1 = 0._JPRB
     If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
        Do level = prof_k % nlevels, 1, -1
           sum1 = sum1 + ow_k ( level, i ) / sum2_ow(level, j)
           o_k( level, i) = o_k( level, i) + sum1 * coef % dpp( level )
        End Do
     Else
        o_k(:,i) = 0._JPRB
     Endif

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


     If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
        o_k(:,i) = o_k(:,i) + or_k(:,i) / coef % ostar(:)
     Endif

     w_k(:,i) = w_k(:,i) + wr_k(:,i) / coef % wstar(:)

     t_k(:,i) = t_k(:,i) + tr_k(:,i) / coef % tstar(:)


     If ( prof % ozone_Data .And. coef % nozone > 0 ) Then
        t_k(:,i) = t_k(:,i) + dto_k(:,i)
     Endif

     t_k(:,i) = t_k(:,i) + dt_k(:,i)

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
  End Do

  nullify ( geom )
  nullify ( prof )
  nullify ( prof_k )
  nullify ( pred )
  nullify ( pred_k )

End Subroutine rttov_setpredictors_k
