!
Subroutine rttov_checkinput( &
     & prof,        &! in
     & coef,        &! in
     & errorstatus ) ! out
  ! Description:
  ! Check input profile/angles
  ! (i)  Are physically realistic
  ! (ii) Profile values are within the basis set used to
  !      generate the coefficients
  ! Unphysical values return a fatal error status
  ! Profile values outside the basis set return a warning status
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
  ! Method: Check input profiles with fixed limits specified
  !         in constants and coeff file.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1       02/01/2003  More comments added (R Saunders)
  !  1.2       29/01/2003  More tests and add CO2 (P Brunel)
  !  1.3       27/06/2005  Uncommented water vapor and ozone profile checks  (R Saunders)
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
       & errorstatus_success ,&
       & errorstatus_warning ,&
       & errorstatus_fatal   ,&
       & nsurftype           ,&
       & gas_id_watervapour  ,&
       & gas_id_ozone        ,&
       & gas_id_co2          ,&
       & tmax                ,&
       & tmin                ,&
       & qmax                ,&
       & qmin                ,&
       & o3max               ,&
       & o3min               ,&
       & co2max              ,&
       & co2min              ,&
       & clwmax              ,&
       & clwmin              ,&
       & pmax                ,&
       & pmin                ,&
       & wmax                ,&
       & zenmax              ,&
       & ctpmax              ,&
       & ctpmin

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type


  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"

  ! subroutine arguments
  ! scalar arguments with intent(in):
  Type(profile_Type), Intent (in) :: prof    ! input profiles
  Type( rttov_coef ), Intent (in) :: coef    ! coefficients

  ! scalar arguments with intent(out):
  Integer(Kind=jpim), Intent (out) :: errorstatus       ! return code



  !local variables:
  Real(Kind=jprb)    :: wind
  Real(Kind=jprb)    :: dp( coef % nlevels )
  Character (len=80) :: errMessage
  Character (len=16) :: NameOfRoutine = 'rttov_checkinput'
  Integer(Kind=jpim) :: firstlevel,ilev
  Integer(Kind=jpim) :: ig          ! gas number
  !- End of header --------------------------------------------------------

  !-------------
  !0. Initialize
  !-------------

  errorstatus = errorstatus_success

  !determine first pressure level above the surface (note levels are top down)
  Do firstlevel = coef % nlevels, 1, -1
     If ( coef % ref_prfl_p(firstlevel) <= prof % s2m % p ) Exit
  End Do

  ! Compare Profile Levels and Model Levels
  If ( prof % nlevels /= coef % nlevels ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "invalid profile number of levels" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  dp(:) = abs ( coef % ref_prfl_p(:) - prof % p(:) ) / coef % ref_prfl_p(:)
  If ( Any( dp > 0.01_JPRB ) ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "invalid profile pressure levels" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  !------------------------------
  !1. Check for unphysical values
  !------------------------------
  ! zenith angle
  If ( prof % zenangle > zenmax .Or. &
       & prof % zenangle < 0._JPRB          ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "invalid zenith angle" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  ! Cloud Top Pressure
  If ( prof % ctp > ctpmax .Or. &
       & prof % ctp < ctpmin      ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "invalid cloud top pressure" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  ! Cloud Fraction
  If ( prof % cfraction > 100._JPRB .Or. &
       & prof % cfraction < 0._JPRB        ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "invalid cloud fraction" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  !1.1 surface variables
  !---------------------

  ! Pressure
  If ( prof % s2m % p > pmax .Or. &
       & prof % s2m % p < pmin      ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "invalid surface pressure" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  ! 2m air temperature
  If ( prof % s2m % t    > tmax .Or. &
       & prof % s2m % t    < tmin ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "invalid 2m air temperature" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  ! 2m water vapour
  If ( prof % s2m % q > qmax .Or. &
       & prof % s2m % q < qmin      ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "invalid 2m water vapour" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  !  surface wind speed
  wind = sqrt(&
        & prof % s2m % u * prof % s2m % u + &
        & prof % s2m % v * prof % s2m % v   )
  If ( wind > wmax .Or. &
       & wind < 0._JPRB      ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "invalid 2m wind speed" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  ! surface skin temperature
  If ( prof % skin % t > tmax .Or. &
       & prof % skin % t < tmin      ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "invalid skin surface temperature" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  ! surface type
  If ( prof % skin % surftype < 0 .Or. prof % skin % surftype > nsurftype ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "some invalid surface type" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  !1.2 atmospheric variables
  !-------------------------

  ! temperature
  If ( Any( prof % t(1:firstlevel) > tmax ) .Or. &
       & Any( prof % t(1:firstlevel) < tmin )      ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "some invalid atmospheric temperature" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  ! water vapour
  If ( Any( prof % q(1:firstlevel) > qmax ) .Or. &
       Any( prof % q(1:firstlevel) < qmin )      ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "some invalid atmospheric water vapour" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  ! ozone
  If ( prof % ozone_Data .And. coef % nozone > 0) Then
     If ( Any( prof % o3(1:firstlevel) > o3max ) .Or. &
          Any( prof % o3(1:firstlevel) < o3min )      ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "some invalid atmospheric ozone" )' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     End If
  Endif

  ! CO2
  If ( prof % co2_Data .And. coef % nco2 > 0) Then
     If ( Any( prof % co2(1:firstlevel) > co2max ) .Or. &
          & Any( prof % co2(1:firstlevel) < co2min )      ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "some invalid atmospheric CO2" )' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     End If
  Endif

  ! cloud liquid water
  If ( prof % clw_Data ) Then
     If ( Any( prof % clw(1:firstlevel) > clwmax ) .Or. &
          & Any( prof % clw(1:firstlevel) < clwmin )      ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "some invalid cloud liquid water" )' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     End If
  Endif

  !-----------------------------
  !2. Check against basis values
  !-----------------------------


  If ( errorstatus /= errorstatus_fatal ) Then

     If ( Any( prof % t(1:firstlevel) > coef % lim_prfl_tmax(1:firstlevel) ) .Or. &
          & Any( prof % t(1:firstlevel) < coef % lim_prfl_tmin(1:firstlevel) ) ) Then
        errorstatus = errorstatus_warning
        Write( errMessage, '( "some atmospheric temperature outside coef. limits" )' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     End If

     ig = coef % fmv_gas_pos( gas_id_watervapour )
     If ( Any( prof % q(1:firstlevel) > coef % lim_prfl_gmax(1:firstlevel, ig) ) .Or. &
          Any( prof % q(1:firstlevel) < coef % lim_prfl_gmin(1:firstlevel, ig) ) ) Then
        errorstatus = errorstatus_warning
        Write( errMessage, '( "some atmospheric water vapour outside coef. limits" )' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     End If 

     If ( prof % ozone_Data .And. coef % nozone > 0) Then
        ig = coef % fmv_gas_pos( gas_id_ozone )
        If ( Any( prof % o3(1:firstlevel) > coef % lim_prfl_gmax(1:firstlevel, ig) ) .Or. &
             Any( prof % o3(1:firstlevel) < coef % lim_prfl_gmin(1:firstlevel, ig) ) ) Then
           errorstatus = errorstatus_warning
           Write( errMessage, '( "some atmospheric ozone outside coef. limits" )' )
           Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        End If
     End If

     If ( prof % co2_Data .And. coef % nco2 > 0) Then
        ig = coef % fmv_gas_pos( gas_id_co2 )
        If ( Any( prof % co2(1:firstlevel) > coef % lim_prfl_gmax(1:firstlevel, ig) ) .Or. &
             & Any( prof % co2(1:firstlevel) < coef % lim_prfl_gmin(1:firstlevel, ig) ) ) Then
           errorstatus = errorstatus_warning
           Write( errMessage, '( "some atmospheric CO2 outside coef. limits" )' )
           Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        End If
     End If

  End If


End Subroutine rttov_checkinput
