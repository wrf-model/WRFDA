!
Subroutine rttov_setgeometry( &
     & prof,   &! in
     & coef,   &! in
     & angles ) ! out
  ! Description:
  ! compute all profile related viewing geometry
  ! The only profile input value is profile%zenangle (zenith angle)
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
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1       02/01/2003  Added more comments (R Saunders)
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
       & deg2rad

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type   ,&
       & geometry_Type


  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Type(profile_Type),  Intent(in)  :: prof   ! profile
  Type(rttov_coef)  ,  Intent(in)  :: coef   ! coefficient
  Type(geometry_Type), Intent(out) :: angles ! angles


  ! local

  !- End of header --------------------------------------------------------

  !Notes on notation:
  ! zen  => zenith angle
  !   (definition: angle at surface between view path to satellite and zenith)
  ! view => view angle
  !   (definition: angle at the satellite between view path and nadir)
  ! _sq = square of given value
  ! _sqrt = square root of given value
  ! _minus1 = given value - 1
  ! trigonometric function abbreviations have their usual meanings

  angles % sinzen           = Sin( prof%zenangle * deg2rad )
  angles % sinzen_sq        = angles%sinzen * angles%sinzen
  angles % coszen           = Cos( prof%zenangle * deg2rad )
  angles % coszen_sq        = angles%coszen * angles%coszen
  angles % seczen           = 1.0_JPRB/Abs(angles%coszen)
  angles % seczen_sq        = angles%seczen * angles%seczen
  angles % seczen_sqrt      = Sqrt(angles%seczen)
  angles % seczen_minus1    = angles%seczen - 1.0_JPRB
  angles % seczen_minus1_sq = angles%seczen_minus1 * angles%seczen_minus1
  angles % sinview          = angles%sinzen / coef % ratoe
  angles % sinview_sq       = angles%sinview * angles%sinview
  angles % cosview_sq       = 1.0_JPRB - angles%sinview_sq
  angles % normzen          = prof%zenangle / 60.0_JPRB        !normalized zenith angle



End Subroutine rttov_setgeometry
