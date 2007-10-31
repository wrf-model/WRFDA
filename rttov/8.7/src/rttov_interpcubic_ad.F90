Subroutine rttov_interpcubic_ad (&
    & coef_scatt,        &! in
    & tab,               &! in
    & itemp,             &! in
    & iwc,               &! in
    & itype,             &! in
    & temp,              &! in
    & wc,                &! in
    & val,               &! out
    & temp_ad,           &! inout
    & wc_ad,             &! inout
    & val_ad)             ! inout

  ! Description:
  ! interface to Numerical Recipes for cubic interpolation
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
  ! - Chevallier, F., and P. Bauer, 2003:
  !     Model rain and clouds over oceans: comparison with SSM/I observations.
  !     Mon. Wea. Rev., 131, 1240-1255.
  ! - Moreau, E., P. Bauer and F. Chevallier, 2002: Microwave radiative transfer modeling in clouds and precipitation.
  !     Part II: Model evaluation.
  !     NWP SAF Report No. NWPSAF-EC-TR-006, 27 pp.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       09/2002   Initial version (E. Moreau)
  !  1.1       05/2003   RTTOV-7.3 compatible (F. Chevallier)
  !  1.2       07/2003   E. Moreau
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:
  Use rttov_types, Only :    &
      & rttov_scatt_coef

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_polcoe.interface"

  !subroutine arguments:
  Type(rttov_scatt_coef),   Intent(in)                 :: coef_scatt   ! RTTOV_SCATT Coefficients
  Real(Kind=jprb),          Intent(in), Dimension(2,4) :: tab          ! part of the mie-tables [2x4]
                                                                       ! = mie-tab(itemp:itemp+1,iwc-1:iwc+2)
  Integer(Kind=jpim),       Intent( in)                :: itemp, iwc, itype
  Real(Kind=jprb),          Intent( in)                :: temp, wc     ! coord. of the interp. variable.
  Real(Kind=jprb),          Intent(out)                :: val          ! interpolate  value
  Real(Kind=jprb),          Intent(inout)              :: temp_ad, wc_ad  ! coord. of the interp. variable.
  Real(Kind=jprb),          Intent(inout)              :: val_ad          ! interpolate  value

  !local
  Real(Kind=jprb), Dimension(2)   :: x0, y0, cof0
  Real(Kind=jprb), Dimension(4)   :: x, y, coef
  Real(Kind=jprb)                 :: xx, t, u
  Integer(Kind=jpim)              :: i

  !- End of header --------------------------------------------------------

  !# linear interpolation w.r.t temperature
  do i = 1, 4
     t = (temp - real(itemp))
     y(i) = (1._JPRB - t) * tab(1,i) + t * tab(2,i)
  enddo

  x(1) = 10._JPRB**( (real(iwc-2+1)+coef_scatt % offset_water)/coef_scatt % scale_water)
  do i = 2, 4
     x(i) = x(i-1) * coef_scatt % from_scale_water
  enddo

  !# cubic interpolation w.r.t  water content
  call rttov_polcoe(x, y, 4, coef)

  !# interpolate value
  xx = 10._JPRB**( (wc + coef_scatt % offset_water)/coef_scatt % scale_water )
  val = coef(1) + coef(2) * xx + coef(3) * xx**2._JPRB + coef(4) * xx**3._JPRB

  !# interpolate derivative of the value w.r.t  water content

  !# interpolate derivative of the value / temp
    !# linear interpolation w.r.t  water content
  do i = 1, 2
     u = (wc - real(iwc))
     y0(i) = (1._JPRB - u) * tab(i,2) + u * tab(i,3)
     if (itype == 1) x0(i) = real(itemp-1+i) + coef_scatt % offset_temp_rain ![K]
     if (itype == 2) x0(i) = real(itemp-1+i) + coef_scatt % offset_temp_sp   ![K]
     if (itype == 3) x0(i) = real(itemp-1+i) + coef_scatt % offset_temp_liq  ![K]
     if (itype == 4) x0(i) = real(itemp-1+i) + coef_scatt % offset_temp_ice  ![K]
  enddo

    !# linear interpolation w.r.t temp
  call rttov_polcoe(x0, y0, 2, cof0)

  !-----------------------------------------------------------
  ! AD code
  !-----------------------------------------------------------

  temp_ad = temp_ad + cof0(2) * val_ad
  wc_ad   = wc_ad + ( coef(2) + 2._JPRB * coef(3) * xx + 3._JPRB * coef(4) * xx**2._JPRB ) * val_ad

  val_ad  = 0._JPRB


End subroutine rttov_interpcubic_ad
