Subroutine rttov_intex_ad( &
      & klevi,   &! in
      & klevf,   &! in
      & presi,   &! inout  AD
      & presf,   &! inout  AD
      & veci,    &! inout  AD
      & vecf,    &! inout  AD
      & presi_d, &! in
      & presf_d, &! in
      & veci_d,  &! in
      & vecf_d  ) ! out

  !
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
  ! Description:
  ! AD of routine
  ! to interpolate the array vec from the presi levels to presf levels
  !
  ! Method:
  ! Linear interpolation in ln(P)
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date        Comment
  ! -------   ----        -------
  ! 1         03/2001     Original code (F. Chevallier)
  ! 1.1       29/03/2005  Add end of header comment (J. Cameron)

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !
  ! Subroutine arguments
  !
  Integer(Kind=jpim), Intent(in) :: klevi      ! number of levels of the initial grid
  Integer(Kind=jpim), Intent(in) :: klevf      ! number of levels of the final grid
  !
  ! AD arrays
  Real(Kind=jprb), Intent(inout), Dimension(klevi) :: presi ! initial grid
  Real(Kind=jprb), Intent(inout), Dimension(klevf) :: presf ! final grid
  Real(Kind=jprb), Intent(inout), Dimension(klevi) :: veci  ! initial vec array
  Real(Kind=jprb), Intent(inout), Dimension(klevf) :: vecf  ! final vec array
  ! Direct model arrays
  Real(Kind=jprb), Intent(in),  Dimension(klevi) :: presi_d ! initial grid
  Real(Kind=jprb), Intent(in),  Dimension(klevf) :: presf_d ! final grid
  Real(Kind=jprb), Intent(in),  Dimension(klevi) :: veci_d  ! initial vec array
  Real(Kind=jprb), Intent(out), Dimension(klevf) :: vecf_d  ! final vec array

  !


  ! Local scalars :
  !
  Integer(Kind=jpim) :: jki, jkf
  Real(Kind=jprb)    :: slope, t1, t2, p1, p2, lp1, lp2
  Real(Kind=jprb)    :: slope_d, t1_d, t2_d, p1_d, p2_d, lp1_d, lp2_d
  Real(Kind=jprb), Dimension(klevi,klevf) :: slope_d2
  Real(Kind=jprb), Dimension(klevf,klevi+1) :: zradt
  Real(Kind=jprb), Dimension(klevi)  :: lpresi
  Real(Kind=jprb), Dimension(klevf)  :: lpresf
  Real(Kind=jprb), Dimension(klevi)  :: lpresi_d
  Real(Kind=jprb), Dimension(klevf)  :: lpresf_d
  !

  !- End of header --------------------------------------------------------

  !
  ! -- Direct computation
  !

  vecf_d(:) = -1000._JPRB
  zradt(:,:) = -1000._JPRB
  lpresi_d(:) = Log( presi_d(:) )
  lpresf_d(:) = Log( presf_d(:) )

  Do jkf = 1,klevf
     Do jki = 1,klevi-1
        p1_d = presi_d(jki)
        p2_d = presi_d(jki+1)
        lp1_d = lpresi_d(jki)
        lp2_d = lpresi_d(jki+1)
        If (presf_d(jkf) >= p1_d .And. presf_d(jkf) < p2_d) Then
           t1_d = veci_d(jki)
           t2_d = veci_d(jki+1)
           slope_d = (t1_d-t2_d)/(lp1_d-lp2_d)
           If (t2_d == 0._JPRB) Then
              slope_d2(jki,jkf) = 0._JPRB
           Else
              slope_d2(jki,jkf) = slope_d
           Endif
           vecf_d(jkf) = t1_d + slope_d2(jki,jkf)*(lpresf_d(jkf)-lp1_d)
           zradt(jkf,jki) = 0._JPRB
           !
        Else If (jki == 1 .And. presf_d(jkf) < p1_d) Then
           vecf_d(jkf) = veci_d(jki)
           zradt(jkf,jki) = 0._JPRB
        Else If (jki == (klevi-1) .And. vecf_d(jkf) == -1000._JPRB ) Then
           vecf_d(jkf) = veci_d(klevi)
        End If
        zradt(jkf,jki+1) = zradt(jkf,jki)
     End Do
  End Do

  !
  ! -- Adjoint computation
  !

  lpresi(:) = 0._JPRB
  lpresf(:) = 0._JPRB
  Do jkf = klevf,1,-1
     Do jki = klevi-1, 1, -1
        p1 = 0._JPRB
        p2 = 0._JPRB
        lp1 = 0._JPRB
        lp2 = 0._JPRB
        p1_d = presi_d(jki)
        p2_d = presi_d(jki+1)
        lp1_d = lpresi_d(jki)
        lp2_d = lpresi_d(jki+1)
        If (presf_d(jkf) >= p1_d .And. presf_d(jkf) < p2_d) Then
           t1_d = veci_d(jki)
           t2_d = veci_d(jki+1)
           t1 = 0._JPRB
           t2 = 0._JPRB
           slope = 0._JPRB
           !
           t1 = t1 + vecf(jkf)
           slope = slope + (lpresf_d(jkf)-lp1_d) * vecf(jkf)
           lpresf(jkf) = lpresf(jkf) + slope_d2(jki,jkf) * vecf(jkf)
           lp1 = lp1 - slope_d2(jki,jkf) * vecf(jkf)
           vecf(jkf) = 0._JPRB
           If (t2_d == 0._JPRB) slope = 0._JPRB
           t1 = t1 + slope / (lp1_d-lp2_d)
           t2 = t2 - slope / (lp1_d-lp2_d)
           lp1 = lp1 - slope * (t1_d-t2_d)/(lp1_d-lp2_d)/(lp1_d-lp2_d)
           lp2 = lp2 + slope * (t1_d-t2_d)/(lp1_d-lp2_d)/(lp1_d-lp2_d)
           veci(jki+1) = veci(jki+1) + t2
           t2 = 0._JPRB
           veci(jki) = veci(jki) + t1
           t1 = 0._JPRB
        Else If (jki == 1 .And. presf_d(jkf) < p1_d) Then
           veci(jki) = veci(jki) + vecf(jkf)
           vecf(jkf) = 0._JPRB
        Else If (jki == (klevi-1) .And. zradt(jkf,jki) == -1000._JPRB ) Then
           veci(klevi) = veci(klevi) + vecf(jkf)
           vecf(jkf) = 0._JPRB
        End If
        lpresi(jki+1) = lpresi(jki+1) + lp2
        lpresi(jki) = lpresi(jki) + lp1
        presi(jki+1) = presi(jki+1) + p2
        presi(jki) = presi(jki) + p1
     End Do
  End Do

  presf(:) = presf(:) + lpresf(:)/presf_d(:)
  presi(:) = presi(:) + lpresi(:)/presi_d(:)



End Subroutine rttov_intex_ad
