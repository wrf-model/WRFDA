Subroutine rttov_intex_tl( &
      & klevi,   &! in
      & klevf,   &! in
      & presi,   &! in     TL
      & presf,   &! in     TL
      & veci,    &! in     TL
      & vecf,    &! inout  TL
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
  ! to interpolate the array vec from the presi levels to the presf levels
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
  ! TL arrays
  Real(Kind=jprb), Intent(in),    Dimension(klevi) :: presi  ! initial grid
  Real(Kind=jprb), Intent(in),    Dimension(klevf) :: presf  ! final grid
  Real(Kind=jprb), Intent(in),    Dimension(klevi) :: veci   ! initial vec array
  Real(Kind=jprb), Intent(inout), Dimension(klevf) :: vecf   ! final vec array
  ! Direct model arrays
  Real(Kind=jprb), Intent(in),  Dimension(klevi) :: presi_d  ! initial grid
  Real(Kind=jprb), Intent(in),  Dimension(klevf) :: presf_d  ! final grid
  Real(Kind=jprb), Intent(in),  Dimension(klevi) :: veci_d   ! initial vec array
  Real(Kind=jprb), Intent(out), Dimension(klevf) :: vecf_d   ! final vec array

  !


  ! Local scalars :
  !
  Integer(Kind=jpim) :: jki, jkf
  Real(Kind=jprb)    :: slope, t1, t2, p1, p2, lp1, lp2
  Real(Kind=jprb)    :: slope_d, t1_d, t2_d, p1_d, p2_d, lp1_d, lp2_d
  !Real, Dimension(klevf,klevi+1) :: zradt
  !Real, Dimension(klevi)  :: lpresi
  Real(Kind=jprb), Dimension(klevf)  :: lpresf
  Real(Kind=jprb), Dimension(klevi)  :: lpresi_d
  Real(Kind=jprb), Dimension(klevf)  :: lpresf_d
  !

  !- End of header --------------------------------------------------------

  !
  ! -- Direct computation
  !

  vecf(:) = -1000._JPRB
  vecf_d(:) = -1000._JPRB
  !zradt(:,:) = -1000.
  !lpresi(:) = presi(:)/presi_d(:)
  lpresf(:) = presf(:)/presf_d(:)
  lpresi_d(:) = Log( presi_d(:) )
  lpresf_d(:) = Log( presf_d(:) )

  Do jkf = 1,klevf
     Do jki = 1,klevi-1
        p1 = presi(jki)
        p1_d = presi_d(jki)
        p2 = presi(jki+1)
        p2_d = presi_d(jki+1)
        lp1 = p1/p1_d
        !lp1_d = Log(p1_d)
        lp1_d = lpresi_d(jki)
        lp2 = p2/p2_d
        !lp2_d = Log(p2_d)
        lp2_d = lpresi_d(jki+1)
        If (presf_d(jkf) >= p1_d .And. presf_d(jkf) < p2_d) Then
           t1 = veci(jki)
           t1_d = veci_d(jki)
           t2 = veci(jki+1)
           t2_d = veci_d(jki+1)
           slope = ((t1-t2)*(lp1_d-lp2_d)-(t1_d-t2_d)*(lp1-lp2))&
                & /(lp1_d-lp2_d)/(lp1_d-lp2_d)
           slope_d = (t1_d-t2_d)/(lp1_d-lp2_d)
           If (t2_d == 0._JPRB) Then
              slope = 0._JPRB
              slope_d = 0._JPRB
           Endif
           vecf(jkf) = t1 + slope_d*(lpresf(jkf)-lp1) + slope*(lpresf_d(jkf)-lp1_d)
           vecf_d(jkf) = t1_d + slope_d*(lpresf_d(jkf)-lp1_d)
           !zradt(jkf,jki) = 0.
           !
        Else If (jki == 1 .And. presf_d(jkf) < p1_d) Then
           vecf(jkf) = veci(jki)
           vecf_d(jkf) = veci_d(jki)
           !zradt(jkf,jki) = 0.
        Else If (jki == (klevi-1) .And. vecf_d(jkf) == -1000._JPRB ) Then
           vecf(jkf) = veci(klevi)
           vecf_d(jkf) = veci_d(klevi)
        End If
        !zradt(jkf,jki+1) = zradt(jkf,jki)
     End Do
  End Do



End Subroutine rttov_intex_tl
