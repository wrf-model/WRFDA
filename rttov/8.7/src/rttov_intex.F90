!
Subroutine rttov_INTEX(    &
      & klevi ,  &! in
      & klevf ,  &! in
      & presi ,  &! in
      & presf ,  &! in
      & veci  ,  &! in
      & vecf  )   ! out

  ! Description:
  ! To interpolate the array vec from the presi levels to presf levels
  !
  ! Copyright:
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
  !
  ! Method:
  ! Linear interpolation in ln(P)
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date        Comment
  ! -------   ----        -------
  ! 1         09/2002     ECMWF
  ! 1.0       04/12/2003  Optimisation (J Hague and D Salmond ECMWF)

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !
  ! Subroutine arguments
  !
  Integer(Kind=jpim), Intent(in) :: klevi      ! number of levels of the initial grid
  Integer(Kind=jpim), Intent(in) :: klevf      ! number of levels of the final grid
  !
  Real(Kind=jprb), Intent(in), Dimension(klevi)  :: presi ! initial grid
  Real(Kind=jprb), Intent(in), Dimension(klevf)  :: presf ! final grid
  !
  Real(Kind=jprb), Intent(in), Dimension(klevi)  :: veci  ! initial vec array
  Real(Kind=jprb), Intent(out), Dimension(klevf) :: vecf  ! final vec array
  !

  ! Local scalars :
  !
  Integer(Kind=jpim) :: jki, jkf
  Integer(Kind=jpim) :: jhi, jlo
  Real(Kind=jprb)    :: slope, t1, t2, p1, p2, lp1, lp2
  Real(Kind=jprb), Dimension(klevi)  :: lpresi
  Real(Kind=jprb), Dimension(klevf)  :: lpresf
  !
  !- End of header --------------------------------------------------------

  vecf(:) = -1000._JPRB
  lpresi(:) = Log( presi(:) )
  lpresf(:) = Log( presf(:) )

  do jkf = 1,klevf-1
    if(presf(jkf+1) < presf(jkf)) exit
  enddo

  do jki = 1,klevi-1
    if(presi(jki+1) < presi(jki)) exit
  enddo

  if(jki /= klevi .OR. jkf /= klevf) THEN
  if(jki /= klevi) write(0,*) (presi(jki),jki=1,klevi)
  if(jkf /= klevf) write(0,*) (presf(jkf),jkf=1,klevf)

  Do jkf = 1,klevf
     Do jki = 1,klevi-1
        p1 = presi(jki)
        p2 = presi(jki+1)
        lp1 = lpresi(jki)
        lp2 = lpresi(jki+1)
        If (presf(jkf) >= p1 .And. presf(jkf) < p2) Then
           t1 = veci(jki)
           t2 = veci(jki+1)
           slope = (t1-t2)/(lp1-lp2)
           If (t2 == 0._JPRB) slope = 0._JPRB
           vecf(jkf) = t1 + slope*(lpresf(jkf)-lp1)
           !
        Else If (jki == 1 .And. presf(jkf) < p1) Then
           vecf(jkf) = veci(jki)
        Else If (jki == (klevi-1) .And. vecf(jkf) == -1000._JPRB ) Then
           vecf(jkf) = veci(klevi)
        End If
     End Do
  End Do

  ELSE

  jkf = 1
  Do While (presf(jkf) < presi(1))
    vecf(jkf) = veci(1)
    jkf = jkf+1
  End Do
  jlo=jkf

  jkf = klevf
  Do While (presf(jkf) >= presi(klevi))
    vecf(jkf) = veci(klevi)
    jkf = jkf-1
  End Do
  jhi = jkf

  jki = 1
  do jkf = jlo,jhi
    Do While (presf(jkf) >= presi(jki+1))
      jki=jki+1
    End Do
    p1 = presi(jki)
    p2 = presi(jki+1)
    lp1 = lpresi(jki)
    lp2 = lpresi(jki+1)
    t1 = veci(jki)
    t2 = veci(jki+1)
    slope = (t1-t2)/(lp1-lp2)
    if (t2 == 0._JPRB) slope = 0._JPRB
    vecf(jkf) = t1 + slope*(lpresf(jkf)-lp1)
  End Do

  ENDIF


End Subroutine rttov_INTEX
