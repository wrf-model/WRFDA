!
Function rttov_cmpuc (String1, String2)
  ! Description:
  !   compare 2 strings after removing all spaces, and upcase characters
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
  Use parkind1, Only : jpim     ,jprb
  Implicit None
  !
  ! Method:
  !   reduce string 1 , upcase string 1
  !   same for string 2
  !   compare strings
  !
  ! Current Code Owner: P. Brunel
  !
  ! History:
  ! Version  Date      Comment
  !
  !  1.0    08/03/01  F90 Original P. Brunel
  !  1.1    01/12/02  Change name  P. Brunel
  !
  ! Code Description:
  !   FORTRAN 90
  !
  ! Declarations
  !
  !
  ! Function arguments
  !   Scalar arguments with intent(in):
  Character (len=*) , Intent (in) :: string1
  Character (len=*) , Intent (in) :: string2
  Logical :: rttov_cmpuc



  ! Local variables
  Character (len = Len(string1)) :: wstr1  ! working string 1
  Character (len = Len(string2)) :: wstr2  ! working string 2
  Integer(Kind=jpim) :: pos                ! position of space character
  Integer(Kind=jpim) :: cur_char           ! ASCII indice for current character
  Integer(Kind=jpim) :: amin               ! ASCII indice for 'a'
  Integer(Kind=jpim) :: amaj               ! ASCII indice for 'A'
  Integer(Kind=jpim) :: zmin               ! ASCII indice for 'z'
  Integer(Kind=jpim) :: i                  ! loop indice
  !- End of header --------------------------------------------------------

  amin = Ichar('a')
  zmin = Ichar('z')
  amaj = Ichar('A')

  ! reduce string 1
  wstr1 = string1
  ! remove all spaces
  Do
     pos = Index (wstr1(1:len_Trim(wstr1)) , ' ')
     If( pos == 0 ) Exit
     wstr1(pos:) = wstr1(pos+1:)
  End Do

  ! reduce string 2
  wstr2 = string2
  ! remove all spaces
  Do
     pos = Index (wstr2(1:len_Trim(wstr2)), ' ')
     If( pos == 0 ) Exit
     wstr2(pos:) = wstr2(pos+1:)
  End Do

  ! upcase string 1
  Do i = 1, Len(wstr1)
     cur_char = Ichar(wstr1(i:i))
     If( cur_char >= amin .And. cur_char <= zmin ) Then
        wstr1(i:i) = Char(cur_char + amaj-amin)
     Endif
  End Do

  ! upcase string 2
  Do i = 1, Len(wstr2)
     cur_char = Ichar(wstr2(i:i))
     If( cur_char >= amin .And. cur_char <= zmin ) Then
        wstr2(i:i) = Char(cur_char + amaj-amin)
     Endif
  End Do

  ! compare the 2 working strings
  rttov_cmpuc = wstr2 .Eq. wstr1



End Function rttov_cmpuc
