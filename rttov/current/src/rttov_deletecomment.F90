!
Subroutine rttov_DeleteComment (String)
  ! Description:
  ! Routine delete comments from string
  !
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
  ! Purpose:
  !   Delete comments and adjust left inside a character string
  !   Comments are starting by a '!' sign
  !
  ! Method:
  !   Check position of character !
  !   If found, change all characters starting at! position to the lenght
  !   of string by a space.
  !   Returns modifyed string adjusted left
  !
  ! Current Code Owner: P. Brunel
  !
  ! History:
  ! Version  Date      Comment
  !  1.0    08/03/01  F90 Original P. Brunel
  !
  ! Code Description:
  !   FORTRAN 90
  !
  ! Declarations
  Use parkind1 , Only : jpim     ,jprb
  Implicit None
  !
  ! Global variables:
  !
  ! Subroutine arguments
  !   Scalar arguments with intent(in/out):
  Character (len=*) , Intent (inout) :: string    ! ..to check



  ! Local variables
  Character (len=1) :: comment = '!'  ! character for starting comment
  Integer(Kind=jpim) :: pos_mark      ! position of character '!' in current string
  Integer(Kind=jpim) :: lenght        ! lenght of string
  Integer(Kind=jpim) :: i             ! loop indice
  !- End of header

  ! find position of comment character in string
  pos_mark = Scan(string,comment)
  lenght = Len(string)

  ! if comment is present, replace comment by spaces
  If(pos_mark > 0) Then
     Do i = pos_mark, lenght
        string(i:i) = ' '
     End Do
  End If

  ! Adjust left string
  string = Adjustl(string)



End Subroutine rttov_DeleteComment
