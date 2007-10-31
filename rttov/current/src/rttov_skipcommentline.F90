!
Subroutine rttov_skipcommentline( fileunit,readstatus )
  ! Description:
  ! read the file while input cards are starting by "!" character
  ! position the file before the first data line
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
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),           Intent(in)  :: fileunit    ! logical unit of file
  Integer(Kind=jpim),           Intent(out) :: readstatus  ! I/O status



  !local variables:
  Character(len=80) :: line  ! input line

  !- End of header --------------------------------------------------------


  readfile: Do

     Read( unit=fileunit,fmt='(a)',iostat=readstatus ) line
     If ( readstatus /= 0 ) Exit

     line = Adjustl(line)
     If ( line(1:1) == '!' .Or. line == '' ) Then
        Cycle !skip blank/comment lines
     Else
        !reposition file at the start of the line and exit
        Backspace( fileunit )
        Exit readfile
     End If

  End Do readfile



End Subroutine rttov_skipcommentline
