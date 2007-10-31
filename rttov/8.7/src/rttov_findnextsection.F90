!
Subroutine rttov_findnextsection( fileunit,readstatus,section )
  ! Description:
  ! Read file (unit fileunit) until reach next section of
  ! coefficient file
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
  ! Modules used:
  ! Imported Parameters:
  Use rttov_const, Only :   &
       & nsections           ,&
       & section_types

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),           Intent(in)  :: fileunit
  Integer(Kind=jpim),           Intent(out) :: readstatus
  Character(len=21), Intent(out) :: section



  !local variables:
  Integer(Kind=jpim) :: i
  Logical :: sectionfound
  Character(len=80) :: line
  !- End of header --------------------------------------------------------


  section = ''
  sectionfound = .False.

  readfile: Do

     Read( unit=fileunit,fmt='(a)',iostat=readstatus ) line
     If ( readstatus /= 0 ) Exit

     line = Adjustl(line)
     If ( line(1:1) == '!' .Or. line == '' ) Then
        Cycle !skip blank/comment lines
     Else If ( .Not. sectionfound ) Then
        !check for a section name
        Do i = 1, nsections
           If ( section_types(i) == line ) Then
              sectionfound = .True.
              section = section_types(i)
              Exit
           End If
        End Do
     Else
        !reposition file at the start of the line and exit
        Backspace( fileunit )
        Exit readfile
     End If

  End Do readfile



End Subroutine rttov_findnextsection
