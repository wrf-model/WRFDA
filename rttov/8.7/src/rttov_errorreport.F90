!
Subroutine rttov_ErrorReport (ErrStatus, ErrMessage, NameOfRoutine)
  ! Description:
  !   Write out fatal and warning error messages to unit 6.
  !   Execution is stopped in the event of a fatal error.
  !
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
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version  Date      Comment
  !
  !  1.0      2/2/96  Original code. (R.J.Renshaw)
  !       - based on routine of same name in Met Office VAR project.
  !  1.1     27/11/96 STOP 999 changed to CALL EXIT to alert
  !                   unix that program has failed.  (R.J.Renshaw)
  !  1.2    08/03/01  F90 and negative integer  P. Brunel
  !  1.3    01/12/02  New F90 code with structures (P Brunel A Smith)
  !  1.4    10/01/03  Use verbosity level (P Brunel)
  !                   and change varaibles in order to use the
  !                   module rttov_global
  !
  ! Code Description:
  !   FORTRAN 90, following AAPP standards
  !
  ! Declarations
  !
  ! Global variables:
  ! Modules used:
  !
  Use rttov_const, Only :   &
       & ErrorStatus_text,    &
       & NerrorStatus

  Use rttov_global, Only: &
       & verbose_message,     &
       & err_init,            &
       & error_unit

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorhandling.interface"

  !
  ! Subroutine arguments
  !   Scalar arguments with intent(in):
  Integer(Kind=jpim) , Intent (in) ::      ErrStatus     ! +ve => fatal error, -ve => warning
  Character (len=*) , Intent (in) :: ErrMessage    ! ..to output
  Character (len=*) , Intent (in) :: NameOfRoutine ! ..calling this one



  ! local
  Character (len=8) :: date
  Character (len=10):: time
  !- End of header --------------------------------------------------------

  Call DATE_AND_Time(date, time)

  ! If globlal variables not defined then use default values
  if( .not. err_init ) then
     call rttov_errorhandling ( -1_jpim , -1_jpim )
  endif


  If ( ErrStatus >= 0 .And. ErrStatus <=nerrorstatus ) Then
     ! Display message only if allowed by verbose_message flag
     if( verbose_message (ErrStatus) ) then
        Write(Error_Unit,"(1X,a4,'/',a2,'/',a2,2x,2(a2,':'),a2,2x,a,a,a)") &
              & date(1:4), date(5:6), date(7:8), &
              & time(1:2), time(3:4), time(5:6), &
              & ErrorStatus_text(errstatus),&
              & " in module ",Trim(NameOfRoutine)
        Write(Error_Unit,"(5X,A)") Trim(ErrMessage)
     Endif
  Else
     ! This error level is different from predefined
     ! Output it anyway
     Write(Error_Unit,"(1X,a4,'/',a2,'/',a2,2x,2(a2,':'),a2,2x,i6,a,a)") &
           & date(1:4), date(5:6), date(7:8), &
           & time(1:2), time(3:4), time(5:6), &
           & errstatus                      ,&
           & " in module ",Trim(NameOfRoutine)
     Write(Error_Unit,"(5X,A)") Trim(ErrMessage)
  Endif

End Subroutine rttov_ErrorReport
