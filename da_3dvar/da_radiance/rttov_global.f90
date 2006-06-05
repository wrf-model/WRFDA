!
Module rttov_global
  ! Description:
  ! Definition of global variables) for RTTOV
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
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       10/01/2003  Original (P Brunel)
  !
  Use rttov_const, Only :   &
       NerrorStatus

  Use parkind1, Only : jpim     ,jprb
  Implicit None
  !- End of header --------------------------------------------------------

  !1. error reporting
  !------------------
  Integer(Kind=jpim) :: error_unit   ! logical unit for the error messages

  Logical :: verbose_message(0:nerrorstatus) !verbose flag for each error level

  Logical :: err_init ! true if module already initialised

  Data err_init/.false./

End Module rttov_global
