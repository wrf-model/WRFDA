!
Subroutine rttov_dealloc_coef (errorstatus, coef)
  ! Description:
  ! de-allocation of a coefficient structure
  ! The allocation is done by the readcoef subroutine called by the user
  ! this subroutine should be called once per coef structure when
  ! all rttov calls are completed.
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
  !  1.1       03/05/2004  Add specific RTTOV8 CO2 variable (P. Brunel)
  !  1.2       02/06/2004  Update for RTTOV8 coefS (P. Brunel)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Parameters:
  Use rttov_const, Only :     &
        & sensor_id_mw        ,&
        & errorstatus_info    ,&
        & errorstatus_success ,&
        & errorstatus_fatal   ,&
        & errorstatus_warning

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"

  ! subroutine arguments
  ! scalar arguments with intent(out):
  Integer(Kind=jpim), Intent (out)   :: errorstatus    ! return code
  Type( rttov_coef ), Intent (inout) :: coef           ! coefficients



  ! Local Arrays and Scalars:
  Integer(Kind=jpim), Parameter :: n_ios = 45
  Integer(Kind=jpim)            :: io_status(n_ios)
  Integer(Kind=jpim)            :: ios
  Character (len=80) :: errMessage
  Character (len=18) :: NameOfRoutine = 'rttov_dealloc_coef'

  !- End of header --------------------------------------------------------

  io_status(:) = 0
  errorstatus = errorstatus_success

  Deallocate ( coef % fmv_gas_id , stat=io_status( 1) )
  Deallocate ( coef % fmv_gas_pos, stat=io_status( 2) )
  Deallocate ( coef % fmv_var    , stat=io_status( 3) )
  Deallocate ( coef % fmv_lvl    , stat=io_status( 4) )
  Deallocate ( coef % ff_ori_chn , stat=io_status( 5) )
  Deallocate ( coef % ff_val_chn , stat=io_status( 6) )
  Deallocate ( coef % ff_cwn     , stat=io_status( 7) )
  Deallocate ( coef % ff_bco     , stat=io_status( 8) )
  Deallocate ( coef % ff_bcs     , stat=io_status( 9) )
  Deallocate ( coef % ff_gam     , stat=io_status(10) )
  Deallocate ( coef % gaz_units  , stat=io_status(11) )

  If( coef % fastem_ver >= 1 ) Then
     Deallocate ( coef % fastem_coef   , stat=io_status(12) )
     Deallocate ( coef % fastem_polar  , stat=io_status(13) )
  End If
  If( coef % ssirem_ver >= 1 ) Then
     Deallocate ( coef % ssirem_chn , stat=io_status(14) )
     Deallocate ( coef % ssirem_a0  , stat=io_status(15) )
     Deallocate ( coef % ssirem_a1  , stat=io_status(16) )
     Deallocate ( coef % ssirem_a2  , stat=io_status(17) )
     Deallocate ( coef % ssirem_xzn1, stat=io_status(18) )
     Deallocate ( coef % ssirem_xzn2, stat=io_status(19) )
  End If

  Deallocate ( coef % ref_prfl_p , stat=io_status(20) )
  Deallocate ( coef % ref_prfl_t , stat=io_status(21) )
  Deallocate ( coef % ref_prfl_mr, stat=io_status(22) )

  Deallocate ( coef % lim_prfl_p   , stat=io_status(23) )
  Deallocate ( coef % lim_prfl_tmax, stat=io_status(24) )
  Deallocate ( coef % lim_prfl_tmin, stat=io_status(25) )
  Deallocate ( coef % lim_prfl_gmin, stat=io_status(26) )
  Deallocate ( coef % lim_prfl_gmax, stat=io_status(27) )


  If ( coef % nmixed > 0 ) Then
     Deallocate ( coef % mixedgas , stat= io_status(28) )
  End If


  If ( coef % nwater > 0 ) Then
     Deallocate ( coef % watervapour, stat= io_status(29) )
  End If

  If ( coef % nozone > 0 ) Then
     Deallocate ( coef % ozone , stat= io_status(30) )
  End If

  If ( coef % nwvcont > 0 ) Then
     Deallocate ( coef % wvcont , stat= io_status(31) )
  End If

  If ( coef % nco2 > 0 ) Then
     Deallocate ( coef % co2 , stat= io_status(32) )
  End If

  If ( coef % nn2o > 0 ) Then
     Deallocate ( coef % n2o , stat= io_status(33) )
  End If

  If ( coef % nco > 0 ) Then
     Deallocate ( coef % co , stat= io_status(34) )
  End If

  If ( coef % nch4 > 0 ) Then
     Deallocate ( coef % ch4 , stat= io_status(35) )
  End If

  ! planck variables
  Deallocate ( coef % planck1 , stat= io_status(36) )
  Deallocate ( coef % planck2 , stat= io_status(37) )
  ! frequency in GHz for MicroWaves
  If( coef % id_sensor == sensor_id_mw ) Then
     Deallocate ( coef % frequency_ghz , stat= io_status(38) )
  Endif

  ! Compute specific variables for RTTOV7/8
  Deallocate ( coef % dp      , stat= io_status(39) )
  Deallocate ( coef % dpp     , stat= io_status(40) )
  Deallocate ( coef % tstar   , stat= io_status(41) )
  Deallocate ( coef % to3star , stat= io_status(42) )
  Deallocate ( coef % wstar   , stat= io_status(43) )
  Deallocate ( coef % ostar   , stat= io_status(44) )
  ! Specific variables for RTTOV8
  If( coef % fmv_model_ver == 8 ) Then
     Deallocate ( coef % co2star   , stat= io_status(45) )
  End If

  ! reinitialize coef structure for all single types
  coef % id_platform       = 0
  coef % id_sat            = 0
  coef % id_inst           = 0
  coef % id_sensor         = 0
  coef % id_comp_lvl       = 0
  coef % id_creation_date  = (/ 0, 0, 0 /)
  coef % id_creation       = 'xxxx'
  coef % id_Common_name    = 'xxxx'
  coef % fmv_model_def     = 'xxxx'
  coef % fmv_model_ver     = 0
  coef % fmv_chn           = 0
  coef % fmv_gas           = 0
  coef % nmixed            = 0
  coef % nwater            = 0
  coef % nozone            = 0
  coef % nwvcont           = 0
  coef % nco2              = 0
  coef % nn2o              = 0
  coef % nco               = 0
  coef % nch4              = 0
  coef % nlevels           = 0
  coef % fc_speedl         = 0._JPRB
  coef % fc_planck_c1      = 0._JPRB
  coef % fc_planck_c2      = 0._JPRB
  coef % fc_sat_height     = 0._JPRB
  coef % fastem_ver        = 0
  coef % fastem_coef_nb    = 0
  coef % ssirem_ver        = 0
  coef % ratoe             = 0._JPRB

  ! Check if any deallocation statement failed
  ! In case of failure send an error report for each case
  ! If more than 2 cases then the error is "Fatal"
  If( Any(io_status /= errorstatus_success) ) Then
     Do ios = 1, n_ios
        If( io_status(ios) /= errorstatus_success ) Then
           errorstatus = errorstatus_info
           Write( errMessage, '( "deallocation of coefficent structure number",i3 )' ) ios
           Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        End If
     End Do
     If ( Count(io_status /= errorstatus_success) <= 2 ) Then
        errorstatus = errorstatus_warning
     Else
        errorstatus = errorstatus_fatal
     End If
     Write( errMessage, '( "deallocation of coefficent structure" )' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  End If

  Nullify(coef % gaz_units)
  Nullify(coef % mixedgas)
  Nullify(coef % watervapour)
  Nullify(coef % ozone)
  Nullify(coef % wvcont)
  Nullify(coef % co2)
  Nullify(coef % n2o)
  Nullify(coef % co)
  Nullify(coef % ch4)



End Subroutine rttov_dealloc_coef
