!
Subroutine rttov_initcoeffs  (&
      & errorstatus,   &! out
      & coef,          &! inout
      & knproc,        &! in Optional
      & kmyproc,       &! in Optional
      & kioproc        )! in Optional
  ! Description:
  !
  !   coef arrays initialisation for all pes
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
  !  1.0       17/05/2004  Original (based on rttov_readcoeffs)
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
       & sensor_id_mw        ,&
       & errorstatus_success ,&
       & errorstatus_fatal   ,&
       & gas_id_mixed        ,&
       & gas_id_watervapour  ,&
       & gas_id_ozone        ,&
       & gas_id_wvcont       ,&
       & gas_id_co2          ,&
       & gas_id_n2o          ,&
       & gas_id_co           ,&
       & gas_id_ch4          ,&
       & gas_unit_specconc   ,&
       & gas_unit_ppmv       ,&
       & earthradius         ,&
       & pressure_top


  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"
#include "rttov_distribcoeffs.interface"
#include "rttov_q2v.interface"

  ! subroutine arguments
  ! scalar arguments with intent(in):
  Integer(Kind=jpim), Optional, Intent(in) :: knproc   ! number of procs
  Integer(Kind=jpim), Optional, Intent(in) :: kmyproc  ! logical processor id
  Integer(Kind=jpim), Optional, Intent(in) :: kioproc  ! procs dedicated for io

  ! scalar arguments with intent(out):
  Integer(Kind=jpim), Intent (out) :: errorstatus       ! return code
  Type( rttov_coef ), Intent (inout) :: coef   ! coefficients

  ! Local Scalars:
  Integer(Kind=jpim) :: inproc,imyproc,iioproc
  Integer(Kind=jpim) :: alloc_status(10)
  Integer(Kind=jpim) :: i,n,h

  Character (len=80) :: errMessage
  Character (len=16) :: NameOfRoutine = 'rttov_initcoeffs'


  !- End of header --------------------------------------------------------

  ! 0 Initialise variables
  !---------------------------------------------
  errorstatus     = errorstatus_success
  alloc_status(:) = 0

  If ( .Not. Present (knproc) ) Then
     inproc = 1
  Else
     inproc = knproc
  Endif

  If ( .Not. Present (kmyproc) ) Then
     imyproc = 1
  Else
     imyproc = kmyproc
  Endif

  If ( .Not. Present (kioproc) ) Then
     iioproc = 1
  Else
     iioproc = kioproc
  Endif

  If ( inproc > 1 ) then
    Call rttov_distribcoeffs  (&
        & imyproc,       &! logical processor id
        & iioproc,       &! processor dedicated for reading
        & coef)           ! inout
  Endif

  ! 5 Now compute auxillary variables and change unit for mixing ratios
  !--------------------------------------------------------------------

  ! ratio satellite altitude to earth radius
  coef % ratoe = ( earthradius + coef % fc_sat_height ) / earthradius

  ! planck variables
  Allocate ( coef % planck1 ( coef % fmv_chn ), stat= alloc_status(1))
  Allocate ( coef % planck2 ( coef % fmv_chn ), stat= alloc_status(2))
  If( Any(alloc_status /= 0) ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "allocation of Planck arrays")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If
  coef % planck1(:) = coef % fc_planck_c1 * coef % ff_cwn(:) **3
  coef % planck2(:) = coef % fc_planck_c2 * coef % ff_cwn(:)

  ! frequency in GHz for MicroWaves
  If( coef % id_sensor == sensor_id_mw ) Then
     Allocate ( coef % frequency_ghz ( coef % fmv_chn ), stat= alloc_status(1))
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of frequency array")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If
     coef % frequency_ghz(:) = coef % fc_speedl * 1.0e-09_JPRB * coef % ff_cwn(:)
  Endif


  ! Conversion of gas mixing ratio units, if needed
  ! The correct unit for RTTOV calculations is ppmv
  ! Take care this conversion is only valid
  ! if all gases have the same unit
  h = coef % fmv_gas_pos(gas_id_watervapour)
  Do n = 1,  coef % fmv_gas
     If( coef % gaz_units( n ) == gas_unit_specconc ) Then
        ! Unit of gaz n is specific concentration
        Do i = 1, coef % nlevels
           ! Convert reference profile mixing ratio
           call rttov_q2v(                     &
                 & coef % gaz_units( h )       ,&
                 & coef % ref_prfl_mr ( i, h ) ,&
                 & coef % fmv_gas_id( n )      ,&
                 & coef % ref_prfl_mr ( i, n ) ,&
                 & coef % ref_prfl_mr ( i, n ) )
           ! Now unit of gaz n is ppmv for the reference
           ! in particular for H2O
           coef % gaz_units( n ) = gas_unit_ppmv
           ! Convert profile minimum limit mixing ratio
           call rttov_q2v(                       &
                 & coef % gaz_units( h )         ,&
                 & coef % ref_prfl_mr ( i, h )   ,&
                 & coef % fmv_gas_id( n )        ,&
                 & coef % lim_prfl_gmin ( i, n ) ,&
                 & coef % lim_prfl_gmin ( i, n ) )
           ! Convert profile maximum limit mixing ratio
           call rttov_q2v(                       &
                 & coef % gaz_units( h )         ,&
                 & coef % ref_prfl_mr ( i, h )   ,&
                 & coef % fmv_gas_id( n )        ,&
                 & coef % lim_prfl_gmax ( i, n ) ,&
                 & coef % lim_prfl_gmax ( i, n ) )
        End Do
     End If
  End Do


  ! 6 Compute specific variables for RTTOV7
  ! ---------------------------------------
  ! Test on model and coeff versions
  If( coef % fmv_model_ver == 7 ) Then
     Allocate ( coef % dp      ( coef % nlevels ), stat= alloc_status(1))
     Allocate ( coef % dpp     ( coef % nlevels ), stat= alloc_status(2))
     Allocate ( coef % tstar   ( coef % nlevels ), stat= alloc_status(3))
     Allocate ( coef % to3star ( coef % nlevels ), stat= alloc_status(4))
     Allocate ( coef % wstar   ( coef % nlevels ), stat= alloc_status(5))
     Allocate ( coef % ostar   ( coef % nlevels ), stat= alloc_status(6))
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of specific RTTOV7 arrays")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     ! pressure intervals between levels
     coef % dp(1) = coef % ref_prfl_p(1)
     coef % dp(2:coef % nlevels) = coef % ref_prfl_p(2:coef % nlevels) -&
                                  & coef % ref_prfl_p(1:coef % nlevels-1)

     ! pressure quantity
     coef % dpp(1) = ( coef % ref_prfl_p(1)-0.004985_JPRB ) * pressure_top
     coef % dpp(2) = ( coef % ref_prfl_p(1)-0.004985_JPRB ) * coef % ref_prfl_p(1)
     Do i = 3, coef % nlevels
        coef % dpp(i) = ( coef % ref_prfl_p(i-1)-coef % ref_prfl_p(i-2) ) * &
               & coef % ref_prfl_p(i-1)
     Enddo

     ! reference layer quantities
     ! temperature
     n = coef % fmv_gas_pos(gas_id_mixed)
     coef % tstar(1) = coef % ref_prfl_t(1 , n)
     coef % tstar(2:coef % nlevels) = &
             & ( coef % ref_prfl_t(1:coef % nlevels-1 , n)  +   &
               & coef % ref_prfl_t(2:coef % nlevels   , n)  ) / 2

     ! temperature for O3 profiles
     If ( coef % nozone > 0 ) Then
        n = coef % fmv_gas_pos(gas_id_ozone)
        coef % to3star(1) = coef % ref_prfl_t(1 , n)
        coef % to3star(2:coef % nlevels) = &
                & ( coef % ref_prfl_t(1:coef % nlevels-1 , n) + &
                  & coef % ref_prfl_t(2:coef % nlevels   , n)  ) / 2
        n = coef % fmv_gas_pos(gas_id_ozone)
     Endif

     ! water vapour
     n = coef % fmv_gas_pos(gas_id_watervapour)
     coef % wstar(1) = coef % ref_prfl_mr(1 , n)
     coef % wstar(2:coef % nlevels) = &
             & ( coef % ref_prfl_mr(1:coef % nlevels-1 , n) + &
               & coef % ref_prfl_mr(2:coef % nlevels   , n)  ) / 2

     ! ozone
     If ( coef % nozone > 0 ) Then
        n = coef % fmv_gas_pos(gas_id_ozone)
        coef % ostar(1) = coef % ref_prfl_mr(1 , n)
        coef % ostar(2:coef % nlevels) = &
                & ( coef % ref_prfl_mr(1:coef % nlevels-1 , n) + &
                  & coef % ref_prfl_mr(2:coef % nlevels   , n)  ) / 2
     Endif

  End If

  ! 7 Compute specific variables for RTTOV8
  ! AT PRESENT SAME VARIABLES AS RTTOV7
  ! ---------------------------------------
  ! Test on model and coeff versions
  If( coef % fmv_model_ver == 8 ) Then
     Allocate ( coef % dp      ( coef % nlevels ), stat= alloc_status(1))
     Allocate ( coef % dpp     ( coef % nlevels ), stat= alloc_status(2))
     Allocate ( coef % tstar   ( coef % nlevels ), stat= alloc_status(3))
     Allocate ( coef % to3star ( coef % nlevels ), stat= alloc_status(4))
     Allocate ( coef % wstar   ( coef % nlevels ), stat= alloc_status(5))
     Allocate ( coef % ostar   ( coef % nlevels ), stat= alloc_status(6))
     Allocate ( coef % co2star ( coef % nlevels ), stat= alloc_status(7))
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of specific RTTOV8 arrays")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     ! pressure intervals between levels
     coef % dp(1) = coef % ref_prfl_p(1)
     coef % dp(2:coef % nlevels) = coef % ref_prfl_p(2:coef % nlevels) -&
                                  & coef % ref_prfl_p(1:coef % nlevels-1)

     ! pressure quantity
     coef % dpp(1) = ( coef % ref_prfl_p(1)-0.004985_JPRB ) * pressure_top
     coef % dpp(2) = ( coef % ref_prfl_p(1)-0.004985_JPRB ) * coef % ref_prfl_p(1)
     Do i = 3, coef % nlevels
        coef % dpp(i) = ( coef % ref_prfl_p(i-1)-coef % ref_prfl_p(i-2) ) * &
               & coef % ref_prfl_p(i-1)
     Enddo

     ! reference layer quantities
     ! temperature
     n = coef % fmv_gas_pos(gas_id_mixed)
     coef % tstar(1) = coef % ref_prfl_t(1 , n)
     coef % tstar(2:coef % nlevels) = &
             & ( coef % ref_prfl_t(1:coef % nlevels-1 , n)  +   &
               & coef % ref_prfl_t(2:coef % nlevels   , n)  ) / 2

     ! temperature for O3 profiles
     If ( coef % nozone > 0 ) Then
        n = coef % fmv_gas_pos(gas_id_ozone)
        coef % to3star(1) = coef % ref_prfl_t(1 , n)
        coef % to3star(2:coef % nlevels) = &
                & ( coef % ref_prfl_t(1:coef % nlevels-1 , n) + &
                  & coef % ref_prfl_t(2:coef % nlevels   , n)  ) / 2
     Endif

     ! water vapour
     n = coef % fmv_gas_pos(gas_id_watervapour)
     coef % wstar(1) = coef % ref_prfl_mr(1 , n)
     coef % wstar(2:coef % nlevels) = &
             & ( coef % ref_prfl_mr(1:coef % nlevels-1 , n) + &
               & coef % ref_prfl_mr(2:coef % nlevels   , n)  ) / 2

     ! ozone
     If ( coef % nozone > 0 ) Then
        n = coef % fmv_gas_pos(gas_id_ozone)
        coef % ostar(1) = coef % ref_prfl_mr(1 , n)
        coef % ostar(2:coef % nlevels) = &
                & ( coef % ref_prfl_mr(1:coef % nlevels-1 , n) + &
                  & coef % ref_prfl_mr(2:coef % nlevels   , n)  ) / 2
     Endif

     ! CO2
     If ( coef % nco2 > 0 ) Then
        n = coef % fmv_gas_pos(gas_id_co2)
        coef % co2star(1) = coef % ref_prfl_mr(1 , n)
        coef % co2star(2:coef % nlevels) = &
             &   ( coef % ref_prfl_mr(1:coef % nlevels-1 , n) + &
             &     coef % ref_prfl_mr(2:coef % nlevels   , n)  ) / 2
     Endif

  End If

  ! Here add specific statements for a later release of the model or
  ! of the coefficients


End Subroutine rttov_initcoeffs
