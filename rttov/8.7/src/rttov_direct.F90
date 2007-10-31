!
Subroutine rttov_direct( &
     & errorstatus,     &! out
     & nfrequencies,    &! in
     & nchannels,       &! in
     & nbtout,          &! in
     & nprofiles,       &! in
     & channels,        &! in
     & polarisations,   &! in
     & lprofiles,       &! in
     & profiles,        &! in
     & coef,            &! in
     & addcloud,        &! in
     & calcemis,        &! in
     & emissivity,      &! inout
     & transmission,    &! inout
     & radiancedata )    ! inout
  !
  ! Description:
  ! to compute multi-channel level to space transmittances,
  ! top of atmosphere and level to space radiances and brightness
  ! temperatures and optionally surface emissivities, for many
  ! profiles in a single call, for satellite
  ! infrared or microwave sensors. The code requires a coefficient file
  ! for each sensor for which simulated radiances are requested.
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
  ! Method: The methodology is described in the following:
  !
  ! Eyre J.R. and H.M. Woolf  1988 Transmittance of atmospheric gases
  ! in the microwave region: a fast model. Applied Optics 27  3244-3249
  !
  ! Eyre J.R. 1991 A fast radiative transfer model for satellite sounding
  ! systems.  ECMWF Research Dept. Tech. Memo. 176 (available from the
  ! librarian at ECMWF).
  !
  ! Saunders R.W., M. Matricardi and P. Brunel 1999 An Improved Fast Radiative
  ! Transfer Model for Assimilation of Satellite Radiance Observations.
  ! QJRMS, 125, 1407-1425.
  !
  ! Matricardi, M., F. Chevallier and S. Tjemkes 2001 An improved general
  ! fast radiative transfer model for the assimilation of radiance
  ! observations. ECMWF Research Dept. Tech. Memo. 345
  ! (available from the librarian at ECMWF).
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !          13/8/92.  For version 2.
  !                    ksat added to argument list; ssu included;
  !                    internal changes to move big arrays from commons to
  !                    arguments and to introduce taskcommons
  !           8/7/97   added ozone and extended water vapour in control vector
  !        01/05/2000  F90 code
  !        21/08/2000  Interface to rtint changed to include pref (surface reflectivity).
  !                    (Stephen English)
  !        31/01/2001  More cloud computations. stored in radov (F. Chevallier)
  !        6/2/2001    pgrody and knav etc arrays removed from call (R Saunders)
  !        18/01/2002  Thread safe (D.Salmond)
  !        01/12/2002  New F90 code with structures (P Brunel A Smith)
  !        02/01/2003  More comments added (R Saunders)
  !        24/01/2003  Error return code by input profile (P Brunel)
  !                    Add WV Continuum and CO2 capability
  !        02/06/2004  Change tests on id_comp_lvl == 7 by tests on fmv_model_ver (P. Brunel)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  ! A user guide and technical documentation is available at
  ! http://www.metoffice.com/research/interproj/nwpsaf/rtm/index.html
  !
  ! Declarations:
  ! Modules used:
  ! Imported Parameters:
  Use rttov_const, Only :   &
       & errorstatus_success ,&
       & errorstatus_warning ,&
       & errorstatus_fatal   ,&
       & max_optical_depth   ,&
       & sensor_id_mw        ,&
       & sensor_id_ir

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type   ,&
       & geometry_Type  ,&
       & predictors_Type,&
       & profile_aux    ,&
       & transmission_type ,&
       & radiance_Type  ,&
       & radiance_aux

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"
#include "rttov_checkinput.interface"
#include "rttov_profaux.interface"
#include "rttov_setgeometry.interface"
#include "rttov_setpredictors.interface"
#include "rttov_setpredictors_8.interface"
#include "rttov_transmit.interface"
#include "rttov_calcemis_ir.interface"
#include "rttov_calcemis_mw.interface"
#include "rttov_integrate.interface"

  !subroutine arguments:
  Integer(Kind=jpim),      Intent(in)    :: nchannels  ! Number of radiances computed
  Integer(Kind=jpim),      Intent(in)    :: nfrequencies  ! Number of frequencies
                                                            !  (= channels used * profiles)
  Integer(Kind=jpim),      Intent(in)    :: nbtout          ! Number of BTs returned
  Integer(Kind=jpim),      Intent(in)    :: nprofiles  ! Number of profiles
  Integer(Kind=jpim),      Intent(in)    :: channels(nfrequencies)    ! Channel indices
  Integer(Kind=jpim),      Intent(in)    :: polarisations(nchannels,3)  ! Channel indices
  Integer(Kind=jpim),      Intent(in)    :: lprofiles(nfrequencies)   !#Profiles indices
  Logical,                 Intent(in)    :: addcloud  ! switch for cloud computations
  Type(profile_Type),      Intent(in)    :: profiles(nprofiles) ! Atmospheric profiles
  Type(rttov_coef),        Intent(in)    :: coef                ! RT Coefficients
  Logical,                 Intent(in)    :: calcemis(nchannels)! switch for emmissivity calc.
  Real(Kind=jprb),         Intent(inout) :: emissivity(nchannels) ! surface emmissivity
  Type(transmission_type), Intent(inout) :: transmission ! transmittances and layer optical depths
  Type(radiance_Type),     Intent(inout) :: radiancedata   ! radiances (mw/cm-1/ster/sq.m) and degK
  Integer(Kind=jpim),      Intent(out)   :: errorstatus(nprofiles)  ! return flag



  !local variables:
  Integer(Kind=jpim) :: i      ! loop index
  Logical :: addcosmic    ! switch for adding temp of cosmic background
  Integer(Kind=jpim) :: alloc_status(10)  ! memory allocation status
  Real(Kind=jprb)    :: reflectivity(nchannels) ! surface reflectivity
  Real(Kind=jprb)    :: od_layer(coef%nlevels,nchannels) ! layer optical depth
  Real(Kind=jprb)    :: opdp_ref(coef%nlevels,nfrequencies) ! layer optical depth before threshold
                                                            ! from each standard pressure level
  Character (len=80) :: errMessage
  Character (len=12) :: NameOfRoutine = 'rttov_direct'

  Type(geometry_Type)   :: angles(nprofiles)  ! geometry angles
  Type(predictors_Type) :: predictors(nprofiles)! predictors
  Type(profile_aux)     :: aux_prof(nprofiles)  ! auxillary profiles informations
  Type(radiance_aux)    :: auxrad               ! auxillary radiances

  !- End of header --------------------------------------------------------


  !-------------
  !0. initialize
  !-------------

  errorstatus(:)  = errorstatus_success
  alloc_status(:) = 0

  !------------------------------------------------------
  !1. check input data is within suitable physical limits
  !------------------------------------------------------


  Do i = 1, nprofiles

     Call rttov_checkinput( &
          & profiles( i ),     &!in
          & coef,              &!in
          & errorstatus(i)    ) !out

  End Do

  ! 1.1 test check input return code
  !-----------------------------_---
  If ( any( errorstatus(:) == errorstatus_warning ) ) Then
     Do i = 1, nprofiles
        If ( errorstatus(i) == errorstatus_warning ) Then
           Write( errMessage, '( "checkinput warning error for profile",i4)' ) i
           Call Rttov_ErrorReport (errorstatus_warning, errMessage, NameOfRoutine)
        End If
     End Do
  End If

  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Do i = 1, nprofiles
        If ( errorstatus(i) == errorstatus_fatal ) Then
           ! Some unphysical values; Do not run RTTOV
           Write( errMessage, '( "checkinput fatal error for profile",i4)' ) i
           Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
        End If
     End Do
     ! nothing processed so all profiles get the fatal error code
     ! user will know which profile
     errorstatus(:) = errorstatus_fatal
     Return
  End If



  !-----------------------------------------
  !2. determine cloud top and surface levels
  !-----------------------------------------
  Do i = 1, nprofiles
     If( coef % id_sensor == sensor_id_mw ) Then
        Allocate( aux_prof(i) % debye_prof( 5 , coef%nlevels ), stat= alloc_status(1)  )
        If( alloc_status(1) /= 0 ) Then
           errorstatus(:) = errorstatus_fatal
           Write( errMessage, '( "allocation of debye_prof")' )
           Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
           Return
        End If
     Endif
     Call rttov_profaux( &
          & profiles(i),   &! in
          & coef,          &! in
          & aux_prof(i))    ! inout
  End Do


  !------------------------------------------------------------------
  !3. set up common geometric variables for transmittance calculation
  !------------------------------------------------------------------

  Do i = 1, nprofiles
     Call rttov_setgeometry( &
          & profiles(i),   &! in
          & coef,          &! in
          & angles(i) )     ! out
  End Do


  !------------------------------------------
  !5. calculate transmittance path predictors
  !------------------------------------------

  Do i = 1, nprofiles
     predictors(i) % nlevels  = coef % nlevels
     predictors(i) % nmixed   = coef % nmixed
     predictors(i) % nwater   = coef % nwater
     predictors(i) % nozone   = coef % nozone
     predictors(i) % nwvcont  = coef % nwvcont
     predictors(i) % nco2     = coef % nco2
     predictors(i) % ncloud   = 0 ! (can be set to 1 inside setpredictors)

     Allocate( predictors(i) % mixedgas&
           & ( coef%nmixed , coef%nlevels ) ,stat= alloc_status(1))
     Allocate( predictors(i) % watervapour&
           & ( coef%nwater , coef%nlevels ) ,stat= alloc_status(2))
     Allocate( predictors(i) % clw&
           & ( coef%nlevels )  ,stat= alloc_status(3))
     If( coef%nozone > 0 ) Then
        Allocate( predictors(i) % ozone&
              & ( coef%nozone , coef%nlevels ) ,stat= alloc_status(4))
     End If
     If( coef%nwvcont > 0 ) Then
        Allocate( predictors(i) % wvcont&
              & ( coef%nwvcont , coef%nlevels ) ,stat= alloc_status(5))
     End If
     If( coef%nco2 > 0 ) Then
        Allocate( predictors(i) % co2&
              & ( coef%nco2 , coef%nlevels ) ,stat= alloc_status(6))
     End If
     If( Any(alloc_status /= 0) ) Then
        errorstatus(:) = errorstatus_fatal
        Write( errMessage, '( "allocation of predictors")' )
        Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
        Return
     End If

     If (coef%fmv_model_ver == 7) Then
        Call rttov_setpredictors( &
             & profiles(i),         &! in
             & angles(i),           &! in
             & coef,                &! in
             & predictors(i) )       ! inout  (in because of mem allocation)

     Else If (coef%fmv_model_ver == 8) Then
        Call rttov_setpredictors_8( &
             & profiles(i),         &! in
             & angles(i),           &! in
             & coef,                &! in
             & predictors(i) )       ! inout  (in because of mem allocation)

     Else
        errorstatus(:) = errorstatus_fatal
        Write( errMessage,&
              & '( "Unexpected RTTOV compatibility version number" )' )
        Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
        Return
     End If

  End Do ! Profile loop


  !----------------------------------------------
  !6. calculate optical depths and transmittances
  !----------------------------------------------
  Call rttov_transmit( &
       & nfrequencies,    &! in
       & nchannels,       &! in
       & nprofiles,       &! in
       & coef%nlevels,    &! in
       & channels,        &! in
       & polarisations,   &! in
       & lprofiles,       &! in
       & predictors,      &! in
       & aux_prof,        &! in
       & coef,            &! in
       & transmission,    &! out
       & od_layer,        &! out
       & opdp_ref)         ! out

  !--------------------------------------
  !7. calculate channel emissivity values
  !--------------------------------------

  If ( Any(calcemis) ) Then
     ! calculate surface emissivity for selected channels
     ! and reflectivity
     If ( coef % id_sensor == sensor_id_ir ) Then
        !Infrared
        Call rttov_calcemis_ir( &
             & profiles,         &! in
             & angles,           &! in
             & coef,             &! in
             & nfrequencies,     &! in
             & nprofiles,        &! in
             & channels,         &! in
             & lprofiles,        &! in
             & calcemis,         &! in
             & emissivity  )      ! inout
        reflectivity(:) = 1 - emissivity(:)

     Elseif ( coef % id_sensor == sensor_id_mw ) Then
        !Microwave
        Call rttov_calcemis_mw ( &
             & profiles,      &! in
             & angles,        &! in
             & coef,          &! in
             & nfrequencies,  &! in
             & nchannels,     &! in
             & nprofiles,     &! in
             & channels,      &! in
             & polarisations, &! in
             & lprofiles,     &! in
             & transmission,  &! in
             & calcemis,      &! in
             & emissivity,    &! inout
             & reflectivity,  &! out
             & errorstatus   ) ! inout
        If ( Any( errorstatus == errorstatus_fatal ) ) Then
           errorstatus(:) = errorstatus_fatal
           Return
        End If
     Else
        ! Hires
        Call rttov_calcemis_ir( &
             & profiles,      &! in
             & angles,        &! in
             & coef,          &! in
             & nfrequencies,  &! in
             & nprofiles,     &! in
             & channels,      &! in
             & lprofiles,     &! in
             & calcemis,      &! in
             & emissivity  )   ! inout
        reflectivity(:) = 1 - emissivity(:)
     End If

     ! reflectivity for other channels
     Where( .Not. calcemis(:) )
        reflectivity(:) = 1 - emissivity(:)
     End Where

  Else
     ! reflectivity for all channels
     reflectivity(:) = 1 - emissivity(:)
  End If


  !--------------------------------------------
  !8. integrate the radiative transfer equation
  !--------------------------------------------
  Allocate(auxrad % layer   (coef % nlevels, nchannels) ,stat= alloc_status(1))
  Allocate(auxrad % surfair (nchannels) ,stat= alloc_status(2))
  Allocate(auxrad % skin    (nchannels) ,stat= alloc_status(3))
  Allocate(auxrad % cosmic  (nchannels) ,stat= alloc_status(4))
  Allocate(auxrad % up      (coef % nlevels, nchannels) ,stat= alloc_status(5))
  Allocate(auxrad % down    (coef % nlevels, nchannels) ,stat= alloc_status(6))
  If ( addcloud ) Then
     Allocate(auxrad % down_cloud    (coef % nlevels, nchannels),stat= alloc_status(7))
  End If
  If( Any(alloc_status /= 0) ) Then
     errorstatus(:) = errorstatus_fatal
     Write( errMessage, '( "allocation of aux radiances")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  addcosmic = ( coef % id_sensor == sensor_id_mw )
  Call rttov_integrate( &
       & addcloud,      &! in
       & addcosmic,     &! in
       & nfrequencies,  &! in
       & nchannels,     &! in
       & nbtout,        &! in
       & nprofiles,     &! in
       & angles,        &! in
       & channels,      &! in
       & polarisations, &! in
       & lprofiles,     &! in
       & emissivity,    &! in
       & reflectivity,  &! in
       & transmission,  &! in
       & profiles,      &! in
       & aux_prof,      &! in
       & coef,          &! in
       & radiancedata,  &! inout
       & auxrad        ) ! inout

  !--------------------
  !9. deallocate memory
  !--------------------
  Do i = 1, nprofiles
     Deallocate( predictors(i) % mixedgas    ,stat= alloc_status(1))
     Deallocate( predictors(i) % watervapour ,stat= alloc_status(2))
     Deallocate( predictors(i) % clw         ,stat= alloc_status(3))
     If( predictors(i) % nozone > 0 ) Then
        Deallocate( predictors(i) % ozone    ,stat= alloc_status(4))
     End If
     If( predictors(i) % nwvcont > 0 ) Then
        Deallocate( predictors(i) % wvcont    ,stat= alloc_status(5))
     End If
     If( predictors(i) % nco2 > 0 ) Then
        Deallocate( predictors(i) % co2    ,stat= alloc_status(6))
     End If
     If( Any(alloc_status /= 0) ) Then
        errorstatus(:) = errorstatus_fatal
        Write( errMessage, '( "deallocation of predictors")' )
        Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
        Return
     End If
  End Do

  Deallocate(auxrad % layer   ,stat= alloc_status(1))
  Deallocate(auxrad % surfair ,stat= alloc_status(2))
  Deallocate(auxrad % skin    ,stat= alloc_status(3))
  Deallocate(auxrad % cosmic  ,stat= alloc_status(4))
  Deallocate(auxrad % up      ,stat= alloc_status(5))
  Deallocate(auxrad % down    ,stat= alloc_status(6))
  If ( addcloud ) Then
     Deallocate(auxrad % down_cloud ,stat= alloc_status(7))
  End If
  If( Any(alloc_status /= 0) ) Then
     errorstatus(:) = errorstatus_fatal
     Write( errMessage, '( "deallocation of aux radiances")' )
     Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
     Return
  End If

  If( coef % id_sensor == sensor_id_mw ) Then
     Do i = 1, nprofiles
        If( Associated( aux_prof(i) % debye_prof) ) Then
           Deallocate( aux_prof(i) % debye_prof ,stat= alloc_status(1))
           If( Any(alloc_status /= 0) ) Then
              errorstatus(:) = errorstatus_fatal
              Write( errMessage, '( "deallocation of debye profile")' )
              Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
              Return
           End If
        End If
     End Do
  Endif

End Subroutine rttov_direct
