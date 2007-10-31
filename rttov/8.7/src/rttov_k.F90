!
Subroutine rttov_k( &
     & errorstatus,    &! out
     & nfrequencies,   &! in
     & nchannels,      &! in
     & nbtout,         &! in
     & nprofiles,      &! in
     & channels,       &! in
     & polarisations,  &! in
     & lprofiles,      &! in
     & profiles,       &! in
     & coef,           &! in
     & addcloud,       &! in
     & switchrad,      &! in
     & calcemis,       &! in
     & emissivity,     &! inout  direct model
     & profiles_k,     &! inout  K mat on profile variables
     & emissivity_k,   &! inout  K mat on surface emissivity
     & transmission,   &! inout  K model
     & transmission_k, &! inout  K input
     & radiancedata,   &! inout  direct model   (input due to pointers alloc)
     & radiance_k     ) ! inout OPTIONAL K radiances
  !
  ! Description:
  ! K matrix of rttov_direct
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
  !  1.1   01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.2   02/01/2003  More comments added (R Saunders)
  !  1.3   24/01/2003  Error return code by input profile (P Brunel)
  !                    Add WV Continuum and CO2 capability
  !  1.4   02/06/2004  Change tests on id_comp_lvl == 7 by tests on 
  !                    fmv_model_ver (P. Brunel)
  !  1.5   17/02/2005  Changed to allow calls from RTTOV_SCATT_K and 
  !                    RTTOV_CLD_K.   (A. Collard)
  !  1.6   24/08/2005  More changes so routine still works for clear 
  !                    sky RTTOV (R.Saunders)
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
       & transmission_Type  ,&
       & radiance_Type  ,&
       & radiance_aux

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"
#include "rttov_profout_k.interface"
#include "rttov_checkinput.interface"
#include "rttov_profaux.interface"
#include "rttov_setgeometry.interface"
#include "rttov_setpredictors.interface"
#include "rttov_setpredictors_8.interface"
#include "rttov_transmit.interface"
#include "rttov_calcemis_ir.interface"
#include "rttov_calcemis_mw.interface"
#include "rttov_integrate.interface"
#include "rttov_profaux_k.interface"
#include "rttov_setpredictors_k.interface"
#include "rttov_setpredictors_8_k.interface"
#include "rttov_transmit_k.interface"
#include "rttov_calcemis_mw_k.interface"
#include "rttov_integrate_k.interface"

  !subroutine arguments:
  Integer(Kind=jpim),      Intent(in)    :: nfrequencies
  Integer(Kind=jpim),      Intent(in)    :: nchannels
  Integer(Kind=jpim),      Intent(in)    :: nbtout
  Integer(Kind=jpim),      Intent(in)    :: nprofiles
  Integer(Kind=jpim),      Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),      Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),      Intent(in)    :: lprofiles(nfrequencies)

  Logical,                 Intent(in)    :: addcloud
  Logical,                 Intent(in)    :: switchrad  ! true if input is BT
  Type(profile_Type),      Intent(in)    :: profiles(nprofiles)
  Type(rttov_coef),        Intent(in)    :: coef
  Logical,                 Intent(in)    :: calcemis(nchannels)
  Integer(Kind=jpim),      Intent(out)   :: errorstatus(nprofiles)
  Real(Kind=jprb),         Intent(inout) :: emissivity(nchannels)
  Type(transmission_Type), Intent(inout) :: transmission! in because of meme allocation
  Type(radiance_Type),     Intent(inout) :: radiancedata! in because of meme allocation


  Type(profile_Type),      Intent(inout) :: profiles_k(nchannels)  ! Normally this has size nbtout but is nchannels
                                                           ! for calls from RTTOV_CLD_K and RTTOV_SCATT_K.
  Real(Kind=jprb),         Intent(inout) :: emissivity_k(nchannels)
  Type(transmission_Type), Intent(inout) :: transmission_k ! in because of meme allocation
  Type(radiance_Type),     Intent(inout), optional :: radiance_k




  !local variables:
  Integer(Kind=jpim) :: freq
  Integer(Kind=jpim) :: i, j, ii             ! loop index
  Integer(Kind=jpim) :: alloc_status(16)   ! memory allocation status
  Logical :: addcosmic          ! switch for adding temp of cosmic background


  Logical :: local_rad_k  ! false if an input K radiance is provided
  Real(Kind=jprb)    :: reflectivity(nchannels)      ! surface reflectivity
  Real(Kind=jprb)    :: reflectivity_k(nchannels)    ! K surface reflectivity
  Real(Kind=jprb)    :: od_layer(coef%nlevels,nchannels) ! layer optical depth
  Real(Kind=jprb)    :: opdp_ref(coef%nlevels,nfrequencies) ! layer optical depth before threshold

  Character (len=80) :: errMessage
  Character (len=8)  :: NameOfRoutine = 'rttov_k '

  Type(geometry_Type)   :: angles(nprofiles)     ! geometry angles
  Type(predictors_Type) :: predictors(nprofiles) ! predictors
  Type(profile_aux)     :: aux_prof(nprofiles)   ! auxillary profiles informations



  Type(radiance_Type)   :: radiancedata_k          ! Local structure for K radiances
  Type(predictors_Type) :: predictors_k(nchannels) ! K of above predictors
  Type(profile_aux)     :: aux_prof_k(nchannels)   ! K of above aux_prof
  Type(radiance_aux)    :: auxrad
  Type(profile_Type)    :: profiles_k_all(nchannels)
  !- End of header ------------------------------------------------------


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
       & transmission,    &! inout
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
             & errorstatus   ) ! out
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
     reflectivity(:) = 1._JPRB - emissivity(:)
  End If


  !--------------------------------------------
  !8. integrate the radiative transfer equation
  !--------------------------------------------
  allocate(auxrad % layer   (coef % nlevels, nchannels),stat= alloc_status(1))
  allocate(auxrad % surfair (nchannels),stat= alloc_status(2))
  allocate(auxrad % skin    (nchannels),stat= alloc_status(3))
  allocate(auxrad % cosmic  (nchannels),stat= alloc_status(4))
  allocate(auxrad % up      (coef % nlevels, nchannels),stat= alloc_status(5))
  allocate(auxrad % down    (coef % nlevels, nchannels),stat= alloc_status(6))
  If ( addcloud ) then
     allocate(auxrad % down_cloud (coef % nlevels, nchannels),stat= alloc_status(7))
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



  ! K matrix
  !----------------

  !
  ! allocate memory for intermediate variables
  Do i = 1, nchannels
     aux_prof_k(i) % nearestlev_surf = 0   ! no meaning
     aux_prof_k(i) % pfraction_surf  = 0._JPRB  ! calculated
     aux_prof_k(i) % nearestlev_ctp  = 0   ! no meaning
     aux_prof_k(i) % pfraction_ctp   = 0._JPRB  ! calculated
     aux_prof_k(i) % cfraction       = 0._JPRB  ! calculated
     If( coef % id_sensor == sensor_id_mw ) Then
        Allocate( aux_prof_k(i) % debye_prof( 5 , coef%nlevels ) ,stat= alloc_status(1))
        If( alloc_status(1) /= 0 ) Then
           errorstatus(:) = errorstatus_fatal
           Write( errMessage, '( "allocation of K debye_prof")' )
           Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
           Return
        End If
        aux_prof_k(i) % debye_prof(:,:)  = 0._JPRB
     Endif
  End Do

  Do i = 1, nchannels
     freq=polarisations(i, 2)
     j = lprofiles(freq)
     Allocate( predictors_k(i) % mixedgas   ( coef%nmixed , coef%nlevels ),stat= alloc_status(1))
     Allocate( predictors_k(i) % watervapour( coef%nwater , coef%nlevels ),stat= alloc_status(2))
     Allocate( predictors_k(i) % clw        ( coef%nlevels ) ,stat= alloc_status(3))
     Allocate( profiles_k_all(i) % p(coef%nlevels ) ,stat= alloc_status(1))
     Allocate( profiles_k_all(i) % t(coef%nlevels ) ,stat= alloc_status(1))
     Allocate( profiles_k_all(i) % q(coef%nlevels ) ,stat= alloc_status(1))
     Allocate( profiles_k_all(i) % o3(coef%nlevels ) ,stat= alloc_status(1))
     Allocate( profiles_k_all(i) % clw(coef%nlevels ) ,stat= alloc_status(1))

     If( Any(alloc_status /= 0) ) Then
        errorstatus(:) = errorstatus_fatal
        Write( errMessage, '( "allocation of K predictors")' )
        Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
        Return
     End If

     If(Present(radiance_k)) Then

       local_rad_k = .false.
       Do ii=1, coef%nlevels
          profiles_k_all(i) % clw (ii) = profiles_k(i) % clw (ii) 
          profiles_k_all(i) % o3 (ii) = profiles_k(i) % o3 (ii) 
          profiles_k_all(i) % t (ii) = profiles_k(i) % t (ii)
          profiles_k_all(i) % q (ii) = profiles_k(i) % q (ii)
          profiles_k_all(i) % p (ii) = profiles_k(i) % p (ii)
       Enddo
       profiles_k_all(i) % s2m % t  = profiles_k(i) % s2m % t
       profiles_k_all(i) % s2m % u  = profiles_k(i) % s2m % u 
       profiles_k_all(i) % s2m % v  = profiles_k(i) % s2m % v 
       profiles_k_all(i) % s2m % q  = profiles_k(i) % s2m % q
       profiles_k_all(i) % s2m % p  = profiles_k(i) % s2m % p
       profiles_k_all(i) % skin % t  = profiles_k(i) % skin % t
       profiles_k_all(i) % skin % fastem(1)  = profiles_k(i) % skin % fastem(1)
       profiles_k_all(i) % skin % fastem(2)  = profiles_k(i) % skin % fastem(2)
       profiles_k_all(i) % skin % fastem(3)  = profiles_k(i) % skin % fastem(3)
       profiles_k_all(i) % skin % fastem(4)  = profiles_k(i) % skin % fastem(4)
       profiles_k_all(i) % skin % fastem(5)  = profiles_k(i) % skin % fastem(5)
       profiles_k_all(i) % ctp  = profiles_k(i) % ctp 
       profiles_k_all(i) % cfraction  = profiles_k(i) % cfraction
       profiles_k_all(i) % nlevels = profiles_k(i) % nlevels 
     ELSE
       Do ii=1, coef%nlevels
          profiles_k_all(i) % clw (ii) = 0.0_JPRB
          profiles_k_all(i) % o3 (ii) = 0.0_JPRB
          profiles_k_all(i) % t (ii) = 0.0_JPRB
          profiles_k_all(i) % q (ii) = 0.0_JPRB
          profiles_k_all(i) % p (ii) = 0.0_JPRB
       Enddo
       profiles_k_all(i) % s2m % t  =0.0_JPRB
       profiles_k_all(i) % s2m % u  =0.0_JPRB
       profiles_k_all(i) % s2m % v  =0.0_JPRB
       profiles_k_all(i) % s2m % q  =0.0_JPRB
       profiles_k_all(i) % s2m % p  =0.0_JPRB
       profiles_k_all(i) % skin % t  =0.0_JPRB
       profiles_k_all(i) % skin % fastem(1)  =0.0_JPRB
       profiles_k_all(i) % skin % fastem(2)  =0.0_JPRB
       profiles_k_all(i) % skin % fastem(3)  =0.0_JPRB
       profiles_k_all(i) % skin % fastem(4)  =0.0_JPRB
       profiles_k_all(i) % skin % fastem(5)  =0.0_JPRB
       profiles_k_all(i) % ctp  =0.0_JPRB
       profiles_k_all(i) % cfraction  =0.0_JPRB
       profiles_k_all(i) % nlevels =  coef % nlevels
     END IF

     predictors_k(i) % mixedgas(:,:)    = 0._JPRB
     predictors_k(i) % watervapour(:,:) = 0._JPRB
     predictors_k(i) % clw(:)           = 0._JPRB
     predictors_k(i) % nlevels  = predictors(j) % nlevels
     predictors_k(i) % nmixed   = predictors(j) % nmixed
     predictors_k(i) % nwater   = predictors(j) % nwater
     predictors_k(i) % nozone   = predictors(j) % nozone
     predictors_k(i) % nwvcont  = predictors(j) % nwvcont
     predictors_k(i) % nco2     = predictors(j) % nco2
     predictors_k(i) % ncloud   = predictors(j) % ncloud

     If( predictors_k(i) % nozone > 0 ) Then
        Allocate( predictors_k(i) % ozone&
              & ( coef%nozone , coef%nlevels ) ,stat= alloc_status(1))
        If( Any(alloc_status /= 0) ) Then
           errorstatus(:) = errorstatus_fatal
           Write( errMessage, '( "allocation of K predictors")' )
           Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
           Return
        End If
        predictors_k(i) % ozone(:,:) = 0._JPRB
     End If

     If( predictors_k(i) % nwvcont > 0 ) Then
        Allocate( predictors_k(i) % wvcont&
              & ( coef%nwvcont , coef%nlevels ) ,stat= alloc_status(1))
        If( Any(alloc_status /= 0) ) Then
           errorstatus(:) = errorstatus_fatal
           Write( errMessage, '( "allocation of K predictors")' )
           Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
           Return
        End If
        predictors_k(i) % wvcont(:,:) = 0._JPRB
     End If

     If( predictors_k(i) % nco2 > 0 ) Then
        Allocate( predictors_k(i) % co2&
              & ( coef%nco2 , coef%nlevels ) ,stat= alloc_status(1))
        If( Any(alloc_status /= 0) ) Then
           errorstatus(:) = errorstatus_fatal
           Write( errMessage, '( "allocation of K predictors")' )
           Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
           Return
        End If
        predictors_k(i) % co2(:,:) = 0._JPRB
     End If
  End Do

  reflectivity_k(:) = 0._JPRB

  !--------------------------------------------
  !8. integrate the radiative transfer equation
  !--------------------------------------------
  If(Present(radiance_k)) Then

     ! use the subroutine argument for K radiances
     addcosmic = ( coef % id_sensor == sensor_id_mw )
     Call rttov_integrate_k( &
          & addcloud,      &! in
          & addcosmic,     &! in
          & switchrad,     &! in
          & nfrequencies,  &! in
          & nchannels,     &! in
          & nbtout,        &! in
          & nprofiles,     &! in
          & angles,        &! in
          & channels,      &! in
          & polarisations, &! in
          & lprofiles,     &! in
          & emissivity,    &! in
          & emissivity_k,    &! inout
          & reflectivity,    &! in
          & reflectivity_k,  &! inout
          & transmission,    &! in
          & transmission_k,  &! inout
          & profiles,        &! in
          & profiles_k_all,  &! inout  (input only due to mem alloc)
          & aux_prof,        &! in
          & aux_prof_k,      &! inout
          & coef,            &! in
          & radiancedata,    &! in
          & auxrad ,         &! in
          & radiance_k      ) ! inout  (output if converstion Bt -> rad)

  Else


     local_rad_k = .true.


     ! create a local structure for input K radiances
     Allocate( radiancedata_k % clear    ( nchannels ) ,stat= alloc_status(1))
     Allocate( radiancedata_k % cloudy   ( nchannels ) ,stat= alloc_status(2))
     Allocate( radiancedata_k % total    ( nchannels ) ,stat= alloc_status(3))
     Allocate( radiancedata_k % bt       ( nchannels ) ,stat= alloc_status(4))
     Allocate( radiancedata_k % bt_clear ( nchannels ) ,stat= alloc_status(5))
     Allocate( radiancedata_k % out      ( nbtout ) ,stat= alloc_status(4))
     Allocate( radiancedata_k % out_clear( nbtout ) ,stat= alloc_status(5))
     Allocate( radiancedata_k % upclear  ( nchannels ) ,stat= alloc_status(6))
     Allocate( radiancedata_k % reflclear( nchannels ) ,stat= alloc_status(7))
     Allocate( radiancedata_k % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(8))
     Allocate( radiancedata_k % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(9))
     Allocate( radiancedata_k % total_out ( nbtout ) ,stat= alloc_status(10))
     Allocate( radiancedata_k % clear_out  ( nbtout ) ,stat= alloc_status(11))
     If( Any(alloc_status /= 0) ) Then
        errorstatus(:) = errorstatus_fatal
        Write( errMessage, '( "allocation of K radiances")' )
        Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
        Return
     End If
     radiancedata_k % clear(:)      = 0._JPRB
     radiancedata_k % clear_out(:)  = 0._JPRB
     radiancedata_k % out_clear(:)  = 0._JPRB
     radiancedata_k % cloudy(:)     = 0._JPRB
     radiancedata_k % bt_clear(:)   = 0._JPRB
     radiancedata_k % upclear(:)    = 0._JPRB
     radiancedata_k % reflclear(:)  = 0._JPRB
     radiancedata_k % overcast(:,:) = 0._JPRB
     radiancedata_k % downcld(:,:)  = 0._JPRB

     radiancedata_k % bt(:)    = 0._JPRB
     radiancedata_k % total(:) = 0._JPRB
     radiancedata_k % out(:)   = 1._JPRB
     radiancedata_k % total_out(:) = 1._JPRB

     addcosmic = ( coef % id_sensor == sensor_id_mw )
     Call rttov_integrate_k( &
          & addcloud,        &! in
          & addcosmic,       &! in
          & switchrad,       &! in
          & nfrequencies,    &! in
          & nchannels,       &! in
          & nbtout,          &! in
          & nprofiles,       &! in
          & angles,          &! in
          & channels,        &! in
          & polarisations,   &! in
          & lprofiles,       &! in
          & emissivity,      &! in
          & emissivity_k,    &! inout
          & reflectivity,    &! in
          & reflectivity_k,  &! inout
          & transmission,    &! in
          & transmission_k,  &! inout
          & profiles,        &! in
          & profiles_k_all,  &! inout  (input only due to mem alloc)
          & aux_prof,        &! in
          & aux_prof_k,      &! inout
          & coef,            &! in
          & radiancedata,    &! in
          & auxrad ,         &! in
          & radiancedata_k  ) ! inout  (output if converstion Bt -> rad)
  Endif


  If ( Any(calcemis) ) Then
     ! calculate surface emissivity for selected channels
     ! and reflectivity
     If ( coef % id_sensor == sensor_id_ir ) Then
        !Infrared
        emissivity_k(:) = -reflectivity_k(:) + emissivity_k(:)

        Elseif ( coef % id_sensor == sensor_id_mw ) Then
           !Microwave
        Call rttov_calcemis_mw_k ( &
             & profiles,         &! in
             & profiles_k_all,   &! inout
             & angles,           &! in
             & coef,             &! in
             & nfrequencies,     &! in
             & nchannels,        &! in
             & nprofiles,        &! in
             & channels,         &! in
             & polarisations,    &! in
             & lprofiles,        &! in
             & transmission,     &! in
             & transmission_k,   &! inout
             & calcemis,         &! in
             & emissivity_k,     &! inout
             & reflectivity_k   ) ! inout
     Else
        ! Hires
        emissivity_k(:) = -reflectivity_k(:) + emissivity_k(:)
     End If

     ! reflectivity for other channels
     Where( .Not. calcemis(:) )
        emissivity_k(:) = -reflectivity_k(:) + emissivity_k(:)
     End Where

  Else
     ! reflectivity for all channels
     emissivity_k(:) = -reflectivity_k(:) + emissivity_k(:)
  End If

  !K of optical depths and transmittances

  Call rttov_transmit_k( &
       & nfrequencies,    &! in
       & nchannels,       &! in
       & nprofiles,       &! in
       & coef%nlevels,    &! in
       & polarisations,   &! in
       & channels,        &! in
       & lprofiles,       &! in
       & predictors,      &! in
       & predictors_k,    &! inout
       & aux_prof,        &! in
       & aux_prof_k,      &! inout
       & coef,            &! in
       & od_layer,        &! in
       & opdp_ref,        &! in
       & transmission,    &! in
       & transmission_k  ) ! inout

  ! K of Predictors RTTOV-7 RTTOV-8
  If (coef%fmv_model_ver == 7) Then
     Call rttov_setpredictors_k( &
          & nfrequencies,  &! in
          & nchannels,     &! in
          & nprofiles,     &! in
          & coef%nlevels,  &! in
          & angles,        &! in
          & polarisations, &! in
          & lprofiles,     &! in
          & profiles,      &! in
          & profiles_k_all,&! inout
          & coef,          &! in
          & predictors,    &! in
          & predictors_k )  ! inout
  Else If (coef%fmv_model_ver == 8) Then
     Call rttov_setpredictors_8_k( &
          & nfrequencies,  &! in
          & nchannels,     &! in
          & nprofiles,     &! in
          & coef%nlevels,  &! in
          & angles,        &! in
          & polarisations, &! in
          & lprofiles,     &! in
          & profiles,      &! in
          & profiles_k_all,&! inout
          & coef,          &! in
          & predictors,    &! in
          & predictors_k )  ! inout
  End If

  ! K on cloud top and surface levels
  Call rttov_profaux_k( &
     & nfrequencies,  &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & polarisations, &! in
     & lprofiles,     &! in
     & profiles,      &! in
     & profiles_k_all,&! inout
     & coef,          &! in
     & aux_prof,      &! in
     & aux_prof_k   )  ! inout

  If (coef % id_sensor == sensor_id_mw .AND. local_rad_k ) Then
  ! We have K wrt all calculated TBs - but user wants K for
  ! instrument channels, so K code only requires an extra routine to modify
  ! output. In AD code we simply exclude unused channels. Note only required
  ! for microwave calculations.

     Call rttov_profout_k( &
            & nfrequencies,      &! in
            & nchannels,         &! in
            & nbtout,            &! in
            & nprofiles,         &! in
            & channels,          &! in
            & lprofiles,         &! in
            & polarisations,     &! in
            & coef,              &! in
            & angles,            &! in
            & profiles_k_all,    &! in
            & profiles_k)         ! Out
	    
	     Do i = 1, nbtout
	     Enddo
    
  Else
     !profiles_k  = profiles_k_all
     Do i = 1, nchannels
        profiles_k(i) % nlevels          = profiles_k_all(i) % nlevels
        profiles_k(i) % s2m % t          = profiles_k_all(i) % s2m % t
        profiles_k(i) % s2m % q          = profiles_k_all(i) % s2m % q
        profiles_k(i) % s2m % p          = profiles_k_all(i) % s2m % p
        profiles_k(i) % s2m % u          = profiles_k_all(i) % s2m % u
        profiles_k(i) % s2m % v          = profiles_k_all(i) % s2m % v
        profiles_k(i) % skin % t         = profiles_k_all(i) % skin % t
        profiles_k(i) % skin % fastem(1) = profiles_k_all(i) % skin % fastem(1)
        profiles_k(i) % skin % fastem(2) = profiles_k_all(i) % skin % fastem(2)
        profiles_k(i) % skin % fastem(3) = profiles_k_all(i) % skin % fastem(3)
        profiles_k(i) % skin % fastem(4) = profiles_k_all(i) % skin % fastem(4)
        profiles_k(i) % skin % fastem(5) = profiles_k_all(i) % skin % fastem(5)
        profiles_k(i) % ctp              = profiles_k_all(i) % ctp
        profiles_k(i) % cfraction        = profiles_k_all(i) % cfraction

        Do  ii=1,coef%nlevels
           profiles_k(i) % t(ii)         = profiles_k_all(i) % t(ii)
           profiles_k(i) % q(ii)         = profiles_k_all(i) % q(ii)
           profiles_k(i) % o3(ii)        = profiles_k_all(i) % o3(ii)
           profiles_k(i) % clw(ii)       = profiles_k_all(i) % clw(ii)
        End Do
     End Do
  End If

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
        Deallocate( predictors(i)   % wvcont ,stat= alloc_status(5))
     End If
     If( predictors(i) % nco2 > 0 ) Then
        Deallocate( predictors(i)   % co2    ,stat= alloc_status(6))
     End If
     If( Any(alloc_status /= 0) ) Then
        errorstatus(:) = errorstatus_fatal
        Write( errMessage, '( "deallocation of predictors")' )
        Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
        Return
     End If
  End Do

  Do i = 1, nchannels
     Deallocate( predictors_k(i) % mixedgas    ,stat= alloc_status(1))
     Deallocate( predictors_k(i) % watervapour ,stat= alloc_status(2))
     Deallocate( predictors_k(i) % clw         ,stat= alloc_status(3))
     If( predictors_k(i) % nozone > 0 ) Then
        Deallocate( predictors_k(i) % ozone    ,stat= alloc_status(4))
     End If
     If( predictors_k(i) % nwvcont > 0 ) Then
        Deallocate( predictors_k(i)   % wvcont ,stat= alloc_status(5))
     End If
     If( predictors_k(i) % nco2 > 0 ) Then
        Deallocate( predictors_k(i)   % co2    ,stat= alloc_status(6))
     End If
     If( Any(alloc_status /= 0) ) Then
        errorstatus(:) = errorstatus_fatal
        Write( errMessage, '( "deallocation of K edictors")' )
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

  Do i = 1, nprofiles
     If( coef % id_sensor == sensor_id_mw ) Then
        If( Associated( aux_prof(i) % debye_prof) ) Then
           Deallocate( aux_prof(i) % debye_prof ,stat= alloc_status(1))
           If( Any(alloc_status /= 0) ) Then
              errorstatus(:) = errorstatus_fatal
              Write( errMessage, '( "deallocation of debye profile")' )
              Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
           End If
        End If
     Endif
  End Do
  Do i = 1, nchannels
     If( coef % id_sensor == sensor_id_mw ) Then
        If( Associated( aux_prof_k(i) % debye_prof) ) Then
           Deallocate( aux_prof_k(i) % debye_prof ,stat= alloc_status(1))
           If( Any(alloc_status /= 0) ) Then
              errorstatus(:) = errorstatus_fatal
              Write( errMessage, '( "deallocation of K debye profile")' )
              Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
           End If
        End If
     Endif
  End Do

  Do i = 1, nchannels
     If( Associated(  profiles_k_all(i) % p )) Then
        Deallocate( profiles_k_all(i) % p ,stat= alloc_status(1))
        Deallocate( profiles_k_all(i) % t ,stat= alloc_status(2))
        Deallocate( profiles_k_all(i) % q ,stat= alloc_status(3))
        Deallocate( profiles_k_all(i) % o3  ,stat= alloc_status(4))
        Deallocate( profiles_k_all(i) % clw ,stat= alloc_status(5))
        If( Any(alloc_status /= 0) ) Then
           errorstatus(:) = errorstatus_fatal
           Write( errMessage, '( "deallocation of profiles_k_all")' )
           Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
           Return
        End If
     End If
  End do

  If( local_rad_k ) Then
     Deallocate( radiancedata_k % clear     ,stat= alloc_status(1))
     Deallocate( radiancedata_k % cloudy    ,stat= alloc_status(2))
     Deallocate( radiancedata_k % total     ,stat= alloc_status(3))
     Deallocate( radiancedata_k % bt        ,stat= alloc_status(4))
     Deallocate( radiancedata_k % bt_clear  ,stat= alloc_status(5))
     Deallocate( radiancedata_k % out       ,stat= alloc_status(4))
     Deallocate( radiancedata_k % out_clear ,stat= alloc_status(5))
     Deallocate( radiancedata_k % upclear   ,stat= alloc_status(6))
     Deallocate( radiancedata_k % reflclear ,stat= alloc_status(7))
     Deallocate( radiancedata_k % overcast  ,stat= alloc_status(8))
     Deallocate( radiancedata_k % downcld   ,stat= alloc_status(9))
     Deallocate( radiancedata_k % total_out  ,stat= alloc_status(10))
     Deallocate( radiancedata_k % clear_out  ,stat= alloc_status(11))
     If( Any(alloc_status /= 0) ) Then
        errorstatus(:) = errorstatus_fatal
        Write( errMessage, '( "deallocation of K radiances")' )
        Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
        Return
     End If
  End If

End Subroutine rttov_k
