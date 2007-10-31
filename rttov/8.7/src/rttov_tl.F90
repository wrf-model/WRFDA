!
Subroutine rttov_tl( &
     & errorstatus,   &! out
     & nfrequencies,  &! in
     & nchannels,     &! in
     & nbtout,        &! in
     & nprofiles,     &! in
     & channels,      &! in
     & polarisations, &! in
     & lprofiles,     &! in
     & profiles,      &! in
     & coef,          &! in
     & addcloud,      &! in
     & calcemis,      &! in
     & emissivity,    &! inout
     & profiles_tl,     &! in
     & emissivity_tl,   &! inout
     & transmission,    &! inout
     & transmission_tl, &! inout
     & radiancedata,    &! inout
     & radiancedata_tl ) ! inout
  !
  ! Description:
  ! Tangent Linear of rttov_direct
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
  !  1.4               Add WV Continuum and CO2 capability
  !  1.5   04/12/2003  Optimisation (J Hague and D Salmond ECMWF)
  !  1.6   02/06/2004  Change tests on id_comp_lvl == 7 by tests on fmv_model_ver (P. Brunel)
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
#include "rttov_checkinput.interface"
#include "rttov_profaux.interface"
#include "rttov_setgeometry.interface"
#include "rttov_setpredictors.interface"
#include "rttov_setpredictors_8.interface"
#include "rttov_transmit.interface"
#include "rttov_calcemis_ir.interface"
#include "rttov_calcemis_mw.interface"
#include "rttov_integrate.interface"
#include "rttov_profaux_tl.interface"
#include "rttov_setpredictors_tl.interface"
#include "rttov_setpredictors_8_tl.interface"
#include "rttov_transmit_tl.interface"
#include "rttov_calcemis_mw_tl.interface"
#include "rttov_integrate_tl.interface"

  !subroutine arguments:
  Integer(Kind=jpim),      Intent(in)    :: nchannels
  Integer(Kind=jpim),      Intent(in)    :: nbtout
  Integer(Kind=jpim),      Intent(in)    :: nfrequencies
  Integer(Kind=jpim),      Intent(in)    :: nprofiles
  Integer(Kind=jpim),      Intent(out)   :: errorstatus(nprofiles)
  Integer(Kind=jpim),      Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),      Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),      Intent(in)    :: lprofiles(nfrequencies)
  Logical,                 Intent(in)    :: addcloud
  Type(profile_Type),      Intent(in)    :: profiles(nprofiles)
  Type(rttov_coef),        Intent(in)    :: coef
  Logical,                 Intent(in)    :: calcemis(nchannels)
  Real(Kind=jprb),         Intent(inout) :: emissivity(nchannels)
  Type(transmission_Type), Intent(inout) :: transmission! in because of meme allocation
  Type(radiance_Type),     Intent(inout) :: radiancedata! in because of meme allocation


  Type(profile_Type),      Intent(in)    :: profiles_tl(nprofiles)
  Real(Kind=jprb),         Intent(inout) :: emissivity_tl(nchannels)
  Type(transmission_Type), Intent(inout) :: transmission_tl! in because of meme allocation
  Type(radiance_Type),     Intent(inout) :: radiancedata_tl ! in because of meme allocation

  !local variables:
  Integer(Kind=jpim) :: i               ! loop index
  Logical :: addcosmic          ! switch for adding temp of cosmic background
  Real(Kind=jprb)    :: reflectivity(nchannels)      ! surface reflectivity
  Real(Kind=jprb)    :: reflectivity_tl(nchannels)   ! TL surface reflectivity
  Real(Kind=jprb)    :: od_layer(coef%nlevels,nchannels) ! layer optical depth
  Real(Kind=jprb)    :: opdp_ref(coef%nlevels,nfrequencies) ! layer optical depth before threshold

  Character (len=80) :: errMessage
  Character (len=8)  :: NameOfRoutine = 'rttov_tl'

  Type(geometry_Type)   :: angles(nprofiles)     ! geometry angles
  Type(predictors_Type) :: predictors(nprofiles) ! predictors
  Type(profile_aux)     :: aux_prof(nprofiles)   ! auxillary profiles informations

  Type(predictors_Type) :: predictors_tl(nprofiles) ! TL of above predictors
  Type(profile_aux)     :: aux_prof_tl(nprofiles)   ! TL of above aux_prof
  Type(radiance_aux)    :: auxrad

  Real(Kind=jprb), target ::  zdeb   (5,coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zdeb_tl(5,coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zmixed   (coef%nmixed,coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zmixed_tl(coef%nmixed,coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zwater   (coef%nwater,coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zwater_tl(coef%nwater,coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zlev   (coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zlev_tl(coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zozone   (coef%nozone,coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zozone_tl(coef%nozone,coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zwvcont   (coef%nwvcont,coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zwvcont_tl(coef%nwvcont,coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zco2   (coef%nco2,coef%nlevels,nprofiles)
  Real(Kind=jprb), target ::  zco2_tl(coef%nco2,coef%nlevels,nprofiles)

  Real(Kind=jprb), target ::  surfair(nchannels)
  Real(Kind=jprb), target ::  skin   (nchannels)
  Real(Kind=jprb), target ::  cosmic (nchannels)
  Real(Kind=jprb), target ::  layer(coef%nlevels,nchannels)
  Real(Kind=jprb), target ::  up        (coef%nlevels,nchannels)
  Real(Kind=jprb), target ::  down      (coef%nlevels,nchannels)
  Real(Kind=jprb), target ::  down_cloud(coef%nlevels,nchannels)

  Integer(Kind=jpim) :: jn

  !- End of header --------------------------------------------------------

  !-------------
  !0. initialize
  !-------------

  errorstatus(:)  = errorstatus_success

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
  If( coef % id_sensor == sensor_id_mw ) Then
     jn=coef%nlevels
     Do i = 1, nprofiles
       aux_prof(i) % debye_prof => zdeb(1:5,1:jn,i)
     End Do
  Endif
  Do i = 1, nprofiles
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

  jn=coef%nlevels
  Do i = 1, nprofiles
     predictors(i) % nlevels  = coef % nlevels
     predictors(i) % nmixed   = coef % nmixed
     predictors(i) % nwater   = coef % nwater
     predictors(i) % nozone   = coef % nozone
     predictors(i) % nwvcont  = coef % nwvcont
     predictors(i) % nco2     = coef % nco2
     predictors(i) % ncloud   = 0 ! (can be set to 1 inside setpredictors)


     predictors(i) % mixedgas    => zmixed(1:coef%nmixed, 1:jn, i)
     predictors(i) % watervapour => zwater(1:coef%nwater, 1:jn, i)
     predictors(i) % clw         => zlev(1:jn, i)

     If( coef%nozone > 0 ) Then
        predictors(i) % ozone    => zozone(1:coef%nozone, 1:jn, i)
     End If
     If( coef%nwvcont > 0 ) Then
        predictors(i) % wvcont    => zwvcont(1:coef%nwvcont, 1:jn, i)
     End If
     If( coef%nco2 > 0 ) Then
        predictors(i) % co2    => zco2(1:coef%nco2, 1:jn, i)
     End If
  End Do ! Profile loop


  Do i = 1, nprofiles
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
     reflectivity(:) = 1 - emissivity(:)
  End If


  !--------------------------------------------
  !8. integrate the radiative transfer equation
  !--------------------------------------------

  auxrad % layer   => layer(:,:)
  auxrad % surfair => surfair(:)
  auxrad % skin    => skin(:)
  auxrad % cosmic  => cosmic(:)
  auxrad % up      => up(:,:)
  auxrad % down    => down(:,:)

  If ( addcloud ) then
     auxrad % down_cloud => down_cloud(:,:)
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



  ! Tangent Linear
  !----------------

  !

  Do i = 1, nprofiles
     aux_prof_tl(i) % nearestlev_surf = 0   ! no meaning
     aux_prof_tl(i) % nearestlev_ctp  = 0   ! no meaning
     aux_prof_tl(i) % pfraction_surf  = 0._JPRB  ! calculated
     aux_prof_tl(i) % pfraction_ctp   = 0._JPRB  ! calculated   inside rttov_profaux_tl
     aux_prof_tl(i) % cfraction       = 0._JPRB  ! calculated
     ! Note that  cfraction and pfraction_ctp are set to 0 in case
     ! of MicroWaves
     If( coef % id_sensor == sensor_id_mw ) Then
        aux_prof_tl(i) % debye_prof => zdeb_tl(1:5,1:jn,i)
     Endif
  End Do

  Do i = 1, nprofiles
     If( coef % id_sensor == sensor_id_mw ) Then
        aux_prof_tl(i) % debye_prof(:,:)  = 0._JPRB
     Endif
     Call rttov_profaux_tl( &
          & profiles(i),     &! in
          & profiles_tl(i),  &! in
          & coef,            &! in
          & aux_prof(i),     &! in
          & aux_prof_tl(i))   ! inout
  End Do

  ! No TL on geometry

  ! TL of predictors

  jn=coef%nlevels
  Do i = 1, nprofiles
     predictors_tl(i) % mixedgas    => zmixed_tl(1:coef%nmixed, 1:jn, i)
     predictors_tl(i) % watervapour => zwater_tl(1:coef%nwater, 1:jn, i)
     predictors_tl(i) % clw         => zlev_tl(1:jn, i)
     If( coef%nozone > 0 ) Then
        predictors_tl(i) % ozone    => zozone_tl(1:coef%nozone, 1:jn, i)
     End If
     If( coef%nwvcont > 0 ) Then
        predictors_tl(i) % wvcont    => zwvcont_tl(1:coef%nwvcont, 1:jn, i)
     End If

     If( coef%nco2 > 0 ) Then
        predictors_tl(i) % co2    => zco2_tl(1:coef%nco2, 1:jn, i)
     End If

  End Do

  Do i = 1, nprofiles
     predictors_tl(i) % nlevels  = predictors(i) % nlevels
     predictors_tl(i) % nmixed   = predictors(i) % nmixed
     predictors_tl(i) % nwater   = predictors(i) % nwater
     predictors_tl(i) % nozone   = predictors(i) % nozone
     predictors_tl(i) % nwvcont  = predictors(i) % nwvcont
     predictors_tl(i) % nco2     = predictors(i) % nco2
     predictors_tl(i) % ncloud   = predictors(i) % ncloud

     If (coef%fmv_model_ver == 7) Then
        Call rttov_setpredictors_tl( &
             & profiles(i),         &! in
             & profiles_tl(i),      &! in
             & angles(i),           &! in
             & coef,                &! in
             & predictors(i),       &! in
             & predictors_tl(i) )    ! inout

     ElseIf (coef%fmv_model_ver == 8) Then
        Call rttov_setpredictors_8_tl( &
             & profiles(i),         &! in
             & profiles_tl(i),      &! in
             & angles(i),           &! in
             & coef,                &! in
             & predictors(i),       &! in
             & predictors_tl(i) )    ! inout
     End If
  End Do


  !TL of optical depths and transmittances
  Call rttov_transmit_tl( &
       & nfrequencies,    &! in
       & nchannels,       &! in
       & nprofiles,       &! in
       & coef%nlevels,    &! in
       & channels,        &! in
       & polarisations,   &! in
       & lprofiles,       &! in
       & predictors,      &! in
       & predictors_tl,   &! in
       & aux_prof,        &! in
       & aux_prof_tl,     &! in
       & coef,            &! in
       & od_layer,        &! in
       & opdp_ref,        &! in
       & transmission,    &! in
       & transmission_tl ) ! inout

  If ( Any(calcemis) ) Then
     ! calculate surface emissivity for selected channels
     ! and reflectivity
     If ( coef % id_sensor == sensor_id_ir ) Then
        !Infrared
        ! nothing to do
        reflectivity_tl(:) =  - emissivity_tl(:)

        Elseif ( coef % id_sensor == sensor_id_mw ) Then
           !Microwave
        Call rttov_calcemis_mw_tl ( &
             & profiles,        &! in
             & profiles_tl,     &! in
             & angles,          &! in
             & coef,            &! in
             & nfrequencies,    &! in
             & nchannels,       &! in
             & nprofiles,       &! in
             & channels,        &! in
             & polarisations,   &! in
             & lprofiles,       &! in
             & transmission,    &! in
             & transmission_tl, &! in
             & calcemis,        &! in
             & emissivity_tl,   &! inout
             & reflectivity_tl ) ! out
     Else
        ! Hires
        reflectivity_tl(:) = - emissivity_tl(:)
     End If

     ! reflectivity for other channels
     Where( .Not. calcemis(:) )
        reflectivity_tl(:) = - emissivity_tl(:)
     End Where

  Else
     ! reflectivity for all channels
     reflectivity_tl(:) = - emissivity_tl(:)
  End If

  !--------------------------------------------
  !8. integrate the radiative transfer equation
  !--------------------------------------------

  addcosmic = ( coef % id_sensor == sensor_id_mw )
  Call rttov_integrate_tl( &
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
       & emissivity_tl,    &! in
       & reflectivity,     &! in
       & reflectivity_tl,  &! in
       & transmission,     &! in
       & transmission_tl,  &! in
       & profiles,         &! in
       & profiles_tl,      &! in
       & aux_prof,         &! in
       & aux_prof_tl,      &! in
       & coef,             &! in
       & radiancedata,     &! in
       & auxrad ,          &! in
       & radiancedata_tl  ) ! inout


End Subroutine rttov_tl
