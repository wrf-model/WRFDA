!
Subroutine rttov_profaux( &
     & prof,   &! in
     & coef,   &! in
     & aux)     ! inout
  !
  ! Description:
  ! Calculates some variables related to the input profile.
  ! variables are nearest surface level, nearest cloud top level
  ! and Debye terms for MW
  ! The reason of having a separate structure for these
  ! variables is that the input profiles should be "read only"
  ! in RTTOV context.
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
  Use rttov_const, Only : &
       & sensor_id_mw,     &! sensor id number for MW
       & mwcldtop ,        &! Upper level for lwp calcs
       & dcoeff             ! debye coefficients

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type   ,&
       & profile_aux


  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Type(profile_Type),  Intent(in)    :: prof  ! profile
  Type(rttov_coef)  ,  Intent(in)    :: coef  ! coefficients
  Type(profile_aux) ,  Intent(inout) :: aux   ! auxilary profile info



  !local
  Integer(Kind=jpim) :: lev
  Real(Kind=jprb)    :: v    ! temperature ratio
  !- End of header --------------------------------------------------------

  !-----------------------------------------
  !2. determine cloud top and surface levels
  !-----------------------------------------

  !nearest level above surface
  Do lev = prof % nlevels - 1, 1, -1
     If ( prof % s2m % p > coef % ref_prfl_p( lev ) ) Exit
  End Do
  aux % nearestlev_surf = lev + 1
  aux % pfraction_surf  = &
       & (coef % ref_prfl_p(aux % nearestlev_surf) - prof % s2m % p)&
        & / coef % dp(aux % nearestlev_surf)

  If( coef % id_sensor /= sensor_id_mw ) Then
     !nearest level above cloud top
     Do lev = prof % nlevels-1, 1, -1
        If ( prof % ctp > coef % ref_prfl_p(lev) ) Exit
     End Do
     aux % nearestlev_ctp = lev+1
     aux % pfraction_ctp  =&
           & (coef % ref_prfl_p(aux % nearestlev_ctp) - prof % ctp)&
           & / coef % dp(aux % nearestlev_ctp)
     aux % cfraction = prof%cfraction
  Else
     ! for micro waves do not consider clouds in the RTTOV basis routines
     aux % nearestlev_ctp = prof % nlevels-1
     aux % pfraction_ctp  = 0._JPRB
     aux % cfraction      = 0._JPRB
  Endif

  ! Description:
  !   To calculate individual debye terms for temperature
  !   at each level. There are five debye terms. These
  !   will be used in fastem and opdep to calculate
  !   permittivity which is required for surface emissivity
  !   and cloud modelling
  !
  ! Method:
  !   The model is a hybrid of LIEBE MPM 1993 and the PIOM laboratory
  !   measurements reported by ELLISON et al. 1999.

  If ( prof % clw_Data ) Then
     Do lev = mwcldtop, prof % nlevels
        v = 300.0_JPRB / prof % t(lev) - 1.0_JPRB
        aux % debye_prof(1,lev) = dcoeff(1) - dcoeff(2) * v + dcoeff(3) * v*v
        aux % debye_prof(2,lev) = dcoeff(4) * aux % debye_prof(1,lev)
        aux % debye_prof(3,lev) = dcoeff(5) * v + dcoeff(6)
        aux % debye_prof(4,lev) = dcoeff(7) * aux % debye_prof(3,lev)
        aux % debye_prof(5,lev) = dcoeff(8)
     Enddo

  Endif



End Subroutine rttov_profaux
