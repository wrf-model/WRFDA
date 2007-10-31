Subroutine rttov_profaux_tl( &
     & prof,      &! in
     & prof_tl,   &! in
     & coef,      &! in
     & aux,       &! in
     & aux_tl)     ! out
  !
  ! Description:
  ! TL of rttov_profaux
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
  !  1.0    07/10/2004  Added history
  !  1.1    29/03/2005  Add end of header comment (J. Cameron)
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

  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type   ,&
       & profile_aux

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Type(profile_Type),  Intent(in)  :: prof
  Type(profile_Type),  Intent(in)  :: prof_tl
  Type(rttov_coef)  ,  Intent(in)  :: coef
  Type(profile_aux) ,  Intent(in)  :: aux
  Type(profile_aux) ,  Intent(inout) :: aux_tl



  ! local
  Integer(Kind=jpim) :: lev
  Real(Kind=jprb)    :: v            ! temperature ratio
  Real(Kind=jprb)    :: v_tl         ! temperature ratio

  !- End of header --------------------------------------------------------

  !-----------------------------------------
  !2. determine cloud top and surface levels
  !-----------------------------------------

  !nearest level above surface
  aux_tl % pfraction_surf  =&
        & - prof_tl % s2m % p / coef % dp(aux % nearestlev_surf)


  If( coef % id_sensor /= sensor_id_mw ) Then
     !nearest level above cloud top
     aux_tl % pfraction_ctp  =&
           & - prof_tl % ctp / coef % dp(aux % nearestlev_ctp)
     aux_tl % cfraction = prof_tl%cfraction
  Else
     ! for micro waves do not consider clouds in the RTTOV basis routines
     aux_tl % pfraction_ctp  = 0._JPRB
     aux_tl % cfraction      = 0._JPRB
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
        v    = 300.0_JPRB  / prof % t(lev) - 1.0_JPRB
        v_tl = -300.0_JPRB * prof_tl % t(lev) / prof % t(lev)**2
        aux_tl % debye_prof(1,lev) = - dcoeff(2) * v_tl +&
              & 2 * dcoeff(3) * v_tl * v
        aux_tl % debye_prof(2,lev) = dcoeff(4) * aux_tl % debye_prof(1,lev)
        aux_tl % debye_prof(3,lev) = dcoeff(5) * v_tl
        aux_tl % debye_prof(4,lev) = dcoeff(7) * aux_tl % debye_prof(3,lev)
        aux_tl % debye_prof(5,lev) = 0._JPRB
     Enddo

  Endif



End Subroutine rttov_profaux_tl
