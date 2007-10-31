Subroutine rttov_profaux_ad( &
     & prof,      &! in
     & prof_ad,   &! inout
     & coef,      &! in
     & aux,       &! in
     & aux_ad)     ! inout
  !
  ! Description:
  ! AD of rttov_profaux
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
  !  1.0    07/10/2004 Added history
  !  1.1    29/03/2005 Add end of header comment (J. Cameron)
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
  Type(profile_Type),  Intent(in)   :: prof
  Type(profile_Type),  Intent(inout):: prof_ad
  Type(rttov_coef)  ,  Intent(in)   :: coef
  Type(profile_aux) ,  Intent(in)   :: aux
  Type(profile_aux) ,  Intent(inOUT):: aux_ad



  ! local
  Integer(Kind=jpim) :: lev
  Real(Kind=jprb)    :: v            ! temperature ratio
  Real(Kind=jprb)    :: v_ad         ! temperature ratio

  !- End of header --------------------------------------------------------

  If ( prof % clw_Data ) Then
     Do lev = prof % nlevels, mwcldtop, -1
        v    = 300.0_JPRB  / prof % t(lev) - 1.0_JPRB

        aux_ad % debye_prof(3,lev) = aux_ad % debye_prof(3,lev) +&
              & aux_ad % debye_prof(4,lev) * dcoeff(7)

        v_ad = aux_ad % debye_prof(3,lev) * dcoeff(5)

        aux_ad % debye_prof(1,lev) = aux_ad % debye_prof(1,lev)  +&
              & aux_ad % debye_prof(2,lev) * dcoeff(4)

        v_ad = v_ad + aux_ad % debye_prof(1,lev) *&
              & (- dcoeff(2) + 2 * dcoeff(3) * v)

        prof_ad % t(lev) = prof_ad % t(lev) + v_ad *&
              & (-300.0_JPRB  / prof % t(lev)**2)

     Enddo
     !aux_ad % debye_prof(:,:) = 0.
  Endif

  If( coef % id_sensor /= sensor_id_mw ) Then
     !nearest level above cloud top
     prof_ad%cfraction = prof_ad%cfraction + aux_ad % cfraction

     prof_ad % ctp = prof_ad % ctp - aux_ad % pfraction_ctp /&
           & coef % dp(aux % nearestlev_ctp)
  !Else
     ! for micro waves do not consider clouds in the RTTOV basis routines
  Endif

  !nearest level above surface
  prof_ad % s2m % p = prof_ad % s2m % p - aux_ad % pfraction_surf /&
        & coef % dp(aux % nearestlev_surf)


End Subroutine rttov_profaux_ad
