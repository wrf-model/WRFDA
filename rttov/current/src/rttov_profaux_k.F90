Subroutine rttov_profaux_k( &
     & nfrequencies,  &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & polarisations, &! in
     & lprofiles,     &! in
     & profiles,      &! in
     & profiles_k,    &! inout
     & coef,          &! in
     & aux_prof,      &! in
     & aux_prof_k   )  ! inout
  !
  ! Description:
  ! K of rttov_profaux
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
  !    Copyright 2005, EUMETSAT, All Rights Reserved.
  !
  ! Method:
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       22/06/2005  initial (P Brunel)
  !                        based on version 1.0 (07/10/04) of AD code
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
  Integer(Kind=jpim),    Intent(in)    :: nfrequencies            ! Number of frequencies
  Integer(Kind=jpim),    Intent(in)    :: nchannels               ! Number of output radiances
  Integer(Kind=jpim),    Intent(in)    :: nprofiles               ! Number of profiles
  Integer(Kind=jpim),    Intent(in)    :: polarisations(nchannels,3) ! polarisation indices
  Integer(Kind=jpim),    Intent(in)    :: lprofiles(nfrequencies)    ! Profiles indices

  Type(profile_Type),    Target, Intent(in)     :: profiles(nprofiles)
  Type(profile_Type),    Target, Intent(inout)  :: profiles_k(nchannels)
  Type(profile_aux),     Target, Intent(in)     :: aux_prof(nprofiles)
  Type(profile_aux),     Target, Intent(inout)  :: aux_prof_k(nchannels)
  Type(rttov_coef),              Intent(in)     :: coef

  !local variables:
  Type(profile_Type),  Pointer  :: prof
  Type(profile_Type),  Pointer  :: prof_k
  Type(profile_aux) ,  Pointer  :: aux
  Type(profile_aux) ,  Pointer  :: aux_k

  Integer(Kind=jpim) :: lev
  Integer(Kind=jpim) :: i
  Integer(Kind=jpim) :: j
  Integer(Kind=jpim) :: freq

  Real(Kind=jprb)    :: v            ! temperature ratio
  Real(Kind=jprb)    :: v_k         ! temperature ratio
  !- End of header --------------------------------------------------------
  nullify ( prof )
  nullify ( prof_k )
  nullify ( aux )
  nullify ( aux_k )

  Do i = 1, nchannels
     freq=polarisations(i,2)
     j = lprofiles( freq )

     prof   => profiles(j)
     aux    => aux_prof(j)
     prof_k => profiles_k(i)
     aux_k  => aux_prof_k(i)

     If ( prof % clw_Data ) Then
        Do lev = prof % nlevels, mwcldtop, -1
           v    = 300.0_JPRB  / prof % t(lev) - 1.0_JPRB

           aux_k % debye_prof(3,lev) = aux_k % debye_prof(3,lev) +&
                & aux_k % debye_prof(4,lev) * dcoeff(7)

           v_k = aux_k % debye_prof(3,lev) * dcoeff(5)

           aux_k % debye_prof(1,lev) = aux_k % debye_prof(1,lev)  +&
                & aux_k % debye_prof(2,lev) * dcoeff(4)

           v_k = v_k + aux_k % debye_prof(1,lev) *&
                & (- dcoeff(2) + 2 * dcoeff(3) * v)

           prof_k % t(lev) = prof_k % t(lev) + v_k *&
                & (-300.0_JPRB  / prof % t(lev)**2)

        Enddo
        !aux_k % debye_prof(:,:) = 0.
     Endif

     !nearest level above surface
     prof_k % s2m % p = prof_k % s2m % p - aux_k % pfraction_surf /&
          & coef % dp(aux % nearestlev_surf)

  End Do

  If( coef % id_sensor /= sensor_id_mw ) Then
     Do i = 1, nchannels
        freq=polarisations(i,2)
        j = lprofiles( freq )

        prof   => profiles(j)
        aux    => aux_prof(j)
        prof_k => profiles_k(i)
        aux_k  => aux_prof_k(i)
        !nearest level above cloud top
        prof_k%cfraction = prof_k%cfraction + aux_k % cfraction

        prof_k % ctp = prof_k % ctp - aux_k % pfraction_ctp /&
             & coef % dp(aux % nearestlev_ctp)
     End Do
     !Else
     ! for micro waves do not consider clouds in the RTTOV basis routines
  Endif

  nullify ( prof )
  nullify ( prof_k )
  nullify ( aux )
  nullify ( aux_k )

End Subroutine rttov_profaux_k
