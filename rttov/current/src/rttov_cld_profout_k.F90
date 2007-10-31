!
Subroutine rttov_cld_profout_k( &
            nfrequencies, &       ! in
            nchannels, &          ! in
            nbtout, &             ! in
            nprofiles, &          ! in
            channels, &           ! in
            lprofiles, &          ! in
            polarisations, &      ! in
            coef, &               ! in
            geometry, &           ! in
            cld_profiles_k_all, & ! in
            cld_profiles_k)       ! Out
  ! Description:
  ! To convert an K-matrix brightness temperatures with 1, 2 or 4 polarisations
  ! polarisation requested by the user.
  ! There are seven options:
  ! 0. Return average of V and H polarisation.
  ! 1. Return AMSU-style mix polarisation (nominal V at nadir)
  ! 2. Return AMSU-style mix polarisation (nominal H at nadir)
  ! 3. Return Vertical polarisation
  ! 4. Return Horizontal polarisation
  ! 5. Return vertical and horizontal polarisation
  ! 6. Return full Stokes vector
  !
  ! For IR channels this variable is not required, and one unpolarised brightness
  ! temperature is computed.
  !
  ! Note options 0-4 return one polarisation per channel. Option 5 returns
  ! 2 polarisations per channel and option 6 four polarisations per channel.
  ! Note also that for options 1-3 two polarisations must be computed in RTTOV,
  ! even though only one is returned. For this reason rad%bt is replaced by
  ! rad%out, where rad%out has length of number of output channels, whereas
  ! rad%bt has length of all brightness temperatures computed in RTTOV.
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
  !    Copyright 2003, EUMETSAT, All Rights Reserved.
  !
  ! Method: Uses band correction coefficients for each channel
  !         read in from RT coefficient file.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  ! 1.0    10/07/2003  New code required for polarimetric RTTOV (Steve English)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:

  Use rttov_const, only :   &
       npolar_return,       &
       npolar_compute,      &
       pol_v ,              &
       pol_h

  Use rttov_types, Only :  &
       rttov_coef,         &
       profile_cloud_Type, &
       geometry_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),  Intent(in)            :: nbtout
  Integer(Kind=jpim),  Intent(in)            :: nchannels
  Integer(Kind=jpim),  Intent(in)            :: nfrequencies
  Integer(Kind=jpim),  Intent(in)            :: nprofiles
  Type(geometry_Type), Intent(in) ,Target    :: geometry(nprofiles)
  Integer(Kind=jpim),  Intent(in)            :: channels(nfrequencies)
  Integer(Kind=jpim),  Intent(in)            :: polarisations(nchannels,3)
  Integer(Kind=jpim),  Intent(in)            :: lprofiles(nfrequencies)
  Type(rttov_coef),    Intent(in)            :: coef           ! Coefficients
  Type(profile_cloud_Type),  Intent(inout) ,Target :: cld_profiles_k(nbtout)
  Type(profile_cloud_Type),  Intent(inout) ,Target :: cld_profiles_k_all(nchannels)

  ! radiances are expressed in mw/cm-1/ster/sq.m
  ! and temperatures in Kelvin

  !local variables:
  Real(Kind=jprb) :: emissfactor_h,emissfactor_v
  Integer(Kind=jpim) :: chan,i,j,ich,ich2,pol_id,ii,nwp_levels
  !Type(geometry_Type), Pointer    :: geom

  !- End of header ------------------------------------------------------

   nwp_levels = cld_profiles_k(1)%nlevels

   ich2=1
   Do i=1,nfrequencies
      chan        = channels(i)
      pol_id      = 0
      pol_id = coef % fastem_polar(chan) + 1
      ich = polarisations(i,1)
      If (pol_id >= 5) then
         ! Return all calculated polarisations (or just computed TB for IR channels)
         Do j=1,polarisations(i,3)
            Do  ii=1,nwp_levels
               cld_profiles_k(ich2+j-1) % p(ii)    = cld_profiles_k_all(ich+j-1) % p(ii)
               cld_profiles_k(ich2+j-1) % ph(ii)   = cld_profiles_k_all(ich+j-1) % ph(ii)
               cld_profiles_k(ich2+j-1) % t(ii)    = cld_profiles_k_all(ich+j-1) % t(ii)
               cld_profiles_k(ich2+j-1) % cc(ii)   = cld_profiles_k_all(ich+j-1) % cc(ii)
               cld_profiles_k(ich2+j-1) % clw(ii)  = cld_profiles_k_all(ich+j-1) % clw(ii)
               cld_profiles_k(ich2+j-1) % ciw(ii)  = cld_profiles_k_all(ich+j-1) % ciw(ii)
               cld_profiles_k(ich2+j-1) % rain(ii) = cld_profiles_k_all(ich+j-1) % rain(ii)
               cld_profiles_k(ich2+j-1) % sp(ii)   = cld_profiles_k_all(ich+j-1) % sp(ii)
            enddo
            cld_profiles_k(ich2+j-1) % ph(nwp_levels+1)  = cld_profiles_k_all(ich+j-1) % ph(nwp_levels+1)
         End Do
      Else If (pol_id == 1) then
         ! Return average of V and H polarisation
         Do  ii=1,nwp_levels
            cld_profiles_k(ich2) % p(ii)        = cld_profiles_k_all(ich) % p(ii) + cld_profiles_k_all(ich+1) % p(ii)
            cld_profiles_k(ich2) % ph(ii)       = cld_profiles_k_all(ich) % ph(ii) + cld_profiles_k_all(ich+1) % ph(ii)
            cld_profiles_k(ich2) % t(ii)        = cld_profiles_k_all(ich) % t(ii) + cld_profiles_k_all(ich+1) % t(ii)
            cld_profiles_k(ich2) % cc(ii)       = cld_profiles_k_all(ich) % cc(ii) + cld_profiles_k_all(ich+1) % cc(ii)
            cld_profiles_k(ich2) % clw(ii)      = cld_profiles_k_all(ich) % clw(ii) + cld_profiles_k_all(ich+1) % clw(ii)
            cld_profiles_k(ich2) % ciw(ii)      = cld_profiles_k_all(ich) % ciw(ii) + cld_profiles_k_all(ich+1) % ciw(ii)
            cld_profiles_k(ich2) % rain(ii)     = cld_profiles_k_all(ich) % rain(ii) + cld_profiles_k_all(ich+1) % rain(ii)
            cld_profiles_k(ich2) % sp(ii)       = cld_profiles_k_all(ich) % sp(ii) + cld_profiles_k_all(ich+1) % sp(ii)
         enddo
         cld_profiles_k(ich2) % ph(nwp_levels+1)  = cld_profiles_k_all(ich) % ph(nwp_levels+1) + &
              cld_profiles_k_all(ich+1) % ph(nwp_levels+1)
      Else
         !geom        => geometry( lprofiles(i) )
         emissfactor_v = pol_v(1,pol_id)+pol_v(2,pol_id)+pol_v(3,pol_id)
         emissfactor_h = pol_h(1,pol_id)+pol_h(2,pol_id)+pol_h(3,pol_id)
         Do  ii=1,nwp_levels
            cld_profiles_k(ich2) % p(ii)        = cld_profiles_k_all(ich) % p(ii)*emissfactor_v +&
                                                cld_profiles_k_all(ich+1) % p(ii)*emissfactor_h
            cld_profiles_k(ich2) % ph(ii)       = cld_profiles_k_all(ich) % ph(ii)*emissfactor_v +&
                                                cld_profiles_k_all(ich+1) % ph(ii)*emissfactor_h
            cld_profiles_k(ich2) % t(ii)        = cld_profiles_k_all(ich) % t(ii)*emissfactor_v + &
                                                cld_profiles_k_all(ich+1) % t(ii)*emissfactor_h
            cld_profiles_k(ich2) % cc(ii)       = cld_profiles_k_all(ich) % cc(ii)*emissfactor_v + &
                                                cld_profiles_k_all(ich+1) % cc(ii)*emissfactor_h
            cld_profiles_k(ich2) % clw(ii)      = cld_profiles_k_all(ich) % clw(ii)*emissfactor_v + &
                                                cld_profiles_k_all(ich+1) % clw(ii)*emissfactor_h
            cld_profiles_k(ich2) % ciw(ii)      = cld_profiles_k_all(ich) % ciw(ii)*emissfactor_v + &
                                                cld_profiles_k_all(ich+1) % ciw(ii)*emissfactor_h
            cld_profiles_k(ich2) % rain(ii)     = cld_profiles_k_all(ich) % rain(ii)*emissfactor_v + &
                                                cld_profiles_k_all(ich+1) % rain(ii)*emissfactor_h
            cld_profiles_k(ich2) % sp(ii)       = cld_profiles_k_all(ich) % sp(ii)*emissfactor_v + &
                                                cld_profiles_k_all(ich+1) % sp(ii)*emissfactor_h
         enddo
         cld_profiles_k(ich2) % ph(nwp_levels+1) = cld_profiles_k_all(ich) % ph(nwp_levels+1)*emissfactor_v +&
                                                cld_profiles_k_all(ich+1) % ph(nwp_levels+1)*emissfactor_h
      End If
      ich2 = ich2 + npolar_return(pol_id)
   End Do
End Subroutine rttov_cld_profout_k




