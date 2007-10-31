!
Subroutine rttov_calcpolarisation_ad( &
     & nfrequencies,   & ! in
     & nchannels,      & ! in
     & nbtout,         & ! in
     & profiles,       & ! in
     & nprofiles,      & ! in
     & geometry,       & ! in
     & channels,       & ! in
     & polarisations,  & ! in
     & lprofiles,      & ! in
     & coeffs,         & ! in
     & rad_ad          ) ! inout
  ! Description:
  ! To convert an array of brightness temperatures with 1, 2 or 4 polarisations
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
       & npolar_return,       &
       & pol_v,               &
       & pol_h

  Use rttov_types, Only : &
       & rttov_coef     ,&
       & radiance_Type  ,&
       & profile_Type   ,&
       & geometry_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),  Intent(in)           :: nfrequencies
  Integer(Kind=jpim),  Intent(in)           :: nchannels
  Integer(Kind=jpim),  Intent(in)           :: nbtout
  Integer(Kind=jpim),  Intent(in)           :: nprofiles
  Type(profile_Type),  Intent(in) ,Target   :: profiles(nprofiles)
  Type(geometry_Type), Intent(in) ,Target   :: geometry(nprofiles)
  Integer(Kind=jpim),  Intent(in)           :: channels(nfrequencies)
  Integer(Kind=jpim),  Intent(in)           :: polarisations(nchannels,3)
  Integer(Kind=jpim),  Intent(in)           :: lprofiles(nfrequencies)
  Type(rttov_coef),    Intent(in)           :: coeffs             ! Coefficients
  Type(radiance_Type), Intent(inout)        :: rad_ad             ! input radiances and output BT

  ! radiances are expressed in mw/cm-1/ster/sq.m
  ! and temperatures in Kelvin

  !local variables:
  Real(Kind=jprb) :: emissfactor_h,emissfactor_v
  Integer(Kind=jpim) :: chan,i,j,ich,ich2,pol_id
  Type(geometry_Type), Pointer    :: geom

  !- End of header ------------------------------------------------------

  ich2=1
  Do i=1,nfrequencies
     chan        = channels(i)
     pol_id      = 0
     pol_id = coeffs % fastem_polar(chan) + 1
     ich = polarisations(i,1)
     If (pol_id >= 4) then
        ! Return all calculated polarisations (or just computed TB for IR channels)
        Do j=1,polarisations(i,3)
           rad_ad%bt(ich+j-1)       = rad_ad%bt(ich+j-1) + rad_ad%out(ich2+j-1)
           rad_ad%bt_clear(ich+j-1) = rad_ad%bt_clear(ich+j-1) + rad_ad%out_clear(ich2+j-1)
           rad_ad%total(ich+j-1)    = rad_ad%total(ich+j-1) + rad_ad%total_out(ich2+j-1)
           rad_ad%clear(ich+j-1)    = rad_ad%clear(ich+j-1) + rad_ad%clear_out(ich2+j-1)
        End Do
     Else If (pol_id == 1) then
        ! Return average of V and H polarisation
        rad_ad%bt(ich)         = 0.5_JPRB*rad_ad%out(ich2)
        rad_ad%bt(ich+1)       = 0.5_JPRB*rad_ad%out(ich2)
        rad_ad%bt_clear(ich)   = 0.5_JPRB*rad_ad%out_clear(ich2)
        rad_ad%bt_clear(ich+1) = 0.5_JPRB*rad_ad%out_clear(ich2)
        rad_ad%total(ich)      = 0.5_JPRB*rad_ad%total_out(ich2)
        rad_ad%total(ich+1)    = 0.5_JPRB*rad_ad%total_out(ich2)
        rad_ad%clear(ich)      = 0.5_JPRB*rad_ad%clear_out(ich2)
        rad_ad%clear(ich+1)    = 0.5_JPRB*rad_ad%clear_out(ich2)
     Else
        geom        => geometry( lprofiles(i) )
        emissfactor_v = pol_v(1,pol_id) + &
         & pol_v(2,pol_id)*geom%sinview_sq + &
         & pol_v(3,pol_id)*geom%cosview_sq
        emissfactor_h = pol_h(1,pol_id) + &
         & pol_h(2,pol_id)*geom%sinview_sq + &
         & pol_h(3,pol_id)*geom%cosview_sq
        rad_ad%bt(ich)         = rad_ad%bt(ich) + rad_ad%out(ich2)*emissfactor_v
        rad_ad%bt(ich+1)       = rad_ad%bt(ich+1) + rad_ad%out(ich2)*emissfactor_h
        rad_ad%bt_clear(ich)   = rad_ad%bt_clear(ich) + rad_ad%out_clear(ich2)*emissfactor_v
        rad_ad%bt_clear(ich+1) = rad_ad%bt_clear(ich+1) + rad_ad%out_clear(ich2)*emissfactor_h
        rad_ad%total(ich)         = rad_ad%total(ich) + rad_ad%total_out(ich2)*emissfactor_v
        rad_ad%total(ich+1)       = rad_ad%total(ich+1) + rad_ad%total_out(ich2)*emissfactor_h
        rad_ad%clear(ich)   = rad_ad%clear(ich) + rad_ad%clear_out(ich2)*emissfactor_v
        rad_ad%clear(ich+1) = rad_ad%clear(ich+1) + rad_ad%clear_out(ich2)*emissfactor_h
     End If
     ich2 = ich2 + npolar_return(pol_id)
  End Do

End Subroutine rttov_calcpolarisation_ad
