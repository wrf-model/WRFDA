Subroutine rttov_integrate_k( &
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
     & emissivity_k,     &! inout
     & reflectivity,     &! in
     & reflectivity_k,   &! inout
     & transmission,     &! in
     & transmission_k,   &! inout
     & profiles,         &! in
     & profiles_k,       &! inout
     & aux_prof,         &! in
     & aux_prof_k,       &! inout
     & coef,             &! in
     & rad ,             &! in
     & auxrad ,          &! in
     & rad_k            ) ! inout
  !
  ! Description:
  ! To perform K integration of radiative transfer equation
  ! in rttov suite, calculating radiances and brightness temperature.
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
  ! Eyre J.R. 1991 A fast radiative transfer model for satellite sounding
  ! systems.  ECMWF Research Dept. Tech. Memo. 176 (available from the
  ! librarian at ECMWF).
  !
  ! Saunders R.W., M. Matricardi and P. Brunel 1999 An Improved Fast Radiative
  ! Transfer Model for Assimilation of Satellite Radiance Observations.
  ! QJRMS, 125, 1407-1425.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !          25/06/91.    Original code.  J.R.EYRE    *ECMWF*
  !          21/08/00.    Emissivity and reflectivity handled separately. Steve English
  !          31/01/01.    More cloud computations. F. Chevallier
  !          23/03/01     New coef. format, new channel numbers (P. Brunel)
  !          31/01/01.    More cloud computations. F. Chevallier
  !          28/09/01     Cosmic background temp added G.Deblonde
  !          18/01/2002   Thread safe (D.Salmond)
  !  1.0     01/12/2002   New F90 code with structures (P Brunel A Smith)
  !  1.1     02/01/2003   Added comments (R Saunders)
  !  1.2     06/05/2003   Init rad%downcld to 0 in section 1 (P  Brunel)
  !  1.3     26/09/2003   Modified to allow for multiple polarisations(S English)
  !  1.4     06/09/2004   Mods. for Vectorisation (D Salmond ECMWF & B Carruthers, Cray)
  !  1.5     29/03/2005   Add end of header comment (J. Cameron)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:

  ! Adjoint Variables
  ! transmission_k % tau_surf        |
  ! transmission_k % tau_layer       |
  ! emissivity_k                     |
  ! reflectivity_k                   | initialised before calling
  ! profiles_k                       |
  ! aux_prof_k                       |

  Use rttov_const, only :   &
       & sensor_id_mw

  Use rttov_types, Only : &
       & geometry_Type  ,&
       & rttov_coef     ,&
       & profile_Type   ,&
       & profile_aux    ,&
       & transmission_Type  ,&
       & radiance_Type  ,&
       & radiance_aux



  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_calcbt_ad.interface"
#include "rttov_calcrad_k.interface"
#include "rttov_calcpolarisation_ad.interface"

  !subroutine arguments:
  Logical,                 Intent(in)    :: addcloud
  Logical,                 Intent(in)    :: addcosmic
  Logical,                 Intent(in)    :: switchrad  ! true if input is BT
  Integer(Kind=jpim),      Intent(in)    :: nbtout          ! Number of BTs returned
  Integer(Kind=jpim),      Intent(in)    :: nfrequencies
  Integer(Kind=jpim),      Intent(in)    :: nchannels
  Integer(Kind=jpim),      Intent(in)    :: nprofiles
  Integer(Kind=jpim),      Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),      Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),      Intent(in)    :: lprofiles(nfrequencies)
  Real(Kind=jprb),         Intent(in)    :: emissivity(nchannels)
  Real(Kind=jprb),         Intent(in)    :: reflectivity(nchannels)
  Type(geometry_Type),     Intent(in)    :: angles(nprofiles)  ! geometry angles
  Type(rttov_coef),        Intent(in)    :: coef
  Type(profile_Type),      Intent(in) ,Target   :: profiles(nprofiles)
  Type(profile_aux) ,      Intent(in) ,Target   :: aux_prof(nprofiles)
  Type(transmission_Type), Intent(in):: transmission
  Type(radiance_Type),     Intent(in)    :: rad
  Type(radiance_aux),      Intent(in)    :: auxrad
  Real(Kind=jprb),         Intent(inout) :: emissivity_k(nchannels)
  Real(Kind=jprb),         Intent(inout) :: reflectivity_k(nchannels)
  Type(profile_Type),      Intent(inout) ,Target   :: profiles_k(nchannels)
  Type(profile_aux) ,      Intent(inout) ,Target   :: aux_prof_k(nchannels)
  Type(transmission_Type), Intent(inout) :: transmission_k
  Type(radiance_Type),     Intent(inout) :: rad_k

  !local constants:
  Real(Kind=jprb), Parameter :: min_tau = 1.0e-8_JPRB

  !local variables:
  Real(Kind=jprb) :: cfraction(nchannels)
  Real(Kind=jprb) :: cfraction_k(nchannels)

  Real(Kind=jprb) :: rad_tmp
  Real(Kind=jprb) :: meanrad_up(nchannels)
  Real(Kind=jprb) :: meanrad_up_2d(nchannels, coef % nlevels)
  Real(Kind=jprb) :: meanrad_down(nchannels)

  Real(Kind=jprb) :: rad_layer_k(coef % nlevels,nchannels)
  Real(Kind=jprb) :: rad_surfair_k(nchannels)
  Real(Kind=jprb) :: rad_skin_k(nchannels)
  Real(Kind=jprb) :: rad_up_k(coef % nlevels, nchannels)
  Real(Kind=jprb) :: rad_down_k(coef % nlevels, nchannels)
  Real(Kind=jprb) :: rad_down_cloud_k(coef % nlevels, nchannels)
  Real(Kind=jprb) :: rad_tmp_k(nchannels)
  Real(Kind=jprb) :: rad_tmp_k_2d(nchannels, coef % nlevels)
  Real(Kind=jprb) :: meanrad_up_k(nchannels)
  Real(Kind=jprb) :: meanrad_up_k_2d(nchannels, coef % nlevels)
  Real(Kind=jprb) :: meanrad_down_k(nchannels)
  Real(Kind=jprb) :: tau_prod

  Integer(Kind=jpim) :: i,j,lev,freq,kpol

  Type(profile_aux),   Pointer   :: aux
  Type(profile_aux),   Pointer   :: aux_k

  !- End of header --------------------------------------------------------

  !----------------------------------------------------------------------------
  ! initialise radiance structure cloud flag
  rad_k % lcloud = addcloud


  !----------------------------
  !1. calculate layer radiances
  !----------------------------
  If (coef % id_sensor == sensor_id_mw) Then
     Call rttov_calcpolarisation_ad( &
      &   nfrequencies,       &! in
      &   nchannels,          &! in
      &   nbtout,             &! in
      &   profiles,           &! in
      &   nprofiles,          &! in
      &   angles,             &! in
      &   channels,           &! in
      &   polarisations,      &! in
      &   lprofiles,          &! in
      &   coef,               &! in
      &   rad_k   )            ! inout
  Else
     rad_k%bt       = rad_k%out
     rad_k%bt_clear = rad_k%out_clear
     rad_k%total    = rad_k%total_out
     rad_k%clear    = rad_k%clear_out
  End If

! if input K unit is temperature, convert it in radiance
! call AD routine because all arguments size are nchannels
  if ( switchrad )  then
     Call rttov_calcbt_ad( &
          & nfrequencies,  &! in
          & nchannels,     &! in
          & channels,      &! in
          & polarisations, &! in
          & coef,          &! in
          & rad,           &! in
          & rad_k     )     ! inout   output is only rad_k%total
  endif



  ! initialisation of local variables
  rad_layer_k(:,:) = 0._JPRB
  rad_surfair_k(:) = 0._JPRB
  rad_skin_k(:)    = 0._JPRB
  rad_up_k(:,:)         = 0._JPRB
  rad_down_k(:,:)       = 0._JPRB
  rad_down_cloud_k(:,:) = 0._JPRB

  Do i = 1, nchannels
     freq=polarisations(i,2)
     cfraction(i) = aux_prof( lprofiles(freq) )%cfraction
  End Do




  !---------------------------
  !6. calculate total radiance
  !---------------------------


  rad_k%clear(:)  = (1 - cfraction(:)) * rad_k%total(:)
  rad_k%cloudy(:) = cfraction(:) * rad_k%total(:)
  cfraction_k(:)  = ( rad%cloudy(:) - rad%clear(:) ) * rad_k%total(:)

  Do i = 1, nchannels
     aux_prof_k( i )%cfraction =&
           & aux_prof_k( i )%cfraction + cfraction_k(i)
  End Do

  Do i = 1, nchannels
     freq=polarisations(i,2)
     aux     => aux_prof(  lprofiles(freq) )
     aux_k   => aux_prof_k( i )

     !--------------------------------------------
     !5.3 Interpolate to given cloud-top pressures
     !--------------------------------------------

     lev = aux % nearestlev_ctp
     rad_k%overcast(lev,i)   = rad_k%overcast(lev,i) +&
                               & (1 - aux % pfraction_ctp) * rad_k%cloudy(i)

     rad_k%overcast(lev-1,i) = rad_k%overcast(lev-1,i) +&
                               & aux % pfraction_ctp * rad_k%cloudy(i)

     aux_k % pfraction_ctp = aux_k % pfraction_ctp +&
               & (rad%overcast(lev-1,i)  - rad%overcast(lev,i)) * rad_k%cloudy(i)

     !rad_k%cloudy(i) is no later used rad_tl % cloudy was an output only
     rad_k%cloudy(i) = 0._JPRB

     !----------------------------------------
     !5. calculate cloudy (overcast) radiances
     !----------------------------------------
     !-----------------
     !5.2 Downward part
     !-----------------

     !(takes reflection and upward clear-sky transmission into account)
     ! surface level
     lev = aux % nearestlev_surf                  ! cor 28/11
     rad_up_k(lev,i) = rad_up_k(lev,i) + rad_k % overcast(lev,i)
     transmission_k % tau_surf(i)=transmission_k % tau_surf(i)+rad_k % overcast(lev,i)*auxrad%skin(i)
     rad_skin_k(i)   = rad_skin_k(i)   + rad_k % overcast(lev,i) * transmission % tau_surf(i)
     rad_k % overcast(lev,i) = 0._JPRB
  End Do

  If ( addcloud ) Then
     Do i = 1, nchannels
        freq=polarisations(i,2)
        aux     => aux_prof(    lprofiles(freq) )

        !(takes reflection and upward clear-sky transmission into account)
        ! surface level
        lev = aux % nearestlev_surf

        Do j = lev-1, 1 ,-1
           If ( transmission % tau_layer(j,i) > min_tau ) Then
               transmission_k% tau_surf(i) = transmission_k% tau_surf(i) + rad_k % downcld(j,i) * &
                     & ( auxrad%down_cloud(j,i) * reflectivity(i) + &
                       & 2 * auxrad%layer(j,i) * transmission % tau_surf(i) *&
                       & reflectivity(i) / transmission % tau_layer(j,i) )

              reflectivity_k(i) =  reflectivity_k(i) + rad_k % downcld(j,i) *&
                    & ( auxrad%down_cloud(j,i)    * transmission % tau_surf(i) +&
                      & auxrad%layer(j,i) * transmission % tau_surf(i) *&
                      & transmission % tau_surf(i) / transmission % tau_layer(j,i) )
           Endif
        End Do
!
!dir$ concurrent
        Do j = lev-1, 1 ,-1
           If ( transmission % tau_layer(j,i) > min_tau ) Then
              rad_down_cloud_k(j,i)  = rad_down_cloud_k(j,i) + rad_k % downcld(j,i) *&
                    & transmission % tau_surf(i) * reflectivity(i)

              rad_layer_k(j,i) = rad_layer_k(j,i) + rad_k % downcld(j,i) *&
                    & reflectivity(i) * transmission % tau_surf(i) * &
                    & transmission % tau_surf(i) / transmission % tau_layer(j,i)

              transmission_k% tau_layer(j,i) = transmission_k% tau_layer(j,i) -  rad_k % downcld(j,i) *&
                      & auxrad%layer(j,i) * transmission % tau_surf(i) * transmission % tau_surf(i) *  reflectivity(i) /&
                    & (transmission % tau_layer(j,i) * transmission % tau_layer(j,i))

              rad_k % downcld(j,i) = 0._JPRB
           Endif
        End Do
     End Do
   End If
  Do i = 1, nchannels
     freq=polarisations(i,2)
     aux     => aux_prof(    lprofiles(freq) )
     lev = aux % nearestlev_surf
     Do j = lev-1, 1 ,-1
       rad_up_k(lev,i) = rad_up_k(lev,i)    + rad_k % overcast(lev,i)
       transmission_k % tau_surf(i)   = transmission_k % tau_surf(i)   + rad_k % overcast(lev,i) * auxrad%skin(i)
       rad_skin_k(i)   = rad_skin_k(i)   + rad_k % overcast(lev,i) * transmission % tau_surf(i)
       rad_k % overcast(lev,i) = 0._JPRB
     End Do
  End Do

  !---------------
  !5.1 Upward part
  !---------------
  rad_up_k(:,:)    = rad_up_k(:,:)    + rad_k % overcast(:,:)
  rad_layer_k(:,:) = rad_layer_k(:,:) + rad_k % overcast(:,:) * transmission % tau_layer(:,:)
  transmission_k% tau_layer(:,:) = transmission_k% tau_layer(:,:) + rad_k % overcast(:,:) * auxrad%layer(:,:)
  rad_k % overcast(:,:) = 0._JPRB

  !--------------------------------
  !4. cosmic temperature correction
  !--------------------------------

  !calculate planck function corresponding to tcosmic=2.7k
  !deblonde tcosmic for microwave sensors only

  If ( addcosmic ) Then
     reflectivity_k(:) = reflectivity_k(:) + rad_k % clear(:) *&
            & auxrad%cosmic(:) * transmission % tau_surf(:) * transmission % tau_surf(:)
     transmission_k% tau_surf(:) = transmission_k% tau_surf(:) + rad_k % clear(:) *&
           & 2 * reflectivity(:) * auxrad%cosmic(:) * transmission % tau_surf(:)
  Endif

  !-----------------------
  !3. surface contribution
  !-----------------------
  emissivity_k(:)  = emissivity_k(:) + rad_k % clear(:) *&
        & auxrad%skin(:) * transmission % tau_surf(:)
  transmission_k% tau_surf(:)    = transmission_k% tau_surf(:)   + rad_k % clear(:) *&
        & auxrad%skin(:) * emissivity(:)
  rad_skin_k(:)    = rad_skin_k(:)   + rad_k % clear(:) *&
        & emissivity(:) * transmission % tau_surf(:)

  emissivity_k(:)  = emissivity_k(:) + rad_k % upclear(:) *&
        & auxrad%skin(:) * transmission % tau_surf(:)
  transmission_k% tau_surf(:)    = transmission_k% tau_surf(:)   + rad_k % upclear(:) *&
        & auxrad%skin(:) * emissivity(:)
  rad_skin_k(:)    = rad_skin_k(:)   + rad_k % upclear(:) *&
        & emissivity(:) * transmission % tau_surf(:)


  Do i = 1, nchannels
     freq=polarisations(i,2)
     aux     => aux_prof(  lprofiles(freq) )
     aux_k   => aux_prof_k( i )
     rad_tmp_k(i)        = 0._JPRB

     !2.3 near-surface layer
     !----------------------
     ! add upward and downward parts
     lev = aux % nearestlev_surf - 1
     If ( aux % pfraction_surf < 0.0_JPRB ) lev = lev + 1

     ! repeat direct code
     meanrad_up(i) = 0.5_JPRB * ( auxrad%surfair(i) + auxrad%layer(lev,i) ) * &
           & ( transmission % tau_layer(lev,i) - transmission % tau_surf(i) )

     If ( transmission % tau_surf(i) > min_tau ) Then
        rad_tmp = meanrad_up(i) / ( transmission % tau_layer(lev,i) * transmission % tau_surf(i) )
     Else
        rad_tmp    = 0.0_JPRB
     End If
     meanrad_down(i) = auxrad%down(lev,i) + rad_tmp
  end do

  If ( addcloud ) Then
     Do i = 1, nchannels
        freq=polarisations(i,2)
        aux     => aux_prof(    lprofiles(freq) )

        !2.3 near-surface layer
        !----------------------
        ! add upward and downward parts
        lev = aux % nearestlev_surf - 1
        If ( aux % pfraction_surf < 0.0_JPRB ) lev = lev + 1

     ! K
        Do j =lev, 1, -1
           ! in the direct model the value of auxrad%down_cloud(j) is later
           ! modified by
           ! auxrad%down_cloud(j) = (auxrad%down_cloud(j) + rad_tmp) * transmission % tau_surf(i)
           ! So to get the right trajectory one need to divide by transmission % tau_surf(i)
           transmission_k% tau_surf(i)         = transmission_k% tau_surf(i)  + rad_down_cloud_k(j,i) *&
                 & auxrad%down_cloud(j,i) / transmission % tau_surf(i)
           rad_tmp_k(i)  = rad_tmp_k(i) + rad_down_cloud_k(j,i) * transmission % tau_surf(i)

           rad_down_cloud_k(j,i) = rad_down_cloud_k(j,i) * transmission % tau_surf(i)
        End Do
       End Do
     End If

    Do i = 1, nchannels
     freq=polarisations(i,2)
     kpol= 1 + i - polarisations(freq,1)    ! Polarisation index
     aux     => aux_prof(    lprofiles(freq) )

     !2.3 near-surface layer
     !----------------------
     ! add upward and downward parts
     lev = aux % nearestlev_surf - 1
     If ( aux % pfraction_surf < 0.0_JPRB ) lev = lev + 1

     meanrad_up_k(i)     = 0._JPRB
     meanrad_down_k(i)   = 0._JPRB
     meanrad_up_k(i) = rad_up_k(aux % nearestlev_surf,i)
     rad_up_k(aux % nearestlev_surf,i) = 0._JPRB
     meanrad_down_k(i)  =   rad_k % reflclear(i) *&
           & transmission % tau_surf(i) *  transmission % tau_surf(i) * reflectivity(i)
     transmission_k% tau_surf(i)     = transmission_k% tau_surf(i)     + rad_k % reflclear(i) *&
           & 2._JPRB * transmission % tau_surf(i) * reflectivity(i) * meanrad_down(i)
     reflectivity_k(i) = reflectivity_k(i) + rad_k % reflclear(i) *&
           & transmission % tau_surf(i) * transmission % tau_surf(i) * meanrad_down(i)
     rad_k % reflclear(i) = 0._JPRB

     meanrad_up_k(i) = meanrad_up_k(i) + rad_k % upclear(i)
     rad_k % upclear(i) = 0._JPRB

     meanrad_up_k(i)      = meanrad_up_k(i)      + rad_k % clear(i)
     meanrad_down_k(i)    = meanrad_down_k(i)    + rad_k % clear(i) *&
           & transmission % tau_surf(i) * transmission % tau_surf(i) * reflectivity(i)
     transmission_k% tau_surf(i)     = transmission_k% tau_surf(i)     + rad_k % clear(i) *&
            & 2._JPRB * transmission % tau_surf(i) * reflectivity(i) * meanrad_down(i)
     reflectivity_k(i) = reflectivity_k(i) + rad_k % clear(i) *&
            & transmission % tau_surf(i) * transmission % tau_surf(i) * meanrad_down(i)
     rad_k % clear(i)  = 0._JPRB

     ! assume that there is no atmospheric source term for 3rd or 4th Stokes vector elements
     if (kpol >= 3) meanrad_up_k(i) = 0.0

     !meanrad_up_k(i)    = meanrad_up_k(i)
     rad_up_k(lev,i) = rad_up_k(lev,i) + meanrad_up_k(i)

     rad_down_k(lev,i) = meanrad_down_k(i)
     rad_tmp_k(i)         = rad_tmp_k(i) + meanrad_down_k(i)
     meanrad_down_k(i)    = 0._JPRB

     ! on peut creer une variable intermediaire pour gagner
     ! du temps rad_tmp_k * meanrad_up / (tau_prod*tau_prod)
     If ( transmission % tau_surf(i) > min_tau ) Then
        tau_prod = transmission % tau_layer(lev,i) * transmission % tau_surf(i)
        meanrad_up_k(i) = meanrad_up_k(i) + rad_tmp_k(i) /&
              & tau_prod
        transmission_k% tau_layer(lev,i) = transmission_k% tau_layer(lev,i) - rad_tmp_k(i) *&
              & meanrad_up(i) *  transmission % tau_surf(i) / (tau_prod*tau_prod)
        transmission_k% tau_surf(i)      = transmission_k% tau_surf(i)      - rad_tmp_k(i) *&
              & meanrad_up(i) * transmission % tau_layer(lev,i) / (tau_prod*tau_prod)
        rad_tmp_k(i) = 0.0_JPRB
     Else
        rad_tmp_k(i) = 0.0_JPRB
     End If

     ! on peut creer une variable intermediaire pour gagner
     ! du temps 0.5 * meanrad_up_k * ( auxrad%surfair(i) + auxrad%layer(lev,i)
     ! rad_surfair_k(i) est certainement inutile car plus utilise
     rad_surfair_k(i)   = rad_surfair_k(i)   + 0.5_JPRB * meanrad_up_k(i) *&
            & ( transmission % tau_layer(lev,i) - transmission % tau_surf(i) )
     rad_layer_k(lev,i) = rad_layer_k(lev,i) + 0.5_JPRB * meanrad_up_k(i) *&
            & ( transmission % tau_layer(lev,i) - transmission % tau_surf(i) )
     transmission_k% tau_layer(lev,i) = transmission_k% tau_layer(lev,i) + 0.5_JPRB * meanrad_up_k(i) *&
            & ( auxrad%surfair(i) + auxrad%layer(lev,i) )
     transmission_k% tau_surf(i)      =  transmission_k% tau_surf(i)     - 0.5_JPRB * meanrad_up_k(i) * &
            & ( auxrad%surfair(i) + auxrad%layer(lev,i) )
     meanrad_up_k(i) = 0._JPRB
    End Do

    rad_tmp_k_2d(:,:) = 0._JPRB
    meanrad_up_k_2d(:,:) = 0._JPRB
    Do i = 1, nchannels
     freq=polarisations(i,2)
     aux     => aux_prof(    lprofiles(freq) )

     !2.2 layers between standard pressure levels
     !-------------------------------------------
     Do lev = coef % nlevels, 2, -1

        ! adjoint
        If ( addcloud .And. lev < aux % nearestlev_surf) Then
           Do j = lev-1, 1, -1
              rad_tmp_k_2d(i,lev) = rad_tmp_k_2d(i,lev) +  rad_down_cloud_k(j,i)
           End Do
        End If
     End do
   End do

  Do i = 1, nchannels
     freq=polarisations(i,2)
     aux     => aux_prof(    lprofiles(freq) )

     !2.2 layers between standard pressure levels
     !-------------------------------------------
     Do lev = coef % nlevels, 2, -1

        ! direct code
        meanrad_up_2d(i,lev) = 0.5_JPRB * ( auxrad%layer(lev,i)   + auxrad%layer(lev-1,i) ) * &
                & ( transmission % tau_layer(lev-1,i) - transmission % tau_layer(lev,i)   )

        rad_tmp_k_2d(i,lev)           = rad_tmp_k_2d(i,lev)           + rad_down_k(lev,i)
        rad_down_k(lev-1,i) = rad_down_k(lev-1,i) + rad_down_k(lev,i)

        If ( transmission % tau_layer(lev,i) > min_tau ) Then
           ! on peut creer une variable intermediaire pour gagner
           ! du temps rad_tmp_k * meanrad_up / (tau_prod*tau_prod)
           tau_prod = transmission % tau_layer(lev,i) * transmission % tau_layer(lev-1,i)
           meanrad_up_k_2d(i,lev)         = meanrad_up_k_2d(i,lev)         + rad_tmp_k_2d(i,lev) /&
                 & tau_prod
           transmission_k % tau_layer(lev,i)   = transmission_k % tau_layer(lev,i)   - rad_tmp_k_2d(i,lev) *&
                 & transmission % tau_layer(lev-1,i) * meanrad_up_2d(i,lev) / (tau_prod*tau_prod)
           transmission_k % tau_layer(lev-1,i) = transmission_k % tau_layer(lev-1,i) - rad_tmp_k_2d(i,lev) *&
                 & transmission % tau_layer(lev,i) * meanrad_up_2d(i,lev) / (tau_prod*tau_prod)
           rad_tmp_k_2d(i,lev) = 0._JPRB
        Else
           rad_tmp_k_2d(i,lev) = 0._JPRB
        End If

        meanrad_up_k_2d(i,lev)      = meanrad_up_k_2d(i,lev)      + rad_up_k(lev,i)
        rad_up_k(lev-1,i) = rad_up_k(lev-1,i) + rad_up_k(lev,i)
        rad_layer_k(lev,i)   = rad_layer_k(lev,i)   + meanrad_up_k_2d(i,lev) *&
              & 0.5_JPRB * ( transmission % tau_layer(lev-1,i) - transmission % tau_layer(lev,i) )
        rad_layer_k(lev-1,i) = rad_layer_k(lev-1,i) + meanrad_up_k_2d(i,lev) *&
              & 0.5_JPRB * ( transmission % tau_layer(lev-1,i) - transmission % tau_layer(lev,i) )
        transmission_k % tau_layer(lev-1,i) = transmission_k % tau_layer(lev-1,i) + meanrad_up_k_2d(i,lev) *&
              & 0.5_JPRB * ( auxrad%layer(lev,i) + auxrad%layer(lev-1,i) )
        transmission_k % tau_layer(lev,i)   = transmission_k % tau_layer(lev,i)  - meanrad_up_k_2d(i,lev) *&
              & 0.5_JPRB * ( auxrad%layer(lev,i) + auxrad%layer(lev-1,i) )

     End Do

  End Do

  !2.1 layer above top pressure level
  !----------------------------------
  !
  rad_up_k(1,:)    = rad_up_k(1,:)      + rad_down_k(1,:) /&
        & transmission % tau_layer(1,:)
  transmission_k% tau_layer(1,:) = transmission_k% tau_layer(1,:)   - rad_down_k(1,:) *&
        & auxrad%down(1,:) / transmission % tau_layer(1,:)
  rad_down_k(1,:)  = 0._JPRB

  rad_layer_k(1,:) = rad_layer_k(1,:) + rad_up_k(1,:) *&
        & ( 1.0_JPRB-transmission % tau_layer(1,:) )
  transmission_k% tau_layer(1,:) = transmission_k% tau_layer(1,:) - rad_up_k(1,:) *&
         & auxrad%layer(1,:)
  rad_up_k(1,:)    = 0._JPRB

  Call rttov_calcrad_k( &
     & nfrequencies,    &! in
     & nchannels,       &! in
     & nprofiles,       &! in
     & channels,        &! in
     & polarisations,   &! in
     & lprofiles,       &! in
     & profiles,        &! in
     & profiles_k,      &! inout
     & coef,            &! in
     & auxrad%skin,     &! in
     & auxrad%surfair,  &! in
     & auxrad%layer,    &! in
     & rad_skin_k,      &! in
     & rad_surfair_k,   &! in
     & rad_layer_k     ) ! in


End Subroutine rttov_integrate_k
