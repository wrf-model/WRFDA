!
Subroutine rttov_integrate( &
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
     & rad,           &! inout
     & auxrad        ) ! inout
  ! Description:
  ! To perform integration of radiative transfer equation
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
  !  1.3     26/09/2003   Modified to allow for multiple polarisations (S English)
  !          03/09/2004   Mods. for Vectorisation (D Salmond ECMWF & BCarruthers, Cray)
  !          28/02/2005   Further mods to vectorisation (D Dent)
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
       & sensor_id_mw

  Use rttov_types, Only : &
       & rttov_coef     ,&
       & geometry_Type  ,&
       & profile_Type   ,&
       & profile_aux    ,&
       & transmission_Type  ,&
       & radiance_Type  ,&
       & radiance_aux

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_calcbt.interface"
#include "rttov_calcrad.interface"
#include "rttov_calcpolarisation.interface"

  !subroutine arguments:
  Logical,             Intent(in)    :: addcloud    ! switch for cloud computations
  Logical,             Intent(in)    :: addcosmic   ! switch for adding cosmic background
  Integer(Kind=jpim),  Intent(in)    :: nbtout          ! Number of BTs returned
  Integer(Kind=jpim),  Intent(in)    :: nfrequencies            ! Number of frequencies
  Integer(Kind=jpim),  Intent(in)    :: nchannels               ! Number of output radiances
  Integer(Kind=jpim),  Intent(in)    :: nprofiles               ! Number of profiles
  Integer(Kind=jpim),  Intent(in)    :: channels(nfrequencies)     ! Channel indices
  Integer(Kind=jpim),  Intent(in)    :: polarisations(nchannels,3) ! Channel indices
  Integer(Kind=jpim),  Intent(in)    :: lprofiles(nfrequencies)    ! Profiles indices
  Real(Kind=jprb),     Intent(in)    :: emissivity(nchannels)   ! surface emmissivity
  Real(Kind=jprb),     Intent(in)    :: reflectivity(nchannels) ! surface reflectivity
  Type(geometry_Type), Intent(in)    :: angles(nprofiles)  ! geometry angles
  Type(rttov_coef),    Intent(in)    :: coef                    ! Coefficients
  Type(profile_Type),  Intent(in) ,Target   :: profiles(nprofiles) ! Profiles
  Type(profile_aux) ,  Intent(in) ,Target   :: aux_prof(nprofiles) ! auxillary profiles info.
  Type(transmission_Type), Intent(in):: transmission               ! transmittances and single-layer od
  Type(radiance_Type), Intent(inout) :: rad    ! radiances (mw/cm-1/ster/sq.m) and BTs
  Type(radiance_aux),  Intent(inout) :: auxrad ! auxillary radiances
  !

  !local constants:
  Real(Kind=jprb), Parameter :: min_tau = 1.0e-8_JPRB

  !local variables:
  Real(Kind=jprb) :: rad_tmp(nchannels, coef % nlevels)
  Real(Kind=jprb) :: meanrad_up
  Real(Kind=jprb) :: meanrad_down
  Integer(Kind=jpim) :: i,j,lev,freq,kpol

  Real(Kind=jprb) :: cfraction(nchannels)        ! cloud fraction
  Real(Kind=jprb) :: pfraction(nchannels)        !
  Real(Kind=jprb) :: rad_down_cloud(coef % nlevels, nchannels)  ! layer downwelling radiance

  Type(profile_aux),   Pointer   :: aux ! pointer on auxillary radiances

integer(Kind=jpim) :: iv1(nchannels), iv2(nchannels), iv3(nchannels)


  !- End of header ------------------------------------------------------
  ! initialise radiance structure cloud flag

  rad % lcloud = addcloud

  !----------------------------
  !1. calculate layer radiances
  !----------------------------

  Call rttov_calcrad( &
       & addcosmic,    &!in
       & nchannels,    &!in
       & nfrequencies, &!in
       & nprofiles,    &!in
       & channels,     &!in
       & polarisations,&!in
       & lprofiles,    &!in
       & profiles,     &!in
       & coef,         &!in
       & auxrad%cosmic,   &!out
       & auxrad%skin,     &!out
       & auxrad%surfair,  &!out
       & auxrad%layer    ) !out


  If ( addcloud ) Then
     rad_down_cloud(:,:) = 0._JPRB
     rad % downcld(:,:)  = 0._JPRB
  Endif

  !2.1 layer above top pressure level
  !----------------------------------
  auxrad%up(1,:)   = auxrad%layer(1,:) * ( 1.0_JPRB-transmission % tau_layer(1,:) )
  auxrad%down(1,:) = auxrad%up(1,:) / transmission % tau_layer(1,:)

  !-------------------------------------
  !2. calculate atmospheric contribution
  !-------------------------------------

     !2.2 layers between standard pressure levels
     !-------------------------------------------

!dir$ concurrent
  Do lev = 2, coef % nlevels

!dir$ concurrent
     Do i = 1, nchannels

        meanrad_up = 0.5_JPRB * ( auxrad%layer(lev,i)+auxrad%layer(lev-1,i) ) * &
             & ( transmission % tau_layer(lev-1,i) - transmission % tau_layer(lev,i) )
        auxrad%up(lev,i) = auxrad%up(lev-1,i) + meanrad_up

        If ( transmission % tau_layer(lev,i) > min_tau ) Then
           rad_tmp(i, lev) = meanrad_up / ( transmission % tau_layer(lev,i) * transmission % tau_layer(lev-1,i) )
        Else
           rad_tmp(i, lev) = 0.0_JPRB
        End If
        auxrad%down(lev,i) = auxrad%down(lev-1,i) + rad_tmp(i, lev)

     end do

  end do


!dir$ concurrent
  Do i = 1, nchannels

     freq=polarisations(i,2)
     aux  => aux_prof( lprofiles(freq) )
     cfraction(i) = aux%cfraction
     pfraction(i) = aux%pfraction_surf
     iv3(i) = aux % nearestlev_surf - 1
     iv2(i) = aux % nearestlev_surf
     iv1(i) = min( coef % nlevels, iv3(i) )
     If ( pfraction(i) < 0.0_JPRB ) iv3(i) = iv3(i) + 1
  End Do

  if( addcloud )then
    Do i = 1, nchannels
       Do lev = 2, iv1(i)

          Do j = 1, lev-1
             rad_down_cloud(j,i) = rad_down_cloud(j,i) + rad_tmp(i,lev)
          End Do
       End Do
    End Do
  end if


!dir$ concurrent
  Do i = 1, nchannels

     freq=polarisations(i,2)
     kpol= 1 + i - polarisations(freq,1)    ! Polarisation index

     !2.3 near-surface layer
     !----------------------
     ! add upward and downward parts

     lev = iv3(i)

     meanrad_up = 0.5_JPRB * ( auxrad%surfair(i) + auxrad%layer(lev,i) ) * &
          & ( transmission % tau_layer(lev,i) - transmission % tau_surf(i) )
     If ( transmission % tau_surf(i) > min_tau ) Then
        rad_tmp(i, lev) = meanrad_up / ( transmission % tau_layer(lev,i) * transmission % tau_surf(i) )
     Else
        rad_tmp(i, lev) = 0.0_JPRB
     End If
     meanrad_down = auxrad%down(lev,i) + rad_tmp(i, lev)
     meanrad_up = auxrad%up(lev,i) + meanrad_up
     ! assume that there is no atmospheric source term for 3rd or 4th Stokes vector elements
     if (kpol >= 3) meanrad_up = 0.0
     rad % clear(i) = meanrad_up + &
          & meanrad_down * reflectivity(i) * transmission % tau_surf(i) * transmission % tau_surf(i)

     ! clear sky radiance without reflection term
     ! without surface contribution at this line
     rad % upclear(i) = meanrad_up

     ! clear sky downwelling radiance
     rad % dnclear(i) = &
          & meanrad_down * transmission % tau_surf(i)

     ! reflected clear sky downwelling radiance
     rad % reflclear(i) = &
          & rad % dnclear(i) * ( reflectivity(i) ) * transmission % tau_surf(i)

     auxrad%up(iv2(i),i) = meanrad_up

  End Do

  If ( addcloud ) Then

     Do i = 1, nchannels

        lev = iv3(i)

        Do j = 1, lev
           rad_down_cloud(j,i) = (rad_down_cloud(j,i) + rad_tmp(i, lev)) * transmission % tau_surf(i)
        End Do

     End Do

  End If
  

  !-----------------------
  !3. surface contribution
  !-----------------------

  rad % clear(:) = rad % clear(:) +&
        & emissivity(:) * transmission % tau_surf(:) * auxrad%skin(:)

  ! clear sky radiance without reflection term
  rad % upclear(:) = rad % upclear(:) +&
        & emissivity(:) * transmission % tau_surf(:) * auxrad%skin(:)

  !--------------------------------
  !4. cosmic temperature correction
  !--------------------------------

  !calculate planck function corresponding to tcosmic=2.7k
  !deblonde tcosmic for microwave sensors only

  If ( addcosmic ) Then
     rad % clear(:) = rad % clear(:) + &
          & reflectivity(:) * auxrad%cosmic(:) * transmission % tau_surf(:) * transmission % tau_surf(:)
  Endif


  !----------------------------------------
  !5. calculate cloudy (overcast) radiances
  !----------------------------------------

  !---------------
  !5.1 Upward part
  !---------------
  ! (levels, channels)
  ! overcast radiance at given cloud top
!dir$ concurrent
  rad % overcast(:,:) = auxrad%up(:,:) + auxrad%layer(:,:) * transmission % tau_layer(:,:)

!dir$ concurrent
  Do i = 1, nchannels

     freq=polarisations(i,2)
     aux  => aux_prof( lprofiles(freq) )

     lev = aux % nearestlev_surf
     rad % overcast(lev,i) = auxrad%up(lev,i) + transmission % tau_surf(i) * auxrad%skin(i)

     !-----------------
     !5.2 Downward part
     !-----------------

     !(takes reflection and upward clear-sky transmission into account)

     If ( addcloud ) Then
!dir$ concurrent
        Do j = 1,lev-1
           If ( transmission % tau_layer(j,i) > min_tau ) &
                !contribution to radiance of downward
                ! cloud emission at given cloud top
           & rad % downcld(j,i) = &
                & ( rad_down_cloud(j,i) + &
                  & auxrad%layer(j,i) * transmission % tau_surf(i)/transmission % tau_layer(j,i) ) &
                & * transmission % tau_surf(i) * (reflectivity(i))
        End Do
        ! No specific action for transmission % tau_layer(j,i) <= min_tau or
        ! levels between aux % nearestlev_surf and nlevels
        ! because the array rad % downcld has already
        ! been init. to 0 in section 1
     End If

     !--------------------------------------------
     !5.3 Interpolate to given cloud-top pressures
     !--------------------------------------------

     lev = aux % nearestlev_ctp
     rad%cloudy(i) = rad % overcast(lev,i) * (1.0_JPRB-aux % pfraction_ctp) + &
          & rad % overcast(lev-1,i) * (aux % pfraction_ctp)

  End Do

  !---------------------------
  !6. calculate total radiance
  !---------------------------

!dir$ concurrent
  rad % total(:) = rad%clear(:) + cfraction(:) * ( rad%cloudy(:) - rad%clear(:) )
  If ( addcloud ) Then
     auxrad%down_cloud(:,:) = rad_down_cloud(:,:)
  End If

  !-----------------------------------------------
  !7. convert radiances to brightness temperatures
  !-----------------------------------------------

  Call rttov_calcbt( &
       & nfrequencies,  &! in
       & nchannels,  &! in
       & channels,   &! in
       & polarisations, &! in
       & coef,       &! in
       & rad        ) ! inout
 
  !-----------------------------------------------------------
  !8. convert brightness temperatures to required polarisation
  !-----------------------------------------------------------

  If (coef % id_sensor == sensor_id_mw) Then
     Call rttov_calcpolarisation( &
        & nfrequencies,       &! in
        & nchannels,          &! in
        & nprofiles,     &! in
        & angles,      &! in
        & channels,      &! in
        & polarisations,      &! in
        & lprofiles,     &! in
        & coef,          &! in
        & rad        )      ! inout
  Else
  rad%out       = rad%bt
        rad%out_clear = rad%bt_clear
  rad%total_out = rad%total
        rad%clear_out = rad%clear
  End If

End Subroutine rttov_integrate
