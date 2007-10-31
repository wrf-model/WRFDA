Subroutine rttov_integrate_tl( &
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
     & rad ,             &! in
     & auxrad ,          &! in
     & rad_tl           ) ! inout 
  !
  ! Description:
  ! To perform TL of integration of radiative transfer equation
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
  !          28/02/2005   Improved vectorisation (D Dent)
  !          29/03/2005   Add end of header comment (J. Cameron)
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
       & geometry_Type  ,&
       & rttov_coef     ,&
       & profile_Type   ,&
       & profile_aux    ,&
       & transmission_Type  ,&
       & radiance_Type  ,&
       & radiance_aux 


  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_calcbt_tl.interface"
#include "rttov_calcrad_tl.interface"
#include "rttov_calcpolarisation_tl.interface"

  !subroutine arguments:
  Logical,             Intent(in)    :: addcloud
  Logical,             Intent(in)    :: addcosmic
  Integer(Kind=jpim),  Intent(in)    :: nfrequencies
  Integer(Kind=jpim),  Intent(in)    :: nchannels
  Integer(Kind=jpim),  Intent(in)    :: nbtout          ! Number of BTs returned 
  Integer(Kind=jpim),  Intent(in)    :: nprofiles
  Integer(Kind=jpim),  Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),  Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),  Intent(in)    :: lprofiles(nfrequencies)
  Real(Kind=jprb),     Intent(in)    :: emissivity(nchannels)
  Real(Kind=jprb),     Intent(in)    :: reflectivity(nchannels)
  Type(geometry_Type), Intent(in)    :: angles(nprofiles)  ! geometry angles
  Type(rttov_coef),    Intent(in)    :: coef
  Type(profile_Type),  Intent(in) ,Target   :: profiles(nprofiles)
  Type(profile_aux) ,  Intent(in) ,Target   :: aux_prof(nprofiles)
  Type(transmission_Type), Intent(in):: transmission
  Type(radiance_Type), Intent(in)    :: rad
  Type(radiance_aux),  Intent(in)    :: auxrad

  Real(Kind=jprb),                Intent(in)    :: emissivity_tl(nchannels)
  Real(Kind=jprb),                Intent(in)    :: reflectivity_tl(nchannels)
  Type(profile_Type),  Intent(in) ,Target   :: profiles_tl(nprofiles)
  Type(profile_aux) ,  Intent(in) ,Target   :: aux_prof_tl(nprofiles)
  Type(transmission_Type), Intent(in):: transmission_tl
  Type(radiance_Type), Intent(inout)   :: rad_tl ! in because of mem allocation





  !local constants:
  Real(Kind=jprb), Parameter :: min_tau = 1.0e-8_JPRB



  !local variables:
  Real(Kind=jprb) :: rad_tmp
  Real(Kind=jprb) :: meanrad_up
  Real(Kind=jprb) :: meanrad_down

  Real(Kind=jprb) :: rad_tmp_tl
  Real(Kind=jprb) :: meanrad_up_tl
  Real(Kind=jprb) :: meanrad_down_tl
  Real(Kind=jprb) :: tau_prod

  Real(Kind=jprb) :: cfraction(nchannels)
  Real(Kind=jprb) :: cfraction_tl(nchannels)

  ! rad_down_cloud overwrite auxrad % down_cloud
  ! because it is in input/output of several code lines
  Real(Kind=jprb) :: rad_down_cloud(coef % nlevels, nchannels)

  Real(Kind=jprb) :: rad_layer_tl(coef % nlevels, nchannels)
  Real(Kind=jprb) :: rad_surfair_tl(nchannels)
  Real(Kind=jprb) :: rad_skin_tl(nchannels)
  Real(Kind=jprb) :: rad_up_tl(coef % nlevels, nchannels)
  Real(Kind=jprb) :: rad_down_tl(coef % nlevels, nchannels)
  Real(Kind=jprb) :: rad_down_cloud_tl(coef % nlevels, nchannels)

  Integer(Kind=jpim) :: i,j,lev,freq,kpol

  Type(profile_aux),   Pointer   :: aux
  Type(profile_aux),   Pointer   :: aux_tl

  real(Kind=jprb)    :: v1(nchannels,coef%nlevels,2), v2(nchannels), p1, p2
  real(Kind=jprb)    :: pfraction(nchannels)
  integer(Kind=jpim) :: iv1(nchannels), iv2(nchannels), iv3(nchannels)

  !- End of header --------------------------------------------------------

!cdir nooverlap(aux,aux_tl)

  !-------------------------------------------------------------------------------
  ! initialise radiance structure cloud flag
  rad_tl % lcloud = addcloud


  !----------------------------
  !1. calculate layer radiances
  !----------------------------

  Call rttov_calcrad_tl( &
       & nfrequencies, &! in
       & nchannels,    &! in
       & nprofiles,    &! in
       & channels,     &! in
       & polarisations, &! in
       & lprofiles,    &! in
       & profiles,     &! in
       & profiles_tl,  &! in
       & coef,         &! in
       & auxrad%skin,     &! in
       & auxrad%surfair,  &! in
       & auxrad%layer,    &! in
       & rad_skin_tl,     &! out
       & rad_surfair_tl,  &! out
       & rad_layer_tl    ) ! out 

  If ( addcloud ) Then
     rad_down_cloud(:,:) =0._JPRB
     rad_down_cloud_tl(:,:) = 0._JPRB
     rad_tl % downcld(:,:)  = 0._JPRB
  Endif

  !2.1 layer above top pressure level
  !----------------------------------
  !

  rad_up_tl(1,:)   = rad_layer_tl(1,:) * ( 1.0_JPRB-transmission % tau_layer(1,:) ) -&
                       & auxrad%layer(1,:) * transmission_tl % tau_layer(1,:)  
  rad_down_tl(1,:) = (rad_up_tl(1,:) - auxrad%down(1,:) * transmission_tl % tau_layer(1,:))  /&
                       & transmission % tau_layer(1,:) 

  !-------------------------------------
  !2. calculate atmospheric contribution
  !-------------------------------------
!cdir nodep
  Do i = 1, nchannels
     freq=polarisations(i,2)
     j   = lprofiles(freq)
     aux     => aux_prof( j )
     aux_tl  => aux_prof_tl( j )
     cfraction(i)       = aux%cfraction
     cfraction_tl(i)    = aux_tl%cfraction
     pfraction(i)       = aux%pfraction_surf
     iv2(i)             = aux%nearestlev_surf
     iv1(i)             = min( coef % nlevels, iv2(i) - 1 )
     iv3(i) = iv2(i) - 1
     If ( pfraction(i) < 0.0_JPRB ) iv3(i) = iv3(i) + 1
  End Do

     !2.2 layers between standard pressure levels
     !-------------------------------------------

  Do i = 1, nchannels
     Do lev = 2, coef % nlevels
        meanrad_up = 0.5_JPRB * ( auxrad%layer(lev,i)   + auxrad%layer(lev-1,i) ) * &
                & ( transmission % tau_layer(lev-1,i) - transmission % tau_layer(lev,i)   ) 

        meanrad_up_tl = 0.5_JPRB *((( auxrad%layer(lev,i)      + auxrad%layer(lev-1,i)  )  * &
               & ( transmission_tl % tau_layer(lev-1,i) - transmission_tl % tau_layer(lev,i) )) + &
              & (( rad_layer_tl(lev,i)   + rad_layer_tl(lev-1,i)) * &
               & ( transmission % tau_layer(lev-1,i)    - transmission % tau_layer(lev,i)    )))  
        rad_up_tl(lev,i) = rad_up_tl(lev-1,i) + meanrad_up_tl

        If ( transmission % tau_layer(lev,i) > min_tau ) Then
           tau_prod = transmission % tau_layer(lev,i) * transmission % tau_layer(lev-1,i) 
           v1(i,lev,1) = meanrad_up / tau_prod
           v1(i,lev,2) = ( meanrad_up_tl  -                                      &
                 & meanrad_up * ( transmission_tl % tau_layer(lev,i) * transmission % tau_layer(lev-1,i)     + &
                                & transmission % tau_layer(lev,i)    * transmission_tl % tau_layer(lev-1,i)) / &
                 & tau_prod ) / tau_prod 
        Else
           v1(i,lev,1) = 0.0_JPRB
           v1(i,lev,2) = 0.0_JPRB
        End If
        rad_down_tl(lev,i) = rad_down_tl(lev-1,i) + v1(i,lev,2)

     End Do
  End Do


  Do i = 1, nchannels
     Do lev = 2, coef % nlevels
        If ( addcloud .And. lev < iv2(i) ) Then
           Do j = 1, lev-1
              rad_down_cloud(j,i)    = rad_down_cloud(j,i)    + v1(i,lev,1)
              rad_down_cloud_tl(j,i) = rad_down_cloud_tl(j,i) + v1(i,lev,2)
           End Do
        End If
     End Do
  End Do


     !2.3 near-surface layer
     !----------------------
     ! add upward and downward parts

!dir$ concurrent
  Do i = 1, nchannels

     lev = iv3(i)
     freq       = polarisations(i,2)               ! Frequency index
     kpol= 1 + i - polarisations(freq,1)    ! Polarisation index

     meanrad_up = 0.5_JPRB * ( auxrad%surfair(i) + auxrad%layer(lev,i) ) * &
           & ( transmission % tau_layer(lev,i) - transmission % tau_surf(i) ) 
     meanrad_up_tl = 0.5_JPRB *((( rad_surfair_tl(i)  + rad_layer_tl(lev,i) )  * &
            & ( transmission % tau_layer(lev,i)   - transmission % tau_surf(i)         )) + &
           & (( auxrad%surfair(i)     + auxrad%layer(lev,i)    )  * &
            & ( transmission_tl % tau_layer(lev,i)- transmission_tl % tau_surf(i)      ))) 

     If ( transmission % tau_surf(i) > min_tau ) Then
        tau_prod = transmission % tau_layer(lev,i) * transmission % tau_surf(i) 
        v1(i,lev,1) = meanrad_up / ( transmission % tau_layer(lev,i) * transmission % tau_surf(i) )
        v1(i,lev,2) = ( meanrad_up_tl  -                               &
              & meanrad_up * ( transmission_tl % tau_layer(lev,i) * transmission % tau_surf(i)     + &
                             & transmission % tau_layer(lev,i)    * transmission_tl % tau_surf(i)) / &
              & tau_prod ) / tau_prod 
     Else
        v1(i,lev,1)    = 0.0_JPRB
        v1(i,lev,2) = 0.0_JPRB
     End If

     meanrad_down = auxrad%down(lev,i) + v1(i,lev,1)
     meanrad_down_tl = rad_down_tl(lev,i) + v1(i,lev,2)

     meanrad_up_tl = rad_up_tl(lev,i) + meanrad_up_tl

     ! assume that there is no atmospheric source term for 3rd or 4th Stokes vector elements
     if (kpol >= 3) meanrad_up_tl = 0.0_JPRB

     rad_tl % clear(i) =&
           & meanrad_up_tl +                               &
           & transmission % tau_surf(i) * ( reflectivity(i)     *         &
              & ( meanrad_down_tl * transmission % tau_surf(i)  +         &
                & 2._JPRB * transmission_tl % tau_surf(i) * meanrad_down ) +   &
              & reflectivity_tl(i) * meanrad_down *  transmission % tau_surf(i)) 

     rad_tl % upclear(i) = meanrad_up_tl

     rad_tl % reflclear(i) =  transmission % tau_surf(i) *         &
             & ( reflectivity(i) * ( meanrad_down_tl * transmission % tau_surf(i) +&
                           & 2.0_JPRB * transmission_tl % tau_surf(i) * meanrad_down ) +&
              & reflectivity_tl(i) * meanrad_down *  transmission % tau_surf(i)) 


     rad_up_tl(iv2(i),i) = meanrad_up_tl
  End Do

  If ( addcloud ) Then
     Do i = 1, nchannels
        lev = iv3(i)
        Do j = 1, lev
           rad_down_cloud_tl(j,i) =&
                 & (rad_down_cloud_tl(j,i) + v1(i,lev,2)) * transmission % tau_surf(i) +&
                 & (rad_down_cloud(j,i)    + v1(i,lev,1)) * transmission_tl % tau_surf(i) 
           rad_down_cloud(j,i) = (rad_down_cloud(j,i) + v1(i,lev,1)) * transmission % tau_surf(i)
        End Do
     End Do
  End If

  !-----------------------
  !3. surface contribution
  !-----------------------

  rad_tl % clear(:) = &
         & rad_tl % clear(:) +&
         & auxrad%skin(:) * (emissivity_tl(:) * transmission % tau_surf(:)  +&
                        & emissivity(:) * transmission_tl % tau_surf(:)) +&
         & rad_skin_tl(:) * emissivity(:) * transmission % tau_surf(:) 


  rad_tl % upclear(:) =&
        & rad_tl % upclear(:) + &
         & auxrad%skin(:) * (emissivity_tl(:) * transmission % tau_surf(:)  +&
                        & emissivity(:) * transmission_tl % tau_surf(:)) +&
         & rad_skin_tl(:) * emissivity(:) * transmission % tau_surf(:) 
  
  
  !--------------------------------
  !4. cosmic temperature correction
  !--------------------------------
  
  !calculate planck function corresponding to tcosmic=2.7k
  !deblonde tcosmic for microwave sensors only

  If ( addcosmic ) Then
     rad_tl % clear(:) =&
           & rad_tl % clear(:) + &
           & reflectivity_tl(:) * auxrad%cosmic(:) * transmission % tau_surf(:) * transmission % tau_surf(:) + &
           & 2 * reflectivity(:) * auxrad%cosmic(:) * transmission_tl % tau_surf(:) * transmission % tau_surf(:) 
  Endif


  !----------------------------------------
  !5. calculate cloudy (overcast) radiances
  !----------------------------------------
  
  !---------------
  !5.1 Upward part
  !---------------

!dir$ concurrent
  rad_tl % overcast(:,:) = &
        & rad_up_tl(:,:) +&
        & rad_layer_tl(:,:) * transmission % tau_layer(:,:) +&
        & auxrad%layer(:,:)    * transmission_tl % tau_layer(:,:) 

!dir$ concurrent
  Do i = 1, nchannels

     lev = iv2(i)
     rad_tl % overcast(lev,i) =           &
           & rad_up_tl(lev,i)                 +&
           & transmission_tl % tau_surf(i) * auxrad%skin(i)   +&
           & transmission % tau_surf(i)    * rad_skin_tl(i) 

  End Do

     !-----------------
     !5.2 Downward part
     !-----------------

     !(takes reflection and upward clear-sky transmission into account)

  If ( addcloud ) Then
!dir$ concurrent
     Do i = 1, nchannels

        lev = iv2(i)
!dir$ concurrent
        Do j = 1,lev-1
           If ( transmission % tau_layer(j,i) > min_tau ) Then
              rad_tl % downcld(j,i) = &
                    & rad_down_cloud_tl(j,i) * transmission % tau_surf(i)    * reflectivity(i)     +&
                    & rad_down_cloud(j,i)    * transmission_tl % tau_surf(i) * reflectivity(i)     +&
                    & rad_down_cloud(j,i)    * transmission % tau_surf(i)    * reflectivity_tl(i)  +&
                    & rad_layer_tl(j,i) * transmission % tau_surf(i)    &
                            & * transmission % tau_surf(i)  * reflectivity(i)     &
                            & / transmission % tau_layer(j,i)                                      +&
                    & auxrad%layer(j,i)    * transmission_tl % tau_surf(i) &
                            & * transmission % tau_surf(i)  * reflectivity(i)     &
                            & / transmission % tau_layer(j,i)                                      +&
                    & auxrad%layer(j,i)    * transmission % tau_surf(i)    &
                            & * transmission % tau_surf(i)  * reflectivity_tl(i)  &
                            & / transmission % tau_layer(j,i)                                      +&
                    & auxrad%layer(j,i)  *&
                         & ( transmission_tl % tau_surf(i) * transmission % tau_layer(j,i) &
                           & - transmission % tau_surf(i) * transmission_tl % tau_layer(j,i)) &
                           & / (transmission % tau_layer(j,i) * transmission % tau_layer(j,i))  *&
                           & transmission % tau_surf(i) * reflectivity(i) 
           Endif
        End Do
        ! No specific action for transmission % tau_layer(j,i) <= min_tau or
        ! levels between aux % nearestlev_surf and nlevels
        ! because the array rad_tl % downcld has already
        ! been init. to 0 in section 1
     End Do
  End If

     !--------------------------------------------
     !5.3 Interpolate to given cloud-top pressures
     !--------------------------------------------

!dir$ concurrent
  Do i = 1, nchannels
     freq = polarisations(i,2)               ! Frequency index
     lev  = aux_prof(    lprofiles(freq) )% nearestlev_ctp
     p1   = aux_prof(    lprofiles(freq) )% pfraction_ctp
     p2   = aux_prof_tl( lprofiles(freq) )% pfraction_ctp

     rad_tl % cloudy(i) =&
            & rad_tl%overcast(lev,i)    +&
           & (rad_tl%overcast(lev-1,i) - rad_tl%overcast(lev,i)) * p1 +&
           & (rad%overcast(lev-1,i)    - rad%overcast(lev,i))    * p2 

  End Do


  !---------------------------
  !6. calculate total radiance
  !---------------------------

!dir$ concurrent
  rad_tl % total(:) = &
        & rad_tl % clear(:) +&
        & cfraction(:)    * ( rad_tl%cloudy(:) - rad_tl%clear(:) ) +&
        & cfraction_tl(:) * ( rad%cloudy(:)    - rad%clear(:) ) 



  !-----------------------------------------------
  !7. convert radiances to brightness temperatures
  !-----------------------------------------------

  Call rttov_calcbt_tl( &
       & nfrequencies,  &! in
       & nchannels,     &! in
       & channels,      &! in
       & polarisations, &! in
       & coef,          &! in
       & rad,           &! in
       & rad_tl     )    ! inout 

  If (coef % id_sensor == sensor_id_mw) Then
     Call rttov_calcpolarisation_tl( &
      &   nfrequencies,       &! in
      &   nchannels,          &! in
      &   nprofiles,          &! in
      &   angles,             &! in
      &   channels,           &! in
      &   polarisations,      &! in
      &   lprofiles,          &! in
      &   coef,               &! in
      &   rad_tl   )           ! inout 
  Else
     rad_tl%out       = rad_tl%bt
     rad_tl%out_clear = rad_tl%bt_clear
     rad_tl%total_out = rad_tl%total
     rad_tl%clear_out = rad_tl%clear
  End If

End Subroutine rttov_integrate_tl
