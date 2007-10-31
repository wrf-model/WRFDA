!
Subroutine rttov_iniscatt_tl (&
      & errorstatus,       &! out
      & nwp_levels,        &! in
      & nrt_levels,        &! in
      & nfrequencies,      &! in
      & nchannels,         &! in
      & nprofiles,         &! in
      & polarisations,     &! in
      & channels,          &! in
      & frequencies,       &! in
      & lprofiles,         &! in
      & lsprofiles,         &! in
      & profiles,          &! in  
      & profiles_tl,       &! in  
      & cld_profiles,      &! in 
      & cld_profiles_tl,   &! in 
      & coef_rttov,        &! in
      & coef_scatt,        &! in
      & transmission,      &! in
      & transmission_tl,   &! in
      & calcemiss,         &! in
      & angles,            &! out
      & scatt_aux,         &! inout
      & scatt_aux_tl)       ! inout 

  !
  ! Description:
  ! Calculates some variables related to the input precipitation profile
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
  !   1.0    09/2002      Initial version     (F. Chevallier)
  !   1.1    05/2003      RTTOV7.3 compatible (F. Chevallier)
  !   1.2    03/2004      Added polarimetry   (R. Saunders)
  !   1.3    08/2004      Polarimetry fixes   (U. O'Keeffe)
  !   1.4    11/2004      Clean-up            (P. Bauer)
  !   1.5    10/2005      Fixes for rttov8 indexing   (U. O'Keeffe)
  !   1.6    11/2005      Add errorstatus to arguments (J. Cameron)   
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !   Documenting Exchangeable Fortran 90 Code".
  !
  ! Declaratiochannelsns:
  ! Modules used:
  ! Imported Type Definitions:
  
  Use rttov_types, Only :     &
       & rttov_coef          ,&
       & rttov_scatt_coef    ,&
       & transmission_type   ,&
       & geometry_Type       ,&
       & profile_scatt_aux   ,&
       & profile_Type        ,&
       & profile_cloud_Type   

  Use rttov_const, Only:      &
       & errorstatus_success, &
       & errorstatus_fatal,   &
       & gravity,             &
       & pressure_top,        &
       & rgp,                 &
       & rm,                  &
       & rho_rain,            &
       & rho_snow,            &
       & ccthres 

  Use parkind1, Only : jpim     ,jprb

  Implicit None

#include "rttov_mieproc_tl.interface"
#include "rttov_iniedd_tl.interface"
#include "rttov_calcemis_mw.interface"
#include "rttov_calcemis_mw_tl.interface"
#include "rttov_setgeometry.interface"
#include "rttov_errorreport.interface"
#include "rttov_intex_tl.interface"

!* Subroutine arguments:

  Integer (Kind=jpim), Intent (in) :: nwp_levels                              ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nrt_levels                              ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nprofiles                               ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: nfrequencies                            ! Number of frequencies  
  Integer (Kind=jpim), Intent (in) :: nchannels                               ! Number of channels*profiles=radiances
  Integer (Kind=jpim), Intent(out) :: errorstatus(nprofiles)                  ! Error return code
  Integer (Kind=jpim), Intent (in) :: frequencies   (nchannels)               ! Frequency indices
  Integer (Kind=jpim), Intent (in) :: channels      (nfrequencies)            ! Channel indices
  Integer (Kind=jpim), Intent (in) :: polarisations (nchannels,3)             ! Polarisation indices
  Integer (Kind=jpim), Intent (in) :: lprofiles     (nfrequencies)               ! Profile indices
  Integer (Kind=jpim), Intent (in) :: lsprofiles     (nchannels)               ! Profile indices
  Logical            , Intent (in) :: calcemiss     (nchannels)               ! Emissivity flags

  Type (profile_Type),        Intent (in)    :: profiles        (nprofiles)   ! Atmospheric profiles
  Type (profile_Type),        Intent (in)    :: profiles_tl     (nprofiles)   ! Atmospheric profiles
  Type (rttov_coef),          Intent (in)    :: coef_rttov                    ! RTTOV Coefficients
  Type (rttov_scatt_coef),    Intent (in)    :: coef_scatt                    ! RTTOV_SCATT Coefficients
  Type (profile_cloud_Type),  Intent (in)    :: cld_profiles    (nprofiles)   ! Cloud profiles with NWP levels
  Type (profile_cloud_Type),  Intent (in)    :: cld_profiles_tl (nprofiles)   ! Cloud profiles on NWP levels
  Type (transmission_Type),   Intent (in)    :: transmission                  ! Transmittances and optical depths
  Type (transmission_Type),   Intent (in)    :: transmission_tl               ! Transmittances and optical depths
  Type (geometry_Type),       Intent (out)   :: angles          (nprofiles)   ! Zenith angles
  Type (profile_scatt_aux),   Intent (inout) :: scatt_aux                     ! Auxiliary profile variables
  Type (profile_scatt_aux),   Intent (inout) :: scatt_aux_tl                  ! Auxiliary profile variables

!* Local variables
  Integer (Kind=jpim) :: ilayer, iprof, ichan, iccmax
  Real    (Kind=jprb) :: p1, p2, pm, p1_tl, p2_tl, pm_tl, dp2dz, de2mr, zccmax

  Real    (Kind=jprb), Dimension (nprofiles,nwp_levels)   :: presf            ! Pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nprofiles,nwp_levels+1) :: presfh           ! Half-level NWP pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nrt_levels)             :: presi            ! Half-level RTTOV pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nchannels,nwp_levels)   :: opd_nwp
  Real    (Kind=jprb), Dimension (nchannels,nrt_levels)   :: opd_nrt
  Real    (Kind=jprb), Dimension (nchannels)              :: zod_up_cld       ! Optical depth from top of the atmosphere 
  Real    (Kind=jprb), Dimension (nchannels)              :: emissivity       ! Surface emissivity
  Real    (Kind=jprb), Dimension (nchannels)              :: reflectivity     ! Surface reflectivity
  Real    (Kind=jprb), Dimension (nprofiles,nwp_levels)   :: presf_tl         ! Pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nprofiles,nwp_levels+1) :: presfh_tl        ! Half-level NWP pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nrt_levels)             :: presi_tl         ! Half-level RTTOV pressure levels [hPa]
  Real    (Kind=jprb), Dimension (nchannels,nwp_levels)   :: opd_nwp_tl
  Real    (Kind=jprb), Dimension (nchannels,nrt_levels)   :: opd_nrt_tl
  Real    (Kind=jprb), Dimension (nchannels)              :: zod_up_cld_tl    ! Optical depth from top of the atmosphere 
  Real    (Kind=jprb), Dimension (nchannels)              :: emissivity_tl    ! Surface emissivity
  Real    (Kind=jprb), Dimension (nchannels)              :: reflectivity_tl  ! Surface reflectivity

  Type (transmission_Type) :: transmissioncld, transmissioncld_tl             ! Clear+cloud transmittances with cloud

  Character (len=80) :: errMessage
  Character (len=18) :: NameOfRoutine = 'rttov_iniscatt_tl '

  !- End of header --------------------------------------------------------

  errorstatus(:) = errorstatus_success

  allocate (transmissioncld    % tau_surf (nchannels))
  allocate (transmissioncld_tl % tau_surf (nchannels))

  de2mr =  1.0E+05_JPRB * rm / rgp
  dp2dz = -1.0E-03_JPRB * rgp / gravity / rm 

  scatt_aux    % ext (:,:) = 0.0_JPRB
  scatt_aux    % ssa (:,:) = 0.0_JPRB
  scatt_aux    % asm (:,:) = 0.0_JPRB
  scatt_aux_tl % ext (:,:) = 0.0_JPRB
  scatt_aux_tl % ssa (:,:) = 0.0_JPRB
  scatt_aux_tl % asm (:,:) = 0.0_JPRB
  
!* Security on user-defined pressures
  Do iprof = 1, nprofiles
     Do ilayer = 1, nwp_levels
        If (cld_profiles (iprof) % p (ilayer) >= pressure_top) Then
            presf_tl (iprof,ilayer) = cld_profiles_tl (iprof) % p (ilayer)
            presf    (iprof,ilayer) = cld_profiles    (iprof) % p (ilayer)
        else
            presf_tl (iprof,ilayer) = 0.0_JPRB
            presf    (iprof,ilayer) = pressure_top
        Endif
     Enddo
     Do ilayer = 1, nwp_levels + 1
        If (cld_profiles(iprof) % ph (ilayer) >= pressure_top) Then
            presfh_tl (iprof,ilayer) = cld_profiles_tl (iprof) % ph (ilayer)
            presfh    (iprof,ilayer) = cld_profiles    (iprof) % ph (ilayer)
        else
            presfh_tl (iprof,ilayer) = 0.0_JPRB
            presfh    (iprof,ilayer) = pressure_top
        Endif
     Enddo
  Enddo

!* Geometric variables
  Do iprof = 1, nprofiles
     Call rttov_setgeometry (profiles (iprof), coef_rttov, angles (iprof)) 
  End Do

!* Temperature at layer boundaries (K)
  Do iprof = 1, nprofiles
     scatt_aux_tl % tbd (iprof,nwp_levels+1) = profiles_tl     (iprof) % s2m % t
     scatt_aux    % tbd (iprof,nwp_levels+1) = profiles        (iprof) % s2m % t
     scatt_aux_tl % tbd (iprof,1)            = cld_profiles_tl (iprof) % t (1)
     scatt_aux    % tbd (iprof,1)            = cld_profiles    (iprof) % t (1)
  Enddo

  Do ilayer = 1, nwp_levels-1
     Do iprof = 1, nprofiles
        p1_tl = presf_tl  (iprof,ilayer+1)
        p1    = presf     (iprof,ilayer+1)
        p2_tl = presf_tl  (iprof,ilayer  )
        p2    = presf     (iprof,ilayer  )
        pm_tl = presfh_tl (iprof,ilayer+1)
        pm    = presfh    (iprof,ilayer+1)
	
        scatt_aux_tl % tbd (iprof,ilayer+1) =  cld_profiles_tl (iprof) % t (ilayer+1)   &
                                          & + (cld_profiles_tl (iprof) % t (ilayer)     &
				 	  & -  cld_profiles_tl (iprof) % t (ilayer+1))  &
				 	  & / log(p2/p1) * log(pm/p1)                   &					                    
				 	  & + (cld_profiles    (iprof) % t (ilayer)     &
				 	  & -  cld_profiles    (iprof) % t (ilayer+1))  &
					  & / (-1.0_JPRB *  log(p2/p1) * log(p2/p1) )   &
					  & * (p2_tl / p2 - p1_tl / p1) * log(pm/p1)    &					  
					  & + (cld_profiles    (iprof) % t  (ilayer)    &
					  & -  cld_profiles    (iprof) % t  (ilayer+1)) &
					  & / log(p2/p1) * (pm_tl / pm - p1_tl / p1) 
        scatt_aux    % tbd (iprof,ilayer+1) =  cld_profiles    (iprof) % t  (ilayer+1)  &
                                          & + (cld_profiles    (iprof) % t  (ilayer)    &
					  & -  cld_profiles    (iprof) % t  (ilayer+1)) &
                                          & / log(p2/p1) * log(pm/p1) 	
     Enddo
  Enddo

!* Horizontal clear-sky fraction
  scatt_aux_tl % clw  (:,:) = 0.0_JPRB
  scatt_aux_tl % ciw  (:,:) = 0.0_JPRB
  scatt_aux_tl % rain (:,:) = 0.0_JPRB
  scatt_aux_tl % sp   (:,:) = 0.0_JPRB
  scatt_aux    % clw  (:,:) = 0.0_JPRB
  scatt_aux    % ciw  (:,:) = 0.0_JPRB
  scatt_aux    % rain (:,:) = 0.0_JPRB
  scatt_aux    % sp   (:,:) = 0.0_JPRB

  scatt_aux_tl % ccmax (:) = 0.0_JPRB
  scatt_aux    % ccmax (:) = 0.0_JPRB
  
  Do iprof = 1, nprofiles  
     zccmax = 0.0_JPRB
     iccmax = 0
     
     Do ilayer = 1, nwp_levels
        if (cld_profiles (iprof) % cc (ilayer) > zccmax) then
	    zccmax = cld_profiles (iprof) % cc (ilayer)
	    iccmax = ilayer
	endif
     end do
     scatt_aux    % ccmax (iprof) = zccmax
     scatt_aux_tl % ccmax (iprof) = cld_profiles_tl (iprof) % cc (iccmax)

     If (scatt_aux % ccmax (iprof) > ccthres) Then
         scatt_aux_tl % clw  (iprof,:) = (cld_profiles_tl (iprof) % clw  (:)  &
	                              & *  scatt_aux % ccmax (iprof) - scatt_aux_tl % ccmax (iprof) &
	                              & *  cld_profiles    (iprof) % clw  (:)) &
				      & / (scatt_aux % ccmax (iprof) * scatt_aux    % ccmax (iprof)) 
         scatt_aux    % clw  (iprof,:) =  cld_profiles    (iprof) % clw  (:)  &
	                              & /  scatt_aux % ccmax (iprof)
         scatt_aux_tl % ciw  (iprof,:) = (cld_profiles_tl (iprof) % ciw  (:)  &
	                              & *  scatt_aux % ccmax (iprof) - scatt_aux_tl % ccmax (iprof) &
	                              & *  cld_profiles    (iprof) % ciw  (:)) &
				      & / (scatt_aux % ccmax (iprof) * scatt_aux    % ccmax (iprof)) 
         scatt_aux    % ciw  (iprof,:) =  cld_profiles    (iprof) % ciw  (:)  &
	                              & /  scatt_aux % ccmax (iprof)
         scatt_aux_tl % rain (iprof,:) = (cld_profiles_tl (iprof) % rain (:)  &
	                              & *  scatt_aux % ccmax (iprof) - scatt_aux_tl % ccmax (iprof) &
	                              & *  cld_profiles    (iprof) % rain (:)) &
				      & / (scatt_aux % ccmax (iprof) * scatt_aux    % ccmax (iprof)) 
         scatt_aux    % rain (iprof,:) =  cld_profiles    (iprof) % rain (:)  &
	                              & /  scatt_aux % ccmax (iprof)
         scatt_aux_tl % sp   (iprof,:) = (cld_profiles_tl (iprof) % sp   (:)  &
	                              & *  scatt_aux % ccmax (iprof) - scatt_aux_tl % ccmax (iprof) &
	                              & *  cld_profiles    (iprof) % sp   (:)) &
				      & / (scatt_aux % ccmax (iprof) * scatt_aux    % ccmax (iprof)) 
         scatt_aux    % sp   (iprof,:) =  cld_profiles    (iprof) % sp   (:)  &
	                              & /  scatt_aux % ccmax (iprof)
     else
         scatt_aux_tl % clw  (iprof,:) = 0.0_JPRB
         scatt_aux    % clw  (iprof,:) = 0.0_JPRB
         scatt_aux_tl % ciw  (iprof,:) = 0.0_JPRB
         scatt_aux    % ciw  (iprof,:) = 0.0_JPRB
         scatt_aux_tl % rain (iprof,:) = 0.0_JPRB
         scatt_aux    % rain (iprof,:) = 0.0_JPRB
         scatt_aux_tl % sp   (iprof,:) = 0.0_JPRB
         scatt_aux    % sp   (iprof,:) = 0.0_JPRB
     Endif
  Enddo

!* Nadir heights (km)
  Do ilayer = nwp_levels, 1, -1
     Do iprof = 1, nprofiles
        p1_tl = presfh_tl (iprof,ilayer+1)
        p1    = presfh    (iprof,ilayer+1)
        p2_tl = presfh_tl (iprof,ilayer  )
        p2    = presfh    (iprof,ilayer  )
	
        If (p1 <= p2) then
           errorstatus (:) = errorstatus_fatal
           Write( errMessage, '( "iniscatt : problem with user-defined pressure layering")' )
           Call Rttov_ErrorReport (errorstatus(iprof), errMessage, NameOfRoutine)
           Return
        End If
		
        scatt_aux_tl % dz (iprof,ilayer) = dp2dz * (Log(p2/p1) * cld_profiles_tl (iprof) % t (ilayer) &
	        		       & + (p2_tl / p2 - p1_tl / p1) * cld_profiles (iprof) % t (ilayer)) 			       
        scatt_aux    % dz (iprof,ilayer) = dp2dz * Log(p2/p1) * cld_profiles (iprof) % t (ilayer) 	
     Enddo
  Enddo
 
!* Interpolate optical depths (at nadir and in hPa-1) to model levels
  Do ichan = 1, nchannels
     iprof = lsprofiles (ichan)
      
     presi_tl (:) = 0.0_JPRB   

     opd_nrt_tl (ichan,1) = transmission_tl % od_singlelayer (1,ichan) / (profiles (iprof) % p (1) - pressure_top)
     opd_nrt    (ichan,1) = transmission    % od_singlelayer (1,ichan) / (profiles (iprof) % p (1) - pressure_top)

     presi             (1) = (profiles (iprof) % p (1) + pressure_top) / 2.0_JPRB

     Do ilayer = 2, nrt_levels   
        opd_nrt_tl (ichan,ilayer) = transmission_tl % od_singlelayer (ilayer,ichan) &
                                 & / (profiles (iprof) % p (ilayer) - profiles (iprof) % p (ilayer-1)) 
        opd_nrt    (ichan,ilayer) = transmission    % od_singlelayer (ilayer,ichan) &
                                 & / (profiles (iprof) % p (ilayer) - profiles (iprof) % p (ilayer-1)) 
        presi (ilayer) = (profiles (iprof) % p (ilayer) + profiles (iprof) % p (ilayer-1)) / 2.0_JPRB
     
     Enddo
     Call rttov_intex_tl (nrt_levels, nwp_levels, &
                        & presi_tl, presf_tl (iprof,:), opd_nrt_tl (ichan,:), opd_nwp_tl (ichan,:), &
                        & presi   , presf    (iprof,:), opd_nrt    (ichan,:), opd_nwp    (ichan,:)) 	
  Enddo

!* Change units
  Do ilayer = 1,nwp_levels

!* Optical depths in km-1 and at nadir
     Do ichan = 1, nchannels
        iprof = lsprofiles (ichan)
     
        scatt_aux_tl % ext (ichan,ilayer) = opd_nwp_tl (ichan,ilayer) * (presfh    (iprof,ilayer+1) - presfh    (iprof,ilayer)) &
                                        & / scatt_aux    % dz (iprof,ilayer) * angles (iprof) % coszen &
                                        & + opd_nwp    (ichan,ilayer) * (presfh_tl (iprof,ilayer+1) - presfh_tl (iprof,ilayer)) &
                                        & / scatt_aux    % dz (iprof,ilayer) * angles (iprof) % coszen &
                                        & - opd_nwp    (ichan,ilayer) * (presfh    (iprof,ilayer+1) - presfh    (iprof,ilayer)) &
                                        & * scatt_aux_tl % dz(iprof,ilayer) / (scatt_aux % dz (iprof,ilayer) &
				        & * scatt_aux % dz (iprof,ilayer)) &
					& * angles (iprof) % coszen 
        scatt_aux    % ext (ichan,ilayer) = opd_nwp    (ichan,ilayer) * (presfh     (iprof,ilayer+1) - presfh   (iprof,ilayer)) &
                                       & / scatt_aux     % dz (iprof,ilayer) * angles (iprof) % coszen 

        If (scatt_aux    % ext (ichan,ilayer) < 1.0e-10_JPRB) Then
            scatt_aux_tl % ext (ichan,ilayer) = 0.0_JPRB
            scatt_aux    % ext (ichan,ilayer) = 1.0e-10_JPRB
        Endif
     Enddo

!* Condensate from g/g to g/m^3
      Do iprof = 1, nprofiles
         scatt_aux_tl % clw (iprof,ilayer) = (scatt_aux_tl % clw (iprof,ilayer) * presf    (iprof,ilayer) &
	                                 & / cld_profiles    (iprof) % t (ilayer) &
                                         & +  scatt_aux    % clw (iprof,ilayer) * presf_tl (iprof,ilayer) &
					 & / cld_profiles    (iprof) % t (ilayer) &
                                         & -  scatt_aux    % clw (iprof,ilayer) * presf    (iprof,ilayer) &
					 & * cld_profiles_tl (iprof) % t (ilayer) &
			    	         & / (cld_profiles (iprof) % t (ilayer) * cld_profiles (iprof) % t (ilayer))) * de2mr

         scatt_aux    % clw (iprof,ilayer) = scatt_aux % clw (iprof,ilayer) * presf (iprof,ilayer) * de2mr & 
	                                 & / cld_profiles (iprof) % t (ilayer)
	     
         scatt_aux_tl % ciw (iprof,ilayer) = (scatt_aux_tl % ciw (iprof,ilayer) * presf    (iprof,ilayer) & 
	                                 & / cld_profiles    (iprof) % t (ilayer) &
                                         & +  scatt_aux    % ciw (iprof,ilayer) * presf_tl (iprof,ilayer) &
					 & / cld_profiles    (iprof) % t (ilayer) &
                                         & -  scatt_aux    % ciw (iprof,ilayer) * presf    (iprof,ilayer) &
					 & * cld_profiles_tl (iprof) % t (ilayer) &
		  	    	         & / (cld_profiles (iprof) % t (ilayer) * cld_profiles (iprof) % t (ilayer))) * de2mr

         scatt_aux    % ciw (iprof,ilayer) = scatt_aux % ciw (iprof,ilayer) * presf (iprof,ilayer) * de2mr & 
	                                 & / cld_profiles (iprof) % t (ilayer) 
     
!* Rates from kg/m^2/s to g/m^3
         scatt_aux_tl % rain (iprof,ilayer) = scatt_aux_tl % rain (iprof,ilayer) / rho_rain
         scatt_aux    % rain (iprof,ilayer) = scatt_aux    % rain (iprof,ilayer) / rho_rain
         scatt_aux_tl % sp   (iprof,ilayer) = scatt_aux_tl % sp   (iprof,ilayer) / rho_snow
         scatt_aux    % sp   (iprof,ilayer) = scatt_aux    % sp   (iprof,ilayer) / rho_snow

         scatt_aux_tl % rain (iprof,ilayer) = scatt_aux_tl % rain (iprof,ilayer) * 3600.0_JPRB 
         scatt_aux    % rain (iprof,ilayer) = scatt_aux    % rain (iprof,ilayer) * 3600.0_JPRB 
         scatt_aux_tl % sp   (iprof,ilayer) = scatt_aux_tl % sp   (iprof,ilayer) * 3600.0_JPRB
         scatt_aux    % sp   (iprof,ilayer) = scatt_aux    % sp   (iprof,ilayer) * 3600.0_JPRB

         if (scatt_aux    % rain (iprof,ilayer) > 0.0_JPRB) then
             scatt_aux_tl % rain (iprof,ilayer) =  scatt_aux_tl % rain (iprof,ilayer) &
               & * (coef_scatt % conv_rain (2)) * (scatt_aux % rain (iprof,ilayer)**(coef_scatt % conv_rain (2) - 1.0_JPRB)) &
               & * (coef_scatt % conv_rain (1))**(coef_scatt % conv_rain (2)) 
             scatt_aux    % rain (iprof,ilayer) = (scatt_aux    % rain (iprof,ilayer) &
                                          & *  coef_scatt % conv_rain (1))**(coef_scatt % conv_rain (2)) 
         else
	     scatt_aux_tl % rain (iprof,ilayer) = 0.0_JPRB
	 endif		     
         if (scatt_aux    % sp   (iprof,ilayer) > 0.0_JPRB) then
             scatt_aux_tl % sp   (iprof,ilayer) = scatt_aux_tl % sp    (iprof,ilayer) &
              & * (coef_scatt % conv_sp   (2)) * (scatt_aux % sp   (iprof,ilayer)**(coef_scatt % conv_sp   (2) - 1.0_JPRB)) &
              & * (coef_scatt % conv_sp   (1))**(coef_scatt % conv_sp   (2)) 
             scatt_aux    % sp   (iprof,ilayer) = (scatt_aux   % sp    (iprof,ilayer) &
              & *  coef_scatt % conv_sp   (1))**(coef_scatt%conv_sp     (2))
         else
             scatt_aux_tl % sp   (iprof,ilayer) = 0.0_JPRB
	 endif		     
     end do   
  Enddo
  
  
!* Cloud/rain absorption/scattering parameters
  Call rttov_mieproc_tl (   &
       & nwp_levels,        &! in
       & nchannels,         &! in
       & nprofiles,         &! in
       & frequencies,       &! in
       & lsprofiles,         &! in
       & cld_profiles,      &! in
       & cld_profiles_tl,   &! in
       & coef_rttov,        &! in
       & coef_scatt,        &! in
       & scatt_aux,         &! inout
       & scatt_aux_tl)       ! inout 
       
!* Scattering parameters for Eddington RT
  Call rttov_iniedd_tl(     &
       & nwp_levels,        &! in
       & nchannels ,        &! in
       & nprofiles ,        &! in
       & lsprofiles ,        &! in
       & angles    ,        &! in
       & coef_scatt,        &! in
       & scatt_aux ,        &! inout
       & scatt_aux_tl)       ! inout 
       
!* Surface emissivities
  zod_up_cld_tl (:) = 0.0_JPRB
  zod_up_cld    (:) = 0.0_JPRB
  
  Do ichan = 1, nchannels
     iprof = lsprofiles (ichan)
     
     Do ilayer = 1, nwp_levels     
        zod_up_cld_tl (ichan) = zod_up_cld_tl (ichan) &
                            & + scatt_aux_tl % ext (ichan,ilayer) * scatt_aux    % dz (iprof,ilayer)  &
                            & + scatt_aux    % ext (ichan,ilayer) * scatt_aux_tl % dz (iprof,ilayer)
        zod_up_cld    (ichan) = zod_up_cld    (ichan) &
                            & + scatt_aux    % ext (ichan,ilayer) * scatt_aux    % dz (iprof,ilayer)  
     Enddo
     if (zod_up_cld (ichan) >= 30.0_JPRB) then
         zod_up_cld    (ichan) = 30.0_JPRB
         zod_up_cld_tl (ichan) =  0.0_JPRB              
     endif
     
     transmissioncld    % tau_surf (ichan) = Exp(-1.0_JPRB * zod_up_cld (ichan) / angles (iprof) % coszen)
     transmissioncld_tl % tau_surf (ichan) = -1.0_JPRB * zod_up_cld_tl (ichan)  / angles (iprof) % coszen &
                                         & * transmissioncld % tau_surf (ichan)
  Enddo

  Call rttov_calcemis_mw(         &
       & profiles,                &! in
       & angles,                  &! in
       & coef_rttov,              &! in
       & nfrequencies,            &! in
       & nchannels,               &! in
       & nprofiles,               &! in
       & channels,                &! in
       & polarisations,           &! in
       & lprofiles,               &! in
       & transmissioncld,         &! in
       & calcemiss,               &! in
       & scatt_aux % ems_cld,     &! inout
       & scatt_aux % ref_cld,     &! out
       & errorstatus          )    ! inout 
       
  Call rttov_calcemis_mw_tl(      &
       & profiles,                &! in
       & profiles_tl,             &! in
       & angles,                  &! in
       & coef_rttov,              &! in
       & nfrequencies,            &! in
       & nchannels,               &! in
       & nprofiles,               &! in
       & channels,                &! in
       & polarisations,           &! in
       & lprofiles,               &! in
       & transmissioncld,         &! in
       & transmissioncld_tl,      &! in
       & calcemiss,               &! in
       & scatt_aux_tl % ems_cld,  &! inout
       & scatt_aux_tl % ref_cld)   ! out 

!* Hemispheric emissivity (= Fastem's effective emissivity)
  scatt_aux_tl % ems_bnd (:) = scatt_aux_tl % ems_cld (:)
  scatt_aux    % ems_bnd (:) = scatt_aux    % ems_cld (:)
  scatt_aux_tl % ref_bnd (:) = scatt_aux_tl % ref_cld (:)
  scatt_aux    % ref_bnd (:) = scatt_aux    % ref_cld (:)

!* Deallocate
  Deallocate (transmissioncld    % tau_surf)
  Deallocate (transmissioncld_tl % tau_surf)
  
End Subroutine rttov_iniscatt_tl
