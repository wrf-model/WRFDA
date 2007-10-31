!
module rttov_types
  ! Description:
  ! defines all derived types for RTTOV
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
  !  1.0   01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1   29/01/2003  Add CO2 variable gaz to profile sturcture (P Brunel)
  !                    Add rain and solid precip. to profile cloud structure
  !  1.2   13/05/2003  Add structure for transmissions and optical depths (F Chevallier)
  !  1.3      08/2003  Add scattering facility (F Chevallier)
  !  1.4   18/09/2003  Add kice and kradip to profile_cloud_type (P Francis)
  !  1.5   09/12/2003  Change type for mclayer to INTEGER (R Saunders)
  !  1.6   06/01/2004  Add CO2 to ref profile (R Saunders)
  !  1.7   02/06/2004  Add fast model version compatibility level in coef type (P. Brunel)
  !  1.8   17/05/2005  Add q to profile_cloud_type ( U O'Keeffe)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:

  ! Imported Parameters:
  use rttov_const, only: &
       & fastem_sp
  Use parkind1, Only : jpim     ,jprb
  Implicit None

  ! Surface skin
  Type sskin_type
  Integer(Kind=jpim) :: surftype        ! 0=land, 1=sea, 2=sea-ice
  Real(Kind=jprb)    :: t               ! radiative skin temperature (K)
  Real(Kind=jprb)    :: fastem(fastem_sp)  ! land/sea-ice surface parameters for fastem-2
  End Type sskin_type

  ! Surface 2m
  Type s2m_type
  Real(Kind=jprb) :: t                  ! temperature (K)
  Real(Kind=jprb) :: q                  ! water vapour (ppmv)
  Real(Kind=jprb) :: o                  ! ozone (ppmv)
  Real(Kind=jprb) :: p                  ! surface pressure (hPa)
  Real(Kind=jprb) :: u                  ! U wind component (m/s)
  Real(Kind=jprb) :: v                  ! V wind component (m/s)
  End Type s2m_type


  ! structure for atmospheric profiles on model pressure levels
  Type profile_type
     ! number of atmospheric levels
  Integer(Kind=jpim) :: nlevels
     ! ozone, CO2 and cloud liquid water profiles available
     logical :: ozone_data
     logical :: co2_data
     logical :: clw_data
     ! atmosphere defined on nlevels
  Real(Kind=jprb), Pointer :: p(:)      ! pressure (hPa)
  Real(Kind=jprb), Pointer :: t(:)      ! temperature (K)
  Real(Kind=jprb), Pointer :: q(:)      ! water vapour (ppmv)
  Real(Kind=jprb), Pointer :: o3(:)     ! ozone (ppmv)
  Real(Kind=jprb), Pointer :: co2(:)    ! carbon dioxide (ppmv)
  Real(Kind=jprb), Pointer :: clw(:)    ! cloud liquid water (kg/kg)
     ! surface
     Type(sskin_type) :: skin
     Type(s2m_type)   :: s2m
     !angles
  Real(Kind=jprb) :: zenangle
  Real(Kind=jprb) :: azangle
     ! Black body cloud
  Real(Kind=jprb) :: ctp               ! cloud top pressure  (hPa)
  Real(Kind=jprb) :: cfraction         ! cloud fraction (0 - 1) 1 for 100% cloud cover
  End Type profile_type

  ! structure for atmospheric profiles with information
  ! on clouds for each level
  Type profile_cloud_type
     ! number of atmospheric levels
  Integer(Kind=jpim) :: nlevels
     ! atmosphere defined on nlevels  (nlevels+1 for ph)
  Real(Kind=jprb), Pointer :: p(:)      ! full-level model pressure (hPa)
  Real(Kind=jprb), Pointer :: ph(:)     ! half-level model pressure (hPa)
  Real(Kind=jprb), Pointer :: t(:)      ! temperature (K)
  Real(Kind=jprb), Pointer :: q(:)      ! specific humidity (kg/kg)
  Real(Kind=jprb), Pointer :: cc(:)     ! cloud cover
  Real(Kind=jprb), Pointer :: clw(:)    ! cloud liquid water (kg/kg)
  Real(Kind=jprb), Pointer :: ciw(:)    ! cloud ice water (kg/kg)
  Real(Kind=jprb), Pointer :: rain(:)   ! rain (kg/(m2._JPRBs))
  Real(Kind=jprb), Pointer :: sp(:)     ! solid precipitation (kg/(m2._JPRBs))
     ! Ice cloud crystal type (0=hexagonal columns, 1=aggregates)
  Integer(Kind=jpim) :: kice
     ! Ice effective size scheme (0=Ou-Liou, 1=Wyser, 2=Boudala et al., 3=McFarquhar))
  Integer(Kind=jpim) :: kradip
  End Type profile_cloud_type

  ! satellite geometry
  Type geometry_type
  Real(Kind=jprb) :: sinzen
  Real(Kind=jprb) :: sinzen_sq
  Real(Kind=jprb) :: coszen
  Real(Kind=jprb) :: coszen_sq
  Real(Kind=jprb) :: seczen
  Real(Kind=jprb) :: seczen_sq
  Real(Kind=jprb) :: seczen_sqrt
  Real(Kind=jprb) :: seczen_minus1
  Real(Kind=jprb) :: seczen_minus1_sq
  Real(Kind=jprb) :: sinview
  Real(Kind=jprb) :: sinview_sq
  Real(Kind=jprb) :: cosview_sq
  Real(Kind=jprb) :: normzen
  End Type geometry_type

  ! Predictors
  Type predictors_type
     ! the nxxxx could be set to 0 to indicate the abscence
     ! of the predictor, in that case there is no need to
     ! allocate the corresponding predictor
  Integer(Kind=jpim) :: nlevels   ! number of levels for predictors (all same)
  Integer(Kind=jpim) :: nmixed    ! number of variables for Mixed Gases
  Integer(Kind=jpim) :: nwater    ! number of variables for Water Vapour
  Integer(Kind=jpim) :: nozone    ! number of variables for Ozone
  Integer(Kind=jpim) :: nwvcont   ! number of variables for WV Continuum
  Integer(Kind=jpim) :: nco2      ! number of variables for CO2
  Integer(Kind=jpim) :: ncloud    ! number of variables for MW Cloud
  Real(Kind=jprb), Pointer     :: mixedgas(:,:)     ! (nmixed,  nlevels)
  Real(Kind=jprb), Pointer     :: watervapour(:,:)  ! (nwater,  nlevels)
  Real(Kind=jprb), Pointer     :: ozone(:,:)        ! (nozone,  nlevels)
  Real(Kind=jprb), Pointer     :: wvcont(:,:)       ! (nwvcont, nlevels)
  Real(Kind=jprb), Pointer     :: co2(:,:)          ! (nco2,    nlevels)
  Real(Kind=jprb), Pointer     :: clw(:)            ! (nlevels)
  End Type predictors_type



  Type rttov_coef
     ! Structure for the storage of RTTOV coefficients
     ! this may differ from what is stored in the coefficient files especially
     ! for the units (ie kg/kg to ppmv)
     ! Gases are separated in MxG WV O3
     ! Number of levels is the same for all gases (taken from MxG).
     !
  Integer(Kind=jpim) :: id_platform  ! platform   (see documentation or MOD_CPARAM)
  Integer(Kind=jpim) :: id_sat    ! satellite  (.....)
  Integer(Kind=jpim) :: id_inst    ! instrument (.....)
  Integer(Kind=jpim) :: id_sensor  ! sensor
     !  1 = Infrared
     !  2 = Micro Wave
     !  3 = High resolution
  Integer(Kind=jpim) :: id_comp_lvl  ! RTTOV coefficient file version number
  Integer(Kind=jpim) ,Dimension(3) :: id_creation_date  ! YYYY MM DD
  Character (len=80)    :: id_creation    ! Creation comment
  Character (len=32)    :: id_Common_name  ! usual name of the satellite


     !FAST_MODEL_VARIABLES section
  Character (len=32)    :: fmv_model_def  ! FMV definition (RTTOV6 OPTRAN RTTOV7)
  Integer(Kind=jpim)               :: fmv_model_ver  ! fast model version compatibility level
  Integer(Kind=jpim)               :: fmv_chn        ! number of channels in file
  Integer(Kind=jpim)               :: fmv_gas        ! number of gases in file
  Integer(Kind=jpim), pointer      :: fmv_gas_id(:)  ! gas id. number i gas_id list (fmv_gas)
  Integer(Kind=jpim), Pointer      :: fmv_gas_pos(:) ! respective position of each gas of gas_id list (ngases_max)
  Integer(Kind=jpim), Pointer      :: fmv_var(:)     ! number of variables/predictors by gaz (fmv_gas)
  Integer(Kind=jpim), Pointer      :: fmv_lvl(:)     ! number of levels(pres/absorber) by gaz (fmv_gas)
  Integer(Kind=jpim)               :: nmixed         ! number of variables/predictors for Mixed Gases
  Integer(Kind=jpim)               :: nwater         ! number of variables/predictors for Water Vapour
  Integer(Kind=jpim)               :: nozone         ! number of variables/predictors for Ozone
  Integer(Kind=jpim)               :: nwvcont        ! number of variables/predictors for WV continuum
  Integer(Kind=jpim)               :: nco2           ! number of variables/predictors for CO2
  Integer(Kind=jpim)               :: nn2o           ! number of variables/predictors for N2O
  Integer(Kind=jpim)               :: nco            ! number of variables/predictors for CO
  Integer(Kind=jpim)               :: nch4           ! number of variables/predictors for CH4
  Integer(Kind=jpim)               :: nlevels        ! number of levels(pres/absorber) same for all gases

     !GAZ_UNITS section
     ! gases are in the order of gas id codes
  Integer(Kind=jpim), Pointer      :: gaz_units(:)   ! unit of gaz concentration for each gaz
                                             ! default value is specific conc. (kg/kg)
                                             ! value inside RTTOV calculations (ppmv)
     !FILTER_FUNCTIONS section  array size is fmv_chn
  Integer(Kind=jpim) ,Pointer :: ff_ori_chn(:)   ! original chan number
  Integer(Kind=jpim) ,Pointer :: ff_val_chn(:)   ! validity of the channel (1=OK)
  Real(Kind=jprb) ,Pointer :: ff_cwn (:)      ! cental wave number (cm-1)
  Real(Kind=jprb) ,Pointer :: ff_bco (:)      ! band correction offset (K)
  Real(Kind=jprb) ,Pointer :: ff_bcs (:)      ! band correction slope (K/K)
  Real(Kind=jprb) ,Pointer :: ff_gam (:)      ! gamma factor transm. correction

     !FUNDAMENTAL_CONSTANTS section
  Real(Kind=jprb) :: fc_speedl         ! speed of light (cm/s)
  Real(Kind=jprb) :: fc_planck_c1      ! first radiation constant (mW/(m2*sr*cm-4))
  Real(Kind=jprb) :: fc_planck_c2      ! second radiation constant (cm*K)
  Real(Kind=jprb) :: fc_sat_height     ! satellite nominal altitude (km)

     !FASTEM section
  Integer(Kind=jpim) :: fastem_ver      ! fastem version number
  Integer(Kind=jpim) :: fastem_coef_nb  ! number of coefficients
  Real(Kind=jprb), Pointer    :: fastem_coef(:)  ! coefficients (fastem_coef_nb)
  Integer(Kind=jpim), Pointer :: fastem_polar(:) ! polarisation of each channel
     ! 0 = 0.5 V+H
     ! 1 = 90 - incident angle
     ! 2 = incident angle
     ! 3 = vertical
     ! 4 = horizontal
     ! 5 = V+H
     ! Full stokes vector

     !SSIREM section     array size is fmv_chn
     ! ems =   ssirem_a0
     !       - ssirem_a1*(zen**ssirem_xzn1)
     !       - ssirem_a2*(zen**ssirem_xzn2)
     ! where zen is satellite zenith angle in degrees, divided by 60.
  Integer(Kind=jpim) :: ssirem_ver                ! version number
  Integer(Kind=jpim),  Pointer  :: ssirem_chn(:)  ! original chan number
  Real(Kind=jprb),  Pointer     :: ssirem_a0(:)   ! constant coef
  Real(Kind=jprb),  Pointer     :: ssirem_a1(:)   ! first coef
  Real(Kind=jprb),  Pointer     :: ssirem_a2(:)   ! second coef
  Real(Kind=jprb),  Pointer     :: ssirem_xzn1(:) ! 1st exponent on zenith angle
  Real(Kind=jprb),  Pointer     :: ssirem_xzn2(:) ! 2nd exponent on zenith angle

     !REFERENCE_PROFILE section  defined on Mixed gases pressure levels
     ! Not working for OPTRAN gas absorber levels
     ! gases are in the order of gas id codes
     ! unit for mr in coeff file is kg/kg or ppmv (see gaz_units section)
     ! unit for mr for optical depth calculations is ppmv
  Real(Kind=jprb), Pointer      :: ref_prfl_p(:)    ! pressure  (hPa)       (levels)
  Real(Kind=jprb), Pointer      :: ref_prfl_t(:,:)  ! temperature (K)       (levels, gases)
  Real(Kind=jprb), Pointer      :: ref_prfl_mr(:,:) ! mixing ratio (ppmv)   (levels, gases)
     !PROFILE_LIMITS section
     ! gases are in the order of gas id codes
     ! unit for mr in coeff file is kg/kg or ppmv (see gaz_units section)
     ! unit for mr for optical depth calculations is ppmv
  Real(Kind=jprb), Pointer      :: lim_prfl_p(:)    ! pressure  (hPa)       (levels)
  Real(Kind=jprb), Pointer      :: lim_prfl_tmax(:) ! max temperature (K)   (levels)
  Real(Kind=jprb), Pointer      :: lim_prfl_tmin(:) ! min temperature (K)   (levels)
  Real(Kind=jprb), Pointer      :: lim_prfl_gmax(:,:) ! max mixing r (ppmv) (levels, gases)
  Real(Kind=jprb), Pointer      :: lim_prfl_gmin(:,:) ! min mixing r (ppmv) (levels, gases)


     !FAST_COEFFICIENTS section
     ! separate arrays to allow dififerent number of variables for each gaz
  Real(Kind=jprb), Pointer      :: mixedgas(:,:,:)    ! Mixed gases coefs  (levels, channels, variables)
  Real(Kind=jprb), Pointer      :: watervapour(:,:,:) ! Water vapour coefs (levels, channels, variables)
  Real(Kind=jprb), Pointer      :: ozone(:,:,:)       ! Ozone coefs        (levels, channels, variables)
  Real(Kind=jprb), Pointer      :: wvcont(:,:,:)      ! WV Cont coefs      (levels, channels, variables)
  Real(Kind=jprb), Pointer      :: co2(:,:,:)         ! CO2 coefs          (levels, channels, variables)
  Real(Kind=jprb), Pointer      :: n2o(:,:,:)         ! N2O coefs          (levels, channels, variables)
  Real(Kind=jprb), Pointer      :: co(:,:,:)          ! CO coefs           (levels, channels, variables)
  Real(Kind=jprb), Pointer      :: ch4(:,:,:)         ! CH4 coefs          (levels, channels, variables)

     ! Auxillary variables
  Real(Kind=jprb)               :: ratoe       ! ratio (H+R)/R  H=sat height, R=Earth radius
  Real(Kind=jprb), pointer      :: planck1(:)       ! C1 * Nu**3
  Real(Kind=jprb), pointer      :: planck2(:)       ! C2 * Nu
  Real(Kind=jprb), pointer      :: frequency_ghz(:) ! frequency in GHz

     ! other predictor variables see Science and Validation report
  Real(Kind=jprb), pointer      :: dp(:)        ! interval between standard p levels (hPa)
  Real(Kind=jprb), pointer      :: dpp(:)       ! pressure based variable (hPa**2)
  Real(Kind=jprb), pointer      :: tstar(:)     ! layer temp (K)
  Real(Kind=jprb), pointer      :: to3star(:)   ! layer temp for O3 calculations (K)
  Real(Kind=jprb), pointer      :: wstar(:)     ! layer WV  (ppmv)
  Real(Kind=jprb), pointer      :: ostar(:)     ! layer O3  (ppmv)
  Real(Kind=jprb), pointer      :: co2star(:)   ! layer co2 (ppmv)

  End Type rttov_coef

  Type rttov_scatt_coef
     ! Structure for the storage of RTTOV_SCATT coefficients
  Integer(Kind=jpim) :: nhydro ! Number of hydrometeors in computation
  Integer(Kind=jpim) :: mtype  ! Number of hydrometeors     in Mie tables
  Integer(Kind=jpim) :: mfreqm ! Number of frequencies      in Mie tables
  Integer(Kind=jpim) :: mtemp  ! Number of temperature bins in Mie tables
  Integer(Kind=jpim) :: mwc    ! Number of water bins       in Mie tables
  Real(Kind=jprb)    :: offset_temp_rain       ! temperature offset in table for rain type
  Real(Kind=jprb)    :: offset_temp_sp         ! temperature offset in table for solid prec. type
  Real(Kind=jprb)    :: offset_temp_liq        ! temperature offset in table for cloud water type
  Real(Kind=jprb)    :: offset_temp_ice        ! temperature offset in table for cloud ice type
  Real(Kind=jprb)    :: offset_water           ! liquid/ice water offset in table
  Real(Kind=jprb)    :: scale_water            ! log10(liquid/ice water) scaling factor in table
  Real(Kind=jprb)    :: from_scale_water       ! 10**(1._JPRB/scale_water)
  Real(Kind=jprb)    :: conv_rain(2)           ! coefficients for rain unit conversion (mm.h-1 to g.m-3)
  Real(Kind=jprb)    :: conv_sp  (2)           ! coefficients for solid prec. unit conversion (mm.h-1 to g.m-3)
  Real(Kind=jprb)    :: conv_liq (2)           ! coefficients for cloud water conversion (not used)
  Real(Kind=jprb)    :: conv_ice (2)           ! coefficients for cloud ice conversion   (not used)
  Real(Kind=jprb), pointer :: mie_freq(:)      ! list of frequencies in Mie table
  Real(Kind=jprb), pointer :: ext(:,:,:,:)     ! extinction coefficent table
  Real(Kind=jprb), pointer :: ssa(:,:,:,:)     ! single scattering albedo table
  Real(Kind=jprb), pointer :: asp(:,:,:,:)     ! assymetry parameter table

  End Type rttov_scatt_coef

  ! Auxillary profile variables
  ! variables calculated by the model from profile
  type profile_aux
  Integer(Kind=jpim) :: nearestlev_surf ! nearest model level above surface
  Real(Kind=jprb)    :: pfraction_surf  ! pressure fraction of surface in model layer (hPa)
  Integer(Kind=jpim) :: nearestlev_ctp  ! nearest model level above cloud top
  Real(Kind=jprb)    :: pfraction_ctp   ! pressure fraction of cloud top pressure in layer (hPa)
  Real(Kind=jprb)    :: cfraction       ! cloud fraction (0 - 1) 1 for 100% cloud cover
  Real(Kind=jprb), pointer :: debye_prof(:,:) ! Debye terms
  end type profile_aux

  ! Auxillary profile variables for RTTOV_SCATT
  ! variables calculated by the model from profile
  Type profile_scatt_aux
  Real(Kind=jprb), pointer :: ccmax(:)    ! horizontal cloud fraction (one value used for all layers)
  Real(Kind=jprb), pointer :: ems_bnd(:)  ! surface emissivity for boundary conditions
  Real(Kind=jprb), pointer :: ref_bnd(:)  ! surface emissivity for boundary conditions
  Real(Kind=jprb), pointer :: ems_cld(:)  ! surface emissivity taking into account cloud/rain impact on od
  Real(Kind=jprb), pointer :: ref_cld(:)  ! surface reflectivity taking into account cloud/rain impact on od
  Real(Kind=jprb), pointer :: dz(:,:)     ! layer depth   [km]
  Real(Kind=jprb), pointer :: tbd(:,:)    ! temperature at layer boundary [K]
  Real(Kind=jprb), Pointer :: clw(:,:)    ! cloud liquid water (g/m3)
  Real(Kind=jprb), Pointer :: ciw(:,:)    ! cloud ice water (g/m3)
  Real(Kind=jprb), Pointer :: rain(:,:)   ! rain (g/m3)
  Real(Kind=jprb), Pointer :: sp(:,:)     ! solid precipitation (g/m3)
!RWS  Real(Kind=jprb), pointer :: mclayer(:)  ! upper level cloud layer
  Integer(Kind=jpim), pointer :: mclayer(:)  ! upper level cloud layer
  Real(Kind=jprb), pointer :: delta(:,:)  ! (= ext*dz/coszen)
  Real(Kind=jprb), pointer :: tau(:,:)    ! optical depths (= exp(-delta))
  Real(Kind=jprb), pointer :: ext(:,:)    ! extinction coefficient integreated over hydrometeor types
  Real(Kind=jprb), pointer :: ssa(:,:)    ! single scattering albedo integreated over hydrometeor types
  Real(Kind=jprb), pointer :: asm(:,:)    ! asymetry parameter integreated over hydrometeor types [-1,1]
  Real(Kind=jprb), pointer :: lambda(:,:) ! eddington approx. variable
                                  ! (= sqrt( 3*ext*ext*(1-ssa)*(1-ssa*asm) )
  Real(Kind=jprb), pointer :: h (:,:)     ! boundary condition variable (= 1.5_JPRB*ext(1-ssa*asm))
  Real(Kind=jprb), pointer :: b0(:,:)     ! lower level temperature
  Real(Kind=jprb), pointer :: b1(:,:)     ! temperature gradient
  Real(Kind=jprb), pointer :: bn(:,:)     ! upper level temperature
  end type profile_scatt_aux

  type transmission_type
     ! Transmissions and optical depths (unitless)
  Real(Kind=jprb), pointer  :: tau_surf(:)          ! transmittance from surface (array size is of size nchannels)
  Real(Kind=jprb), pointer  :: tau_layer(:,:)       ! transmittance from each standard pressure level
                                                    !   (array size is of size (nlevels,nchannels))
  Real(Kind=jprb), pointer  :: od_singlelayer(:,:)  ! single-layer optical depth
                                                    !   (array size is of size (nlevels,nchannels))
  end type transmission_type

  type radiance_type
     ! Radiance and corresponding brightness temperature
     ! Array size is of size nchannels
     ! except for cloudy calculations (nlevels, nchannels)
     ! unit for radiance is mw/cm-1/ster/sq.m
     ! unit for temperature is Kelvin
     !
     logical :: lcloud  ! if true the last array is calculated
                        ! if false it does not need to be allocated
     !
  Real(Kind=jprb), pointer  :: clear(:)      ! clear sky radiance
  Real(Kind=jprb), pointer  :: clear_out(:)  ! clear sky radiance
  Real(Kind=jprb), pointer  :: cloudy(:)     ! 100% cloudy radiance for given cloud
  Real(Kind=jprb), pointer  :: total(:)      ! cloudy radiance for given cloud
  Real(Kind=jprb), pointer  :: total_out(:)  ! cloudy radiance for given cloud
  Real(Kind=jprb), pointer  :: out(:)        ! BT equivalent to total radiance
  Real(Kind=jprb), pointer  :: out_clear(:)  ! BT equivalent to clear radiance
  Real(Kind=jprb), pointer  :: bt(:)         ! Brightness temp equivalent to total radiance
  Real(Kind=jprb), pointer  :: bt_clear(:)   ! Brightness temp equivalent to clear radiance
  Real(Kind=jprb), pointer  :: upclear(:)    ! clear sky radiance without reflection term
  Real(Kind=jprb), pointer  :: dnclear(:)    ! clear sky downwelling radiance
  Real(Kind=jprb), pointer  :: reflclear(:)  ! reflected clear sky downwelling radiance
  Real(Kind=jprb), pointer  :: overcast(:,:) ! overcast radiance at given cloud
                                             !   top  (levels,channels)
  Real(Kind=jprb), pointer  :: downcld(:,:)  ! contribution to radiance of downward
                                             !   cloud emission at given cloud top
                                             !   (levels,channels)
  end type radiance_type

  type radiance_cloud_type
     ! Emissivity and radiance arrays for cloudy conditions
     ! see rttov_cld
     ! Array size is of size nchannels
     ! except for cloudy calculations (nlevels, nchannels)
     !
     ! First part, same definition as the radiance type
  logical :: lcloud     ! if true the last array is calculated
                        ! if false it does not need to be allocated
  Real(Kind=jprb), pointer  :: clear(:)      ! clear sky radiance
  Real(Kind=jprb), pointer  :: clear_out(:)  ! clear sky radiance
  Real(Kind=jprb), pointer  :: cloudy(:)     ! 100% cloudy radiance for given cloud
  Real(Kind=jprb), pointer  :: total(:)      ! cloudy radiance for given cloud
  Real(Kind=jprb), pointer  :: total_out(:)  ! cloudy radiance for given cloud
  Real(Kind=jprb), pointer  :: out(:)        ! Brightness temp equivalent to total radiance
  Real(Kind=jprb), pointer  :: out_clear(:)  ! Brightness temp equivalent to clear radiance
  Real(Kind=jprb), pointer  :: bt(:)         ! Brightness temp equivalent to total radiance
  Real(Kind=jprb), pointer  :: bt_clear(:)   ! Brightness temp equivalent to clear radiance
  Real(Kind=jprb), pointer  :: upclear(:)    ! clear sky radiance without reflection term
  Real(Kind=jprb), pointer  :: dnclear(:)    ! clear sky downwelling radiance
  Real(Kind=jprb), pointer  :: reflclear(:)  ! reflected clear sky downwelling radiance
  Real(Kind=jprb), pointer  :: overcast(:,:) ! overcast radiance at given cloud
                                             !   top  (levels,channels)
  Real(Kind=jprb), pointer  :: downcld(:,:)  ! contribution to radiance of downward
                                             !   cloud emission at given cloud top
                                             !   (levels,channels)
     !
     ! Second part Cloud specific
  Real(Kind=jprb), pointer  :: cldemis(:,:)  ! cloud emissivity (levels, channels)
  Real(Kind=jprb), pointer  :: wtoa(:,:)     ! toa weights of of cloud layers
  Real(Kind=jprb), pointer  :: wsurf(:,:)    ! surface weights of cloud layers
  Real(Kind=jprb), pointer  :: cs_wtoa(:)    ! contribution from clear sky fraction
  Real(Kind=jprb), pointer  :: cs_wsurf(:)   ! contribution from clear sky fraction

     !
     ! Third part scatter specific
  Real(Kind=jprb), pointer  :: freq_used(:)  ! list of frequencies actually used for the Mie computations
     ! (they may not be strictly equal to the frequencies requested)
  end type radiance_cloud_type



  type radiance_aux
     ! auxillary calculation arrays for RTE integration
     ! Direct model arrays need to be passed to TL AD and K codes
     ! array size is of (nchannels) or (nlevels, nchannels)
  Real(Kind=jprb), pointer :: layer(:,:)
  Real(Kind=jprb), pointer :: surfair(:)
  Real(Kind=jprb), pointer :: skin(:)
  Real(Kind=jprb), pointer :: cosmic(:)
  Real(Kind=jprb), pointer :: up(:,:)         ! sum( B * dT )
  Real(Kind=jprb), pointer :: down(:,:)       ! sum ( B / T**2 dT )
  Real(Kind=jprb), pointer :: down_cloud(:,:)
  end type radiance_aux

End Module rttov_types
