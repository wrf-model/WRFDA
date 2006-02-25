MODULE DA_Constants

!-----------------------------------------------------------------------------
!  PURPOSE: Common reference point for constants.
!
!  METHOD:  Straightforward definitions.
!
!  HISTORY: 11/08/1999 - Creation.                           Dale Barker
!              07/2005 - Add radiance assimilation           Zhiquan Liu
!                        namelist parameter 'rtminit'
!------------------------------------------------------------------------------

   IMPLICIT NONE

!------------------------------------------------------------------------------
!  [1.0] Physical parameter constants (all NIST standard values):
!------------------------------------------------------------------------------

!  Fundamental constants:
   REAL, PARAMETER    :: pi = 3.1415926535897932346
#ifdef MM5_CONSTANT
   REAL, PARAMETER    :: gas_constant = 287.04   ! Value used in MM5.
   REAL, PARAMETER    :: gas_constant_v = 461.51 ! 
   REAL, PARAMETER    :: cp = 1004.0             ! Value used in MM5.
#else
   REAL, PARAMETER    :: gas_constant = 287.     ! Value used in WRF.
   REAL, PARAMETER    :: gas_constant_v = 461.6  ! Value used in WRF.
   REAL, PARAMETER    :: cp = 7.*gas_constant/2. ! Value used in WRF.
#endif
   REAL, PARAMETER    :: t_kelvin = 273.15

   REAL, PARAMETER    :: kappa = gas_constant / cp
   REAL, PARAMETER    :: rd_over_rv = gas_constant / gas_constant_v
   REAL, PARAMETER    :: rd_over_rv1 = 1.0 - rd_over_rv
   REAL, PARAMETER    :: L_over_Rv = 5418.12

   REAL, PARAMETER    :: gamma = 1.4

!  Earth constants:
   REAL, PARAMETER    :: gravity = 9.81        ! m/s - value used in MM5.
!  REAL, PARAMETER    :: earth_radius = 6378.15
   REAL, PARAMETER    :: earth_radius = 6370.0		! Be consistant with WRF
!  REAL, PARAMETER    :: earth_omega  = 2.0*pi/86400.0  ! Omega
   REAL, PARAMETER    :: earth_omega  = 0.000072921     ! Omega 7.2921*10**-5

!  Saturation Vapour Pressure Constants(Rogers & Yau, 1989) 
   REAL, PARAMETER    :: es_alpha = 611.2
   REAL, PARAMETER    :: es_beta = 17.67
   REAL, PARAMETER    :: es_gamma = 243.5
   REAL, PARAMETER    :: es_gammabeta = es_gamma * es_beta
   REAL, PARAMETER    :: es_gammakelvin = es_gamma - t_kelvin

!  Explicit moist constants:
   REAL, PARAMETER    :: SVP1=0.6112, SVP2=17.67, SVP3=29.65
   REAL, PARAMETER    :: SVPT0=273.15, TO=273.15
   REAL, PARAMETER    :: N0R=8.E6, N0S=2.E7, RHOS=0.1
   REAL, PARAMETER    :: AVT=841.99667, BVT=0.8, BVT2=2.5+.5*BVT, BVT3=3.+BVT
   REAL, PARAMETER    :: PPI=1./(pi*N0R), PPIS=1./(pi*N0S*RHOS)
   REAL, PARAMETER    :: XLV1=2370., XLF0=.3337E6, XLV0=3.15E6
   REAL, PARAMETER    :: XLS=XLV0-XLV1*273.16+XLF0

!  Planetary boundary physics (/MM5/physics/pbl_sfc/mrfpbl/mrfpbl.F) constants
   REAL, PARAMETER         :: k_kar = 0.4    ! Von Karman constant

!  GPS Refractivity constant  
   real, parameter    :: coeff = 3.73e5 / 77.6
   
!------------------------------------------------------------------------------
!  [2.0] 3DVAR parameter constants:
!------------------------------------------------------------------------------

! Missing values and the index number of the quality contro

   INTEGER, PARAMETER ::  missing       = -888888
   REAL   , PARAMETER ::  missing_r     = -888888.
   REAL   , PARAMETER ::  Max_StHeight_Diff = 100.

!  3DVAR Minimisation:
   LOGICAL            :: write_switch   ! switch to write results in the
   INTEGER            :: write_interval ! write_interval (number of iterations)
   INTEGER            :: iter
   INTEGER, PARAMETER :: MP = 6
   INTEGER, PARAMETER :: LP = 6
   INTEGER, PARAMETER :: MAXFEV = 10
   REAL, PARAMETER    :: FTOL = 1.0E-4
   REAL, PARAMETER    :: GTOL = 0.9
   REAL, PARAMETER    :: XTOL = 1.0E-17
   REAL, PARAMETER    :: STPMIN = 1.0E-20
   REAL, PARAMETER    :: STPMAX = 1.0E+20

!  Background errors:
   REAL, PARAMETER    :: pplow = 1.0e-8       ! Machine lowest number?
   REAL, PARAMETER    :: pp_umin = 1.0e-2     ! Minimum u back. error (m/s).
   REAL, PARAMETER    :: pp_vmin = 1.0e-2     ! Minimum v back. error (m/s).
   REAL, PARAMETER    :: pp_tmin = 1.0e-2     ! Minimum t back. error (K).
   REAL, PARAMETER    :: pp_qmin = 1.0e-6     ! Minimum q back. error (kg/kg)
   REAL, PARAMETER    :: pp_pmin= 1.0e+1      ! Minimum pp back. error (Pa).

!  FFTs:
   INTEGER, PARAMETER :: Forward_FFT     = -1 ! Grid to spectral
   INTEGER, PARAMETER :: Inverse_FFT     =  1 ! Spectral to grid.
   INTEGER, PARAMETER :: num_fft_factors = 10 ! Max number of factors.
 
!  Balance:
   INTEGER, PARAMETER :: balance_geo = 1      ! Geostrophic balance.
   INTEGER, PARAMETER :: balance_cyc = 2      ! Cyclostrophic balance.
   INTEGER, PARAMETER :: balance_geocyc = 3   ! Geo/cyclostrophic balance.

!  Adjoint tests:
   REAL, PARAMETER    :: typical_u_rms = 2.0     ! m/s
   REAL, PARAMETER    :: typical_v_rms = 2.0     ! m/s
   REAL, PARAMETER    :: typical_speed_rms = 2.0 ! m/s
   REAL, PARAMETER    :: typical_tb19v_rms = 1.0 ! K
   REAL, PARAMETER    :: typical_tb19h_rms = 1.0 ! K
   REAL, PARAMETER    :: typical_tb22v_rms = 1.0 ! K
   REAL, PARAMETER    :: typical_tb37v_rms = 1.0 ! K
   REAL, PARAMETER    :: typical_tb37h_rms = 1.0 ! K
   REAL, PARAMETER    :: typical_tb85v_rms = 1.0 ! K
   REAL, PARAMETER    :: typical_tb85h_rms = 1.0 ! K
   REAL, PARAMETER    :: typical_t_rms = 1.0     ! K
   REAL, PARAMETER    :: typical_p_rms = 100.0   ! Pa
   REAL, PARAMETER    :: typical_q_rms = 0.00001 ! g/kg
   REAL, PARAMETER    :: typical_rho_rms = 0.01  ! kg/m^3
   REAL, PARAMETER    :: typical_tpw_rms = 0.2   ! cm
   REAL, PARAMETER    :: typical_ref_rms = 5.0   ! N unit
   REAL, PARAMETER    :: typical_rh_rms = 20.0   ! %
   REAL, PARAMETER    :: typical_thickness_rms = 50.0   ! m
   REAL, PARAMETER    :: typical_qrn_rms = 0.00001 ! g/kg
   REAL, PARAMETER    :: typical_qcw_rms = 0.00001 ! g/kg
   REAL, PARAMETER    :: typical_w_rms = 0.1     ! m/s
   REAL, PARAMETER    :: typical_rv_rms = 1.0    ! m/s
   REAL, PARAMETER    :: typical_rf_rms = 1.0    ! dBZ

!  The following typical mean squared values depend on control variable. They 
!  are calculated in DA_Setup_Background_Errors and used in the VvToVp adjoint 
!  test:

   REAL, PARAMETER    :: inv_typ_vp1_sumsq = 0.00001 ! 1/SUM(psi**2)
   REAL, PARAMETER    :: inv_typ_vp2_sumsq = 0.00001 ! 1/SUM(chi**2)
   REAL, PARAMETER    :: inv_typ_vp3_sumsq = 0.00001 ! 1/SUM(phi_u**2)
   REAL, PARAMETER    :: inv_typ_vp4_sumsq = 10000.0 ! 1/SUM(q**2)
   REAL, PARAMETER    :: inv_typ_vp5_sumsq = 0.00001 ! 1/SUM(?**2)

!  Fortran unit  parameters:

   ! Stderr and stdout can be controlled by namelist Record1. 
   ! stdout is set to stdout_unit, ditto stderr
   INTEGER :: stdout_unit = 6      ! Standard output
   INTEGER :: stderr_unit = 0     ! Standard error
   INTEGER :: stdout = 6          ! Standard output
   INTEGER :: stderr = 0          ! Standard error

   INTEGER, PARAMETER :: jpout = 11           ! Unit number for x output.
   INTEGER, PARAMETER :: stats_unit = 12      ! Unit number for stats output.
   INTEGER, PARAMETER :: innov_vector_unit = 14 ! Innovation vector diagnostics

   INTEGER, PARAMETER :: cov_unit = 25 ! Covariances unit

   INTEGER, PARAMETER :: be_cv_2_unit = 32    ! Unit number for be input.
   INTEGER, PARAMETER :: be_cv_3_unit = 33    ! Unit number for be input.
   INTEGER, PARAMETER :: be_cv_4_unit = 34    ! Unit number for be input. 
   INTEGER, PARAMETER :: be_cv_5_unit = 35    ! Unit number for be input.

   INTEGER, PARAMETER :: terrain_unit = 40    ! Unit number for terrain input.
   INTEGER, PARAMETER :: jpin = 41            ! Unit number for xb input.

   integer, parameter :: rand_unit = 45       ! Output of noise.
   integer, parameter :: yp_unit = 46         ! Output of H(x'(yo+noise)).
   integer, parameter :: y_unit = 47          ! Output of H(x'(yo)).
   integer, parameter :: jo_unit = 48         ! Output of components of Jo, ErrFac.
   INTEGER, PARAMETER :: anl_inc_unit = 141   ! Analysis increment unit     
   integer, parameter :: fac_unit = 49        ! Input components of ErrFac.
   integer, parameter :: omb_unit = 50        ! Output for O-B values.

    INTEGER, PARAMETER :: bufrtovs_unit = 55  ! Input BUFR/TOVS data

   INTEGER, PARAMETER :: check_max_iv_unit=60 ! Unit number for O-B check.

   integer, parameter :: cost_unit = 81       ! Output of cost-function value
   integer, parameter :: grad_unit = 82       ! Output of gradient norm

   INTEGER, PARAMETER :: bufr_iunit = 91      ! Unit number for bufr obs input.
   INTEGER, PARAMETER :: gts_iunit  = 92      ! Unit number for GTS obs input.
   INTEGER, PARAMETER :: ssmi_iunit = 93      ! Unit number for SSMI obs input.
   INTEGER, PARAMETER :: Radar_iunit = 94     ! Unit number for Radar input.
   INTEGER, PARAMETER :: airs_table_unit = 95 ! AIRS bufr table input
   INTEGER, PARAMETER :: rtm_error_unit = 99  ! RTTOV error unit
   INTEGER, PARAMETER :: rtm_info_unit = 109  ! Radiance info file
   INTEGER, PARAMETER :: rtm_bias_unit = 110  ! Radiance bias file
   INTEGER, PARAMETER :: filtered_obs_iunit  = 192  ! Unit number for writing filtered obs
   INTEGER, PARAMETER :: unit_983            = 983 ! TBD
   integer, parameter  :: iunit = 70
   integer, parameter  :: ounit = iunit + 1
   integer, parameter  :: namelist_unit = 72
   integer, parameter  :: spec_unit = 73     ! Unit for spectral diag. output.

   INTEGER, PARAMETER :: max_num_of_var = 200 ! Maximum # of stored fields.

!------------------------------------------------------------------------------
!  [3.0] Variables used in MM5 part of code:
!------------------------------------------------------------------------------

   INTEGER            :: map_projection       ! 1=LamConf/2=PolarSte/3=Mercator
   REAL               :: ycntr
   INTEGER            :: coarse_ix            ! COARSE DOMAIN DIM IN I DIRECTION.
   INTEGER            :: coarse_jy            ! COARSE DOMAIN DIM IN Y DIRECTION.
   REAL               :: coarse_ds            ! Coarse domain gridlength (km)
   REAL               :: start_x              ! i posn. of (1,1) in coarse domain.
   REAL               :: start_y              ! j posn. of (1,1) in coarse domain.
   REAL               :: start_lat            ! Latitude coresponds to start_(x,y)
   REAL               :: start_lon            ! Longitude coresponds to start_(x,y)
   REAL               :: delt_lat             ! Latitude increments for global grids
   REAL               :: delt_lon             ! Longitude increments for global grids

   REAL               :: phic                 ! COARSE DOMAIN CENTRAL LAT(DEGREE)
   REAL               :: xlonc                ! COARSE DOMAIN CENTRAL LON(DEGREE)
   REAL               :: cone_factor          ! Cone Factor
   REAL               :: truelat1_3dv         ! True latitude 1 (degrees)
   REAL               :: truelat2_3dv         ! True latitude 2 (degrees)
   REAL               :: pole                 ! Pole latitude (degrees)
   REAL               :: dsm                  ! Current domain gridlength (km)
   REAL               :: psi1                 ! ?
   REAL               :: c2                   ! earth_radius * COS(psi1)

   REAL               :: ptop
   REAL               :: ps0
   REAL               :: ts0
   REAL               :: tlp
   REAL               :: tis0

!------------------------------------------------------------------------------
! 4.0 vertical interpolation options
!------------------------------------------------------------------------------

   INTEGER, PARAMETER :: v_interp_not_specified = missing, &
                         v_interp_p             = 1, &
                         v_interp_h             = 2

!------------------------------------------------------------------------------
! 3DVAR scalar constants:
!------------------------------------------------------------------------------
 integer                :: Anal_Space  ! Space of analysis
                                       ! ( 1 = Full model,
                                       !   2 = Transformed grid,
                                       !   3 = Ob space (PSAS) )

 integer                :: mix         ! 1st dimension of analysis grid.
 integer                :: mjy         ! 2nd dimension of analysis grid.
 integer                :: mkz         ! 3rd dimension of analysis grid.

!Minimisation:
 logical                :: calculate_cg_cost_function
 logical                :: write_outer_loop
 logical                :: lat_stats_option
 logical                :: W_INCREMENTS  ! Analysis includes w increments
 logical                :: DT_cloud_model ! Time step from cloud model parameterization 
 logical                :: write_qrn
 logical                :: write_qcw
 logical                :: write_qci
 logical                :: write_qsn
 logical                :: write_qgr

 integer                :: max_ext_its ! Max. number of external iterations.
 integer                :: ntmax       ! Max. number of minim. its.
 integer                :: nsave       ! #Minimisation states to store.
 integer                :: minimisation_option  ! Minimisation option. 1. QN, 2 CG.

 real, dimension(10)    :: eps         ! Fraction of initial gradient
                                       ! required for min. convergence.

 INTEGER                :: cv_size     ! Size of control variable.
 INTEGER,dimension(5)   :: kz_vv,kz_vp ! vertical dimensions of vv and vp arrays

!Recursive filter:
 INTEGER                :: rf_passes   ! Number of passes in recursive filter.
 REAL                   :: var_scaling1! Variance rescaling factor.
 REAL                   :: var_scaling2! Variance rescaling factor.
 REAL                   :: var_scaling3! Variance rescaling factor.
 REAL                   :: var_scaling4! Variance rescaling factor.
 REAL                   :: var_scaling5! Variance rescaling factor.
 REAL                   :: len_scaling1! Correlation rescaling factor.
 REAL                   :: len_scaling2! Correlation rescaling factor.
 REAL                   :: len_scaling3! Correlation rescaling factor.
 REAL                   :: len_scaling4! Correlation rescaling factor.
 REAL                   :: len_scaling5! Correlation rescaling factor.

 REAL, ALLOCATABLE      :: rf_turnconds(:) ! RF turning conditions.

 INTEGER, PARAMETER     :: max_ob_levels      = 1001    ! Maximum levels for single observation.
 INTEGER, PARAMETER     :: max_fgat_time      = 100     ! Maximum levels for FGAT.
 
 integer                :: current_ob_time
                       
 integer                :: num_gpspw_tot, num_synop_tot, num_metar_tot, &
                           num_pilot_tot, num_ssmi_rv_tot, num_ssmi_tb_tot, &
                           num_ssmi_tot,  num_ssmt1_tot, num_ssmt2_tot, &
                           num_satem_tot, num_geoamv_tot,num_polaramv_tot, num_ships_tot, &
                           num_sound_tot, num_airep_tot, num_qscat_tot, &
                           num_profiler_tot, num_buoy_tot, num_gpsref_tot, &
                           num_Radar_tot, num_bogus_tot

 integer       :: num_pseudo
 real          :: pseudo_x, pseudo_y, pseudo_z
 real          :: pseudo_val, pseudo_err
 character*3   :: pseudo_var = '   '

!------------------------------------------------------------------------------
! 2.0 Namelist parameters:
!------------------------------------------------------------------------------

 logical :: lvar4d ! select 4dvar

 CHARACTER*(8) :: MODEL_TYPE    ! Model type.
 CHARACTER*(8) :: ANALYSIS_TYPE ! Either '3D-PSAS' OR '3D-VAR'
 LOGICAL       :: write_increments ! Write out analysis increments if true.

 CHARACTER(LEN=24) :: ANALYSIS_DATE ! Analysis time, "CCYY-MM-DD_HH:MM:SS.ssss"
 INTEGER           :: ANALYSIS_ACCU ! Analysis time accuracy in seconds.

 integer       :: fg_format     ! 1 = WRF Input/Output.
                                ! 2 = MM5 Input/Output.
 integer       :: ob_format     ! 1 = WRF (BUFR) Input/Output.
                                ! 2 = MM5 (ASCII) Input/Output.
 integer       :: num_fgat_time ! number of FGAT time level.
 CHARACTER*(8) :: PROCESS_OBS   ! Observation preprocessing parameter.
 REAL          :: TIME_WINDOW   ! Assimilation time window.
 integer       :: print_detail  ! 0=Minimum, 1=More, 2=excessive!
 LOGICAL       :: Testing_3DVAR ! Adjoint, gradient tests if done if .TRUE.
 logical       :: omb_set_rand  ! Randomize O-Bs if .true.
 logical       :: put_rand_seed ! Used to initialise random numbers.
 integer       :: seed_array1, seed_array2
 real          :: set_omb_rand_fac ! Scaling factor for random O-B (*sigma_o).
 logical       :: omb_add_noise ! Add random noise to O-Bs if .true.
 INTEGER       :: obs_qc_pointer ! 3dvar uses the obs data with qc >= obs_qc_pointer
 INTEGER       :: cv_options
 integer       :: sfc_assi_options     ! Surface assimilation option.

 REAL, DIMENSION(3) :: as1, as2, as3, as4, as5

 INTEGER       :: cv_options_hum
 INTEGER       :: balance_type
 INTEGER       :: vert_corr
 INTEGER       :: vertical_ip
 INTEGER       :: vert_evalue
 REAL          :: max_vert_var1, max_vert_var2, max_vert_var3, &
                  max_vert_var4, max_vert_var5
 LOGICAL       :: test_transforms
 LOGICAL       :: test_statistics
 LOGICAL       :: print_obs_info
 LOGICAL       :: check_max_iv
 INTEGER       :: check_rh
 logical       :: interpolate_stats
 logical       :: gaussian_lats      

! switch for testing different dm parallel task configurations
 LOGICAL       :: testing_dm_bitwise_exact

!Namelist variables in future?:
 REAL, PARAMETER :: maximum_rh = 100.0
 REAL, PARAMETER :: minimum_rh =  10.0

! set to .TRUE. to select the obs type to be assimilated:

  logical       :: use_obs_errfac, &
                   use_synopObs, &                    
                   use_shipsObs, &
                   use_metarObs, &  
                   use_pilotObs, &  
                   use_soundObs, &  
                   use_bogusObs, &  
                   use_satemObs, &
                   use_GeoAMVObs, &
                   use_polaramvObs, &
                   use_airepObs, & 
                   use_gpspwObs , &
                   use_gpsrefObs, &
                   use_ssmiretrievalobs , &
                   use_ssmitbobs, &
                   use_ssmt1obs, &
                   use_ssmt2obs, &
                   use_qscatobs, &  
                   use_RadarObs, &  
                   use_Radar_rv, &  
                   use_Radar_rf, &  
                   use_profilerObs, &
                   use_buoyObs, &
                   use_Hirs2Obs, &
                   use_Hirs3Obs, &
                   use_MsuObs, &
                   use_AmsuaObs, &
                   use_AmsubObs, &
                   use_Eos_RadObs, &
                   use_Eos_AmsuaObs, &
                   use_AirsObs, &
                   use_HsbObs

   logical :: def_sub_domain
   real    :: x_start_sub_domain, x_end_sub_domain, &
              y_start_sub_domain, y_end_sub_domain

!other


 CHARACTER*80  CHEADL1
 CHARACTER*80  CHEADL2
 CHARACTER*160 CHEADL3


 INTEGER, PARAMETER :: jperr = 6

!C...NCEP ERRORS (U in m/s, V in m/s, T in K, H in %, P in Pa)
!C...RH HAS BEEN DIVIDED BY 2

 REAL, PARAMETER :: err_k(0:jperr+1) = &
                    (/200000., 100100.,70000.,50000.,30000.,10000.,5000., 1./)
 REAL, PARAMETER :: err_u(0:jperr+1) = &
                    (/ 1.4, 1.4,   2.4,   2.8,   3.4,   2.5,  2.7,  2.7/)
 REAL, PARAMETER :: err_v(0:jperr+1) = &
                    (/ 1.4, 1.4,   2.4,   2.8,   3.4,   2.5,  2.7 , 2.7 /)
 REAL, PARAMETER :: err_t(0:jperr+1) = &
                    (/ 1.8, 1.8,   1.3,   1.3,   2.0,   3.1,  4.0 , 4.0 /)
 REAL, PARAMETER :: err_rh(0:jperr+1) = &
                    (/ 10.0, 10.0,  10.0,  10.0,  10.0,  10.0, 10.0,  10.0/)
 REAL, PARAMETER :: err_p(0:jperr+1) = &
                    (/ 100.0,100.0, 100.0, 100.0, 100.0, 100.0,100.0,100.0 /)

! Maximum error check factors:  INV > (Obs_error*factor) --> fails_error_max

 LOGICAL, PARAMETER :: check_max_iv_print = .TRUE.
 
 REAL, PARAMETER :: max_error_t              = 5, &
                    max_error_uv             = 5, &
                    max_error_pw             = 5, &
                    max_error_ref            = 5, &
                    max_error_rh             = 5, &
                    max_error_q              = 5, &
                    max_error_p              = 5, &
                    max_error_tb             = 5, &
                    max_error_thickness      = 5, &
                    max_error_rv             = 5, &
                    max_error_rf             = 5, &
                    max_error_buv            = 500, &
                    max_error_bt             = 500, &
                    max_error_bq             = 500, &
                    max_error_slp            = 500

!  Define various ways for bad data to be flagged.  

   INTEGER, PARAMETER ::  &

   missing_data            = -88, &     ! Data is missing with the value of 
                                        ! missing_r
   outside_of_domain       = -77, &     ! Data outside horizontal domain 
                                        ! or time window, data set to missing_r
   wrong_direction         = -15, &     ! Wind vector direction <0 or> 360 
                                        ! => direction set to missing_r
   negative_spd            = -14, &     ! Wind vector norm is negative 
                                        ! => norm set to missing_r
   zero_spd                = -13, &     ! Wind vector norm is zero 
                                        ! => norm set to missing_r
   wrong_wind_data         = -12, &     ! Spike in wind profile 
                                        ! =>direction and norm set to missing_r 
   zero_t_td               = -11, &     ! t or td = 0 => t or td, rh and qv 
                                        ! are set to missing_r, 
   t_fail_supa_inver       = -10, &     ! superadiabatic temperature
                                        ! 
   wrong_t_sign            = - 9, &     ! Spike in Temperature profile 
                                        ! 
   above_model_lid         = - 8, &     ! heigh above model lid
                                        ! => no action
   far_below_model_surface = - 7, &     ! heigh far below model surface
                                        ! => no action
   below_model_surface     = - 6, &     ! height below model surface
                                        ! => no action
   standard_atmosphere     = - 5, &     ! Missing h, p or t
                                        ! =>Datum interpolated from standard atm
   from_background         = - 4, &     ! Missing h, p or t
                                        ! =>Datum interpolated from model
   fails_error_max         = - 3, &     ! Datum Fails error max check
                                        ! => no action
   fails_buddy_check       = - 2, &     ! Datum Fails buddy check
                                        ! => no action
   no_buddies              = - 1, &     ! Datum has no buddies
                                        ! => no action
   good_quality            =   0, &     ! OBS datum has good quality
                                        !
   convective_adjustment   =   1, &     ! convective adjustement check
                                        ! =>apply correction on t, td, rh and qv
   surface_correction      =   2, &     ! Surface datum
                                        ! => apply correction on datum
   Hydrostatic_recover     =   3, &     ! Height from hydrostaic assumption with
                                        ! the OBS data calibration
   Reference_OBS_recover   =   4, &     ! Height from reference state with OBS
                                        ! data calibration
   Other_check             =  88        ! passed other quality check

!Observations:
 INTEGER, PARAMETER     :: max_sound = 5000,   &   ! Maximum Number of radiosonde obs.
                           max_synop = 50000,  &   ! Maximum Number of surface obs.
                           max_geoamv= 300000, &   ! Maximum Number of Geo. AMV  wind obs.
                           max_polaramv= 300000, &   ! Maximum Number of Polar AMV wind obs.
                           max_airep = 200000, &   ! Maximum Number of AIREP obs.
                           max_satem = 50000,  &   ! Maximum Number of SATEM obs.
                           max_pilot = 30000,  &   ! Maximum Number of PILOT obs.
                           max_metar = 30000,  &   ! Maximum Number of METAR obs.
                           max_gpspw = 4000,   &   ! Maximum Number of GPSPW obs.
                           max_gpsref= 800000, &   ! Maximum Number of GPSRef obs.
                           max_ships = 20000,  &   ! Maximum Number of SHIP obs.
                           max_ssmi_retrieval = 30000, & ! Maximum Number of SSMI_retrieval obs.
                           max_ssmi_tb        = 30000, & ! Maximum Number of SSMI_tb obs.
                           max_ssmt1 = 30000, &    ! Maximum Number of SSM/T1 obs.
                           max_ssmt2 = 30000, &    ! Maximum Number of SSM/T2 obs.
                           max_profiler = 30000, & ! Maximum Number of profiler obs.
                           max_buoy = 30000, &     ! Maximum Number of buoy obs.
                           max_Radar = 10000, &   ! Maximum Number of Radar obs.
                           max_qscat  = 50000, &   ! Maximum Number of Quik SCAT obs.
                           max_bogus  = 5000       ! Maximum Number of bogus obs.

 integer                :: max_sound_input, &
                           max_synop_input, &
                           max_geoamv_input, &
                           max_polaramv_input, &
                           max_airep_input, &
                           max_satem_input, &
                           max_pilot_input, &
                           max_metar_input, &
                           max_gpspw_input, &
                           max_gpsref_input, &
                           max_ships_input, &
                           max_ssmi_retrieval_input, &
                           max_ssmi_tb_input, &
                           max_ssmt1_input, &
                           max_ssmt2_input, &
                           max_profiler_input, &
                           max_buoy_input,  &
                           max_Radar_input, &
                           max_Bogus_input, &
                           max_qscat_input

 integer                :: numb_procs, &         ! Number of total processors.
                           my_proc_id            ! My processor ID.
 LOGICAL                :: global 

!
! RTM_INIT setup parameter (read from namelist.3dvar)
!
 integer, parameter            :: maxsensor = 30

 integer                       :: rtminit_print,    &
                                  rtminit_nsensor

 integer, dimension(maxsensor) :: rtminit_platform, &
                                  rtminit_satid,    &
                                  rtminit_sensor

 logical                       :: use_landem      ! if use weng's landem model
 integer                       :: mw_emis_sea     ! surface emissivity type over sea
                                                  ! 0:FASTEM-2; 1:Weng's model

! type( rttov_coef ), pointer :: coefs(:)         ! RTTOV coefficients
 logical                       :: use_kma1dvar
 logical                       :: use_radiance,  use_obsgts, &
                                  lread_biascoef,lbiascorr, lrttov_scatt, lmonitoring, &
                                  lwrite_iv, lwrite_profile

 CONTAINS

#include "da_advance_cymdh.inc"
#include "DA_Array_Print.inc"
#include "da_change_date.inc"
#include "DA_Find_FFT_Factors.inc"
#include "DA_Find_FFT_Trig_Funcs.inc"
#include "DA_gamma.inc"

end module DA_Constants

