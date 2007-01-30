module da_control

   !--------------------------------------------------------------------------
   ! Purpose: Common reference point for WRFVAR control.
   !--------------------------------------------------------------------------

   use module_driver_constants, only : max_domains,max_eta, max_moves

!JRB
!#ifdef DM_PARALLEL
!   use mpi, only : mpi_real8, mpi_double_complex, mpi_sum, &
!      mpi_integer, mpi_character,mpi_maxloc,mpi_minloc, &
!      mpi_status_size,mpi_2double_precision, &
!
!      mpi_allgather, mpi_send, mpi_recv, mpi_bcast, mpi_reduce
!#endif

   implicit none

#include "namelist_defines.inc"


   !---------------------------------------------------------------------------
   ! [1.0] Physical parameter constants (all NIST standard values):
   !---------------------------------------------------------------------------

   ! Fundamental constants:
   real, parameter    :: pi = 3.1415926535897932346
   real, parameter    :: gas_constant = 287.     ! Value used in WRF.
   real, parameter    :: gas_constant_v = 461.6  ! Value used in WRF.
   real, parameter    :: cp = 7.*gas_constant/2. ! Value used in WRF.
   real, parameter    :: t_kelvin = 273.15
   real, parameter       :: ps0_inv = 1.0 / 100000.0  ! Base pressure.

   real, parameter    :: kappa = gas_constant / cp
   real, parameter    :: rd_over_rv = gas_constant / gas_constant_v
   real, parameter    :: rd_over_rv1 = 1.0 - rd_over_rv
   real, parameter    :: L_over_Rv = 5418.12

   real, parameter    :: gamma = 1.4

   ! Earth constants:
   real, parameter    :: gravity = 9.81        ! m/s - value used in MM5.
   ! real, parameter    :: earth_radius = 6378.15
   real, parameter    :: earth_radius = 6370.0          ! Be consistant with WRF
   ! real, parameter    :: earth_omega  = 2.0*pi/86400.0  ! Omega
   real, parameter    :: earth_omega  = 0.000072921     ! Omega 7.2921*10**-5

   ! Saturation Vapour Pressure Constants(Rogers & Yau, 1989) 
   real, parameter    :: es_alpha = 611.2
   real, parameter    :: es_beta = 17.67
   real, parameter    :: es_gamma = 243.5
   real, parameter    :: es_gammabeta = es_gamma * es_beta
   real, parameter    :: es_gammakelvin = es_gamma - t_kelvin

   ! Explicit moist constants:
   real, parameter    :: SVP1=0.6112, SVP2=17.67, SVP3=29.65
   real, parameter    :: SVPT0=273.15, TO=273.15
   real, parameter    :: N0R=8.E6, N0S=2.E7, RHOS=0.1
   real, parameter    :: AVT=841.99667, BVT=0.8, BVT2=2.5+.5*BVT, BVT3=3.+BVT
   real, parameter    :: PPI=1./(pi*N0R), PPIS=1./(pi*N0S*RHOS)
   real, parameter    :: XLV1=2370., XLF0=.3337E6, XLV0=3.15E6
   real, parameter    :: XLS=XLV0-XLV1*273.16+XLF0

   ! Planetary boundary physics constants
   real, parameter         :: k_kar = 0.4    ! Von Karman constant

   ! GPS Refractivity constant  
   real, parameter    :: coeff = 3.73e5 / 77.6

#if RWORDSIZE==8
   real, parameter :: da_zero = 0D0
#else
   real, parameter :: da_zero = 0.0
#endif

complex, parameter :: da_zero_complex = (da_zero,da_zero)
   
   !---------------------------------------------------------------------------
   ! [2.0] WRF-Var parameter constants:
   !---------------------------------------------------------------------------

   ! Missing values and the index number of the quality contro

   integer, parameter ::  missing       = -888888
   real   , parameter ::  missing_r     = -888888.
   real   , parameter ::  Max_StHeight_Diff = 100.

   logical :: anal_type_verify=.false.
   logical :: anal_type_randomcv=.false.
   logical :: anal_type_qcobs=.false.

   integer,parameter :: qc_good = 1
   integer,parameter :: qc_bad  = -1

   integer, parameter :: bufr_satellite_id   = 1
   integer, parameter :: bufr_ifov           = 2
   integer, parameter :: bufr_year           = 3
   integer, parameter :: bufr_month          = 4
   integer, parameter :: bufr_day            = 5
   integer, parameter :: bufr_hour           = 6
   integer, parameter :: bufr_minute         = 7
   integer, parameter :: bufr_second         = 8
   integer, parameter :: bufr_lat            = 9
   integer, parameter :: bufr_lon            = 10
   integer, parameter :: bufr_satzen         = 11
   integer, parameter :: bufr_solzen         = 12
   integer, parameter :: bufr_station_height = 13
   integer, parameter :: bufr_landsea_mask   = 14

   integer, parameter :: nchan_amsua = 15
   integer, parameter :: nchan_msu = 4
   integer, parameter :: nchan_hirs2 = 19
   integer, parameter :: nchan_hirs3 = 19

   ! WRFVAR Minimisation:

   integer            :: iter
   integer, parameter :: MP = 6
   integer, parameter :: LP = 6
   integer, parameter :: MAXFEV = 10
   real, parameter    :: FTOL = 1.0E-4
   real, parameter    :: GTOL = 0.9
   real, parameter    :: XTOL = 1.0E-17
   real, parameter    :: STPMin = 1.0E-20
   real, parameter    :: STPMAX = 1.0E+20

   ! Background errors:
   real, parameter    :: pplow = 1.0e-8       ! Machine lowest number?
   real, parameter    :: pp_umin = 1.0e-2     ! Minimum u back. error (m/s).
   real, parameter    :: pp_vmin = 1.0e-2     ! Minimum v back. error (m/s).
   real, parameter    :: pp_tmin = 1.0e-2     ! Minimum t back. error (K).
   real, parameter    :: pp_qmin = 1.0e-6     ! Minimum q back. error (kg/kg)
   real, parameter    :: pp_pmin= 1.0e+1      ! Minimum pp back. error (Pa).

   ! FFTs:
   integer, parameter :: Forward_FFT     = -1 ! Grid to spectral
   integer, parameter :: Inverse_FFT     =  1 ! Spectral to grid.
   integer, parameter :: num_fft_factors = 10 ! Max number of factors.
 
   ! Balance:
   integer, parameter :: balance_geo = 1      ! Geostrophic balance.
   integer, parameter :: balance_cyc = 2      ! Cyclostrophic balance.
   integer, parameter :: balance_geocyc = 3   ! Geo/cyclostrophic balance.

   ! Adjoint tests:
   real, parameter    :: typical_u_rms = 2.0     ! m/s
   real, parameter    :: typical_v_rms = 2.0     ! m/s
   real, parameter    :: typical_speed_rms = 2.0 ! m/s
   real, parameter    :: typical_tb19v_rms = 1.0 ! K
   real, parameter    :: typical_tb19h_rms = 1.0 ! K
   real, parameter    :: typical_tb22v_rms = 1.0 ! K
   real, parameter    :: typical_tb37v_rms = 1.0 ! K
   real, parameter    :: typical_tb37h_rms = 1.0 ! K
   real, parameter    :: typical_tb85v_rms = 1.0 ! K
   real, parameter    :: typical_tb85h_rms = 1.0 ! K
   real, parameter    :: typical_t_rms = 1.0     ! K
   real, parameter    :: typical_p_rms = 100.0   ! Pa
   real, parameter    :: typical_q_rms = 0.00001 ! g/kg
   real, parameter    :: typical_rho_rms = 0.01  ! kg/m^3
   real, parameter    :: typical_tpw_rms = 0.2   ! cm
   real, parameter    :: typical_ref_rms = 5.0   ! N unit
   real, parameter    :: typical_rh_rms = 20.0   ! %
   real, parameter    :: typical_thickness_rms = 50.0   ! m
   real, parameter    :: typical_qrn_rms = 0.00001 ! g/kg
   real, parameter    :: typical_qcw_rms = 0.00001 ! g/kg
   real, parameter    :: typical_w_rms = 0.1     ! m/s
   real, parameter    :: typical_rv_rms = 1.0    ! m/s
   real, parameter    :: typical_rf_rms = 1.0    ! dBZ

   ! The following typical mean squared values depend on control variable. They   
   ! are calculated in da_setup_background_errors and used in the VvToVp adjoint 
   ! test:

   real, parameter    :: inv_typ_vp1_sumsq = 0.00001 ! 1/sum(psi**2)
   real, parameter    :: inv_typ_vp2_sumsq = 0.00001 ! 1/sum(chi**2)
   real, parameter    :: inv_typ_vp3_sumsq = 0.00001 ! 1/sum(phi_u**2)
   real, parameter    :: inv_typ_vp4_sumsq = 10000.0 ! 1/sum(q**2)
   real, parameter    :: inv_typ_vp5_sumsq = 0.00001 ! 1/sum(?**2)
   real, parameter    :: inv_typ_vpalpha_sumsq = 1.0 ! 1/sum(?**2)

   character(len=*),parameter :: wrfvar_version = "WRFVAR V2.2"
   character(len=*),parameter :: wrf_version    = "WRF V2.2"

   integer, parameter :: fg_format_wrf = 1
   integer, parameter :: fg_format_kma = 3

   integer, parameter :: ob_format_bufr = 1
   integer, parameter :: ob_format_ascii = 2

   integer, parameter :: convert_fd2uv = 1
   integer, parameter :: convert_uv2fd = -1

   ! Fortran unit  parameters:

   ! stdout, stderr, trace_unit all controlled from namelist

   ! Units 9,10 are used for reading and writing namelist.input/output in WRF

   ! Do not use get_unit/free_unit because tracing is too low level
   integer, parameter :: trace_csv_unit = 8

   integer :: y_unit, yp_unit, cost_unit, grad_unit, stats_unit, jo_unit
   integer :: check_max_iv_unit, rand_unit, omb_unit, filtered_obs_unit
   integer :: biasprep_unit

   integer,parameter :: filename_len = 200

   integer, parameter :: num_alpha_corr_types = 3
   integer, parameter :: num_sound_diag = 4 

   integer :: alpha_corr_unit1(num_alpha_corr_types)
   integer :: alpha_corr_unit2(num_alpha_corr_types)
   integer :: sound_diag_unit(num_sound_diag)

   integer, parameter :: max_num_of_var = 200 ! Maximum # of stored fields.

   integer, parameter :: unit_start = 20
   integer, parameter :: unit_end = 500
   logical :: unit_used(unit_start:unit_end) = .false.


   !---------------------------------------------------------------------------
   ! [3.0] Variables used in MM5 part of code:
   !---------------------------------------------------------------------------

   integer            :: map_projection       ! 1=LamConf/2=PolarSte/3=Mercator
   real               :: ycntr
   integer            :: coarse_ix            ! COARSE DOMAin DIM in I DIRECTION.
   integer            :: coarse_jy            ! COARSE DOMAin DIM in Y DIRECTION.
   real               :: coarse_ds            ! Coarse domain gridlength (km)
   real               :: start_x              ! i posn. of (1,1) in coarse domain.
   real               :: start_y              ! j posn. of (1,1) in coarse domain.
   real               :: start_lat            ! Latitude coresponds to start_(x,y)
   real               :: start_lon            ! Longitude coresponds to start_(x,y)
   real               :: delt_lat             ! Latitude increments for global grids
   real               :: delt_lon             ! Longitude increments for global grids

   real               :: phic                 ! COARSE DOMAin CENTRAL LAT(DEGREE)
   real               :: xlonc                ! COARSE DOMAin CENTRAL LON(DEGREE)
   real               :: cone_factor          ! Cone Factor
   real               :: truelat1_3dv         ! True latitude 1 (degrees)
   real               :: truelat2_3dv         ! True latitude 2 (degrees)
   real               :: pole                 ! Pole latitude (degrees)
   real               :: dsm                  ! Current domain gridlength (km)
   real               :: psi1                 ! ?
   real               :: c2                   ! earth_radius * COS(psi1)

   real               :: ptop
   real               :: ps0
   real               :: ts0 = 300.0          ! Base potential temperture
                                              ! mm5 code may try to change value
   real               :: tlp
   real               :: tis0

   !------------------------------------------------------------------------------
   ! 4.0 vertical interpolation options
   !------------------------------------------------------------------------------

   integer, parameter :: v_interp_not_specified = missing, &
                         v_interp_p             = 1, &
                         v_interp_h             = 2

   !------------------------------------------------------------------------------
   ! WRFVAR scalar constants:
   !------------------------------------------------------------------------------

   integer                :: Anal_Space  ! Space of analysis
                                         ! ( 1 = Full model,
                                         !   2 = Transformed grid,
                                         !   3 = Ob space (PSAS) )

   integer                :: mix         ! 1st dimension of analysis grid.
   integer                :: mjy         ! 2nd dimension of analysis grid.
   integer                :: mkz         ! 3rd dimension of analysis grid.

   ! Recursive filter:

   real, allocatable      :: rf_turnconds(:) ! RF turning conditions.

   integer, parameter     :: max_ob_levels = 1001 ! Maximum levels for single ob
   integer, parameter     :: max_fgat_time = 100  ! Maximum levels for FGAT.

   integer                :: current_ob_time

   integer                :: num_gpspw_tot, num_synop_tot, num_metar_tot, &
                             num_pilot_tot, num_ssmi_rv_tot, num_ssmi_tb_tot, &
                             num_ssmi_tot,  num_ssmt1_tot, num_ssmt2_tot, &
                             num_satem_tot, num_geoamv_tot,num_polaramv_tot, &
                             num_ships_tot, &
                             num_sound_tot, num_airep_tot, num_qscat_tot, &
                             num_profiler_tot, num_buoy_tot, num_gpsref_tot, &
                             num_Radar_tot, num_bogus_tot,num_airsr_tot, &
                             num_radiance_tot

   logical       :: gaussian_lats  


   integer       :: cv_size_domain_jb    ! Total jb cv size.
   integer       :: cv_size_domain_je    ! Total je cv size.
   integer       :: cv_size_domain       ! Total cv size.    



   ! Namelist variables in future?:
   real, parameter :: maximum_rh = 100.0
   real, parameter :: minimum_rh =  10.0

   ! other

   character*80  cheadl1
   character*80  cheadl2
   character*160 cheadl3


   integer, parameter :: jperr = 6

   ! NCEP errors (U in m/s, V in m/s, T in K, H in %, P in Pa)
   ! rh has been divided by 2

   real, parameter :: err_k(0:jperr+1) = &
                      (/200000., 100100.,70000.,50000.,30000.,10000.,5000., 1./)
   real, parameter :: err_u(0:jperr+1) = &
                      (/ 1.4, 1.4,   2.4,   2.8,   3.4,   2.5,  2.7,  2.7/)
   real, parameter :: err_v(0:jperr+1) = &
                      (/ 1.4, 1.4,   2.4,   2.8,   3.4,   2.5,  2.7 , 2.7 /)
   real, parameter :: err_t(0:jperr+1) = &
                      (/ 1.8, 1.8,   1.3,   1.3,   2.0,   3.1,  4.0 , 4.0 /)
   real, parameter :: err_rh(0:jperr+1) = &
                      (/ 10.0, 10.0,  10.0,  10.0,  10.0,  10.0, 10.0,  10.0/)
   real, parameter :: err_p(0:jperr+1) = &
                      (/ 100.0,100.0, 100.0, 100.0, 100.0, 100.0,100.0,100.0 /)

   ! Maximum error check factors:  inV > (Obs_error*factor) --> fails_error_max

   real, parameter :: max_error_t              = 5, &
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

   ! Define various ways for bad data to be flagged.  

   integer, parameter ::  &
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

   ! Observations:

   integer, parameter     :: max_Radar = 10000    ! Maximum Number of Radar obs.

   integer                :: num_procs            ! Number of total processors.
   integer                :: myproc               ! My processor ID.
   integer, parameter     :: root = 0             ! Number of root processor
   logical                :: rootproc             ! Am I the root processor

   integer, parameter :: var4d_coupling_disk_linear = 1
   integer, parameter :: var4d_coupling_disk_simul  = 2

   integer, parameter :: rtm_option_rttov = 1
   integer, parameter :: rtm_option_crtm = 2

   ! RTM_inIT setup parameter

   integer, parameter            :: maxsensor = 30

   ! type( rttov_coef ), pointer :: coefs(:)         ! RTTOV coefficients

   ! Tracing

   integer :: trace_start_points=0   ! Number of routines to initiate trace

   integer, parameter :: num_ob_indexes = 23

   integer, parameter :: sound_index          = 1
   integer, parameter :: synop_index          = 2
   integer, parameter :: pilot_index          = 3
   integer, parameter :: satem_index          = 4
   integer, parameter :: geoamv_index         = 5
   integer, parameter :: polaramv_index       = 6
   integer, parameter :: airep_index          = 7
   integer, parameter :: gpspw_index          = 8
   integer, parameter :: gpsref_index         = 9
   integer, parameter :: metar_index          = 10
   integer, parameter :: ships_index          = 11
   integer, parameter :: ssmi_retrieval_index = 12
   integer, parameter :: ssmi_tb_index        = 13
   integer, parameter :: ssmt1_index          = 14
   integer, parameter :: ssmt2_index          = 15
   integer, parameter :: qscat_index          = 16
   integer, parameter :: profiler_index       = 17
   integer, parameter :: buoy_index           = 18
   integer, parameter :: bogus_index          = 19
   integer, parameter :: pseudo_index         = 20
   integer, parameter :: radar_index          = 21
   integer, parameter :: radiance_index       = 22
   integer, parameter :: airsr_index          = 23

   character(len=14), parameter :: obs_names(num_ob_indexes) = (/ &
      "SOUND         ", &
      "SYNOP         ", &
      "PILOT         ", &
      "SATEM         ", &
      "Geo AMV       ", &
      "Polar AMV     ", &
      "AIREP         ", &
      "GPSPW         ", &
      "GPSRF         ", &
      "METAR         ", &
      "SHIP          ", &
      "SSMI_RETRIEVAL", &
      "SSMI_TB       ", &
      "SSMT1         ", &
      "SSMT2         ", &
      "QSCAT         ", &
      "Profiler      ", &
      "Buoy          ", &
      "Bogus         ", &
      "Pseudo        ", &
      "Radar         ", &
      "Radiance      ", &
      "AIRS retrieval"  &
   /)

   integer, parameter :: max_no_fm = 290

   integer, parameter :: num_ob_vars=9

   logical, parameter :: in_report(num_ob_vars,2) = reshape((/&
     .false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false., & ! sound
     .true.,.true.,.true.,.true.,.true.,.true.,.false.,.false.,.false./), &
     (/num_ob_vars,2/))

   integer, parameter :: report_h   = 1
   integer, parameter :: report_u   = 2
   integer, parameter :: report_v   = 3
   integer, parameter :: report_t   = 4
   integer, parameter :: report_q   = 5
   integer, parameter :: report_p   = 6
   integer, parameter :: report_rh  = 7
   integer, parameter :: report_slp = 8
   integer, parameter :: report_zk  = 9

   logical :: obs_use(num_ob_indexes) = .false.



   ! Special cases

   integer, parameter :: fm_satem = 86
   integer, parameter :: fm_amv   = 88

   integer, parameter :: fm_index(max_no_fm) = (/ &
      0,0,0,0,0,0,0,0,0,0,                                & ! 1-10
      0,Synop_index,Ships_index,0,Metar_index,            & ! 11-15
      Metar_index,Ships_index,buoy_index,buoy_index,0,    & ! 16-20
      0,0,0,0,0,0,0,0,0,0,                                & ! 21-30
      0,pilot_index,pilot_index,pilot_index,sound_index,  & ! 31-35
      sound_index,sound_index,sound_index,0,0,            & ! 36-40
      0,airep_index,0,0,0,0,0,0,0,0,                      & ! 41-50
      0,0,0,0,0,0,0,0,0,0,                                & ! 51-60
      0,0,0,0,0,0,0,0,0,0,                                & ! 61-70
      0,0,0,0,0,0,0,0,0,0,                                & ! 71-80
      0,0,0,0,0,satem_index,0,geoamv_index,0,0,           & ! 81-90
      0,0,0,0,0,airep_index,airep_index,0,0,0,            & ! 91-100
      0,0,0,0,0,0,0,0,0,0,                                & ! 101-110
      gpspw_index,0,0,gpspw_index,0,gpsref_index,0,0,0,0, & ! 111-120
      ssmt1_index,ssmt2_index,0,0,0,0,0,0,0,0,            & ! 121-130
      0,profiler_index,airsr_index,0,bogus_index,0,0,0,0,0, & ! 131-140
      0,0,0,0,0,0,0,0,0,0,                                & ! 141-150
      0,0,0,0,0,0,0,0,0,0,                                & ! 151-160
      0,0,0,0,0,0,0,0,0,0,                                & ! 161-170
      0,0,0,0,0,0,0,0,0,0,                                & ! 171-180
      0,0,0,0,0,0,0,0,0,0,                                & ! 181-190
      0,0,0,0,0,0,0,0,0,0,                                & ! 191-200
      0,0,0,0,0,0,0,0,0,0,                                & ! 201-210
      0,0,0,0,0,0,0,0,0,0,                                & ! 211-220
      0,0,0,0,0,0,0,0,0,0,                                & ! 231-230
      0,0,0,0,0,0,0,0,0,0,                                & ! 231-240
      0,0,0,0,0,0,0,0,0,0,                                & ! 241-250
      0,0,0,0,0,0,0,0,0,0,                                & ! 251-260
      0,0,0,0,0,0,0,0,0,0,                                & ! 261-270
      0,0,0,0,0,0,0,0,0,0,                                & ! 271-280
      qscat_index,0,0,0,0,0,0,0,0,0 /)                      ! 281-290

   character(len=120)  :: fmt_info ='(a12,1x,a19,1x,a40,1x,i6,3(f12.3,11x),6x,a5)'
   character(len=120)  :: fmt_srfc = '(7(:,f12.3,i4,f7.2))'
   character(len=120)  :: fmt_each = &
      '(3(f12.3,i4,f7.2),11x,3(f12.3,i4,f7.2),11x,1(f12.3,i4,f7.2))'

   ! lat/long information calculated in da_setup_firstguess_wrf

   real, parameter :: deg_to_rad = pi/180.0
  
   real, allocatable :: cos_xls(:)
   real, allocatable :: sin_xls(:)
   real, allocatable :: cos_xle(:)
   real, allocatable :: sin_xle(:)

   integer :: ierr ! General error code
   integer :: comm ! MPI communicator

   integer :: ids,ide,jds,jde,kds,kde
   integer :: ims,ime,jms,jme,kms,kme
   integer :: its,ite,jts,jte,kts,kte
   integer :: ips,ipe,jps,jpe,kps,kpe

contains

#include "da_advance_cymdh.inc"
#include "da_array_print.inc"
#include "da_change_date.inc"
#include "da_find_fft_factors.inc"
#include "da_find_fft_trig_funcs.inc"

end module da_control
