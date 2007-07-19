MODULE da_verif_control
!----------------------------------------------------------------------------   
! History:
!
!  Abstract:  
!   Main module for 
!   defining and initializing various data type, 
!   defining unit numbers and cosnstants
!
!  Author:   Syed RH Rizvi     NCAR/MMM         05/25/2006
!----------------------------------------------------------------------------   
   implicit none

  integer, parameter       :: maxnum = 10, nstd = 16
  integer, parameter       :: num_verif_var =5        
  real,    dimension(nstd) :: stdp
  real                     :: rmiss = -99.99
  integer                  :: num_miss = -99

  data stdp/1000.0, 925.0, 850.0, 700.0, 500.0, 400.0, 300.0, &
             250.0, 200.0, 150.0, 100.0,  70.0,  50.0 ,30.0, 20.0 ,10.0/ 

  character (len=1)   :: verif_var(num_verif_var)
  character (len= 2)  :: verif_type(2)
  data verif_var/'U','V','T','Q','P'/
  data verif_type/'OI','AO'/

  type stats_value
    integer     :: num
    real        :: abias
    real        :: bias
    real        :: rmse
  end type stats_value

  type surface_type
    type (stats_value) :: uomb, uoma
    type (stats_value) :: vomb, voma
    type (stats_value) :: tomb, toma
    type (stats_value) :: pomb, poma
    type (stats_value) :: qomb, qoma
  end type surface_type

  type upr_type
    type (stats_value) :: uomb(nstd), uoma(nstd)
    type (stats_value) :: vomb(nstd), voma(nstd)
    type (stats_value) :: tomb(nstd), toma(nstd)
    type (stats_value) :: qomb(nstd), qoma(nstd)
  end type upr_type

  type gpspw_type
    type (stats_value)          :: tpwomb, tpwoma         
  end type gpspw_type

  type gpsref_type
    type (stats_value)          :: refomb, refoma         
  end type gpsref_type

! namelist.varstats variables

! record1
  INTEGER                               :: exp_num   ! number of experiments
  CHARACTER(LEN=512),DIMENSION(maxnum)  :: exp_dirs, out_dirs   
! record2
  CHARACTER(LEN=10)      :: start_date
  CHARACTER(LEN=10)      :: end_date
  INTEGER                :: interval       ! interval(h) between initial times
!                                          ! Typically 6 or 12 hours

! record3

  LOGICAL  :: if_plot_rmse
  LOGICAL  :: if_plot_bias
  LOGICAL  :: if_plot_abias

! record4
  LOGICAL  :: if_plot_surface

  LOGICAL  :: if_plot_synop  
  LOGICAL  :: if_plot_metar  
  LOGICAL  :: if_plot_ships  
  LOGICAL  :: if_plot_buoy   
  LOGICAL  :: if_plot_sonde_sfc
  LOGICAL  :: if_plot_qscat

  LOGICAL  :: if_plot_upr   
!
  LOGICAL  :: if_plot_sound
  LOGICAL  :: if_plot_airep
  LOGICAL  :: if_plot_pilot 
  LOGICAL  :: if_plot_profiler
  LOGICAL  :: if_plot_polaramv 
  LOGICAL  :: if_plot_geoamv   

  LOGICAL  :: if_plot_gpspw 
  LOGICAL  :: if_plot_gpsref
  LOGICAL  :: if_plot_airsret 
! record5
  character (len=50)    :: file_path_string

  NAMELIST /Record1/ exp_num, exp_dirs, out_dirs
  NAMELIST /Record2/ start_date, end_date, interval
  NAMELIST /Record3/ if_plot_rmse, if_plot_bias, if_plot_abias
  NAMELIST /Record4/ if_plot_synop, if_plot_sonde_sfc,if_plot_metar, &
                     if_plot_ships, if_plot_buoy , if_plot_qscat, &
                     if_plot_sound, if_plot_airep, if_plot_pilot, if_plot_profiler, &
                     if_plot_geoamv, if_plot_polaramv, if_plot_gpspw, if_plot_gpsref, &
                     if_plot_airsret
  NAMELIST /Record5/ file_path_string
!
! Namelist declaration over

  integer      :: nml_unit, diag_unit_in, diag_unit_out, info_unit
end MODULE da_verif_control
