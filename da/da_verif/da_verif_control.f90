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
   IMPLICIT NONE
!
  INTEGER, parameter       :: maxnum = 10, nstd = 16
  INTEGER, parameter       :: num_verif_var =5        
  real,    dimension(nstd) :: stdp
  real                     :: rmiss = -99.99
  integer                  :: num_miss = -99
!
  data stdp/1000., 925., 850., 700., 500., 400., 300., &
             250., 200., 150., 100.,  70.,  50. ,30., 20. ,10./ 
!
  character (len=1)   :: verif_var(num_verif_var)
  character (len= 2)  :: verif_type(2)
  data verif_var/'U','V','T','Q','P'/
  data verif_type/'OI','AO'/

!
  TYPE stats_value
    INTEGER     :: num
    REAL        :: abias
    REAL        :: bias
    REAL        :: rmse
  END TYPE stats_value

  TYPE surface_type
    TYPE (stats_value) :: uomb, uoma
    TYPE (stats_value) :: vomb, voma
    TYPE (stats_value) :: tomb, toma
    TYPE (stats_value) :: pomb, poma
    TYPE (stats_value) :: qomb, qoma
  END TYPE surface_type

  TYPE upr_type
    TYPE (stats_value) :: uomb(nstd), uoma(nstd)
    TYPE (stats_value) :: vomb(nstd), voma(nstd)
    TYPE (stats_value) :: tomb(nstd), toma(nstd)
    TYPE (stats_value) :: qomb(nstd), qoma(nstd)
  END TYPE upr_type

  TYPE gpspw_type
    TYPE (stats_value)          :: tpwomb, tpwoma         
  END TYPE gpspw_type

  TYPE gpsref_type
    TYPE (stats_value)          :: refomb, refoma         
  END TYPE gpsref_type

! namelist.varstats variables
!
  NAMELIST /Record1/ exp_num, exp_dirs, out_dirs
  NAMELIST /Record2/ start_date, end_date, interval
  NAMELIST /Record3/ if_plot_rmse, if_plot_bias, if_plot_abias
  NAMELIST /Record4/ if_plot_synop, if_plot_sonde_sfc,if_plot_metar, &
                     if_plot_ships, if_plot_buoy , if_plot_qscat, &
                     if_plot_sound, if_plot_airep, if_plot_pilot, if_plot_profiler, &
                     if_plot_geoamv, if_plot_polaramv, if_plot_gpspw, if_plot_gpsref, &
                     if_plot_airsret
  NAMELIST /Record5/ file_path_string

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
!
! Namelist declaration over

  integer      :: nml_unit, diag_unit_in, diag_unit_out, info_unit
end MODULE da_verif_control
