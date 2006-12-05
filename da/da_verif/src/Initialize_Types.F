MODULE Define_Constants
!----------------------------------------------------------------------------   
! History:
!
!  Abstract:  
!   Main module for defining various obs_type, unit numbers and cosnstants
!
!  Author:   Syed RH Rizvi     NCAR/MMM         05/25/2006
!----------------------------------------------------------------------------   
   IMPLICIT NONE
!
  INTEGER, parameter       :: nstd = 16
  INTEGER, parameter       :: num_verif_var =5        
  real,    dimension(nstd) :: stdp
!
  data stdp/1000., 925., 850., 700., 500., 400., 300., &
             250., 200., 150., 100.,  70., 50., 30., 20., 10./ 
!
  character (len=10)  :: date
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
    TYPE (stats_value) :: vomb, uoma
    TYPE (stats_value) :: tomb, uoma
    TYPE (stats_value) :: pomb, uoma
    TYPE (stats_value) :: qomb, uoma
  END TYPE surface_type

  TYPE upr_type  
    TYPE (stats_value), :: uomb (nstd), uoma(nstd)
    TYPE (stats_value), :: vomb (nstd), voma(nstd)
    TYPE (stats_value), :: tomb (nstd), toma(nstd)
    TYPE (stats_value), :: pomb (nstd), poma(nstd)
    TYPE (stats_value), :: qomb (nstd), qoma(nstd)
  END TYPE upr_type


  TYPE gpspw_type
    TYPE (stats_value)          :: tpwomb, tpwoma         
  END TYPE gpspw_type

  TYPE gpsref_type
    TYPE (stats_value)          :: refomb, refoma         
  END TYPE gpsref_type

! namelist.var_stats variables
!
! record1
! INTEGER                               :: exp_num   ! number of experiments
  CHARACTER(LEN=12), DIMENSION(maxnum)  :: exp_names
! record2
  CHARACTER(LEN=10)      :: start_date
  CHARACTER(LEN=10)      :: end_date
  INTEGER                :: init_interval  ! interval(h) between initial times

  LOGICAL  :: if_plot_rmse
  LOGICAL  :: if_plot_bias
  LOGICAL  :: if_plot_abias

! record4
  LOGICAL  :: if_plot_surface
!
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

   integer      :: diag_unit, ounit, info_unit
END MODULE Define_Constants
