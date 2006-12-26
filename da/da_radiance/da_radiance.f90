module da_radiance

!------------------------------------------------------------------------------
!  Purpose: module for radiance data assimilation.
!
!  METHOD:  radiance for all sensor are defined in a common data structure 
!
!  HISTORY: 07/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
!   use da_obs
   use da_par_util
   USE da_reporting

#ifdef RTTOV
   use rttov_const,  only : &
            errorstatus_success, &
            platform_name      , &
            inst_name          , &
            gas_id_watervapour  ,&
            gas_unit_specconc   ,&
            gas_unit_ppmv, sensor_id_mw
   use rttov_types
#endif
   use gsi_kinds      ,  only : r_kind,r_double,i_kind,r_single
   use gsi_constants  ,  only : deg2rad, rad2deg,       &
                            init_constants_derived, &
                            one, three, zero, half, &
                            one_tenth, two, four

!   use irsse_model, only: forward_irsse
   implicit none
   
   real, parameter             :: q2ppmv = 1.60771704e+6
   integer                     :: n_scatt_coef
   character(len=5), pointer   :: coefs_scatt_instname(:)
   real,             pointer   :: time_slots(:)
#ifdef RTTOV
   TYPE( rttov_coef ), pointer :: coefs(:)         ! RTTOV8_5 coefficients
   TYPE( rttov_scatt_coef ), pointer :: coefs_scatt(:)
#endif

   TYPE datalink
     type (rad_type)           :: rad
     type (residual_rad_type)  :: rad_ob
     integer                   :: sensor_index,ifgat
     type (datalink), pointer  :: next ! pointer to next data
   END TYPE datalink

   TYPE con_vars_type
     integer            ::  nlevels
     real   ,  pointer  ::  t(:)
     real   ,  pointer  ::  q(:)
     real               ::  ps
   END TYPE con_vars_type

   TYPE con_cld_vars_type
     integer            ::  nwp_levels
     real   ,  pointer  ::  p(:)
     real   ,  pointer  ::  ph(:)
     real   ,  pointer  ::  t(:)
     real   ,  pointer  ::  cc(:)
     real   ,  pointer  ::  clw(:)   ! kg/kg
     real   ,  pointer  ::  ciw(:)   ! kg/kg
     real   ,  pointer  ::  rain(:)  ! kg/m2/s
     real   ,  pointer  ::  sp(:)    ! kg/m2/s
   END TYPE con_cld_vars_type

   TYPE aux_vars_type
     integer            ::  surftype
     real               ::  surft, t2m, q2m, u10, v10
     real               ::  satzen, satazi  !!, fastem(5)
   END TYPE aux_vars_type

   TYPE maxmin_rad_stats_type
        TYPE (maxmin_type)         :: maximum, minimum
        REAL                       :: ave, rms
        INTEGER                    :: num
   END TYPE maxmin_rad_stats_type

   TYPE stats_rad_type
        TYPE (maxmin_rad_stats_type), pointer  :: ichan(:)
   END TYPE stats_rad_type

   TYPE rad_header_type                       ! innovation file header
      CHARACTER (LEN = 19) :: date_char       ! YYYY-MM-DD-HH (assimilation window date)
      INTEGER              :: assim_win       ! assimilation window hours, e.g., 6 hours
      CHARACTER(LEN=20)    :: rttovid_string  ! e.g., noaa-16-amsua 
      INTEGER              :: platform_id     ! e.g., 1  for noaa (see RTTOV UG)
      INTEGER              :: satellite_id    ! e.g., 16 for noaa-16
      INTEGER              :: sensor_id       ! e.g., 3  for amsua
      INTEGER              :: num_rad         ! pixel number in file
      INTEGER              :: nchan           ! channel number of each pixel
      INTEGER ,  pointer   :: ichan(:)        ! index of nchan channels
      INTEGER              :: nemis           ! emissivity number of each pixel
                                              ! may be different with nchan
                                              ! due to polarisation in microwave
      INTEGER              :: nlevel_fix      ! fixed pressure level number for RTM 
                                              ! e.g., 43 for RTTOV8_5 
      REAL   ,   pointer   :: pres(:)         ! pressure with nlevel_fix
      INTEGER              :: nlevel_cld      ! cloud profile level number for RTM
   END TYPE rad_header_type

   TYPE rad_data_type                       ! innovation file pixel data

!  part from Observation
!---------------------------
        integer            :: landmask      ! 1:land; 0:sea
        integer            :: scanline      ! number of scan line
        integer            :: scanpos       ! number of scan position
        REAL               :: lat           ! Latitude in degree
        REAL               :: lon           ! Longitude in degree
        REAL               :: elv           ! Elevation in m
        real               :: satzen        ! satellite zenith angle in degree
        real               :: satazi        ! satellite azimuth angle in degree
        real               :: solzen        ! solar zenith angle in degree
        real               :: solazi        ! solar azimuth angle in degree
        real,    pointer   :: tb(:)         ! observed brightness temperatures in Kelvin
        real,    pointer   :: inv(:)        ! innovation (obs - background) in Kelvin
        real,    pointer   :: bias(:)       ! bias correction values in Kelvin
        real,    pointer   :: err(:)        ! std of observation error in Kelvin
        real,    pointer   :: qc(:)         ! quality control flag
                                            ! 0:good; <0:rejected; other:suspected
        real,    pointer   :: emiss(:)      ! surface emissivity

!  part from background field
!-------------------------------
        integer            :: surftype      ! surface type
                                            ! 0:sea     1:sea-ice     2:land     3:snow
                                            ! 4:mix-sea 5:mix-sea-ice 6:mix-land 7:mix-snow
        integer            :: terrain       ! model terrain in m
        integer            :: soiltyp       ! soil type (MM5/WRF USGS 24 catagories)
        integer            :: vegtyp        ! vegetation type (MM5/WRF 16 catagories)
        real               :: vegfra        ! vegetation fraction
        real               :: soilm         ! soil moisture
        real               :: soilt         ! soil temperature
        real               :: snowh         ! snow depth
        real               :: ps            ! surface pressure in hPa
        real               :: ts            ! surface skin temperature in Kelvin
        real               :: t2m           ! T in Kelvin at 2m 
        real               :: mr2m          ! volume mixture ratio in ppmv at 2m
        real               :: u10,v10       ! u/v wind in m/s at 10m
        real,    pointer   :: t(:)          ! temperatures at fixed pressure levels
        real,    pointer   :: mr(:)         ! volume mixture ratio in ppmv at fixed pressure levels
        real,    pointer   :: zk(:)         ! vertical interpolation weight from model level to fixed pressure levels
        real,    pointer   :: pm(:)         ! full-level pressure at model levels
        real,    pointer   :: phm(:)        ! half-level pressure at model levels
        real,    pointer   :: tm(:)         ! temperatures at model levels
        real,    pointer   :: cc(:)         ! cloud cover at model levels
        real,    pointer   :: rain(:)       ! rainfall rate in kg/m2/s
        real,    pointer   :: solidp(:)     ! solid precipitation rate in kg/m2/s
        real,    pointer   :: clw(:)        ! cloud liquid water (kg/kg)
        real,    pointer   :: ciw(:)        ! cloud ice water    (kg/kg)

   END TYPE rad_data_type

   TYPE satinfo_type
        integer, pointer   :: ichan(:)      ! channel index
        integer, pointer   :: iuse (:)      ! usage flag (-1: not use)
        real   , pointer   :: error(:)      ! error Standard Deviation
        real   , pointer   :: polar(:)      ! polarisation (0:vertical; 1:horizontal)
        real   , pointer   :: rms(:,:)      ! rms of bias corr file
        real   , pointer   :: std(:,:)      ! std of bias corr file
        real   , pointer   :: a(:,:)        ! bias corr coef a Tb*(xb)=a+b*Tb(xb)
        real   , pointer   :: b(:,:)        ! bias corr coef b
        real   , pointer   :: error_factor(:) ! error tuning factor
   END TYPE satinfo_type
   
   TYPE (satinfo_type), pointer :: satinfo(:)

   TYPE BIAS
      INTEGER :: nchan     ! number of channels
      INTEGER :: npred     ! number of predictors
      INTEGER :: platform_id,satellite_id,sensor_id
      INTEGER :: year, month, day, hour, min, sec
      INTEGER :: scanline,scanpos
      INTEGER :: landmask
      INTEGER, pointer :: qc_flag(:) ! 1/0:good/bad
      INTEGER, pointer :: cloud_flag(:) ! 1/0:no-cloud/cloud
      INTEGER :: surf_flag  ! surface type
      REAL    :: elevation,lat,lon,ps, t2m, q2m, tsk, clwp
      REAL, pointer  :: tb(:), omb(:), bias(:)
      REAL, pointer  :: pred(:)
   END TYPE BIAS

   integer, allocatable :: num_tovs_before(:,:)
   integer, allocatable :: num_tovs_after(:,:)
   integer, allocatable :: tovs_send_pe(:,:)
   integer, allocatable :: tovs_send_start(:,:)
   integer, allocatable :: tovs_send_count(:,:)
   integer, allocatable :: tovs_recv_pe(:,:)
   integer, allocatable :: tovs_recv_start(:,:)
   integer, allocatable :: tovs_copy_count(:)


CONTAINS

#include "da_jo_and_grady_rad.inc"
#include "da_residual_rad.inc"
#include "da_biascorr_rad.inc"
#include "da_biasprep.inc"
#include "da_write_biasprep.inc"
#include "da_predictor.inc"
#include "da_qc_rad.inc"
#include "da_qc_amsua.inc"
#include "da_qc_amsub.inc"
#include "da_write_iv_rad_ascii.inc"
#include "da_write_filtered_rad.inc"
#include "da_read_filtered_rad.inc"
#include "da_write_oa_rad_ascii.inc"
#include "da_get_innov_vector_rad.inc"
#include "da_detsurtyp.inc"
#include "da_oma_stats_rad.inc"
#include "da_omb_stats_rad.inc"
#include "da_print_stats_rad.inc"
#include "da_transform_xtoy_rad.inc"
#include "da_transform_xtoy_rad_adj.inc"
#include "da_calculate_grady_rad.inc"
#include "da_get_julian_time.inc"
#include "da_get_time_slots.inc"

#include "da_rttov_init.inc"
#include "da_rttov_direct.inc"
#include "da_rttov_tl.inc"
#include "da_rttov_ad.inc"
#include "da_read_bufrtovs.inc"
#include "da_read_bufrairs.inc"
#include "da_read_kma1dvar.inc"
#include "da_sort_rad.inc"
#include "da_setup_bufrtovs_structures.inc"
#include "da_status_rad.inc"

#include "gsi_emiss.inc"
#include "emiss_ssmi.inc"
#include "iceem_amsu.inc"
#include "siem_ats.inc"
#include "siem_bts.inc"
#include "siem_interpolate.inc"
#include "landem.inc"
#include "snwem_amsu.inc"
#include "seaem.inc"
#include "ossmem.inc"

end module da_radiance

