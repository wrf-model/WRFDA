module module_radiance

   !---------------------------------------------------------------------------
   ! Purpose: module for radiance data assimilation. 
   !---------------------------------------------------------------------------

   use da_control, only : pi, use_landem
   use da_reporting, only : da_error,message

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

#ifdef CRTM
  ! -- Modules to define CRTM constants etc.
   USE CRTM_Parameters, only : INVALID_WMO_SENSOR_ID
   !USE Type_Kinds
   !USE Error_Handler
   !USE CRTM_Utility

  ! -- CRTM RT_models modules
   USE CRTM_Module, only : graupel_cloud, rain_cloud, snow_cloud,crtm_adjoint, &
      crtm_allocate_atmosphere, crtm_allocate_surface, crtm_assign_atmosphere, &
      crtm_assign_surface,crtm_destroy_atmosphere,crtm_destroy_surface, &
      crtm_forward,crtm_init,crtm_k_matrix,crtm_set_channelinfo, &
      crtm_tangent_linear, grass_soil, h2o_id,hail_cloud,ice_cloud,new_snow, &
      o3_id, water_cloud, crtm_rtsolution_type, crtm_channelinfo_type, &
      crtm_atmosphere_type, crtm_surface_type, crtm_geometryinfo_type, &
      crtm_zero_surface, crtm_zero_atmosphere

   USE CRTM_SensorInfo
#endif

   use gsi_kinds      ,  only : r_kind,r_double,i_kind,r_single
   use gsi_constants  ,  only : deg2rad, rad2deg,       &
                            init_constants_derived, &
                            one, three, zero, half, &
                            one_tenth, two, four

   ! use irsse_model, only: forward_irsse
   implicit none
   
   real, parameter             :: q2ppmv = 1.60771704e+6

! n=noaa; f=dmsp; g=goes; c=npoess; eos-1/2=aqua/terra;
   character(len=8), parameter :: crtm_platform_name(1:20) = &
       (/ 'n       ', 'f       ', 'meteosat', 'g       ', 'gms     ', &
          'fy2     ', 'trmm    ', 'ers     ', 'eos     ', 'metop   ', &
          'envisat ', 'msg     ', 'fy1     ', 'adeos   ', 'mtsat   ', &
          'coriolis', 'c       ', 'gifts   ', 'xxxxxxxx', 'xxxxxxxx'/)

! List of instruments  !!!! HIRS is number 0
  Character (len=8), Dimension(0:34) :: crtm_sensor_name  =                &
       & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
       &    'avhrr   ', 'ssmi    ', 'vtpr1   ', 'vtpr2   ', 'tmi     ',  &
       &    'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
       &    'mhs     ', 'iasi    ', 'amsre   ', 'imager  ', 'atms    ',  &
       &    'mviri   ', 'seviri  ', 'imgr    ', 'sndr    ', 'imager  ',  &
       &    'vissr   ', 'mvisr   ', 'cris    ', 'cmis    ', 'viirs   ',  &
       &    'windsat ', 'gifts   ', 'amsre   ', 'SUBSET  ', 'xxxxxxxx'   /)

   integer                     :: n_scatt_coef
   character(len=5), pointer   :: coefs_scatt_instname(:)
   real,             pointer   :: time_slots(:)
#ifdef RTTOV
   type( rttov_coef ), pointer :: coefs(:)         ! RTTOV8_5 coefficients
   type( rttov_scatt_coef ), pointer :: coefs_scatt(:)
#endif

   type satinfo_type
      integer, pointer   :: ichan(:)      ! channel index
      integer, pointer   :: iuse (:)      ! usage flag (-1: not use)
      real   , pointer   :: error(:)      ! error Standard Deviation
      real   , pointer   :: polar(:)      ! polarisation (0:vertical; 1:horizontal)
      real   , pointer   :: rms(:,:)      ! rms of bias corr file
      real   , pointer   :: std(:,:)      ! std of bias corr file
      real   , pointer   :: a(:,:)        ! bias corr coef a Tb*(xb)=a+b*Tb(xb)
      real   , pointer   :: b(:,:)        ! bias corr coef b
      real   , pointer   :: error_factor(:) ! error tuning factor
   end type satinfo_type

   type (satinfo_type), pointer :: satinfo(:)

#ifdef RTTOV
   CHARACTER( 80 ), pointer :: Sensor_Descriptor(:)
#endif

contains

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

end module module_radiance

