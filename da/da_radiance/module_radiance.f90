module module_radiance

   !---------------------------------------------------------------------------
   ! Purpose: module for radiance data assimilation. 
   !---------------------------------------------------------------------------

   use da_reporting

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
   USE CRTM_Module   ! include 12 modules
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

