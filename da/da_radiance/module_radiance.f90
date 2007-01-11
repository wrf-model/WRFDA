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
   use gsi_kinds      ,  only : r_kind,r_double,i_kind,r_single
   use gsi_constants  ,  only : deg2rad, rad2deg,       &
                            init_constants_derived, &
                            one, three, zero, half, &
                            one_tenth, two, four

   ! use irsse_model, only: forward_irsse
   implicit none
   
   real, parameter             :: q2ppmv = 1.60771704e+6
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

