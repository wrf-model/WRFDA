module da_ssmi

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_grid_definitions
   use da_physics
   use da_tools

   ! The "stats_ssmi_rv_type" is ONLY used locally in da_ssmi_rv:

   type maxmin_ssmi_rv_stats_type
      type (maxmin_type)         :: tpw      ! Toatl precipitable water cm
      type (maxmin_type)         :: Speed    ! Wind speed (m/s)
   end type maxmin_ssmi_rv_stats_type

   type stats_ssmi_retrieval_type
      type (maxmin_ssmi_rv_stats_type)      :: maximum, minimum
      type (residual_ssmi_retrieval_type)   :: average, rms_err
   end type stats_ssmi_retrieval_type

   ! The "stats_ssmi_tb_type" is ONLY used locally in da_ssmi_tb:

   type maxmin_ssmi_tb_stats_type
      type (maxmin_type)         :: tb19v    ! brightness temperature (K)
      type (maxmin_type)         :: tb19h    ! brightness temperature (K)
      type (maxmin_type)         :: tb22v    ! brightness temperature (K)
      type (maxmin_type)         :: tb37v    ! brightness temperature (K)
      type (maxmin_type)         :: tb37h    ! brightness temperature (K)
      type (maxmin_type)         :: tb85v    ! brightness temperature (K)
      type (maxmin_type)         :: tb85h    ! brightness temperature (K)
   end type maxmin_ssmi_tb_stats_type

   type stats_ssmi_tb_type
      type (maxmin_ssmi_tb_stats_type)  :: maximum, minimum
      type (residual_ssmi_tb_type)      :: average, rms_err
   end type stats_ssmi_tb_type


contains

#include "da_ao_stats_ssmi.inc"
#include "da_ao_stats_ssmi_rv.inc"
#include "da_ao_stats_ssmi_tb.inc"
#include "da_read_ssmi.inc"
#include "da_scan_ssmi.inc"
#include "da_jo_and_grady_ssmi.inc.inc"
#include "da_jo_and_grady_ssmi.inc_rv.inc"
#include "da_jo_and_grady_ssmi.inc_tb.inc"
#include "da_residual_ssmi.inc"
#include "da_residual_ssmi_rv.inc"
#include "da_residual_ssmi_tb.inc"
#include "da_oi_stats_ssmi.inc"
#include "da_oi_stats_ssmi_rv.inc"
#include "da_oi_stats_ssmi_tb.inc"
#include "da_transform_xtospeed.inc"
#include "da_transform_xtospeed_lin.inc"
#include "da_transform_xtospeed_adj.inc"
#include "da_transform_xtoseasfcwind.inc"
#include "da_transform_xtoseasfcwind_lin.inc"
#include "da_transform_xtoseasfcwind_adj.inc"
#include "da_transform_xtotb.inc"
#include "da_transform_xtotb_lin.inc"
#include "da_transform_xtotb_adj.inc"
#include "da_transform_xtoy_ssmi.inc"
#include "da_transform_xtoy_ssmi_adj.inc"
#include "da_transform_xtoy_ssmi_rv.inc"
#include "da_transform_xtoy_ssmi_rv_adj.inc"
#include "da_transform_xtoy_ssmi_tb.inc"
#include "da_transform_xtoy_ssmi_tb_adj.inc"
#include "da_transform_xtozrhoq.inc"
#include "da_transform_xtozrhoq_lin.inc"
#include "da_transform_xtozrhoq_adj.inc"
#include "da_jo_and_grady_ssmt1.inc.inc"
#include "da_jo_and_grady_ssmt2.inc.inc"
#include "da_residual_ssmt1.inc"
#include "da_residual_ssmt2.inc"
#include "da_check_max_iv_ssmi_rv.inc"
#include "da_check_max_iv_ssmi_tb.inc"
#include "da_check_max_iv_ssmt1.inc"
#include "da_check_max_iv_ssmt2.inc"
#include "da_get_innov_vector_ssmi.inc"
#include "da_get_innov_vector_ssmi_rv.inc"
#include "da_get_innov_vector_ssmi_tb.inc"
#include "da_get_innov_vector_ssmt1.inc"
#include "da_get_innov_vector_ssmt2.inc"
#include "da_ao_stats_ssmt1.inc"
#include "da_ao_stats_ssmt2.inc"
#include "da_oi_stats_ssmt1.inc"
#include "da_oi_stats_ssmt2.inc"
#include "da_print_stats_ssmt1.inc"
#include "da_print_stats_ssmt2.inc"
#include "da_transform_xtoy_ssmt1.inc"
#include "da_transform_xtoy_ssmt1_adj.inc"
#include "da_transform_xtoy_ssmt2.inc"
#include "da_transform_xtoy_ssmt2_adj.inc"
#include "da_calculate_grady_ssmi.inc"
#include "da_calculate_grady_ssmt1.inc"
#include "da_calculate_grady_ssmt2.inc"
#include "da_sigma_v.inc"
#include "da_tb_adj.inc"
#include "da_sigma_v_adj.inc"
#include "da_effang_adj.inc"
#include "da_effht_adj.inc"
#include "da_epsalt_adj.inc"
#include "da_roughem_adj.inc"
#include "da_spemiss_adj.inc"
#include "da_tbatmos_adj.inc"

#include "epsalt.inc"
#include "spemiss.inc"
#include "tb.inc"
#include "tbatmos.inc"
#include "effht.inc"
#include "effang.inc"
#include "roughem.inc"

   
end module da_ssmi

