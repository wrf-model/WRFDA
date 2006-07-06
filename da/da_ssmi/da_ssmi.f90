MODULE da_ssmi

   USE da_constants
   USE da_define_structures
   USE da_interpolation
   USE da_statistics
   USE da_grid_definitions
   USE da_physics
   USE da_tools
   USE da_par_util
   USE module_wrf_error

   ! The "stats_ssmi_rv_type" is ONLY used locally in DA_Ssmi_Rv:

   TYPE maxmin_ssmi_rv_stats_type
        TYPE (maxmin_type)         :: tpw      ! Toatl precipitable water cm
        TYPE (maxmin_type)         :: Speed    ! Wind speed (m/s)
   END TYPE maxmin_ssmi_rv_stats_type

   TYPE stats_ssmi_retrieval_type
        TYPE (maxmin_ssmi_rv_stats_type)      :: maximum, minimum
        TYPE (residual_ssmi_retrieval_type)   :: average, rms_err
   END TYPE stats_ssmi_retrieval_type

   ! The "stats_ssmi_tb_type" is ONLY used locally in DA_Ssmi_tb:

   TYPE maxmin_ssmi_tb_stats_type
        TYPE (maxmin_type)         :: tb19v    ! brightness temperature (K)
        TYPE (maxmin_type)         :: tb19h    ! brightness temperature (K)
        TYPE (maxmin_type)         :: tb22v    ! brightness temperature (K)
        TYPE (maxmin_type)         :: tb37v    ! brightness temperature (K)
        TYPE (maxmin_type)         :: tb37h    ! brightness temperature (K)
        TYPE (maxmin_type)         :: tb85v    ! brightness temperature (K)
        TYPE (maxmin_type)         :: tb85h    ! brightness temperature (K)
   END TYPE maxmin_ssmi_tb_stats_type

   TYPE stats_ssmi_tb_type
        TYPE (maxmin_ssmi_tb_stats_type)  :: maximum, minimum
        TYPE (residual_ssmi_tb_type)      :: average, rms_err
   END TYPE stats_ssmi_tb_type


CONTAINS

#include "da_ao_stats_ssmi.inc"
#include "da_ao_stats_ssmi_rv.inc"
#include "da_ao_stats_ssmi_tb.inc"
#include "da_read_ssmi.inc"
#include "da_scan_ssmi.inc"
#include "da_cal_jo_and_grady_ssmi.inc"
#include "da_cal_jo_and_grady_ssmi_rv.inc"
#include "da_cal_jo_and_grady_ssmi_tb.inc"
#include "da_cal_residual_ssmi.inc"
#include "da_cal_residual_ssmi_rv.inc"
#include "da_cal_residual_ssmi_tb.inc"
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
#include "da_cal_jo_and_grady_ssmt1.inc"
#include "da_cal_jo_and_grady_ssmt2.inc"
#include "da_cal_residual_ssmt1.inc"
#include "da_cal_residual_ssmt2.inc"
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
#include "da_adj_tb.inc"
#include "da_adj_cal_sigma_v.inc"
#include "da_adj_effang.inc"
#include "da_adj_effht.inc"
#include "da_adj_epsalt.inc"
#include "da_adj_roughem.inc"
#include "da_adj_spemiss.inc"
#include "da_adj_tbatmos.inc"
   
end module da_ssmi

