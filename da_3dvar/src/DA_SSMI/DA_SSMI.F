MODULE DA_SSMI

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Grid_Definitions
   USE DA_Physics
   USE DA_Tools
   USE PAR_UTIL

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

#include "DA_AO_Stats_SSMI.inc"
#include "DA_AO_Stats_SSMI_Rv.inc"
#include "DA_AO_Stats_SSMI_Tb.inc"
#include "DA_Read_SSMI.inc"
#include "DA_Scan_SSMI.inc"
#include "DA_Cal_Jo_and_GradY_SSMI.inc"
#include "DA_Cal_Jo_and_GradY_SSMI_Rv.inc"
#include "DA_Cal_Jo_and_GradY_SSMI_Tb.inc"
#include "DA_Cal_Residual_SSMI.inc"
#include "DA_Cal_Residual_SSMI_Rv.inc"
#include "DA_Cal_Residual_SSMI_Tb.inc"
#include "DA_OI_Stats_SSMI.inc"
#include "DA_OI_Stats_SSMI_Rv.inc"
#include "DA_OI_Stats_SSMI_Tb.inc"
#include "DA_Transform_XToSpeed.inc"
#include "DA_Transform_XToSpeed_Lin.inc"
#include "DA_Transform_XToSpeed_Adj.inc"
#include "DA_Transform_XToSeaSfcWind.inc"
#include "DA_Transform_XToSeaSfcWind_Lin.inc"
#include "DA_Transform_XToSeaSfcWind_Adj.inc"
#include "DA_Transform_XToTb.inc"
#include "DA_Transform_XToTb_Lin.inc"
#include "DA_Transform_XToTb_Adj.inc"
#include "DA_Transform_XToY_SSMI.inc"
#include "DA_Transform_XToY_SSMI_Adj.inc"
#include "DA_Transform_XToY_SSMI_Rv.inc"
#include "DA_Transform_XToY_SSMI_Rv_Adj.inc"
#include "DA_Transform_XToY_SSMI_Tb.inc"
#include "DA_Transform_XToY_SSMI_Tb_Adj.inc"
#include "DA_Transform_XToZRhoQ.inc"
#include "DA_Transform_XToZRhoQ_Lin.inc"
#include "DA_Transform_XToZRhoQ_Adj.inc"
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
#include "da_oa_stats_ssmt1.inc"
#include "da_oa_stats_ssmt2.inc"
#include "da_ob_stats_ssmt1.inc"
#include "da_ob_stats_ssmt2.inc"
#include "da_print_stats_ssmt1.inc"
#include "da_print_stats_ssmt2.inc"
#include "da_transform_xtoy_ssmt1.inc"
#include "da_transform_xtoy_ssmt1_adj.inc"
#include "da_transform_xtoy_ssmt2.inc"
#include "da_transform_xtoy_ssmt2_adj.inc"
#include "DA_Calculate_GradY_SSMI.inc"
#include "DA_Calculate_GradY_SSMT1.inc"
#include "DA_Calculate_GradY_SSMT2.inc"
   
END MODULE DA_SSMI

