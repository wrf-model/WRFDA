MODULE DA_Gpspw

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Physics
   USE DA_Tools
   USE PAR_UTIL

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#endif

CONTAINS

#include "DA_AO_Stats_Gpspw.inc"
#include "DA_Calculate_Jo_and_GradY_Gpspw.inc"
#include "DA_Calculate_Residual_Gpspw.inc"
#include "DA_OI_Stats_Gpspw.inc"
#include "DA_Print_Stats_Gpspw.inc"
#include "DA_Transform_XToY_Gpspw.inc"
#include "DA_Transform_XToY_Gpspw_Adj.inc"
#include "da_check_max_iv_gpspw.inc"
#include "da_get_innov_vector_gpspw.inc"
#include "DA_Calculate_GradY_Gpspw.inc"


END MODULE DA_Gpspw

