MODULE DA_Gpsref

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE PAR_UTIL

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#endif

CONTAINS

#include "DA_AO_Stats_Gpsref.inc"
#include "DA_Calculate_GradY_Gpsref.inc"
#include "DA_Calculate_Jo_and_GradY_Gpsref.inc"
#include "DA_Calculate_Residual_Gpsref.inc"
#include "DA_OI_Stats_Gpsref.inc"
#include "DA_Print_Stats_Gpsref.inc"
#include "DA_Transform_XToY_Gpsref.inc"
#include "DA_Transform_XToY_Gpsref_Adj.inc"
#include "da_check_max_iv_Gpsref.inc"
#include "da_get_innov_vector_Gpsref.inc"

END MODULE DA_Gpsref

