MODULE DA_Setup_Structures

!------------------------------------------------------------------------------
!  PURPOSE: Sets up various structures.
!
!  METHOD:  Varied.
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!           11/15/2004 - Added spectral transform programs  Syed RH Rizvi 
!
!------------------------------------------------------------------------------

   use module_state_description

   USE DA_Define_Structures
   USE DA_Constants
   USE DA_Grid_Definitions
   USE DA_Obs
   USE DA_SSMI
   USE DA_VToX_Transforms
   USE DA_Physics
   USE DA_Tools
   USE da_h_ops
   USE DA_ReadWrite_MM5
   USE da_spectral
   USE da_gen_be  
   use module_wrf_3dvar_io

   IMPLICIT NONE

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#endif

   CONTAINS

#include <da_add_increments.inc>
#include <DA_Add_PBL_And_SFC_Info.inc>
#include <da_get_vertical_truncation.inc>
#include <da_interpolate_regcoeff.inc>
#include <da_interpolate_stats.inc>
#include <da_rescale_background_errors.inc>
#include <da_setup_background_errors.inc>
#include <da_setup_background_errors2.inc>
#include <da_setup_background_errors3.inc>
#include <da_setup_background_errors4.inc>
#include <da_setup_background_errors5.inc>
#include <da_chgvres.inc>
#include <da_setup_firstguess.inc>
#include <da_setup_firstguess_mm5.inc>
#include <da_setup_firstguess_wrf.inc>
#include <da_setup_firstguess_kma.inc>
#include <da_setup_obs_structures.inc>
#include <da_setup_obs_structures_ascii.inc>
#include <da_setup_obs_structures_bufr.inc>
#include <da_setup_obs_interp_wts.inc>
#include <da_setup_runconstants.inc>
#include <da_transfer_mm5toxb.inc>
#include <da_transfer_wrftoxb.inc>
#include <da_transfer_kmatoxb.inc>
#include <da_transfer_xatowrf.inc>
#include <da_transfer_xatokma.inc>
!include <da_transfer_wrftltoxa.inc>
!include <da_transfer_wrftltoxa_adj.inc>
!include <da_transfer_xatowrftl.inc>
!include <da_transfer_xatowrftl_adj.inc>
#include <DA_Transfer_XatoAnalysis.inc>
#include <da_write_interpolated_be.inc>
#include <da_cloud_model.inc>
#include <da_write_increments.inc>
#include <DA_Write_KMA_Increments.inc>
#include <DA_Get_Bins_Info.inc>

END MODULE DA_Setup_Structures
