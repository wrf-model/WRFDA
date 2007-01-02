MODULE DA_Gen_Be_Stats

!-------------------------------------------------------------------------
!  PURPOSE: Collection of routines for DA_Gen_Be_Stats
!
!  HISTORY: 08/15/2004                                         Mi-Seon Lee 
!-------------------------------------------------------------------------
   USE module_wrf_3dvar_io

   USE LAPACK
   USE DA_FFTs
   USE DA_Tools
   USE DA_Test	
   USE DA_Dynamics
   USE DA_Constants
   USE DA_VToX_Transforms
   USE DA_Setup_Structures
   USE DA_Define_Structures

   IMPLICIT NONE
!------------------------------------------------------------------------
   character(len= 120) :: DIRECTORY_NAME, FILE_HEAD 
   character(len= 120) :: mss_directory

   character(len= 24) :: BGN_DATE, END_DATE
   logical :: PRINT_INFO
   logical :: from_mss

   integer :: background_option, members
   
   integer :: T_FORECAST1, T_FORECAST2, FILE_INTERVAL


#if 0
#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif
#endif

   CONTAINS

#include "DA_Stats_Namelist.inc"
#include "DA_Statistics_Step0.inc"
#include "DA_advance_cymdh0.inc"
#include "DA_set_current_date.inc"
#include "DA_get_ymdh.inc"
#include "DA_Assign_Value.inc"
#include "DA_Difference.inc"
#include "DA_New_Statistics_Variable.inc"
#include "DA_Write_Diff.inc"
#include "DA_Make_Ensemble_Mean.inc"
#include "DA_Ens_Sum.inc"
#include "DA_Zero_Ens.inc"
#include "DA_Ens_Magnify.inc"
#include "DA_Test_Transforms.inc"
#include "DA_Mss_Read.inc"
#include "DA_Data_Delete.inc"
#include "DA_Gen_Date.inc"
#include "DA_Make_Filename.inc"

END MODULE DA_Gen_Be_Stats
