MODULE DA_Grid_Definitions
   
!------------------------------------------------------------------------------
!  PURPOSE: Calculation of analysis grid variables:
!
!  METHOD:  Varied.
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!
!------------------------------------------------------------------------------
   
   USE DA_Constants
   USE DA_Define_Structures     ! For xpose_type.
   
   IMPLICIT NONE
   
   CONTAINS
   
#include <da_ref_height.inc>
#include <da_ref_pres.inc>
#include <ffdduv.inc>
#include <earth_2_model_wind.inc>
#include <set_map_para.inc>

END MODULE DA_Grid_Definitions

