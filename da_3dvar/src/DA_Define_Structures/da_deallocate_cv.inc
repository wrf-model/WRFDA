SUBROUTINE da_deallocate_cv( cv )

!------------------------------------------------------------------------------
!  PURPOSE: Allocate components of control variable.
!
!  METHOD:  Allocate component in turn.
!
!  HISTORY: 06/08/2000 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_Define_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (cv_type), INTENT(INOUT)        :: cv     ! Control variable structure.
   
!------------------------------------------------------------------------------
!  [1.0] Deallocate arrays:
!------------------------------------------------------------------------------

   DEALLOCATE ( cv % array )
   
END SUBROUTINE da_deallocate_cv

