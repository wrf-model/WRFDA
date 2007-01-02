SUBROUTINE DA_Deallocate_Background_Errors ( be )

!------------------------------------------------------------------------------
!  PURPOSE: Deallocate components of 3dvar background errors.
!
!  METHOD:  Deallocate component in turn.
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_Define_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (be_type), intent(inout)        :: be     ! Background error structure.
   
!-----End Of Header------------------------------------------------------------

!  Deallocate gridpoint errors:

   IF ( be % v1 % mz > 0 ) DEALLOCATE ( be % v1 % val )
   IF ( be % v2 % mz > 0 ) DEALLOCATE ( be % v2 % val )
   IF ( be % v3 % mz > 0 ) DEALLOCATE ( be % v3 % val )
   IF ( be % v4 % mz > 0 ) DEALLOCATE ( be % v4 % val )
   IF ( be % v5 % mz > 0 ) DEALLOCATE ( be % v5 % val )  
   
   IF ( be % v1 % mz > 0 ) DEALLOCATE ( be % v1 % rf_alpha )
   IF ( be % v2 % mz > 0 ) DEALLOCATE ( be % v2 % rf_alpha )
   IF ( be % v3 % mz > 0 ) DEALLOCATE ( be % v3 % rf_alpha )
   IF ( be % v4 % mz > 0 ) DEALLOCATE ( be % v4 % rf_alpha )
   IF ( be % v5 % mz > 0 ) DEALLOCATE ( be % v5 % rf_alpha )
   
!  Deallocate eigenvectors of vertical error covariance:

   IF ( vert_corr == 2) THEN

      IF ( be % v1 % mz > 0 )DEALLOCATE ( be % v1 % evec )
      IF ( be % v2 % mz > 0 )DEALLOCATE ( be % v2 % evec )
      IF ( be % v3 % mz > 0 )DEALLOCATE ( be % v3 % evec )
      IF ( be % v4 % mz > 0 )DEALLOCATE ( be % v4 % evec )
      IF ( be % v5 % mz > 0 )DEALLOCATE ( be % v5 % evec )

   END IF
   
   IF ( be % v1 % mz > 0 ) DEALLOCATE( be % v1 % rf_alpha )
   IF ( be % v2 % mz > 0 ) DEALLOCATE( be % v2 % rf_alpha )
   IF ( be % v3 % mz > 0 ) DEALLOCATE( be % v3 % rf_alpha )
   IF ( be % v4 % mz > 0 ) DEALLOCATE( be % v4 % rf_alpha )
   IF ( be % v5 % mz > 0 ) DEALLOCATE( be % v5 % rf_alpha )
   
!  Deallocate control variable errors (in future uncomment use these to allow 
!  eg NMC error correlations).

!  DEALLOCATE ( be % cv % val )

END SUBROUTINE DA_Deallocate_Background_Errors
