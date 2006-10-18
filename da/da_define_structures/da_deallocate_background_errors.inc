subroutine da_deallocate_background_errors ( be )

   !---------------------------------------------------------------------------
   ! PURPOSE: Deallocate components of wrfvar background errors.
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (be_type), intent(inout)        :: be     ! Background error structure.
   
   IF (trace_use) call da_trace_entry("da_deallocate_background_errors")

   ! Deallocate gridpoint errors:

   if (associated(be % v1 % val)) deallocate ( be % v1 % val )
   if (associated(be % v2 % val)) deallocate ( be % v2 % val )
   if (associated(be % v3 % val)) deallocate ( be % v3 % val )
   if (associated(be % v4 % val)) deallocate ( be % v4 % val )
   if (associated(be % v5 % val)) deallocate ( be % v5 % val )  

   if (associated(be % v1 % val_g)) deallocate ( be % v1 % val_g )
   if (associated(be % v2 % val_g)) deallocate ( be % v2 % val_g )
   if (associated(be % v3 % val_g)) deallocate ( be % v3 % val_g )
   if (associated(be % v4 % val_g)) deallocate ( be % v4 % val_g )
   if (associated(be % v5 % val_g)) deallocate ( be % v5 % val_g ) 
   
   if (associated(be % v1 % rf_alpha)) deallocate ( be % v1 % rf_alpha )
   if (associated(be % v2 % rf_alpha)) deallocate ( be % v2 % rf_alpha )
   if (associated(be % v3 % rf_alpha)) deallocate ( be % v3 % rf_alpha )
   if (associated(be % v4 % rf_alpha)) deallocate ( be % v4 % rf_alpha )
   if (associated(be % v5 % rf_alpha)) deallocate ( be % v5 % rf_alpha )
   
   ! deallocate eigenvectors of vertical error covariance:

   if (vert_corr == 2) then
      if (associated(be % v1 % evec)) deallocate ( be % v1 % evec )
      if (associated(be % v2 % evec)) deallocate ( be % v2 % evec )
      if (associated(be % v3 % evec)) deallocate ( be % v3 % evec )
      if (associated(be % v4 % evec)) deallocate ( be % v4 % evec )
      if (associated(be % v5 % evec)) deallocate ( be % v5 % evec )
      if (associated(be % v1 % evec_g)) deallocate ( be % v1 % evec_g )
      if (associated(be % v2 % evec_g)) deallocate ( be % v2 % evec_g )
      if (associated(be % v3 % evec_g)) deallocate ( be % v3 % evec_g )
      if (associated(be % v4 % evec_g)) deallocate ( be % v4 % evec_g )
      if (associated(be % v5 % evec_g)) deallocate ( be % v5 % evec_g )
   END IF
   
   ! Deallocate control variable errors (in future uncomment use these to allow 
   ! eg NMC error correlations).

   ! if (associated(be%cv%val)) deallocate ( be % cv % val )

   ! Other

   if (associated(be%reg_chi)) deallocate(be%reg_chi)
   if (associated(be%reg_ps))  deallocate(be%reg_ps)
   if (associated(be%reg_t))   deallocate(be%reg_t)

   IF (trace_use) call da_trace_exit("da_deallocate_background_errors")

end subroutine da_deallocate_background_errors

