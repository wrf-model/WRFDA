subroutine da_zero_vp_type( vp )

   IMPLICIT NONE

   TYPE (vp_type), INTENT(INOUT) :: vp

   IF (trace_use) call da_trace_entry("da_zero_vp_type")
 
   !  Standard fields:
   if (associated(vp % v1)) vp % v1(:,:,:) = 0.0
   if (associated(vp % v2)) vp % v2(:,:,:) = 0.0
   if (associated(vp % v3)) vp % v3(:,:,:) = 0.0
   if (associated(vp % v4)) vp % v4(:,:,:) = 0.0
   if (associated(vp % v5)) vp % v5(:,:,:) = 0.0

   ! Flow-dependent control variables:
   if (associated(vp % alpha) ) vp % alpha(:,:,:) = 0.0

   IF (trace_use) call da_trace_exit("da_zero_vp_type")

end subroutine da_zero_vp_type

