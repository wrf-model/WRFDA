SUBROUTINE DA_Zero_vp_type( vp )

   IMPLICIT NONE

   TYPE (vp_type), INTENT(INOUT) :: vp
 
   if(associated(vp % v1)) vp % v1(:,:,:) = 0.0
   if(associated(vp % v2)) vp % v2(:,:,:) = 0.0
   if(associated(vp % v3)) vp % v3(:,:,:) = 0.0
   if(associated(vp % v4)) vp % v4(:,:,:) = 0.0
   if(associated(vp % v5)) vp % v5(:,:,:) = 0.0

END SUBROUTINE DA_Zero_vp_type

