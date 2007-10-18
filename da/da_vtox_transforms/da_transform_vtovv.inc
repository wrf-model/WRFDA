subroutine da_alloc_and_copy_be_arrays (vg , v, jts,jte, kts,kte)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (be_subtype), intent(inout) :: vg       ! Global backgrd error struct.
   type (be_subtype), intent(inout) :: v        ! Local backgrd error struct.
   integer, intent(in)              :: jts,jte  ! Tile dimension.
   integer, intent(in)              :: kts,kte  ! Tile dimension.

   ! Allocate local-grid structure.
   v % mz = vg % mz
   if (v % mz > 0) then
      allocate  (v % val(jts:jte,1:v % mz))
      allocate  (v % evec(jts:jte,kts:kte,1:v % mz))
      allocate  (v % rf_alpha(1:v % mz))
   end if

   ! Make local copies from global arrays.
   if (v % mz > 0) then
       v % val(jts:jte,1:v % mz) = vg % val(jts:jte,1:v % mz)
       v % evec(jts:jte,kts:kte,1:v % mz) = vg % evec(jts:jte,kts:kte,1:v % mz)
       v % rf_alpha(1:v % mz) = vg % rf_alpha(1:v % mz)
   end if

   ! Deallocate global arrays.
   if (v % mz > 0) then 
      deallocate  (vg % val )
      deallocate  (vg % evec)
      deallocate  (vg % rf_alpha)
   end if

end subroutine da_alloc_and_copy_be_arrays 


     call da_transform_through_rf(grid,mz, be % v1 % rf_alpha, be % v1 % val, vv % v1)
   end if

   ! [2.2] Transform 2nd control variable:

   mz = be % v2 % mz
   if (mz > 0) then
      call da_transform_through_rf(grid, mz, be % v2 % rf_alpha, be % v2 % val, vv % v2)
   end if

   ! [2.3] Transform 3rd control variable

   mz = be % v3 % mz
   if (mz > 0) then
      call da_transform_through_rf(grid, mz, be % v3 % rf_alpha,be % v3 % val, vv % v3)
   end if

   ! [2.4] Transform 4th control variable
      
   mz = be % v4 % mz
   if (mz > 0) then
      call da_transform_through_rf(grid, mz, be % v4 % rf_alpha, be % v4 % val, vv % v4)
   end if

   ! [2.5] Transform 5th control variable

   mz = be % v5 % mz
   if (mz > 0) then
      call da_transform_through_rf(grid, mz, be % v5 % rf_alpha, be % v5 % val, vv % v5)
   end if

   ! [2.6] Transform alpha control variable

   mz = be % alpha % mz
   if (mz > 0) then
      call da_transform_through_rf(grid, mz, be % alpha % rf_alpha, be % alpha % val, vv % alpha)
   end if

   if (trace_use) call da_trace_exit("da_transform_vtovv")

end subroutine da_transform_vtovv


