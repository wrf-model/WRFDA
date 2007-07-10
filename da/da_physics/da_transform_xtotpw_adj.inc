subroutine da_transform_xtotpw_adj(grid)

   !---------------------------------------------------------------------
   ! Purpose: TBD
   !---------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid

   integer :: i, j, k, is, js, ie, je

   real    :: pw, dzpw

   if (trace_use) call da_trace_entry("da_transform_xtotpw_adj")

   is = grid%xb%its
   js = grid%xb%jts

   ie = grid%xb%ite
   je = grid%xb%jte

   if (test_wrfvar) then
      is = grid%xb%its-1
      js = grid%xb%jts-1

      ie = grid%xb%ite+1
      je = grid%xb%jte+1

      if (is < grid%xb%ids) is = grid%xb%ids
      if (js < grid%xb%jds) js = grid%xb%jds

      if (ie > grid%xb%ide) ie = grid%xb%ide
      if (je > grid%xb%jde) je = grid%xb%jde
   end if

   do j=js, je
      do i=is, ie
         pw = 0.1*grid%xa%tpw(i,j)

         do k=grid%xb%kts, grid%xb%kte
            dzpw = (grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k))*pw

            grid%xa%  q(i,j,k)=grid%xa%  q(i,j,k)+dzpw*grid%xb%rho(i,j,k)
            grid%xa%rho(i,j,k)=grid%xa%rho(i,j,k)+dzpw*grid%xb%  q(i,j,k)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_transform_xtotpw_adj")
 
end subroutine da_transform_xtotpw_adj


