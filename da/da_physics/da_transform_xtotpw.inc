subroutine da_transform_xtotpw(grid)

   !---------------------------------------------------------------------
   ! Purpose: TBD
   !---------------------------------------------------------------------

   implicit none
   
   type (domain),  intent(inout) :: grid

   integer :: i, j, K

   real    :: pw

   ! WEIGHTED SUM OF VERTICAL COLUMN
   do j=jts, jte
      do i=its, ite
         pw = 0.0
         do k=kts, kte
            pw = pw + (grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k)) &
                    * (grid%xa%q(i,j,k)*grid%xb%rho(i,j,k) &
                    +  grid%xb%q(i,j,k)*grid%xa%rho(i,j,k))
         end do
 
         ! To convert the unit of PW to cm:
         grid%xa%tpw(i,j) = 0.1*pw
      end do
   end do
 
end subroutine da_transform_xtotpw


