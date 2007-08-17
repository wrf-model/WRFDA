subroutine da_filter(var, xp)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none
   
   type (xpose_type), intent(inout) :: xp      ! Transpose variables.

   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                      intent(inout) :: var

   integer :: i, j, k

   real, dimension (3) :: w

   data  w/0.25,0.5,0.25/

   ! Copy var for transpose.

   xp%v1z(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte) = &
      var(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte)

   ! Apply (i',j',k -> i,j',k') transpose (v1z -> v1x).

   call da_transpose_z2x (xp)

   ! Perform x-direction filter:

   do k = xp%ktsx, xp%ktex
      do j=xp%jtsx, xp%jtex
         ! Forward 
         do i=xp%ids+1, xp%ide-1
            xp%v1x(i,j,k) = w(1)*xp%v1x(i-1,j,k) + w(2)*xp%v1x(i,j,k) + &
               w(3)*xp%v1x(i+1,j,k)
         end do

         ! Backward
         do i=xp%ide-1,xp%ids+1,-1
            xp%v1x(i,j,k) = w(1)*xp%v1x(i-1,j,k) + w(2)*xp%v1x(i,j,k) + &
               w(3)*xp%v1x(i+1,j,k)
         end do
      end do
   end do


   ! Apply (i,j',k' -> i',j,k') transpose (v1x -> v1y).

   call da_transpose_x2y (xp)

   ! Perform y-direction filter:

   do k=xp%ktsy, xp%ktey
      do i=xp%itsy, xp%itey
         ! Forward
         do j=xp%jds+1, xp%jde-1
            xp%v1y(i,j,k) = w(1)*xp%v1y(i,j-1,k) + w(2)*xp%v1y(i,j,k) + &
               w(3)*xp%v1y(i,j+1,k)
         end do

         ! Backward
         do j=xp%jde-1,xp%jds+1,-1
            xp%v1y(i,j,k) = w(1)*xp%v1y(i,j-1,k) + w(2)*xp%v1y(i,j,k) + &
               w(3)*xp%v1y(i,j+1,k)
         end do
      end do
   end do

   ! Apply (i',j,k' -> i',j',k) transpose (v1y -> v1z).

   call da_transpose_y2z (xp)

   var(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte) = &
      xp%v1z(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte)

end subroutine da_filter


