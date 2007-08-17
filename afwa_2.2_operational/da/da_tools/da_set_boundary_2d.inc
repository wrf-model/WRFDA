subroutine da_set_boundary_2d(var)

   !------------------------------------------------------------------------
   ! Purpose: Merge East-West boundary values for input 3d-array (var)   
   !------------------------------------------------------------------------

   implicit none
   real, dimension(ims:ime, jms:jme) :: var

   integer :: n, j

   if ((its /= ids) .or. (ite /= ide)) return

   do j=jts, jte
      do n=1,bdyzone
         var(ids-n,j) = var(ide+1-n,j)
         var(ide+n,j) = var(ids-1+n,j)
      end do
   end do

end subroutine da_set_boundary_2d


