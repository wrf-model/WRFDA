SUBROUTINE DA_Set_Boundary_Xa( xa, xb )
!------------------------------------------------------------------------
!  Purpose: 
!
!  Merge East-West boundary values for the desired Xa-type variables  
!
!  History:          
!          01/30/2005     Author:   Syed RH Rizvi
!------------------------------------------------------------------------
   implicit none

   type (x_type),  intent(inout) :: xa       ! gridded analysis increment.
   type (xb_type), intent(in)    :: xb       ! first guess state.

   integer :: n, j, k

   if((xb%its /= xb%ids) .or. (xb%ite /= xb%ide)) return

!--2d

   k = xb%kte + 1
   do j=xb%jms, xb%jme
   do n=1,bdyzone
      xa%psfc(xb%ids-n,j) = xa%psfc(xb%ide+1-n,j)
      xa%w(xb%ids-n,j,k) = xa%w(xb%ide+1-n,j,k)

      xa%psfc(xb%ide+n,j) = xa%psfc(xb%ids-1+n,j)
      xa%w(xb%ide+n,j,k) = xa%w(xb%ids-1+n,j,k)
   enddo
   enddo

!--3d

   do k=xb%kts, xb%kte
   do j=xb%jms, xb%jme
   do n=1,bdyzone
      xa%u(xb%ids-n,j,k) = xa%u(xb%ide+1-n,j,k)
      xa%v(xb%ids-n,j,k) = xa%v(xb%ide+1-n,j,k)
      xa%t(xb%ids-n,j,k) = xa%t(xb%ide+1-n,j,k)
      xa%p(xb%ids-n,j,k) = xa%p(xb%ide+1-n,j,k)
      xa%q(xb%ids-n,j,k) = xa%q(xb%ide+1-n,j,k)
      xa%w(xb%ids-n,j,k) = xa%w(xb%ide+1-n,j,k)

      xa%u(xb%ide+n,j,k) = xa%u(xb%ids-1+n,j,k)
      xa%v(xb%ide+n,j,k) = xa%v(xb%ids-1+n,j,k)
      xa%t(xb%ide+n,j,k) = xa%t(xb%ids-1+n,j,k)
      xa%p(xb%ide+n,j,k) = xa%p(xb%ids-1+n,j,k)
      xa%q(xb%ide+n,j,k) = xa%q(xb%ids-1+n,j,k)
      xa%w(xb%ide+n,j,k) = xa%w(xb%ids-1+n,j,k)
   enddo
   enddo
   enddo

END SUBROUTINE DA_Set_Boundary_Xa

