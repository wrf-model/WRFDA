SUBROUTINE DA_Set_Boundary_Xb(xb)
!------------------------------------------------------------------------
!  Purpose: 
!
!  Merge East-West boundary values for the desired Xb-type variables  
!
!  History:          
!          01/30/2005      Author:   Syed RH Rizvi
!------------------------------------------------------------------------
   implicit none

   type (xb_type), intent(inout) :: xb       ! first guess state.

   integer :: n, j, k

   if((xb%its /= xb%ids) .or. (xb%ite /= xb%ide)) return

!--2d

   do j=xb%jts, xb%jte
   do n=1,bdyzone
      xb%psfc(xb%ids-n,j)       = xb%psfc(xb%ide+1-n,j)
      xb%map_factor(xb%ids-n,j) = xb%map_factor(xb%ide+1-n,j)
      xb%coefx(xb%ids-n,j)      = xb%coefx(xb%ide+1-n,j)
      xb%coefy(xb%ids-n,j)      = xb%coefy(xb%ide+1-n,j)

      xb%psfc(xb%ide+n,j)       = xb%psfc(xb%ids-1+n,j)
      xb%map_factor(xb%ide+n,j) = xb%map_factor(xb%ids-1+n,j)
      xb%coefx(xb%ide+n,j)      = xb%coefx(xb%ids-1+n,j)
      xb%coefy(xb%ide+n,j)      = xb%coefy(xb%ids-1+n,j)
   enddo
   enddo

!--3d
   do k=xb%kts, xb%kte
   do j=xb%jts, xb%jte
   do n=1,bdyzone
      xb%h(xb%ids-n,j,k) = xb%h(xb%ide+1-n,j,k)
      xb%u(xb%ids-n,j,k) = xb%u(xb%ide+1-n,j,k)
      xb%v(xb%ids-n,j,k) = xb%v(xb%ide+1-n,j,k)
      xb%t(xb%ids-n,j,k) = xb%t(xb%ide+1-n,j,k)
      xb%p(xb%ids-n,j,k) = xb%p(xb%ide+1-n,j,k)
      xb%q(xb%ids-n,j,k) = xb%q(xb%ide+1-n,j,k)
      xb%qs(xb%ids-n,j,k) = xb%qs(xb%ide+1-n,j,k)


      xb%h(xb%ide+n,j,k) = xb%h(xb%ids-1+n,j,k)
      xb%u(xb%ide+n,j,k) = xb%u(xb%ids-1+n,j,k)
      xb%v(xb%ide+n,j,k) = xb%v(xb%ids-1+n,j,k)
      xb%t(xb%ide+n,j,k) = xb%t(xb%ids-1+n,j,k)
      xb%p(xb%ide+n,j,k) = xb%p(xb%ids-1+n,j,k)
      xb%q(xb%ide+n,j,k) = xb%q(xb%ids-1+n,j,k)
      xb%qs(xb%ide+n,j,k) = xb%qs(xb%ids-1+n,j,k)
   enddo
   enddo
   enddo

END SUBROUTINE DA_Set_Boundary_Xb

