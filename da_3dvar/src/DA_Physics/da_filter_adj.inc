SUBROUTINE da_filter_adj(var, xp)

!------------------------------------------------------------------------------
!  HISTORY: 08/27/2004	Wei Huang
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xpose_type), INTENT(INOUT) :: xp      ! Transpose variables.

   REAL, DIMENSION(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                      INTENT(INout) :: var

   integer :: i, j, k

   real, dimension (3) :: w

   data  w/0.25,0.5,0.25/

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

!------------------------------------------------------------------------------

!  Copy var for transpose.

   xp%v1z(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte) = &
      var(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte)

!------------------------------------------------------------------------------

!--Apply (i',j',k -> i',j,k') transpose ( v1z -> v1y ).

   call transpose_z2y ( xp )

!--Perform y-direction filter:

   do k=xp%ktsy, xp%ktey
   do i=xp%itey, xp%itsy, -1
!-----Backward
      do j=xp%jds+1, xp%jde-1
         xp%v1y(i,j-1,k) = xp%v1y(i,j-1,k) + w(1)*xp%v1y(i,j,k)
         xp%v1y(i,j+1,k) = xp%v1y(i,j+1,k) + w(3)*xp%v1y(i,j,k)
         xp%v1y(i,j  ,k) = w(2)*xp%v1y(i,j,k)
      enddo

!-----Forward
      do j=xp%jde-1,xp%jds+1,-1
         xp%v1y(i,j-1,k) = xp%v1y(i,j-1,k) + w(1)*xp%v1y(i,j,k)
         xp%v1y(i,j+1,k) = xp%v1y(i,j+1,k) + w(3)*xp%v1y(i,j,k)
         xp%v1y(i,j  ,k) = w(2)*xp%v1y(i,j,k)
      enddo
   enddo
   enddo

!------------------------------------------------------------------------------

!--Apply (i',j,k' -> i,j',k') transpose (v1y -> v1x).

   call transpose_y2x ( xp )

!--Perform x-direction filter:

   do k=xp%ktsx, xp%ktex
   do j=xp%jtex, xp%jtsx, -1
!-----Backward
      do i=xp%ids+1, xp%ide-1
         xp%v1x(i-1,j,k) = xp%v1x(i-1,j,k) + w(1)*xp%v1x(i,j,k)
         xp%v1x(i+1,j,k) = xp%v1x(i+1,j,k) + w(3)*xp%v1x(i,j,k)
         xp%v1x(i,j  ,k) = w(2)*xp%v1x(i,j,k)
      enddo

!-----Forward 
      do i=xp%ide-1,xp%ids+1,-1
         xp%v1x(i-1,j,k) = xp%v1x(i-1,j,k) + w(1)*xp%v1x(i,j,k)
         xp%v1x(i+1,j,k) = xp%v1x(i+1,j,k) + w(3)*xp%v1x(i,j,k)
         xp%v1x(i,j  ,k) = w(2)*xp%v1x(i,j,k)
      enddo
   enddo
   enddo

!------------------------------------------------------------------------------

!--Apply (i,j',k' -> i',j',k) transpose ( v1x -> v1z ).

   call transpose_x2z ( xp )

!------------------------------------------------------------------------------

   var(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte) = &
   xp%v1z(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte)

END SUBROUTINE da_filter_adj

