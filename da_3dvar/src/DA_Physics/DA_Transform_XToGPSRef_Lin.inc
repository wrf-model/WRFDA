subroutine da_transform_XTogpsref_lin(xa, xb, xp)
!
!  input: xa%q, xa%p, xa%t, xb%q, xb%p, xb%t, and xp
! output: xa%ref
!
! modified from MM5-4DVAR. Wei Huang, 
! Reviewed and modified by Y.-R. Guo.
!                                              June 9, 2004

   type (x_type),  intent(inout) :: xa       ! gridded analysis increment.
   type (xb_type), intent(in)    :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN) :: xp       ! Domain decomposition vars.
   
   integer :: i, j, k, is, ie, js, je, ks, ke
   real    :: partone, parttwo, dividnd
   real    :: partone9,parttwo9,dividnd9

! 1.0 To get the domain range

   is = xp%its; ie = xp%ite
   js = xp%jts; je = xp%jte
   ks = xp%kts; ke = xp%kte      

! 2.0 Loop for a tile

   do k = ks, ke
      do j = js, je
         do i = is, ie

!-----calculate refractivity
!     Note: p in Pascal, q is the specific humidity

! 2.1 Part one: hydrostatic part

      partone  = 0.776*xa%p(i,j,k)/xb%t(i,j,k) &
               - 0.776*xb%p(i,j,k)*xa%t (i,j,k)/(xb%t(i,j,k)*xb%t(i,j,k))
      partone9 = 0.776*xb%p(i,j,k)/xb%t(i,j,k)

! 2.2 Part two considering the moisture:

      dividnd  = xa%t (i,j,k)*(0.622+0.378*xb%q(i,j,k)) &
               + xb%t(i,j,k)*       0.378*xa%q(i,j,k)
      dividnd9 = xb%t(i,j,k)*(0.622+0.378*xb%q(i,j,k))

      parttwo  =     coeff*xa%q(i,j,k)/dividnd9 &
               -     coeff*xb%q(i,j,k)*dividnd /(dividnd9*dividnd9)
      parttwo9 = 1.0+coeff*xb%q(i,j,k)/dividnd9

! 2.3 Total refractivity

      xa%ref(i,j,k)= partone9 * parttwo &
                   + partone  * parttwo9
         enddo
      enddo
   enddo
   
end subroutine da_transform_XTogpsref_lin

