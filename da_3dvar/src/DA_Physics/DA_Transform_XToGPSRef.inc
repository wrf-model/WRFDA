subroutine da_transform_XTogpsref(xb, xp)
!
! input : xb%q, xb%p, xb%t, and xp
! output: xb%ref
!
! modified from MM5-4DVAR. Wei Huang, reviewed and modified by Y.-R. Guo 
!                                                           June 9, 2004

   TYPE (xb_type), INTENT(inout) :: xb      ! First guess structure.
   type(xpose_type), intent(in)  :: xp      ! Domain decomposition vars.
   
   integer :: i, j, k, is, ie, js, je, ks, ke
   real    :: partone, parttwo, dividnd

   is = xp%its; ie = xp%ite
   js = xp%jts; je = xp%jte
   ks = xp%kts; ke = xp%kte      

   do k = ks, ke
      do j = js, je
         do i = is, ie

!--calculate refractivity

!  1, Hydrostatic part of refractivity:
!     Note: p in Pascal.

      partone  = 0.776*xb%p(i,j,k)/xb%t(i,j,k)

!  2, (Wet part) / (hydrostatic part):
!     Note: xb%q is the specific humidity --- a 3DAVR analysis variable. 

      dividnd  = xb%t(i,j,k)*(0.622+0.378*xb%q(i,j,k))
      parttwo  = 1.0+coeff*xb%q(i,j,k)/dividnd

!  3, Refractivity:
      xb%ref(i,j,k)= partone * parttwo

         enddo
      enddo
   enddo
   
end subroutine da_transform_XTogpsref

