SUBROUTINE DA_UV_To_Vorticity(xb , u, v, vor, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
 
!------------------------------------------------------------------------------
!  PURPOSE: Calculate vorticity on a co-ordinate surface, given an input
!           wind field.
!
!  METHOD:  vor = m**2 ( d(v/m)/dx - d(u/m)/dy )
!
!
!  HISTORY: 09/28/1999 - Creation of F90 version.           Dale Barker
!
!           02/02/2005 - Updated for global                 Syed RH Rizvi
!
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
   integer, intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
   integer, intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.

   TYPE (xb_type), INTENT(IN)           :: xb         ! First guess structure.
   real, intent(in)   :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(in)   :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
   real, intent(inout):: vor(ims:ime,jms:jme,kms:kme) ! Vorticity.

   integer            :: i, j, k                      ! Loop counters.
   integer            :: is, ie                       ! 1st dim. end points.
   integer            :: js, je                       ! 2nd dim. end points.
   integer            :: ks, ke                       ! 3rd dim. end points.
   real               :: one_third                    ! 1/3.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   one_third = 1.0 / 3.0

   vor = 0.0

!  Computation to check for edge of domain:
   is = its; ie = ite; js = jts; je = jte
   if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
   if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

!------------------------------------------------------------------------------
!  [2.0] Calculate vorticity:
!------------------------------------------------------------------------------

   DO k = kts, kte

!     [2.1] Compute finite difference vorticity at interior points:
   
      DO j = js, je
      DO i = is, ie
         vor(i,j,k) = xb%coefy(i,j) * ( -u(i,j+1,k) + u(i,j-1,k) )  + & 
                      xb%coefx(i,j) * (  v(i+1,j,k) - v(i-1,j,k) )
      END DO
      END DO

!    [2.2] Impose zero vorticity gradient condition at boundaries:

!     [2.2.1] Bottom boundaries:

      if ( its == ids ) then
         i = its 
         do j = jts, jte
            vor(i,j,k) = one_third * ( 4.0 * vor(i+1,j,k) - vor(i+2,j,k) )
         end do
      end if

!     [2.2.2] Top boundaries:

      if ( ite == ide ) then
         i = ite
         do j = jts, jte
            vor(i,j,k) = one_third * ( 4.0 * vor(i-1,j,k) - vor(i-2,j,k) )
         end do
      end if

!     [2.2.3] Left boundaries:

      if ( jts == jds ) then
         j = jts
         do i = its, ite
            vor(i,j,k) = one_third * ( 4.0 * vor(i,j+1,k) - vor(i,j+2,k) )
         end do
      end if

!     [2.2.4] right boundaries:

      if ( jte == jde ) then
         j = jte
         do i = its, ite
            vor(i,j,k) = one_third * ( 4.0 * vor(i,j-1,k) - vor(i,j-2,k) )
         end do
      end if

   END DO

END SUBROUTINE DA_UV_To_Vorticity
