SUBROUTINE DA_UV_To_Divergence( ds, map_factor, u, v, div, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
 
!------------------------------------------------------------------------------
!  PURPOSE: Calculate divergence on a co-ordinate surface, given an input
!           wind field.
!
!  METHOD:  div = m**2 ( d(u/m)/dx + d(v/m)/dy )
!
!  ASSUMPTIONS: dx = dy = ds
!
!  HISTORY: 09/28/1999 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
   integer, intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
   integer, intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.

   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: map_factor(ims:ime,jms:jme)  ! Map fact.
   real, intent(in)   :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(in)   :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
   real, intent(inout):: div(ims:ime,jms:jme,kms:kme) ! Divergence.

   integer            :: i, j, k                      ! Loop counters.
   integer            :: is, ie                       ! 1st dim. end points.
   integer            :: js, je                       ! 2nd dim. end points.
   integer            :: ks, ke                       ! 3rd dim. end points.
   real               :: inv_2ds                      ! 1/2ds.
   real               :: one_third                    ! 1/3.
   real               :: coeff
   real               :: um(ims:ime,jms:jme)          ! Temp. storage of u/m.
   real               :: vm(ims:ime,jms:jme)          ! Temp. storage of v/m.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   inv_2ds = 0.5 / ds
   one_third = 1.0 / 3.0

   div = 0.0

!  Computation to check for edge of domain:
   is = its; ie = ite; js = jts; je = jte
   if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
   if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

!------------------------------------------------------------------------------
!  [2.0] Calculate divergence:
!------------------------------------------------------------------------------

   DO k = kts, kte
   
      um(is-1:ie+1,js-1:je+1) = u(is-1:ie+1,js-1:je+1,k) / map_factor(is-1:ie+1,js-1:je+1)
      vm(is-1:ie+1,js-1:je+1) = v(is-1:ie+1,js-1:je+1,k) / map_factor(is-1:ie+1,js-1:je+1)

!     [2.1] Compute fd divergence at interior points:

      DO j = js, je
      DO i = is, ie
         coeff = map_factor(i,j) * map_factor(i,j) * inv_2ds
         div(i,j,k) = ( um(i+1,j) - um(i-1,j) + vm(i,j+1) - vm(i,j-1) ) * coeff
      END DO
      END DO
      
!     [2.2] Impose zero divergence gradient condition at boundaries:

!     [2.2.1] Bottom boundaries:

      if ( its == ids ) then
         i = its 
         do j = jts, jte
            div(i,j,k) = one_third * ( 4.0 * div(i+1,j,k) - div(i+2,j,k) )
         end do
      end if

!     [2.2.2] Top boundaries:

      if ( ite == ide ) then
         i = ite
         do j = jts, jte
            div(i,j,k) = one_third * ( 4.0 * div(i-1,j,k) - div(i-2,j,k) )
         end do
      end if

!     [2.2.3] Left boundaries:

      if ( jts == jds ) then
         j = jts
         do i = its, ite
            div(i,j,k) = one_third * ( 4.0 * div(i,j+1,k) - div(i,j+2,k) )
         end do
      end if

!     [2.2.4] right boundaries:

      if ( jte == jde ) then
         j = jte
         do i = its, ite
            div(i,j,k) = one_third * ( 4.0 * div(i,j-1,k) - div(i,j-2,k) )
         end do
      end if

   END DO

END SUBROUTINE DA_UV_To_Divergence

