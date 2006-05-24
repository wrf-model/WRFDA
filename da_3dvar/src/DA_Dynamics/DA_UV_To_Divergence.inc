SUBROUTINE DA_UV_To_Divergence( xb, u, v, div, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
 
!------------------------------------------------------------------------------
!  PURPOSE: Calculate divergence on a co-ordinate surface, given an input
!           wind field.
!
!
!  HISTORY: 09/28/1999 - Creation of F90 version.           Dale Barker
!           02/02/2005 - Global update       Syed RH Rizvi
!           06/02/2005 - add correct divergence for regional. Y.-R. Guo
!.
!                        d   U      d   V
!           Div = m^2 *[---(---) + ---(---) ] 
!                        dx  m      dy  M
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
   integer, intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
   integer, intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims

   TYPE (xb_type), INTENT(IN)           :: xb         ! First guess structure.
   real, intent(in)   :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(in)   :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
   real, intent(inout):: div(ims:ime,jms:jme,kms:kme) ! Divergence.

   integer            :: i, j, k                      ! Loop counters.
   integer            :: is, ie                       ! 1st dim. end points.
   integer            :: js, je                       ! 2nd dim. end points.
   integer            :: ks, ke                       ! 3rd dim. end points.
   real               :: one_third                    ! 1/3.

   real               :: coeff, inv_2ds
   real               :: um(ims:ime,jms:jme)          ! Temp. storage of u/m.
   real               :: vm(ims:ime,jms:jme)          ! Temp. storage of v/m.
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------
   one_third = 1.0 / 3.0
   div = 0.0

!------------------------------------------------------------------------------
!  Computation to check for edge of domain:
!------------------------------------------------------------------------------
   is = its; ie = ite; js = jts; je = jte
   if ( .not. global .and. its == ids ) is = ids+1  
   if ( .not. global .and. ite == ide ) ie = ide-1
   if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1
  
   if ( .not.global ) inv_2ds = 0.5 / xb%ds

!------------------------------------------------------------------------------
!  [2.0] Calculate divergence:
!------------------------------------------------------------------------------
   DO k = kts, kte
   
!     [2.1] Compute fd divergence at interior points:

   if ( global ) then
 
      DO j = js, je
      DO i = is, ie
         div(i,j,k) = xb%coefx(i,j) * (u(i+1,j,k) - u(i-1,j,k) ) + &
                      xb%coefy(i,j) * (v(i,j+1,k) - v(i,j-1,k) )
      END DO
      END DO

!      if( global ) cycle
   else

      um(is-1:ie+1,js-1:je+1) = u(is-1:ie+1,js-1:je+1,k) / xb%map_factor(is-1:ie+1,js-1:je+1)
      vm(is-1:ie+1,js-1:je+1) = v(is-1:ie+1,js-1:je+1,k) / xb%map_factor(is-1:ie+1,js-1:je+1)

!     [2.1] Compute fd divergence at interior points:

      DO j = js, je
      DO i = is, ie
         coeff = xb%map_factor(i,j) * xb%map_factor(i,j) * inv_2ds
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
   
    endif

    END DO

   if( global ) then
      call da_set_boundary_3d(div, &
                              ids,ide, jds,jde, kds,kde,  &
                              ims,ime, jms,jme, kms,kme,  &
                              its,ite, jts,jte, kts,kte )
   endif

END SUBROUTINE DA_UV_To_Divergence

