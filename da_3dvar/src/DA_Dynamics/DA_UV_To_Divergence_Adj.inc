SUBROUTINE DA_UV_To_Divergence_adj( xp, xb, u, v, div, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
 
!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of the subroutine DA_UV_To_Divergence
!
!  HISTORY: 09/28/2002 - Qingnong Xiao
!           02/02/2005 - Global update       Syed RH Rizvi 
!
!           06/02/2005 - add correct divergence for regional. Y.-R. Guo
!.
!                        d   U      d   V
!           Div = m^2 *[---(---) + ---(---) ] 
!                        dx  m      dy  M
!------------------------------------------------------------------------------

   implicit none

   TYPE (xpose_type), INTENT(INOUT) :: xp      ! Dimensions and xpose buffers.

   integer, intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
   integer, intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
   integer, intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.

   TYPE (xb_type), INTENT(IN)           :: xb         ! First guess structure.
   real, intent(out)  :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(out)  :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
   real, intent(inout):: div(ims:ime,jms:jme,kms:kme) ! Divergence.

   integer            :: i, j, k                      ! Loop counters.
   integer            :: is, ie                       ! 1st dim. end points.
   integer            :: js, je                       ! 2nd dim. end points.
   real               :: one_third                    ! 1/3.

   real               :: coeff, inv_2ds
   real               :: um(ims:ime,jms:jme)          ! Temp. storage of u/m.
   real               :: vm(ims:ime,jms:jme)          ! Temp. storage of v/m

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------
   if ( .not. global ) then
      um(ims:ime,jms:jme) = 0.
      vm(ims:ime,jms:jme) = 0.
      inv_2ds = 0.5 / xb%ds
   endif

   one_third = 1.0 / 3.0

   if(Testing_3DVAR) then
      is = its; ie = ite; js = jts; je = jte
   else
      is = its - 1; ie = ite + 1; js = jts - 1; je = jte + 1
   endif

   if ( .not. global .and. its == ids ) is = ids+1
   if ( .not. global .and. ite == ide ) ie = ide-1
   if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

!------------------------------------------------------------------------------
!  [2.0] Calculate divergence:
!------------------------------------------------------------------------------

   if( global ) then
      call da_set_boundary_3d(div, &
                              ids,ide, jds,jde, kds,kde,  &
                              ims,ime, jms,jme, kms,kme,  &
                              its,ite, jts,jte, kts,kte )
   endif

   DO k =kds, kde
      if( .not. global ) then
!     [2.2] Impose zero divergence gradient condition at boundaries:

!     [2.2.4] Right boundaries:

      if ( jte == jde ) then
         j = jte
         do i = its, ite    ! This is different to original
            div(i,j-1,k)=div(i,j-1,k)+div(i,j,k)*one_third*4.0
            div(i,j-2,k)=div(i,j-2,k)-div(i,j,k)*one_third
            div(i,j,k)=0.
         end do
      end if

!     [2.2.3] Left boundaries:

      if ( jts == jds ) then
         j = jts
         do i = its, ite    ! This is different to original
            div(i,j+1,k)=div(i,j+1,k)+div(i,j,k)*one_third*4.0
            div(i,j+2,k)=div(i,j+2,k)-div(i,j,k)*one_third
            div(i,j,k)=0.
         end do
      end if

!     [2.2.2] Top boundaries:

      if ( ite == ide ) then
         i = ite
         do j = jts, jte
            div(i-1,j,k)=div(i-1,j,k)+div(i,j,k)*one_third*4.0
            div(i-2,j,k)=div(i-2,j,k)-div(i,j,k)*one_third
            div(i,j,k)=0.
         end do
      end if

!     [2.2.1] Bottom boundaries:

      if ( its == ids ) then
         i = its 
         do j = jts, jte
            div(i+1,j,k)=div(i+1,j,k)+div(i,j,k)*one_third*4.0
            div(i+2,j,k)=div(i+2,j,k)-div(i,j,k)*one_third
            div(i,j,k)=0.
         end do
      end if

!     [2.1] Compute fd divergence at interior points:
!-------------------------------------------------------------------------
!-----Computation to check for edge of domain:
!     This is only for adjoint, as we have to cross the processor boundary
!     to get the contribution.

      if(.not. Testing_3DVAR) then
         xp%vxy(its:ite, jts:jte) = div(its:ite, jts:jte, k)
         call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id12)

         div(is, js:je, k) = xp%vxy(is, js:je)
         div(ie, js:je, k) = xp%vxy(ie, js:je)
         div(is:ie, js, k) = xp%vxy(is:ie, js)
         div(is:ie, je, k) = xp%vxy(is:ie, je)
      endif
!-------------------------------------------------------------------------

      DO j = js, je
         DO i = is, ie
           coeff = xb%map_factor(i,j) * xb%map_factor(i,j) * inv_2ds
           um(i+1,j)=um(i+1,j)+div(i,j,k)*coeff
           um(i-1,j)=um(i-1,j)-div(i,j,k)*coeff
           vm(i,j+1)=vm(i,j+1)+div(i,j,k)*coeff
           vm(i,j-1)=vm(i,j-1)-div(i,j,k)*coeff
        END DO
      END DO
   
      u(is-1:ie+1,js-1:je+1,k) = u(is-1:ie+1,js-1:je+1,k) &
                               +um(is-1:ie+1,js-1:je+1) / xb%map_factor(is-1:ie+1,js-1:je+1)
      v(is-1:ie+1,js-1:je+1,k) = v(is-1:ie+1,js-1:je+1,k) &
                               +vm(is-1:ie+1,js-1:je+1) / xb%map_factor(is-1:ie+1,js-1:je+1)

      um(:,:)=0.
      vm(:,:)=0.

      else

!-------------------------------------------------------------------------
!     [2.1] Compute fd divergence at interior points:
!-------------------------------------------------------------------------

    do j = je, js, -1
       do i = ie, is, -1  
          u(i+1,j,k) = u(i+1,j,k) + xb%coefx(i,j) * div(i,j,k) 
          u(i-1,j,k) = u(i-1,j,k) - xb%coefx(i,j) * div(i,j,k) 
          v(i,j+1,k) = v(i,j+1,k) + xb%coefy(i,j) * div(i,j,k) 
          v(i,j-1,k) = v(i,j-1,k) - xb%coefy(i,j) * div(i,j,k) 
       enddo
    enddo


      endif

   END DO

   div = 0.0

END SUBROUTINE DA_UV_To_Divergence_adj

