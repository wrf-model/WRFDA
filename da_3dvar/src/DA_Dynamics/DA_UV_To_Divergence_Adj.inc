SUBROUTINE DA_UV_To_Divergence_adj( xp, u, v, coefx, coefy, div, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
 
!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of the subroutine DA_UV_To_Divergence
!
!  HISTORY: 09/28/2002 - Qingnong Xiao
!           02/02/2005 - Global update       Syed RH Rizvi 
!
!------------------------------------------------------------------------------

   implicit none

   TYPE (xpose_type), INTENT(INOUT) :: xp      ! Dimensions and xpose buffers.

   integer, intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
   integer, intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
   integer, intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.

   real, intent(in)   :: coefx(ims:ime,jms:jme)       ! Multiplicative const.
   real, intent(in)   :: coefy(ims:ime,jms:jme)       ! Multiplicative const.
   real, intent(out)  :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(out)  :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
   real, intent(inout):: div(ims:ime,jms:jme,kms:kme) ! Divergence.

   integer            :: i, j, k                      ! Loop counters.
   integer            :: is, ie                       ! 1st dim. end points.
   integer            :: js, je                       ! 2nd dim. end points.
   real               :: one_third                    ! 1/3.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

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

      endif

!-------------------------------------------------------------------------
!     [2.1] Compute fd divergence at interior points:
!-------------------------------------------------------------------------

    do j = je, js, -1
       do i = ie, is, -1  
          u(i+1,j,k) = u(i+1,j,k) + coefx(i,j) * div(i,j,k) 
          u(i-1,j,k) = u(i-1,j,k) - coefx(i,j) * div(i,j,k) 
          v(i,j+1,k) = v(i,j+1,k) + coefy(i,j) * div(i,j,k) 
          v(i,j-1,k) = v(i,j-1,k) - coefy(i,j) * div(i,j,k) 
       enddo
    enddo
   END DO

   div = 0.0

END SUBROUTINE DA_UV_To_Divergence_adj

