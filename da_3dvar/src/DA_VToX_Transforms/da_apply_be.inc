SUBROUTINE da_apply_be( be, cv, vp ,xb ,xp,&
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

   IMPLICIT NONE

   TYPE (be_type), INTENT(IN)           :: be   ! Background error structure.
   type (xb_type), intent(in)           :: xb   ! first guess.
   REAL, INTENT(IN)                     :: cv(:)! Control variable.
   TYPE (vp_type), INTENT(INOUT)        :: vp   ! Grid point/EOF equivalent.
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers.
   integer, intent(in)  :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)  :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)  :: its,ite, jts,jte, kts,kte ! tile   dims.

   INTEGER                              :: i,j,k

!-------------------------------------------------------------------------
!  [1.0] Make local-grid copy of vp from 1-dimensional global-grid cv.
!-------------------------------------------------------------------------

   call cv_to_vv( cv_size, cv, xp, be, vp )

!  [2.0] Transform control variable:

      do k=xp%kts,xp%kte
      do j=xp%jts,xp%jte
      do i=xp%its,xp%ite
         vp % v1(i,j,k)=vp % v1(i,j,k)*be % corz(i,j,k,1)
         vp % v2(i,j,k)=vp % v2(i,j,k)*be % corz(i,j,k,2)
         vp % v3(i,j,k)=vp % v3(i,j,k)*be % corz(i,j,k,3)
         vp % v4(i,j,k)=vp % v4(i,j,k)*be % corz(i,j,k,4)
      enddo
      enddo
      enddo

!-----Transform 5th control variable
      k=1
      do j=xp%jts,xp%jte
      do i=xp%its,xp%ite
         vp % v5(i,j,k)=vp % v5(i,j,k)*be % corp(i,j)
      enddo
      enddo

   CALL DA_apply_rf( be, vp , xp, &
                     ids,ide, jds,jde, kds,kde, &
                     ims,ime, jms,jme, kms,kme, &
                     its,ite, jts,jte, kts,kte )


END SUBROUTINE da_apply_be          

