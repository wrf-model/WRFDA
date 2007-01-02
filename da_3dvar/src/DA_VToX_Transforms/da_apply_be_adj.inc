SUBROUTINE da_apply_be_adj( be, cv, vp ,xb,xp,&
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

   IMPLICIT NONE

   TYPE (be_type), INTENT(IN)           :: be   ! Background error structure.
   type (xb_type), intent(in)           :: xb   ! first guess.
   REAL, INTENT(INOUT)                  :: cv(:)! Control variable.
   TYPE (vp_type), INTENT(INOUT)        :: vp   ! Grid point/EOF equivalent.
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers.
   integer, intent(in)  :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)  :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)  :: its,ite, jts,jte, kts,kte ! tile   dims.

   INTEGER                              :: i,j,k,ijk,ij,iunit

!-------------------------------------------------------------------------
!  [2.0] Transform from cv to vp:
!-------------------------------------------------------------------------

   CALL DA_apply_rf_adj( be, vp, xp, &
                        ids,ide, jds,jde, kds,kde, &
                        ims,ime, jms,jme, kms,kme, &
                        its,ite, jts,jte, kts,kte )

!  [2.1] Transform control variable:
!!!!!!!!!!!!!!!!!!!!!!!

   do k=kts,kte
   do j=jts,jte
   do i=its,ite
      vp % v1(i,j,k)=vp % v1(i,j,k)*be % corz(i,j,k,1)
      vp % v2(i,j,k)=vp % v2(i,j,k)*be % corz(i,j,k,2)
      vp % v3(i,j,k)=vp % v3(i,j,k)*be % corz(i,j,k,3)
      vp % v4(i,j,k)=vp % v4(i,j,k)*be % corz(i,j,k,4)
   enddo
   enddo         
   enddo         

!-----Transform 5th control variable
      k=1
   do j=jts,jte
      do i=its,ite
         vp % v5(i,j,k)=vp % v5(i,j,k)*be % corp(i,j)
      enddo
   enddo         

!-------------------------------------------------------------------------
!  [1.0] Make global-grid copy of cc from 3-dimensional local-grid vv.
!-------------------------------------------------------------------------

   call vv_to_cv( vp, xp, be, cv_size, cv )

END SUBROUTINE da_apply_be_adj

