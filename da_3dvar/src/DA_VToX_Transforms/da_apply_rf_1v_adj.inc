SUBROUTINE da_apply_rf_1v_adj( be, vp, xp, nv, &
                            ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte )              

   IMPLICIT NONE

   TYPE (be_type), INTENT(IN)       :: be   ! Background error structure.
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers.

   integer, intent(in) :: nv                        ! # of var.
   integer, intent(in) :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in) :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in) :: its,ite, jts,jte, kts,kte ! tile   dims.

   real, dimension(ims:ime, jms:jme, kms:kme), INTENT(INOUT) :: vp   ! working array

   integer             :: in, jn, kn, k

!-------------------------------------------------------------------------
!  [1.0] Perform 1D recursive filter in x-y direction:
!-------------------------------------------------------------------------

   do k = kts,kte
      xp % v1z(its:ite,jts:jte,k) = vp(its:ite,jts:jte,k)
   end do


   call transpose_z2y ( xp )

   in=xp%ipey-xp%ipsy
   jn=xp%jpey-xp%jpsy

   do k=xp%kpsy,xp%kpey
      call smoothy(in,jn,xp % v1y(xp%ipsy:xp%ipey,xp%jpsy:xp%jpey,k),&
                   be%sljy(xp%ipsy:xp%ipey,xp%jpsy:xp%jpey,k,nv),be%ndeg,&
                   be%be,be%nta,be%swidth,be%table)
   enddo


   call transpose_y2x ( xp )

   in=xp%ipex-xp%ipsx
   jn=xp%jpex-xp%jpsx

   do k=xp%kpsx,xp%kpex
      call smoothx(in,jn,xp % v1x(xp%ipsx:xp%ipex,xp%jpsx:xp%jpex,k),&
                   be%slix(xp%ipsx:xp%ipex,xp%jpsx:xp%jpex,k,nv),be%ndeg,&
                   be%be,be%nta,be%swidth,be%table)
   enddo

   call transpose_x2z ( xp )

   do k = kts,kte
      vp(its:ite,jts:jte,k)= xp % v1z(its:ite,jts:jte,k)
   end do

!-------------------------------------------------------------------------
!  [2.0]: Perform 1D recursive filter in z direction:
!-------------------------------------------------------------------------

   in=ite-its+1
   jn=jte-jts+1
   kn=kte-kts+1

   call da_rfz(vp(ims:ime,jms:jme,kms:kme),in,jn,kn,be%ndeg,&
     be%vz(kts:kte,its:ite,jts:jte,nv),be%be,be%table,be%nta,be%swidth,&
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )

END SUBROUTINE da_apply_rf_1v_adj

