SUBROUTINE da_apply_rf_adj( be, vp , xp, &
                            ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte )              

   IMPLICIT NONE

   TYPE (be_type), INTENT(IN)       :: be   ! Background error structure.
   TYPE (vp_type), INTENT(INOUT)    :: vp   ! working array
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers.
   integer, intent(in) :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in) :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in) :: its,ite, jts,jte, kts,kte ! tile   dims.

   integer             :: i, j, k
   integer             :: in, jn, kn

!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v_adj( be, vp%v1, xp, 1, &
#else
   call da_apply_rf_1v_adj( be, vp%v1(ims,jms,kms), xp, 1, &
#endif
                            ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte )              

!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v_adj( be, vp%v2, xp, 2, &
#else
   call da_apply_rf_1v_adj( be, vp%v2(ims,jms,kms), xp, 2, &
#endif
                            ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte )              

!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v_adj( be, vp%v3, xp, 3, &
#else
   call da_apply_rf_1v_adj( be, vp%v3(ims,jms,kms), xp, 3, &
#endif
                            ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte )              

!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v_adj( be, vp%v4, xp, 4, &
#else
   call da_apply_rf_1v_adj( be, vp%v4(ims,jms,kms), xp, 4, &
#endif
                            ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte )              

!-------------------------------------------------------------------------
!  [1.0] Perform 1D recursive filter in x-y direction:
!-------------------------------------------------------------------------

   xp % v1z(its:ite,jts:jte,1) = vp%v5(its:ite,jts:jte,1)

   call transpose_z2y ( xp )

   in=xp%ipey-xp%ipsy
   jn=xp%jpey-xp%jpsy

   call smoothy(in,jn,xp % v1y(xp%ipsy:xp%ipey,xp%jpsy:xp%jpey,1),&
                be%sljpy(xp%ipsy:xp%ipey,xp%jpsy:xp%jpey),be%ndeg,&
                be%be,be%nta,be%swidth,be%table)

   call transpose_y2x ( xp )

   in=xp%ipex-xp%ipsx
   jn=xp%jpex-xp%jpsx

   call smoothx(in,jn,xp % v1x(xp%ipsx:xp%ipex,xp%jpsx:xp%jpex,1),&
                be%slipx(xp%ipsx:xp%ipex,xp%jpsx:xp%jpex),be%ndeg,&
                be%be,be%nta,be%swidth,be%table)

   call transpose_x2z ( xp )

   vp%v5(its:ite,jts:jte,1)= xp % v1z(its:ite,jts:jte,1)

!-------------------------------------------------------------------------

END SUBROUTINE da_apply_rf_adj      

