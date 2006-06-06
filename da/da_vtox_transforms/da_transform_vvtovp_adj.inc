SUBROUTINE da_transform_vvtovp_adj( evec, eval, vertical_wgt, &
                                    vp, vv, mz, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_Transform_VvToVp.
!------------------------------------------------------------------------------

   implicit none
   
   integer, intent(in)   :: mz                         ! # vertical modes.
   integer, intent(in)   :: ids,ide, jds,jde, kds,kde  ! domain dims.
   integer, intent(in)   :: ims,ime, jms,jme, kms,kme  ! memory dims.
   integer, intent(in)   :: its,ite, jts,jte, kts,kte  ! tile   dims

   real, intent(in)      :: evec(jds:jde,kds:kde,1:mz) ! Eigenvectors.
   real, intent(in)      :: eval(jds:jde,1:mz)         ! Eigenvalues.
   real, intent(in)      :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   real, intent(inout)   :: vp(ims:ime,jms:jme,kms:kme)! CV in level space.
   real, intent(out)     :: vv(ims:ime,jms:jme,kms:kme)! CV in EOF space.
 
   INTEGER               :: i, j, m, k                 ! Loop counters.
   REAL                  :: temp

  IF (trace_use) CALL da_trace_entry("da_transform_vvtovp_adj")


!-------------------------------------------------------------------
!  [1.0] Apply inner-product weighting if vertical_ip > 0:
!------------------------------------------------------------------- 

   IF ( vertical_ip /= 0 ) THEN

      vp(its:ite,jts:jte,kts:kte) = vp(its:ite,jts:jte,kts:kte) / &
                                    vertical_wgt(its:ite,jts:jte,kts:kte)

   END IF

!-------------------------------------------------------------------
!  [2.0] Perform vp(i,j,k) = E L^{1/2} vv(i,j,m) transform:
!------------------------------------------------------------------- 

   vv = 0.0
   DO m = 1, mz
      DO k = kts, kte
         DO j = jts, jte
            temp = evec(j,k,m) * eval(j,m)

            DO i = its, ite
               vv(i,j,m) = vv(i,j,m) + temp*vp(i,j,k)
            END DO
         END DO
      END DO
   END DO

  IF (trace_use) CALL da_trace_exit("da_transform_vvtovp_adj")


END SUBROUTINE da_transform_vvtovp_adj

