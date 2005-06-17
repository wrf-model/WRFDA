SUBROUTINE DA_Transform_VvToVp_Adj( evec, eval, vertical_wgt, &
                                    vp, vv, mz, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_Transform_VvToVp.
!
!  METHOD:  Standard adjoint coding.
!
!  HISTORY: 06/06/2000 - Creation of F90 version.           Dale Barker
!           09/18/2003 - Modified for WRF.                  Wei Huang
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

END SUBROUTINE DA_Transform_VvToVp_Adj

