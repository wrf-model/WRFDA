subroutine DA_Transform_VvToVp( evec, eval, vertical_wgt, &
                                vv, vp, mz, &
                                ids,ide, jds,jde, kds,kde,  &
                                ims,ime, jms,jme, kms,kme,  &
                                its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Transform from fields on vertical EOFS to fields on vertical levels.
!
!  METHOD:  Perform vp(i,j,k) = P E L^{1/2} vv(i,j,m) transform.
!
!  HISTORY: 02/23/2000 - Creation of F90 version.           Dale Barker
!           10/30/2001 - Parallel version.                  Dale Barker
!------------------------------------------------------------------------------

   implicit none
   
   integer, intent(in)   :: mz                         ! # vertical modes.
   integer, intent(in)   :: ids,ide, jds,jde, kds,kde  ! domain dims.
   integer, intent(in)   :: ims,ime, jms,jme, kms,kme  ! memory dims.
   integer, intent(in)   :: its,ite, jts,jte, kts,kte  ! tile   dims

   real, intent(in)      :: evec(jds:jde,kds:kde,1:mz) ! Eigenvectors.
   real, intent(in)      :: eval(jds:jde,1:mz)         ! Eigenvalues.
   real, intent(in)      :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   real, intent(in)      :: vv(ims:ime,jms:jme,kms:kme)   ! CV in EOF space.
   real, intent(out)     :: vp(ims:ime,jms:jme,kms:kme)! CV in level space.
   
   integer               :: i, j, k, m                 ! Loop counters.
   real                  :: temp

!-------------------------------------------------------------------
!  [1.0] Perform vp(i,j,k) = E L^{1/2} vv(i,j,m) transform:
!------------------------------------------------------------------- 

   vp = 0.0
   DO k = kts, kte
      DO m = 1, mz
         DO j = jts, jte
            temp = evec(j,k,m) * eval(j,m)

            DO i = its, ite
               vp(i,j,k) = vp(i,j,k) + temp*vv(i,j,m)
            END DO
         END DO
      END DO
   END DO
   
!-------------------------------------------------------------------
!  [2.0] Apply inner-product weighting if vertical_ip > 0:
!------------------------------------------------------------------- 

   IF ( vertical_ip /= 0 ) THEN

      vp(its:ite,jts:jte,kts:kte) = vp(its:ite,jts:jte,kts:kte) / &
                                    vertical_wgt(its:ite,jts:jte,kts:kte)
                           
   END IF

END subroutine DA_Transform_VvToVp

