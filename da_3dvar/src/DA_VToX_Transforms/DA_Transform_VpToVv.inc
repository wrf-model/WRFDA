SUBROUTINE DA_Transform_VpToVv( evec, eval, vertical_wgt, &
                                vp, vv, mz, &
                                ids,ide, jds,jde, kds,kde,  &
                                ims,ime, jms,jme, kms,kme,  &
                                its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Transform from fields on vertical levels to fields on vertical EOFS.
!
!  METHOD:  Perform vv(i,j,m) = L^{-1/2} E^{T} vp(i,j,k) transform.
!
!  HISTORY: 02/23/2000 - Creation of F90 version.           Dale Barker
!           10/30/2001 - Parallel version.                  Dale Barker
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in)   :: mz                         ! # vertical modes.
   integer, intent(in)   :: ids,ide, jds,jde, kds,kde  ! domain dims.
   integer, intent(in)   :: ims,ime, jms,jme, kms,kme  ! memory dims.
   integer, intent(in)   :: its,ite, jts,jte, kts,kte  ! tile   dims
   real, intent(in)      :: evec(jts:jte,kds:kde,1:mz) ! Eigenvectors.
   real, intent(in)      :: eval(jts:jte,1:mz)         ! Eigenvalues.
   real, intent(in)      :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   real, intent(inout)   :: vp(ims:ime,jms:jme,kms:kme)! CV in level space.
   real, intent(out)     :: vv(ims:ime,jms:jme,1:mz)   ! CV in EOF space.
   
   INTEGER               :: i, j, m                    ! Loop counters.
   REAL                  :: ETVp                       ! E(k,m)^{T}*vp(i,j,k)
   
!-------------------------------------------------------------------
!  [1.0] Apply inner-product weighting if vertical_ip > 0:
!------------------------------------------------------------------- 

   IF ( vertical_ip /= 0 ) THEN
      vp(its:ite,jts:jte,kts:kte) = vp(its:ite,jts:jte,kts:kte) * &
                                    vertical_wgt(its:ite,jts:jte,kts:kte)
   END IF

!-------------------------------------------------------------------
!  [2.0] Perform vv(i,j,m) = L^{-1/2} E^T vp(i,j,k) transform:
!-------------------------------------------------------------------

   do m = 1, mz
      do j = jts, jte
         do i = its, ite
            ETVp = SUM( evec(j,kts:kte,m) * vp(i,j,kts:kte) )
            vv(i,j,m) = ETVp / eval(j,m)
         end do
      end do
   end do

END SUBROUTINE DA_Transform_VpToVv

