SUBROUTINE DA_Test_Transforms (diff, xb, xbx, xp,  &
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!  PURPOSE: Test of Transform (Inverse Test) Using Analytical Solution
!
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   IMPLICIT NONE

   type (statistics_type), INTENT(INout) :: diff
   type (xb_type),         INTENT(IN) :: xb
   TYPE (xbx_type),   INTENT(IN)      :: xbx     ! Header & non-gridded vars.
   TYPE (xpose_type), INTENT(INOUT)   :: xp      ! Dimensions and xpose buffer

   integer, intent(in)          :: ids,ide, jds,jde, kds,kde
   integer, intent(in)          :: ims,ime, jms,jme, kms,kme
   integer, intent(in)          :: its,ite, jts,jte, kts,kte

   real :: rms_fild, rms_diff, inv_size

   real, dimension(ims:ime,jms:jme)                   :: coeff

   real, dimension(ims:ime,jms:jme,kms:kme) :: rdiff, u, v
   real, dimension(ims:ime,jms:jme,kms:kme) :: vor, psi
   real, dimension(ims:ime,jms:jme,kms:kme) :: div, chi

   integer :: i,j,k, nx,ny
   real    :: x,y

   coeff(its:ite,jts:jte) = 1.0 / ( xb % map_factor(its:ite,jts:jte) * &
                                       xb % map_factor(its:ite,jts:jte) )

!test.0915
   nx=ite-its !+1
   ny=jte-jts !+1
   print*,'nx,ny',nx,ny,'pi=',pi

   do k=kts,kte
   do j=jts,jte
   do i=its,ite
      x=pi/real(nx) * (i-1)
      y=pi/real(ny) * (j-1)
      diff%psi(i,j,k)=sin(2*x)*sin(3*y)
      diff%chi(i,j,k)=0.0
      diff%u(i,j,k)=0.0
      diff%v(i,j,k)=0.0
   enddo
   enddo
   enddo

   CALL DA_PsiChi_To_UV(diff % psi, diff % chi, xb % coefx, &
                        xb % coefy , diff % u, diff % v, &
                        ids,ide, jds,jde, kds,kde, &
                        ims,ime, jms,jme, kms,kme, &
                        its,ite, jts,jte, kts,kte )

#ifndef DEREF_KLUDGE
   CALL DA_UV_To_Vorticity( xb, diff % u, diff % v, vor, &
#else
   CALL DA_UV_To_Vorticity( xb, diff % u(ims,jms,kms), &
                            diff % v(ims,jms,kms), &
                            vor(ims,jms,kms), &
#endif
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

   DO k = kts, kte
     vor(its:ite,jts:jte,k) = coeff(its:ite,jts:jte) * &
                                  vor(its:ite,jts:jte,k)
   END DO


#ifndef DEREF_KLUDGE
   CALL DA_UV_To_Divergence(xb, diff % u, diff % v, div, &
#else
   CALL DA_UV_To_Divergence(xb, diff % u(ims,jms,kms), &
                                diff % v(ims,jms,kms), &
                            div(ims,jms,kms), &
#endif
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

   DO k = kts, kte
     div(its:ite,jts:jte,k) = coeff(its:ite,jts:jte) * &
                              div(its:ite,jts:jte,k)
   END DO

   CALL DA_Solve_PoissonEqn_FST(xb, xbx, xp, vor, psi,    &
                                    ids,ide, jds,jde, kds,kde,      &
                                    ims,ime, jms,jme, kms,kme,      &
                                    its,ite, jts,jte, kts,kte )


   CALL DA_Solve_PoissonEqn_FST(xb, xbx, xp, div, chi,    &
                                    ids,ide, jds,jde, kds,kde,      &
                                    ims,ime, jms,jme, kms,kme,      &
                                    its,ite, jts,jte, kts,kte )

   CALL DA_PsiChi_To_UV(psi, chi, xb % coefx, &
                        xb % coefy , u, v, &
                        ids,ide, jds,jde, kds,kde, &
                        ims,ime, jms,jme, kms,kme, &
                        its,ite, jts,jte, kts,kte )

!   do k=1,1 !kde
!   write(301,'(10e13.5)') ((u(i,j,k),i=1,ide),j=1,jde)
!   write(302,'(10e13.5)') ((v(i,j,k),i=1,ide),j=1,jde)
!   write(303,'(10e13.5)') ((psi(i,j,k),i=1,ide),j=1,jde)
!   write(304,'(10e13.5)') ((chi(i,j,k),i=1,ide),j=1,jde)

!   write(401,'(10e13.5)') ((vor(i,j,k),i=1,ide),j=1,jde)
!   write(402,'(10e13.5)') ((div(i,j,k),i=1,ide),j=1,jde)
!   enddo


!u inverse test
   rdiff(its:ite, jts:jte, kts:kte) = u(its:ite, jts:jte, kts:kte) &
                                   - diff% u(its:ite, jts:jte, kts:kte)

   rms_fild = SQRT( SUM( diff % u(its:ite, jts:jte, kts:kte) &
                       * diff % u(its:ite, jts:jte, kts:kte) ) )
   rms_diff = SQRT( SUM( rdiff(its:ite, jts:jte, kts:kte) &
                       * rdiff(its:ite, jts:jte, kts:kte) ) )

   write(unit=*, fmt='(a,1pe10.4)') ' u RMS error = ', rms_diff
   write(unit=*, fmt='(a,1pe10.4)') ' u RMS field = ', rms_fild
   write(unit=*, fmt='(a,1pe10.4)') ' u RMS error/RMS field = ', rms_diff/rms_fild

!v inverse test
   rdiff(its:ite, jts:jte, kts:kte) = v(its:ite, jts:jte, kts:kte) &
                                   - diff% v(its:ite, jts:jte, kts:kte)

   rms_fild = SQRT( SUM( diff % v(its:ite, jts:jte, kts:kte) &
                       * diff % v(its:ite, jts:jte, kts:kte) ) )
   rms_diff = SQRT( SUM( rdiff(its:ite, jts:jte, kts:kte) &
                       * rdiff(its:ite, jts:jte, kts:kte) ) )

   write(unit=*, fmt='(a,1pe10.4)') ' v RMS error = ', rms_diff
   write(unit=*, fmt='(a,1pe10.4)') ' v RMS field = ', rms_fild
   write(unit=*, fmt='(a,1pe10.4)') ' v RMS error/RMS field = ', rms_diff/rms_fild

END SUBROUTINE DA_Test_Transforms
