SUBROUTINE da_transfer_xatokma( xbx, grid, &
!
#include "em_dummy_args.inc"
!
                              )
!------------------------------------------------------------------------------
!  PURPOSE: Convert analysis increments into KMA increments 
!
!  HISTORY: 08/06/2004	 Syed RH Rizvi  
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xbx_type), INTENT(INOUT)     :: xbx          ! Header & non-gridded vars.

   TYPE(domain), TARGET               :: grid

!  Definitions of dummy arguments to solve

#include "em_dummy_decl.inc"

!---------------------------------------------------------------------------
   integer :: i, j, k, n
   integer :: is, ie, js, je, ks, ke
   real    :: sdmd, s1md
   real    :: PU, PD, coeff

   IF (trace_use) CALL da_trace_entry("da_transfer_xatokma")

!---------------------------------------------------------------------------
!--SET ARRAY RANGE INDICES FOR PROCESSOR SUBDOMAIN.
!---------------------------------------------------------------------------

   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte
!------------------------------------------------------------------------------
!  ADD INCREMENT TO THE ORIGINAL GUESS AND UPDATE xb and "grid"
!------------------------------------------------------------------------------
   do j=js,je
      do i=is,ie
         xb%w(i,j,ke+1)=  xb%w(i,j,ke+1) + xa%w(i,j,ke+1)
      enddo
   do i=is,ie
     do k = ks, ke
      xb%u(i,j,k)   = xa%u(i,j,k) + xb%u(i,j,k)
      xb%v(i,j,k)   = xa%v(i,j,k) + xb%v(i,j,k)
      xb%t(i,j,k)   = xa%t(i,j,k) + xb%t(i,j,k)
      xb%w(i,j,k)   = xa%w(i,j,k) + xb%w(i,j,k)
      xb%q(i,j,k)   = xa%q(i,j,k) + xb%q(i,j,k)
! compute pressure increments at KMA full levels
! Note: Psfc is in hPa in  P = A + B * Psfc 
       if(k == ke) then
        coeff = xb%KMA_B(K)/(xb%KMA_A(K)+xb%KMA_B(K)*xb%psfc(I,J)/100.0)
       else
          PU = xb%KMA_A(K+1) + xb%KMA_B(K+1)*xb%psfc(I,J)/100.0
          PD = xb%KMA_A(K  ) + xb%KMA_B(K  )*xb%psfc(I,J)/100.0
          coeff=xb%KMA_B(K)  *1.0/(PD-PU)**2*(-PU*(LOG(PD)-LOG(PU))+PD-PU)&
              + xb%KMA_B(K+1)*1.0/(PD-PU)**2*( PD*(LOG(PD)-LOG(PU))-PD+PU)
       end if
       xa%p(i,j,k) = xa%psfc(i,j) * coeff
       xa%p(i,j,k) = xb%psfc(i,j)*xa%psfc(i,j)
       xb%p(i,j,k) = xb%p(i,j,k) + xa%p(I,J,k)
     enddo
       xb%psfc(i,j) = xb%psfc(i,j) + xa%psfc(i,j)
   enddo
   enddo
 if(write_increments) call da_write_kma_increments(xp, xb, xa)

   do j=js,je
      do i=is,ie
        w_2(i,j,ke+1)=  w_2(i,j,ke+1) + xa%w(i,j,ke+1)
        psfc(i,j) = psfc(i,j) + xa%psfc(i,j)
      enddo
   end do

   do k=ks,ke
      do j=js,je
      do i=is,ie
         u_2(i,j,k) = u_2(i,j,k) + xa%u(i,j,k)
         v_2(i,j,k) = v_2(i,j,k) + xa%v(i,j,k)
         w_2(i,j,k) = w_2(i,j,k) + xa%w(i,j,k)
         moist(i,j,k,P_QV) = moist(i,j,k,P_QV) + xa%q(i,j,k)
      enddo
      enddo
   enddo

   IF (trace_use) CALL da_trace_exit("da_transfer_xatokma")


END SUBROUTINE da_transfer_xatokma
