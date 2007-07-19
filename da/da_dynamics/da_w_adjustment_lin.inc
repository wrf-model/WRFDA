subroutine da_w_adjustment_lin(xb,W_a,WZ_a)

   !---------------------------------------------------------------------------
   ! Purpose: Adjust vertical velocity increments
   !
   ! Assumptions: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb                ! First guess structure.

   integer :: I,J,K

   real, dimension(ims:ime,jms:jme,kms:kme), intent(out) :: W_a
   real, dimension(ims:ime,jms:jme,kms:kme), intent(inout) :: WZ_a

   real, dimension(ims:ime,jms:jme,kms:kme) :: WZ_b

   real :: EBXL1,EBXL2
   real :: EBXL19,EBXL29

   call da_wz_base(xb,WZ_b)

   do J=jts,jte
      do I=its,ite
         EBXL1=0.0
         EBXL19=0.0
         EBXL2=0.0
         EBXL29=0.0
         do K=kte,kts,-1
            EBXL1=EBXL1+WZ_a(I,J,K)*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
            EBXL19=EBXL19+WZ_b(I,J,K)*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
            EBXL2=EBXL2+WZ_a(I,J,K)*(xb%hf(I,J,K)-xb%hf(I,J,K+1))   &
                  *SIGN(1.0,WZ_b(I,J,K))
            EBXL29=EBXL29+ABS(WZ_b(I,J,K))*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
         end do

         do K=kts,kte
            WZ_a(I,J,K)=WZ_a(I,J,K)*(1.-EBXL19/EBXL29*SIGN(1.0,WZ_b(I,J,K)))-  &
                        EBXL1*ABS(WZ_b(I,J,K))/EBXL29+                        &
                        EBXL2*ABS(WZ_b(I,J,K))*EBXL19/EBXL29**2
         end do

      end do
   end do

end subroutine da_w_adjustment_lin


