subroutine da_integrat_dz(xb)

   !---------------------------------------------------------------------------
   ! Non-linear PW forward operator.
   ! ===============================
   !
   ! Purpose: To calculate the IWV from the model QV and PP, TT.
   !
   ! Method:  IWV = sum {QV * RHO * dZ}
   !
   !           Unit: Qv (Kg/Kg), RHO(Kg/M^3), dZ(M)
   !                 PW (cm)
   !
   ! Input     : QV, PP, TT
   !
   ! Output    : PW
   !
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(inout) :: xb    ! first guess (local).

   integer :: i, j, K 

   real    :: pw

   ! WEIGHTED sum OF VERTICAL COLUMN 

   do j=xb%jts, xb%jte
      do i=xb%its, xb%ite
         pw = 0.0
         do k=xb%kts, xb%kte
            pw = pw + (xb%hf(i,j,k+1)-xb%hf(i,j,k)) &
                 *  xb%q(i,j,k)*xb%rho(i,j,k)
         end do

         xb%tpw(i,j) = 0.1*pw
      end do
   end do

end subroutine da_integrat_dz


