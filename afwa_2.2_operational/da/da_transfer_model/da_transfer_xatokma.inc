subroutine da_transfer_xatokma(grid)

   !---------------------------------------------------------------------------
   !  Purpose: Convert analysis increments into KMA increments 
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain),    intent(inout) :: grid

   integer :: i, j, k
   integer :: is, ie, js, je, ks, ke
   real    :: PU, PD, coeff

   if (trace_use) call da_trace_entry("da_transfer_xatokma")

   !------------------------------------------------------------------------
   ! Set array range indices for processor subdomain.
   !------------------------------------------------------------------------

   is = grid%xp % its
   ie = grid%xp % ite
   js = grid%xp % jts
   je = grid%xp % jte
   ks = grid%xp % kts
   ke = grid%xp % kte

   !---------------------------------------------------------------------------
   ! Add increment to the original guess and update xb and "grid"
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         grid%xb%w(i,j,ke+1)=  grid%xb%w(i,j,ke+1) + grid%xa%w(i,j,ke+1)
      end do
      do i=is,ie
         do k = ks, ke
            grid%xb%u(i,j,k)   = grid%xa%u(i,j,k) + grid%xb%u(i,j,k)
            grid%xb%v(i,j,k)   = grid%xa%v(i,j,k) + grid%xb%v(i,j,k)
            grid%xb%t(i,j,k)   = grid%xa%t(i,j,k) + grid%xb%t(i,j,k)
            grid%xb%w(i,j,k)   = grid%xa%w(i,j,k) + grid%xb%w(i,j,k)
            grid%xb%q(i,j,k)   = grid%xa%q(i,j,k) + grid%xb%q(i,j,k)
            ! compute pressure increments at KMA full levels
            ! Note: Psfc is in hPa in  P = A + B * Psfc 
            if (k == ke) then
               coeff = grid%xb%KMA_B(K)/ &
                  (grid%xb%KMA_A(K)+grid%xb%KMA_B(K)*grid%xb%psfc(I,J)/100.0)
            else
               PU = grid%xb%KMA_A(K+1) + &
                  grid%xb%KMA_B(K+1)*grid%xb%psfc(I,J)/100.0
               PD = grid%xb%KMA_A(K ) + &
                  grid%xb%KMA_B(K )*grid%xb%psfc(I,J)/100.0
               coeff=grid%xb%KMA_B(K) * &
                  1.0/(PD-PU)**2*(-PU*(LOG(PD)-LOG(PU))+PD-PU) &
                  + grid%xb%KMA_B(K+1)* &
                  1.0/(PD-PU)**2*(PD*(LOG(PD)-LOG(PU))-PD+PU)
            end if
            grid%xa%p(i,j,k) = grid%xa%psfc(i,j) * coeff
            grid%xa%p(i,j,k) = grid%xb%psfc(i,j)*grid%xa%psfc(i,j)
            grid%xb%p(i,j,k) = grid%xb%p(i,j,k) + grid%xa%p(I,J,k)
         end do
         grid%xb%psfc(i,j) = grid%xb%psfc(i,j) + grid%xa%psfc(i,j)
      end do
   end do

   if (write_increments) call da_write_kma_increments(grid%xp, grid%xb, grid%xa)

   do j=js,je
      do i=is,ie
        grid%em_w_2(i,j,ke+1)=  grid%em_w_2(i,j,ke+1) + grid%xa%w(i,j,ke+1)
        grid%psfc(i,j) = grid%psfc(i,j) + grid%xa%psfc(i,j)
      end do
   end do

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%em_u_2(i,j,k) = grid%em_u_2(i,j,k) + grid%xa%u(i,j,k)
            grid%em_v_2(i,j,k) = grid%em_v_2(i,j,k) + grid%xa%v(i,j,k)
            grid%em_w_2(i,j,k) = grid%em_w_2(i,j,k) + grid%xa%w(i,j,k)
            grid%moist(i,j,k,P_QV) = grid%moist(i,j,k,P_QV) + grid%xa%q(i,j,k)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_transfer_xatokma")

end subroutine da_transfer_xatokma


