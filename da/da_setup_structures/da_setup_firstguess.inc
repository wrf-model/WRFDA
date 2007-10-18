subroutine da_convertor_v_interp( v_in, z_in, n_in, &
                                  v_out, z_out, n_out )

   implicit none

   integer,                intent(in)  :: n_in, n_out
   real, dimension(n_in),  intent(in)  :: v_in, z_in
   real, dimension(n_out), intent(in)  :: z_out
   real, dimension(n_out), intent(out) :: v_out

   integer :: k, kabv, kblw, i
   real    :: w1, w2

   logical :: increase_in_vertical

   ! does vertical coordinate increase or decrease with increasing k?
   ! set offset appropriately

   if (z_in(n_in) > z_in(1)) then
      increase_in_vertical = .true.
   else
      increase_in_vertical = .false.
   end if

   if (increase_in_vertical) then
      do k=1, n_out
         if (z_out(k) <= z_in(1)) then
            kabv = 2
         else if (z_out(k) >= z_in(n_in)) then
            kabv = n_in
         else
            i_loop: do i=2, n_in
               if (z_out(k) <= z_in(i)) then
                  kabv = i
                  exit i_loop
               end if
            end do i_loop
         end if

         kblw = kabv - 1
         w2 = (z_in(kabv)-z_out(k))/(z_in(kabv)-z_in(kblw))
         w1 = 1.0-w2
         v_out(k) = w1*v_in(kabv) + w2*v_in(kblw)
      end do
   else
      do k=1, n_out
         if (z_out(k) >= z_in(1)) then
            kabv = 2
         else if (z_out(k) <= z_in(n_in)) then
            kabv = n_in
         else
            d_loop: do i=2, n_in
               if (z_out(k) >= z_in(i)) then
                  kabv = i
                  exit d_loop
               end if
            end do d_loop
         end if

         kblw = kabv - 1
         w2 = (z_in(kabv)-z_out(k))/(z_in(kabv)-z_in(kblw))
         w1 = 1.0-w2
         v_out(k) = w1*v_in(kabv) + w2*v_in(kblw)
      end do
   end if

end subroutine da_convertor_v_interp

WRF C-grid format file:

      call da_setup_firstguess_wrf(xbx, grid)
   else if (fg_format == fg_format_kma_global) then
      ! First guess is an KMA format file:
      call da_setup_firstguess_kma(xbx, grid)
   end if

   !---------------------------------------------------------------------------
   ! Exchange halo region for XB arrays.
   !---------------------------------------------------------------------------

   if (fg_format == fg_format_wrf) then
      ! Calculate multiplicative constants for PsiChi_TO_UV 
      grid%xb%coefx(is:ie,js:je) = 0.5 * grid%xb%map_factor(is:ie,js:je)/grid%xb%ds
      grid%xb%coefy(is:ie,js:je) = grid%xb%coefx(is:ie,js:je)
      grid%xb%coefz(is:ie,js:je) = 0.5 / (grid%xb%map_factor(is:ie,js:je)*grid%xb%ds)
   else if (fg_format == fg_format_kma_global) then
      ! Calculate multiplicative constants for PsiChi_TO_UV 
      ddx =  earth_radius*1000 * 2.0 * pi / (grid%xb%ide-grid%xb%ids+1)
      ddy =  earth_radius*1000       * pi / (grid%xb%jde-grid%xb%jds)
      grid%xb% coefx(is:ie,js:je) = 0.5 / (ddx * cos(grid%xlat(is:ie,js:je)*pi/180.))
      grid%xb% coefy(is:ie,js:je) = 0.5 /  ddy
   else
      write(unit=message(1),fmt='(A,I5)') &
         "Wrong choice for fg_format = ",fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

#ifdef DM_PARALLEL
#include "HALO_INIT.inc"
#endif
   periodic_x = grid%periodic_x

   if (global) then     
      ! Set East-West boundary for Xb-array 
      call da_set_boundary_xb(grid)
   end if

   !---------------------------------------------------------------------------      
   ! [2.0] Setup grid-dependent constants used:
   !---------------------------------------------------------------------------

   ! [2.1] Set up fast Fourier & Legendre transform constants:

   call da_setup_runconstants(grid, xbx)

   if (trace_use) call da_trace_exit("da_setup_firstguess")

end subroutine da_setup_firstguess


