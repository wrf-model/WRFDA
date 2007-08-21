subroutine da_check_xtoy_adjoint_geoamv (iv, y, adjtest_lhs, pertile_lhs)

   !-------------------------------------------------------------------------
   ! Purpose:  Adjoint Test for Geo. AMVs Obs
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer                        :: n, k          ! Loop counter.

   if (iv%nlocal(geoamv) > 0) then
      do n=1, iv%nlocal(geoamv)
         if (iv%geoamv(n)%loc%proc_domain) then
            do k=1, iv%geoamv(n)%info%levels
               adjtest_lhs = adjtest_lhs + &
                           (y%geoamv(n)%u(k)/typical_u_rms)**2 + &
                           (y%geoamv(n)%v(k)/typical_v_rms)**2

            end do
         end if

         do k=1, iv%geoamv(n)%info%levels
            pertile_lhs = pertile_lhs + &
                        (y%geoamv(n)%u(k)/typical_u_rms)**2 + &
                        (y%geoamv(n)%v(k)/typical_v_rms)**2

            y%geoamv(n)%u(k)= y%geoamv(n)%u(k) / typical_u_rms ** 2
            y%geoamv(n)%v(k)= y%geoamv(n)%v(k) / typical_v_rms ** 2
         end do
      end do
   end if

end subroutine da_check_xtoy_adjoint_geoamv


