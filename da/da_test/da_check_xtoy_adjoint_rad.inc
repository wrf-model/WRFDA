subroutine da_check_xtoy_adjoint_rad( iv, y, adjtest_lhs, pertile_lhs )

   !------------------------------------------------------------------------------
   ! PURPOSE: Calculate innovation vector for radiance data.
   !------------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer                        :: inst, n, k          ! Loop counter.

   if ( iv%num_inst < 1 ) return

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle

      do n= 1, iv%instid(inst)%num_rad       ! loop for pixel
         ! if (iv%instid(inst)%rad(n)%loc%proc_domain_with_halo) then
         if (iv%instid(inst)%rad(n)%loc%proc_domain) then
            do k = 1, iv%instid(inst)%nchan
               ! if ( iv%instid(inst)%rad(n)%tb(k)%qc >= obs_qc_pointer ) &
               adjtest_lhs = adjtest_lhs + &
                  (y%instid(inst)%rad(n)%tb(k)/iv%instid(inst)%rad(n)%tb(k)%error)**2
            end do
         endif

         do k=1, iv%instid(inst)%nchan
            ! if ( iv%instid(inst)%rad(n)%tb(k)%qc >= obs_qc_pointer ) &
            pertile_lhs = pertile_lhs + &
              (y%instid(inst)%rad(n)%tb(k)/iv%instid(inst)%rad(n)%tb(k)%error)**2

            y%instid(inst)%rad(n)%tb(k) =  &
              y%instid(inst)%rad(n)%tb(k) / (iv%instid(inst)%rad(n)%tb(k)%error)**2
         enddo
         ! end if
      end do
   end do 

end subroutine da_check_xtoy_adjoint_rad

