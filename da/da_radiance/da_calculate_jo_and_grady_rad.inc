subroutine da_calculate_jo_and_grady_rad( iv, re, jo, jo_grad_y ) 

   !---------------------------------------------------------------------------
   ! PURPOSE: Calculate Gradient_y i and cost function Jo for radiance data.
   !
   ! METHOD:  grad_y = -R^-1 ( d - H delta_x )
   !              Jo = -( d - H delta_x ) grad_y
   !---------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type) , intent(in)    :: re          ! Residual vector.
   type (y_type) , intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.

   integer                       :: n, k, i

   if ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_calculate_jo_and_grady_rad")

   allocate ( jo % rad(1:iv%num_inst) )

   do i =1, iv%num_inst
      allocate (jo % rad(i) % jo_ichan(iv%instid(i)%nchan))
      allocate (jo % rad(i) % num_ichan(iv%instid(i)%nchan))

      jo % rad(i)%jo_ichan(:) = 0.0
      jo % rad(i)%num_ichan(:) = 0

      if (iv%instid(i)%num_rad < 1) cycle

      do n=1, iv%instid(i)%num_rad
         do k=1, iv%instid(i)%nchan
            jo_grad_y%instid(i)%rad(n)%tb(k) = -re%instid(i)%rad(n)%tb(k) / &
                                    ( iv%instid(i)%rad(n)%tb(k)%error * &
                                      iv%instid(i)%rad(n)%tb(k)%error )
         end do
         IF (iv%instid(i)%rad(n)%loc%proc_domain) THEN
            do k=1, iv%instid(i)%nchan
               IF (iv%instid(i)%rad(n)%tb(k)%qc >= obs_qc_pointer) THEN
                  jo % rad(i) % jo_ichan(k) = jo % rad(i) % jo_ichan(k) - &
                     re%instid(i)%rad(n)%tb(k) * jo_grad_y%instid(i)%rad(n)%tb(k)
                  jo % rad(i) % num_ichan(k) = jo % rad(i) % num_ichan(k) + 1
               end if
            end do
         end if
      end do
      jo % rad(i)%jo_ichan(:) = 0.5 * jo % rad(i)%jo_ichan(:)
   end do

   if (trace_use) call da_trace_exit("da_calculate_jo_and_grady_rad")

end subroutine da_calculate_jo_and_grady_rad

