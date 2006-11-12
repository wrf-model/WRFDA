subroutine da_calculate_grady_ssmi( oi, re, jo_grad_y )

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)   :: oi          ! Ob Inc. structure.
   type (y_type), intent(inout) :: re          ! Residual structure.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n

   if ( oi%num_ssmi_tb .gt. 0 ) then
      do n=1, oi%num_ssmi_tb

         if(oi%ssmi_tb(n)%tb19v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb19v = 0.0
         jo_grad_y%ssmi_tb(n)%tb19v = - re%ssmi_tb(n)%tb19v / &
                                      ( oi%ssmi_tb(n)%tb19v%error * &
                                        oi%ssmi_tb(n)%tb19v%error )

         if(oi%ssmi_tb(n)%tb19h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb19h = 0.0
         jo_grad_y%ssmi_tb(n)%tb19h = - re%ssmi_tb(n)%tb19h / &
                                      ( oi%ssmi_tb(n)%tb19h%error * &
                                        oi%ssmi_tb(n)%tb19h%error )

         if(oi%ssmi_tb(n)%tb22v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb22v = 0.0
         jo_grad_y%ssmi_tb(n)%tb22v = - re%ssmi_tb(n)%tb22v / &
                                      ( oi%ssmi_tb(n)%tb22v%error * &
                                        oi%ssmi_tb(n)%tb22v%error )

         if(oi%ssmi_tb(n)%tb37v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb37v = 0.0
         jo_grad_y%ssmi_tb(n)%tb37v = - re%ssmi_tb(n)%tb37v / &
                                      ( oi%ssmi_tb(n)%tb37v%error * &
                                        oi%ssmi_tb(n)%tb37v%error )

         if(oi%ssmi_tb(n)%tb37h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb37h = 0.0
         jo_grad_y%ssmi_tb(n)%tb37h = - re%ssmi_tb(n)%tb37h / &
                                      ( oi%ssmi_tb(n)%tb37h%error * &
                                        oi%ssmi_tb(n)%tb37h%error )

         if(oi%ssmi_tb(n)%tb85v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb85v = 0.0
         jo_grad_y%ssmi_tb(n)%tb85v = - re%ssmi_tb(n)%tb85v / &
                                      ( oi%ssmi_tb(n)%tb85v%error * &
                                        oi%ssmi_tb(n)%tb85v%error )

         if(oi%ssmi_tb(n)%tb85h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb85h = 0.0
         jo_grad_y%ssmi_tb(n)%tb85h = - re%ssmi_tb(n)%tb85h / &
                                      ( oi%ssmi_tb(n)%tb85h%error * &
                                        oi%ssmi_tb(n)%tb85h%error )
      end do
   else if ( oi%num_ssmi_retrieval .gt. 0) then
      do n=1, oi%num_ssmi_retrieval
        if (oi%ssmi_retrieval(n)%speed%qc < obs_qc_pointer) then
            re%ssmi_retrieval(n)%speed = 0.0
        end if
        jo_grad_y%ssmi_retrieval(n)%Speed = - re%ssmi_retrieval(n)%Speed / &
            ( oi%ssmi_retrieval(n)%Speed%error * &
              oi%ssmi_retrieval(n)%Speed%error )

        if (oi%ssmi_retrieval(n)%tpw%qc < obs_qc_pointer) then
           re%ssmi_retrieval(n)%tpw = 0.0
        end if
        jo_grad_y%ssmi_retrieval(n)%tpw = -re%ssmi_retrieval(n)%tpw / &
                                         ( oi%ssmi_retrieval(n)%tpw%error * &
                                           oi%ssmi_retrieval(n)%tpw%error )
      end do  
   end if

end subroutine da_calculate_grady_ssmi
