subroutine da_check_max_iv_ssmi_rv(iv, it, num_qcstat_conv)              

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it ! Outer loop 
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)

   logical :: failed
   integer :: n
   
   if (trace_use) call da_trace_entry("da_check_max_iv_ssmi_rv")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(ssmi_rv)%n1,iv%info(ssmi_rv)%n2
      if( iv%info(ssmi_rv)%proc_domain(1,n) ) then

      if( iv%ssmi_rv(n)%tpw%qc == fails_error_max ) iv%ssmi_rv(n)%tpw%qc = 0 
      if( iv%ssmi_rv(n)%tpw%qc >= obs_qc_pointer ) then
      call da_max_error_qc (it, iv%info(ssmi_rv), n, iv%ssmi_rv(n)%tpw, max_error_pw, failed)
                 num_qcstat_conv(1,ssmi_rv,7,1) = num_qcstat_conv(1,ssmi_rv,7,1) + 1
      if(failed) then
        num_qcstat_conv(2,ssmi_rv,7,1) = num_qcstat_conv(2,ssmi_rv,7,1) + 1
        write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
        'ssmi_rv',ob_vars(7),iv%info(ssmi_rv)%lat(1,n),iv%info(ssmi_rv)%lon(1,n),'1013.25'                  
      end if
      end if

      if( iv%ssmi_rv(n)%speed%qc == fails_error_max ) iv%ssmi_rv(n)%speed%qc = 0 
      if( iv%ssmi_rv(n)%speed%qc >= obs_qc_pointer ) then
      call da_max_error_qc (it, iv%info(ssmi_rv), n, iv%ssmi_rv(n)%speed, max_error_uv, failed)
                 num_qcstat_conv(1,ssmi_rv,6,1) = num_qcstat_conv(1,ssmi_rv,6,1) + 1
      if(failed)then
         num_qcstat_conv(2,ssmi_rv,6,1) = num_qcstat_conv(2,ssmi_rv,6,1) + 1
        write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
        'ssmi_rv',ob_vars(6),iv%info(ssmi_rv)%lat,iv%info(ssmi_rv)%lon,'1013.25'                  
      endif
      end if

     endif  
   end do

   if (trace_use) call da_trace_exit("da_check_max_iv_ssmi_rv")

end subroutine da_check_max_iv_ssmi_rv
