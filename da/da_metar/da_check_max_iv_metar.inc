subroutine da_check_max_iv_metar(iv,ob, it, num_qcstat_conv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it    ! outer iteration
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)
   type(y_type),  intent(in)    :: ob      ! Observation structure.


   logical :: failed
   integer :: n

  if (trace_use_dull) call da_trace_entry("da_check_max_iv_metar")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(metar)%n1,iv%info(metar)%n2
    if( iv%info(metar)%proc_domain(1,n) ) then
      if( iv%metar(n)%u%qc == fails_error_max )iv%metar(n)%u%qc =0
      if( iv%metar(n)%u%qc >= obs_qc_pointer ) then
      call da_max_error_qc (it, iv%info(metar), n, iv%metar(n)%u, max_error_uv, failed)
      num_qcstat_conv(1,metar,1,1)= num_qcstat_conv(1,metar,1,1) + 1
      if(failed) then
      num_qcstat_conv(2,metar,1,1)= num_qcstat_conv(2,metar,1,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
      'metar',ob_vars(1),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
      end if
      end if

      if( iv%metar(n)%v%qc == fails_error_max )iv%metar(n)%v%qc =0
      if( iv%metar(n)%v%qc >= obs_qc_pointer ) then
      call da_max_error_qc (it, iv%info(metar), n, iv%metar(n)%v, max_error_uv, failed)             
      num_qcstat_conv(1,metar,2,1)= num_qcstat_conv(1,metar,2,1) + 1
      if(failed) then
      num_qcstat_conv(2,metar,2,1)= num_qcstat_conv(2,metar,2,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
      'metar',ob_vars(2),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
      end if
      end if

      if( iv%metar(n)%t%qc == fails_error_max )iv%metar(n)%t%qc =0
      if( iv%metar(n)%t%qc >= obs_qc_pointer ) then
      call da_max_error_qc (it, iv%info(metar), n, iv%metar(n)%t, max_error_t , failed)
      num_qcstat_conv(1,metar,3,1)= num_qcstat_conv(1,metar,3,1) + 1
      if(failed) then
      num_qcstat_conv(2,metar,3,1)= num_qcstat_conv(2,metar,3,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
      'metar',ob_vars(3),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
      end if
      end if

      if( iv%metar(n)%q%qc == fails_error_max )iv%metar(n)%q%qc =0
      if( iv%metar(n)%q%qc >= obs_qc_pointer ) then
      call da_max_error_qc (it, iv%info(metar), n, iv%metar(n)%q, max_error_q , failed)
      num_qcstat_conv(1,metar,4,1)= num_qcstat_conv(1,metar,4,1) + 1
      if(failed) then
      num_qcstat_conv(2,metar,4,1)= num_qcstat_conv(2,metar,4,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
      'metar',ob_vars(4),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
      end if
      end if

      if( iv%metar(n)%p%qc == fails_error_max )iv%metar(n)%p%qc =0
      if( iv%metar(n)%p%qc >= obs_qc_pointer ) then
      call da_max_error_qc (it, iv%info(metar), n, iv%metar(n)%p, max_error_p , failed)         
      num_qcstat_conv(1,metar,5,1)= num_qcstat_conv(1,metar,5,1) + 1
      if(failed) then
      num_qcstat_conv(2,metar,5,1)= num_qcstat_conv(2,metar,5,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
      'metar',ob_vars(5),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
      end if
      end if
    end if 
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_metar")

end subroutine da_check_max_iv_metar


