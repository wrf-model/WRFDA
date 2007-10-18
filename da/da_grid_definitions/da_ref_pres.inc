subroutine da_ref_pres(height, ref_pres)

   !---------------------------------------------------------------------------
   ! Purpose: To calculate the reference pressure from the height.
   !---------------------------------------------------------------------------

   implicit none

   real, intent(in)  :: height
   real, intent(out) :: ref_pres

   real, parameter :: rovg = gas_constant/gravity

   real :: aa, bb, cc, hh, htop, ziso, p0iso

   if (trace_use_frequent) call da_trace_entry("da_ref_pres")

   call da_ref_height(ptop, htop)

   bb = rovg * t0
   aa = rovg * base_lapse * 0.5

   hh = min(htop, height)
   cc = (-bb + sqrt(bb * bb - 4.0 * aa * hh))/(2.0*aa)
   ref_pres = base_pres * exp(cc)

   if (base_temp > 0.0) then
      p0iso=base_pres*exp((base_temp-t0)/base_lapse)
      cc=log(p0iso/base_pres)
      ziso=-(aa*cc+bb)*cc

      if (height > ziso) then
         ref_pres = p0iso/exp((height-ziso)/(rovg*base_temp))
      end if
   end if

   if (trace_use_frequent) call da_trace_exit("da_ref_pres")

end subroutine da_ref_pres


e (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for Buoy_U   = ',max_error_uv, &
         'Error max test ratio for Buoy_V   = ',max_error_uv, &
         'Error max test ratio for Buoy_P   = ',max_error_p,  &
         'Error max test ratio for Buoy_T   = ',max_error_t,  &
         'Error max test ratio for Buoy_QV  = ',max_error_q
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do n=iv%info(buoy)%n1,iv%info(buoy)%n2
      call da_max_error_qc_new (it, iv%info(buoy), n, iv%buoy(n)%u, max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', failed, check_max_iv_print)
      call da_max_error_qc_new (it, iv%info(buoy), n, iv%buoy(n)%v, max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', failed, check_max_iv_print)                           
      call da_max_error_qc_new (it, iv%info(buoy), n, iv%buoy(n)%p, max_error_p, itp, itpf, check_max_iv_unit, 'Press', failed, check_max_iv_print)         
      call da_max_error_qc_new (it, iv%info(buoy), n, iv%buoy(n)%t, max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', failed, check_max_iv_print)
      ! Mixing ratio, same ratio as relative humidity
      call da_max_error_qc_new (it, iv%info(buoy), n, iv%buoy(n)%q, max_error_q, itqv, itqvf, check_max_iv_unit, 'QV   ', failed, check_max_iv_print)
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_buoy")       

end subroutine da_check_max_iv_buoy


