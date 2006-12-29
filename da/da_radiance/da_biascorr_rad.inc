subroutine da_biascorr_rad ( i, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform bias correction for radiance data.
   !
   ! METHOD:  Tb*(xb)=a+b*Tb(xb)
   !---------------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: i       ! sensor index.
   type (y_type),  intent(in)    :: ob         ! Observation structure.
   type (ob_type), intent(inout) :: iv         ! O-B structure.

   ! Local variables

   integer   :: k,iscan, n1,n2,n

   if (trace_use) call da_trace_entry("da_biascorr_rad")

   n1 = iv%ob_numb(iv%current_ob_time-1)%radiance(i)+1
   n2 = iv%ob_numb(iv%current_ob_time)%radiance(i)

   do n=n1,n2
      iscan = iv%instid(i)%scanpos(n)
      do k=1,iv%instid(i)%nchan
         iv%instid(i)%tb_inv(k,n) = ob%instid(i)%tb(k,n) - satinfo(i)%a(iscan,k) &
            - satinfo(i)%b(iscan,k)*iv%instid(i)%tb_xb(k,n)
      end do
   end do

   if (trace_use) call da_trace_exit("da_biascorr_rad")

end subroutine da_biascorr_rad


