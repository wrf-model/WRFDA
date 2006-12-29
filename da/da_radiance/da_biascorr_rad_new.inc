subroutine da_biascorr_rad_new (inst, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform bias correction for radiance data.
   !
   ! Method:  Tb*(xb)=a+b*Tb(xb)
   !---------------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: inst       ! sensor index.
   type (y_type),  intent(in)    :: ob         ! Observation structure.
   type (ob_type), intent(inout) :: iv         ! O-B structure.

   ! Local variables

   integer   :: i,iscan, n1,n2,n

   if (trace_use) call da_trace_entry("da_biascorr_rad_new")

   n1 = iv%ob_numb(iv%current_ob_time-1)%radiance(inst)+1
   n2 = iv%ob_numb(iv%current_ob_time)%radiance(inst)

   do n=n1,n2
      iscan = iv%instid(inst)%scanpos(n)
      do i=1,iv%instid(inst)%nchan
         iv%instid(inst)%tb_inv(i,n) = ob%instid(inst)%tb(i,n) - satinfo(inst)%a(iscan,i) &
            + satinfo(inst)%b(iscan,i)*iv%instid(inst)%tb_xb(i,n)
      end do
   end do

   if (trace_use) call da_trace_exit("da_biascorr_rad_new")

end subroutine da_biascorr_rad_new


