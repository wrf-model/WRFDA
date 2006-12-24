subroutine da_biascorr_rad ( inst, nchan, tb_ob, iv )

   !---------------------------------------------------------------------------
   ! Purpose: perform bias correction for radiance data.
   !
   ! method:  tb*(xb)=a+b*tb(xb)
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)             :: inst       ! sensor index.
   integer, intent(in)             :: nchan      ! number of channel
   real,    intent(in)             :: tb_ob(nchan)  ! observation tb.
   type (rad_type), intent(inout)  :: iv         ! o-b structure.

   ! local variables

   integer   :: i,iscan
   real      :: tb_xb

   if (trace_use) call da_trace_entry("da_biascorr_rad")

   iscan = iv%scanpos
   do i=1,nchan
      tb_xb = satinfo(inst)%a(iscan,i) + satinfo(inst)%b(iscan,i)*iv%tb_xb(i)
      iv%tb(i)%inv = tb_ob(i) - tb_xb
   end do

   if (trace_use) call da_trace_exit("da_biascorr_rad")

end subroutine da_biascorr_rad


