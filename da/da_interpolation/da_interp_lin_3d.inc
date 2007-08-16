subroutine da_interp_lin_3d(fm3d, loc, fo3d, ml, zk, nl)

   !-----------------------------------------------------------------------
   ! Purpose: TBD 
   !-----------------------------------------------------------------------

   implicit none

   real,                  intent(in)    :: fm3d(ims:ime,jms:jme,kms:kme)
   type (model_loc_type), intent(in)    :: loc
   integer,               intent(in)    :: ml, nl                        
   real,                  intent(inout) :: fo3d(ml)
   real,                  intent(in)    :: zk(nl)

   integer :: i,j,k, kk
   real    :: dx, dxm, dy, dym,dz, dzm
   real    :: fmz(kms:kme)

   if (trace_use_frequent) call da_trace_entry("da_interp_lin_3d")

   i   = loc%i
   j   = loc%j
   dx  = loc%dx
   dy  = loc%dy
   dxm = loc%dxm
   dym = loc%dym

   fmz(:) = 0.0

   fmz(kts:kte) = dym*(dxm*fm3d(i,j,kts:kte) + dx *fm3d(i+1,j,kts:kte)) &
      + dy *(dxm*fm3d(i,j+1,kts:kte) + dx *fm3d(i+1,j+1,kts:kte))

   fo3d(:) = 0.0

   do kk = 1, nl
      if (zk(kk) > 0.0 .or. anal_type_verify ) then
         call da_togrid(zk(kk), kts, kte, k, dz, dzm)
         fo3d(kk) = dzm*fmz(k) + dz*fmz(k+1)
      end if
   end do

   if (trace_use_frequent) call da_trace_exit("da_interp_lin_3d")

end subroutine da_interp_lin_3d


