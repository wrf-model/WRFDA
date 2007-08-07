subroutine da_interp_lin_3d_adj(fm3d, loc, fo3d, ml, zk, nl)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,                  intent(inout) :: fm3d(ims:ime,jms:jme,kms:kme)
   type (model_loc_type), intent(in)    :: loc
   real,                  intent(in)    :: fo3d(ml)
   integer,               intent(in)    :: nl,ml
   real,                  intent(in)    :: zk(nl)


   integer :: i,j,k,kk
   real    :: dx, dxm, dy, dym,dz, dzm
   real    :: fmz(kms:kme)

   if (trace_use_frequent) call da_trace_entry("da_interp_lin_3d_adj")

   i   = loc%i
   j   = loc%j
   dx  = loc%dx
   dy  = loc%dy
   dxm = loc%dxm
   dym = loc%dym

   fmz = 0.0

   do kk = 1, nl
      if (zk(kk) > 0.0) then
         call da_togrid(zk(kk), kts, kte, k, dz, dzm)
         fmz(k)   = dzm*fo3d(kk) + fmz(k)
         fmz(k+1) = dz *fo3d(kk) + fmz(k+1)
      end if
   end do

   fm3d(i  ,j  ,kts:kte) = dym*dxm*fmz(kts:kte) + fm3d(i  ,j  ,kts:kte)
   fm3d(i+1,j  ,kts:kte) = dym*dx *fmz(kts:kte) + fm3d(i+1,j  ,kts:kte)
   fm3d(i  ,j+1,kts:kte) = dy *dxm*fmz(kts:kte) + fm3d(i  ,j+1,kts:kte)
   fm3d(i+1,j+1,kts:kte) = dy *dx *fmz(kts:kte) + fm3d(i+1,j+1,kts:kte)

   if (trace_use_frequent) call da_trace_exit("da_interp_lin_3d_adj")

end subroutine da_interp_lin_3d_adj


