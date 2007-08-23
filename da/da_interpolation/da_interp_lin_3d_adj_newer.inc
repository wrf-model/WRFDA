subroutine da_interp_lin_3d_adj_newer(fm3d, loc, max_lev,n1,n2, fo3d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,                 intent(inout) :: fm3d(ims:ime,jms:jme,kms:kme)
   type(model_loc_type_new), intent(in)    :: loc
   integer,              intent(in)    :: max_lev,n1,n2
   real,                 intent(in)    :: fo3d(max_lev,n1:n2) 

   integer :: n,k
   real    :: fmz(kms:kme)

   if (trace_use) call da_trace_entry("da_interp_lin_3d_adj_newer")

   do n=n1,n2
      fmz = 0.0
      do k = 1, max_lev
         if (loc%k(k,n) > 0) then
            fmz(loc%k(k,n))   = loc%dzm(k,n) * fo3d(k,n) + fmz(loc%k(k,n))
            fmz(loc%k(k,n)+1) = loc%dz (k,n) * fo3d(k,n) + fmz(loc%k(k,n)+1)
         end if
      end do
  
      do k=kts,kte
         fm3d(loc%i(k,n)  ,loc%j(k,n),k)   = loc%dym(k,n)*loc%dxm(k,n)*fmz(k) + fm3d(loc%i(k,n)  ,loc%j(k,n)  ,k)
         fm3d(loc%i(k,n)+1,loc%j(k,n),k)   = loc%dym(k,n)*loc%dx (k,n)*fmz(k) + fm3d(loc%i(k,n)+1,loc%j(k,n)  ,k)
         fm3d(loc%i(k,n)  ,loc%j(k,n)+1,k) = loc%dy (k,n)*loc%dxm(k,n)*fmz(k) + fm3d(loc%i(k,n)  ,loc%j(k,n)+1,k)
         fm3d(loc%i(k,n)+1,loc%j(k,n)+1,k) = loc%dy (k,n)*loc%dx (k,n)*fmz(k) + fm3d(loc%i(k,n)+1,loc%j(k,n)+1,k)
      end do
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_3d_adj_newer")

end subroutine da_interp_lin_3d_adj_newer


