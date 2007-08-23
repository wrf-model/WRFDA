subroutine da_interp_lin_3d_newer(fm3d, loc, max_lev, n1,n2, fo3d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,                 intent(in)    :: fm3d(ims:ime,jms:jme,kms:kme) ! Input variable  
   type(model_loc_type_new), intent(in)    :: loc                           ! Location
   integer,              intent(in)    :: max_lev                       ! number of input levels      
   integer,              intent(in)    :: n1,n2                         ! number of obs           
   real,                 intent(inout) :: fo3d(max_lev,n1:n2)           ! Output variable 

   integer :: n, k
   real    :: fmz(kms:kme)

   if (trace_use) call da_trace_entry("da_interp_lin_3d_newer")

   fo3d(:,:) = 0.0

   do n=n1,n2
      fmz(:)=0.0

      do k = kts,kte
         fmz(k) = &
              loc%dym(k,n) * (loc%dxm(k,n)*fm3d(loc%i(k,n), loc%j(k,n), k) &
            + loc%dx (k,n) * fm3d(loc%i(k,n)+1,loc%j(k,n), k)) &
            + loc%dy (k,n) * (loc%dxm(k,n)*fm3d(loc%i(k,n), loc%j(k,n)+1, k) &
            + loc%dx (k,n) * fm3d(loc%i(k,n)+1, loc%j(k,n)+1, k))
      end do
      do k = 1, max_lev
         if (loc%k(k,n) > 0) then
            fo3d(k,n) = loc%dzm(k,n)*fmz(loc%k(k,n)) + loc%dz(k,n)*fmz(loc%k(k,n)+1)
         end if
      end do
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_3d_newer")

end subroutine da_interp_lin_3d_newer


