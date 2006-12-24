subroutine da_interp_lin_3d_new(fm3d, xp, &
                         i, j, k, dx, dy, dz, dxm, dym, dzm, &
                         fo3d, nl,num)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (xpose_type),      intent(in)  :: xp    ! Dimensions and xpose buffers.
   integer,                intent(in)  :: nl     ! number of input levels      
   integer,                intent(in)  :: num    ! number of ns   
   integer,                intent(in)  :: i(num), j(num), k(nl,num)
   real,                   intent(in)  :: dx(num), dxm(num)
   real,                   intent(in)  :: dy(num), dym(num)
   real,                   intent(in)  :: dz(nl,num), dzm(nl,num)
   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                           intent(in)  :: fm3d    ! Input variable              
   real,                   intent(inout) :: fo3d(nl,num)  ! Output variable 

   integer                             :: n, kk
   real                                :: fmz(xp%kms:xp%kme)

   if (trace_use) call da_trace_entry("da_interp_lin_3d_new")

   fo3d(:,:) = 0.0

   do n=1,num
      fmz(:)=0.0

      fmz(xp%kts:xp%kte) = dym(n)*(dxm(n)*fm3d(i(n), j(n), xp%kts:xp%kte) &
                            +dx(n) *fm3d(i(n)+1,j(n), xp%kts:xp%kte)) &
                      + dy(n) *(dxm(n)*fm3d(i(n), j(n)+1, xp%kts:xp%kte) &
                            +dx(n) * fm3d(i(n)+1, j(n)+1, xp%kts:xp%kte))
      do kk = 1, nl
         if (k(kk,n) > 0) then
            fo3d(kk,n) = dzm(kk,n)*fmz(k(kk,n)) + dz(kk,n)*fmz(k(kk,n)+1)
         end if
      end do
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_3d_new")

end subroutine da_interp_lin_3d_new


