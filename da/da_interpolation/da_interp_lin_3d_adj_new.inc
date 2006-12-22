subroutine da_interp_lin_3d_adj_new(fm3d, xp, &
                             i, j, k, dx, dy, dz, dxm, dym, dzm, &
                             fo3d, nl,num)

   implicit none

   type (xpose_type),      intent(in)    :: xp  ! Dimensions and xpose buffers.
   integer,                intent(in)    :: nl,num
   integer,                intent(in)    :: i(num), j(num), k(nl,num)
   real,                   intent(in)    :: dx(num), dxm(num)
   real,                   intent(in)    :: dy(num), dym(num)
   real,                   intent(in)    :: dz(nl,num), dzm(nl,num)
   real,                   intent(in)    :: fo3d(nl,num) 
   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                           intent(inout) :: fm3d    ! Input/Output variable

   integer                :: point,kk
   real                   :: fmz(xp%kms:xp%kme)

   if (trace_use) call da_trace_entry("da_interp_lin_3d_adj_new")

   do point=1,num
      fmz = 0.0
      do kk = 1, nl
         if (k(kk,point) > 0) then
            fmz(k(kk,point))   = dzm(kk,point)*fo3d(kk,point) + fmz(k(kk,point))
            fmz(k(kk,point)+1) = dz(kk,point) *fo3d(kk,point) + fmz(k(kk,point)+1)
         end if
      end do

      fm3d(i(point)  ,j(point)  ,xp%kts:xp%kte) = dym(point)*dxm(point)*fmz(xp%kts:xp%kte) &
                                  + fm3d(i(point)  ,j(point)  ,xp%kts:xp%kte)
      fm3d(i(point)+1,j(point)  ,xp%kts:xp%kte) = dym(point)*dx(point) *fmz(xp%kts:xp%kte) &
                                  + fm3d(i(point)+1,j(point)  ,xp%kts:xp%kte)
      fm3d(i(point)  ,j(point)+1,xp%kts:xp%kte) = dy(point) *dxm(point)*fmz(xp%kts:xp%kte) &
                                  + fm3d(i(point)  ,j(point)+1,xp%kts:xp%kte)
      fm3d(i(point)+1,j(point)+1,xp%kts:xp%kte) = dy(point) *dx(point) *fmz(xp%kts:xp%kte) &
                                  + fm3d(i(point)+1,j(point)+1,xp%kts:xp%kte)
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_3d_adj_new")

end subroutine da_interp_lin_3d_adj_new


