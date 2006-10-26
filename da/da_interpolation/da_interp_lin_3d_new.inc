subroutine da_interp_lin_3d_new(fm3d, xp, &
                         i, j, k, dx, dy, dz, dxm, dym, dzm, &
                         fo3d, ml, nl,num)

   implicit none

   TYPE (xpose_type),      intent(in)  :: xp    ! Dimensions and xpose buffers.
   integer,                intent(in)  :: ml     ! number of output levels
   integer,                intent(in)  :: nl     ! number of input levels      
   integer,                intent(in)  :: num    ! number of points   
   integer,                intent(in)  :: i(num), j(num), k(nl,num)
   real,                   intent(in)  :: dx(num), dxm(num)
   real,                   intent(in)  :: dy(num), dym(num)
   real,                   intent(in)  :: dz(nl,num), dzm(nl,num)
   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                           intent(in)  :: fm3d    ! Input variable              
   real,                   intent(out) :: fo3d(nl,num)  ! Output variable 

   integer                             :: point, kk
   real                                :: fmz(xp%kms:xp%kme)

   if (trace_use) call da_trace_entry("da_interp_lin_3d_new")

   fo3d(:,:) = 0.0

   do point=1,num
      fmz(:)=0.0
      fmz(xp%kts:xp%kte) = dym(point)*(dxm(point)*fm3d(i(point), j(point), xp%kts:xp%kte) &
                            +dx(point) *fm3d(i(point)+1,j(point), xp%kts:xp%kte)) &
                      + dy(point) *(dxm(point)*fm3d(i(point), j(point)+1, xp%kts:xp%kte) &
                            +dx(point) * fm3d(i(point)+1, j(point)+1, xp%kts:xp%kte))
      do kk = 1, nl
         if(k(kk,point) > 0) then
            fo3d(kk,point) = dzm(kk,point)*fmz(k(kk,point)) + dz(kk,point)*fmz(k(kk,point)+1)
         endif
      enddo
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_3d_new")

end subroutine da_interp_lin_3d_new

