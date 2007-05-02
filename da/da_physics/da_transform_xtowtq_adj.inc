subroutine da_transform_xtowtq_adj (xp, xb, jo_grad_x)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)    :: xp          ! domain decomposition vars.
   type (xb_type),    intent(in)    :: xb          ! first guess state.
   type (x_type),     intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: i, j, kz, is, js, ie, je
   real    :: height

   if (trace_use) call da_trace_entry("da_transform_xtowtq_adj")

   kz = xp%kts

   is = xb%its
   js = xb%jts

   ie = xb%ite
   je = xb%jte

   if (Testing_WRFVAR) then
      is = xb%its-1
      js = xb%jts-1

      ie = xb%ite+1
      je = xb%jte+1

      if (is < xb%ids) is = xb%ids
      if (js < xb%jds) js = xb%jds

      if (ie > xb%ide) ie = xb%ide
      if (je > xb%jde) je = xb%jde
   end if

   ! Adjoint from Gridded 10-m wind and 2-m moisture and temperature
   ! to the model adjoint variables

   do j=js, je
      do i=is, ie
         jo_grad_x%tgrn(i,j)=0.0

         height = xb%h(i,j,kz) - xb%terr(i,j)                 
         if (height <= 0.0) then
            message(1) = "Negative height found"
            write(unit=message(2),FMT='(2I6,A,F10.2,A,F10.2)') &
               i,j,' ht = ',xb%h(i,j,kz) ,' terr =  ',xb%terr(i,j)
            call da_error(__FILE__,__LINE__,message(1:2))
         endif
         call da_sfc_wtq_adj(xb%psfc(i,j), xb%tgrn(i,j), &
            xb%p(i,j,kz), xb%t(i,j,kz), xb%q(i,j,kz), &
            xb%u(i,j,kz), xb%v(i,j,kz), &
            xb%regime(i,j),  &
            jo_grad_x%psfc(i,j), jo_grad_x%tgrn(i,j), &
            jo_grad_x%p(i,j,kz), jo_grad_x%t(i,j,kz), jo_grad_x%q(i,j,kz), &
            jo_grad_x%u(i,j,kz), jo_grad_x%v(i,j,kz), &
            height      , xb%rough(i,j), xb%xland(i,j), &
            jo_grad_x%u10(i,j),jo_grad_x%v10(i,j), &
            jo_grad_x%t2 (i,j),jo_grad_x%q2 (i,j))

         jo_grad_x%tgrn(i,j)=0.0
      end do
   end do

   if (trace_use) call da_trace_exit("da_transform_xtowtq_adj")

end subroutine da_transform_xtowtq_adj


