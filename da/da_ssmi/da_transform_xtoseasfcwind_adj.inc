subroutine da_transform_xtoseasfcwind_adj(xa, xb)

   !-------------------------------------------------------------------------
   ! Purpose: Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !-------------------------------------------------------------------------

   implicit none

   type (x_type),  intent(inout) :: xa   ! model space analysis (local).
   type (xb_type), intent(in)    :: xb   ! first guess.

   real :: const, rgh_fac, var, height
   integer :: i, j, k, is, js, ie, je

   if (trace_use) call da_trace_entry("da_transform_xtoseasfcwind_adj")

   const = log(10./0.0001)
   k = xb%kts

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


   do j=js, je
      do i=is, ie
        height = xb%h(i,j,k) - xb%terr(i,j)
         if (height <= 0.0) then
            message(1) = "Negative height found"
            write(unit=message(2),FMT='(2I,A,F10.2,A,F10.2)') &
               i,j,' ht = ',xb%h(i,j,k) ,' terr =  ',xb%terr(i,j)
            call da_error(__FILE__,__LINE__,message(1:2))
         endif

         rgh_fac = const/log(height/0.0001) ! roughness = 0.0001

         var = rgh_fac*rgh_fac/xb%speed(i,j)

         xa%u(i,j,k)=xa%u(i,j,k)+var*xa%speed(i,j)*xb%u(i,j,k)
         xa%v(i,j,k)=xa%v(i,j,k)+var*xa%speed(i,j)*xb%v(i,j,k)
      end do
   end do

   if (trace_use) call da_trace_exit("da_transform_xtoseasfcwind_adj")

end subroutine da_transform_xtoseasfcwind_adj


