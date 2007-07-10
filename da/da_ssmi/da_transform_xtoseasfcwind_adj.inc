subroutine da_transform_xtoseasfcwind_adj(grid)

   !-------------------------------------------------------------------------
   ! Purpose: Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !-------------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid

   real :: const, rgh_fac, var, height
   integer :: i, j, k, is, js, ie, je

   if (trace_use) call da_trace_entry("da_transform_xtoseasfcwind_adj")

   const = log(10./0.0001)
   k = grid%xb%kts

   is = grid%xb%its
   js = grid%xb%jts

   ie = grid%xb%ite
   je = grid%xb%jte

   if (test_wrfvar) then
      is = grid%xb%its-1
      js = grid%xb%jts-1

      ie = grid%xb%ite+1
      je = grid%xb%jte+1

      if (is < grid%xb%ids) is = grid%xb%ids
      if (js < grid%xb%jds) js = grid%xb%jds

      if (ie > grid%xb%ide) ie = grid%xb%ide
      if (je > grid%xb%jde) je = grid%xb%jde
   end if


   do j=js, je
      do i=is, ie
        height = grid%xb%h(i,j,k) - grid%xb%terr(i,j)
         if (height <= 0.0) then
            message(1) = "Negative height found"
            write(unit=message(2),FMT='(2I6,A,F10.2,A,F10.2)') &
               i,j,' ht = ',grid%xb%h(i,j,k) ,' terr =  ',grid%xb%terr(i,j)
            call da_error(__FILE__,__LINE__,message(1:2))
         endif

         rgh_fac = const/log(height/0.0001) ! roughness = 0.0001

         var = rgh_fac*rgh_fac/grid%xb%speed(i,j)

         grid%xa%u(i,j,k)=grid%xa%u(i,j,k)+var*grid%xa%speed(i,j)*grid%xb%u(i,j,k)
         grid%xa%v(i,j,k)=grid%xa%v(i,j,k)+var*grid%xa%speed(i,j)*grid%xb%v(i,j,k)
      end do
   end do

   if (trace_use) call da_trace_exit("da_transform_xtoseasfcwind_adj")

end subroutine da_transform_xtoseasfcwind_adj


