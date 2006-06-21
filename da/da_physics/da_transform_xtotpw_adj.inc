subroutine da_transform_xtotpw_adj( xa, xb )

   implicit none

   type (x_type),  intent(inout) :: xa       ! gridded analysis increment.
   type (xb_type), intent(in)    :: xb       ! first guess state.

   integer :: i, j, k, is, js, ie, je

   real    :: pw, dzpw

   if (trace_use) call da_trace_entry("da_transform_xtotpw_adj")

   is = xb%its
   js = xb%jts

   ie = xb%ite
   je = xb%jte

   if ( Testing_WRFVAR ) then
      is = xb%its-1
      js = xb%jts-1

      ie = xb%ite+1
      je = xb%jte+1

      if ( is < xb%ids ) is = xb%ids
      if ( js < xb%jds ) js = xb%jds

      if ( ie > xb%ide ) ie = xb%ide
      if ( je > xb%jde ) je = xb%jde
   endif

   do j=js, je
      do i=is, ie
         pw = 0.1*xa%tpw(i,j)

         do k=xb%kts, xb%kte
            dzpw = (xb%hf(i,j,k+1)-xb%hf(i,j,k))*pw

            xa%  q(i,j,k)=xa%  q(i,j,k)+dzpw*xb%rho(i,j,k)
            xa%rho(i,j,k)=xa%rho(i,j,k)+dzpw*xb%  q(i,j,k)
         enddo
      enddo
   enddo

   if (trace_use) call da_trace_exit("da_transform_xtotpw_adj")
 
end subroutine da_transform_xtotpw_adj

