subroutine da_pt_to_rho_adj(xb, xa) 

   !---------------------------------------------------------------------------
   !  Purpose: Adjoint of da_pt_to_rho.
   !
   !  Assumptions: 1) Model level stored top down
   !---------------------------------------------------------------------------

   implicit none
   
   type (xb_type), intent(in)    :: xb           ! First guess structure.
   type (x_type),  intent(inout) :: xa           ! increment structure.

   integer                       :: i,j,k        ! Loop counter.

   integer                       :: is, ie, js, je

   real                          :: temp

   if (trace_use) call da_trace_entry("da_pt_to_rho_adj")
   
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

   !---------------------------------------------------------------------------
   ! Calculate rho increments:
   !---------------------------------------------------------------------------

   do j=js, je
      do k=xb%kts, xb%kte
         do i=is, ie
            temp = xa%rho(i,j,k) * xb%rho(i,j,k)

            xa%p(i,j,k) = xa%p(i,j,k) + temp/xb%p(i,j,k)
                           
            xa%t(i,j,k) = xa%t(i,j,k) - temp/xb%t(i,j,k)

            xa%rho(i,j,k) = 0.0
         end do        
      end do        
   end do        

   if (trace_use) call da_trace_exit("da_pt_to_rho_adj")

end subroutine da_pt_to_rho_adj


