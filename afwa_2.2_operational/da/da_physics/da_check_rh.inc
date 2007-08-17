subroutine da_check_rh(xb, xa, xp)

   !---------------------------------------------------------------------------
   ! Purpose: Remove RH over 100% or under 10%
   !          Made the modification to those levels, which RH are less than 95%
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)           :: xb    ! First guess state
   type (x_type),  intent(inout)        :: xa    ! Analysis increment
   type (xpose_type), intent(in)        :: xp    ! domain decomposition vars.

   integer   :: imod(xp%kts:xp%kte)
   real      :: rhtol(xp%kts:xp%kte)
   real      :: x_qs(xp%kts:xp%kte)
   real      :: dz(xp%kts:xp%kte)

   integer :: is,ie, js,je, ks,ke, i, j, k
   real    :: tol_adjust_moist, tol_moist, oldrha, each_moist, es, weight
   real    :: upper_modify_rh, lower_modify_rh

   if (trace_use) call da_trace_entry("da_check_rh")

   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte

   upper_modify_rh = 95.0
   lower_modify_rh = 11.0

! To get the xa%rh for the moisture adjustments (YRG, 04/02/2007):

      if ( cv_options_hum == 1  ) then
         write(6,'(/a,i2,a)')'cv_options_hum=',cv_options_hum,' to get xa%rh..'
         call DA_TPQ_To_RH_Lin( xb, xp, xa )
      endif

   do i=is,ie
      do j=js,je

         tol_adjust_moist = 0.0
         tol_moist        = 0.0

         DO k=ks,ke
            dz(k)=xb%hf(i,j,k+1)-xb%hf(i,j,k)

            imod(k)           = 0
            x_qs(k)           = 0.
            rhtol(k)          = xb%rh(i,j,k)+xa%rh(i,j,k)
         ENDDO

         do k=ks,ke
            if (rhtol(k) .gt. maximum_rh) then
               oldrha       = xa%rh(i,j,k)
               ! modify xa%rh
               xa%rh(i,j,k) = maximum_rh - xb%rh(i,j,k)

               call da_tp_to_qs(xb%t(i,j,k)+xa%t(i,j,k), &
                  xb%p(i,j,k)+xa%p(i,j,k), es, x_qs(k))

               ! calculate xa%q
               call da_tprh_to_q_lin1(xb%t(i,j,k), xb%p(i,j,k), &
                  xb%es(i,j,k), xb%q(i,j,k), xb%rh(i,j,k),  xa%t(i,j,k), &
                  xa%p(i,j,k), xa%rh(i,j,k), xa%q(i,j,k))

               tol_adjust_moist = tol_adjust_moist + x_qs(k)*(oldrha - &
                  xa%rh(i,j,k))* dz(k)*(xb%rho(i,j,k)+xa%rho(i,j,k))
               imod(k)=-1
            end if

            if (rhtol(k) .lt. minimum_rh) then
               oldrha           = xa%rh(i,j,k)
               xa%rh(i,j,k)     = minimum_rh - xb%rh(i,j,k)
               call da_tp_to_qs(xb%t(i,j,k)+xa%t(i,j,k), &
                  xb%p(i,j,k)+xa%p(i,j,k), es, x_qs(k))

               call da_tprh_to_q_lin1(xb%t(i,j,k), xb%p(i,j,k), &
                  xb%es(i,j,k), xb%q(i,j,k), xb%rh(i,j,k),  xa%t(i,j,k), &
                  xa%p(i,j,k), xa%rh(i,j,k), xa%q(i,j,k))


               tol_adjust_moist = tol_adjust_moist + x_qs(k)*(oldrha - &
                  xa%rh(i,j,k))* dz(k)*(xb%rho(i,j,k)+xa%rho(i,j,k))
               imod(k)=-1
            end if
         end do

         if (tol_adjust_moist .gt. 0.0) then
            do k=ks,ke
               if (rhtol(k) .lt. upper_modify_rh .and. imod(k) .eq. 0) then
                  call da_tp_to_qs(xb%t(i,j,k)+xa%t(i,j,k), &
                                    xb%p(i,j,k)+xa%p(i,j,k),es,x_qs(k))

                  each_moist   = rhtol(k)*x_qs(k)* &
                                 dz(k)*(xb%rho(i,j,k)+xa%rho(i,j,k))
                  tol_moist    = tol_moist + each_moist
                  imod(k)      = 1
               end if
            end do
         end if

         if (tol_adjust_moist .lt. 0.0) then
            do k=ks,ke
               if (rhtol(k) .gt. lower_modify_rh .and. imod(k) .eq. 0) then
                  call da_tp_to_qs(xb%t(i,j,k)+xa%t(i,j,k), &
                                    xb%p(i,j,k)+xa%p(i,j,k), es, x_qs(k))

                  each_moist   = rhtol(k)*x_qs(k)* &
                                 dz(k)*(xb%rho(i,j,k)+xa%rho(i,j,k))
                  tol_moist    = tol_moist + each_moist
                  imod(k)      = 1
               end if
            end do
         end if

         if (tol_moist > 0) then
           weight       = tol_adjust_moist/tol_moist
           do k=ks,ke
             if (imod(k) .eq. 1) then
               xa%rh(i,j,k) = xa%rh(i,j,k)+(xb%rh(i,j,k)+xa%rh(i,j,k))*weight
               call da_tprh_to_q_lin1(xb%t(i,j,k), xb%p(i,j,k), xb%es(i,j,k), &
                                      xb%q(i,j,k), xb%rh(i,j,k),  xa%t(i,j,k), &
                                      xa%p(i,j,k), xa%rh(i,j,k), xa%q(i,j,k))

             end if
           end do
         end if
      end do
   end do

   if (trace_use) call da_trace_exit("da_check_rh")

end subroutine da_check_rh


