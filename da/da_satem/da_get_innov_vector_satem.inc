subroutine da_get_innov_vector_satem( it, xb, xp, ob, iv)

   implicit none

   integer, intent(in)             :: it       ! External iteration.
   type(xb_type), intent(in)      :: xb       ! first guess state.
   type(xpose_type), intent(in)   :: xp       ! Domain decomposition vars.
   type(y_type),  intent(in)      :: ob       ! Observation structure.
   type(ob_type), intent(inout)   :: iv       ! O-B structure.

   integer                         :: n        ! Loop counter.
   integer                         :: i, j, k  ! Index dimension.
   integer                         :: num_levs ! Number of obs levels.
   real                            :: dx, dxm  ! Interpolation weights.
   real                            :: dy, dym  ! Interpolation weights.
   real, dimension(1:max_ob_levels) :: model_thickness !Model thickness at ob loc
   integer           :: ithickness,ithicknessf

   real, dimension(xp%kts-1:xp%kte+1)   :: pre_ma,tv_ma
   integer                         :: layer1,layer2,ks,ke
   real                            :: tv1,tv2,pres2

   if (iv % num_satem > 0) then
      ithickness = 0 ; ithicknessf = 0

      do n=iv%ob_numb(iv%current_ob_time-1)%satem + 1, iv%ob_numb(iv%current_ob_time)%satem
         num_levs = iv % satem(n) % info % levels

         if (num_levs < 1) cycle

         model_thickness(:) = 0.0

         ! [1.0] Get cross pt. horizontal interpolation weights:

         i = iv%satem(n)%loc%i
         dy = iv%satem(n)%loc%dy
         dym = iv%satem(n)%loc%dym
         j = iv%satem(n)%loc%j
         dx = iv%satem(n)%loc%dx
         dxm = iv%satem(n)%loc%dxm
         ks = xp%kts; ke=xp%kte

         !------------------------------------------------------------------------

         ! [2.0] Calculate vertical profile of virtual temperature at obs pt.

         call da_tv_profile(xp,xb,i,j,dx,dxm,dy,dym,pre_ma,tv_ma)

         ! [3.0] Find model vertical position of pressure and do interp.

         call da_find_layer(layer2,tv2,iv%satem(n)%ref_p,pre_ma,tv_ma,ks,ke)
         pres2 = iv%satem(n)%ref_p

         ! [4.0] Thickness innovations calculation

         do k = 1, num_levs
            iv % satem(n) % thickness(k) % inv = 0.0
            call da_find_layer(layer1,tv1,iv%satem(n)%p(k),pre_ma,tv_ma,ks,ke)
            call da_thickness(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1,layer2,   &
              iv%satem(n)%p(k),pres2,model_thickness(k))

            if (ABS(ob % satem(n) % thickness(k) - missing_r) > 1. .and. &
                 iv % satem(n) % thickness(k)%qc /= missing_data) then
              iv % satem(n) % thickness(k) % inv =     &
                     ob % satem(n) % thickness(k) - model_thickness(k)
              ! write(unit=stdout,fmt="(A, 2F10.3,F10.0,A,F5.0,A)") &
              !   "observed, model_thickness, layer = ", &
              !   ob%satem(n)%thickness(k), &
              ! model_thickness(k), 0.01*pres2, " -", &
              ! iv%satem(n)%p(k)*0.01,'hPa'
            end if

            pres2 = iv%satem(n)%p(k)
            layer2 = layer1
            tv2 = tv1
         end do

         !------------------------------------------------------------------------
         ! [5.0] Perform optional maximum error check:
         !------------------------------------------------------------------------

         if (check_max_iv) then
           call da_check_max_iv_satem(it, iv % satem(n), ithickness,ithicknessf)
         end if

         !------------------------------------------------------------------------
         ! [6.0] Perform land/ocean check
         !------------------------------------------------------------------------

         if (xb%landmask(i,j ) /= 0. .or. xb%landmask(i+1,j ) /= 0. .or.  &
             xb%landmask(i,j+1) /= 0. .or. xb%landmask(i+1,j+1) /= 0.) then
            iv % satem(n) % thickness(1) % inv = 0.
            ithicknessf = ithicknessf + 1
         end if
      end do

      if (check_max_iv_print) then
         write(unit = check_max_iv_unit, fmt ='(A,i5,A)')'For outer iteration ',it, &
            ', Total Rejections for Satem follows:'
         write(unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
            'Number of failed thicknesses observations:     ',&
                      ithicknessf, ' on ',ithickness
      end if
   end if

end subroutine da_get_innov_vector_satem


