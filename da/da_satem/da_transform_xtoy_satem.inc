subroutine da_transform_xtoy_satem ( xa, xb, iv, xp, y )

   implicit none

   type (x_type),  intent(in)   :: xa       ! gridded analysis increment.
   type (xb_type),  intent(in)  :: xb       ! first guess state.
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in):: xp       ! Domain decomposition vars.
   type (y_type), intent(inout) :: y        ! y = h (xa)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  !
   real                         :: dy, dym  !
   integer                      :: num_levs ! obs vertical levels

   integer                      :: k
   real, dimension(xp%kts-1:xp%kte+1)   :: pre_ma,tv_ma
   integer                      :: layer1,layer2,ks,ke
   real                         :: tv1,tv2,pres2

   real, dimension(xp%kts-1:xp%kte+1)   :: TGL_pre_ma,TGL_tv_ma
   real                         :: TGL_tv1,TGL_tv2

   if ( iv%num_satem > 0 ) then
      do n=iv%ob_numb(iv%current_ob_time-1)%satem + 1, &
         iv%ob_numb(iv%current_ob_time)%satem

         !xyh         y%satem(n)%thickness(:) = 0.0
         num_levs = iv % satem(n) % info % levels

         ! [1.0] Get horizontal interpolation weights:

         i = iv%satem(n)%loc%i
         dy = iv%satem(n)%loc%dy
         dym = iv%satem(n)%loc%dym
         j = iv%satem(n)%loc%j
         dx = iv%satem(n)%loc%dx
         dxm = iv%satem(n)%loc%dxm
         ks = xp%kts; ke = xp%kte


         ! [2.0] Virtual temperature at obs pt.

         call da_tv_profile_tl(xp,xa,xb,i,j,dx,dxm,dy,dym,                &
              pre_ma,tv_ma,TGL_pre_ma,TGL_tv_ma)

         ! [3.0] Find model vertical position of pressure and do interp.

         call da_find_layer_tl(layer2,tv2,iv%satem(n)%ref_p,              &
              pre_ma,tv_ma,ks,ke,TGL_tv2,TGL_pre_ma,TGL_tv_ma)
         pres2 = iv%satem(n)%ref_p

         ! [4.0] Thickness calculation

         do k=1, num_levs
            if ( ABS( iv % satem(n) %p (k) - missing_r) > 1. ) then

               call da_find_layer_tl(layer1,tv1,iv%satem(n)%p(k),            &
                  pre_ma,tv_ma,ks,ke,TGL_tv1,TGL_pre_ma,TGL_tv_ma)

               call da_thickness_tl(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1,layer2,&
                  iv%satem(n)%p(k),pres2,TGL_pre_ma,TGL_tv_ma,           &
                  TGL_tv1,TGL_tv2,y%satem(n)%thickness(k))

               pres2 = iv%satem(n)%p(k)
               layer2 = layer1
               tv2 = tv1
            end if
         end do
      end do
   end if

end subroutine da_transform_xtoy_satem

