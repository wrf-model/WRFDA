subroutine da_transform_xtoy_satem_adj(iv, jo_grad_y, jo_grad_x, grid)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)
   type (domain),  intent(in)    :: grid

   integer                       :: n        ! Loop counter.
   integer                       :: i, j     ! Index dimension.
   real                          :: dx, dxm  !
   real                          :: dy, dym  !
   integer                       :: num_levs ! obs vertical levels

   integer                        :: k
   real, dimension(kts-1:kte+1)       :: pre_ma,tv_ma
   integer                        :: layer1,layer2,ks,ke
   real                           :: tv1,tv2,pres2

   real, dimension(kts-1:kte+1)       :: ADJ_pre_ma,ADJ_tv_ma
   real                           :: ADJ_tv1,ADJ_tv2

   ADJ_pre_ma(:) = 0.0
   ADJ_tv_ma(:)  = 0.0
   ADJ_tv1 = 0.0
   ADJ_tv2 = 0.0

   if (iv%num_satem > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%satem + 1, &
         iv%ob_numb(iv%current_ob_time)%satem
         num_levs = iv % satem(n) % info % levels

         ! [1.0] Get horizontal interpolation weights:

         i   = iv%satem(n)%loc%i
         dy  = iv%satem(n)%loc%dy
         dym = iv%satem(n)%loc%dym
         j   = iv%satem(n)%loc%j
         dx  = iv%satem(n)%loc%dx
         dxm = iv%satem(n)%loc%dxm
         ks  = kts; ke = kte

         ! [2.0] Virtual temperature at obs pt.

         call da_tv_profile(grid,i,j,dx,dxm,dy,dym,pre_ma,tv_ma)

         ! [3.0] Find model vertical position of pressure and do interp.

         call da_find_layer(layer2,tv2,iv%satem(n)%ref_p,pre_ma,tv_ma,ks,ke)
         pres2 = iv%satem(n)%ref_p

         ! [4.0] Adjoint calculation of satem thickness

         do k=1, num_levs
            if (ABS(iv % satem(n) %p (k) - missing_r) > 1.0) then
               call da_find_layer(layer1,tv1,iv%satem(n)%p(k),pre_ma,tv_ma, &
                 ks,ke)

               call da_thickness_adj(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1, &
                   layer2, iv%satem(n)%p(k),pres2,ADJ_pre_ma,ADJ_tv_ma, &
                   ADJ_tv1,ADJ_tv2,jo_grad_y%satem(n)%thickness(k))

               call da_find_layer_adj(layer1,tv1,iv%satem(n)%p(k),         &
                   pre_ma,tv_ma,ks,ke,ADJ_tv1,ADJ_pre_ma,ADJ_tv_ma)

               pres2 = iv%satem(n)%p(k)
               layer2 = layer1
               tv2 = tv1
            end if
         end do

         ! [5.0] Adjoint of layer-finding and vertical interpolation

         call da_find_layer_adj(layer2,tv2,iv%satem(n)%ref_p,              &
              pre_ma,tv_ma,ks,ke,ADJ_tv2,ADJ_pre_ma,ADJ_tv_ma)

         ! [6.0] Adjoint of horizontal interpolation

         call da_tv_profile_adj(grid,jo_grad_x,i,j,dx,dxm,dy,dym,         &
              pre_ma,tv_ma,ADJ_pre_ma,ADJ_tv_ma)
      end do
   end if

end subroutine da_transform_xtoy_satem_adj


