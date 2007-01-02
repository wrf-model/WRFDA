SUBROUTINE DA_TRANSFORM_XTOY_SATEM_ADJ( iv, xp, jo_grad_y, jo_grad_x, xb)
!-------------------------------------------------------------------------
!  History: 01/2002 - Creation of F90 version. Qingnong Xiao
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)
   TYPE (xb_type),  INTENT(IN)   :: xb          ! first guess state.

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j     ! Index dimension.
   REAL                          :: dx, dxm  !
   REAL                          :: dy, dym  !
   INTEGER                       :: num_levs ! obs vertical levels

   INTEGER                        :: k
   REAL, DIMENSION(xp%kts-1:xp%kte+1)       :: pre_ma,tv_ma
   INTEGER                        :: layer1,layer2,ks,ke
   REAL                           :: tv1,tv2,pres2

   REAL, DIMENSION(xp%kts-1:xp%kte+1)       :: ADJ_pre_ma,ADJ_tv_ma
   REAL                           :: ADJ_tv1,ADJ_tv2

   ADJ_pre_ma(:) = 0.
   ADJ_tv_ma(:)  = 0.
   ADJ_tv1 = 0.
   ADJ_tv2 = 0.
    
   IF ( iv%num_satem > 0 ) THEN

      do n=iv%ob_numb(iv%current_ob_time-1)%satem + 1, iv%ob_numb(iv%current_ob_time)%satem

       if(iv%satem(n)%loc%proc_domain_with_halo) then

         num_levs = iv % satem(n) % info % levels

!        [1.0] Get horizontal interpolation weights:

         i = iv%satem(n)%loc%i
         dy = iv%satem(n)%loc%dy
         dym = iv%satem(n)%loc%dym
         j = iv%satem(n)%loc%j
         dx = iv%satem(n)%loc%dx
         dxm = iv%satem(n)%loc%dxm
         ks = xp%kts; ke = xp%kte

!        [2.0] Virtual temperature at obs pt.

         call Tv_profile(xp,xb,i,j,dx,dxm,dy,dym,pre_ma,tv_ma)

!        [3.0] Find model vertical position of pressure and do interp.

         call find_layer(layer2,tv2,iv%satem(n)%ref_p,pre_ma,tv_ma,ks,ke)
         pres2 = iv%satem(n)%ref_p

!        [4.0] Adjoint calculation of Satem thickness

         do k=1, num_levs

          IF ( ABS( iv % satem(n) %p (k) - missing_r) > 1. ) THEN
 
            call find_layer(layer1,tv1,iv%satem(n)%p(k),pre_ma,tv_ma,ks,ke)
 
            call ADJ_thickness(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1,layer2,  &
                 iv%satem(n)%p(k),pres2,ADJ_pre_ma,ADJ_tv_ma,           &
                 ADJ_tv1,ADJ_tv2,jo_grad_y%satem(n)%thickness(k))
 
            call ADJ_find_layer(layer1,tv1,iv%satem(n)%p(k),            &
                 pre_ma,tv_ma,ks,ke,ADJ_tv1,ADJ_pre_ma,ADJ_tv_ma)

            pres2 = iv%satem(n)%p(k)
            layer2 = layer1
            tv2 = tv1

          ENDIF

         enddo

!        [5.0] Adjoint of layer-finding and vertical interpolation

         call ADJ_find_layer(layer2,tv2,iv%satem(n)%ref_p,              &
              pre_ma,tv_ma,ks,ke,ADJ_tv2,ADJ_pre_ma,ADJ_tv_ma)

!        [6.0] Adjoint of horizontal interpolation

         call ADJ_Tv_profile(xp,jo_grad_x,xb,i,j,dx,dxm,dy,dym,            &
              pre_ma,tv_ma,ADJ_pre_ma,ADJ_tv_ma)

        endif

      ENDDO

   ENDIF

END SUBROUTINE DA_TRANSFORM_XTOY_SATEM_ADJ

