SUBROUTINE DA_Transform_XToY_Satem ( xa, xb, iv, xp, y )
!-------------------------------------------------------------------------
!  History: 01/2002 - Creation of F90 version. Qingnong Xiao
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (xb_type),  INTENT(IN)  :: xb       ! first guess state.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  !
   REAL                         :: dy, dym  !
   INTEGER                      :: num_levs ! obs vertical levels

   INTEGER                      :: k
   REAL, DIMENSION(xp%kts-1:xp%kte+1)   :: pre_ma,tv_ma
   INTEGER                      :: layer1,layer2,ks,ke
   REAL                         :: tv1,tv2,pres2

   REAL, DIMENSION(xp%kts-1:xp%kte+1)   :: TGL_pre_ma,TGL_tv_ma
   REAL                         :: TGL_tv1,TGL_tv2

   IF ( iv%num_satem > 0 ) THEN

      do n=iv%ob_numb(iv%current_ob_time-1)%satem + 1, iv%ob_numb(iv%current_ob_time)%satem

       if(iv%satem(n)%loc%proc_domain_with_halo) then

!xyh         y%satem(n)%thickness(:) = 0.0
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

         call TGL_Tv_profile(xp,xa,xb,i,j,dx,dxm,dy,dym,                   &
              pre_ma,tv_ma,TGL_pre_ma,TGL_tv_ma)

!        [3.0] Find model vertical position of pressure and do interp.

         call TGL_find_layer(layer2,tv2,iv%satem(n)%ref_p,              &
              pre_ma,tv_ma,ks,ke,TGL_tv2,TGL_pre_ma,TGL_tv_ma)
         pres2 = iv%satem(n)%ref_p

!        [4.0] Thickness calculation

         do k=1, num_levs

          IF ( ABS( iv % satem(n) %p (k) - missing_r) > 1. ) THEN
 
            call TGL_find_layer(layer1,tv1,iv%satem(n)%p(k),            &
                 pre_ma,tv_ma,ks,ke,TGL_tv1,TGL_pre_ma,TGL_tv_ma)
 
            call TGL_thickness(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1,layer2,  &
                 iv%satem(n)%p(k),pres2,TGL_pre_ma,TGL_tv_ma,           &
                 TGL_tv1,TGL_tv2,y%satem(n)%thickness(k))

            pres2 = iv%satem(n)%p(k)
            layer2 = layer1
            tv2 = tv1

          ENDIF

         enddo

        endif

      ENDDO

   ENDIF

END SUBROUTINE DA_Transform_XToY_Satem

