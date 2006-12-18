subroutine Tv_profile(xp, xb, i, j, dx, dxm, dy, dym, pre_ma, tv_ma)

!  This subroutine calculates virtual temperature (tv_ma) on each level
!  (pre_ma, pressure at the level) at the observed location (i,j). 
!  dx, dxm, dy, dym are horizontal interpolation weighting.

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)     :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Dimensions and xpose buffers.
   INTEGER                        :: i, j     ! OBS location
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
                          
   integer                        :: ii,jj,ks,ke    ! Index dimension.
   REAL, DIMENSION(xp%kts-1:xp%kte+1)   :: pre_ma,tv_ma
   real, dimension(2,2,xp%kts:xp%kte)   :: tv_m     ! Virtual temperatures

   ks = xp%kts; ke = xp%kte

!  Virtual temperature

   do ii=i,i+1
   do jj=j,j+1
      tv_m(ii-i+1,jj-j+1,ks:ke) = xb%t(ii,jj,ks:ke) * (1. + 0.61*xb%q(ii,jj,ks:ke))
   enddo
   enddo

!  Horizontal interpolation to the obs. pt.

   pre_ma(ks:ke) = dym* ( dxm * xb%p(i,j,ks:ke) + dx * xb%p(i+1,j,ks:ke) ) + &
                   dy * ( dxm * xb%p(i,j+1,ks:ke) + dx * xb%p(i+1,j+1,ks:ke) )

   tv_ma (ks:ke) = dym* ( dxm * tv_m (1,1,ks:ke) + dx * tv_m (2,1,ks:ke) ) + &
                   dy * ( dxm * tv_m (1,2,ks:ke) + dx * tv_m (2,2,ks:ke) )

end subroutine Tv_profile


subroutine find_layer(layer,tv,pre,pre_ma,tv_ma,ks,ke)

!  This routine is to find the vertical location in the Tv profile given
!  a specific pressure and vertically interpolate Tv to that height.
!  pre_ma,tv_ma give vertical profile of virtual temperature
!  pre is a given pressure, alpha is the percentage of pre in the layer.
!  layer,tv are calculated vertical layer and interpolated virtual temp.
 
   IMPLICIT NONE

   integer                    :: ks, ke, k
   integer                    :: layer
   real, dimension(ks-1:ke+1) :: pre_ma, tv_ma
   real                       :: pre, tv
   real                       :: alpha, coef1, coef2

   if ( pre >= pre_ma(ks) ) then

!     Below model bottom
      layer = ks
      alpha = log(pre_ma(ks)/pre)/log(pre_ma(ks)/pre_ma(ks+1))
      tv = tv_ma(ks) * (1.-alpha) + tv_ma(ks+1) * alpha
      pre_ma(ks-1)=pre
   else if (pre <= pre_ma(ke) ) then

!     Above model top
      layer = ke+1
      alpha = log(pre_ma(ke-1)/pre)/log(pre_ma(ke-1)/pre_ma(ke))
      tv = tv_ma(ke-1) * (1.-alpha) + tv_ma(ke) * alpha
      pre_ma(ke+1) = pre
   else

!     Between model layers 
      do k=ks,ke-1
         if ( pre>=pre_ma(k+1) .and. pre<pre_ma(k) ) then
            layer = k+1
            alpha = log(pre_ma(k)/pre)/log(pre_ma(k)/pre_ma(k+1))
            tv = tv_ma(k) * (1.-alpha) + tv_ma(k+1) * alpha
            exit
         endif
      enddo
   endif
 
end subroutine find_layer


subroutine thickness(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1,layer2,pre1,pre2,thk)

!  This routine calculates the thickness between two layers 
!  using vertical integration of virtual temperatures.
!  pre1 and pre2 are two pressures for the two layers

   IMPLICIT NONE

   integer                  :: layer1,layer2         ! two layers
   real                     :: tv1,tv2               ! virtual temp.
   real                     :: pre1,pre2             ! pressure

   integer                  :: ks,ke,k
   real, dimension(ks-1:ke+1) :: pre_ma,tv_ma,p_tmp    ! Tv profile

   real                     :: thk                   ! thickness

!  Thickness at the top and bottom parts of the layer.

   thk = 0.5 * gas_constant/gravity * (tv1*log(pre_ma(layer1-1)/pre1) +  &
               tv2*log(pre2/pre_ma(layer2)) )

!  Temporary pressure

   p_tmp(layer1) = pre1
   p_tmp(layer2-1) = pre2
   do k = layer2, layer1-1
      p_tmp(k) = pre_ma(k)
   enddo

!  Vertical integration of the virtual temperature

   do k=layer2,layer1-1
      thk = thk + 0.5 * gas_constant/gravity * tv_ma(k) *                &
               log(p_tmp(k-1)/p_tmp(k+1))
   enddo

end subroutine thickness


subroutine TGL_Tv_profile(xp,xa,xb,i,j,dx,dxm,dy,dym,             &
           pre_ma,tv_ma,TGL_pre_ma,TGL_tv_ma)

!  This is the tangent-linear routine for Tv_profile

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)     :: xa       ! gridded analysis increment.
   TYPE (xb_type),  INTENT(IN)    :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Dimensions and xpose buffers.
   INTEGER                        :: i, j     ! OBS location
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.

   integer                        :: ii,jj,ks,ke
   real, dimension(2,2,xp%kts:xp%kte)       :: tv_m,TGL_tv_m
   real, dimension(xp%kts-1:xp%kte+1)       :: pre_ma,tv_ma
   real, dimension(xp%kts-1:xp%kte+1)       :: TGL_pre_ma,TGL_tv_ma

   ks = xp%kts; ke = xp%kte

   do ii=i,i+1
   do jj=j,j+1

      TGL_tv_m(ii-i+1,jj-j+1,ks:ke) = xa%t(ii,jj,ks:ke)*(1.+0.61*xb%q(ii,jj,ks:ke)) +  &
                                      0.61*xb%t(ii,jj,ks:ke)*xa%q(ii,jj,ks:ke)
      tv_m(ii-i+1,jj-j+1,ks:ke)     = xb%t(ii,jj,ks:ke)*(1.+0.61*xb%q(ii,jj,ks:ke))

   enddo
   enddo
 
   TGL_pre_ma(ks:ke) = dym* ( dxm * xa%p(i,j,ks:ke) + dx * xa%p(i+1,j,ks:ke) ) + &
                       dy * ( dxm * xa%p(i,j+1,ks:ke) + dx * xa%p(i+1,j+1,ks:ke) )
   TGL_tv_ma (ks:ke) = dym* ( dxm * TGL_tv_m(1,1,ks:ke) + dx * TGL_tv_m(2,1,ks:ke) ) + &
                       dy * ( dxm * TGL_tv_m(1,2,ks:ke) + dx * TGL_tv_m(2,2,ks:ke) )
   pre_ma(ks:ke) = dym* ( dxm * xb%p(i,j,ks:ke) + dx * xb%p(i+1,j,ks:ke) ) + &
                   dy * ( dxm * xb%p(i,j+1,ks:ke) + dx * xb%p(i+1,j+1,ks:ke) )
   tv_ma (ks:ke) = dym* ( dxm * tv_m (1,1,ks:ke) + dx * tv_m (2,1,ks:ke) ) + &
                   dy * ( dxm * tv_m (1,2,ks:ke) + dx * tv_m (2,2,ks:ke) )
 
end subroutine TGL_Tv_profile


subroutine TGL_find_layer(layer,tv,pre,pre_ma,tv_ma,ks,ke,     &
                          TGL_tv,TGL_pre_ma,TGL_tv_ma)

!  This is the tangent-linear routine for find_layer

   IMPLICIT NONE

   integer                   :: ks, ke, k
   integer                   :: layer
   real, dimension(ks-1:ke+1)  :: pre_ma, tv_ma
   real, dimension(ks-1:ke+1)  :: TGL_pre_ma, TGL_tv_ma
   real                      :: pre, tv
   real                      :: TGL_tv
   real                      :: alpha, coef1, coef2
   real                      :: TGL_alpha

!  coef1, coef2 are temporarily used in this routine

   if ( pre >= pre_ma(ks) ) then

!     Below model bottom
      layer = ks
      coef1=log(pre/pre_ma(ks+1))/(pre_ma(ks)*     &
            (log(pre_ma(ks)/pre_ma(ks+1)))**2)
      coef2=log(pre_ma(ks)/pre)/(pre_ma(ks+1)*     &
            (log(pre_ma(ks)/pre_ma(ks+1)))**2)
      TGL_alpha = coef1 * TGL_pre_ma(ks) + coef2 * TGL_pre_ma(ks+1)
      alpha = log(pre_ma(ks)/pre)/log(pre_ma(ks)/pre_ma(ks+1))

      TGL_tv = (1.-alpha)*TGL_tv_ma(ks) +               &
               (tv_ma(ks+1)-tv_ma(ks))*TGL_alpha +     &
               alpha*TGL_tv_ma(ks+1)
      TGL_pre_ma(ks-1) = 0.
      tv = tv_ma(ks) * (1.-alpha) + tv_ma(ks+1) * alpha
      pre_ma(ks-1) = pre

   else if (pre <= pre_ma(ke) ) then

!     Above model top
      layer = ke+1
      coef1=log(pre/pre_ma(ke))/(pre_ma(ke-1)*           &
            (log(pre_ma(ke-1)/pre_ma(ke)))**2)
      coef2=log(pre_ma(ke-1)/pre)/(pre_ma(ke)*           &
            (log(pre_ma(ke-1)/pre_ma(ke)))**2)
      TGL_alpha = coef1 * TGL_pre_ma(ke-1) + coef2 * TGL_pre_ma(ke)
      alpha = log(pre_ma(ke-1)/pre)/log(pre_ma(ke-1)/pre_ma(ke))

      TGL_tv = (1.-alpha)*TGL_tv_ma(ke-1) +                 &
               (tv_ma(ke)-tv_ma(ke-1))*TGL_alpha +           &
               alpha*TGL_tv_ma(ke)
      TGL_pre_ma(ke+1) = 0.
      tv = tv_ma(ke-1) * (1.-alpha) + tv_ma(ke) * alpha
      pre_ma(ke+1) = pre

   else

!     Between model layers
      do k=ks,ke-1
         if ( pre>=pre_ma(k+1) .and. pre<pre_ma(k) ) then
            layer = k+1
            coef1=log(pre/pre_ma(k+1))/(pre_ma(k)*   &
                  (log(pre_ma(k)/pre_ma(k+1)))**2)
            coef2=log(pre_ma(k)/pre)/(pre_ma(k+1)*   &
                  (log(pre_ma(k)/pre_ma(k+1)))**2)
            TGL_alpha = coef1 * TGL_pre_ma(k) + coef2 * TGL_pre_ma(k+1)
            alpha = log(pre_ma(k)/pre)/log(pre_ma(k)/pre_ma(k+1))
            TGL_tv = (1.-alpha)*TGL_tv_ma(k) +                 &
                     (tv_ma(k+1)-tv_ma(k))*TGL_alpha +         &
                      alpha*TGL_tv_ma(k+1)
            tv = tv_ma(k) * (1.-alpha) + tv_ma(k+1) * alpha
            exit
         endif
      enddo

   endif
 
end subroutine TGL_find_layer


subroutine TGL_thickness(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1,layer2,pre1,pre2,   &
                         TGL_pre_ma,TGL_tv_ma,TGL_tv1,TGL_tv2,TGL_thk)

!  This is the tangent-linear routine for thickness

   IMPLICIT NONE

   integer                  :: layer1,layer2
   integer                  :: ks,ke,k
   real, dimension(ks-1:ke+1) :: pre_ma,tv_ma,p_tmp
   real, dimension(ks-1:ke+1) :: TGL_pre_ma,TGL_tv_ma,TGL_p_tmp
   real                     :: tv1,tv2
   real                     :: TGL_tv1,TGL_tv2
   real                     :: pre1,pre2
   real                     :: thk
   real                     :: TGL_thk

   TGL_thk = TGL_tv1*log(pre_ma(layer1-1)/pre1) +          &
             TGL_pre_ma(layer1-1)*tv1/pre_ma(layer1-1) +    &
             TGL_tv2*log(pre2/pre_ma(layer2)) -            &
             TGL_pre_ma(layer2)*tv2/pre_ma(layer2)
   TGL_thk = 0.5 * gas_constant/gravity * TGL_thk

   TGL_p_tmp(layer1) = 0.
   p_tmp(layer1) = pre1
   TGL_p_tmp(layer2-1) = 0.
   p_tmp(layer2-1) = pre2

   do k=layer2,layer1-1
      TGL_p_tmp(k) = TGL_pre_ma(k)
      p_tmp(k) = pre_ma(k)
   enddo

!  Vertical integration of the virtual temperature

   do k=layer2,layer1-1
      TGL_thk = TGL_thk + 0.5 * gas_constant/gravity * (   &
                TGL_tv_ma(k)*log(p_tmp(k-1)/p_tmp(k+1)) + &
                TGL_p_tmp(k-1)*tv_ma(k)/p_tmp(k-1) -       &
                TGL_p_tmp(k+1)*tv_ma(k)/p_tmp(k+1)     )
   enddo

end subroutine TGL_thickness


subroutine ADJ_Tv_profile(xp,jo_grad_x,xb,i,j,dx,dxm,dy,dym,    &
              pre_ma,tv_ma,ADJ_pre_ma,ADJ_tv_ma)

!  This is the adjoint routine for Tv_profile

   IMPLICIT NONE

   TYPE (x_type) , INTENT(INOUT)  :: jo_grad_x ! grad_x(jo)
   TYPE (xb_type),  INTENT(IN)    :: xb        ! first guess state.
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Dimensions and xpose buffers.
   INTEGER                        :: i, j      ! OBS location
   REAL                           :: dx, dxm   ! Interpolation weights.
   REAL                           :: dy, dym   ! Interpolation weights.

   integer                        :: ii,jj,ks,ke
   real, dimension(2,2,xp%kts:xp%kte)    :: tv_m,ADJ_tv_m
   REAL, DIMENSION(xp%kts-1:xp%kte+1)    :: pre_ma,tv_ma
   REAL, DIMENSION(xp%kts-1:xp%kte+1)    :: ADJ_pre_ma,ADJ_tv_ma

   ks = xp%kts; ke = xp%kte

   ADJ_tv_m(1,1,ks:ke) = dym*dxm * ADJ_tv_ma (ks:ke)
   ADJ_tv_m(2,1,ks:ke) = dym*dx *  ADJ_tv_ma (ks:ke)
   ADJ_tv_m(1,2,ks:ke) = dy*dxm*   ADJ_tv_ma (ks:ke)
   ADJ_tv_m(2,2,ks:ke) = dy*dx*    ADJ_tv_ma (ks:ke)
   jo_grad_x%p(i,j,ks:ke)    = jo_grad_x%p(i,j,ks:ke) + dym*dxm  * ADJ_pre_ma(ks:ke)
   jo_grad_x%p(i+1,j,ks:ke)  = jo_grad_x%p(i+1,j,ks:ke) + dym*dx * ADJ_pre_ma(ks:ke)
   jo_grad_x%p(i,j+1,ks:ke)  = jo_grad_x%p(i,j+1,ks:ke) + dy*dxm * ADJ_pre_ma(ks:ke)
   jo_grad_x%p(i+1,j+1,ks:ke)= jo_grad_x%p(i+1,j+1,ks:ke) + dy*dx* ADJ_pre_ma(ks:ke)
   ADJ_tv_ma (ks:ke)     = 0.
   ADJ_pre_ma(ks:ke)     = 0.

   do ii=i,i+1
   do jj=j,j+1
      jo_grad_x%t(ii,jj,ks:ke) = jo_grad_x%t(ii,jj,ks:ke) +                    &
                  ADJ_tv_m(ii-i+1,jj-j+1,ks:ke)*(1.+0.61*xb%q(ii,jj,ks:ke))
      jo_grad_x%q(ii,jj,ks:ke) = jo_grad_x%q(ii,jj,ks:ke) +                    &
                  0.61*xb%t(ii,jj,ks:ke)*ADJ_tv_m(ii-i+1,jj-j+1,ks:ke)
   enddo
   enddo

end subroutine ADJ_Tv_profile


subroutine ADJ_find_layer(layer,tv,pre,pre_ma,tv_ma,ks,ke,     &
                          ADJ_tv,ADJ_pre_ma,ADJ_tv_ma)

!  This is the adjoint routine for find_layer

   IMPLICIT NONE

   integer                   :: ks, ke, k
   integer                   :: layer
   real, dimension(ks-1:ke+1)  :: pre_ma, tv_ma
   real, dimension(ks-1:ke+1)  :: ADJ_pre_ma, ADJ_tv_ma
   real                      :: pre, tv
   real                      :: ADJ_tv
   real                      :: alpha, coef1, coef2
   real                      :: ADJ_alpha

   if ( pre >= pre_ma(ks) ) then

      layer = ks
      coef1=log(pre/pre_ma(ks+1))/(pre_ma(ks)*     &
            (log(pre_ma(ks)/pre_ma(ks+1)))**2)
      coef2=log(pre_ma(ks)/pre)/(pre_ma(ks+1)*     &
            (log(pre_ma(ks)/pre_ma(ks+1)))**2)
      alpha = log(pre_ma(ks)/pre)/log(pre_ma(ks)/pre_ma(ks+1))

      ADJ_pre_ma(ks-1)= 0.
      ADJ_tv_ma(ks)   = ADJ_tv_ma(ks) + (1.-alpha)*ADJ_tv
      ADJ_alpha        = (tv_ma(ks+1)-tv_ma(ks))*ADJ_tv
      ADJ_tv_ma(ks+1) = ADJ_tv_ma(ks+1) + alpha*ADJ_tv

      ADJ_pre_ma(ks)    = ADJ_pre_ma(ks) + coef1 * ADJ_alpha
      ADJ_pre_ma(ks+1)  = ADJ_pre_ma(ks+1) + coef2 * ADJ_alpha

   else if (pre <= pre_ma(ke) ) then

      layer = ke+1
      coef1=log(pre/pre_ma(ke))/(pre_ma(ke-1)*           &
            (log(pre_ma(ke-1)/pre_ma(ke)))**2)
      coef2=log(pre_ma(ke-1)/pre)/(pre_ma(ke)*           &
            (log(pre_ma(ke-1)/pre_ma(ke)))**2)
      alpha = log(pre_ma(ke-1)/pre)/log(pre_ma(ke-1)/pre_ma(ke))

      ADJ_pre_ma(ke+1)    = 0.
      ADJ_tv_ma(ke-1)     = ADJ_tv_ma(ke-1) + (1.-alpha)*ADJ_tv
      ADJ_alpha        = (tv_ma(ke)-tv_ma(ke-1))*ADJ_tv
      ADJ_tv_ma(ke)     = ADJ_tv_ma(ke) + alpha*ADJ_tv

      ADJ_pre_ma(ke-1) = ADJ_pre_ma(ke-1) + coef1 * ADJ_alpha
      ADJ_pre_ma(ke) = ADJ_pre_ma(ke) + coef2 * ADJ_alpha

   else

      do k=ks,ke-1
         if ( pre>=pre_ma(k+1) .and. pre<pre_ma(k) ) then
            layer = k+1
            coef1=log(pre/pre_ma(k+1))/(pre_ma(k)*   &
                  (log(pre_ma(k)/pre_ma(k+1)))**2)
            coef2=log(pre_ma(k)/pre)/(pre_ma(k+1)*   &
                  (log(pre_ma(k)/pre_ma(k+1)))**2)
            alpha = log(pre_ma(k)/pre)/log(pre_ma(k)/pre_ma(k+1))

            ADJ_tv_ma(k)     = ADJ_tv_ma(k) + (1.-alpha)*ADJ_tv
            ADJ_alpha        = (tv_ma(k+1)-tv_ma(k))*ADJ_tv
            ADJ_tv_ma(k+1)   = ADJ_tv_ma(k+1) + alpha * ADJ_tv

            ADJ_pre_ma(k)   = ADJ_pre_ma(k) + coef1 * ADJ_alpha
            ADJ_pre_ma(k+1) = ADJ_pre_ma(k+1) + coef2 * ADJ_alpha
            exit
         endif
      enddo

   endif
   ADJ_tv           = 0.
 
end subroutine ADJ_find_layer


subroutine ADJ_thickness(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1,layer2,pre1,pre2,   &
                         ADJ_pre_ma,ADJ_tv_ma,ADJ_tv1,ADJ_tv2,ADJ_thk)

!  This is the adjoint routine for thickness

   IMPLICIT NONE

   integer                  :: layer1,layer2
   integer                  :: ks,ke,k
   real, dimension(ks-1:ke+1) :: pre_ma,tv_ma,p_tmp
   real, dimension(ks-1:ke+1) :: ADJ_pre_ma,ADJ_tv_ma,ADJ_p_tmp
   real                     :: tv1,tv2
   real                     :: ADJ_tv1,ADJ_tv2
   real                     :: pre1,pre2
   real                     :: thk
   real                     :: ADJ_thk

!  p_tmp and ADJ_p_tmp are temporary (local) variables

   ADJ_p_tmp(:)=0.

   p_tmp(layer1) = pre1
   p_tmp(layer2-1) = pre2
   do k=layer2,layer1-1
      p_tmp(k) = pre_ma(k)
   enddo

   do k=layer2,layer1-1
      ADJ_p_tmp(k+1)  = ADJ_p_tmp(k+1) - 0.5*gas_constant/gravity *     &
                        ADJ_thk*tv_ma(k)/p_tmp(k+1)
      ADJ_p_tmp(k-1)  = ADJ_p_tmp(k-1) + 0.5*gas_constant/gravity *     &
                        ADJ_thk*tv_ma(k)/p_tmp(k-1)
      ADJ_tv_ma(k)    = ADJ_tv_ma(k)   + 0.5*gas_constant/gravity *     &
                        ADJ_thk*log(p_tmp(k-1)/p_tmp(k+1))
   enddo

   do k=layer2,layer1-1
      ADJ_pre_ma(k) = ADJ_pre_ma(k) + ADJ_p_tmp(k)
   enddo

   ADJ_thk = 0.5 * gas_constant/gravity * ADJ_thk
   ADJ_pre_ma(layer2) = ADJ_pre_ma(layer2) - ADJ_thk*tv2/pre_ma(layer2)
   ADJ_tv2 = ADJ_tv2 + ADJ_thk*log(pre2/pre_ma(layer2))
   ADJ_pre_ma(layer1-1) = ADJ_pre_ma(layer1-1) +              &
                          ADJ_thk*tv1/pre_ma(layer1-1)
   ADJ_tv1 = ADJ_tv1 + ADJ_thk*log(pre_ma(layer1-1)/pre1)

end subroutine ADJ_thickness

