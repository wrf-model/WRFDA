subroutine da_thickness_tl(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1,layer2,pre1,pre2,   &
                         TGL_pre_ma,TGL_tv_ma,TGL_tv1,TGL_tv2,TGL_thk)

   !-----------------------------------------------------------------------
   ! Purpose: tangent-linear routine for thickness
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                  :: layer1,layer2
   integer, intent(in)                  :: ks,ke
   real, dimension(ks-1:ke+1), intent(in) :: pre_ma,tv_ma
   real, dimension(ks-1:ke+1), intent(in) :: TGL_pre_ma,TGL_tv_ma
   real, intent(in)                     :: tv1,tv2
   real, intent(in)                     :: TGL_tv1,TGL_tv2
   real, intent(in)                     :: pre1,pre2
   real, intent(out)                    :: TGL_thk

   integer                    :: k
   real, dimension(ks-1:ke+1) :: p_tmp
   real, dimension(ks-1:ke+1) :: TGL_p_tmp

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
   end do

   ! Vertical integration of the virtual temperature

   do k=layer2,layer1-1
      TGL_thk = TGL_thk + 0.5 * gas_constant/gravity * (   &
                TGL_tv_ma(k)*log(p_tmp(k-1)/p_tmp(k+1)) + &
                TGL_p_tmp(k-1)*tv_ma(k)/p_tmp(k-1) -       &
                TGL_p_tmp(k+1)*tv_ma(k)/p_tmp(k+1)     )
   end do

end subroutine da_thickness_tl


