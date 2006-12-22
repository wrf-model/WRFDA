subroutine da_transform_xtoy_gpspw_adj( xb, oi, xp, jo_grad_y, jo_grad_x )

   !----------------------------------------------------------------
   !  HISTORY
   !
   !    Modified: 2004-05-10 by S. R. H. Rizvi and H. Kusaka
   !    Purpose:  Considering the difference in the elevation 
   !              between model surface and GPS site
   !
   !    Reviewed and corrected the part of adjoint code by Y.-R. Guo
   !    because the original adjoint code is wrong.      07/16/2004
   !----------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb          ! first guess state.
   type (ob_type), intent(in)    :: oi          ! obs. inc vector (o-b).
   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: i, j     ! Index dimension.
   real                          :: dx, dxm  !
   real                          :: dy, dym  !

   integer                :: k        ! Index dimension.
   real                   :: dpw,ddpw     ! adjustment pw [mm]
   real                   :: obs_terr     ! real terrain height at GPS site [m]
   real                   :: model_terr   ! model terrain height at GPS site[m]
   real,dimension(xp%kts:xp%kte):: model_q   ! model q at GPS site [kg/kg]
   real,dimension(xp%kts:xp%kte):: model_z   ! model z at GPS site [m]
   real,dimension(xp%kts:xp%kte):: model_rho ! model rho at GPS site [kg/m^3]
   real,dimension(xp%kts:xp%kte):: model_dq   ! model dq at GPS site [kg/kg]
   real,dimension(xp%kts:xp%kte):: model_drho ! model drho at GPS site [kg/m^3]

   if ( oi%num_Gpspw > 0 ) then
!xyh      do n=1, oi % num_Gpspw
      do n=oi%ob_numb(oi%current_ob_time-1)%gpspw + 1, oi%ob_numb(oi%current_ob_time)%gpspw
         i = oi%gpspw(n)%loc%i
         dy = oi%gpspw(n)%loc%dy
         dym = oi%gpspw(n)%loc%dym
         j = oi%gpspw(n)%loc%j
         dx = oi%gpspw(n)%loc%dx
         dxm = oi%gpspw(n)%loc%dxm

         !  Initialise the varibles
         dpw           = 0.
         model_q(:)    = 0.
         model_dq(:)   = 0.
         model_rho(:)  = 0.
         model_drho(:) = 0.

         obs_terr   = oi%gpspw(n)%info%elv
         model_terr = dym*(dxm*xb%terr(i,j)   + dx*xb%terr(i+1,j)) + &
                      dy *(dxm*xb%terr(i,j+1) + dx*xb%terr(i+1,j+1))

         dpw =       0.1 *jo_grad_y%gpspw(n)%tpw

         jo_grad_x%tpw(i  ,j  ) = jo_grad_x%tpw(i  ,j  ) &
                                + dxm*dym*jo_grad_y%gpspw(n)%tpw
         jo_grad_x%tpw(i+1,j  ) = jo_grad_x%tpw(i+1,j  ) &
                                + dym*dx *jo_grad_y%gpspw(n)%tpw
         jo_grad_x%tpw(i  ,j+1) = jo_grad_x%tpw(i  ,j+1) &
                                + dxm *dy*jo_grad_y%gpspw(n)%tpw
         jo_grad_x%tpw(i+1,j+1) = jo_grad_x%tpw(i+1,j+1) &
                                + dx *dy *jo_grad_y%gpspw(n)%tpw

         if ( obs_terr <= model_terr ) then 
           model_q(1)   = dym*(dxm*xb%q(i,j,1)   + dx*xb%q(i+1,j,1)) + &
                          dy *(dxm*xb%q(i,j+1,1) + dx*xb%q(i+1,j+1,1))
           model_rho(1) = dym*(dxm*xb%rho(i,j,1)   + dx*xb%rho(i+1,j,1)) + &
                          dy *(dxm*xb%rho(i,j+1,1) + dx*xb%rho(i+1,j+1,1))

           model_dq(1)   =  model_rho(1)*(obs_terr - model_terr)*dpw
           model_drho(1) =  model_q(1)  *(obs_terr - model_terr)*dpw

           jo_grad_x%q(i,j,1)       = jo_grad_x%q(i,j,1)       + dym*dxm*model_dq(1)
           jo_grad_x%q(i+1,j,1)     = jo_grad_x%q(i+1,j,1)     + dym*dx*model_dq(1)
           jo_grad_x%q(i,j+1,1)     = jo_grad_x%q(i,j+1,1)     + dy*dxm*model_dq(1)
           jo_grad_x%q(i+1,j+1,1)   = jo_grad_x%q(i+1,j+1,1)   + dy*dx*model_dq(1)

           jo_grad_x%rho(i,j,1)     = jo_grad_x%rho(i,j,1)     + dym*dxm*model_drho(1)
           jo_grad_x%rho(i+1,j,1)   = jo_grad_x%rho(i+1,j,1)   + dym*dx*model_drho(1)
           jo_grad_x%rho(i,j+1,1)   = jo_grad_x%rho(i,j+1,1)   + dy*dxm*model_drho(1)
           jo_grad_x%rho(i+1,j+1,1) = jo_grad_x%rho(i+1,j+1,1) + dy*dx*model_drho(1)
         else 
           model_z(1) = dym*(dxm*xb%hf(i,j,1)   + dx*xb%hf(i+1,j,1)) + &
                        dy *(dxm*xb%hf(i,j+1,1) + dx*xb%hf(i+1,j+1,1))

           do k = xp%kts, xp%kte
             if ( model_z(k) >= obs_terr ) exit

             model_z(k+1) = dym*(dxm*xb%hf(i,j,k+1)   + dx*xb%hf(i+1,j,k+1)) + &
                            dy *(dxm*xb%hf(i,j+1,k+1) + dx*xb%hf(i+1,j+1,k+1))
             model_q(k) = dym*(dxm*xb%q(i,j,k)   + dx*xb%q(i+1,j,k)) + &
                          dy *(dxm*xb%q(i,j+1,k) + dx*xb%q(i+1,j+1,k))
             model_rho(k) = dym*(dxm*xb%rho(i,j,k)   + dx*xb%rho(i+1,j,k)) + &
                            dy *(dxm*xb%rho(i,j+1,k) + dx*xb%rho(i+1,j+1,k))

             ddpw = dpw

             if ( model_z(k+1) <= obs_terr ) then
               model_dq  (k) = model_rho(k) *(model_z(k+1) - model_z(k))*ddpw 
               model_drho(k) = model_q(k)   *(model_z(k+1) - model_z(k))*ddpw 
             else
               model_dq  (k) = model_rho(k) *(obs_terr-model_z(k))*ddpw 
               model_drho(k) = model_q(k)   *(obs_terr-model_z(k))*ddpw 
             end if 

             ! No feedback to x%hf was considered here (Refer to comments in
             ! da_transform_xtoy_gpspw.inc).       Y.-R. Guo  07/15/2002
             !......................................................................... 

             jo_grad_x%q(i,j,k)       = jo_grad_x%q(i,j,k)       + dym*dxm*model_dq(k)
             jo_grad_x%q(i+1,j,k)     = jo_grad_x%q(i+1,j,k)     + dym*dx*model_dq(k)
             jo_grad_x%q(i,j+1,k)     = jo_grad_x%q(i,j+1,k)     + dy*dxm*model_dq(k)
             jo_grad_x%q(i+1,j+1,k)   = jo_grad_x%q(i+1,j+1,k)   + dy*dx*model_dq(k)

             jo_grad_x%rho(i,j,k)     = jo_grad_x%rho(i,j,k)     + dym*dxm*model_drho(k)
             jo_grad_x%rho(i+1,j,k)   = jo_grad_x%rho(i+1,j,k)   + dym*dx*model_drho(k)
             jo_grad_x%rho(i,j+1,k)   = jo_grad_x%rho(i,j+1,k)   + dy*dxm*model_drho(k)
             jo_grad_x%rho(i+1,j+1,k) = jo_grad_x%rho(i+1,j+1,k) + dy*dx*model_drho(k)
           end do
         end if 
      end do
   end if

end subroutine da_transform_xtoy_gpspw_adj


