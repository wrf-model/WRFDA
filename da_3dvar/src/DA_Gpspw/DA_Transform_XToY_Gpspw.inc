SUBROUTINE DA_Transform_XToY_Gpspw ( xb, iv, xa, xp, y )
!----------------------------------------------------------------
!  HISTORY
!
!    Modified: 2004-04-08 by H. Kusaka and S. R. H. Rizvi
!    Purpose:  Considering the difference in the elevation 
!              between model surface and GPS site
!
!    Reviewed and re-written part of the code by Y.-R. Guo 
!    because the logic is not suitable for adjoint code.            
!                                                    07/16/2004
!----------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)   :: xb       ! first guess state.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
!--   
   INTEGER                :: k          ! Index dimension.
   REAL                   :: dpw, ddpw  ! adjustment pw [mm]
   REAL                   :: obs_terr   ! real terrain height at GPS site [m]
   REAL                   :: model_terr ! model terrain height at GPS site[m]
   REAL,DIMENSION(xp%kts:xp%kte):: model_q    ! model q at GPS site [kg/kg]
   REAL,DIMENSION(xp%kts:xp%kte):: model_z    ! model z at GPS site [m]
   REAL,DIMENSION(xp%kts:xp%kte):: model_rho  ! model rho at GPS site [kg/m^3]
   REAL,DIMENSION(xp%kts:xp%kte):: model_dq   ! model dq at GPS site [kg/kg]
   REAL,DIMENSION(xp%kts:xp%kte):: model_dz   ! model dz at GPS site [m]
   REAL,DIMENSION(xp%kts:xp%kte):: model_drho ! model drho at GPS site [kg/m^3]
!--

   IF ( iv%num_Gpspw > 0 ) THEN

      y % gpspw(:)% tpw = 0.0

      do n=1, iv % num_Gpspw

       if(iv%gpspw(n)%loc%proc_domain_with_halo) then

         i = iv%gpspw(n)%loc%i
         dy = iv%gpspw(n)%loc%dy
         dym = iv%gpspw(n)%loc%dym
         j = iv%gpspw(n)%loc%j
         dx = iv%gpspw(n)%loc%dx
         dxm = iv%gpspw(n)%loc%dxm

! Mathematical transformation:

!-- kusaka 2004-04-08
!         y % gpspw(n)% tpw = dym* ( dxm * xa%tpw(i,j) + &
!                                    dx  * xa%tpw(i+1,j) ) + &
!                             dy * ( dxm * xa%tpw(i,j+1) + &
!                                    dx  * xa%tpw(i+1,j+1) )
         dpw = 0.0
         obs_terr   = iv%gpspw(n)%info%elv
         model_terr = dym*(dxm*xb%terr(i,j)   + dx*xb%terr(i+1,j)) + &
                      dy *(dxm*xb%terr(i,j+1) + dx*xb%terr(i+1,j+1))

         IF ( obs_terr <= model_terr ) THEN 

            model_q(1) = dym*(dxm*xb%q(i,j,1)   + dx*xb%q(i+1,j,1)) + &
                         dy *(dxm*xb%q(i,j+1,1) + dx*xb%q(i+1,j+1,1))
            model_rho(1) = dym*(dxm*xb%rho(i,j,1)   + dx*xb%rho(i+1,j,1)) + &
                           dy *(dxm*xb%rho(i,j+1,1) + dx*xb%rho(i+1,j+1,1))

            model_dq(1) = dym*(dxm*xa%q(i,j,1)   + dx*xa%q(i+1,j,1)) + &
                          dy *(dxm*xa%q(i,j+1,1) + dx*xa%q(i+1,j+1,1))
            model_drho(1) = dym*(dxm*xa%rho(i,j,1)   + dx*xa%rho(i+1,j,1)) + &
                            dy *(dxm*xa%rho(i,j+1,1) + dx*xa%rho(i+1,j+1,1))

            dpw = ( model_rho(1)*model_dq(1) + model_drho(1)*model_q(1) ) &
                * ( obs_terr - model_terr )

         ELSE 

            model_z(1) = dym*(dxm*xb%hf(i,j,1)   + dx*xb%hf(i+1,j,1)) + &
                         dy *(dxm*xb%hf(i,j+1,1) + dx*xb%hf(i+1,j+1,1))

         DO k = xp%kts, xp%kte

            if (model_z(k) >= obs_terr ) exit

            model_z(k+1) = dym*(dxm*xb%hf(i,j,k+1)   + dx*xb%hf(i+1,j,k+1)) + &
                           dy *(dxm*xb%hf(i,j+1,k+1) + dx*xb%hf(i+1,j+1,k+1))

            model_q(k) = dym*(dxm*xb%q(i,j,k)   + dx*xb%q(i+1,j,k)) + &
                         dy *(dxm*xb%q(i,j+1,k) + dx*xb%q(i+1,j+1,k))
            model_rho(k) = dym*(dxm*xb%rho(i,j,k)   + dx*xb%rho(i+1,j,k)) + &
                           dy *(dxm*xb%rho(i,j+1,k) + dx*xb%rho(i+1,j+1,k))

            model_dq(k) = dym*(dxm*xa%q(i,j,k)   + dx*xa%q(i+1,j,k)) + &
                          dy *(dxm*xa%q(i,j+1,k) + dx*xa%q(i+1,j+1,k))
            model_drho(k) = dym*(dxm*xa%rho(i,j,k)   + dx*xa%rho(i+1,j,k)) + &
                            dy *(dxm*xa%rho(i,j+1,k) + dx*xa%rho(i+1,j+1,k))
!
! Here assumed that "model_z" is constant, i.e. xa%hf=0.0. With MM5, 
! this is true, but with WRF, xa%hf may not be zero (?). In the WRF 
! model space (x,y,znu), only the "znu" is constant, but all variables 
! including height could be changed at the "znu" levels. So here is only 
! an approximation for WRF. The following few lines of code is written
! by Y.-R. Guo 07/16/2004.
!
            if ( model_z(k+1) <= obs_terr ) then
               ddpw = ( model_rho(k)*model_dq(k) + model_drho(k)*model_q(k) ) &
                    * ( model_z(k+1) - model_z(k) )
            else 
               ddpw = ( model_rho(k)*model_dq(k) + model_drho(k)*model_q(k) ) &
                    * ( obs_terr -  model_z(k) )
            endif

            dpw = dpw + ddpw

          END DO 

         END IF 

         y % gpspw(n)% tpw = dym* ( dxm * xa%tpw(i,j) + &
                                    dx  * xa%tpw(i+1,j) ) + &
                             dy * ( dxm * xa%tpw(i,j+1) + &
                                    dx  * xa%tpw(i+1,j+1) ) &
                             + 0.1*dpw

!-- 

       endif

      end do

   ENDIF

END SUBROUTINE DA_Transform_XToY_Gpspw

