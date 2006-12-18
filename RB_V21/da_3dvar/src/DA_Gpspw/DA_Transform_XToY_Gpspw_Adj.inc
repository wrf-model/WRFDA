SUBROUTINE DA_Transform_XToY_Gpspw_Adj( xb, oi, xp, jo_grad_y, jo_grad_x )
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

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)    :: xb          ! first guess state.
   TYPE (ob_type), INTENT(IN)    :: oi          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j     ! Index dimension.
   REAL                          :: dx, dxm  !
   REAL                          :: dy, dym  !

!-- 2004-04-08
   INTEGER                :: k        ! Index dimension.
   REAL                   :: dpw,ddpw     ! adjustment pw [mm]
   REAL                   :: obs_terr     ! real terrain height at GPS site [m]
   REAL                   :: model_terr   ! model terrain height at GPS site[m]
   REAL,DIMENSION(xp%kts:xp%kte):: model_q   ! model q at GPS site [kg/kg]
   REAL,DIMENSION(xp%kts:xp%kte):: model_z   ! model z at GPS site [m]
   REAL,DIMENSION(xp%kts:xp%kte):: model_rho ! model rho at GPS site [kg/m^3]
   REAL,DIMENSION(xp%kts:xp%kte):: model_dq   ! model dq at GPS site [kg/kg]
   REAL,DIMENSION(xp%kts:xp%kte):: model_dz   ! model dz at GPS site [m]
   REAL,DIMENSION(xp%kts:xp%kte):: model_drho ! model drho at GPS site [kg/m^3]
!--

   IF ( oi%num_Gpspw > 0 ) THEN


      do n=1, oi % num_Gpspw

       if(oi%gpspw(n)%loc%proc_domain_with_halo) then

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

         IF ( obs_terr <= model_terr ) THEN 

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

         ELSE 

            model_z(1) = dym*(dxm*xb%hf(i,j,1)   + dx*xb%hf(i+1,j,1)) + &
                         dy *(dxm*xb%hf(i,j+1,1) + dx*xb%hf(i+1,j+1,1))

! ..............................................................................
! The following part of code is written by Y.-R. Guo             07/16/2004

            DO k = xp%kts, xp%kte

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
!
! No feedback to x%hf was considered here (Refer to comments in
! DA_Transform_XToY_Gpspw.inc).       Y.-R. Guo  07/15/2002
! ..................................................................................... 
               jo_grad_x%q(i,j,k)       = jo_grad_x%q(i,j,k)       + dym*dxm*model_dq(k)
               jo_grad_x%q(i+1,j,k)     = jo_grad_x%q(i+1,j,k)     + dym*dx*model_dq(k)
               jo_grad_x%q(i,j+1,k)     = jo_grad_x%q(i,j+1,k)     + dy*dxm*model_dq(k)
               jo_grad_x%q(i+1,j+1,k)   = jo_grad_x%q(i+1,j+1,k)   + dy*dx*model_dq(k)

               jo_grad_x%rho(i,j,k)     = jo_grad_x%rho(i,j,k)     + dym*dxm*model_drho(k)
               jo_grad_x%rho(i+1,j,k)   = jo_grad_x%rho(i+1,j,k)   + dym*dx*model_drho(k)
               jo_grad_x%rho(i,j+1,k)   = jo_grad_x%rho(i,j+1,k)   + dy*dxm*model_drho(k)
               jo_grad_x%rho(i+1,j+1,k) = jo_grad_x%rho(i+1,j+1,k) + dy*dx*model_drho(k)

             END DO

         END IF 
!-- 
       endif

      end do

   ENDIF

END SUBROUTINE DA_Transform_XToY_Gpspw_Adj
