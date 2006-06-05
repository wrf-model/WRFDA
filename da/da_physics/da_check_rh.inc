      SUBROUTINE da_check_rh( xb, xa, xp )

!------------------------------------------------------------------------------
!  PURPOSE: Remove RH over 100% or under 10%
!           Made the modification to those levels, which RH are less than 95%
!
!  HISTORY: 01/07/2001 - Creation of F90 version.           Shuhua Chen
!           02/21/2002 - Modified for parallel version.     Al Bourgeois.
!
!  PARENT_MODULE: DA_Check
!------------------------------------------------------------------------------
      IMPLICIT NONE

      TYPE (xb_type), INTENT(in)           :: xb    ! First guess state
      TYPE (x_type),  INTENT(inout)        :: xa    ! Analysis increment
      type (xpose_type), intent(in)        :: xp    ! Domain decomposition vars.

      integer   :: imod(xp%kts:xp%kte)
      real      :: rhtol(xp%kts:xp%kte)
      real      :: x_qs(xp%kts:xp%kte)
      real      :: oldq(xp%kts:xp%kte)
      real      :: dz(xp%kts:xp%kte)

      integer :: is,ie, js,je, ks,ke, i, j, k
      real    :: tol_adjust_moist, tol_moist, oldrha, each_moist, es, weight
      real    :: upper_modify_rh, lower_modify_rh

      is = xp % its
      ie = xp % ite
      js = xp % jts
      je = xp % jte
      ks = xp % kts
      ke = xp % kte

      upper_modify_rh = 95.
      lower_modify_rh = 11.
!
      DO i=is,ie
      DO j=js,je

         tol_adjust_moist = 0.
         tol_moist        = 0.

         dz(1)             = xb%ztop-xb%hf(i,j,1)
         imod(1)           = 0
         x_qs(1)           = 0.
         rhtol(1)          = xb%rh(i,j,1)+xa%rh(i,j,1)
         oldq (1)          = xa%q(i,j,1)

         DO k=ks+1,ke
            dz(k)=xb%hf(i,j,k-1)-xb%hf(i,j,k)

            imod(k)           = 0
            x_qs(k)           = 0.
            rhtol(k)          = xb%rh(i,j,k)+xa%rh(i,j,k)
            oldq (k)          = xa%q(i,j,k)
         ENDDO

         DO k=ks,ke
            if ( rhtol(k) .gt. maximum_rh ) then
               oldrha       = xa%rh(i,j,k)
! modify xa%rh
               xa%rh(i,j,k) = maximum_rh - xb%rh(i,j,k)

               call DA_TP_To_Qs( xb%t(i,j,k)+xa%t(i,j,k), xb%p(i,j,k)+xa%p(i,j,k), &
                                 es, x_qs(k))

! calculate xa%q
               call DA_TPRH_To_Q_Lin1( xb%t(i,j,k), xb%p(i,j,k), xb%es(i,j,k),      &
                                 xb%q(i,j,k), xb%rh(i,j,k),  xa%t(i,j,k) ,           &
                                 xa%p(i,j,k), xa%rh(i,j,k), xa%q(i,j,k) )

               tol_adjust_moist = tol_adjust_moist + x_qs(k)*(oldrha - xa%rh(i,j,k))* &
                                  dz(k)*(xb%rho(i,j,k)+xa%rho(i,j,k))
               imod(k)=-1
            endif

            if ( rhtol(k) .lt. minimum_rh ) then
               oldrha           = xa%rh(i,j,k)
               xa%rh(i,j,k)     = minimum_rh - xb%rh(i,j,k)
               call DA_TP_To_Qs( xb%t(i,j,k)+xa%t(i,j,k), xb%p(i,j,k)+xa%p(i,j,k), &
                                 es, x_qs(k) )

               call DA_TPRH_To_Q_Lin1( xb%t(i,j,k), xb%p(i,j,k), xb%es(i,j,k),      &
                                 xb%q(i,j,k), xb%rh(i,j,k),  xa%t(i,j,k),            &
                                 xa%p(i,j,k), xa%rh(i,j,k), xa%q(i,j,k) )


               tol_adjust_moist = tol_adjust_moist + x_qs(k)*(oldrha - xa%rh(i,j,k))* &
                                  dz(k)*(xb%rho(i,j,k)+xa%rho(i,j,k))
               imod(k)=-1
            endif
         ENDDO
      
         if ( tol_adjust_moist .gt. 0. ) then
            DO k=ks,ke
               if ( rhtol(k) .lt. upper_modify_rh .and. imod(k) .eq. 0) then
                  call DA_TP_To_Qs( xb%t(i,j,k)+xa%t(i,j,k), &
                                    xb%p(i,j,k)+xa%p(i,j,k),es,x_qs(k))

                  each_moist   = rhtol(k)*x_qs(k)* &
                                 dz(k)*(xb%rho(i,j,k)+xa%rho(i,j,k))
                  tol_moist    = tol_moist + each_moist
                  imod(k)      = 1
               endif
            ENDDO
         endif

         if ( tol_adjust_moist .lt. 0. ) then
            DO k=ks,ke
               if ( rhtol(k) .gt. lower_modify_rh .and. imod(k) .eq. 0) then
                  call DA_TP_To_Qs( xb%t(i,j,k)+xa%t(i,j,k), &
                                    xb%p(i,j,k)+xa%p(i,j,k), es, x_qs(k) )

                  each_moist   = rhtol(k)*x_qs(k)* &
                                 dz(k)*(xb%rho(i,j,k)+xa%rho(i,j,k))
                  tol_moist    = tol_moist + each_moist
                  imod(k)      = 1
               endif
            ENDDO
         endif

         if (tol_moist > 0) then
           weight       = tol_adjust_moist/tol_moist
           DO k=ks,ke
             if ( imod(k) .eq. 1) then
               xa%rh(i,j,k) = xa%rh(i,j,k)+(xb%rh(i,j,k)+xa%rh(i,j,k))*weight
               call DA_TPRH_To_Q_Lin1( xb%t(i,j,k), xb%p(i,j,k), xb%es(i,j,k),  &
                                      xb%q(i,j,k), xb%rh(i,j,k),  xa%t(i,j,k) , &
                                      xa%p(i,j,k), xa%rh(i,j,k), xa%q(i,j,k) )

             endif
           ENDDO
         endif

      ENDDO
      ENDDO

      END SUBROUTINE da_check_rh

