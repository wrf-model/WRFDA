SUBROUTINE DA_Transform_VpToX( xb, vp, xa, be )

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)           :: xb ! First guess structure.
   TYPE (vp_type), INTENT(IN)           :: vp ! CV on grid structure.
   TYPE (x_type), INTENT(OUT)           :: xa ! Analysis increments.
   TYPE (be_type), INTENT(IN), OPTIONAL :: be ! Background errors.

   INTEGER                              :: iy ! Array dimension.
   INTEGER                              :: jx ! Array dimension.
   INTEGER                              :: kz ! Array dimension.
   INTEGER                              :: i, j, k, kk  ! Loop counters.
   REAL                                 :: sum_vphi ! Summing variable.
   REAL, ALLOCATABLE                    :: phi_b(:,:,:) ! Balanced mass increment.
#ifndef __crayx1
   REAL, ALLOCATABLE                    :: phi_b_copy(:,:,:) ! Copy.
   REAL, ALLOCATABLE                    :: reg_copy(:,:,:) ! Copy
#endif
   
!-------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------- 
  
!-------------------------------------------------------------------
!  [2.0] Transform x=Uv:
!------------------------------------------------------------------- 
  
   IF ( cv_options == 1) THEN

!     [2.1] Control variables are u, v, t, q and p:
      
      xa % u(:,:,:) = vp % v1(:,:,:)
      xa % v(:,:,:) = vp % v2(:,:,:)
      xa % t(:,:,:) = vp % v3(:,:,:)
      xa % q(:,:,:) = vp % v4(:,:,:)
      xa % p(:,:,:) = vp % v5(:,:,:)

      CALL DA_PT_To_Rho_Lin( xb, xa )

   ELSE IF ( cv_options == 2) THEN

!     [2.2] Transform psi and chi_u to chi:

!     [2.3] Transform psi and chi to u and v:

      CALL DA_PsiChi_To_UV( xb, vp % v1(:,:,:), vp % v2(:,:,:), &
                            xa % u(:,:,:), xa % v(:,:,:) )

!     [2.3] Calculate phi_b from u and v:

      iy = SIZE(vp % v3(:,:,:),DIM=1)
      jx = SIZE(vp % v3(:,:,:),DIM=2)
      kz = SIZE(vp % v3(:,:,:),DIM=3)
      
      ALLOCATE( phi_b(1:iy,1:jx,1:kz) )

      CALL DA_Balance_Equation_Lin( xb, xa % u(:,:,:), xa % v(:,:,:), &
                                    phi_b(:,:,:) )

!     [2.4] Compute phi:

      IF ( PRESENT(be) ) THEN
#if defined  __crayx1
!dir$ concurrent
!dir$ preferstream
         DO k = 1, kz
          DO j = 1, jx
!dir$ concurrent
!dir$ prefervector
           DO i= 1, iy
            xa%p(i,j,k) = vp%v3(i,j,k)
            DO kk = 1, kz
             xa%p(i,j,k)=xa%p(i,j,k)+be%pb_vert_reg(i,k,kk)*phi_b(i,j,kk)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
#else      
         ALLOCATE( reg_copy(1:kz,1:iy,1:kz) )
         ALLOCATE( phi_b_copy(1:kz,1:iy,1:jx) )

         DO kk = 1, kz
            reg_copy(kk,1:iy,1:kz) = be % pb_vert_reg(1:iy,1:kz,kk)
            phi_b_copy(kk,1:iy,1:jx) = phi_b(1:iy,1:jx,kk)
         END DO

         DO k = 1, kz
            DO j = 1, jx
               DO i= 1, iy
                  sum_vphi = reg_copy(1,i,k) * phi_b_copy(1,i,j)
                  DO kk = 2, kz
                     sum_vphi = sum_vphi + &
                                reg_copy(kk,i,k) * phi_b_copy(kk,i,j)
                  END DO
                  xa % p(i,j,k) = vp % v3(i,j,k) + sum_vphi
               END DO
            END DO
         END DO
         DEALLOCATE( reg_copy )
         DEALLOCATE( phi_b_copy )
#endif
      ELSE
         xa % p(1:iy,1:jx,1:kz) = phi_b(1:iy,1:jx,1:kz) + &
                                  vp % v3(1:iy,1:jx,1:kz) 
      END IF

      xa % p(1:iy+1,jx+1,1:kz) = 0.0 ! Last p col not needed.
      xa % p(iy+1,1:jx+1,1:kz) = 0.0 ! Last p row not needed.

      DEALLOCATE( phi_b )

!     [2.5] Compute density increments using hydrostatic eqn:

      CALL DA_HydrostaticP_To_Rho_Lin( xb, xa % p(:,:,:), xa % rho(:,:,:) )

      xa % rho(1:iy+1,jx+1,1:kz) = 0.0 ! Last rho col not needed.
      xa % rho(iy+1,1:jx+1,1:kz) = 0.0 ! Last rho row not needed.
         
!     [2.6] Compute temperature increments from p/rho increments:

      CALL DA_PRho_To_T_Lin( xb, xa )

!     [2.7] Choice of moisture control variable:

      iy = SIZE(vp % v4(:,:,:),DIM=1)
      jx = SIZE(vp % v4(:,:,:),DIM=2)
      kz = SIZE(vp % v4(:,:,:),DIM=3)

      IF ( cv_options_hum == 1 ) THEN

         xa % q(1:iy,1:jx,1:kz) = vp % v4(1:iy,1:jx,1:kz)
  
      ELSE IF ( cv_options_hum == 2 ) THEN
      
         xa % rh(1:iy,1:jx,1:kz) = vp % v4(1:iy,1:jx,1:kz)
      
         CALL DA_TPRH_To_Q_Lin( xb % t, xb % p_c, xb % es, xb % q, xb % rh, &
#ifdef F95
                                xa % t, xa % p, xa % rh, xa % q )
#else
                                xa % t, xa % p, xa % rh, xa % q, iy*jx*kz )
#endif
        ELSE IF ( cv_options_hum == 3 ) THEN
  
  !     [2.10] Partition of hydrometeor increments via warm rain process
  
           xa % qt (1:iy,1:jx,1:kz) = vp % v4(1:iy,1:jx,1:kz)
  
           CALL DA_Moist_Phys_Lin( xb, xa,                         &
                                 1,xb%mix, 1,xb%mjx, 1,xb%mkx,  &
                                 1,xb%mix, 1,xb%mjx, 1,xb%mkx,  &
                                 1,xb%mix, 1,xb%mjx, 1,xb%mkx )
  
           xa % qcw(1:iy+1,jx+1,1:kz) = 0.0 ! Last col not needed.
           xa % qcw(iy+1,1:jx+1,1:kz) = 0.0 ! Last row not needed.
           xa % qrn(1:iy+1,jx+1,1:kz) = 0.0 ! Last col not needed.
           xa % qrn(iy+1,1:jx+1,1:kz) = 0.0 ! Last row not needed.
           xa % qt (1:iy+1,jx+1,1:kz) = 0.0 ! Last col not needed.
           xa % qt (iy+1,1:jx+1,1:kz) = 0.0 ! Last row not needed.

      END IF
      
      xa % q(1:iy+1,jx+1,1:kz) = 0.0 ! Last q col not needed.
      xa % q(iy+1,1:jx+1,1:kz) = 0.0 ! Last q row not needed.
         
!     [2.8] Ground temperature control variable:

!      iy = SIZE(vp % v5(:,:,:),DIM=1)
!      jx = SIZE(vp % v5(:,:,:),DIM=2)
!      kz = SIZE(vp % v5(:,:,:),DIM=3)
!      xa % tgrn(1:iy,1:jx,1:kz) = vp % v5(1:iy,1:jx,1:kz)
!      xa % tgrn(1:iy+1,jx+1,1:kz) = 0.0 ! Last col not needed.
!      xa % tgrn(iy+1,1:jx+1,1:kz) = 0.0 ! Last row not needed.

!     [2.9] Compute w increments using Richardson's eqn.
       if (W_INCREMENTS) &
      CALL DA_UVPRho_To_W_Lin( xb, xa,                        &
                               1,xb%mix, 1,xb%mjx, 1,xb%mkx,  &
                               1,xb%mix, 1,xb%mjx, 1,xb%mkx,  &
                               1,xb%mix, 1,xb%mjx, 1,xb%mkx ) 
         
   END IF     

END SUBROUTINE DA_Transform_VpToX







