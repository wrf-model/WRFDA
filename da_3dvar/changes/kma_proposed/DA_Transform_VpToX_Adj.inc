SUBROUTINE DA_Transform_VpToX_Adj( xb, xa, vp, be )

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)           :: xb ! First guess structure.
   TYPE (x_type), INTENT(INOUT)         :: xa ! Grad_x(Jo)
   TYPE (vp_type), INTENT(INOUT)        :: vp ! CV on grid structure.
   TYPE (be_type), INTENT(IN), OPTIONAL :: be ! Background errors.

   INTEGER                       :: iy               ! Size of 1st dim.
   INTEGER                       :: jx               ! Size of 2nd dim.
   INTEGER                       :: kz               ! Size of 3rd dim.
   INTEGER                       :: i, j, k, kk      ! Loop counters.
   REAL                          :: sum_v_p          ! Summing variable.
   REAL, ALLOCATABLE             :: phi_b(:,:,:)     ! Balanced mass inc.
#ifndef __crayx1
   REAL, ALLOCATABLE             :: p_copy(:,:,:)    ! Copy.
   REAL, ALLOCATABLE             :: reg_copy(:,:,:)  ! Copy
#endif

   IF ( cv_options == 1) THEN
      
      CALL DA_PT_To_Rho_Adj( xb, xa )
         
      vp % v1(:,:,:) = vp % v1(:,:,:) + xa % u(:,:,:)
      vp % v2(:,:,:) = vp % v2(:,:,:) + xa % v(:,:,:)
      vp % v3(:,:,:) = vp % v3(:,:,:) + xa % t(:,:,:)
      vp % v4(:,:,:) = vp % v4(:,:,:) + xa % q(:,:,:)
      vp % v5(:,:,:) = vp % v5(:,:,:) + xa % p(:,:,:)

   ELSE IF ( cv_options == 2) THEN

!    [2.9] Compute w increments using Richardson's eqn.
     if (W_INCREMENTS) &
     CALL DA_UVPRho_To_W_Adj( xb, xa,                        &
                              1,xb%mix, 1,xb%mjx, 1,xb%mkx,  &
                              1,xb%mix, 1,xb%mjx, 1,xb%mkx,  &
                              1,xb%mix, 1,xb%mjx, 1,xb%mkx )

!    [2.8] Ground temperature control variable:

!      iy = SIZE(vp % v5(:,:,:),DIM=1)
!      jx = SIZE(vp % v5(:,:,:),DIM=2)
!      kz = SIZE(vp % v5(:,:,:),DIM=3)

!      vp % v5(1:iy+1,jx+1,1:kz) = 0.0 ! Last col not needed.
!      vp % v5(iy+1,1:jx+1,1:kz) = 0.0 ! Last row not needed.
!      vp % v5(1:iy,1:jx,1:kz) = vp % v5(1:iy,1:jx,1:kz) + &
!                                 xa % tgrn(1:iy,1:jx,1:kz)
    
!     [2.7] Choice of moisture control variable:

      iy = SIZE(vp % v4(:,:,:),DIM=1)
      jx = SIZE(vp % v4(:,:,:),DIM=2)
      kz = SIZE(vp % v4(:,:,:),DIM=3)
      
      IF ( cv_options_hum == 1 ) THEN

         vp % v4(1:iy,1:jx,1:kz) = vp % v4(1:iy,1:jx,1:kz) + &
                                   xa % q(1:iy,1:jx,1:kz)
  
      ELSE IF ( cv_options_hum == 2 ) THEN
      
         CALL DA_TPRH_To_Q_Adj( xb % t, xb % p_c, xb % es, xb % q, xb % rh, &
#ifdef F95
                                xa % t, xa % p, xa % rh, xa % q )
#else
                                xa % t, xa % p, xa % rh, xa % q, iy*jx*kz )
#endif
         vp % v4(1:iy,1:jx,1:kz) = vp % v4(1:iy,1:jx,1:kz) + &
                                   xa % rh(1:iy,1:jx,1:kz)
     ELSE IF ( cv_options_hum == 3 ) THEN

!    [2.10] Partition of hydrometeor increments via warm rain process

         CALL DA_Moist_Phys_Adj( xb, xa,                         &
                                  1,xb%mix, 1,xb%mjx, 1,xb%mkx,  &
                                  1,xb%mix, 1,xb%mjx, 1,xb%mkx,  &
                                  1,xb%mix, 1,xb%mjx, 1,xb%mkx )

         vp % v4(1:iy,1:jx,1:kz) = vp % v4(1:iy,1:jx,1:kz) + &
                                   xa % qt(1:iy,1:jx,1:kz)


      END IF

!     [2.6] Compute temperature increments from p/rho increments:
               
      CALL DA_PRho_To_T_Adj( xb, xa )
         
!     [2.5] Compute density increments using hydrostatic eqn:

      CALL DA_HydrostaticP_To_Rho_Adj( xb, xa % rho(:,:,:), xa % p(:,:,:) )
      
!     [2.4] Compute phi:

      iy = SIZE(vp % v3(:,:,:),DIM=1)
      jx = SIZE(vp % v3(:,:,:),DIM=2)
      kz = SIZE(vp % v3(:,:,:),DIM=3)
         
      ALLOCATE( phi_b(1:iy,1:jx,1:kz) )

      IF ( PRESENT(be) ) THEN
#if defined __crayx1
!dir$ concurrent
!dir$ preferstream
         DO k = 1, kz
          DO j = 1, jx
!dir$ concurrent
!dir$ prefervector
           DO i= 1, iy
            phi_b(i,j,k) = 0.0
            DO kk = 1, kz
             phi_b(i,j,k)=phi_b(i,j,k)+be%pb_vert_reg(i,kk,k)*xa%p(i,j,kk)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
#else      
         ALLOCATE( reg_copy(1:kz,1:iy,1:kz) )
         ALLOCATE( p_copy(1:kz,1:iy,1:jx) )
      
         DO kk = 1, kz
            reg_copy(kk,1:iy,1:kz) = be % pb_vert_reg(1:iy,kk,1:kz)
            p_copy(kk,1:iy,1:jx) = xa % p(1:iy,1:jx,kk)
         END DO
      
         DO k = 1, kz
            DO j = 1, jx
               DO i= 1, iy
                  sum_v_p = reg_copy(1,i,k) * p_copy(1,i,j)
                  DO kk = 2, kz
                     sum_v_p= sum_v_p + &
                              reg_copy(kk,i,k) * p_copy(kk,i,j)
                  END DO
                  phi_b(i,j,k) = sum_v_p
               END DO
            END DO
         END DO
         
         DEALLOCATE( reg_copy )
         DEALLOCATE( p_copy )
#endif
         
      ELSE
         phi_b(1:iy,1:jx,1:kz) = xa % p(1:iy,1:jx,1:kz)
      END IF

      vp % v3(1:iy,1:jx,1:kz) = vp % v3(1:iy,1:jx,1:kz) + &
                                xa % p(1:iy,1:jx,1:kz)

!     [2.3] Calculate phi_b from u and v:

      CALL DA_Balance_Equation_Adj( xb, phi_b(:,:,:), &
                                    xa % u(:,:,:), xa % v(:,:,:) )

      DEALLOCATE( phi_b )

!     [2.2] Transform psi and chi to u and v:

      CALL DA_PsiChi_To_UV_Adj( xb, xa % u(:,:,:), xa % v(:,:,:), &
                                vp % v1(:,:,:), vp % v2(:,:,:) )

!     [2.1] Transform psi and chi_u to chi:

   END IF     

END SUBROUTINE DA_Transform_VpToX_Adj

