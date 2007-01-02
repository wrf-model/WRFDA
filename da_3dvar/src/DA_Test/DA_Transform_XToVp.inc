SUBROUTINE DA_Transform_XToVp( xb, xbx, xa, xp, vp, be, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
!------------------------------------------------------------------------------
!  PURPOSE: Transforms analysis to control variables (Vp-type)
!
!  HISTORY:   02/02/2005 - Updated for global              Syed RH Rizvi 
!             07/07/2005 - Include cv_options = 5.         Y.-R. Guo
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)           :: xb         ! First guess structure.
   TYPE (xbx_type),INTENT(IN)           :: xbx        ! Header/non-gridded vars.
   TYPE (x_type), INTENT(INOUT)         :: xa         ! Analysis increments.
   type (xpose_type), intent(inout)     :: xp         ! Dimensions and xpose buffers. 
   TYPE (vp_type), INTENT(OUT)          :: vp         ! CV on grid structure.
   type (be_type), intent(in), optional :: be         ! Background errors.
   integer, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
   integer, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
   integer, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

   real, dimension(ims:ime,jms:jme,kms:kme) :: vor, & ! Vorticity.
                                               div, & ! Divergence.
                                               phi_b  ! Balanced mass increment.

   real, dimension(kms:kme,ims:ime,jms:jme) :: phi_b_copy
   real, dimension(kms:kme,jms:jme,kms:kme) :: reg_copy
   real, dimension(ims:ime,jms:jme)         :: one_over_m2    !   Multiplicative coeff.

   integer :: i, j, k, kk                             ! Loop counters.
   real :: sum_vphi                                   ! Summing variable.

!-------------------------------------------------------------------
!  [1.0] Perform transform v = U^{-1} x
!-------------------------------------------------------------------      
   print*,' in XToVp'     
   IF ( cv_options /= 2 .and. cv_options /=4 .and. cv_options /= 5) THEN
      write(unit=*, fmt='(a,i6)') &
           'Cannot perform Transform_XToVp for cv_options:', cv_options

      CALL wrf_shutdown
      stop 'Wrong cv_options.'
   ENDIF



   call DA_Zero_vp_type ( vp )

!  [2.2] Transform u, v to streamfunction via vorticity:

!  Communicate halo region.
   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)
  print*,' calling UV_To_Vorticity'
#ifndef DEREF_KLUDGE
   CALL DA_UV_To_Vorticity( xb, xa % u, xa % v, vor, &
#else
   CALL DA_UV_To_Vorticity( xb, xa % u(ims,jms,kms), &
                            xa % v(ims,jms,kms), &
                            vor(ims,jms,kms), &
#endif
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

!  Convert vorticity to Del**2 psi:
   if(.not. global ) then               
     if (fg_format == 1 .or. fg_format == 2) then
    one_over_m2(its:ite,jts:jte) = 1.0 / ( xb % map_factor(its:ite,jts:jte) * &
                                        xb % map_factor(its:ite,jts:jte) )
    DO k = kts, kte
       vor(its:ite,jts:jte,k) = one_over_m2(its:ite,jts:jte)*vor(its:ite,jts:jte,k)
    END DO
    else
    print*,' Wrong choice of fg_format= ',fg_format,' with global = ',global
    stop
    end if
   end if

!  Calculate psi:
  print*,' calling Solve_PoissonEquation for Psi'
   CALL DA_Solve_PoissonEqn_FCT(xb, xbx, vor, vp%v1, xp)

!  [2.3] Transform u, v to velocity potential via divergence:

  print*,' calling UV_To_Divergence'
#ifndef DEREF_KLUDGE
   CALL DA_UV_To_Divergence(xb, xa % u, xa % v, div, &
#else
   CALL DA_UV_To_Divergence(xb, xa % u(ims,jms,kms), &
                                xa % v(ims,jms,kms), &
                            div(ims,jms,kms), &
#endif
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

!  Convert divergence to Del**2 chi:
   if(.not. global )  then              
     if (fg_format == 1 .or. fg_format == 2) then
    DO k = kts, kte
      div(its:ite,jts:jte,k) = one_over_m2(its:ite,jts:jte) * div(its:ite,jts:jte,k)
    END DO
    else
    print*,' Wrong choice of fg_format= ',fg_format,' with global = ',global
    stop
    end if
   end if

!  Calculate chi:

  print*,' calling Solve_PoissonEquation for Chi'
   CALL DA_Solve_PoissonEqn_FCT(xb, xbx, div, vp%v2, xp)

   if( cv_options == 4 .or.cv_options == 5 ) then
!  [2.4] Transform chi to chi_u:
  print*,' calculating chi_u'                         
    do k=kts,kte
    do j=jts,jte
      vp%v2(its:ite,j,k) = vp%v2(its:ite,j,k) - be%reg_chi(j,k)*vp%v1(its:ite,j,k)
    enddo
    enddo
   end if

   if( cv_options == 2) then
!  Communicate halo region.
     CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id1)

!  [2.5] Calculate phi_b from u and v and use it with p
!   to calculate unbalanced phi:

#ifndef DEREF_KLUDGE
     CALL DA_Balance_Equation_Lin( xb, xbx, xp,                &
                                 xa % u, xa % v, phi_b,      &
#else
     CALL DA_Balance_Equation_Lin( xb, xbx, xp,                &
                                 xa % u(ims,jms,kms), &
                                 xa % v(ims,jms,kms), &
                                 phi_b(ims,jms,kms), &
#endif
                                 ids,ide, jds,jde, kds,kde,  &
                                 ims,ime, jms,jme, kms,kme,  &
                                 its,ite, jts,jte, kts,kte )
!  [2.4] Compute phi:
     IF ( PRESENT(be) ) THEN


        DO k = kts, kte ! Need copies for all model levels for sum:

           reg_copy(k,jts:jte,kts:kte) = be % pb_vert_reg(jts:jte,kts:kte,k)
           phi_b_copy(k,its:ite,jts:jte) = phi_b(its:ite,jts:jte,k)
    
        END DO

        DO k = kts, kte
        DO j = jts, jte
        DO i = its, ite
!--------SUM OVER ALL LEVELS (kts:kte):
           sum_vphi = 0.0

           DO kk = kts, kte
              sum_vphi = sum_vphi + reg_copy(kk,j,k) * phi_b_copy(kk,i,j)
           END DO
           xa % p(i,j,k) = sum_vphi
        END DO
        END DO
        END DO
     ELSE
        xa % p(its:ite,jts:jte,kts:kte) = phi_b(its:ite,jts:jte,kts:kte)
     ENDIF
     vp % v3(its:ite,jts:jte,kts:kte) = xa % p(its:ite,jts:jte,kts:kte)

   else if( cv_options == 4 .or. cv_options == 5 ) then
     print*,' calculating t_u'                         
!  [2.4] Compute t_u:
     do k=kts,kte
     do j=jts,jte
     do i=its,ite
        vp%v3(i,j,k) = xa%t(i,j,k) - sum(be%reg_t(j,k,kts:kte)*vp%v1(i,j,kts:kte))
     enddo
     enddo
     enddo
   end if

!  [2.6] Choice of moisture control variable: 
   
   if (cv_options == 2 ) then

     IF ( cv_options_hum == 2 ) THEN

       CALL DA_TPQ_To_RH_Lin( xb, xp, xa )

       vp % v4(its:ite,jts:jte,kts:kte) = xa % rh(its:ite,jts:jte,kts:kte)

     else IF ( cv_options_hum == 3 ) THEN
       xa % qt(its:ite,jts:jte,kts:kte) = xa % q  (its:ite,jts:jte,kts:kte) +   &
                                          xa % qcw(its:ite,jts:jte,kts:kte) +   &
                                          xa % qrn(its:ite,jts:jte,kts:kte)
       vp % v4(its:ite,jts:jte,kts:kte) = xa % qt (its:ite,jts:jte,kts:kte)
     ELSE IF ( cv_options_hum == 1 ) THEN

      vp % v4(its:ite,jts:jte,kts:kte) = xa % q(its:ite,jts:jte,kts:kte)
     end if
   else if( cv_options == 4 .or. cv_options == 5 ) then       ! Pseudo RH

     print*,' calculating psudo rh'                    
     vp % v4(its:ite,jts:jte,kts:kte) = xa % q  (its:ite,jts:jte,kts:kte) /   &
                                        xb % qs (its:ite,jts:jte,kts:kte) 
   end if

!  [2.7] Choice of surface pressure control variable: 
      
   if ( cv_options == 2) then
!  [2.7] v5 set to zero (ground temperature in future):
     vp % v5(its:ite,jts:jte,kts:kte) = 0.0

   else if ( cv_options == 4 .or.cv_options == 5 ) then
!  [2.7] compute psfc_u:
     print*,' calculating psfc_u '                     
     do j=jts,jte
     do i=its,ite
       vp % v5(i,j,1) = xa%psfc(i,j) - sum(be%reg_ps(j,kts:kte)*vp%v1(i,j,kts:kte))
     end do
     end do
   end if

END SUBROUTINE DA_Transform_XToVp
