subroutine da_transform_xtovp( xb, xbx, xa, xp, vp, be, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )

   !---------------------------------------------------------------------------
   ! PURPOSE: Transforms analysis to control variables (Vp-type)
   !---------------------------------------------------------------------------

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
                                               div    ! Divergence.

   real, dimension(ims:ime,jms:jme)         :: one_over_m2    !   Multiplicative coeff.

   integer :: i, j, k                             ! Loop counters.

   !----------------------------------------------------------------
   ! [1.0] Perform transform v = U^{-1} x
   !----------------------------------------------------------------      

   call da_zero_vp_type ( vp )

   ! [2.2] Transform u, v to streamfunction via vorticity:

   ! Communicate halo region.
   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

#ifndef DEREF_KLUDGE
   call da_uv_to_vorticity( xb, xa % u, xa % v, vor, &
#else
   call da_uv_to_vorticity( xb, xa % u(ims,jms,kms), &
                            xa % v(ims,jms,kms), &
                            vor(ims,jms,kms), &
#endif
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

   ! Convert vorticity to Del**2 psi:
   if(.not. global ) then               
      if (fg_format == 1 .or. fg_format == 2) then
         one_over_m2(its:ite,jts:jte) = 1.0 / ( xb % map_factor(its:ite,jts:jte) * &
                                        xb % map_factor(its:ite,jts:jte) )
         DO k = kts, kte
            vor(its:ite,jts:jte,k) = &
              one_over_m2(its:ite,jts:jte)*vor(its:ite,jts:jte,k)
         END DO
      else
         WRITE(UNIT=errmsg(1),FMT='(A,I5,A,L10)') &
            ' Wrong choice of fg_format= ',fg_format,' with global = ',global
         call da_error(__FILE__,__LINE__,errmsg(1))
      end if
   end if

   ! Calculate psi:
   print*,' calling Solve_PoissonEquation for Psi'
   call da_solve_poissoneqn_fct(xbx, vor, vp%v1, xp)

   ! [2.3] Transform u, v to velocity potential via divergence:

   call wrf_message('calling UV_To_Divergence')
#ifndef DEREF_KLUDGE
   call da_uv_to_divergence(xb, xa % u, xa % v, div, &
#else
   call da_uv_to_divergence(xb, xa % u(ims,jms,kms), &
                                xa % v(ims,jms,kms), &
                            div(ims,jms,kms), &
#endif
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

   ! Convert divergence to Del**2 chi:
   if(.not. global )  then              
      if (fg_format == 1 .or. fg_format == 2) then
         DO k = kts, kte
            div(its:ite,jts:jte,k) = &
               one_over_m2(its:ite,jts:jte) * div(its:ite,jts:jte,k)
         END DO
      else
         WRITE(UNIT=errmsg(1),FMT='(A,I5,A,L10)') &
            ' Wrong choice of fg_format= ',fg_format,' with global = ',global
         call da_error(__FILE__,__LINE__,errmsg(1))
      end if
   end if

   ! Calculate chi:

   call wrf_message(' calling Solve_PoissonEquation for Chi')
   call da_solve_poissoneqn_fct(xbx, div, vp%v2, xp)

   ! [2.4] Transform chi to chi_u:
   call wrf_message(' calculating chi_u')
   do k=kts,kte
      do j=jts,jte
         vp%v2(its:ite,j,k) = vp%v2(its:ite,j,k) - &
            be%reg_chi(j,k)*vp%v1(its:ite,j,k)
      enddo
   enddo

   call wrf_message(' calculating t_u')
   ! [2.4] Compute t_u:
   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            vp%v3(i,j,k) = xa%t(i,j,k) - &
               sum(be%reg_t(j,k,kts:kte)*vp%v1(i,j,kts:kte))
         enddo
      enddo
   enddo

   ! [2.6] Choice of moisture control variable: 
   
   call wrf_message(' calculating psudo rh')
   vp % v4(its:ite,jts:jte,kts:kte) = xa % q  (its:ite,jts:jte,kts:kte) /   &
                                      xb % qs (its:ite,jts:jte,kts:kte) 

   ! [2.7] Choice of surface pressure control variable: 
      
   ! [2.7] compute psfc_u:
   call wrf_message(' calculating psfc_u ')
   do j=jts,jte
      do i=its,ite
          vp % v5(i,j,1) = xa%psfc(i,j) - &
             sum(be%reg_ps(j,kts:kte)*vp%v1(i,j,kts:kte))
      end do
   end do

END subroutine da_transform_xtovp
