SUBROUTINE check_sfc_assi( xb, xa, iv, xp, y, &
                           ids,ide, jds,jde, kds,kde, &
                           ims,ime, jms,jme, kms,kme, &
                           its,ite, jts,jte, kts,kte )
   
   implicit none
  
   type (xb_type),    intent(in)    :: xb    ! first guess (local).
   type (x_type),     intent(inout) :: xa    ! analysis increments (local).
   type (ob_type),    intent(inout) :: iv    ! ob. increment vector.
   type (xpose_type), intent(inout) :: xp    ! Dimensions and xpose buffers(be).
   TYPE (y_type),     intent(inout) :: y     ! y = h (xa)

   integer, intent(in)              :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)              :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)              :: its,ite, jts,jte, kts,kte ! tile   dims.

!-------------------------------------------------------------------------

   integer                        :: ierror        ! MPI error code.

!-------------------------------------------------------------------------

   REAL                           :: adj_ttl_lhs   ! < y, y >
   REAL                           :: adj_ttl_rhs   ! < x, x_adj >

   REAL                           :: partial_lhs   ! < y, y >
   REAL                           :: partial_rhs   ! < x, x_adj >

   REAL                           :: pertile_lhs   ! < y, y >
   REAL                           :: pertile_rhs   ! < x, x_adj >

!-------------------------------------------------------------------------

   INTEGER                        :: kz, n

!-------------------------------------------------------------------------

   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_u, xa2_v, xa2_t, &
                                                 xa2_p, xa2_q
 
   real, dimension(ims:ime, jms:jme)          :: xa2_u10, xa2_v10, xa2_t2, &
                                                 xa2_q2, xa2_tgrn, xa2_psfc

!-------------------------------------------------------------------------
   
   write(unit=*, fmt='(/a)') ' check_sfc_assi: Adjoint Test Results:'
   
!-------------------------------------------------------------------------
   
   xa2_u(ims:ime, jms:jme, kms:kme) = xa%u(ims:ime, jms:jme, kms:kme)
   xa2_v(ims:ime, jms:jme, kms:kme) = xa%v(ims:ime, jms:jme, kms:kme)
   xa2_t(ims:ime, jms:jme, kms:kme) = xa%t(ims:ime, jms:jme, kms:kme)
   xa2_p(ims:ime, jms:jme, kms:kme) = xa%p(ims:ime, jms:jme, kms:kme)
   xa2_q(ims:ime, jms:jme, kms:kme) = xa%q(ims:ime, jms:jme, kms:kme)

   xa2_psfc(ims:ime, jms:jme) = xa%psfc(ims:ime, jms:jme)
   xa2_tgrn(ims:ime, jms:jme) = xa%tgrn(ims:ime, jms:jme)
   xa2_u10 (ims:ime, jms:jme) = xa%u10 (ims:ime, jms:jme)
   xa2_v10 (ims:ime, jms:jme) = xa%v10 (ims:ime, jms:jme)
   xa2_t2  (ims:ime, jms:jme) = xa%t2  (ims:ime, jms:jme)
   xa2_q2  (ims:ime, jms:jme) = xa%q2  (ims:ime, jms:jme)

!-------------------------------------------------------------------------

   kz = xb%mkz

!-------------------------------------------------------------------------

!     call check_psfc( xb, xa, iv, xp, y, &
!                      ids,ide, jds,jde, kds,kde, &
!                      ims,ime, jms,jme, kms,kme, &
!                      its,ite, jts,jte, kts,kte )

!     CALL wrf_shutdown
!     stop 'check_psfc'

!-------------------------------------------------------------------------

   call DA_Transform_XToWTQ ( xp, xb, xa )

!--Exchange XA(SURFACE) halo region.
   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id6)

!-------------------------------------------------------------------------

   partial_lhs = 0.0
   pertile_lhs = 0.0

   do n=1, iv%num_synop
      if(iv%synop(n)%loc%proc_domain_with_halo) then
         CALL DA_Transform_XToPsfc( xb, xa, xp, iv%synop(n), y%synop(n) )


         pertile_lhs = pertile_lhs &
                     + y%synop(n)%u * y%synop(n)%u &
                     + y%synop(n)%v * y%synop(n)%v &
                     + y%synop(n)%t * y%synop(n)%t &
                     + y%synop(n)%p * y%synop(n)%p &
                     + y%synop(n)%q * y%synop(n)%q

         if(iv%synop(n)%loc%proc_domain) then
            partial_lhs = partial_lhs &
                        + y%synop(n)%u * y%synop(n)%u &
                        + y%synop(n)%v * y%synop(n)%v &
                        + y%synop(n)%t * y%synop(n)%t &
                        + y%synop(n)%p * y%synop(n)%p &
                        + y%synop(n)%q * y%synop(n)%q
         endif
      endif
   enddo

!-------------------------------------------------------------------------
!     [5.0] Perform adjoint operation:
!-------------------------------------------------------------------------

   call DA_Zero_X( xa )

   DO n=1, iv%num_synop
      if(iv%synop(n)%loc%proc_domain_with_halo) then
         CALL DA_Transform_XToPsfc_Adj(xb,xp,iv%synop(n),y%synop(n),xa)
      endif
   ENDDO

   call DA_Transform_XToWTQ_adj ( xp, xb, xa )
   
   pertile_rhs = sum( xa%u(ims:ime, jms:jme, kms:kme) * xa2_u(ims:ime, jms:jme, kms:kme) ) + &
                 sum( xa%v(ims:ime, jms:jme, kms:kme) * xa2_v(ims:ime, jms:jme, kms:kme) ) + &
                 sum( xa%t(ims:ime, jms:jme, kms:kme) * xa2_t(ims:ime, jms:jme, kms:kme) ) + &
                 sum( xa%p(ims:ime, jms:jme, kms:kme) * xa2_p(ims:ime, jms:jme, kms:kme) ) + &
                 sum( xa%q(ims:ime, jms:jme, kms:kme) * xa2_q(ims:ime, jms:jme, kms:kme) ) + &
                 sum( xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme) )

!---- --------------------------------------------------------------------
!     [6.0] Calculate RHS of adjivnt test equation:
!-------------------------------------------------------------------------
   
   partial_rhs = sum( xa%u(its:ite, jts:jte, kts:kte) * xa2_u(its:ite, jts:jte, kts:kte) ) + &
                 sum( xa%v(its:ite, jts:jte, kts:kte) * xa2_v(its:ite, jts:jte, kts:kte) ) + &
                 sum( xa%t(its:ite, jts:jte, kts:kte) * xa2_t(its:ite, jts:jte, kts:kte) ) + &
                 sum( xa%p(its:ite, jts:jte, kts:kte) * xa2_p(its:ite, jts:jte, kts:kte) ) + &
                 sum( xa%q(its:ite, jts:jte, kts:kte) * xa2_q(its:ite, jts:jte, kts:kte) ) + &
                 sum( xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte) )
   
!---- --------------------------------------------------------------------
!     [7.0] Print output:
!-------------------------------------------------------------------------
   
   write(unit=*, fmt='(A,1pe22.14)') &
        ' Tile < y, y     > = ', pertile_lhs, &
        ' Tile < x, x_adj > = ', pertile_rhs

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( partial_lhs, adj_ttl_lhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )

   call MPI_ALLREDUCE( partial_rhs, adj_ttl_rhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )
   
   IF ( wrf_dm_on_monitor() ) THEN
      write(unit=*, fmt='(A,1pe22.14)') ' Whole Domain < y, y     > = ', adj_ttl_lhs
      write(unit=*, fmt='(A,1pe22.14)') ' Whole Domain < x, x_adj > = ', adj_ttl_rhs
   ENDIF
#else
   adj_ttl_lhs = partial_lhs
   adj_ttl_rhs = partial_rhs
   write(unit=*, fmt='(A,1pe22.14)') ' Whole Domain < y, y     > = ', adj_ttl_lhs
   write(unit=*, fmt='(A,1pe22.14)') ' Whole Domain < x, x_adj > = ', adj_ttl_rhs
#endif

!-------------------------------------------------------------------------
!--recover xa
   xa%u(ims:ime, jms:jme, kms:kme) = xa2_u(ims:ime, jms:jme, kms:kme)
   xa%v(ims:ime, jms:jme, kms:kme) = xa2_v(ims:ime, jms:jme, kms:kme)
   xa%t(ims:ime, jms:jme, kms:kme) = xa2_t(ims:ime, jms:jme, kms:kme)
   xa%p(ims:ime, jms:jme, kms:kme) = xa2_p(ims:ime, jms:jme, kms:kme)
   xa%q(ims:ime, jms:jme, kms:kme) = xa2_q(ims:ime, jms:jme, kms:kme)

   xa%psfc(ims:ime, jms:jme) = xa2_psfc(ims:ime, jms:jme)
   xa%tgrn(ims:ime, jms:jme) = xa2_tgrn(ims:ime, jms:jme)
   xa%u10 (ims:ime, jms:jme) = xa2_u10 (ims:ime, jms:jme)
   xa%v10 (ims:ime, jms:jme) = xa2_v10 (ims:ime, jms:jme)
   xa%t2  (ims:ime, jms:jme) = xa2_t2  (ims:ime, jms:jme)
   xa%q2  (ims:ime, jms:jme) = xa2_q2  (ims:ime, jms:jme)

   CALL wrf_shutdown
   stop 'In file: __FILE__'
   
END SUBROUTINE check_sfc_assi

SUBROUTINE check_psfc( xb, xa, iv, xp, y, &
                       ids,ide, jds,jde, kds,kde, &
                       ims,ime, jms,jme, kms,kme, &
                       its,ite, jts,jte, kts,kte )
   
!------------------------------------------------------------------------------
   
   implicit none
   
   type (xb_type), intent(in)        :: xb    ! first guess (local).
   type (x_type),  intent(inout)     :: xa    ! analysis increments (local).
   type (ob_type), intent(inout)     :: iv    ! ob. increment vector.
   type (xpose_type), intent(inout)  :: xp    ! Dimensions and xpose buffers(be).
   type (y_type),  intent(inout)     :: y     ! residual

   integer, intent(in)               :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)               :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)               :: its,ite, jts,jte, kts,kte ! tile   dims.

!-------------------------------------------------------------------------

   integer                        :: ierror        ! MPI error code.

!-------------------------------------------------------------------------

   REAL                           :: adj_ttl_lhs   ! < y, y >
   REAL                           :: adj_ttl_rhs   ! < x, x_adj >

   REAL                           :: partial_lhs   ! < y, y >
   REAL                           :: partial_rhs   ! < x, x_adj >

   REAL                           :: pertile_lhs   ! < y, y >
   REAL                           :: pertile_rhs   ! < x, x_adj >

!-------------------------------------------------------------------------

   INTEGER                        :: n

!-------------------------------------------------------------------------
 
   real, dimension(ims:ime, jms:jme) :: xa2_u10, xa2_v10, xa2_t2, &
                                        xa2_q2, xa2_tgrn, xa2_psfc

!-------------------------------------------------------------------------
   
   write(unit=*, fmt='(/3a,i6/a)') &
        'File: ', __FILE__, ', line:', __LINE__, &
        'Adjoint Test Results:'

!-------------------------------------------------------------------------
!--save input

   xa2_psfc(ims:ime, jms:jme) = xa%p   (ims:ime, jms:jme, kts)
   xa2_tgrn(ims:ime, jms:jme) = xa%tgrn(ims:ime, jms:jme)
   xa2_u10 (ims:ime, jms:jme) = xa%u10 (ims:ime, jms:jme)
   xa2_v10 (ims:ime, jms:jme) = xa%v10 (ims:ime, jms:jme)
   xa2_t2  (ims:ime, jms:jme) = xa%t2  (ims:ime, jms:jme)
   xa2_q2  (ims:ime, jms:jme) = xa%q2  (ims:ime, jms:jme)

!-------------------------------------------------------------------------

   partial_lhs = 0.0
   pertile_lhs = 0.0

   do n=1, iv%num_synop
      if(iv%synop(n)%loc%proc_domain_with_halo) then
         CALL DA_Transform_XToPsfc( xb, xa, xp, iv%synop(n), y%synop(n) )
         pertile_lhs = pertile_lhs &
                     + y%synop(n)%u * y%synop(n)%u &
                     + y%synop(n)%v * y%synop(n)%v &
                     + y%synop(n)%t * y%synop(n)%t &
                     + y%synop(n)%p * y%synop(n)%p &
                     + y%synop(n)%q * y%synop(n)%q

         if(iv%synop(n)%loc%proc_domain) then
            partial_lhs = partial_lhs & 
                        + y%synop(n)%u * y%synop(n)%u &
                        + y%synop(n)%v * y%synop(n)%v &
                        + y%synop(n)%t * y%synop(n)%t &
                        + y%synop(n)%p * y%synop(n)%p &
                        + y%synop(n)%q * y%synop(n)%q
         endif
      endif
   enddo

!-------------------------------------------------------------------------
!     [5.0] Perform adjivnt operation:
!-------------------------------------------------------------------------

   xa%psfc(ims:ime, jms:jme) = 0.0
   xa%tgrn(ims:ime, jms:jme) = 0.0
   xa%u10 (ims:ime, jms:jme) = 0.0
   xa%v10 (ims:ime, jms:jme) = 0.0
   xa%t2  (ims:ime, jms:jme) = 0.0
   xa%q2  (ims:ime, jms:jme) = 0.0
   
   DO n=1, iv%num_synop
      if(iv%synop(n)%loc%proc_domain_with_halo) then
         CALL DA_Transform_XToPsfc_Adj(xb,xp,iv%synop(n),y%synop(n),xa)
      endif
   ENDDO

   pertile_rhs = sum( xa%u10 (ims:ime, jms:jme) * xa2_u10 (ims:ime, jms:jme) ) &
               + sum( xa%v10 (ims:ime, jms:jme) * xa2_v10 (ims:ime, jms:jme) ) &
               + sum( xa%t2  (ims:ime, jms:jme) * xa2_t2  (ims:ime, jms:jme) ) &
               + sum( xa%q2  (ims:ime, jms:jme) * xa2_q2  (ims:ime, jms:jme) ) &
               + sum( xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme) )

   partial_rhs = sum( xa%u10 (its:ite, jts:jte) * xa2_u10 (its:ite, jts:jte) ) &
               + sum( xa%v10 (its:ite, jts:jte) * xa2_v10 (its:ite, jts:jte) ) &
               + sum( xa%t2  (its:ite, jts:jte) * xa2_t2  (its:ite, jts:jte) ) &
               + sum( xa%q2  (its:ite, jts:jte) * xa2_q2  (its:ite, jts:jte) ) &
               + sum( xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte) )
   
!---- --------------------------------------------------------------------
!     [6.0] Calculate RHS of adjivnt test equation:
!-------------------------------------------------------------------------

!---- --------------------------------------------------------------------
!     [7.0] Print output:
!-------------------------------------------------------------------------
   
   write(unit=*, fmt='(A,1pe22.14)') &
        ' Tile < y, y     > = ', pertile_lhs, &
        ' Tile < x, x_adj > = ', pertile_rhs

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( partial_lhs, adj_ttl_lhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )

   call MPI_ALLREDUCE( partial_rhs, adj_ttl_rhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )
   
   IF ( wrf_dm_on_monitor() ) THEN
      write(unit=*, fmt='(A,1pe22.14)') ' Whole Domain < y, y     > = ', adj_ttl_lhs
      write(unit=*, fmt='(A,1pe22.14)') ' Whole Domain < x, x_adj > = ', adj_ttl_rhs
   ENDIF
#else
   adj_ttl_lhs = partial_lhs
   adj_ttl_rhs = partial_rhs
   write(unit=*, fmt='(A,1pe22.14)') ' Whole Domain < y, y     > = ', adj_ttl_lhs
   write(unit=*, fmt='(A,1pe22.14)') ' Whole Domain < x, x_adj > = ', adj_ttl_rhs
#endif

!--recover
   xa%psfc(ims:ime, jms:jme) = xa2_psfc(ims:ime, jms:jme)
   xa%u10 (ims:ime, jms:jme) = xa2_u10 (ims:ime, jms:jme)
   xa%v10 (ims:ime, jms:jme) = xa2_v10 (ims:ime, jms:jme)
   xa%t2  (ims:ime, jms:jme) = xa2_t2  (ims:ime, jms:jme)
   xa%q2  (ims:ime, jms:jme) = xa2_q2  (ims:ime, jms:jme)

END SUBROUTINE check_psfc

