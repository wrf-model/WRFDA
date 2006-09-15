subroutine da_check_psfc( xb, xa, iv, xp, y, &
                       ids,ide, jds,jde, kds,kde, &
                       ims,ime, jms,jme, kms,kme, &
                       its,ite, jts,jte, kts,kte )
     
   implicit none
   
   type (xb_type), intent(in)        :: xb  ! first guess (local).
   type (x_type),  intent(inout)     :: xa  ! analysis increments (local).
   type (ob_type), intent(inout)     :: iv  ! ob. increment vector.
   type (xpose_type), intent(inout)  :: xp  ! Dimensions and xpose buffers(be).
   type (y_type),  intent(inout)     :: y   ! residual

   integer, intent(in)               :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)               :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)               :: its,ite, jts,jte, kts,kte ! tile   dims.

   REAL                           :: adj_ttl_lhs   ! < y, y >
   REAL                           :: adj_ttl_rhs   ! < x, x_adj >

   REAL                           :: partial_lhs   ! < y, y >
   REAL                           :: partial_rhs   ! < x, x_adj >

   REAL                           :: pertile_lhs   ! < y, y >
   REAL                           :: pertile_rhs   ! < x, x_adj >

   INTEGER                        :: n

   real, dimension(ims:ime, jms:jme) :: xa2_u10, xa2_v10, xa2_t2, &
                                        xa2_q2, xa2_psfc

   IF (trace_use) call da_trace_entry("da_check_psfc")

   ! JRB Stop compiler whinging
   IF (ids == 0 .OR. ide == 0 .OR. kme == 0 .OR. jde == 0 .OR. jds == 0 .OR. &
      kde == 0 .OR. kds == 0 .OR. kte == 0 .OR. kms == 0) THEN
   END IF
   
   write(UNIT=stdout, fmt='(/3a,i6/a)') &
        'File: ', __FILE__, ', line:', __LINE__, &
        'Adjoint Test Results:'

   !--save input

   xa2_psfc(ims:ime, jms:jme) = xa%p   (ims:ime, jms:jme, kts)
   xa2_u10 (ims:ime, jms:jme) = xa%u10 (ims:ime, jms:jme)
   xa2_v10 (ims:ime, jms:jme) = xa%v10 (ims:ime, jms:jme)
   xa2_t2  (ims:ime, jms:jme) = xa%t2  (ims:ime, jms:jme)
   xa2_q2  (ims:ime, jms:jme) = xa%q2  (ims:ime, jms:jme)

   !----------------------------------------------------------------------

   partial_lhs = 0.0
   pertile_lhs = 0.0

   do n=1, iv%num_synop
      if(iv%synop(n)%loc%proc_domain_with_halo) then
         call da_transform_xtopsfc( xb, xa, xp, iv%synop(n), y%synop(n) )
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
   ! [5.0] Perform adjivnt operation:
   !-------------------------------------------------------------------------

   xa%psfc(ims:ime, jms:jme) = 0.0
   xa%tgrn(ims:ime, jms:jme) = 0.0
   xa%u10 (ims:ime, jms:jme) = 0.0
   xa%v10 (ims:ime, jms:jme) = 0.0
   xa%t2  (ims:ime, jms:jme) = 0.0
   xa%q2  (ims:ime, jms:jme) = 0.0
   
   DO n=1, iv%num_synop
      if(iv%synop(n)%loc%proc_domain_with_halo) then
         call da_transform_xtopsfc_adj(xb,xp,iv%synop(n),y%synop(n),xa)
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
   
   !----------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjivnt test equation:
   !----------------------------------------------------------------------

   !----------------------------------------------------------------------
   ! [7.0] Print output:
   !----------------------------------------------------------------------
   
   write(UNIT=stdout, fmt='(A,1pe22.14)') &
      ' Tile < y, y     > = ', pertile_lhs, &
      ' Tile < x, x_adj > = ', pertile_rhs

   adj_ttl_lhs = wrf_dm_sum_real( partial_lhs )
   adj_ttl_rhs = wrf_dm_sum_real( partial_rhs )
   write (unit=stdout,fmt='(A,2F10.2)') &
      'TEST_COVERAGE_check_sfc_assi_B:  adj_ttl_lhs,adj_ttl_rhs = ', &
      adj_ttl_lhs,adj_ttl_rhs
   IF ( wrf_dm_on_monitor() ) THEN
      write(UNIT=stdout, fmt='(A,1pe22.14)') ' Whole Domain < y, y     > = ', &
         adj_ttl_lhs
      write(UNIT=stdout, fmt='(A,1pe22.14)') ' Whole Domain < x, x_adj > = ', &
         adj_ttl_rhs
   ENDIF

   ! recover
   xa%psfc(ims:ime, jms:jme) = xa2_psfc(ims:ime, jms:jme)
   xa%u10 (ims:ime, jms:jme) = xa2_u10 (ims:ime, jms:jme)
   xa%v10 (ims:ime, jms:jme) = xa2_v10 (ims:ime, jms:jme)
   xa%t2  (ims:ime, jms:jme) = xa2_t2  (ims:ime, jms:jme)
   xa%q2  (ims:ime, jms:jme) = xa2_q2  (ims:ime, jms:jme)

   IF (trace_use) call da_trace_exit("da_check_psfc")

END subroutine da_check_psfc


