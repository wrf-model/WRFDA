SUBROUTINE DA_Check_XToY_Adjoint( xb, xa, iv, xp, y, &
                                 ids,ide, jds,jde, kds,kde, &
                                 ims,ime, jms,jme, kms,kme, &
                                 its,ite, jts,jte, kts,kte )
   
!---- -------------------------------------------------------------------------
!  PURPOSE: Test observation operator transform and adjivnt for compatibility.
!
!  METHOD:  Standard adjoint test: < y, y > = < x, x_adj >.
!
!  HISTORY: 02/23/2000 - Creation of F90 version.           Dale Barker
!           03/04/2005 - Added AMV's from Geostationary     Syed RH Rizvi
!                        and Polar orbiting satellite
!                        as profile data
!------------------------------------------------------------------------------
   
   implicit none
   
   type (xb_type),    intent(in)    :: xb    ! first guess (local).
   type (x_type),     intent(inout) :: xa    ! analysis increments (local).
   type (ob_type),    intent(in)    :: iv    ! ob. increment vector.
   type (xpose_type), intent(in)    :: xp    ! Dimensions and xpose buffers(be).
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

   INTEGER                        :: i, j, k, n
   
!-------------------------------------------------------------------------

  real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_u, xa2_v, xa2_t, &
                                                xa2_p, xa2_q, xa2_rh
  real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_w
  real, dimension(ims:ime, jms:jme)          :: xa2_psfc
  real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_qcw, xa2_qrn

!-------------------------------------------------------------------------

   write(unit=*, fmt='(/a)') ' DA_Check_XToY_Adjoint: Adjoint Test Results:'

!-------------------------------------------------------------------------
!     [1.0] Initialise:
!-------------------------------------------------------------------------

!--Exchange XA (u, v, t, p, q, psfc) halo region.
   CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id4)

!  xa%u(ims:ime, jms:jme, kms:kme) = 0.0
!  xa%v(ims:ime, jms:jme, kms:kme) = 0.0
!  xa%w(ims:ime, jms:jme, kms:kme) = 0.0
!  xa%t(ims:ime, jms:jme, kms:kme) = 0.0
!  xa%p(ims:ime, jms:jme, kms:kme) = 0.0
!  xa%q(ims:ime, jms:jme, kms:kme) = 0.0
!  xa%rh(ims:ime, jms:jme, kms:kme) = 0.0
!  xa%psfc(ims:ime, jms:jme) = 0.0

   xa2_u(ims:ime, jms:jme, kms:kme) = xa%u(ims:ime, jms:jme, kms:kme)
   xa2_v(ims:ime, jms:jme, kms:kme) = xa%v(ims:ime, jms:jme, kms:kme)
   xa2_t(ims:ime, jms:jme, kms:kme) = xa%t(ims:ime, jms:jme, kms:kme)
   xa2_p(ims:ime, jms:jme, kms:kme) = xa%p(ims:ime, jms:jme, kms:kme)
   xa2_q(ims:ime, jms:jme, kms:kme) = xa%q(ims:ime, jms:jme, kms:kme)
   xa2_w(ims:ime, jms:jme, kms:kme) = xa%w(ims:ime, jms:jme, kms:kme)
   xa2_rh(ims:ime, jms:jme, kms:kme)= xa%rh(ims:ime, jms:jme, kms:kme)
   xa2_psfc(ims:ime, jms:jme)       = xa%psfc(ims:ime, jms:jme)

   xa2_qcw(ims:ime, jms:jme, kms:kme) = xa%qcw(ims:ime, jms:jme, kms:kme)
   xa2_qrn(ims:ime, jms:jme, kms:kme) = xa%qrn(ims:ime, jms:jme, kms:kme)

!-------------------------------------------------------------------------

   call DA_PT_To_Rho_Lin( xb, xa, xp )

!-------------------------------------------------------------------------

   if (sfc_assi_options == 2) then
      call DA_Transform_XToWTQ ( xp, xb, xa )
!-----Exchange XA (surface variable: u10, v10, t2, q2) halo region.
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id6)
   endif

   if ( use_ssmt1obs .or. use_ssmt2obs .or. Use_GpspwObs .or. &
        use_GpsrefObs .or.                                    &
        Use_SsmiTbObs .or. Use_SsmiRetrievalObs ) then

!-----Now do something for PW
      call DA_Transform_XToTPW( xa, xb )

!--------GPS Refractivity:
      if ( use_gpsrefobs ) &
        call da_transform_XTogpsref_lin(xa, xb, xp)

      if ( use_ssmt1obs .or. use_ssmt2obs .or. &
           Use_SsmiTbObs .or. Use_SsmiRetrievalObs ) then
         if( cv_options == 4) then
         print*,' This will not work as xb%speed is not available' 
         print*,' Please see the comments in da_transfer_kmatoxb.inc'
         stop
         end if
         call DA_Transform_XToSeaSfcWind_Lin( xa, xb )
      endif

      if ( Use_SsmiTbObs) call DA_Transform_XToTb_Lin (xa, xb)

!-----Exchange XA halo region.
      CALL wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id8)
   endif

!-------------------------------------------------------------------------
!  [2.0] Perform y = Hx transform:
!-------------------------------------------------------------------------
   
   call DA_Transform_XToY( xb, iv, xa, xp, y )

!-------------------------------------------------------------------------
!  [3.0] Calculate LHS of adjoint test equation and
!        Rescale input to adjoint routine :
!-------------------------------------------------------------------------

   partial_lhs = 0.0
   pertile_lhs = 0.0

   if( Use_SoundObs ) then
       call da_check_xtoy_adjoint_Sound( iv, y, partial_lhs, pertile_lhs )
       call da_check_xtoy_adjoint_Sonde_sfc( iv, y, partial_lhs, pertile_lhs )
   endif
   if( Use_SynopObs ) &
   call da_check_xtoy_adjoint_Synop( iv, y, partial_lhs, pertile_lhs )

   if( Use_GeoAMVObs ) &
   call da_check_xtoy_adjoint_GeoAMV( iv, y, partial_lhs, pertile_lhs )

   if( Use_PolarAMVObs ) &
   call da_check_xtoy_adjoint_PolarAMV( iv, y, partial_lhs, pertile_lhs )

   if( Use_AirepObs ) &
   call da_check_xtoy_adjoint_Airep( iv, y, partial_lhs, pertile_lhs )

   if( Use_PilotObs ) &
   call da_check_xtoy_adjoint_Pilot( iv, y, partial_lhs, pertile_lhs )

   if( Use_RadarObs ) &
   call da_check_xtoy_adjoint_Radar( iv, y, partial_lhs, pertile_lhs )

   if( Use_SatemObs ) &
   call da_check_xtoy_adjoint_Satem( iv, y, partial_lhs, pertile_lhs )

   if( Use_MetarObs ) &
   call da_check_xtoy_adjoint_Metar( iv, y, partial_lhs, pertile_lhs )
 
   if( Use_ShipsObs ) &
   call da_check_xtoy_adjoint_Ships( iv, y, partial_lhs, pertile_lhs )

   if( Use_GpspwObs ) &
   call da_check_xtoy_adjoint_Gpspw( iv, y, partial_lhs, pertile_lhs )

   if ( use_GpsrefObs ) &
   call da_check_xtoy_adjoint_Gpsref( iv, y, partial_lhs, pertile_lhs )
   
   if( Use_SsmiTbObs .or. Use_SsmiRetrievalObs ) &
   call da_check_xtoy_adjoint_SSMI ( iv, y, partial_lhs, pertile_lhs )

   if( use_ssmt1obs ) &
   call da_check_xtoy_adjoint_ssmt1( iv, y, partial_lhs, pertile_lhs )

   if( use_ssmt2obs ) &
   call da_check_xtoy_adjoint_ssmt2( iv, y, partial_lhs, pertile_lhs )

   if( use_qscatobs ) &
   call da_check_xtoy_adjoint_qscat( iv, y, partial_lhs, pertile_lhs )

   if( Use_ProfilerObs ) &
   call da_check_xtoy_adjoint_Profiler( iv, y, partial_lhs, pertile_lhs )

   if( Use_BuoyObs ) &
   call da_check_xtoy_adjoint_Buoy( iv, y, partial_lhs, pertile_lhs )

   if( Use_BogusObs ) &
   call da_check_xtoy_adjoint_Bogus( iv, y, partial_lhs, pertile_lhs )

!-------------------------------------------------------------------------
!     [5.0] Perform adjoint operation:
!-------------------------------------------------------------------------

   call DA_Zero_X( xa )
   
   call DA_Transform_XToY_Adj( xb, iv, xp, y, xa )

!-------------------------------------------------------------------------

   if ( use_ssmt1obs .or. use_ssmt2obs .or. Use_GpspwObs .or. &
        use_GpsrefObs .or.                                    &
        Use_SsmiTbObs .or. Use_SsmiRetrievalObs ) then

      if ( Use_SsmiTbObs) call DA_Transform_XToTb_Adj (xa, xb)

!-----for PW
      call DA_Transform_XToTPW_Adj( xa, xb )

!-----GPS Refractivity:
      if ( use_GpsrefObs ) &
      call da_transform_XTogpsref_adj(xa, xb, xp)

      if ( use_ssmt1obs .or. use_ssmt2obs .or. &
           Use_SsmiTbObs .or. Use_SsmiRetrievalObs ) then
         if( cv_options == 4) then
         print*,' This will not work as xb%speed is not available' 
         print*,' Please see the comments in da_transfer_kmatoxb.inc'
         stop
         end if
         call DA_Transform_XToSeaSfcWind_Adj( xa, xb )
      endif
   endif

!--Now do something for surface variables
   if (sfc_assi_options == 2) then
      call DA_Transform_XToWTQ_adj ( xp, xb, xa )
   endif

!-------------------------------------------------------------------------

   call DA_PT_To_Rho_Adj( xb, xa )

!-------------------------------------------------------------------------

   pertile_rhs = sum( xa%u(ims:ime, jms:jme, kms:kme) * xa2_u(ims:ime, jms:jme, kms:kme) ) &
               + sum( xa%v(ims:ime, jms:jme, kms:kme) * xa2_v(ims:ime, jms:jme, kms:kme) ) &
               + sum( xa%w(ims:ime, jms:jme, kms:kme) * xa2_w(ims:ime, jms:jme, kms:kme) ) &
               + sum( xa%t(ims:ime, jms:jme, kms:kme) * xa2_t(ims:ime, jms:jme, kms:kme) ) &
               + sum( xa%p(ims:ime, jms:jme, kms:kme) * xa2_p(ims:ime, jms:jme, kms:kme) ) &
               + sum( xa%q(ims:ime, jms:jme, kms:kme) * xa2_q(ims:ime, jms:jme, kms:kme) ) &
               + sum( xa%rh(ims:ime, jms:jme, kms:kme)* xa2_rh(ims:ime, jms:jme, kms:kme) ) &
               + sum( xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme) )
   pertile_rhs = pertile_rhs &
               + sum( xa%qcw(ims:ime, jms:jme, kms:kme) * xa2_qcw(ims:ime, jms:jme, kms:kme) ) &
               + sum( xa%qrn(ims:ime, jms:jme, kms:kme) * xa2_qrn(ims:ime, jms:jme, kms:kme) )

!-------------------------------------------------------------------------
!     [6.0] Calculate RHS of adjoint test equation:
!-------------------------------------------------------------------------
   
   partial_rhs = sum( xa%u(its:ite, jts:jte, kts:kte) * xa2_u(its:ite, jts:jte, kts:kte) ) &
               + sum( xa%v(its:ite, jts:jte, kts:kte) * xa2_v(its:ite, jts:jte, kts:kte) ) &
               + sum( xa%w(its:ite, jts:jte, kts:kte+1) * xa2_w(its:ite, jts:jte, kts:kte+1) ) &
               + sum( xa%t(its:ite, jts:jte, kts:kte) * xa2_t(its:ite, jts:jte, kts:kte) ) &
               + sum( xa%p(its:ite, jts:jte, kts:kte) * xa2_p(its:ite, jts:jte, kts:kte) ) &
               + sum( xa%q(its:ite, jts:jte, kts:kte) * xa2_q(its:ite, jts:jte, kts:kte) ) &
               + sum( xa%rh(its:ite, jts:jte, kts:kte)* xa2_rh(its:ite, jts:jte, kts:kte) ) &
               + sum( xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte) ) 

   partial_rhs = partial_rhs &
               + sum( xa%qcw(its:ite, jts:jte, kts:kte) * xa2_qcw(its:ite, jts:jte, kts:kte) ) &
               + sum( xa%qrn(its:ite, jts:jte, kts:kte) * xa2_qrn(its:ite, jts:jte, kts:kte) )

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
   
END SUBROUTINE DA_Check_XToY_Adjoint
