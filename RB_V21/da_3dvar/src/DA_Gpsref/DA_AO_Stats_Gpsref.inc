SUBROUTINE DA_AO_Stats_GPSRef( stats_unit, oi, re )

! ----------------------------------------------------------------------
!  Called by DA_Minimisation/da_write_diagnostics.inc.
!
!                                           Y.-R. Guo  06/02/2004
! ----------------------------------------------------------------------
   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! A - O

   TYPE (stats_gpsref_type)         :: GPSRef
   INTEGER                          :: ngpsref
   INTEGER                          :: n, k
   real                             :: o_minus_b, o_minus_a, sigma_o, sigma_b
   real                             :: o_minus_b_0, o_minus_a_0, sigma_o_0, sigma_b_0
   integer                        :: ierror        ! MPI error code.

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   ngpsref = 0
   num_gpsref_tot = 0
   o_minus_b_0 = 0.0; o_minus_a_0 = 0.0; sigma_o_0 = 0.0; sigma_b_0 = 0.0
   
   GPSRef%maximum%ref = maxmin_type( missing_r, 0, 0)
   GPSRef%minimum%ref = maxmin_type(-missing_r, 0, 0)
   GPSRef%average = residual_gpsref1_type(0.0,0.0,0.0,0.0)
   GPSRef%rms_err = GPSRef%average

   DO n=1, oi%num_GPSRef

      if( oi%gpsref(n)%loc%proc_domain ) then

         do k=1, oi%gpsref(n)%info%levels
            CALL Stats_Calculate( n, k, oi%GPSRef(n)%ref(k)%qc, & 
                          re%gpsref(n)%ref(k), ngpsref, &
                          GPSRef%minimum%ref, GPSRef%maximum%ref, &
                          GPSRef%average%ref, GPSRef%rms_err%ref )

           if ( pseudo_var(1:3) == 'ref' .and. num_pseudo > 0) then
                o_minus_b = oi%GPSRef(n)%ref(k)%inv
                o_minus_a = re%gpsref(n)%ref(k)
                sigma_o   = oi%gpsref(n)%ref(k)%error
           endif

         end do

           if ( pseudo_var(1:3) == 'ref' .and. num_pseudo > 0) then

    !          Calculate equivalent sigma_b using
    ! 
    !          O-A=(O-B)*sigma_o/(sigma_o+sigma_b)

             sigma_b = SQRT( (o_minus_b - o_minus_a) &
                     / o_minus_a ) * sigma_o
#ifdef DM_PARALLEL 
#if 0
   call MPI_ALLREDUCE( o_minus_b, o_minus_b_0, 1, TRUE_MPI_REAL, MPI_SUM, & 
                       MPI_COMM_WORLD, IERROR ) 
   call MPI_ALLREDUCE( o_minus_a, o_minus_a_0, 1, TRUE_MPI_REAL, MPI_SUM, & 
                       MPI_COMM_WORLD, IERROR ) 
   call MPI_ALLREDUCE( sigma_o, sigma_o_0, 1, TRUE_MPI_REAL, MPI_SUM, & 
                       MPI_COMM_WORLD, IERROR ) 
   call MPI_ALLREDUCE( sigma_b, sigma_b_0, 1, TRUE_MPI_REAL, MPI_SUM, & 
                       MPI_COMM_WORLD, IERROR ) 
   IF ( wrf_dm_on_monitor() ) THEN 
             write(stats_unit,'(/A,A3,A,f12.3)')  &
                         ' Pseudo ', pseudo_var, ' O-B: ', o_minus_b_0
             write(stats_unit,'( A,A3,A,f12.3)')  &
                         ' Pseudo ', pseudo_var, ' O-A: ', o_minus_a_0
             write(stats_unit,'( A,A3,A,f12.3)')  &
                         ' Pseudo ', pseudo_var, ' sigma_o: ', sigma_o_0
             write(stats_unit,'(A,A3,A,f12.3)')  &
                         ' Pseudo ', pseudo_var, ' sigma_b: ', sigma_b_0
   ENDIF 
#endif
#else 
   o_minus_b_0 = o_minus_b
   o_minus_a_0 = o_minus_a
   sigma_o_0   = sigma_o
   sigma_b_0   = sigma_b
             write(stats_unit,'(/A,A3,A,f12.3)')  & 
                         ' Pseudo ', pseudo_var, ' O-B: ', o_minus_b_0 
             write(stats_unit,'( A,A3,A,f12.3)')  & 
                         ' Pseudo ', pseudo_var, ' O-A: ', o_minus_a_0 
             write(stats_unit,'( A,A3,A,f12.3)')  & 
                         ' Pseudo ', pseudo_var, ' sigma_o: ', sigma_o_0 
             write(stats_unit,'(A,A3,A,f12.3)')  & 
                         ' Pseudo ', pseudo_var, ' sigma_b: ', sigma_b_0
#endif 

           endif

      endif    ! end if( oi%gpsref(n)%loc%proc_domain )
   END DO

!--Do inter-processor communication to gather statistics.

   call proc_sum_int( ngpsref )
   num_gpsref_tot = ngpsref
    
   call proc_stats_combine(GPSRef%average%ref, GPSRef%rms_err%ref, &
             GPSRef%minimum%ref%value, GPSRef%maximum%ref%value, &
             GPSRef%minimum%ref%n, GPSRef%maximum%ref%n, &
             GPSRef%minimum%ref%l, GPSRef%maximum%ref%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
      if(ngpsref > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for GPSRef'
   
         call DA_Print_Stats_GPSRef(stats_unit, ngpsref, GPSRef)
      endif
   END IF

END SUBROUTINE DA_AO_Stats_GPSRef

