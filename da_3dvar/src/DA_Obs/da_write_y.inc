SUBROUTINE da_write_y( iv, y )
!----------------------------------------------------------------------------   
!  History:
!
!  Additions:
!             07/08/2003      Profiler and Buoy Obs            S. R. H. Rizvi
!
!  Purpose: Writes out components of y=H(x_inc) structure.
!----------------------------------------------------------------------------   

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv   ! O-B structure.
   TYPE (y_type), INTENT(IN)     :: y    ! y = H(x_inc) structure.

   integer                       :: unit ! Output file unit.
   integer                       :: n, k
   real                          :: f1, f2, f3, f4, f5, f6, f7

   if ( omb_add_noise ) then
      unit = yp_unit            ! 3DVAR is a perturbed ob run.
   else
      unit = y_unit             ! 3DVAR is an unperturbed ob run.
   end if

!  [1] Transfer surface obs:

   IF ( iv % num_synop > 0 ) THEN
      write(unit,'(a,i8)')'synop', iv % num_synop
      DO n = 1, iv % num_synop      
         call da_check_missing(iv%synop(n)%u%qc, y%synop(n)%u, f1)
         call da_check_missing(iv%synop(n)%v%qc, y%synop(n)%v, f2)
         call da_check_missing(iv%synop(n)%t%qc, y%synop(n)%t, f3)
         call da_check_missing(iv%synop(n)%p%qc, y%synop(n)%p, f4)
         call da_check_missing(iv%synop(n)%q%qc, y%synop(n)%q, f5)

         write(unit,'(2i8,10e15.7)')n, 1, f1, f2, f3, f4, f5
      END DO
   ENDIF

!  [2] Transfer metar obs:

   IF ( iv % num_metar > 0 ) THEN
      write(unit,'(a,i8)')'metar', iv % num_metar
      DO n = 1, iv % num_metar
         call da_check_missing(iv%metar(n)%u%qc, y%metar(n)%u, f1)
         call da_check_missing(iv%metar(n)%v%qc, y%metar(n)%v, f2)
         call da_check_missing(iv%metar(n)%t%qc, y%metar(n)%t, f3)
         call da_check_missing(iv%metar(n)%p%qc, y%metar(n)%p, f4)
         call da_check_missing(iv%metar(n)%q%qc, y%metar(n)%q, f5)

         write(unit,'(2i8,10e15.7)')n, 1, f1, f2, f3, f4, f5
      END DO
   ENDIF

!  [3] Transfer ships obs:

   IF ( iv % num_ships > 0 ) THEN
      write(unit,'(a,i8)')'ships', iv % num_ships
      DO n = 1, iv % num_ships
         call da_check_missing(iv%ships(n)%u%qc, y%ships(n)%u, f1)
         call da_check_missing(iv%ships(n)%v%qc, y%ships(n)%v, f2)
         call da_check_missing(iv%ships(n)%t%qc, y%ships(n)%t, f3)
         call da_check_missing(iv%ships(n)%p%qc, y%ships(n)%p, f4)
         call da_check_missing(iv%ships(n)%q%qc, y%ships(n)%q, f5)

         write(unit,'(2i8,10e15.7)')n, 1, f1, f2, f3, f4, f5
      END DO
   ENDIF


!  [4.1] Transfer Geo. AMVs Obs:

   IF ( iv % num_geoamv > 0 ) THEN 
      write(unit,'(a,i8)')'geoamv', iv % num_geoamv
      DO n = 1, iv % num_geoamv
         write(unit,'(i8)')iv % geoamv(n) % info % levels
         DO k = 1, iv % geoamv(n) % info % levels
         call da_check_missing(iv%geoamv(n)%u(k)%qc, y%geoamv(n)%u(k), f1)
         call da_check_missing(iv%geoamv(n)%v(k)%qc, y%geoamv(n)%v(k), f2)

         write(unit,'(2i8,10e15.7)')n, k, f1, f2
      END DO
    END DO
   ENDIF

!  [4.2] Transfer Polar AMVs Obs:

   IF ( iv % num_polaramv > 0 ) THEN
      write(unit,'(a,i8)')'polaramv', iv % num_polaramv
      DO n = 1, iv % num_polaramv
         write(unit,'(i8)')iv % polaramv(n) % info % levels
         DO k = 1, iv % polaramv(n) % info % levels
         call da_check_missing(iv%polaramv(n)%u(k)%qc, y%polaramv(n)%u(k), f1)
         call da_check_missing(iv%polaramv(n)%v(k)%qc, y%polaramv(n)%v(k), f2)

         write(unit,'(2i8,10e15.7)')n, k, f1, f2
      END DO
    END DO
   ENDIF

!  [5] Transfer gpspw obs:

   IF ( iv % num_gpspw > 0 ) THEN
      write(unit,'(a,i8)')'gpspw', iv % num_gpspw
      DO n = 1, iv % num_gpspw
         call da_check_missing(iv%gpspw(n)%tpw%qc, y%gpspw(n)%tpw, f1)

         write(unit,'(2i8,10e15.7)')n, 1, f1
      END DO
   ENDIF

!  [6] Transfer sonde obs:

   IF ( iv % num_sound > 0 ) THEN
      write(unit,'(a,i8)')'sound', iv % num_sound
      DO n = 1, iv % num_sound
         write(unit,'(i8)')iv % sound(n) % info % levels
         DO k = 1, iv % sound(n) % info % levels
            call da_check_missing(iv%sound(n)%u(k)%qc, y%sound(n)%u(k), f1)
            call da_check_missing(iv%sound(n)%v(k)%qc, y%sound(n)%v(k), f2)
            call da_check_missing(iv%sound(n)%t(k)%qc, y%sound(n)%t(k), f3)
            call da_check_missing(iv%sound(n)%q(k)%qc, y%sound(n)%q(k), f5)

            write(unit,'(2i8,10e15.7)')n, k, f1, f2, f3, f4, f5
         END DO
      END DO

      write(unit,'(a,i8)')'sonde_sfc', iv % num_sound
      DO n = 1, iv % num_sound
         call da_check_missing(iv%sonde_sfc(n)%u%qc, y%sonde_sfc(n)%u, f1)
         call da_check_missing(iv%sonde_sfc(n)%v%qc, y%sonde_sfc(n)%v, f2)
         call da_check_missing(iv%sonde_sfc(n)%t%qc, y%sonde_sfc(n)%t, f3)
         call da_check_missing(iv%sonde_sfc(n)%p%qc, y%sonde_sfc(n)%p, f4)
         call da_check_missing(iv%sonde_sfc(n)%q%qc, y%sonde_sfc(n)%q, f5)

         write(unit,'(2i8,10e15.7)')n, 1, f1, f2, f3, f4, f5
      END DO
   ENDIF

!  [7] Transfer airep obs:

   IF ( iv % num_airep > 0 ) THEN
      write(unit,'(a,i8)')'airep', iv % num_airep
      DO n = 1, iv % num_airep
         write(unit,'(i8)')iv % airep(n) % info % levels
         DO k = 1, iv % airep(n) % info % levels
            call da_check_missing(iv%airep(n)%u(k)%qc, y%airep(n)%u(k), f1)
            call da_check_missing(iv%airep(n)%v(k)%qc, y%airep(n)%v(k), f2)
            call da_check_missing(iv%airep(n)%t(k)%qc, y%airep(n)%t(k), f3)

            write(unit,'(2i8,10e15.7)')n, k, f1, f2, f3
         END DO
      END DO
   ENDIF

!  [8] Transfer pilot obs:

   IF ( iv % num_pilot > 0 ) THEN
      write(unit,'(a,i8)')'pilot', iv % num_pilot
      DO n = 1, iv % num_pilot
         write(unit,'(i8)')iv % pilot(n) % info % levels
         DO k = 1, iv % pilot(n) % info % levels
            call da_check_missing(iv%pilot(n)%u(k)%qc, y%pilot(n)%u(k), f1)
            call da_check_missing(iv%pilot(n)%v(k)%qc, y%pilot(n)%v(k), f2)

            write(unit,'(2i8,10e15.7)')n, k, f1, f2
         END DO
      END DO
   ENDIF

!  [9] Transfer SSM/I obs:SSMI:

   IF ( iv % num_ssmi_retrieval > 0 ) THEN
      write(unit,'(a,i8)')'ssmir', iv % num_ssmi_retrieval
      DO n = 1, iv % num_ssmi_retrieval
         call da_check_missing( iv%ssmi_retrieval(n)%speed%qc, &
                                y % ssmi_retrieval(n) % speed, f1 )
         call da_check_missing( iv%ssmi_retrieval(n)% tpw % qc, &
                                y % ssmi_retrieval(n) % tpw, f2 )

         write(unit,'(2i8,10e15.7)')n, 1, f1, f2

      END DO
   ENDIF

   IF ( iv % num_ssmi_tb > 0 ) THEN
      write(unit,'(a,i8)')'ssmiT', iv % num_ssmi_tb
      DO n = 1, iv % num_ssmi_tb
         call da_check_missing( iv%ssmi_tb(n)%tb19h%qc, &
                                y %ssmi_tb(n)%tb19h, f1 )
         call da_check_missing( iv%ssmi_tb(n)%tb19v%qc, &
                                y %ssmi_tb(n)%tb19v, f2 )
         call da_check_missing( iv%ssmi_tb(n)%tb22v%qc, &
                                y %ssmi_tb(n)%tb22v, f3 )
         call da_check_missing( iv%ssmi_tb(n)%tb37h%qc, &
                                y %ssmi_tb(n)%tb37h, f4 )
         call da_check_missing( iv%ssmi_tb(n)%tb37v%qc, &
                                y %ssmi_tb(n)%tb37v, f5 )
         call da_check_missing( iv%ssmi_tb(n)%tb85h%qc, &
                                y %ssmi_tb(n)%tb85h, f6 )
         call da_check_missing( iv%ssmi_tb(n)%tb85v%qc, &
                                y %ssmi_tb(n)%tb85v, f7 )
                                                                                                                 
         write(unit,'(2i8,10e15.7)')n, 1, f1, f2, f3, f4, f5, f6, f7
      END DO
   ENDIF

!  [10] Transfer satem obs:

   IF ( iv % num_satem > 0 ) THEN
      write(unit,'(a,i8)')'satem', iv % num_satem
      DO n = 1, iv % num_satem
         write(unit,'(i8)')iv % satem(n) % info % levels
         DO k = 1, iv % satem(n) % info % levels
            call da_check_missing(iv%satem(n)%thickness(k)%qc, y % satem(n) % thickness(k), f1)
            write(unit,'(2i8,10e15.7)')n, k, f1
         END DO
      END DO
   ENDIF
   
!  [11] Transfer ssmt1 obs:

   IF ( iv % num_ssmt1 > 0 ) THEN
      write(unit,'(a,i8)')'ssmt1', iv % num_ssmt1
      DO n = 1, iv % num_ssmt1
         write(unit,'(i8)')iv % ssmt1(n) % info % levels
         DO k = 1, iv % ssmt1(n) % info % levels
            call da_check_missing(iv%ssmt1(n)%t(k)%qc, y % ssmt1(n) % t(k), f1)
            write(unit,'(2i8,10e15.7)')n, k, f1
         END DO
      END DO
   ENDIF

!  [12] Transfer ssmt2 obs:

   IF ( iv % num_ssmt2 > 0 ) THEN
      write(unit,'(a,i8)')'ssmt2', iv % num_ssmt2
      DO n = 1, iv % num_ssmt2
         write(unit,'(i8)')iv % ssmt2(n) % info % levels
         DO k = 1, iv % ssmt2(n) % info % levels
            call da_check_missing(iv%ssmt2(n)%rh(k)%qc, y % ssmt2(n) % rh(k), f1)
            write(unit,'(2i8,10e15.7)')n, k, f1
         END DO
      END DO
   ENDIF

!  [13] Transfer scatterometer obs:

   IF ( iv % num_qscat > 0 ) THEN
      write(unit,'(a,i8)')'qscat', iv % num_qscat
      DO n = 1, iv % num_qscat
         call da_check_missing(iv%qscat(n)%u%qc, y%qscat(n)%u, f1)
         call da_check_missing(iv%qscat(n)%v%qc, y%qscat(n)%v, f2)
         write(unit,'(2i8,10e15.7)')n, 1, f1, f2
      END DO
   ENDIF
!
   
!  [14] Transfer profiler obs:

   IF ( iv % num_profiler > 0 ) THEN
      write(unit,'(a,i8)')'profiler', iv % num_profiler
      DO n = 1, iv % num_profiler
         write(unit,'(i8)')iv % profiler(n) % info % levels
         DO k = 1, iv % profiler(n) % info % levels
            call da_check_missing(iv%profiler(n)%u(k)%qc, y%profiler(n)%u(k), f1)
            call da_check_missing(iv%profiler(n)%v(k)%qc, y%profiler(n)%v(k), f2)

            write(unit,'(2i8,10e15.7)')n, k, f1, f2
         END DO
      END DO
   ENDIF

!  [15] Transfer buoy  obs:

   IF ( iv % num_buoy > 0 ) THEN
      write(unit,'(a,i8)')'buoy', iv % num_buoy
      DO n = 1, iv % num_buoy
         call da_check_missing(iv%buoy(n)%u%qc, y%buoy(n)%u, f1)
         call da_check_missing(iv%buoy(n)%v%qc, y%buoy(n)%v, f2)
         call da_check_missing(iv%buoy(n)%t%qc, y%buoy(n)%t, f3)
         call da_check_missing(iv%buoy(n)%p%qc, y%buoy(n)%p, f4)
         call da_check_missing(iv%buoy(n)%q%qc, y%buoy(n)%q, f5)

         write(unit,'(2i8,10e15.7)')n, 1, f1, f2, f3, f4, f5
      END DO
   ENDIF

!  [16] Transfer TC bogus obs:

   IF ( iv % num_bogus > 0 ) THEN
      write(unit,'(a,i8)')'bogus', iv % num_bogus
      DO n = 1, iv % num_bogus
         call da_check_missing(iv%bogus(n)%slp%qc, y%bogus(n)%slp, f1)
         write(unit,'(2i8,10e15.7)')n, 1, f1
         write(unit,'(i8)')iv % bogus(n) % info % levels
         DO k = 1, iv % bogus(n) % info % levels
            call da_check_missing(iv%bogus(n)%u(k)%qc, y%bogus(n)%u(k), f2)
            call da_check_missing(iv%bogus(n)%v(k)%qc, y%bogus(n)%v(k), f3)
            call da_check_missing(iv%bogus(n)%t(k)%qc, y%bogus(n)%t(k), f4)
            call da_check_missing(iv%bogus(n)%q(k)%qc, y%bogus(n)%q(k), f5)

            write(unit,'(2i8,10e15.7)')n, k, f2, f3, f4, f5
         END DO
      END DO
   ENDIF

END SUBROUTINE da_write_y
