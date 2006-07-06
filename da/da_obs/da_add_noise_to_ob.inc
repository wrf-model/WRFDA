SUBROUTINE da_add_noise_to_ob( iv, ob )
!----------------------------------------------------------------------------   
!  History:
!
!  Additions:
!             07/08/2003  -   Profiler and Buoy Obs         Syed RH Rizvi    
!             06/23/2006  -   MPI update                    Syed RH Rizvi    
!             07/03/2006  -   update for AIRS retrievals    Syed RH Rizvi    
!
!  Purpose: Allocates observation structure and fills it fro iv.
!----------------------------------------------------------------------------   
   IMPLICIT NONE
#ifdef DM_PARALLEL
#   include <mpif.h>
#endif


   TYPE (ob_type), INTENT(INOUT) :: iv   ! Obs and header structure.
   TYPE (y_type), INTENT(INOUT)  :: ob   ! (Smaller) observation structure.

   real                          :: z1, z2, z3, z4, z5, z6, z7, dum ! Random numbers.
   integer                       :: n, k     ! Loop counters.
   integer                       :: myrank    ! Id of my processor.
   integer                       :: ierr      ! Error code from MPI routines.
   integer                       :: ounit     ! Output unit
   integer                       :: num_obs                 
!----------------------------------------------------------------------------
!  Fix output unit
!----------------------------------------------------------------------------
      ounit = rand_unit        
   if( numb_procs > 1) then
#ifdef DM_PARALLEL
! Get output unit for MPI run
  call MPI_COMM_RANK( MPI_COMM_WORLD, myrank, ierr )
  ounit = ounit*10 +  myrank
#endif
   end if
   dum = -999999.9
!----------------------------------------------------------------------
!  [1.0] Initiate random number sequence:
!----------------------------------------------------------------------

   call random_seed
   
!----------------------------------------------------------------------
!  [2.0] Create noise and output:
!----------------------------------------------------------------------

!  [2.1] Transfer surface obs:

   IF ( iv % num_synop > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_synop
       if(iv%synop(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'synop', num_obs   
      num_obs = 0 

      DO n = 1, iv % num_synop
       if(iv%synop(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')  1
!        Add random perturbation:
         call da_add_noise( iv % synop(n) % u, ob % synop(n) % u, z1 )
         call da_add_noise( iv % synop(n) % v, ob % synop(n) % v, z2 )
         call da_add_noise( iv % synop(n) % t, ob % synop(n) % t, z3 )
         call da_add_noise( iv % synop(n) % p, ob % synop(n) % p, z4 )
         call da_add_noise( iv % synop(n) % q, ob % synop(n) % q, z5 )

!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, iv % synop(n) % u % error, z1, &
                                  iv % synop(n) % v % error, z2, &
                                  iv % synop(n) % t % error, z3, &
                                  iv % synop(n) % p % error, z4, &
                                  iv % synop(n) % q % error, z5
       endif
      END DO
   ENDIF

!  [2.2] Transfer metar obs:

   IF ( iv % num_metar > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_metar
       if(iv%metar(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'metar', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_metar
       if(iv%metar(n)%loc%proc_domain) then
         num_obs = num_obs + 1 
         write(ounit,'(i8)')  1
!        Add random perturbation:
         call da_add_noise( iv % metar(n) % u, ob % metar(n) % u, z1 )
         call da_add_noise( iv % metar(n) % v, ob % metar(n) % v, z2 )
         call da_add_noise( iv % metar(n) % t, ob % metar(n) % t, z3 )
         call da_add_noise( iv % metar(n) % p, ob % metar(n) % p, z4 )
         call da_add_noise( iv % metar(n) % q, ob % metar(n) % q, z5 )

!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % metar(n) % u % error, z1, &
                                  iv % metar(n) % v % error, z2, &
                                  iv % metar(n) % t % error, z3, &
                                  iv % metar(n) % p % error, z4, &
                                  iv % metar(n) % q % error, z5
       endif
      END DO
   ENDIF

!  [2.3] Transfer ships obs:

   IF ( iv % num_ships > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_ships
       if(iv%ships(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'ships', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_ships 
       if(iv%ships(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')  1
!        Add random perturbation:
         call da_add_noise( iv % ships(n) % u, ob % ships(n) % u, z1 )
         call da_add_noise( iv % ships(n) % v, ob % ships(n) % v, z2 )
         call da_add_noise( iv % ships(n) % t, ob % ships(n) % t, z3 )
         call da_add_noise( iv % ships(n) % p, ob % ships(n) % p, z4 )
         call da_add_noise( iv % ships(n) % q, ob % ships(n) % q, z5 )
!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % ships(n) % u % error, z1, &
                                  iv % ships(n) % v % error, z2, &
                                  iv % ships(n) % t % error, z3, &
                                  iv % ships(n) % p % error, z4, &
                                  iv % ships(n) % q % error, z5
       endif
      END DO
   ENDIF


!  [2.4.1] Transfer Geostationary AMVs obs:

   IF ( iv % num_geoamv > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_geoamv
       if(iv%geoamv(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'geoamv', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_geoamv
       if(iv%geoamv(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv % geoamv(n) % info % levels
         DO k = 1, iv % geoamv(n) % info % levels
!        Add random perturbation:
            call da_add_noise( iv % geoamv(n) % u(k), ob % geoamv(n) % u(k), z1)
            call da_add_noise( iv % geoamv(n) % v(k), ob % geoamv(n) % v(k), z2)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % geoamv(n) % u(k) % error, z1, &
                               iv % geoamv(n) % v(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
         END DO
       endif
      END DO
   ENDIF

!  [2.4.2] Transfer Polar AMVs obs:

   IF ( iv % num_polaramv > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_polaramv
       if(iv%polaramv(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'polaramv', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_polaramv
       if(iv%polaramv(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv % polaramv(n) % info % levels
         DO k = 1, iv % polaramv(n) % info % levels
!        Add random perturbation:
            call da_add_noise( iv % polaramv(n) % u(k), ob % polaramv(n) % u(k), z1)
            call da_add_noise( iv % polaramv(n) % v(k), ob % polaramv(n) % v(k), z2)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % polaramv(n) % u(k) % error, z1, &
                               iv % polaramv(n) % v(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
         END DO
       endif
      END DO
   ENDIF

!  [2.5] Transfer gpspw obs:

   IF ( iv % num_gpspw > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_gpspw
       if(iv%gpspw(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'gpspw', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_gpspw
       if(iv%gpspw(n)%loc%proc_domain) then
        num_obs = num_obs + 1
        write(ounit,'(i8)')  1
!        Add random perturbation:
         call da_add_noise( iv % gpspw(n) % tpw, ob % gpspw(n) % tpw, z1 )
         
!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, iv % gpspw(n) % tpw % error, z1, &
                               dum, dum, dum, dum, dum, dum, dum, dum
       endif
      END DO
   ENDIF

!  [2.6] Transfer sonde obs:

   IF ( iv % num_sound > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_sound
       if(iv%sound(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'sound', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_sound
       if(iv%sound(n)%loc%proc_domain) then
          num_obs = num_obs + 1
         write(ounit,'(i8)')iv % sound(n) % info % levels
         DO k = 1, iv % sound(n) % info % levels
!           Add random perturbation:
            call da_add_noise( iv % sound(n) % u(k), ob % sound(n) % u(k), z1)
            call da_add_noise( iv % sound(n) % v(k), ob % sound(n) % v(k), z2)
            call da_add_noise( iv % sound(n) % t(k), ob % sound(n) % t(k), z3)
            call da_add_noise( iv % sound(n) % q(k), ob % sound(n) % q(k), z4)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % sound(n) % u(k) % error, z1, &
                               iv % sound(n) % v(k) % error, z2, &
                               iv % sound(n) % t(k) % error, z3, &
                               iv % sound(n) % q(k) % error, z4, &
                               dum, dum
         END DO
       endif
      END DO
! Now do surface level
      num_obs = 0
      DO n = 1, iv % num_sound
       if(iv%sound(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'sonde_sfc', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_sound
       if(iv%sound(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') 1
!           Add random perturbation:
         call da_add_noise( iv % sonde_sfc(n) % u, ob % sonde_sfc(n) % u, z1 )
         call da_add_noise( iv % sonde_sfc(n) % v, ob % sonde_sfc(n) % v, z2 )
         call da_add_noise( iv % sonde_sfc(n) % t, ob % sonde_sfc(n) % t, z3 )
         call da_add_noise( iv % sonde_sfc(n) % p, ob % sonde_sfc(n) % p, z4 )
         call da_add_noise( iv % sonde_sfc(n) % q, ob % sonde_sfc(n) % q, z5 )

!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, iv % sonde_sfc(n) % u % error, z1, &
                                  iv % sonde_sfc(n) % v % error, z2, &
                                  iv % sonde_sfc(n) % t % error, z3, &
                                  iv % sonde_sfc(n) % p % error, z4, &
                                  iv % sonde_sfc(n) % q % error, z5
       endif
      ENDDO
   ENDIF

!  [2.7] Transfer airep obs:

   IF ( iv % num_airep > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_airep
       if(iv%airep(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'airep', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_airep
       if(iv%airep(n)%loc%proc_domain)then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv % airep(n) % info % levels
         DO k = 1, iv % airep(n) % info % levels
!           Add random perturbation:
            call da_add_noise( iv % airep(n) % u(k), ob % airep(n) % u(k), z1)
            call da_add_noise( iv % airep(n) % v(k), ob % airep(n) % v(k), z2)
            call da_add_noise( iv % airep(n) % t(k), ob % airep(n) % t(k), z3)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % airep(n) % u(k) % error, z1, &
                               iv % airep(n) % v(k) % error, z2, &
                               iv % airep(n) % t(k) % error, z3, &
                               dum, dum, dum, dum
         END DO
       endif
      END DO
   ENDIF

!  [2.8] Transfer pilot obs:

   IF ( iv % num_pilot > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_pilot
       if(iv%pilot(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'pilot', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_pilot
       if(iv%pilot(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv % pilot(n) % info % levels
         DO k = 1, iv % pilot(n) % info % levels
!           Add random perturbation:
            call da_add_noise( iv % pilot(n) % u(k), ob % pilot(n) % u(k), z1)
            call da_add_noise( iv % pilot(n) % v(k), ob % pilot(n) % v(k), z2)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % pilot(n) % u(k) % error, z1, &
                               iv % pilot(n) % v(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
         END DO
       endif
      END DO
   ENDIF

!  [2.9] Transfer SSM/I obs:SSMI:

   IF ( iv % num_ssmi_retrieval > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_ssmi_retrieval
       if(iv%ssmi_retrieval(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'ssmir', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_ssmi_retrieval
       if(iv%ssmi_retrieval(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') 1 
 
!        Add random perturbation:
         call da_add_noise( iv % ssmi_retrieval(n) % speed, &
                            ob % ssmi_retrieval(n) % speed, z1 )
         call da_add_noise( iv % ssmi_retrieval(n) % tpw, &
                            ob % ssmi_retrieval(n) % tpw, z2 )
!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % ssmi_retrieval(n) % speed % error, z1, &
                                  iv % ssmi_retrieval(n) % tpw % error, z2,   & 
                                  dum, dum, dum, dum, dum, dum
       endif
      END DO
   ENDIF

   IF ( iv % num_ssmi_tb > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_ssmi_tb
       if(iv%ssmi_tb(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'ssmiT', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_ssmi_tb
       if(iv%ssmi_tb(n)%loc%proc_domain) then
         num_obs = num_obs + 1
!        Add random perturbation:
         call da_add_noise( iv % ssmi_tb(n) % tb19h, &
                            ob % ssmi_tb(n) % tb19h, z1)
         call da_add_noise( iv % ssmi_tb(n) % tb19v, &
                            ob % ssmi_tb(n) % tb19v, z2)
         call da_add_noise( iv % ssmi_tb(n) % tb22v, &
                            ob % ssmi_tb(n) % tb22v, z3)
         call da_add_noise( iv % ssmi_tb(n) % tb37h, &
                            ob % ssmi_tb(n) % tb37h, z4)
         call da_add_noise( iv % ssmi_tb(n) % tb37v, &
                            ob % ssmi_tb(n) % tb37v, z5)
         call da_add_noise( iv % ssmi_tb(n) % tb85h, &
                            ob % ssmi_tb(n) % tb85h, z6)
         call da_add_noise( iv % ssmi_tb(n) % tb85v, &
                            ob % ssmi_tb(n) % tb85v, z7)

!        Write out data:
         write(ounit,'(i8)') 1 
         write(ounit,'(2i8,14e15.7)')num_obs, 1, &
                                  iv % ssmi_tb(n) % tb19h % error, z1, &
                                  iv % ssmi_tb(n) % tb19v % error, z2, &
                                  iv % ssmi_tb(n) % tb22v % error, z3, &
                                  iv % ssmi_tb(n) % tb37h % error, z4, &
                                  iv % ssmi_tb(n) % tb37v % error, z5, &
                                  iv % ssmi_tb(n) % tb85h % error, z6, &
                                  iv % ssmi_tb(n) % tb85v % error, z7
       endif
      END DO
   ENDIF

!  [2.10] Transfer satem obs:

   IF ( iv % num_satem > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_satem
       if(iv%satem(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'satem', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_satem
       if(iv%satem(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv % satem(n) % info % levels
         DO k = 1, iv % satem(n) % info % levels
!           Add random perturbation:
            call da_add_noise( iv % satem(n) % thickness(k), &
                               ob % satem(n) % thickness(k), z1 )
!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                                     iv % satem(n) % thickness(k) % error, z1, &
                                     dum, dum, dum, dum, dum, dum, dum, dum
         END DO
       endif
      END DO
   ENDIF
   
!  [2.11] Transfer ssmt1 obs:

   IF ( iv % num_ssmt1 > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_ssmt1
       if(iv%ssmt1(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'ssmt1', num_obs   
      num_obs = 0 

      DO n = 1, iv % num_ssmt1
       if(iv%ssmt1(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv % ssmt1(n) % info % levels
         
         DO k = 1, iv % ssmt1(n) % info % levels

!           Add random perturbation:
            call da_add_noise( iv % ssmt1(n) % t(k), &
                               ob % ssmt1(n) % t(k), z1 )
!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, iv % ssmt1(n) % t(k) % error, z1, &
                                     dum, dum, dum, dum, dum, dum, dum, dum
         END DO
       endif
      END DO
   ENDIF

!  [2.12] Transfer ssmt2 obs:

   IF ( iv % num_ssmt2 > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_ssmt2
       if(iv%ssmt2(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'ssmt2', num_obs   
      num_obs = 0 

      DO n = 1, iv % num_ssmt2
       if(iv%ssmt2(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv % ssmt2(n) % info % levels
         
         DO k = 1, iv % ssmt2(n) % info % levels

!           Add random perturbation:
            call da_add_noise( iv % ssmt2(n) % rh(k), &
                               ob % ssmt2(n) % rh(k), z1 )
!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, iv % ssmt2(n) % rh(k) % error, z1, &
                                     dum, dum, dum, dum, dum, dum, dum, dum
         END DO
       endif
      END DO
   ENDIF
   
!  [2.13] Transfer scatterometer obs:

   IF ( iv % num_qscat > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_qscat
       if(iv%qscat(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'qscat', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_qscat
       if(iv%qscat(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') 1
!        Add random perturbation:
        call da_add_noise( iv % qscat(n) % u, ob % qscat(n) % u, z1 )
        call da_add_noise( iv % qscat(n) % v, ob % qscat(n) % v, z2 )

!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % qscat(n) % u % error, z1, &
                                  iv % qscat(n) % v % error, z2, &
                                  dum, dum, dum, dum, dum, dum
       endif
      END DO
   ENDIF

!  [2.14] Transfer buoy obs:

   IF ( iv % num_buoy > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_buoy
       if(iv%buoy(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'buoy', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_buoy
       if(iv%buoy(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') 1
!        Add random perturbation:
         call da_add_noise( iv % buoy(n) % u, ob % buoy(n) % u, z1 )
         call da_add_noise( iv % buoy(n) % v, ob % buoy(n) % v, z2 )
         call da_add_noise( iv % buoy(n) % t, ob % buoy(n) % t, z3 )
         call da_add_noise( iv % buoy(n) % p, ob % buoy(n) % p, z4 )
         call da_add_noise( iv % buoy(n) % q, ob % buoy(n) % q, z5 )

!        Write out data:
        write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % buoy(n) % u % error, z1, &
                                  iv % buoy(n) % v % error, z2, &
                                  iv % buoy(n) % t % error, z3, &
                                  iv % buoy(n) % p % error, z4, &
                                  iv % buoy(n) % q % error, z5
      endif
     END DO
   ENDIF

!  [2.15] Transfer profiler obs:

   IF ( iv % num_profiler > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_profiler
       if(iv%profiler(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'profiler', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_profiler
       if(iv%profiler(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv % profiler(n) % info % levels
         DO k = 1, iv % profiler(n) % info % levels
!           Add random perturbation:
            call da_add_noise( iv % profiler(n) % u(k), ob % profiler(n) % u(k), z1)
            call da_add_noise( iv % profiler(n) % v(k), ob % profiler(n) % v(k), z2)
!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % profiler(n) % u(k) % error, z1, &
                               iv % profiler(n) % v(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
         END DO
       endif
      END DO
   ENDIF

!  [2.16] Transfer TC bogus obs:

   IF ( iv % num_bogus > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_bogus
       if(iv%bogus(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'bogus', num_obs   
      num_obs = 0 

      DO n = 1, iv % num_bogus
       if(iv%bogus(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') 1
         call da_add_noise( iv % bogus(n) % slp, ob % bogus(n) % slp, z1 )
         write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % bogus(n) % slp % error, z1, &
                                  dum, dum, dum, dum, dum, dum, dum, dum

         write(ounit,'(i8)')iv % bogus(n) % info % levels
         DO k = 1, iv % bogus(n) % info % levels
!           Add random perturbation:
            call da_add_noise( iv % bogus(n) % u(k), ob % bogus(n) % u(k), z1)
            call da_add_noise( iv % bogus(n) % v(k), ob % bogus(n) % v(k), z2)
            call da_add_noise( iv % bogus(n) % t(k), ob % bogus(n) % t(k), z3)
            call da_add_noise( iv % bogus(n) % q(k), ob % bogus(n) % q(k), z4)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % bogus(n) % u(k) % error, z1, &
                               iv % bogus(n) % v(k) % error, z2, &
                               iv % bogus(n) % t(k) % error, z3, &
                               iv % bogus(n) % q(k) % error, z4, &
                               dum, dum
         END DO
       endif
      END DO
   ENDIF
!
!  Transfer AIRS retrievals:
!
   IF ( iv % num_airsr > 0 ) THEN
      num_obs = 0
      DO n = 1, iv % num_airsr
       if(iv%airsr(n)%loc%proc_domain) num_obs = num_obs + 1
      ENDDO
      write(ounit,'(a20,i8)')'airsr', num_obs   
      num_obs = 0 
      DO n = 1, iv % num_airsr
       if(iv%airsr(n)%loc%proc_domain) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv % airsr(n) % info % levels
         DO k = 1, iv % airsr(n) % info % levels
!           Add random perturbation:
            call da_add_noise( iv % airsr(n) % t(k), ob % airsr(n) % t(k), z1)
            call da_add_noise( iv % airsr(n) % q(k), ob % airsr(n) % q(k), z2)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % airsr(n) % t(k) % error, z1, &
                               iv % airsr(n) % q(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
         END DO
       endif
      END DO
   ENDIF

  write(ounit,'(a20,i8)')'********************', 0
  close (ounit)

END SUBROUTINE da_add_noise_to_ob
