!WRF:DRIVER_LAYER:INTEGRATION
!

MODULE module_integrate_ad

CONTAINS

RECURSIVE SUBROUTINE integrate_ad ( grid, nl_date_string, nl_date_index, ad_date_string, ad_date_index )



   USE module_domain
   USE module_driver_constants
   USE module_nesting
   USE module_configure
   USE module_timing
   USE WRF_ESMF_MOD

   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , POINTER :: grid

   character(len=19), dimension(1000), intent(in)    :: nl_date_string
   integer,                            intent(inout) :: nl_date_index
   character(len=19), dimension(1000), intent(in)    :: ad_date_string
   integer,                            intent(inout) :: ad_date_index

! module_integrate:integrate
! <DESCRIPTION> 
! This is a driver-level routine that controls the integration of a
! domain and subdomains rooted at the domain. 
! 
! The integrate routine takes a domain pointed to by the argument
! <em>grid</em> and advances the domain and its associated nests from the
! grid's current time, stored as grid%current_time, to a given time
! forward in the simulation, stored as grid%stop_subtime. The
! stop_subtime value is arbitrary and does not have to be the same as
! time that the domain finished integrating.  The simulation stop time
! for the grid is known to the grid's clock (grid%domain_clock) and that
! is checked with a call to ESMF_ClockIsStopTime prior to beginning the
! loop over time period that is specified by the
! current_time/stop_subtime interval.
! 
! The clock, the simulation stop time for the domain, and other timing
! aspects for the grid are set up in the routine
! (<a href="setup_timekeeping.html">setup_timekeeping</a>) at the time
! that the domain is initialized.
! The lower-level time library and the type declarations for the times
! and time intervals used are defined in external/esmf_time_f90 which, 
! depending on build-time options, either incorporates the embedded ESMF 
! subset implementation contained in that directory or incorporates a 
! site-specific installation of the ESMF library.  Note that arithmetic and 
! comparison is performed on these data types using F90 operator overloading, 
! also defined in that library.
! 
! This routine is the lowest level of the WRF Driver Layer and for the most
! part the WRF routines that are called from here are in the topmost level
! of the Mediation Layer.  Mediation layer routines typically are not 
! defined in modules. Therefore, the routines that this routine calls
! have explicit interfaces specified in an interface block in this routine.
!
! As part of the Driver Layer, this routine is intended to be non model-specific
! and so a minimum of WRF-specific logic is coded at this level. Rather, there
! are a number of calls to mediation layer routines that contain this logic, some
! of which are merely stubs in WRF Mediation Layer that sits below this routine
! in the call tree.  The routines that integrate calls in WRF are defined in
! share/mediation_integrate.F.
! 
! Flow of control
! 
! 1. Check to see that the domain is not finished 
! by testing the value returned by ESMF_ClockIsStopTime for the
! domain.
! 
! 2. <a href=model_to_grid_config_rec.html>Model_to_grid_config_rec</a> is called to load the local config_flags
! structure with the configuration information for the grid stored
! in model_config_rec and indexed by the grid's unique integer id. These
! structures are defined in frame/module_configure.F.
! 
! 3. The current time of the domain is retrieved from the domain's clock
! using ESMF_ClockGet.  There is another call to ESMF_ClockGet
! inside the WHILE loop that follows.
! 
! 4. Iterate forward while the current time is less than the stop subtime.
! 
! 4.a. Start timing for this iteration (only on node zero in distributed-memory runs)
! 
! 4.b. Call <a href=med_setup_step.html>med_setup_step</a> to allow the mediation layer to 
! do anything that's needed to call the solver for this domain.  In WRF this means setting
! the indices into the 4D tracer arrays for the domain.
! 
! 4.c. Check for any nests that need to be started up at this time.  This is done 
! calling the logical function <a href=nests_to_open.html>nests_to_open</a> (defined in 
! frame/module_nesting.F) which returns true and the index into the current domain's list
! of children to use for the nest when one needs to be started.
! 
! 4.c.1  Call <a href=alloc_and_configure_domain.html>alloc_and_configure_domain</a> to allocate
! the new nest and link it as a child of this grid.
! 
! 4.c.2  Call <a href=setup_Timekeeping.html>setup_Timekeeping</a> for the nest.
! 
! 4.c.3  Initialize the nest's arrays by calling <a href=med_nest_initial.html>med_nest_initial</a>. This will
! either interpolate data from this grid onto the nest, read it in from a file, or both. In a restart run, this
! is also where the nest reads in its restart file.
! 
! 4.d  If a nest was opened above, check for and resolve overlaps (this is not implemented in WRF 2.0, which
! supports multiple nests on the same level but does not support overlapping).
! 
! 4.e  Give the mediation layer an opportunity to do something before the solver is called by
! calling <a href=med_before_solve_io.html>med_before_solve_io</a>. In WRF this is the point at which history and
! restart data is output.
! 
! 4.f  Call <a href=solve_interface_ad.html>solve_interface_ad</a> which calls the solver that advance the domain 
! one time step, then advance the domain's clock by calling ESMF_ClockAdvance.  Upon advancing the clock,
! the current time for the grid is updated by calling ESMF_ClockGet. The enclosing WHILE loop around
! this section is for handling other domains with which this domain may overlap.  It is not active in WRF 2.0 and
! only executes one trip.  
! 
! 4.h Iterate over the children of this domain (<tt>DO kid = 1, max_nests</tt>) and check each child pointer to see
! if it is associated (and therefore, active).
! 
! 4.h.1  Force the nested domain boundaries from this domain by calling <a href=med_nest_force.html>med_nest_force</a>.
! 
! 4.h.2  Setup the time period over which the nest is to run. Sine the current grid has been advanced one time step
! and the nest has not, the start for the nest is this grid's current time minus one time step.  The nest's stop_subtime
! is the current time, bringing the nest up the same time level as this grid, its parent.
! 
! 4.h.3  Recursively call this routine, integrate, to advance the nest's time.  Since it is recursive, this will
! also advance all the domains who are nests of this nest and so on.  In other words, when this call returns, all
! the domains rooted at the nest will be at the current time.
! 
! 4.h.4  Feedback data from the nested domain back onto this domain by calling <a href=med_nest_feedback.html>med_nest_feedback</a>.
! 
! 4.i  Write the time to compute this grid and its subtree. This marks the end of the loop begun at step 4, above.
! 
! 5. Give the mediation layer an opportunity to do I/O at the end of the sequence of steps that brought the
! grid up to stop_subtime with a call to <a href=med_last_solve_io.html>med_last_solve_io</a>.  In WRF, this 
! is used to generate the final history and/or restart output when the domain reaches the end of it's integration.
! There is logic here to make sure this occurs correctly on a nest, since the nest may finish before its parent.
! </DESCRIPTION>

   !  Local data.

   CHARACTER*32                           :: outname, rstname
   TYPE(domain) , POINTER                 :: grid_ptr , new_nest
   TYPE(domain)                           :: intermediate_grid
   INTEGER                                :: step
   INTEGER                                :: nestid , kid
   LOGICAL                                :: a_nest_was_opened
   INTEGER                                :: fid , rid
   LOGICAL                                :: lbc_opened
   REAL                                   :: time, btime, bfrq
   CHARACTER*256                          :: message, message2
   TYPE (grid_config_rec_type)            :: config_flags
   LOGICAL , EXTERNAL                     :: wrf_dm_on_monitor
   INTEGER                                :: idum1 , idum2 , ierr , open_status
   INTEGER                                :: rc

   CHARACTER (LEN=25)                     :: currTime_str

! Local
   TYPE(ESMF_TimeInterval) :: zero_time, interval

   ! interface
   INTERFACE
       ! mediation-supplied solver
     SUBROUTINE solve_interface_ad ( grid )
       USE module_domain
       TYPE (domain) grid
     END SUBROUTINE solve_interface_ad
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O before the call to the solve routine
     SUBROUTINE med_before_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_before_solve_io
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O after the call to the solve routine
     SUBROUTINE med_after_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_after_solve_io
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O to initialize a new nest
     SUBROUTINE med_nest_initial ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_initial

       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! provide parent->nest forcing
     SUBROUTINE med_nest_force ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_force

#ifdef MOVE_NESTS
     SUBROUTINE med_nest_move ( parent , grid )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
     END SUBROUTINE med_nest_move
#endif

       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! provide parent->nest feedback
     SUBROUTINE med_nest_feedback ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_feedback

       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O prior to the close of this call to integrate
     SUBROUTINE med_last_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_last_solve_io
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform setup before iteration over steps in this call to integrate
     SUBROUTINE med_setup_step ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_setup_step
       ! mediation-supplied routine that intializes the nest from the grid
       ! by interpolation

     SUBROUTINE Setup_Timekeeping( grid )
       USE module_domain
       TYPE(domain), POINTER :: grid
     END SUBROUTINE

   END INTERFACE

   print *, 'ESMF_ClockIsStartTime(grid%domain_clock ,rc=rc)=', &
             ESMF_ClockIsStartTime(grid%domain_clock ,rc=rc)

   IF ( .NOT. ESMF_ClockIsStartTime(grid%domain_clock ,rc=rc) ) THEN
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
      CALL ESMF_ClockGet( grid%domain_clock, currTime=grid%current_time, rc=rc )

!-----start check nonlinear and adjoint input
      CALL wrf_clockprint ( 150, grid%domain_clock, 'DEBUG:  integrate_ad(),' )

      CALL wrf_timetoa( grid%current_time, currTime_str )

!-----start check non-linear input
      if(currTime_str(1:19) .eq. nl_date_string(nl_date_index)(1:19)) then
         write(unit=*, fmt='(a,i5,4(x,a))') &
              'xyh nl_date_index =', nl_date_index, &
              'read NL at currTime_str(1:19)=', currTime_str(1:19), &
              'nl_date_string(nl_date_index)(1:19)=', nl_date_string(nl_date_index)(1:19)
! ----do input
         CALL med_auxinput2_in4ad ( grid , config_flags , nl_date_string(nl_date_index) )
         write(unit=10, fmt='(a,i4,2a,2x,a,f12.4)') &
              'xyh nl_date_string(', nl_date_index, ')=',nl_date_string(nl_date_index)(1:19), &
              'grid%t_2(1,1,1)=', grid%em_t_2(1,1,1)
               nl_date_index = nl_date_index + 1
      else
         write(unit=*, fmt='(2a)') &
              'xyh no NL state at ', currTime_str(1:19)
         write(unit=*, fmt='(2a)') &
              'xyh use NL state from ', nl_date_string(nl_date_index)
         CALL med_auxinput2_in4ad ( grid , config_flags , nl_date_string(nl_date_index) )
         write(unit=10, fmt='(a,i4,2a,2x,a,f12.4)') &
              'xyh nl_date_string(', nl_date_index, ')=',nl_date_string(nl_date_index)(1:19), &
              'grid%t_2(1,1,1)=', grid%em_t_2(1,1,1)
      endif
!-----end check non-linear input


!-----start check adjoint input
      if(currTime_str(1:19) .eq. ad_date_string(ad_date_index)(1:19)) then
         print *, 'ad_date_index =', ad_date_index
         write(unit=*, fmt='(2a)') &
              'currTime_str(1:19)=', currTime_str(1:19), &
              'ad_date_string(ad_date_index)(1:19)=', ad_date_string(ad_date_index)(1:19)
!-       do input
         CALL med_auxinput3_in ( grid , config_flags )
         CALL add_forcing_to_ad(grid)
         ad_date_index = ad_date_index + 1
!     else
!        write(unit=*, fmt='(2a)') &
!             'currTime_str(1:19)=', currTime_str(1:19), &
!             'ad_date_string(ad_date_index)(1:19)=', ad_date_string(ad_date_index)(1:19)
      endif
!-----end check adjoint input

      CALL ESMF_TimeIntervalPrint(grid%domain_clock%TimeStep, 'DEBUG:  integrate_ad.', rc)
      CALL print_a_timeinterval(grid%domain_clock%TimeStep)
      DO WHILE ( grid%current_time .GT. grid%start_subtime )
         IF ( wrf_dm_on_monitor() ) THEN
           CALL start_timing
         END IF

!--------start check nonlinear and adjoint input

         CALL ESMF_ClockGet( grid%domain_clock, currTime=grid%current_time, rc=rc )
         CALL wrf_clockprint ( 150, grid%domain_clock, 'DEBUG:  integrate_ad(),' )

         CALL wrf_timetoa( grid%current_time, currTime_str )

!-----start check non-linear input
         if(currTime_str(1:19) .eq. nl_date_string(nl_date_index)(1:19)) then
            write(unit=*, fmt='(a,i5,4(x,a))') &
                 'xyh nl_date_index =', nl_date_index, &
                 'read NL at currTime_str(1:19)=', currTime_str(1:19), &
                 'nl_date_string(nl_date_index)(1:19)=', nl_date_string(nl_date_index)(1:19)
! ----do input
            CALL med_auxinput2_in4ad ( grid , config_flags , nl_date_string(nl_date_index) )
            write(unit=10, fmt='(a,i4,2a,2x,a,f12.4)') &
                 'xyh nl_date_string(', nl_date_index, ')=',nl_date_string(nl_date_index)(1:19), &
                 'grid%t_2(1,1,1)=', grid%em_t_2(1,1,1)
                  nl_date_index = nl_date_index + 1
         else
            write(unit=*, fmt='(2a)') &
                 'xyh no NL state at ', currTime_str(1:19)
            write(unit=*, fmt='(2a)') &
                 'xyh use NL state from ', nl_date_string(nl_date_index)
            CALL med_auxinput2_in4ad ( grid , config_flags , nl_date_string(nl_date_index) )
            write(unit=10, fmt='(a,i4,2a,2x,a,f12.4)') &
                 'xyh nl_date_string(', nl_date_index, ')=',nl_date_string(nl_date_index)(1:19), &
                 'grid%t_2(1,1,1)=', grid%em_t_2(1,1,1)
         endif
!-----end check non-linear input

!--------start check adjoint input

         if(currTime_str(1:19) .eq. ad_date_string(ad_date_index)(1:19)) then
            print *, 'ad_date_index =', ad_date_index
            write(unit=*, fmt='(2a)') &
                 'currTime_str(1:19)=', currTime_str(1:19), &
                 'ad_date_string(ad_date_index)(1:19)=', ad_date_string(ad_date_index)(1:19)
!-          do input
            CALL med_auxinput3_in ( grid , config_flags )
            CALL add_forcing_to_ad(grid)
            ad_date_index = ad_date_index + 1
!        else
!           write(unit=*, fmt='(2a)') &
!                'currTime_str(1:19)=', currTime_str(1:19), &
!                'ad_date_string(ad_date_index)(1:19)=', ad_date_string(ad_date_index)(1:19)
         endif
!--------end check adjoint input

         CALL med_setup_step ( grid , config_flags )
         a_nest_was_opened = .false.
         ! for each nest whose time has come...
!        CALL med_var_solve_io ( grid , config_flags )
         DO WHILE ( nests_to_open( grid , nestid , kid ) )
            ! nestid is index into model_config_rec (module_configure) of the grid
            ! to be opened; kid is index into an open slot in grid's list of children
            a_nest_was_opened = .true.
            CALL alloc_and_configure_domain ( domain_id  = nestid ,                          &
                                              grid       = new_nest ,                        &
                                              parent     = grid ,                            &
                                              kid        = kid                               )
            CALL Setup_Timekeeping (new_nest)
            CALL med_nest_initial ( grid , new_nest , config_flags )
         END DO
         IF ( a_nest_was_opened ) THEN
            CALL set_overlaps ( grid )   ! find overlapping and set pointers
         END IF
         grid_ptr => grid
         DO WHILE ( ASSOCIATED( grid_ptr ) )
            CALL wrf_debug( 100 , 'module_integrate_ad: calling solve interface ad' )
            CALL solve_interface_ad ( grid_ptr ) 
            CALL wrf_clockprint ( 150, grid_ptr%domain_clock, &
                                  'DEBUG integrate_ad():  before ESMF_ClockBack,' )
            CALL ESMF_ClockBack( grid_ptr%domain_clock, rc=rc )

!--
!-- Add check _ad_date_string for input forcing.
!--
            CALL wrf_check_error( ESMF_SUCCESS, rc, &
                                  'ESMF_ClockAdvance() FAILED', &
                                  __FILE__ , &
                                  __LINE__  )
            CALL wrf_clockprint ( 150, grid_ptr%domain_clock, &
                                  'DEBUG integrate_ad():  after ESMF_ClockBack,' )
            CALL ESMF_ClockGet( grid_ptr%domain_clock, currTime=grid_ptr%current_time, rc=rc )
            CALL wrf_debug( 100 , 'module_integrate_ad: back from solve interface ad' )
            grid_ptr => grid_ptr%sibling
         END DO
         grid_ptr => grid
         DO WHILE ( ASSOCIATED( grid_ptr ) )
            DO kid = 1, max_nests
              IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
                ! Recursive -- advance nests from previous time level to this time level.
                CALL wrf_debug( 100 , 'module_integrate_ad: calling med_nest_force ' )
                CALL med_nest_force ( grid_ptr , grid_ptr%nests(kid)%ptr , config_flags )
                CALL wrf_debug( 100 , 'module_integrate_ad: back from med_nest_force ' )
                grid_ptr%nests(kid)%ptr%start_subtime = grid%current_time - grid%step_time
                grid_ptr%nests(kid)%ptr%stop_subtime = grid%current_time
                CALL integrate_ad ( grid_ptr%nests(kid)%ptr, nl_date_string, nl_date_index, ad_date_string, ad_date_index ) 
                CALL wrf_debug( 100 , 'module_integrate_ad: back from recursive call to integrate_ad ' )
                CALL wrf_debug( 100 , 'module_integrate_ad: calling med_nest_feedback ' )
                CALL med_nest_feedback ( grid_ptr , grid_ptr%nests(kid)%ptr , config_flags )
                CALL wrf_debug( 100 , 'module_integrate_ad: back from med_nest_feedback ' )
#ifdef MOVE_NESTS
                CALL med_nest_move ( grid_ptr , grid_ptr%nests(kid)%ptr )
#endif
              END IF
            END DO
            grid_ptr => grid_ptr%sibling
         END DO
         !  Report on the timing for a single time step.
         IF ( wrf_dm_on_monitor() ) THEN
           CALL wrf_timetoa ( grid%current_time, message2 )
           WRITE ( message , FMT = '("main: time ",A," on domain ",I3)' ) TRIM(message2), grid%id
           CALL end_timing ( TRIM(message) )
         END IF
      END DO
      ! Avoid double writes on nests if this is not really the last time;
      ! Do check for write if the parent domain is ending.
      IF ( grid%id .EQ. 1 ) THEN               ! head_grid
!       CALL med_last_solve_ad_io ( grid , config_flags )

!--------start check nonlinear and adjoint input

         CALL ESMF_ClockGet( grid%domain_clock, currTime=grid%current_time, rc=rc )
         CALL wrf_clockprint ( 1, grid%domain_clock, 'Check last time:  integrate_ad(),' )

         CALL wrf_timetoa( grid%current_time, currTime_str )

!--------start check non-linear input
         if(currTime_str(1:19) .eq. nl_date_string(nl_date_index)(1:19)) then
            write(unit=*, fmt='(a,i5,4(x,a))') &
                 'xyh nl_date_index =', nl_date_index, &
                 'read NL at currTime_str(1:19)=', currTime_str(1:19), &
                 'nl_date_string(nl_date_index)(1:19)=', nl_date_string(nl_date_index)(1:19)
! ----------do input
            CALL med_auxinput2_in4ad ( grid , config_flags , nl_date_string(nl_date_index) )
            write(unit=10, fmt='(a,i4,2a,2x,a,f12.4)') &
                 'xyh nl_date_string(', nl_date_index, ')=',nl_date_string(nl_date_index)(1:19), &
                'grid%t_2(1,1,1)=', grid%em_t_2(1,1,1)
                  nl_date_index = nl_date_index + 1
         else
            write(unit=*, fmt='(2a)') &
                 'xyh no NL state at ', currTime_str(1:19)
            write(unit=*, fmt='(2a)') &
                 'xyh use NL state from ', nl_date_string(nl_date_index)
            CALL med_auxinput2_in4ad ( grid , config_flags , nl_date_string(nl_date_index) )
            write(unit=10, fmt='(a,i4,2a,2x,a,f12.4)') &
                 'xyh nl_date_string(', nl_date_index, ')=',nl_date_string(nl_date_index)(1:19), &
                'grid%t_2(1,1,1)=', grid%em_t_2(1,1,1)
         endif
!--------end check non-linear input

!--------start check adjoint input
         if(currTime_str(1:19) .eq. ad_date_string(ad_date_index)(1:19)) then
            print *, 'ad_date_index =', ad_date_index
            write(unit=*, fmt='(2a)') &
                 'currTime_str(1:19)=', currTime_str(1:19), &
                 'ad_date_string(ad_date_index)(1:19)=', ad_date_string(ad_date_index)(1:19)
!-          do input
            CALL med_auxinput3_in ( grid , config_flags )
            CALL add_forcing_to_ad(grid)
            ad_date_index = ad_date_index + 1
!        else
!           write(unit=*, fmt='(2a)') &
!                'currTime_str(1:19)=', currTime_str(1:19), &
!                'ad_date_string(ad_date_index)(1:19)=', ad_date_string(ad_date_index)(1:19)
         endif
!--------end check adjoint input
!          CALL med_last_solve_ad_io ( grid , config_flags )
         call med_filter_out ( grid , config_flags )
!--------Force output last gradient.
         call med_hist_out ( grid , 0 , config_flags )
      ELSE
        IF ( ESMF_ClockIsStartTime(grid%domain_clock , rc=rc) .OR. &
             ESMF_ClockIsStartTime(grid%parents(1)%ptr%domain_clock , rc=rc ) ) THEN
!          CALL med_last_solve_ad_io ( grid , config_flags )
        ENDIF
      ENDIF
   END IF

END SUBROUTINE integrate_ad

END MODULE module_integrate_ad

