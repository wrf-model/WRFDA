
      REAL FUNCTION da_dot_cv( x, y, n, xp, mzs )

!
!     Forms the dot product of two vectors that are organized in the 
!     format of a "cv_type".  
!
!     Capable of producing bitwise-exact results for distributed-memory 
!     parallel runs for testing.  This feature is very slow and consumes 
!     lots of memory.  
!
        IMPLICIT NONE

        INTEGER, INTENT(IN)            :: n
        REAL, DIMENSION(n), INTENT(IN) :: x,y
        TYPE (xpose_type),  INTENT(IN) :: xp    ! decomposed dimensions
        INTEGER,            INTENT(IN) :: mzs(:)    ! mz for each variable
                                                    ! (to identify 2D arrays)

        REAL, POINTER :: xg(:), yg(:)
        INTEGER :: cv_size_global

        REAL :: dtemp1,dtemp1x
        LOGICAL, EXTERNAL :: wrf_dm_on_monitor

        dtemp1 = 0.0

        ! Bitwise-exact reduction preserves operation order of serial code for
        ! testing, at the cost of much-increased run-time.  Turn it off when not
        ! testing.  This will always be .FALSE. for a serial run or 
        ! one-processor DM_PARALLEL run.
        IF ( testing_dm_bitwise_exact ) THEN

          ! Collect local cv arrays x and y to globally-sized serially-ordered 
          ! arrays xg and yg.  Note that xg and yg will only exist on the 
          ! monitor task.  
          cv_size_global = ( xp%ide-xp%ids+1 ) * ( xp%jde-xp%jds+1 ) * SUM(mzs)
          IF ( wrf_dm_on_monitor() ) THEN
            ALLOCATE( xg(cv_size_global) )
            ALLOCATE( yg(cv_size_global) )
          ENDIF
          CALL cv_to_global( x, n, xp, mzs, cv_size_global, xg )
          CALL cv_to_global( y, n, xp, mzs, cv_size_global, yg )

          IF ( wrf_dm_on_monitor() ) THEN
            ! da_dot now does serial computations only
            dtemp1 = da_dot( xg, yg, cv_size_global )
            DEALLOCATE( xg, yg )
          ENDIF

          ! Broadcast result from monitor to other tasks.  
          CALL wrf_dm_bcast_real( dtemp1, 1 )

        ELSE

          ! da_dot now does serial computations only
          dtemp1 = da_dot( x, y, n )

          IF ( .NOT. global ) THEN
             dtemp1x = dtemp1
             ! summation across processors
             dtemp1 = wrf_dm_sum_real( dtemp1x )
          ENDIF

        ENDIF

        da_dot_cv = dtemp1

      END FUNCTION da_dot_cv

