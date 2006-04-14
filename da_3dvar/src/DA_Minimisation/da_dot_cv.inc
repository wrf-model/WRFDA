real function da_dot_cv( cv_size, cv_size_domain, x, y, xp, mzs )

! Forms the dot product of two vectors that are organized in the 
! format of a "cv_type".  
!
! Capable of producing bitwise-exact results for distributed-memory 
! parallel runs for testing.  This feature is very slow and consumes 
! lots of memory. 
!

   implicit none

   integer, intent(in)          :: cv_size           ! Size of array (tile).
   integer, intent(in)          :: cv_size_domain    ! Size of array (domain).
   real, dimension(1:cv_size), intent(in) :: x       ! 1st vector.
   real, dimension(1:cv_size), intent(in) :: y       ! 1st vector.
        TYPE (xpose_type),  INTENT(IN) :: xp         ! decomposed dimensions
        INTEGER,            INTENT(IN) :: mzs(:)     ! mz for each variable
                                                     ! (to identify 2D arrays)

   real, pointer                :: xg(:), yg(:)      ! Serial data arrays.

   real                         :: dtemp1, dtemp1x   ! Temporary.
   logical, external            :: wrf_dm_on_monitor ! True if on monitor PE.

   dtemp1 = 0.0

   ! Bitwise-exact reduction preserves operation order of serial code for
   ! testing, at the cost of much-increased run-time.  Turn it off when not
   ! testing.  This will always be .FALSE. for a serial run or 
   ! one-processor DM_PARALLEL run.

   IF ( testing_dm_exact ) THEN

      ! Collect local cv arrays x and y to globally-sized serially-ordered 
      ! arrays xg and yg.  Note that xg and yg will only exist on the 
      ! monitor task.  

      IF ( wrf_dm_on_monitor() ) THEN
         allocate( xg(1:cv_size_domain) )
         allocate( yg(1:cv_size_domain) )
      ENDIF

      call cv_to_global( cv_size, cv_size_domain, x, xp, mzs, xg )
      call cv_to_global( cv_size, cv_size_domain, x, xp, mzs, yg )

      IF ( wrf_dm_on_monitor() ) THEN
         dtemp1 = da_dot( cv_size_domain, xg, yg )
         deallocate( xg, yg )
      ENDIF

!     Broadcast result from monitor to other tasks.  
      call wrf_dm_bcast_real( dtemp1, 1 )

   else

      dtemp1 = da_dot( cv_size, x, y )

      if ( .not. global ) then
         dtemp1x = dtemp1
!        summation across processors:
         dtemp1 = wrf_dm_sum_real( dtemp1x )
      end if

   end if

   da_dot_cv = dtemp1

end function da_dot_cv

