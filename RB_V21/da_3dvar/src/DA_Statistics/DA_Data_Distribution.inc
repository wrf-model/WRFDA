SUBROUTINE DA_Data_Distribution( ob_name, num_obs, min_val, max_val, &
                                 bin_width, ob )
   
!---- -------------------------------------------------------------------------
!  PURPOSE: Bin ob data to get distribution.
!
!  METHOD:  Straightforward.
!
!  HISTORY: 10/16/2000 - Creation of F90 version.           Dale Barker
!
!  PARENT MODULE: DA_Statistics
!------------------------------------------------------------------------------
   
   CHARACTER (LEN=*), INTENT(IN)    :: ob_name       ! Data description.
   INTEGER, INTENT(IN)              :: num_obs       ! Number of obs.
   REAL, INTENT(IN)                 :: min_val       ! Minimum bin value.
   REAL, INTENT(IN)                 :: max_val       ! Maximum bin value.
   REAL, INTENT(IN)                 :: bin_width     ! Bin width.
   REAL, INTENT(IN)                 :: ob(:)         ! Ob data.
   
   INTEGER                          :: num_bins      ! Number of bins
   INTEGER                          :: bin           ! Bin counter.
   INTEGER                          :: n             ! Data counter.
   INTEGER                          :: num_missing   ! Number of missing data.
   
   REAL, ALLOCATABLE                :: bin_val(:)    ! Central value of bin.
   REAL, ALLOCATABLE                :: bin_min(:)    ! Minimum value of bin.
   INTEGER, ALLOCATABLE             :: num_in_bin(:) ! Number of values in bin.   
   
!------------------------------------------------------------------------------
!  [1.0] Initialise bins:
!------------------------------------------------------------------------------

   num_bins = INT( ( max_val - min_val ) / bin_width ) + 1

   ALLOCATE( bin_val(1:num_bins) )
   bin_val(1) = min_val
   DO bin = 2, num_bins
      bin_val(bin) = bin_val(bin-1) + bin_width
   END DO
   
   ALLOCATE( bin_min(1:num_bins+1) )
   bin_min(1:num_bins) = bin_val(1:num_bins) - 0.5 * bin_width
   bin_min(num_bins+1) = bin_val(num_bins) + 0.5 * bin_width

   ALLOCATE( num_in_bin(0:num_bins+1) )
   num_in_bin(0:num_bins+1) = 0
   num_missing = 0
   
!------------------------------------------------------------------------------
!  [2.0] Assign data to bins:
!------------------------------------------------------------------------------
   
   DO n = 1, num_obs

      IF ( ob(n) == missing_r ) THEN
         num_missing = num_missing + 1
      ELSE IF ( ob(n) < bin_min(1) .AND. ob(n) /= missing_r ) THEN
         num_in_bin(0) = num_in_bin(0) + 1
      ELSE IF ( ob(n) >= bin_min(num_bins+1) ) THEN
         num_in_bin(num_bins+1) = num_in_bin(num_bins+1) + 1
      ELSE
         DO bin = 1, num_bins
            IF ( ob(n) >= bin_min(bin) .AND. ob(n) < bin_min(bin+1) ) THEN
                 num_in_bin(bin) = num_in_bin(bin) + 1
               EXIT
            END IF
         END DO
      END IF

   END DO

!------------------------------------------------------------------------------
!  [3.0] Output statistics:
!------------------------------------------------------------------------------
   
   WRITE(0,'(A,A,A,I8)')' Number of ', TRIM( ob_name ), ' obs = ', num_obs
   WRITE(0,'(A,I8)')' Number with missing data indicator = ',  num_missing
   WRITE(0,'(A,f12.5,A,I8)')' Number below minimum O-B(', &
                            min_val-0.5*bin_width, ') = ', num_in_bin(0)
   DO bin = 1, num_bins
      WRITE(0,'(I4,f12.5,I8)')bin, bin_val(bin), num_in_bin(bin)
   END DO
   WRITE(0,'(A,f12.5,A,I8)')' Number above maximum O-B(', &
                            max_val+0.5*bin_width, ') = ', &
                            num_in_bin(num_bins+1)
                               
!------------------------------------------------------------------------------
!  [4.0] Tidy up:
!------------------------------------------------------------------------------

   DEALLOCATE( bin_val )
   DEALLOCATE( bin_min )
   DEALLOCATE( num_in_bin )

END SUBROUTINE DA_Data_Distribution
