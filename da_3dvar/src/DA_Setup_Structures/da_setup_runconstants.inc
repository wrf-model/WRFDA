SUBROUTINE DA_Setup_RunConstants( xp, xbx, ds )

!------------------------------------------------------------------------------
!  PURPOSE: Define constants used.
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!           10/19/2001 - Mods for parallel implementation.  Al Bourgeois
!           09/26/2006 - Mods for parallel implementation to WRF.  Wei Huang
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   type (xpose_type), intent(in)    :: xp       ! Dimensions and xpose buffers.
   type (xbx_type),   intent(inout) :: xbx      ! Header & non-gridded vars.
   real,              intent(in)    :: ds       ! Model grid distance

   INTEGER, PARAMETER            :: nrange = 50 ! Range to search for efficient FFT.
   INTEGER                       :: n           ! Loop counter.

   INTEGER                       :: fft_size_i  ! Efficient FFT 1st dimension.
   INTEGER                       :: fft_size_j  ! Efficient FFT 2nd dimension.
   LOGICAL                       :: found_magic ! TRUE when efficient FFT found
   LOGICAL                       :: need_pad    ! True when need pad_i > 0.

   INTEGER                  :: i, j
   INTEGER                  :: nx            ! nx + 1 = ix + pad_i
   INTEGER                  :: ny            ! ny + 1 = jy + pad_j
   REAL                     :: const         ! Multiplicative constants.
   REAL                     :: coeff_nx      ! Multiplicative coefficients.
   REAL                     :: coeff_ny      ! Multiplicative coefficients.
   REAL                     :: cos_coeff_nx  ! Multiplicative coefficients.
   REAL                     :: cos_coeff_ny  ! Multiplicative coefficients.
   
!------------------------------------------------------------------------------
!  [1.0] Calculate padded grid-size required for efficient FFTs and factors:
!------------------------------------------------------------------------------

   IF ( cv_options == 2 ) THEN

!     [1.1] In x direction:

      nx = xp%ide - xp%ids

      ALLOCATE( xbx % fft_factors_x(num_fft_factors) )

      DO n = nx, nx+nrange
         CALL DA_Find_FFT_Factors(n, found_magic, xbx % fft_factors_x )

         IF ( found_magic ) THEN
            IF ( mod(n, 2) == 0 ) THEN
               fft_size_i = n
               xbx % fft_pad_i = n - nx
               EXIT
            END IF
         END IF
      END DO
      
      IF ( .NOT. found_magic ) THEN
         WRITE(0,'(A)')' DA_Setup_RunConstants: No factor found. Stop'
         STOP
      END IF
      
      ALLOCATE( xbx % trig_functs_x(1:3*fft_size_i) )  

      CALL DA_Find_FFT_Trig_Funcs( fft_size_i, xbx % trig_functs_x(:) )

!     [1.2] In y direction:

      ny = xp%jde - xp%jds
      
      ALLOCATE( xbx % fft_factors_y(num_fft_factors) )

      DO n = ny, ny+nrange
         CALL DA_Find_FFT_Factors(n, found_magic, xbx % fft_factors_y )

         IF ( found_magic ) THEN
            IF ( mod(n, 2) == 0 ) THEN
               fft_size_j = n
               xbx % fft_pad_j = n - ny
               EXIT
            END IF
         END IF
      END DO
      
      IF ( .NOT. found_magic ) THEN
         WRITE(0,'(A)')' DA_Setup_RunConstants: No factor found. Stop'
         STOP
      END IF
      
      ALLOCATE( xbx % trig_functs_y(1:3*fft_size_j) )  

      CALL DA_Find_FFT_Trig_Funcs( fft_size_j, xbx % trig_functs_y(:) )

!-----Multiplicative coefficent for solution of spectral Poission eqn:

      const = -0.5 * ds * ds

      nx = xp%ide - xp%ids + xbx%fft_pad_i
      ny = xp%jde - xp%jds + xbx%fft_pad_j

      coeff_nx = pi / REAL( nx )
      coeff_ny = pi / REAL( ny )

      xbx%fft_ix = nx + 1
      xbx%fft_jy = ny + 1

      ALLOCATE( xbx%fft_coeffs(1:xbx%fft_ix,1:xbx%fft_jy) )

      DO j = 2, ny
         cos_coeff_ny = COS(coeff_ny * REAL(j-1))
         DO i = 2, nx
            cos_coeff_nx = COS(coeff_nx * REAL(i-1))
            xbx%fft_coeffs(i,j) = const / ( 1.0 - cos_coeff_nx * cos_coeff_ny)
         END DO
      END DO

!  Set to zero all coefficients which are multiplied by sin(0.0) in FST:

      xbx%fft_coeffs(1,1:xbx%fft_jy)  = 0.0
      xbx%fft_coeffs(xbx%fft_ix,1:xbx%fft_jy) = 0.0
      xbx%fft_coeffs(1:xbx%fft_ix,1)  = 0.0
      xbx%fft_coeffs(1:xbx%fft_ix,xbx%fft_jy) = 0.0
   
      xbx%fft_adjoint_factor = 4.0 / REAL( nx * ny )

!------------------------------------------------------------------------------
      
!-----Calculate i increment for distributing pad region across processors.

      need_pad = .TRUE.
      if(xbx%fft_pad_i <= 0) then
         need_pad = .FALSE.
      else if(xbx%fft_pad_i > (xp%ide-xp%ids+1)) then
        write(unit=*, fmt='(2x,a,i4)') &
             '(xp%ide-xp%ids+1) = ', (xp%ide-xp%ids+1), &
             'xbx%fft_pad_i = ', xbx%fft_pad_i
        write(wrf_err_message, fmt='(a)') &
             'WRF3DVAR ERROR in da_setup_runconstants.inc.'
        CALL WRF_ERROR_FATAL ( wrf_err_message )

        write(wrf_err_message, fmt='(a)') &
             'FFT xbx%fft_pad_i is too large!'
        CALL WRF_ERROR_FATAL ( wrf_err_message )
      else
         xbx%pad_inc = (xp%ide-xp%ids+1)/xbx%fft_pad_i
      endif

!-----Calculate number of pad vectors in x to be done on this processor.
!-----Need to save xbx%pad_num, xbx%pad_inc, and xbx%pad_loc

      xbx%pad_num = 0
      if(need_pad) then
         do n=1, xbx%fft_pad_i
            i = (n-1)*xbx%pad_inc + 1
            if((i >= xp%itsy) .and. (i <= xp%itey)) then
               xbx%pad_num = xbx%pad_num + 1
            endif
         enddo

         if(xbx%pad_num > 0) then
            allocate(xbx%pad_loc(1:xbx%pad_num))
            allocate(xbx%pad_pos(1:xbx%pad_num))
         endif

         xbx%pad_num = 0
         do n=1, xbx%fft_pad_i
            i = (n-1)*xbx%pad_inc + 1
            if((i >= xp%itsy) .and. (i <= xp%itey)) then
               xbx%pad_num = xbx%pad_num + 1
               xbx%pad_loc(xbx%pad_num) = i
               xbx%pad_pos(xbx%pad_num) = xp%ide + n
            endif
         enddo
      endif

!------------------------------------------------------------------------------

   END IF
   
END SUBROUTINE DA_Setup_RunConstants

