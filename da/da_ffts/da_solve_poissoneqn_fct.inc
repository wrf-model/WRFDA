subroutine da_solve_poissoneqn_fct(xbx, del2b, b, xp)

   !---------------------------------------------------------------------------
   ! PURPOSE: Solve Del**2 B = A for B with zero gradient boundary conditions.
   !
   ! METHOD:  1) Compute spectral del2b using double forward FCT.
   !          2) Calculate spectral b.
   !          3) Reform gridpt. b using inverse double FCT.
   !          4) Remove mean b (arbitrary constant).
   !--------------------------------------------------------------------------

   implicit none
   
   type (xbx_type),   intent(in)    :: xbx     ! Header & non-gridded vars.
   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                      intent(in)    :: del2b   ! Del**2 B.
   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme), &
                      intent(out)   :: b       ! B


   integer                  :: vector_inc    ! Increment between FST data.
   integer                  :: vector_jump   ! Jump between start of vectors.
   integer                  :: vector_size   ! Of form 2**p 3**q 5**r for FSTs.
   integer                  :: num_vectors   ! Number of FSTs to perform.
   integer                  :: work_area     ! Dimension for FST routine.
   integer                  :: idim          ! Size of 1st dimension for FST.
   integer                  :: jdim          ! Size of 2nd dimension for FST.

   integer                  :: i, j, k, n, ij     ! loop counter

   real, allocatable, dimension(:) :: work_1d     ! FFT work array

   real, dimension(xp%kts:xp%kte) :: global_mean, local_mean

   real :: rij

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------

   ! Calculate work space needed.

   n = max(xbx%fft_ix*(xp%jtex-xp%jtsx+1), &
           xbx%fft_jy*(xp%itey-xp%itsy+1+xbx%pad_num))

   ! Allocate work arrays.
   allocate( work_1d(1:n) )

   ! Copy del2b for transpose.

   xp%v1z(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte) = &
    del2b(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte)

   ! if(xp%ite == xp%ide) xp%v1z(xp%ite,xp%jts:xp%jte,xp%kts:xp%kte) = 0.0
   ! if(xp%jte == xp%jde) xp%v1z(xp%its:xp%ite,xp%jte,xp%kts:xp%kte) = 0.0

   !---------------------------------------------------------------------------
   ! [2.0] Perform calculation of gridpoint b level by level:
   !---------------------------------------------------------------------------

   ! [2.1] Apply (i',j',k -> i,j',k') transpose (v1z -> v1x).

   call da_transpose_z2x ( xp )

   ! [2.2] Set up FFT parameters:

   idim = xbx%fft_ix
   jdim = xp%jtex-xp%jtsx+1

   vector_inc  = 1
   vector_jump = idim
   vector_size = idim - 1

   num_vectors = jdim

   work_area   = (vector_size+1)*num_vectors


   ! [2.3] Perform forward FFT:

   do k = xp%ktsx, xp%ktex
      ij = 0
      do j=xp%jtsx, xp%jtex
         do i=xp%ids, xp%ide
            ij=ij+1
            work_1d(ij) = xp%v1x(i,j,k)
         end do

         do i=1, xbx%fft_pad_i
            ij=ij+1
            work_1d(ij) = 0.0
         end do
      end do
      call fft551( Forward_FFT, vector_inc, vector_jump, &
                                     num_vectors, vector_size, &
                                     xbx%fft_factors_x, xbx%trig_functs_x, &
                                     work_1d(1), work_area )
      
      ij = 0
      do j=xp%jtsx, xp%jtex
         do i=xp%ids, xp%ide
            ij=ij+1
            xp%v1x(i,j,k) = work_1d(ij)
         end do

         do n=1, xbx%fft_pad_i
            i=(n-1)*xbx%pad_inc + 1
            ij=ij+1
            xp%v2x(i,j,k) = work_1d(ij)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [3.0] For each k-level, perform forward FFT in y direction, apply spectral
   !       Poisson equation, and then perform inverse FFT in y direction:
   !---------------------------------------------------------------------------

   ! [3.1] Apply (i,j',k' -> i',j,k') transpose ( v1x -> v1y ).

   call da_transpose_x2y ( xp )
   call da_transpose_x2y_v2 ( xp )

   ! [3.2] Set up FFT parameters:

   idim = xp%itey - xp%itsy + 1 + xbx%pad_num
   jdim = xbx%fft_jy

   vector_inc  = idim
   vector_jump = 1
   vector_size = jdim - 1
   num_vectors = idim

   work_area   = (vector_size+1)*num_vectors


   ! [2.3] Perform forward FFT in j:

   do k = xp%ktsy, xp%ktey
      ij = 0
      do j=xp%jds, xp%jde
         do i=xp%itsy, xp%itey
            ij=ij+1
            work_1d(ij) = xp%v1y(i,j,k)
         end do

         do n=1, xbx%pad_num
            i=xbx%pad_loc(n)
            ij=ij+1
            work_1d(ij) = xp%v2y(i,j,k)
         end do
      end do

      do j=1, xbx%fft_pad_j
         do i=xp%itsy, xp%itey+xbx%pad_num
            ij=ij+1
            work_1d(ij) = 0.0
         end do
      end do

      call fft551( Forward_FFT, vector_inc, vector_jump, &
                                   num_vectors, vector_size, &
                                   xbx % fft_factors_y, xbx % trig_functs_y, &
                                   work_1d(1), work_area )

      !------------------------------------------------------------------------
      ! [4.0] Solve spectral Poisson equation:
      !------------------------------------------------------------------------

      ij = 0
      do j=xp%jds, xbx%fft_jy
         do i=xp%itsy, xp%itey
            ij=ij+1
            work_1d(ij) = xbx%fft_coeffs(i,j)*work_1d(ij)
         end do
         do n=1, xbx%pad_num
            i=xbx%pad_pos(n)
            ij=ij+1
            work_1d(ij) = xbx%fft_coeffs(i,j)*work_1d(ij)
         end do
      end do

      ! [2.3] Reform gridpt. b using inverse double FST in i.

      call fft551( Inverse_FFT, vector_inc, vector_jump, &
                                     num_vectors, vector_size, &
                                   xbx % fft_factors_y, xbx % trig_functs_y, &
                                   work_1d(1), work_area )

      ij = 0
      do j=xp%jds, xp%jde
         do i=xp%itsy, xp%itey
            ij=ij+1
            xp%v1y(i,j,k) = work_1d(ij)
         end do

         do n=1, xbx%pad_num
            i=xbx%pad_loc(n)
            ij=ij+1
            xp%v2y(i,j,k) = work_1d(ij)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! Perform inverse FFT in x direction:
   !---------------------------------------------------------------------------

   ! Apply (i',j,k' -> i,j',k') transpose ( v1y -> v1x ).

   call da_transpose_y2x ( xp )
   call da_transpose_y2x_v2 ( xp )

   ! Set up FFT parameters:

   idim = xbx%fft_ix
   jdim = xp%jtex-xp%jtsx+1

   vector_inc  = 1
   vector_jump = idim
   vector_size = idim - 1

   num_vectors = jdim

   work_area   = (vector_size+1)*num_vectors

   do k = xp%ktsx, xp%ktex
      ij = 0
      do j=xp%jtsx, xp%jtex
         do i=xp%ids, xp%ide
            ij=ij+1
            work_1d(ij) = xp%v1x(i,j,k)
         end do

         do n=1, xbx%fft_pad_i
            i=(n-1)*xbx%pad_inc + 1
            ij=ij+1
            work_1d(ij) = xp%v2x(i,j,k)
         end do
      end do

      call fft551( Inverse_FFT, vector_inc, vector_jump, &
                                     num_vectors, vector_size, &
                                     xbx % fft_factors_x, xbx % trig_functs_x, &
                                     work_1d(1), work_area )
      ij = 0
      do j=xp%jtsx, xp%jtex
         do i=xp%ids, xp%ide
            ij=ij+1
            xp%v1x(i,j,k) = work_1d(ij)
         end do

         ij=ij+xbx%fft_pad_i
      end do
   end do

   ! Apply (i,j',k') -> i',j',k ) transpose to restore v1z.

   call da_transpose_x2z ( xp )

   ! Remove mean b (set arbitrary constant to zero):

   rij = 1.0/real((xp%ide-xp%ids+1)*(xp%jde-xp%jds+1))

   do k=xp%kts, xp%kte
      local_mean(k) = sum(xp%v1z(xp%its:xp%ite,xp%jts:xp%jte,k))*rij
   end do

   call wrf_dm_sum_reals ( local_mean, global_mean )

   do k=xp%kts,xp%kte
      PRinT *,'TEST_COVERAGE_DA_Solve_PoissonEqn_FCT:  global_mean(', &
         k,') = ', global_mean(k)
   end do

   ! [2.5] Write data array into b:

   do k=xp%kts, xp%kte
      b(xp%its:xp%ite,xp%jts:xp%jte,k) = &
         xp%v1z(xp%its:xp%ite,xp%jts:xp%jte,k) - global_mean(k)
   end do

   !---------------------------------------------------------------------------
   ! [5.0] Tidy up:
   !---------------------------------------------------------------------------

   if (allocated(work_1d)) deallocate(work_1d)

end subroutine da_solve_poissoneqn_fct


