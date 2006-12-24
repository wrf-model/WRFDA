subroutine da_solve_poissoneqn_fst(xbx, xp, del2b, b,          &
                                    ide, jde,      &
                                    ims,ime, jms,jme, kms,kme,      &
                                    its,ite, jts,jte, kts,kte)

   !--------------------------------------------------------------------------
   !  Purpose: Solve Del**2 B = A for B with zero field boundary conditions.
   !
   !  METHOD:  1) Compute spectral del2b using double forward FST.
   !           2) Calculate spectral b.
   !           3) Reform gridpt. b using inverse double FST.
   !           Note no mean removed (would make b.ne.0 at boundaries) so
   !           arbitrary constant still present.
   !--------------------------------------------------------------------------

   implicit none

   type (xbx_type),intent(in) :: xbx           ! Header & non-gridded vars.
   type (xpose_type),intent(inout)  :: xp      ! Dimensions and xpose buffers. 
   integer   , intent(in)     :: ide, jde   ! domain dims.
   integer   , intent(in)     :: ims,ime, jms,jme, kms,kme   ! memory dims.
   integer   , intent(in)     :: its,ite, jts,jte, kts,kte   ! tile   dims.
   real, intent(in)           :: del2b(ims:ime,jms:jme,kms:kme)  ! Del**2 B.
   real, intent(out)          :: b(ims:ime,jms:jme,kms:kme)  ! B.
   
   integer                  :: vector_inc    ! Increment between FST data.
   integer                  :: vector_jump   ! Jump between start of vectors.
   integer                  :: vector_size   ! Of form 2**p 3**q 5**r for FSTs.
   integer                  :: num_vectors   ! Number of FSTs to perform.
   integer                  :: work_area     ! Dimension for FST routine.
   integer                  :: idim          ! Size of 1st dimension for FST.
   integer                  :: jdim          ! Size of 2nd dimension for FST.

   integer                  :: i, j, k, n, ij     ! loop counter

   real, allocatable, dimension(:) :: work_1d     ! FFT work array

   !------------------------------------------------------------------------------
   ! [1.0] Initialise:
   !------------------------------------------------------------------------------

   ! Calculate work space needed.

   n = max(xbx%fft_ix*(xp%jtex-xp%jtsx+1), &
           xbx%fft_jy*(xp%itey-xp%itsy+1+xbx%pad_num))

   ! Allocate work arrays.
   allocate(work_1d(1:n))

   ! Copy del2b for transpose.

   xp%v1z(its:ite,jts:jte,kts:kte) = del2b(its:ite,jts:jte,kts:kte)

   if (ite == ide) xp%v1z(ite,jts:jte,kts:kte) = 0.0
   if (jte == jde) xp%v1z(its:ite,jte,kts:kte) = 0.0

   !---------------------------------------------------------------------------
   ! [2.0] Perform forward FFT in x direction:
   !---------------------------------------------------------------------------

   ! [2.1] Apply (i',j',k -> i,j',k') transpose (v1z -> v1x).

   call da_transpose_z2x (xp)

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

      call fft661(Forward_FFT, vector_inc, vector_jump, &
                                   num_vectors, vector_size, &
                                   xbx % fft_factors_x, xbx % trig_functs_x, &
                                   work_1d(1), work_area)
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
   !        Poisson equation, and then perform inverse FFT in y direction:
   !---------------------------------------------------------------------------

   ! [3.1] Apply (i,j',k' -> i',j,k') transpose (v1x -> v1y).

   call da_transpose_x2y (xp)
   call da_transpose_x2y_v2 (xp)

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

      call fft661(Forward_FFT, vector_inc, vector_jump, &
                                   num_vectors, vector_size, &
                                   xbx % fft_factors_y, xbx % trig_functs_y, &
                                   work_1d(1), work_area)

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

      call fft661(Inverse_FFT, vector_inc, vector_jump, &
                                   num_vectors, vector_size, &
                                   xbx % fft_factors_y, xbx % trig_functs_y, &
                                   work_1d(1), work_area)
                          
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

   ! Apply (i',j,k' -> i,j',k') transpose (v1y -> v1x).

   call da_transpose_y2x (xp)
   call da_transpose_y2x_v2 (xp)

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

      call fft661(Inverse_FFT, vector_inc, vector_jump, &
                                   num_vectors, vector_size, &
                                   xbx % fft_factors_x, xbx % trig_functs_x, &
                                   work_1d(1), work_area)
      ij = 0
      do j=xp%jtsx, xp%jtex
         do i=xp%ids, xp%ide
            ij=ij+1
            xp%v1x(i,j,k) = work_1d(ij)
         end do

         ij=ij+xbx%fft_pad_i
      end do
   end do

   ! Apply (i,j',k') -> i',j',k) transpose to restore v1z.

   call da_transpose_x2z (xp)

   !---------------------------------------------------------------------------
   ! [5.0] Tidy up:
   !---------------------------------------------------------------------------

   if (allocated(work_1d)) deallocate(work_1d)

   ! [2.5] Write data array into b:

   b(its:ite,jts:jte,kts:kte) = xp%v1z(its:ite,jts:jte,kts:kte)

end subroutine da_solve_poissoneqn_fst


