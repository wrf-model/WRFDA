subroutine da_print_be_stats_v( outunit, variable, nk, num_bins2d, &
                                e_vec, e_val, e_vec_loc, e_val_loc )

   implicit none

   integer, intent(inout)   :: outunit                    ! Output file unit.
   character*10, intent(in) :: variable                   ! Variable name
   integer, intent(in)      :: nk                         ! Vertical dimension.
   integer, intent(in)      :: num_bins2d                 ! # bins for 2D fields.
   real, intent(in)         :: e_vec(1:nk,1:nk)           ! Domain-averaged eigenvectors.
   real, intent(in)         :: e_val(1:nk)                ! Domain-averaged eigenvalues.
   real, intent(in) :: e_vec_loc(1:nk,1:nk,1:num_bins2d)  ! Latitudinally varying eigenvectors.
   real, intent(in) :: e_val_loc(1:nk,1:num_bins2d)       ! Latitudinally varying eigenvalues.

   integer                  :: k, m, b                    ! Loop counters.

   write(6,'(3a,i5)')' First 5 Global eigenvectors for variable ', trim(variable), &
                     ' in unit ', outunit
   do k = 1, nk
      write(outunit,'(i4,5f15.8)')k, ( e_vec(k,m), m = 1, 5 )
   end do

   outunit = outunit + 1
   write(6,'(3a,i5)')' Global eigenvalues for variable ', trim(variable), &
                     ' in unit ', outunit

   do k = 1, nk
      write(outunit,'(i4,1pe18.5)')k, e_val(k)
   end do

   outunit = outunit + 1
   write(6,'(3a,i5)')' First 5 local eigenvalues for variable ', trim(variable), &
                     ' in unit ', outunit

   do b = 1, num_bins2d
      write(outunit,'(i4,5f20.3)')b, ( e_val_loc(m,b), m = 1, 5 )
   end do

   outunit = outunit + 1 
   write(6,*)

end subroutine da_print_be_stats_v

