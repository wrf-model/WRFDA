subroutine da_print_be_stats_p( outunit, ni, nj, nk, num_bins, num_bins2d, bin, bin2d, &
                                regcoeff1, regcoeff2, regcoeff3 )

   implicit none

   integer, intent(inout) :: outunit                      ! Output file unit.
   integer, intent(in) :: ni, nj, nk                      ! Grid dimensions. 
   integer, intent(in) :: num_bins                        ! Number of 3D field bins.
   integer, intent(in) :: num_bins2d                      ! Number of 2D field bins.
   integer, intent(in) :: bin(1:ni,1:nj,1:nk)             ! Bin assigned to each 3D point.
   integer, intent(in) :: bin2d(1:ni,1:nj)                ! Bin assigned to each 3D point.
   real, intent(in)    :: regcoeff1(1:num_bins)           ! psi/chi regression cooefficient.
   real, intent(in)    :: regcoeff2(1:nk,1:num_bins2d)    ! psi/ps regression cooefficient.
   real, intent(in)    :: regcoeff3(1:nk,1:nk,1:num_bins2d) ! psi/T regression cooefficient.

   integer             :: k, j, b                         ! Loop counters.

   write(6,'(a,i5)')' psi/chi regression coefficient in unit ', outunit
   write(6,'(a,i5)')' psi/ps  regression coefficient in unit ', outunit+1
   write(6,'(a,i5)')' psi/T   regression coefficient in unit ', outunit+2
   write(6,*)

   do k = 1, nk
      do j = 1, nj
         b = bin(1,j,k)
         write(outunit,'(1pe13.5)')regcoeff1(b)
         b = bin2d(1,j)
         write(outunit+1,'(1pe13.5)')regcoeff2(k,b)
         write(outunit+2,'(1pe13.5)')regcoeff3(k,k,b)
      end do
   end do

   outunit = outunit + 3

end subroutine da_print_be_stats_p

