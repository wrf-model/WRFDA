subroutine da_matmulti(mata,matb,matc,ni,nj,nab)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in) :: ni, nj, nab
   real, intent(in) :: mata(ni,nab), matb(nab, nj)
   real, intent(out) :: matc(ni,nj)

   integer             :: i, j, k            ! Loop counters

   matc = 0.0

   do i=1,ni
      do j=1,nj 
         do k=1,nab
            matc(i,j) = matc(i,j) + mata(i,k)*matb(k,j)
         end do
      end do
   end do

end subroutine da_matmulti


