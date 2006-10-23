subroutine da_innerprod(mata,matc,ni,nj)

   implicit none

   integer, intent(in) :: ni, nj
   real, intent(in) :: mata(ni,nj)
   real, intent(out) :: matc(nj,nj)

   integer             :: i1, i2, k       ! Loop counters.

   matc = 0.

   do i1=1,nj
     do i2=1,nj
       do k=1,ni
          matc(i1,i2) = matc(i1,i2) + mata(k,i1)*mata(k,i2)
       end do
    end do
 end do

end subroutine da_innerprod

