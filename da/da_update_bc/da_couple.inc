subroutine da_couple (mut, field, msf, &
                    ids, ide, jds, jde, kds, kde)

   implicit none

   integer, intent(in) :: ids, ide, jds, jde, kds, kde

   real, dimension(ids:ide, jds:jde),          intent(in  ) :: mut, msf
  
   real, dimension(ids:ide, jds:jde, kds:kde), intent(inout) :: field
  
  
   integer :: i, j, k
  
   do j=jds,jde
      do k=kds,kde
         do i=ids,ide
            field(i,j,k)=field(i,j,k)*mut(i,j)/msf(i,j)
         end do
      end do
   end do

end subroutine da_couple
