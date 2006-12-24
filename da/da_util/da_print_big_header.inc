subroutine da_print_big_header(bhi, bhr, bhic, bhrc)

   implicit none

   integer,           dimension(50,20), intent(in) :: bhi
   real,              dimension(20,20), intent(in) :: bhr
   character(len=80), dimension(50,20), intent(in) :: bhic
   character(len=80), dimension(20,20), intent(in) :: bhrc

   integer :: i, index

   index = bhi(1,1)

   do
      write(unit=*, fmt='(/a/)') '-----------integers-----------'

      do i=1,size(bhi,1)
         if (bhi(i,index) /= -999) then
            write(unit=*, fmt='(a,I3,a,I2,a,I9,2a)') &
                  'BHI(', i, ',', index, ')=', &
                   bhi(i,index),' : ',trim(bhic(i,index))
         end if
      end do

      write(unit=*, fmt='(/a/)') '------------Floats------------'

      do i=1,size(bhr,1)
         if (abs(bhr(i,index)+999.) > 0.1) then
            write(unit=*, fmt='(a,I3,a,I2,a,F9.2,2a)') &
                 'BHR(', i, ',', index, ')=', &
                  bhr(i,index),' : ',trim(bhrc(i,index))
         end if
      end do

      if (index < 11) exit

      index = index + 1

      if (index > 17) exit
   end do

end subroutine da_print_big_header


