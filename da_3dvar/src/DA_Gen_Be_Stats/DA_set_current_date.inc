subroutine DA_set_current_date(current_date,yy,mm,dd,hh)

!-------------------------------------------------------------------------
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   implicit none

   character(*), intent(out) :: current_date
   integer,      intent(in)  :: yy, mm, dd, hh

   write(current_date(1:4),'(I4.4)') yy
   write(current_date(6:7),'(I2.2)') mm
   write(current_date(9:10),'(I2.2)') dd
   write(current_date(12:13),'(I2.2)') hh

   current_date(5:5)='-'
   current_date(8:8)='-'
   current_date(11:11)='_'

end subroutine DA_set_current_date
