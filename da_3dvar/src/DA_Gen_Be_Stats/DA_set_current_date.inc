subroutine DA_set_current_date(current_date,yy,mm,dd,hh)

!-------------------------------------------------------------------------
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   implicit none

   character(*), intent(out) :: current_date
   integer,      intent(in)  :: yy, mm, dd, hh

   write(UNIT=current_date(1:4),  FMT='(I4.4)') yy
   write(UNIT=current_date(6:7),  FMT='(I2.2)') mm
   write(UNIT=current_date(9:10), FMT='(I2.2)') dd
   write(UNIT=current_date(12:13),FMT='(I2.2)') hh

   current_date(5:5)='-'
   current_date(8:8)='-'
   current_date(11:11)='_'

end subroutine DA_set_current_date
