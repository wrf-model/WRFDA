subroutine DA_get_ymdh(current_date,yy,mm,dd,hh)

!-------------------------------------------------------------------------
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   implicit none

   character(*), intent(in)  :: current_date
   integer,      intent(out) :: yy, mm, dd, hh

   read(current_date(1:4),'(I4)') yy
   read(current_date(6:7),'(I2)') mm
   read(current_date(9:10),'(I2)') dd
   read(current_date(12:13),'(I2)') hh

end subroutine DA_get_ymdh
