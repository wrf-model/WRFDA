SUBROUTINE DA_Make_Filename (file_date, fcst_date, number, filename, index)

!-------------------------------------------------------------------------
!  PURPOSE: Generate Filename
!
!  HISTORY: 10/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   CHARACTER(len=19),  INTENT(INOUT) :: file_date
   CHARACTER(len=19),  INTENT(INOUT) :: fcst_date
   CHARACTER(len=120), INTENT(OUT)   :: filename
   INTEGER,            INTENT(IN)    :: number
   
   INTEGER                           :: ccyy, mm, dd, hh
   character (len=2)                 :: mem
   INTEGER,            INTENT(IN)    :: index
   integer                           :: advance_time

!--------------------------------------------------------------------------

       if (number.lt.10) write(mem,'(I1)') number
       if (number.ge.10.and.number.lt.100) write(mem,'(I2)') number

       if (index == 99) then 
       advance_time = file_interval
       else
          if (BACKGROUND_OPTION == 1) advance_time = t_forecast2
          if (BACKGROUND_OPTION == 2) advance_time = t_forecast1
       endif

       call DA_Gen_Date(file_date, fcst_date, advance_time)

       fcst_date(1:19) = fcst_date(1:13)//':00:00'

       if (index == 99) file_date = fcst_date

       filename =trim(directory_name)//&
       '/'//trim(file_head)//'_'//file_date(1:19)

       IF (BACKGROUND_OPTION == 2) &
       filename = trim(filename)//'.'//trim(mem)

END SUBROUTINE DA_Make_Filename	
