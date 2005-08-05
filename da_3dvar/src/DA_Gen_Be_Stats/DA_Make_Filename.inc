SUBROUTINE DA_Make_Filename (file_date, fcst_date, number, filename)

!-------------------------------------------------------------------------
!  PURPOSE: Generate Filename
!
!  HISTORY: 10/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   CHARACTER(len=19),  INTENT(IN) :: file_date
   CHARACTER(len=19),  INTENT(IN) :: fcst_date
   CHARACTER(len=120), INTENT(OUT)   :: filename
   INTEGER,            INTENT(IN)    :: number
   
   INTEGER                           :: ccyy, mm, dd, hh
   character (len=10)                :: cdate
   character (len=2)                 :: mem
   integer                           :: advance_time

!--------------------------------------------------------------------------

       if (number.lt.10) write(mem,'(I1)') number
       if (number.ge.10.and.number.lt.100) write(mem,'(I2)') number

       cdate(1:10) = file_date(1:4)//file_date(6:7)//file_date(9:10)//file_date(12:13)
       filename =trim(directory_name)//&
       '/'//cdate(1:10)//&
       '/'//trim(file_head)//'_'//fcst_date(1:19)

       IF (BACKGROUND_OPTION == 2) &
       filename = trim(filename)//'.'//trim(mem)

END SUBROUTINE DA_Make_Filename	
