SUBROUTINE DA_Gen_Date (file_date1, file_date2, advance_time)

!-------------------------------------------------------------------------
!  PURPOSE: Generate Date
!
!  HISTORY: 10/01/2004                                         Mi-Seon Lee
!
! Input : file_date1 (character(1:19)) and advance_time (integer in hour)
! Output: file_date2 (character(1:19)).
!                                                  Yong-Run Guo 02/17/2005
!-------------------------------------------------------------------------

   CHARACTER(len=19), INTENT(IN)    :: file_date1
   CHARACTER(len=19), INTENT(OUT)   :: file_date2
   INTEGER,           INTENT(IN)    :: advance_time 

   INTEGER                          :: ccyy, mm, dd, hh

!--------------------------------------------------------------------------

       call DA_get_ymdh(file_date1, ccyy, mm, dd, hh)
       call DA_advance_cymdh0(ccyy, mm, dd, hh, advance_time)
       call DA_set_current_date(file_date2,ccyy,mm,dd,hh)

       file_date2(1:19) = file_date2(1:13)//':00:00'

END SUBROUTINE DA_Gen_Date
