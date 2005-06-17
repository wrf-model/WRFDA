Subroutine DA_Mss_Read(file_date)

!-------------------------------------------------------------------------
!  PURPOSE: Read Ensemble Forecast from MSS
!
!  HISTORY: 09/10/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

	IMPLICIT NONE
	CHARACTER(LEN=19) , INTENT(IN)  :: file_date
        CHARACTER(LEN=120)              :: cmd_line
	CHARACTER(len=6)                :: tar_date

!--------------------------------------------------------------

#ifndef crayx1
       tar_date=file_date(6:7)//file_date(9:10)//file_date(12:13)
       cmd_line = 'msrcp mss:'//trim(mss_directory)//'/' &
                   //file_head(1:6)//'_'//tar_date//'.tar '// &
                   trim(directory_name)//'/.'

       write(*,'(a90)') trim(cmd_line)
       call system(cmd_line)

       cmd_line = 'tar -xvf '//trim(directory_name)//'/' &
                   //file_head(1:6)//'_'//tar_date//'.tar '

       write(*,'(a90)') trim(cmd_line)

       call system(cmd_line)

       cmd_line = 'mv '//trim(file_head)//'* '//&
                  trim(directory_name)//'/.'

       write(*,'(a90)') trim(cmd_line)

       call system(cmd_line)
#endif

End Subroutine DA_Mss_Read
