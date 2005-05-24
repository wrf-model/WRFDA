Subroutine DA_Data_Delete(file_date)

!-------------------------------------------------------------------------
!  PURPOSE: Clean Disk
!
!  HISTORY: 09/10/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

	IMPLICIT NONE
	CHARACTER(LEN=19), INTENT(IN)   :: file_date
        CHARACTER(LEN=120)              :: cmd_line

!--------------------------------------------------------------

#ifndef crayx1
       cmd_line = 'rm -rf '//trim(directory_name)//'/' &
                   //trim(file_head)//'_'//trim(file_date)//'*'

       write(*,'(a90)') trim(cmd_line)

       call system(cmd_line)

       cmd_line = 'rm -rf '//trim(directory_name)//'/' &
                   //'*.tar'

       write(*,'(a90)') trim(cmd_line)

       call system(cmd_line)
#endif

End Subroutine DA_Data_Delete
