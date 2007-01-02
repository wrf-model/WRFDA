Subroutine DA_Stats_Namelist

!-------------------------------------------------------------------------
!  PURPOSE: Read Namelist for Statistics
!
!  HISTORY: 08/15/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------
   implicit none

   namelist /filerecd/ directory_name,  file_head, &
                       bgn_date,        end_date, &
                       test_transforms, print_detail, &
                       from_mss,        mss_directory 

   namelist /timerecd/ t_forecast1, t_forecast2, file_interval

   namelist /analtype/ background_option, members

! ------ Default Values:                                                                                                
   TEST_TRANSFORMS = .TRUE.

   cv_options = 3
   FG_FORMAT = 1
   directory_name  = './'
   file_head       = 'wrfout_d1'
   bgn_date        = '9999-12-31_23:59:59.9999'
   end_date        = '0000-01-00_00:00:00.0000'

   t_forecast1     = 12
   t_forecast2     = 24

! ------ Get the control parameters from namelist file:                                                                 

   open(10, file='namelist.stats', form='formatted', status = 'OLD')
   read(10, nml = filerecd)
   read(10, nml = timerecd)
   read(10, nml = analtype)

   write(unit=*, fmt='(2a)') ' DIRECTORY =  ',directory_name
   write(unit=*, fmt='(2a)') ' FILE_HEAD =  ',file_head
   write(unit=*, fmt='(2a)') ' BGN_DATE  =  ',bgn_date
   write(unit=*, fmt='(2a)') ' END_DATE  =  ',end_date

   write(unit=*, fmt='(a,I4,a)') ' T_FORECAST1 = ',t_forecast1,' hour(s)'
   write(unit=*, fmt='(a,I4,a)') ' T_FORECAST2 = ',t_forecast2,' hour(s)'
   write(unit=*, fmt='(a,I4,a)') ' FILE_INTERVAL = ',file_interval,' hour(s)'

   write(unit=*, fmt='(a,i4)') ' BE_TYPE  = ', background_option
   if (background_option == 2) write (unit=*, fmt='(a,i2)') ' members  = ', members

   close(10)

End Subroutine DA_Stats_Namelist
