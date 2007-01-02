SUBROUTINE DA_Statistics_Step0 (grid, config_flags,    &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte, &
#include <em_dummy_args.inc>
                 )
!-------------------------------------------------------------------------
!  PURPOSE: Write differences with some control variables.
!
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   IMPLICIT NONE
!----
   integer :: i, j, k
   integer :: numfiles
   real :: factors
!-----
   TYPE(domain) ,  INTENT(INOUT) :: grid
   TYPE (grid_config_rec_type)   :: config_flags
!--Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>
   TYPE(xbx_type)                  :: xbx         ! Header & non-gridded vars.

   CHARACTER(len=120)              :: filename
   CHARACTER(len=19)               :: fcst_date, file_date, file_date1

   INTEGER,      INTENT(IN)        :: ids,ide, jds,jde, kds,kde   ! domain dims.
   INTEGER,      INTENT(IN)        :: ims,ime, jms,jme, kms,kme   ! memory dims.
   INTEGER,      INTENT(IN)        :: its,ite, jts,jte, kts,kte   ! tile  dims.

   INTEGER                         :: unit, number
   character (len=2)               :: mem
   INTEGER                         :: ccyy, mm, dd, hh
   integer                         :: t_forecast
!-------------------------------------------------------------------------

       numfiles = 0
       file_date(1:19) = bgn_date(1:19)

loop_date:       do while (file_date(1:19) <= end_date(1:19))

       t_forecast = t_forecast2
       if ( background_option == 2 ) t_forecast = t_forecast1

       call DA_Gen_Date(file_date, fcst_date, t_forecast)

       IF ( BACKGROUND_OPTION == 1 .and. &
           file_date(1:19) == end_date(1:19) ) EXIT loop_date
       write(*,'(a,2a20)') 'do while ',file_date(1:19),end_date(1:19)

       if (from_mss) call DA_Mss_Read (file_date)

       numfiles = numfiles + 1

       if (BACKGROUND_OPTION == 2) &
       call DA_Make_Ensemble_Mean (grid, config_flags, file_date, &
#include <em_dummy_args.inc>
                           )

!------T24 READ
       do number = 1, members

       write(*,'(/a,i4," *********")') 'PROCESS for T24, member=', number

       call DA_Make_Filename (file_date, fcst_date, number, filename)
print *, file_date, fcst_date, filename
       call med_initialdata_input_3dvar ( grid , config_flags, &
                                        fcst_date, filename )
       call DA_Setup_FirstGuess( xbx, grid, &
#include <em_dummy_args.inc>
                           )
       call DA_Assign_Value (xb, xb24) 
       write(*,'(3(a,e20.12))')   &
          'xb24: xb%u(10,10,10)=',xb%u(10,10,10), &
              '  xb%t(10,10,10)=',xb%t(10,10,10), &
           '  xb%Psfc(10,10)   =',xb%psfc(10,10)

!------T12 READ

       if (BACKGROUND_OPTION == 1) then

       write(*,'(/a)') 'PROCESS for T12'

       call DA_Gen_Date(file_date, file_date1, t_forecast1)
       call DA_Make_Filename(file_date1, fcst_date, number, filename)
print *, file_date1, fcst_date, filename

       call med_initialdata_input_3dvar ( grid , config_flags, &
                                        fcst_date, filename )
       call DA_Setup_FirstGuess( xbx, grid, &
#include <em_dummy_args.inc>
                           )

       call DA_Assign_value (xb, xb12)

       endif
      
       call DA_Difference (diff, xb24, xb12,its,ite, jts,jte, kts,kte)
!       write(*,'(3(a,E20.12))')   &
!          'diff: diff%u(10,10,10)=',diff%u(10,10,10), &
!              '  diff%t(10,10,10)=',diff%t(10,10,10), &
!           '  diff%Psfc(10,10)   =',diff%psfc(10,10)
       call DA_New_Statistics_Variable (diff, xb, xbx, xp, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
!       write(*,'(2(a,e20.12))')   &
!          'diff: diff%psi(10,10,10)=',diff%psi(10,10,10), &
!              '  diff%chi(10,10,10)=',diff%chi(10,10,10)
       unit=51
       call DA_Write_Diff (unit, file_date, number, diff, xp, &
                           ids,ide, jds,jde, kds,kde,  &
                           ims,ime, jms,jme, kms,kme,  &
                           its,ite, jts,jte, kts,kte, xb12 )

       IF (TEST_TRANSFORMS.and.number == 1) then
       CALL DA_Test_Transforms (diff, xb, xbx, xp,  &
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )
       stop
       ENDIF

123    CONTINUE  

!member-loop end
       enddo

       if (from_mss) call DA_Data_Delete(file_date)


       call DA_Gen_Date(file_date, fcst_date, file_interval)
       file_date(1:19) = fcst_date(1:19)

!time-loop end
       enddo loop_date

       factors= 1./float(numfiles*members)
       print*,'numfiles ',numfiles,members, factors

END SUBROUTINE DA_Statistics_Step0
