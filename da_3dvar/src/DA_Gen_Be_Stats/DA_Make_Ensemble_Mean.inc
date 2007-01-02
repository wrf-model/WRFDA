SUBROUTINE DA_Make_Ensemble_Mean (grid, config_flags, file_date, &
#include <em_dummy_args.inc>
                           )

!-------------------------------------------------------------------------
!  PURPOSE: Generate Ensemble Mean
!
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE(xbx_type)                        :: xbx  

   CHARACTER (LEN=19),  INTENT(INOUT)    :: file_date
!---
   TYPE(domain) ,  INTENT(IN)    :: grid
   TYPE (grid_config_rec_type)   :: config_flags
#include <em_dummy_decl.inc>

!---
   integer                               :: number, numfiles
   integer                               :: i,j,k
   real                                  :: factors
   character (len=120)                   :: filename
   character (len=19)                    :: fcst_date
   character (len=2)                     :: mem

   integer :: ccyy, mm, dd, hh

!-------------------------------------------------------------------------------

       write(*,'(a)') 'PROCESS of Ensemble Mean'

       numfiles = 0
  
       call DA_Zero_Ens (xb12)

       do number = 1, members 
       numfiles= numfiles+1

       call DA_Make_Filename (file_date, fcst_date, number, filename)
       call med_initialdata_input_3dvar ( grid , config_flags, &
                                        fcst_date, filename )
       call DA_Setup_FirstGuess( xbx, grid, &
#include <em_dummy_args.inc>
                           )
       call DA_Ens_Sum (xb, xb12)
 
       print*,'xb12 ',xb%u(10,10,10), xb12%u(10,10,10) 

       enddo

       factors = 1.0/float(numfiles)

       call DA_Ens_Magnify (xb12, factors)

END SUBROUTINE DA_Make_Ensemble_Mean
