      module fftpack5

      use da_constants
!      use da_define_structures

!-----------------------------------------------------------------------
!  Contains all necessary routines to perform FFT                      
!-----------------------------------------------------------------------

      CONTAINS

#   include <rfft1b.inc>          
#   include <rfft1i.inc>          
#   include <rffti1.inc>          

#   include <xerfft.inc>          
#   include <rfftb1.inc>          

#   include <r1f2kb.inc>          
#   include <r1f3kb.inc>          
#   include <r1f4kb.inc>          
#   include <r1f5kb.inc>          
#   include <r1fgkb.inc>          

#   include <rfft1f.inc>          

#   include <rfftf1.inc>          

#   include <r1f2kf.inc>          
#   include <r1f3kf.inc>          
#   include <r1f4kf.inc>          
#   include <r1f5kf.inc>          
#   include <r1fgkf.inc>   
       
      end module fftpack5
