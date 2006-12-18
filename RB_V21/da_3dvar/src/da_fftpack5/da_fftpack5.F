      module DA_FFTPACK5

      use da_constants
!      use da_define_structures

!-----------------------------------------------------------------------
!  Contains all necessary routines to perform FFT                      
!-----------------------------------------------------------------------

      CONTAINS

#   include <rfft1b.f>          
#   include <rfft1i.f>          
#   include <rffti1.f>          

#   include <xerfft.f>          
#   include <rfftb1.f>          

#   include <r1f2kb.f>          
#   include <r1f3kb.f>          
#   include <r1f4kb.f>          
#   include <r1f5kb.f>          
#   include <r1fgkb.f>          

#   include <rfft1f.f>          

#   include <rfftf1.f>          

#   include <r1f2kf.f>          
#   include <r1f3kf.f>          
#   include <r1f4kf.f>          
#   include <r1f5kf.f>          
#   include <r1fgkf.f>          
      end module da_fftpack5
