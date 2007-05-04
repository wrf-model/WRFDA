subroutine da_find_fft_trig_funcs(n, trig_functs)

   !---------------------------------------------------------------------------
   ! Purpose: Set up constants required for Fourier, sine and cosine transforms
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)        :: n
   real, intent(out)          :: trig_functs(:)

   integer                    :: k, nil, nhl
   real                       :: del, angle

   !---------------------------------------------------------------------------
   ! [1.0] Trig functions for real periodic transform:
   !---------------------------------------------------------------------------

   trig_functs(:) = 0.0

   DEL=4.0*(pi/2.0)/FLOAT(N)
   NIL=0
   NHL=(N/2)-1

   do K=NIL,NHL
      ANGLE=FLOAT(K)*DEL
      trig_functs(2*K+1)=COS(ANGLE)
      trig_functs(2*K+2)=Sin(ANGLE)
   end do

   ! [1.1] Extra trig functions for cosine transform:

   DEL=0.5*DEL
   do K=1,NHL
      ANGLE=FLOAT(K)*DEL
      trig_functs(2*N+K)=Sin(ANGLE)
   end do
  
   ! [1.2] Extra trig functions for shifted cosine transform:

   DEL=0.5*DEL
   do K=1,N
      ANGLE=FLOAT(K)*DEL
      trig_functs(N+K)=Sin(ANGLE)
   end do

end subroutine da_find_fft_trig_funcs


