subroutine da_find_fft_factors( n, n_ok, fft_factors )

   !---------------------------------------------------------------------------
   ! PURPOSE: Calculates prime factors of input number.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)        :: n
   logical, intent(out)       :: n_ok
   integer, intent(out)       :: fft_factors(:)

   integer                    :: i, k, l
   integer                    :: nfax, nu, ifac
   integer                    :: jfax(num_fft_factors)
   integer                    :: lfax(7)

   DATA lfax /6,8,5,4,3,2,1/

   !---------------------------------------------------------------------------
   ! [1.0] Find factors of vector size (8,6,5,4,3,2; ONLY ONE 8 ALLOWED):
   !---------------------------------------------------------------------------

   n_ok = .false.
   fft_factors(:) = 0

   ! LOOK FOR SIXES FIRST, STORE FACTORS in DESCendinG ORDER
   NU=N
   ifAC=6
   K=0
   L=1

20 continue

   if (MOD(NU,ifAC).NE.0) goto 30
   
   ! 6 is a factor:
   K=K+1
   JFAX(K)=ifAC
   if (ifAC.NE.8) goto 25
   if (K.EQ.1) goto 25
   JFAX(1)=8
   JFAX(K)=6

25 continue
   NU=NU/ifAC
   if (NU.EQ.1) goto 50
   if (ifAC.NE.8) goto 20

30 continue
   L=L+1
   ifAC=LFAX(L)
   if (ifAC .GT. 1) goto 20

   ! Illegal factors:
   ! write (unit=stderr,fmt='(a,i4,a)') 'N = ', N, ' contains illegal factors.'
   
   goto 9

   ! NOW REVERSE ORDER OF FACTORS
50 continue
   NFAX=K
   fft_factors(1)=NFAX
   do I=1,NFAX
      fft_factors(NFAX+2-I)=JFAX(I)
   end do
   
   n_ok = .true.
      
9  continue

end subroutine da_find_fft_factors
