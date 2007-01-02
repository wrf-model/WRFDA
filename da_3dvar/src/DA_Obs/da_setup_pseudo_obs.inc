SUBROUTINE da_setup_pseudo_obs( iv, ob )

!-------------------------------------------------------------------------
!  Purpose: Sets up pseudo ob part of observation structure.
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(INOUT) :: iv   ! Obs and header structure.
   TYPE (y_type), INTENT(INOUT)  :: ob   ! (Smaller) observation structure.

   integer                       :: n    ! Loop counters.


   write(unit=*, fmt='(a, i8)') &
        'num_pseudo=', num_pseudo

   iv % num_pseudo = num_pseudo
   ob % num_pseudo = num_pseudo

   if ( iv % num_pseudo > 0 ) then

      allocate (iv % pseudo(1:ob % num_pseudo)) 
    
      do n=1, iv % num_pseudo

         iv % pseudo(n) % u % inv = missing_r
         iv % pseudo(n) % v % inv = missing_r
         iv % pseudo(n) % t % inv = missing_r
         iv % pseudo(n) % p % inv = missing_r
         iv % pseudo(n) % q % inv = missing_r
         
         iv % pseudo(n) % u % error = missing_r
         iv % pseudo(n) % v % error = missing_r
         iv % pseudo(n) % t % error = missing_r
         iv % pseudo(n) % p % error = missing_r
         iv % pseudo(n) % q % error = missing_r
         
         iv % pseudo(n) % u % qc  = missing_data
         iv % pseudo(n) % v % qc  = missing_data
         iv % pseudo(n) % t % qc  = missing_data
         iv % pseudo(n) % p % qc  = missing_data
         iv % pseudo(n) % q % qc  = missing_data
         
         ob % pseudo(n) % u = missing_r
         ob % pseudo(n) % v = missing_r
         ob % pseudo(n) % t = missing_r
         ob % pseudo(n) % p = missing_r
         ob % pseudo(n) % q = missing_r

!------------------------------------------------------------------------
!        [1.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

         iv % pseudo(n)%loc%x = pseudo_x
         iv % pseudo(n)%loc%y = pseudo_y
         iv % pseudo(n)%zk    = pseudo_z

         iv%pseudo(n)%loc%i = int(pseudo_x)
         iv%pseudo(n)%loc%j = int(pseudo_y)

         iv%pseudo(n)%loc%dx = pseudo_x-real(iv%pseudo(n)%loc%i)
         iv%pseudo(n)%loc%dy = pseudo_y-real(iv%pseudo(n)%loc%j)
         iv%pseudo(n)%loc%dxm=1.0-iv%pseudo(n)%loc%dx
         iv%pseudo(n)%loc%dym=1.0-iv%pseudo(n)%loc%dy
                  
         if ( pseudo_var(1:1) == 'u' ) then
            iv % pseudo(n) % u % inv = pseudo_val
            iv % pseudo(n) % u % error = pseudo_err
            iv % pseudo(n) % u % qc = 0
         else if ( pseudo_var(1:1) == 'v' ) then
            iv % pseudo(n) % v % inv = pseudo_val
            iv % pseudo(n) % v % error = pseudo_err
            iv % pseudo(n) % v % qc = 0
         else if ( pseudo_var(1:1) == 't' ) then
            iv % pseudo(n) % t % inv = pseudo_val
            iv % pseudo(n) % t % error = pseudo_err
            iv % pseudo(n) % t % qc = 0
         else if ( pseudo_var(1:1) == 'p' ) then
            iv % pseudo(n) % p % inv = pseudo_val
            iv % pseudo(n) % p % error = pseudo_err
            iv % pseudo(n) % p % qc = 0
         else if ( pseudo_var(1:1) == 'q' ) then
            iv % pseudo(n) % q % inv = pseudo_val
            iv % pseudo(n) % q % error = pseudo_err
            iv % pseudo(n) % q % qc = 0
         end if 
         
         write(6,'(a4,2f15.5)')pseudo_var, pseudo_val, pseudo_err
         write(6,'(3f15.5)')pseudo_x, pseudo_y, pseudo_z
      end do
   end if

END SUBROUTINE da_setup_pseudo_obs




