subroutine da_setup_pseudo_obs(xp, iv, ob)

   !-------------------------------------------------------------------------
   ! Purpose: Sets up pseudo ob part of observation structure.
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(inout) :: iv   ! Obs and header structure.
   type (y_type), intent(inout)  :: ob   ! (Smaller) observation structure.
   type(xpose_type), intent(in)  :: xp   ! Domain decomposition vars.

   logical                       :: outside      !wrt local domain
   integer                       :: i, j ! local variables
   integer                       :: n    ! Loop counters.

   if (trace_use) call da_trace_entry("da_setup_pseudo_obs")

   outside = .false.
   i = int(pseudo_x)
   j = int(pseudo_Y)

   if (fg_format == fg_format_kma_global) then
      if ((j < xp%jts-1) .or. (j > xp%jte)) outside = .true.
   else

      if ((i < xp%ids) .or. (i >= xp%ide) .or. &
         (j < xp%jds) .or. (j >= xp%jde)) outside = .true.

      if ((i < xp%its-1) .or. (i > xp%ite) .or. &
         (j < xp%jts-1) .or. (j > xp%jte)) outside = .true.
   end if

   if (outside) then
      iv%num_pseudo = 0
      iv%ob_numb(iv%current_ob_time)%pseudo = 0
      return
   else
      iv % num_pseudo = num_pseudo
      ob % num_pseudo = num_pseudo
      iv%num_pseudo_glo = num_pseudo
      iv%ob_numb(iv%current_ob_time)%pseudo = iv%num_pseudo
   end if

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

      !---------------------------------------------------------------
      ! [1.0] Initialise components of innovation vector:
      !---------------------------------------------------------------

      iv % pseudo(n)%loc%x = pseudo_x
      iv % pseudo(n)%loc%y = pseudo_y
      iv % pseudo(n)%zk    = pseudo_z

      iv%pseudo(n)%loc%i = int(pseudo_x)
      iv%pseudo(n)%loc%j = int(pseudo_y)

      iv%pseudo(n)%loc%dx = pseudo_x-real(iv%pseudo(n)%loc%i)
      iv%pseudo(n)%loc%dy = pseudo_y-real(iv%pseudo(n)%loc%j)
      iv%pseudo(n)%loc%dxm=1.0-iv%pseudo(n)%loc%dx
      iv%pseudo(n)%loc%dym=1.0-iv%pseudo(n)%loc%dy

      if (pseudo_var(1:1) == 'u' .or. pseudo_var(1:1) == 'U') then
         iv % pseudo(n) % u % inv = pseudo_val
         iv % pseudo(n) % u % error = pseudo_err
         iv % pseudo(n) % u % qc = 0
      else if (pseudo_var(1:1) == 'v' .or. pseudo_var(1:1) == 'V') then
         iv % pseudo(n) % v % inv = pseudo_val
         iv % pseudo(n) % v % error = pseudo_err
         iv % pseudo(n) % v % qc = 0
      else if (pseudo_var(1:1) == 't' .or. pseudo_var(1:1) == 'T') then
         iv % pseudo(n) % t % inv = pseudo_val
         iv % pseudo(n) % t % error = pseudo_err
         iv % pseudo(n) % t % qc = 0
      else if (pseudo_var(1:1) == 'p' .or. pseudo_var(1:1) == 'P') then
         iv % pseudo(n) % p % inv = pseudo_val
         iv % pseudo(n) % p % error = pseudo_err
         iv % pseudo(n) % p % qc = 0
      else if (pseudo_var(1:1) == 'q' .or. pseudo_var(1:1) == 'Q') then
         iv % pseudo(n) % q % inv = pseudo_val
         iv % pseudo(n) % q % error = pseudo_err
         iv % pseudo(n) % q % qc = 0
      end if 
      
      write(unit=stdout,fmt='(a4,2f15.5)')pseudo_var, pseudo_val, pseudo_err
      write(unit=stdout,fmt='(3f15.5)')pseudo_x, pseudo_y, pseudo_z
   end do

   if (trace_use) call da_trace_exit("da_setup_pseudo_obs")

end subroutine da_setup_pseudo_obs


