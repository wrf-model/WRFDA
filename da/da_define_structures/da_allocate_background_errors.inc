subroutine da_print_be_stats_h_global(outunit, variable, k, max_wavenumber, &
  total_power)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none 

   integer,      intent(inout) :: outunit        ! Output file unit.
   character*10, intent(in)    :: variable       ! Variable name.
   integer,      intent(in)    :: k              ! Vertical index.
   integer,      intent(in)    :: max_wavenumber ! Smallest scale required (ni/2-1)

   ! Total Power spectrum (averaged over time/members)
   real,         intent(in) :: total_power(0:max_wavenumber)

   integer :: n                     ! Loop counter.
   real    :: accum_power, variance ! Accumulated power, variance.

   if (trace_use) call da_trace_entry("da_print_be_stats_h_global")

   accum_power = 0.0
   variance = sum(total_power(0:max_wavenumber))

   write(unit=stdout,fmt='(3a,i5,a,i5)')' Power spectra for variable ', trim(variable), &
                          ' and level ', k, ' in unit ', outunit

   do n = 0, max_wavenumber
      accum_power = accum_power + total_power(n)
      write(unit=outunit,fmt='(2i4,2f18.5,f8.3)')k, n, total_power(n), accum_power, &
                                        accum_power / variance
   end do

   outunit = outunit + 1
   write(unit=stdout,fmt=*) ''

   if (trace_use) call da_trace_exit("da_print_be_stats_h_global")

end subroutine da_print_be_stats_h_global


 !--------------------------------------------------------------------------

   if (mz > 0) then
      allocate  (be_sub % val(1:jy,1:mz))
      
      if (vert_corr == vert_corr_2) then

         !--------------------------------------------------------------------
         ! [3.0] Allocate eigenvalues of vertical error covariance matrix:
         !--------------------------------------------------------------------

         if (vert_evalue == vert_evalue_global) then
            ! use global eigenvalues:
            do m = 1, mz
               be_sub % val(1:jy,m) = sqrt (l(m))
            end do
         else
            ! use eigenvalues varying with j-direction.
            do j = 1, jy
               do k = 1, mz
                  if (be_eval_loc(j,k) <=0) then
                     write (unit=message(1),fmt='(A,I5,A,I5,A,F10.2)') &
                        "At lat= ",j," For mode = ",k," local eigen value= ",be_eval_loc(j,k)
                     call da_error(__FILE__,__LINE__,message(1:1))
                  end if
               end do
            end do
            be_sub % val(1:jy,1:mz) = sqrt (be_eval_loc(1:jy,1:mz))            
         end if
 
         if (print_detail_be) then
            write(unit=message(1),fmt='(A,A)') 'j*k Eigenvalues for ', be_sub % name
            call da_array_print(2, be_sub % val(1:jy,1:mz), message(1))
         end if

         !----------------------------------------------------------------------- 
         ! [4.0] Allocate global eigenvectors of vertical error cov.:
         !-----------------------------------------------------------------------

         allocate  (be_sub % evec(1:jy,1:kz,1:mz))
         
         if (vert_evalue == vert_evalue_global) then
            ! use global eigenvectors:
            do j = 1, jy
               be_sub % evec(j,1:kz,1:mz) = e(1:kz,1:mz)
            end do
         else
            ! use eigenvectors varying with i-direction.
            be_sub % evec(1:jy,1:kz,1:mz) =  be_evec_loc(1:jy,1:kz,1:mz)
         end if
         
         if (print_detail_be) then      
            write(unit=message(1),fmt='(A,A)') 'k*k Eigenvectors for j = 1 ', be_sub % name
            call da_array_print (2, be_sub % evec(1,1:kz,1:mz), message(1))
         
            write(unit=message(1),fmt='(A,A)') 'k*k Eigenvectors for j = jy ', be_sub % name
            call da_array_print (2, be_sub % evec(jy,1:kz,1:mz), message(1))
         end if

         allocate (be_sub%val_g(1:mz))
         allocate (be_sub%evec_g(1:kz,1:mz))
  
         be_sub % val_g(1:mz) = l(1:mz)
         be_sub % evec_g(1:kz,1:mz) = e(1:kz,1:mz)
      else if (vert_corr == vert_corr_1) then
         if (print_detail_be) then
           write(unit=stdout,fmt='(A)') &
              'Change be std dev to variance in NMC code'
         end if
         if (vert_evalue == vert_evalue_global) then
            ! use global eigenvalues:
          do m = 1, mz
          be_sub % val(1:jy,m) = l(m)
          end do 
         else
          be_sub % val(1:jy,1:mz) = be_eval_loc(1:jy,1:mz)
         end if
      end if

      !-----------------------------------------------------------------------
      ! [2.2] Allocate recursive filter lengthscales and variance scaling factors:
      !-----------------------------------------------------------------------

      allocate (be_sub % rf_alpha(1:mz))

      be_sub % rf_alpha(1:mz) = 1.0    
   end if

   if (trace_use_dull) call da_trace_exit("da_allocate_background_errors")

end subroutine da_allocate_background_errors


