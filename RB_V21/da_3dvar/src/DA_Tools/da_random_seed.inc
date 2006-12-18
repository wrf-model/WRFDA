subroutine da_random_seed

   implicit none

   integer              :: seed_size
   integer, allocatable :: seed_array(:)

!----------------------------------------------------------------------------
!  Check that right seed_size is being used:
!----------------------------------------------------------------------------

   call random_seed(size=seed_size)              ! Get size of seed array.
   allocate( seed_array(1:seed_size) )
   seed_array(1:seed_size) = 0

   if ( put_rand_seed ) then            ! Manually set random seed.
      if ( seed_size /= 2 ) then
         write(6,'(a)')' Warning: Only setting first two values of seed_array'
      end if
     
      seed_array(1) = seed_array1
      seed_array(2) = seed_array2
      write(6,'(a,i16)')' Setting seed_array(1) = ', seed_array1
      write(6,'(a,i16)')' Setting seed_array(2) = ', seed_array2
      call random_seed(put=seed_array(1:seed_size)) ! Set random seed.
   else                                 ! Random seed set "randomly"
      call random_seed
      call random_seed(get=seed_array(1:seed_size))
      write(6,'(a,10i16)')' Random number seed array = ', seed_array
   end if
   
   deallocate( seed_array )

end subroutine da_random_seed
