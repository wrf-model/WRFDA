subroutine da_get_2d_sum(var, name)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, dimension(ims:ime, jms:jme), intent(in) :: var
   character(len=*), intent(in) :: name

   real :: partial, total

   partial = sum(var(its:ite,jts:jte)*var(its:ite,jts:jte))

   total = wrf_dm_sum_real( partial)
   write(unit=stdout,fmt=*) 'TEST_COVERAGE_get_2d_sum:  total = ', total

   write(unit=stderr, fmt='(3a, e24.14)') &
        'Square sum of <', trim(name), '>=', total

end subroutine da_get_2d_sum


