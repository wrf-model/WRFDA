subroutine da_get_3d_sum(var, name)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, dimension(ims:ime, jms:jme, kms:kme), intent(in) :: var
   character(len=*), intent(in) :: name

   real :: partial, total

   partial = sum(var(its:ite,jts:jte,kts:kte)*var(its:ite,jts:jte,kts:kte))

   total = wrf_dm_sum_real( partial)

   write(unit=stdout, fmt='(3a, e24.14)') &
        'Square sum of <', trim(name), '>=', total

end subroutine da_get_3d_sum


