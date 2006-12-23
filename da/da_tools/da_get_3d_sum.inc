subroutine da_get_3d_sum(var, name, &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte)

   implicit none
   integer, intent(in) :: ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte
   real, dimension(ims:ime, jms:jme, kms:kme), intent(in) :: var
   character(len=*), intent(in) :: name

   real :: partial, total

   ! JRB use variables to stop compiler complaining
   if (ids == 1 .OR. ide == 1 .OR. jds == 1 .OR. jde == 1 .OR. &
       kds == 1 .OR. kde == 1) then
   end if

   partial = sum(var(its:ite,jts:jte,kts:kte)*var(its:ite,jts:jte,kts:kte))

   total = wrf_dm_sum_real( partial)
   write(unit=*,fmt=*) 'TEST_COVERAGE_get_3d_sum:  total = ', total

   write(unit=stderr, fmt='(3a, e24.14)') &
        'Square sum of <', trim(name), '>=', total

end subroutine da_get_3d_sum


