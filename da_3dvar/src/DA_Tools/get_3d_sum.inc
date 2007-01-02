subroutine get_3d_sum(var, name, &
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
   integer :: ierror

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#endif

   partial = SUM(var(its:ite,jts:jte,kts:kte)*var(its:ite,jts:jte,kts:kte))

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( partial, total, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )
#else
   total = partial
#endif

   write(unit=0, fmt='(3a, e24.14)') &
        'Square sum of <', trim(name), '>=', total

end subroutine get_3d_sum

