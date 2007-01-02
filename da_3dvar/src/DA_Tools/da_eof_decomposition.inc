subroutine da_eof_decomposition( kz, bx, e, l )
   
!------------------------------------------------------------------------------
!  PURPOSE: Compute eigenvectors E and eigenvalues L of vertical covariance matrix
!           B_{x} defined by equation:  E^{T} B_{x} E = L, given input kz x kz 
!           BE field.
!
!  METHOD:
!
!  HISTORY: 05/18/2000 - Creation of F90 version.           Dale Barker
!           10/15/2004 - 2D version for WRF.                Dale Barker
!------------------------------------------------------------------------------
   
   implicit none

   integer, intent(in)      :: kz               ! Dimension of error matrix. 
   real, intent(in)         :: bx(1:kz,1:kz)    ! Vert. background error.
   real, intent(out)        :: e(1:kz,1:kz)     ! Eigenvectors of Bx.
   real, intent(out)        :: l(1:kz)          ! Eigenvalues of Bx.

   integer                  :: work             ! Size of work array.
   integer                  :: m                ! Loop counters
   integer                  :: info             ! Info code.

   real                     :: work_array(1:3*kz-1)
   real                     :: ecopy(1:kz,1:kz)
   real                     :: lcopy(1:kz)

!-------------------------------------------------------------------------
!     [5.0]: Perform global eigenvalue decomposition using LAPACK software:
!-------------------------------------------------------------------------
   
   work = 3 * kz - 1   
   ecopy(1:kz,1:kz) = bx(1:kz,1:kz)
   lcopy(1:kz) = 0.0

   call dsyev( 'V', 'U', kz, ecopy, kz, lcopy, work_array, work, info )
   
   if ( info /= 0 ) then
      write(0,'(A,I4,A)')' da_eof_decomposition: info = ', &
                         info,' - error in decomposition. Stop.'
      stop
   end if
   
!--Swap order of eigenvalues, vectors so 1st is one with most variance:
   
   do m = 1, kz
      l(m) = lcopy(kz+1-m)
      e(1:kz,m) = ecopy(1:kz,kz+1-m)
   end do
   
end subroutine da_eof_decomposition
