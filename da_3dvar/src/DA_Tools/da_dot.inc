#ifdef DM_PARALLEL
      real function da_dot(x,y,n)
!
!     forms the dot product of two vectors.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
      implicit none

      INCLUDE 'mpif.h'

      integer, intent(in)               :: n
      real, dimension(n), intent(in)    :: x,y

      real dtemp1,dtemp1x
      integer i,m,mp1
      integer ierror     ! MPI error code.

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#endif
!
      da_dot = 0.0
      if(n <= 0)return
      dtemp1 = 0.0
!
!        code for both increments equal to 1
!
      if(n > 0 ) then
        m = mod(n,5)
        if( m /= 0 ) then
          do i = 1,m
            dtemp1 = dtemp1 + x(i)*y(i)
          end do
        end if
        if( n >= 5 ) then
          mp1 = m + 1
          do i = mp1,n,5
            dtemp1 = dtemp1 + x(i    )*y(i    ) + x(i + 1)*y(i + 1) + &
                              x(i + 2)*y(i + 2) + x(i + 3)*y(i + 3) + &
                              x(i + 4)*y(i + 4)
          end do
        end if
      end if
!
      call mpi_allreduce(dtemp1,dtemp1x,1,TRUE_MPI_REAL,mpi_sum,mpi_comm_world,ierror)
!
      da_dot = dtemp1x
      return
      end function da_dot

#else

      real function da_dot(x,y,n)

!
!------------------------------------------------------------------------------
!  PURPOSE: Prepare for dot product calculation - separate out for MPI coding
!
!  METHOD:
!
!  HISTORY: 06/14/2001 - Creation of F90 version.           John Derber
!
!  PARENT_MODULE: DA_Minimisation
!------------------------------------------------------------------------------

      implicit none
      
      integer, intent(in)               :: n
      real, dimension(n), intent(in)    :: x,y

      da_dot=dot_product(x,y)

      end function da_dot
#endif

