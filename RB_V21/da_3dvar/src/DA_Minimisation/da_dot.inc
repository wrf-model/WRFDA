      real function da_dot(x,y,n)
!
!     forms the dot product of two vectors.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
      implicit none

      integer, intent(in)               :: n
      real, dimension(n), intent(in)    :: x,y

      real dtemp1,dtemp1x
      integer i,m,mp1
      integer ierror     ! MPI error code.
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
#ifdef DM_PARALLEL
      if(global) then
         da_dot = dtemp1
      else
         call mpi_allreduce(dtemp1,dtemp1x,1,TRUE_MPI_REAL,mpi_sum,mpi_comm_world,ierror)
         da_dot = dtemp1x
      endif
#else
      da_dot = dtemp1
#endif
      end function da_dot


