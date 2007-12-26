! This file created from test/mpi/f77/ext/allocmem.f with f77tof90
! -*- Mode: Fortran; -*- 
!
!  (C) 2004 by Argonne National Laboratory.
!      See COPYRIGHT in top-level directory.
!
        program main
        use mpi
!
! This program makes use of a common (but not universal; g77 doesn't 
! have it) extension: the "Cray" pointer.  This allows MPI_Alloc_mem
! to allocate memory and return it to Fortran, where it can be used.
!
        real a
        pointer (p,a(100,100))
        integer ierr
        integer i,j
        call mpi_init(ierr)     
        call mpi_alloc_mem(4*100*100,MPI_INFO_NULL,p,ierr )

        do i=1,100
            do j=1,100
                a(i,j) = -1
            enddo
        enddo
        a(3,5) = 10.0

        call mpi_free_mem( a, ierr )
        call mpi_finalize(ierr)
        print *, ' No Errors'
        end
