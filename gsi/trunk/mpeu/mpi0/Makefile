#!/bin/make

MODULE		= mpi0

SRCS_mks	= Makefile			\
		  Makefile.conf.AIX		\
		  Makefile.conf.IRIX		\
		  Makefile.conf.IRIX64		\
		  Makefile.conf.OSF1		\
		  Makefile.conf.T3E		\
		  Makefile.conf.UNICOS		\
		  Makefile.dependency		\
		  Makefile.rules		\
		  configure			\
		  unconfigure			\
		  README

# Single PE replacements of a minimum subset of libmpi.a, if there is
# no any "message-passing" call.

SRCS_F90	= m_SHMEM.F90                   \
		  mpi_abort.F90			\
		  mpi_comm_dup.F90		\
		  mpi_comm_free.F90		\
		  mpi_comm_rank.F90		\
		  mpi_comm_size.F90		\
		  mpi_error_string.F90		\
		  mpi_finalize.F90		\
		  mpi_init.F90			\
		  mpi_initialized.F90		\
		  mpi_isend.F90			\
		  mpi_send.F90			\
		  mpi_recv.F90			\
		  mpi_irecv.F90			\
		  mpi_waitall.F90               \
		  mpi_wtick.F90			\
		  mpi_wtime.F90			\
		  mpi_barrier.F90		\
		  mpi_bcast.F90			\
		  mpi_gather.F90		\
		  mpi_gatherv.F90		\
		  mpi_scatter.F90		\
		  mpi_scatterv.F90		\
		  mpi_allgather.F90		\
		  mpi_allgatherv.F90		\
		  mpi_alltoall.F90		\
		  mpi_alltoallv.F90		\
		  mpi_reduce.F90		\
		  mpi_allreduce.F90		\
		  mpi_reduce_scatter.F90	\
		  mpi0_copy.F90			\
		  mpi_zzz.F90

SRCS_F		= m_mpi0.F 

SRCS_H		= Const_MPItype.H

SRCS_ALL	= $(SRCS_f)		\
		  $(SRCS_F)		\
		  $(SRCS_f90)		\
		  $(SRCS_F90)		\
		  $(SRCS_C)

OBJS_ALL	= $(SRCS_f:.f=.o)	\
		  $(SRCS_F:.F=.o)	\
		  $(SRCS_f90:.f90=.o)	\
		  $(SRCS_F90:.F90=.o)	\
		  $(SRCS_C:.c=.o)

ARCH_TARGETS	= $(SRCS_mks) $(SRCS_ALL) $(SRCS_H)
ALL_TARGETS	= lib$(MODULE).a

#	Try to define _Ddft, _Dsys and _Dnod in proper Makefiles.
#
#		1) _D for the make() time macros, as in
#		make _D="-Dxxx ..."
#
#		2) _Ddft for the default global macros defined
#		in the main Makefile.
#
#		3) _Dsys for the OS dependent macros defined
#		in the main Makefile and can be redefined in
#		the Makefile.conf file.
#
#		4) _Dnod for the OS or the node dependent
#		macros defined in the Makefile.conf file.

_Ddft	=
_Dsys	= -Dsys`uname -s`

include Makefile.rules
include Makefile.conf

_DF	= $(_Ddft) $(_Dsys) $(_Dnod) $(_D)

_I	= $(_Inull) -I. $(_Iconf)

# targets

lib$(MODULE).a:	$(OBJS_ALL)
	$(RM) $@
	@ echo $(AR) $@ $(OBJS_ALL)
	@ $(AR) $@ $(OBJS_ALL)

# A test for availability of MPI on your system.

ut_mpif:
	@ echo "      program $@"		 >ut_mpif.f
	@ echo "      include 'mpif.h'"		>>ut_mpif.f
	@ echo "      end program $@"		>>ut_mpif.f
	@ $(FC) -c $(_Iconf) ut_mpif.f >/dev/null 2>&1; q=$$?  ;\
	  if [ $$q -ne 0 ]; then	\
	    h="neither `uname -n`";	\
	    m="nor your $(MAKEFILE)";	\
	    echo "$@: $$h $$m is configured to use MPI!"; fi ;\
	  $(RM) ut_mpif.o ut_mpif.f; exit $$q

# dependencies:

include Makefile.dependency

#.
