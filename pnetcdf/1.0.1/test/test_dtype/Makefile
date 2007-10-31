srcdir		= .


INCDIR		= $(srcdir)/../../src/lib
LIBDIR		= ../../src/lib

INCLUDES	= -I$(INCDIR)

CC		= /big/users/wrfhelp/external/mpi/mpich2-1.0.5p4/pgi_i686/bin/mpicc
CFLAGS          = -g -O2 -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 $(INCLUDES) 

SRCS		= test_subarray.c test_darray.c test_nonblocking.c
OBJS		= $(SRCS:.c=.o)
PROGS		= test_subarray test_darray test_nonblocking
LDFLAGS		= -L$(LIBDIR) 
LIBS		= -lpnetcdf 

.c.o:
	$(CC) $(CFLAGS) -c $<

all:		$(PROGS)

test_subarray: test_subarray.o
	$(CC) -o $@ $@.o $(LDFLAGS) $(LIBS)

test_darray: test_darray.o
	$(CC) -o $@ $@.o $(LDFLAGS) $(LIBS)

test_nonblocking: test_nonblocking.o
	$(CC) -o $@ $@.o $(LDFLAGS) $(LIBS)

clean:
		rm -f $(OBJS) $(PROGS) *.nc
