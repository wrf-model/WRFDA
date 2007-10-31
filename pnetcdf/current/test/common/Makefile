srcdir = .


CC = /big/users/wrfhelp/external/mpi/mpich2-1.0.5p4/pgi_i686/bin/mpicc
CFLAGS = -g -O2 -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64

OBJS = testutils.o

all: libtestutils.a

libtestutils.a: $(OBJS)
	ar cru libtestutils.a $(OBJS)
testutils.o: testutils.c testutils.h

clean:
	rm -f *.o *.a
