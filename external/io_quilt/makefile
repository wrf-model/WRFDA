#makefile to build io_quilt that does quilting to binary i/o

OBJSL   = io_quilt.o collect_on_comm.o module_quilt_outbuf_ops.o # ../io_int/pack_utils.o
OBJS    = $(OBJSL)
OPTS    =
FFLAGS  =  $(OPTS) -w
LIBS    = 
CPP     = /lib/cpp -C -P $(TRADFLAG) $(CPPFLAGS)
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar

.SUFFIXES:      .F90 .F .f .o .code

all : collect_on_comm.o

#libwrfio_quilt.a:		$(OBJS)
#			/bin/rm -f libwrfio_quilt.a
#			$(AR) cr libwrfio_quilt.a $(OBJSL)
#
#io_quilt.o:               io_quilt.F90 module_quilt_outbuf_ops.o
#			$(CPP) io_quilt.F90 | $(M4) - > io_quilt.f
#			$(FC) $(FFLAGS) -c io_quilt.f
#
#module_quilt_outbuf_ops.o:
#			$(CPP) module_quilt_outbuf_ops.F | $(M4) - > module_quilt_outbuf_ops.f
#			$(FC) $(FFLAGS) -c module_quilt_outbuf_ops.f

collect_on_comm.o:	collect_on_comm.c
			$(CC) $(CFLAGS) -c collect_on_comm.c

#../io_int/pack_utils.o:	../io_int/pack_utils.c
#			( cd ../io_int ; $(CC) $(CFLAGS) -c pack_utils.c )

clean:  
			/bin/rm -f  *.o
