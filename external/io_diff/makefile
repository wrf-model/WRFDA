#makefile to build io_int that does binary i/o

FFLAGS  =  -w
CPP     = /lib/cpp -C -P $(TRADFLAG) \
	  -I../io_quilt -I../io_netcdf -I../io_int

.SUFFIXES:      .F90 .F .f .o .code

all : diffwrf diffwrf8

diffwrf:	diffwrf.F \
		../../frame/pack_utils.o \
		../../frame/internal_header_util.o \
		../../frame/module_machine.o \
		../io_int/libwrfio_int.a
		$(CPP) diffwrf.F > diffwrf.f
		$(FC) $(FFLAGS) -c diffwrf.f
		$(FC) $(FFLAGS) $(LDFLAGS) \
		-I. -I../io_int \
		-L../io_int/libwrfio_int.a \
		-o diffwrf \
		../../frame/pack_utils.o \
		../../frame/internal_header_util.o \
		../../frame/module_machine.o \
		diffwrf.o


diffwrf8:	diffwrf8.F      \
		../../frame/pack_utils.o        \
		../../frame/internal_header_util.o      \
		../../frame/module_machine.o    \
		../io_int/libwrfio_int.a
		$(CPP) diffwrf8.F > diffwrf8.f
		$(FC) $(FFLAGS) $(LDFLAGS) -o diffwrf8 \
		-I. -I../io_int \
		../../frame/pack_utils.o \
		../../frame/internal_header_util.o \
		../../frame/module_machine.o \
		../io_int/libwrfio_int.a
		diffwrf8.f

clean:  
		/bin/rm -f  *.f *.o *.mod

