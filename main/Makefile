#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES = 

OBJS    =

LIBPATHS = 

include ../configure.wrf

wrfvar : wrfvar.o
	$(RANLIB) libwrflib.a
	$(LD) -o wrfvar.exe $(LDFLAGS) wrfvar.o libwrflib.a $(LIB)

kma2netcdf : kma2netcdf.o
	$(RANLIB) libwrflib.a
	$(LD) -o kma2netcdf.exe $(LDFLAGS) kma2netcdf.o libwrflib.a $(LIB) -L../convertor

netcdf2kma : netcdf2kma.o
	$(RANLIB) libwrflib.a
	$(LD) -o netcdf2kma.exe $(LDFLAGS) netcdf2kma.o libwrflib.a $(LIB) -L../convertor 

$(SOLVER) : $(SOLVER).o
	$(RANLIB) libwrflib.a
	$(LD) -o $(SOLVER).exe $(LDFLAGS) $(SOLVER).o libwrflib.a $(LIB)

#############################################################################################################################

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

wrfvar.o: ../da_3dvar/module_wrfvar_top.o

# DO NOT DELETE
