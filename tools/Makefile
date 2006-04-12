.SUFFIXES: .c .o

CC = cc
CFLAGS = #-ansi
LDFLAGS =
DEBUG = -g 
OBJ = registry.o my_strtok.o reg_parse.o data.o type.o misc.o \
      gen_defs.o gen_allocs.o gen_mod_state_descr.o gen_scalar_indices.o \
      gen_args.o gen_config.o sym.o symtab_gen.o gen_wrf_io.o \
      gen_model_data_ord.o gen_interp.o gen_comms.o gen_scalar_derefs.o

registry : $(OBJ)
	$(CC) -o registry $(DEBUG) $(LDFLAGS) $(OBJ)

.c.o :
	$(CC) $(CFLAGS) -c $(DEBUG) $<

clean:
	/bin/rm -f $(OBJ) gen_comms.c

gen_comms.c : gen_comms.stub
	/bin/cp gen_comms.stub gen_comms.c

advance_cymdh : advance_cymdh.o 
	rm $@
	$(FC) -o advance_cymdh advance_cymdh.o

advance_cymdh.o : advance_cymdh.F
	$(FC) -c $(FCFLAGS) -o advance_cymdh.o advance_cymdh.F

bufr_little_endian.exe : bufr_little_endian.o
			$(RM) $@
			$(FC) -o bufr_little_endian.exe bufr_little_endian.o $(DA_SRC)/da_bufrlib.o \
                          $(DA_SRC)/bort_exit.o $(DA_SRC)/restd.o $(DA_SRC)/wrdesc.o

bufr_little_endian.o : $(DA_SRC)/bort_exit.o $(DA_SRC)/restd.o $(DA_SRC)/wrdesc.o $(DA_SRC)/da_bufrlib.o
			$(CPP) $(CPPFLAGS) bufr_little_endian.F > bufr_little_endian.f
			$(FC) -c $(F77FLAGS_ENDIAN) bufr_little_endian.f


# regenerate this list with "makedepend -Y *.c"

# DO NOT DELETE THIS LINE -- make depend depends on it.

data.o: registry.h protos.h data.h
gen_allocs.o: protos.h registry.h data.h
gen_args.o: protos.h registry.h data.h
gen_scalar_derefs.o: protos.h registry.h data.h
gen_config.o: protos.h registry.h data.h
gen_defs.o: protos.h registry.h data.h
gen_mod_state_descr.o: protos.h registry.h data.h
gen_model_data_ord.o: protos.h registry.h data.h
gen_scalar_indices.o: protos.h registry.h data.h
gen_wrf_io.o: protos.h registry.h data.h
misc.o: protos.h registry.h data.h
my_strtok.o: registry.h protos.h data.h
reg_parse.o: registry.h protos.h data.h
registry.o: protos.h registry.h data.h
sym.o: sym.h
type.o: registry.h protos.h data.h
gen_interp.o: registry.h protos.h data.h
