#	Top level Makefile for wrf system

LN      =       ln -s
MAKE    =       make -i -r
MV	=	/bin/mv
RM      =       /bin/rm -f

include ./configure.wrf

EM_MODULE_DIR = -I../dyn_em
EM_MODULES =  $(EM_MODULE_DIR)

DA_3DVAR_MODULES = $(INCLUDE_MODULES)
DA_3DVAR_MODULES_2 = $(INC_MOD_3DVAR)

DA_CONVERTOR_MOD_DIR = -I../convertor -p../convertor
DA_CONVERTOR_MODULES = $(DA_CONVERTOR_MOD_DIR) $(INCLUDE_MODULES)

#### 3.d.   add macros to specify the modules for this core

#EXP_MODULE_DIR = -I../dyn_exp
#EXP_MODULES =  $(EXP_MODULE_DIR)


NMM_MODULE_DIR = -I../dyn_nmm
NMM_MODULES =  $(NMM_MODULE_DIR)

ALL_MODULES =                           \
               $(EM_MODULE_DIR)         \
               $(NMM_MODULES)           \
               $(EXP_MODULES)           \
               $(INCLUDE_MODULES)

deflt :
		@ echo Please compile the code using ./compile

configcheck:
	@if [ "$(A2DCASE)" -a "$(DMPARALLEL)" ] ; then \
	 echo "------------------------------------------------------------------------------" ; \
	 echo "WRF CONFIGURATION ERROR                                                       " ; \
	 echo "The $(A2DCASE) case cannot be used on distributed memory parallel systems." ; \
	 echo "Only 3D WRF cases will run on these systems." ; \
	 echo "Please chose a different case or rerun configure and chose a different option."  ; \
	 echo "------------------------------------------------------------------------------" ; \
         exit 2 ; \
	fi

var : 
	/bin/rm -f main/libwrflib.a
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" ext
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" toolsdir
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" REGISTRY="Registry" framework
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" shared
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" da_3dvar_io
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" wrfvar_src
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" da_3dvar_interface
	( cd main ; $(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" wrfvar )

pure_var : 
	@ echo 'This option assumes that you have already compiled the WRF frame part correctly.'
	@ echo 'If you have not done so, please use compile 3dvar'
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" da_3dvar_io
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" wrfvar_src
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" da_3dvar_interface
	( cd main ; $(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" wrfvar )

k2n : 
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" ext
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" toolsdir
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" REGISTRY="Registry" framework
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" shared
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" physics
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" em_core
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" da_3dvar_io
	$(MAKE) MODULE_DIRS="$(DA_CONVERTOR_MODULES)" convertor_drivers
	( cd main ; \
          /bin/rm -f kma2netcdf.exe ; \
	  $(MAKE) MODULE_DIRS="$(DA_CONVERTOR_MODULES)" kma2netcdf )

n2k : 
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" ext
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" toolsdir
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" REGISTRY="Registry" framework
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" shared
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" physics
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" em_core
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" da_3dvar_io
	$(MAKE) MODULE_DIRS="$(DA_CONVERTOR_MODULES)" convertor_drivers
	( cd main ; \
          /bin/rm -f netcdf2kma.exe ; \
	  $(MAKE) MODULE_DIRS="$(DA_CONVERTOR_MODULES)" netcdf2kma )

be :
	/bin/rm -f main/libwrflib.a
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES_2)" gen_be_short
	( cd gen_be ; \
	/bin/rm -f *.exe ; \
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" SOLVER=gen_be )

be_wrf :
	/bin/rm -f main/libwrflib.a
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" ext
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" toolsdir
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" REGISTRY="Registry" framework
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" shared
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" da_3dvar_io
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES_2)" gen_be_long
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" gen_be_interface
	( cd gen_be ; \
	/bin/rm -f *.exe ; \
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" SOLVER=gen_be ; \
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" gen_be_stage0 )

pure_be_wrf :
	@ echo 'This option assumes that you have already compiled the WRF frame part correctly.'
	@ echo 'If you have not done so, please use compile be_wrf'
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" da_3dvar_io
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES_2)" gen_be_long
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" gen_be_interface
	( cd gen_be ; \
	/bin/rm -f *.exe ; \
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" SOLVER=gen_be ; \
	$(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" gen_be_stage0 )

ext :
	@ echo '--------------------------------------'
	( cd frame ; $(MAKE) externals )

framework :
	@ echo '--------------------------------------'
	( cd frame ; $(MAKE) framework; \
	cd ../external/io_netcdf ; make NETCDFPATH="$(NETCDFPATH)" FC="$(FC) $(FCBASEOPTS)" RANLIB="$(RANLIB)" CPP="$(CPP)" diffwrf; \
	cd ../io_grib1 ; make FC="$(FC) -I. $(FCBASEOPTS)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" CPP="$(CPP)"; \
	cd ../io_int ; $(MAKE) FC="$(FC) $(FCBASEOPTS)" RANLIB="$(RANLIB)" CPP="$(CPP)" diffwrf ; cd ../../frame )

shared :
	@ echo '--------------------------------------'
	( cd share ; $(MAKE) )

em_core :
	@ echo '--------------------------------------'
	( cd dyn_em ; $(MAKE) )

wrfvar_src :
	@ echo '--------------------------------------'
	( cd da_3dvar/src; $(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES_2)" 3dvar )

da_3dvar_io :
	@ echo '--------------------------------------'
	( cd da_3dvar; $(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" da_3dvar_io )

da_3dvar_interface :
	@ echo '--------------------------------------'
	( cd da_3dvar; $(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" da_3dvar_interface )

gen_be_interface :
	@ echo '--------------------------------------'
	( cd da_3dvar; $(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES)" gen_be_interface )

convertor_drivers :
	@ echo '--------------------------------------'
	( cd convertor ; $(MAKE) )

gen_be_short :
	@ echo '--------------------------------------'
	( cd da_3dvar/src; $(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES_2)" gen_be_short )

gen_be_long :
	@ echo '--------------------------------------'
	( cd da_3dvar/src; $(MAKE) MODULE_DIRS="$(DA_3DVAR_MODULES_2)" gen_be_long )

# rule used by configure to test if this will compile with MPI 2 calls MPI_Comm_f2c and _c2f
mpi2_test :
	@ cd tools ; /bin/rm -f mpi2_test ; $(CC) -o mpi2_test mpi2_test.c ; cd ..

### 3.b.  sub-rule to build the expimental core

toolsdir :
	@ echo '--------------------------------------'
	( cd tools ; $(MAKE) CC="$(CC_TOOLS)" )

clean :
		@ echo 'Use the clean script'

# DO NOT DELETE
