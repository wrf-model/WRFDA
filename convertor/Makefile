#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

OBJS	=	module_mm5_wrf_interface.o \
		module_wrf_mm5_interface.o \
		module_kma2netcdf_interface.o \
		module_netcdf2kma_interface.o

MODULES =	module_convert_tool.o \
		module_mm5_wrf.o \
		module_wrf_mm5.o \
		module_kma_wave2grid.o \
		module_wave2grid_kma.o \

LIBTARGET    =  convertor
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS)
		$(AR) ../main/lib_3dvar.a $(OBJS) $(MODULES)

include ../configure.3dvar

module_mm5_wrf_interface.o: \
		module_mm5_wrf.o \
		module_mm5_wrf_interface.F
		$(CPP) $(CPPFLAGS) -I../inc module_mm5_wrf_interface.F > module_mm5_wrf_interface.f
		$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) -I../da_3dvar/src module_mm5_wrf_interface.f

module_wrf_mm5_interface.o: \
		module_wrf_mm5.o \
		module_wrf_mm5_interface.F
		$(CPP) $(CPPFLAGS) -I../inc module_wrf_mm5_interface.F > module_wrf_mm5_interface.f
		$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) -I../da_3dvar/src module_wrf_mm5_interface.f

module_convert_tool.o: \
		convert_tool/da_convertor_v_interp.inc \
		convert_tool/da_ref_height.inc \
		convert_tool/module_convert_tool.F
		$(CPP) -I./convert_tool $(CPPFLAGS) convert_tool/module_convert_tool.F > module_convert_tool.f
		$(FC) -c -I./convert_tool -I../da_3dvar/src $(FCFLAGS_3DVAR) $(MODULE_DIRS) module_convert_tool.f

module_mm5_wrf.o: \
		mm5_wrf/da_interp_sigma2eta.inc \
		mm5_wrf/da_setup_mm5.inc \
		mm5_wrf/da_transfer_mm5toxb.inc \
		mm5_wrf/da_transfer_xbtowrf.inc \
		mm5_wrf/module_mm5_wrf.F
		$(CPP) $(CPPFLAGS) -I./mm5_wrf -I../inc mm5_wrf/module_mm5_wrf.F > module_mm5_wrf.f
		$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) -I./mm5_wrf -I../da_3dvar/src module_mm5_wrf.f

module_wrf_mm5.o: \
		wrf_mm5/da_interp_eta2sigma.inc \
		wrf_mm5/da_setup_mm5_reference.inc \
		wrf_mm5/da_transfer_xbtomm5.inc \
		wrf_mm5/module_wrf_mm5.F
		$(CPP) $(CPPFLAGS) -I./wrf_mm5 -I../inc wrf_mm5/module_wrf_mm5.F > module_wrf_mm5.f
		$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) -I./mm5_wrf -I../da_3dvar/src module_wrf_mm5.f

module_kma_wave2grid.o: \
		kma_wave2grid/BSSLZ1.inc \
		kma_wave2grid/CUT.inc \
		kma_wave2grid/FFT991.inc \
		kma_wave2grid/G2W.inc \
		kma_wave2grid/G2WDZ.inc \
		kma_wave2grid/G2WPP.inc \
		kma_wave2grid/GAUSS.inc \
		kma_wave2grid/GOUT.inc \
		kma_wave2grid/IDCMP.inc \
		kma_wave2grid/LGNDR1.inc \
		kma_wave2grid/LGNUV.inc \
		kma_wave2grid/LGNW2G.inc \
		kma_wave2grid/LT2GAU.inc \
		kma_wave2grid/MNMX.inc \
		kma_wave2grid/MOVEC.inc \
		kma_wave2grid/PACK.inc \
		kma_wave2grid/RADB2M.inc \
		kma_wave2grid/RADB3M.inc \
		kma_wave2grid/RADB4M.inc \
		kma_wave2grid/RADB5M.inc \
		kma_wave2grid/RADBGM.inc \
		kma_wave2grid/RADF2M.inc \
		kma_wave2grid/RADF3M.inc \
		kma_wave2grid/RADF4M.inc \
		kma_wave2grid/RADF5M.inc \
		kma_wave2grid/RADFGM.inc \
		kma_wave2grid/REDDAT.inc \
		kma_wave2grid/REDHED.inc \
		kma_wave2grid/REDIEG.inc \
		kma_wave2grid/REOWAV.inc \
		kma_wave2grid/REOWV.inc \
		kma_wave2grid/RESET.inc \
		kma_wave2grid/RFFTBM.inc \
		kma_wave2grid/RFFTFM.inc \
		kma_wave2grid/RFFTIM.inc \
		kma_wave2grid/RFTB1M.inc \
		kma_wave2grid/RFTB2M.inc \
		kma_wave2grid/RFTB9M.inc \
		kma_wave2grid/RFTF1M.inc \
		kma_wave2grid/RFTF2M.inc \
		kma_wave2grid/RFTF3M.inc \
		kma_wave2grid/RFTF9M.inc \
		kma_wave2grid/RFTI1M.inc \
		kma_wave2grid/SETARY.inc \
		kma_wave2grid/W2G.inc \
		kma_wave2grid/W2GCONV.inc \
		kma_wave2grid/W2GPXY.inc \
		kma_wave2grid/W2GUV.inc \
		kma_wave2grid/WAVMAG.inc \
		kma_wave2grid/WEIHT2.inc \
		kma_wave2grid/ZNME2PXX.inc \
		kma_wave2grid/module_kma_wave2grid.F
		$(CPP) -I./kma_wave2grid $(CPPFLAGS) kma_wave2grid/module_kma_wave2grid.F > module_kma_wave2grid.f
		$(FC) -c -I./kma_wave2grid $(F77FLAGS_3DVAR) $(MODULE_DIRS) module_kma_wave2grid.f

module_wave2grid_kma.o: \
		wave2grid_kma/BSSLZ1.inc \
		wave2grid_kma/CR8I2V.inc \
		wave2grid_kma/CVDATE.inc \
		wave2grid_kma/GAUSS.inc \
		wave2grid_kma/GH2TV.inc \
		wave2grid_kma/LT2GAU.inc \
		wave2grid_kma/GPLHGT.inc \
		wave2grid_kma/MINMAX.inc \
		wave2grid_kma/MONTWO.inc \
		wave2grid_kma/MOVEC.inc \
		wave2grid_kma/OUTZ.inc \
		wave2grid_kma/PACK.inc \
		wave2grid_kma/PRESUB.inc \
		wave2grid_kma/REDANL.inc \
		wave2grid_kma/REDDAT.inc \
		wave2grid_kma/REDDAT_ASCII.inc \
		wave2grid_kma/REDDAT_BIN.inc \
		wave2grid_kma/REDGES.inc \
		wave2grid_kma/REDHED.inc \
		wave2grid_kma/RESET.inc \
		wave2grid_kma/SPLDIF3_H.inc \
		wave2grid_kma/TETEN.inc \
		wave2grid_kma/VPRM.inc \
		wave2grid_kma/WRTDAT.inc \
		wave2grid_kma/WRTEOF.inc \
		wave2grid_kma/WRTHED.inc \
		wave2grid_kma/ZE2TVE.inc \
		wave2grid_kma/ZMNLAT.inc \
		wave2grid_kma/ZMNT.inc \
		wave2grid_kma/PREGSM.inc \
		wave2grid_kma/module_wave2grid_kma.F
		$(CPP) -I./wave2grid_kma $(CPPFLAGS) wave2grid_kma/module_wave2grid_kma.F > module_wave2grid_kma.f
		$(FC) -c -I./wave2grid_kma $(F77FLAGS_3DVAR) $(MODULE_DIRS) module_wave2grid_kma.f

clean:
	$(RM) *.f *.o *.mod
#	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

