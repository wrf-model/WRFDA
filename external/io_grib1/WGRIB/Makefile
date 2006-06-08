include ../../../configure.wrf

WGRID_OBJ=wgrib_main.o seekgrib.o ibm2flt.o readgrib.o intpower.o cnames.o BDSunpk.o \
  flt2ieee.o wrtieee.o levels.o PDStimes.o missing.o  \
  nceptable_reanal.o nceptable_opn.o ensemble.o \
  ombtable.o ec_ext.o gribtable.o gds_grid.o PDS_date.o ectable_128.o \
  ectable_129.o ectable_130.o ectable_131.o ectable_140.o ectable_150.o \
  ectable_151.o ectable_160.o ectable_170.o ectable_180.o nceptab_129.o \
  dwdtable_002.o dwdtable_201.o dwdtable_202.o dwdtable_203.o \
  cptectable_254.o nceptab_130.o nceptab_131.o

all: wgrib

archive: wgrib

wgrib: $(WGRIB_OBJ)
	$(CC) -o wgrib $(WGRIB_OBJ) -lm
	( cd .. ; $(RM) wgrib ; $(LN) WGRIB/wgrib wgrib ; cd WGRIB )

clean:	
	@ $(RM) $(WGRIB_OBJ) wgrib
	@ ( cd .. ; $(RM) wgrib ; cd WGRIB )

superclean : clean

