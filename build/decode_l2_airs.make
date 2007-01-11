DECODE_L2_AIRS_OBJS = geth_newdate.o calc_rh.o 

DECODE_L2_AIRS_MODULES =  module_read_airs.o

JPEG_LIB    = $(HOME)/jpeg/jpeg-6b_gcc/lib
ZLIB_LIB    = $(HOME)/zlib/zlib-1.2.3_gcc/lib
HDF_LIB     = $(HOME)/hdf/hdf_4.2r1_g95/lib
HDF_INC     = $(HOME)/hdf/hdf_4.2r1_g95/hdf/src
HDFEOS_LIB  = $(HOME)/hdfeos/hdfeos_g95/lib
HDFEOS_INC  = $(HOME)/hdfeos/hdfeos_g95/include
    
decode_airs : decode_airs.o $(DECODE_L2_AIRS_MODULES) $(DECODE_L2_AIRS_OBJS)
	$(FC) -o $@.exe decode_airs.o $(DECODE_L2_AIRS_MODULES) $(DECODE_L2_AIRS_OBJS) \
 -L$(HDFEOS_LIB) -L$(HDF_LIB) -I$(HDFEOS_INC) -I$(HDF_INC) -lhdfeos -lGctp -lmfhdf -ldf \
 -L$(ZLIB_LIB) -lz -L$(JPEG_LIB) -ljpeg -lm $(LOCAL_LIBS)
