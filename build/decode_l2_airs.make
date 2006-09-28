DECODE_L2_AIRS_OBJS = geth_newdate.o calc_rh.o 

DECODE_L2_AIRS_MODULES =  module_read_airs.o

JPEGLIB    =  $(EXTERNAL)/jpeg/jpeg-6b/lib
ZLIBLIB    =  $(EXTERNAL)/zlib/zlib-1.2.3/lib

decode_airs : decode_airs.o $(DECODE_L2_AIRS_MODULES) $(DECODE_L2_AIRS_OBJS)
	$(FC) -o $@.exe decode_airs.o $(DECODE_L2_AIRS_MODULES) $(DECODE_L2_AIRS_OBJS) \
 -L$(HDFEOS_LIB) -L$(HDF_LIB) -I$(HDFEOS_INC) -I$(HDF_INC) -lhdfeos -lGctp -lmfhdf -ldf \
 -L$(ZLIBLIB) -lz -L$(JPEGLIB) -ljpeg -lm $(EXTRA_LIBS)
