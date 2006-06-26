DECODE_L2_AIRS_OBJS = geth_newdate.o calc_rh.o 

DECODE_L2_AIRS_MODULES =  module_read_airs.o

HDFEOS_LIB =  $(EXTERNAL)/HDF-EOS/hdfeos/lib
HDFEOS_INC =  $(EXTERNAL)/HDF-EOS/hdfeos/include
HDFINC     =  $(EXTERNAL)/HDF-EOS/HDF4.2r1/NewHDF/include
HDFLIB     =  $(EXTERNAL)/HDF-EOS/HDF4.2r1/NewHDF/lib
JPEGLIB    =  $(EXTERNAL)/jpeg/jpeg-6b/lib
ZLIBLIB    =  $(EXTERNAL)/zlib/zlib-1.2.3/lib

decode_airs : decode_airs.o $(DECODE_L2_AIRS_MODULES) $(DECODE_L2_AIRS_OBJS)
	$(FC) -o $@.exe decode_airs.o $(DECODE_L2_AIRS_MODULES) $(DECODE_L2_AIRS_OBJS) \
 -L${HDFEOS_LIB} -L${HDFLIB} -I${HDFEOS_INC} -I${HDFINC} -lhdfeos -lGctp -lmfhdf -ldf \
 -L${ZLIBLIB} -lz -L${JPEGLIB} -ljpeg -lm
