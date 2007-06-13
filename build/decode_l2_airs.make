DECODE_L2_AIRS_OBJS = geth_newdate.o calc_rh.o module_read_airs.o decode_airs.o

decode_airs : $(DECODE_L2_AIRS_OBJS)
	$(SFC) -o $@.exe $(DECODE_L2_AIRS_OBJS) $(HDFEOS_LIB) $(HDF4_LIB) \
           $(ZLIB_LIB) $(JPEG_LIB) $(LOCAL_LIB)
