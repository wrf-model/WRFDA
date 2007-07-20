# RSL_LITE

RSL_LITE_OBJS = c_code.o buf_for_proc.o rsl_malloc.o rsl_bcast.o \
   task_for_point.o period.o swap.o cycle.o f_pack.o f_xpose.o

librsl_lite.a : $(RSL_LITE_OBJS)
	$(RM) librsl_lite.a
	$(AR) librsl_lite.a $(RSL_LITE_OBJS)
			
c_code.o : c_code.c
	$(CC) -c $(CCFLAGS) $(RSL_CCFLAGS) $*.c

buf_for_proc.o : buf_for_proc.c
	$(CC) -c $(CCFLAGS) $(RSL_CCFLAGS) $*.c

rsl_malloc.o : rsl_malloc.c
	$(CC) -c $(CCFLAGS) $(RSL_CCFLAGS) $*.c

rsl_bcast.o : rsl_bcast.c
	$(CC) -c $(CCFLAGS) $(RSL_CCFLAGS) $*.c

task_for_point.o : task_for_point.c
	$(CC) -c $(CCFLAGS) $(RSL_CCFLAGS) $*.c

period.o : period.c
	$(CC) -c $(CCFLAGS) $(RSL_CCFLAGS) $*.c

swap.o : swap.c
	$(CC) -c $(CCFLAGS) $(RSL_CCFLAGS) $*.c

cycle.o : cycle.c
	$(CC) -c $(CCFLAGS) $(RSL_CCFLAGS) $*.c

f_pack.o : f_pack.F90

f_xpose.o : f_xpose.F90
