RSL_LITE_OBJS = c_code.o buf_for_proc.o rsl_malloc.o rsl_bcast.o \
   task_for_point.o period.o swap.o cycle.o f_pack.o f_xpose.o

librsl_lite.a:          $(RSL_LITE_OBJS)
			$(RM) librsl_lite.a
			$(AR) librsl_lite.a $(RSL_LITE_OBJS)
			
c_code.o : c_code.c

buf_for_proc.o : buf_for_proc.c

rsl_malloc.o : rsl_malloc.c

rsl_base.o : rsl_bcast.c

task_for_point.o : task_for_point.c

period.o : period.c

swap.o : swap.c

cycle.o : cycle.c

f_pack.o : f_pack.F90

f_xpose.o : f_xpose.F90
