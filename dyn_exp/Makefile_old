include ../configure.wrf

MODULES =                 		\
        module_exp.o         		\
	$(CASE_MODULE)

# possible CASE_MODULE settings
#	module_initialize_exp.o        

OBJS    = 		          	\
	solve_exp.o init_modules_exp.o

LIBTARGET    =  dyn_exp
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS)
		$(AR) ../main/libwrflib.a $(MODULES) $(OBJS)

clean:
	@ $(RM) *.o *.f

superclean : clean

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

solve_exp.o:   module_exp.o

