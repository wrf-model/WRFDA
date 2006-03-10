ESMF_WRAPPER_OBJS = ESMF_Indirection.o ESMF_Mod_Wrapper.o

libesmfwrapper.a : $(ESMF_WRAPPER_OBJS)
	$(AR) ru libesmfwrapper.a $(ESMF_WRAPPER_OBJS)
	$(RANLIB) libesmfwrapper.a

# DEPENDENCIES : only dependencies after this line 

ESMF_Indirection.o : 

ESMF_Mod_Wrapper.o : ESMF_Indirection.o

