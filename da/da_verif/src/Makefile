#

include ../compiler_macros

OBJS		= \
		get_diag.o			\
		Define_Constants.o	\
		Initialize.o

get_diag.exe:	$(OBJS)
		$(FC) -o $@ $(FCFLAGS) $(OBJS) $(LIBS)

get_diag.o: \
		Define_Constants.o	\
		Initialize.o		\
		get_diag.F
		$(FC) -c $(FCFLAGS) $(FPPFLAG) \
		get_diag.F

Define_Constants.o: \
		Define_Constants.F
		$(FC) -c $(FCFLAGS) $(FPPFLAG) \
		Define_Constants.F

Initialize.o: \
		Initialize.F
		$(FC) -c $(FCFLAGS) $(FPPFLAG) \
		Initialize.F

clean:	
	$(RM) $(RM_LIST)

#-----------------------------------------------------------------------------
# Don't touch anything below this line
#-----------------------------------------------------------------------------
.f.o:
	@echo " "
	@$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f
.F.f:
	@echo " "
	@$(RM) $@
	$(CPP) $(CPPFLAGS) $*.F > $@
.F.o:
	@echo " "
	@$(RM) $*.f
	$(CPP) $(CPPFLAGS) $*.F > $*.f
	$(FC) -c $(FCFLAGS) $*.f
	@$(RM) $*.f
.F.mod:
	@echo " "
	@$(RM) $*.f $*.o
	$(CPP) $(CPPFLAGS) $*.F > $*.f
	$(FC) -c $(FCFLAGS) $*.f
	@$(RM) $*.f
.f.mod:
	@echo " "
	$(FC) -c $(FCFLAGS) $<
