# Don't optimise these big routines

module_dm.o : 
	@ $(RM) $@
	@ $(SED_FTN) $*.F > $*.b 
	  $(CPP) $(CPPFLAGS) $(FPPFLAGS) $*.b  > $*.f
	@ $(RM) $*.b
	  $(FC) -c $(FCFLAGS_NOOPT) $*.f

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

# DO NOT DELETE
