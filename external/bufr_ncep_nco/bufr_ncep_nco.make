BUFR_OBJS        =	bufrlib.o 		\
	                bort_exit.o 		\
	                restd.o 		\
	                wrdesc.o

libbufr.a : $(BUFR_OBJS)
	$(AR) libbufr.a $(BUFR_OBJS)


bufrlib.prm:		bufrlib.param
			$(RM) $@
			$(CPP) $(CPPFLAGS) $(FPPFLAGS_BUFR) bufrlib.param > bufrlib.prm

bufrlib.o:		bufrlib.f90 bufrlib.prm
			$(RM) $@
			$(CPP) $(CPPFLAGS) $(FPPFLAGS) bufrlib.f90 > bufrlib.f
			$(FFC) -c $(FIXEDFLAGS_BUFR) bufrlib.f

bort_exit.o:		bort_exit.c
			$(RM) $@
			$(CC) -c $(CCFLAGS_BUFR) bort_exit.c

restd.o:		restd.c
			$(RM) $@
			$(CC) -c $(CCFLAGS_BUFR) restd.c

wrdesc.o:		wrdesc.c
			$(RM) $@
			$(CC) -c $(CCFLAGS_BUFR) wrdesc.c

##############################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

