FFTPACK_OBJS =	da_fftpack5.o

include ../../configure.wrf

fftpack5.o:	         \
			../da/da_constants.o		\
			r1f2kb.inc  \
			r1f2kf.inc  \
			r1f3kb.inc  \
			r1f3kf.inc  \
			r1f4kb.inc  \
			r1f4kf.inc  \
			r1f5kb.inc  \
			r1f5kf.inc  \
			r1fgkb.inc  \
			r1fgkf.inc  \
			rfft1b.inc  \
			rfft1f.inc  \
			rfft1i.inc  \
			rfftb1.inc  \
			rfftf1.inc  \
			rffti1.inc  \
			xerfft.inc  \
			fftpack5.f90
			$(CPP) $(CPPFLAGS) fftpack5.f90 > fftpack5.f
			$(FFC) -c $(FIXEDFLAGS_WRFVAR) $(MODULE_DIRS) fftpack5.f

##############################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
