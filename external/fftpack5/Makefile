include ../../configure.wrfvar

FFTPACK5_OBJS =	fftpack5.o

all : $(FFTPACK5_OBJS)
	$(AR) libfftpack5.a $(FFTPACK5_OBJS)

fftpack5.o:	         \
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
			$(CPP) $(CPPFLAGS)  $(FPPFLAGS) fftpack5.f90 > fftpack5.f
			$(FFC) -c  $(FIXEDFLAGS) -I$(BUILD) fftpack5.f

clean :
	$(RM) $(FFTPACK_OBJS) *.mod *.f

superclean : clean

##############################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
