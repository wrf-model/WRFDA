include ../../configure.wrfvar

LAPACK_OBJS = lapack.o

all : $(LAPACK_OBJS)
	$(AR) liblapack.a $(LAPACK_OBJS)

lapack.o:		lapack.f90 \
			../blas/blas.o \
			dlae2.inc \
			dlaev2.inc \
			dlamc1.inc \
			dlamc2.inc \
			dlamc3.inc \
			dlamc4.inc \
			dlamc5.inc \
			dlamch.inc \
			dlanst.inc \
			dlansy.inc \
			dlapy2.inc \
			dsyev.inc
			$(CPP) $(CPPFLAGS)  $(FPPFLAGS) lapack.f90 > lapack.f
			$(FFC) -c  $(FIXEDFLAGS) -I../blas lapack.f

clean :
	@ $(RM) *.o *.f *.mod

superclean : clean
        

##############################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
