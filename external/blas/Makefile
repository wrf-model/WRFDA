include ../../configure.wrf

BLAS_OBJS =	blas.o 

all : $(BLAS_OBJS)
	$(AR) libblas.a $(BLAS_OBJS)

blas.o:	         blas.f90 \
			daxpy.inc  \
			dcopy.inc  \
			ddot.inc   \
			dgemm.inc  \
			dgemv.inc  \
			dger.inc   \
			dnrm2.inc  \
			dscal.inc  \
			dswap.inc  \
			dsymv.inc  \
			dsyr2.inc  \
			dsyr2k.inc \
			dtrmm.inc  \
			dtrmv.inc  \
			lsame.inc  \
			xerbla.inc
			$(CPP) $(CPPFLAGS) blas.f90 > blas.f
			$(SFC) -c $(FCFIXED) $(MODULE_DIRS) $(PROMOTION) blas.f

clean :
	@ $(RM) *.o *.f *.mod

superclean : clean

##############################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
