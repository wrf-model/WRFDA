BLAS_OBJS =	BLAS.o 

include ../../configure.wrf


BLAS.o:	         \
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
			xerbla.inc \
			blas.f90
			$(CPP) $(CPPFLAGS) blas.f90 > blas.f
			$(FFC) -c $(FIXEDFLAGS_WRFVAR) $(MODULE_DIRS) blas.f

##############################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
