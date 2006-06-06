BLAS_OBJS        =	blas.o

blas.o:	               blas.f90      \
		       daxpy.inc    \
		       dcopy.inc    \
		       ddot.inc     \
		       dgemm.inc    \
		       dgemv.inc    \
		       dger.inc     \
		       dnrm2.inc    \
		       dscal.inc    \
		       dswap.inc    \
		       dsymv.inc    \
		       dsyr2.inc    \
		       dsyr2k.inc   \
		       dtrmm.inc    \
		       dtrmv.inc    \
		       lsame.inc    \
		       xerbla.inc   \
		       module_wrf_error.o
			$(CPP) $(FPPFLAGS) blas.f90 > blas.f
			$(FFC) -c $(FIXEDFLAGS) blas.f

##############################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

