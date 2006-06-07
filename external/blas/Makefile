include ../../configure.wrf

BLAS_OBJS =	blas.o 

blas.o:	         \
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

clean :
	@ $(RM) *.o *.f *.mod

superclean : clean

##############################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
