MACHINE=`uname -n`
FC=${1:-g95}
CC=${2:-gcc}

PROCESSOR=`uname -p`

# Wait until files on wrfhelp
#if test -d /mmm/users/wrfhelp; then
#   WRFHELP=/mmm/users/wrfhelp
#elif test -d /data3/mp/wrfhelp; then
#   WRFHELP=/data3/mp/wrfhelp
#elif test -d /users/wrfhelp; then
#   WRFHELP=/users/wrfhelp
#elif test -d ~wrfhelp; then
#   WRFHELP=~wrfhelp
#elif test -d /mmm/users/bray; then
if test -d /mmm/users/bray; then
   WRFHELP=/mmm/users/bray
elif test -d /data7/da/bray; then
   WRFHELP=/data7/da/bray
elif test -d /users/bray; then
   WRFHELP=/users/bray
elif test -d ~bray; then
   WRFHELP=~bray
else
   WRFHELP=~
fi

if test $FC = xlf; then
   if test -d /opt/ibmcmp/xlf/8.1; then
      export PATH=/opt/ibmcmp/xlf/8.1/bin:$PATH
      export MANPATH=/opt/ibmcmp/xlf/8.1/man/en_US/man:$MANPATH
   fi
fi

if test $CC = xlc; then
   if test -d /usr/vac/bin; then
      export PATH=/usr/vac/bin:/usr/vacpp/bin:$PATH
   fi
fi

if test $FC = g95; then
   if test -d ${WRFHELP}/g95/g95_${PROCESSOR}; then
      export PATH=${WRFHELP}/g95/g95_${PROCESSOR}:$PATH
   fi
fi

if test -d ${WRFHELP}/netcdf/netcdf-3.6.1_${FC}_${PROCESSOR}; then
  export NETCDF=${WRFHELP}/netcdf/netcdf-3.6.1_${FC}_${PROCESSOR}
fi
if test -d ${WRFHELP}/rttov/rttov85_${FC}_${PROCESSOR}; then
   export RTTOV=${WRFHELP}/rttov/rttov85_${FC}_${PROCESSOR}
fi
if test -d ${WRFHELP}/crtm/crtm_${FC}_${PROCESSOR}; then
   export CRTM=${WRFHELP}/crtm/crtm_${FC}_${PROCESSOR}
fi
if test -d ${WRFHELP}/mpich/mpich-1.2.7p1_${FC}_${PROCESSOR}; then
   export MPICH=${WRFHELP}/mpich/mpich-1.2.7p1_${FC}_${PROCESSOR}
fi
if test -d ${WRFHELP}/blas/blas_${FC}_${PROCESSOR}; then
   export BLAS=${WRFHELP}/blas/blas_${FC}_${PROCESSOR}
fi
if test -d ${WRFHELP}/lapack/lapack_${FC}_${PROCESSOR}; then
   export LAPACK=${WRFHELP}/lapack/lapack_${FC}_${PROCESSOR}
fi
if test -d ${WRFHELP}/fftpack5/fftpack5_${FC}_${PROCESSOR}; then
   export FFTPACK5=${WRFHELP}/fftpack5/fftpack5_${FC}_${PROCESSOR}
fi
if test -d ${WRFHELP}/bufr/bufr_ncep_nco_${FC}_${PROCESSOR}; then
   export BUFR=${WRFHELP}/bufr/bufr_ncep_nco_${FC}_${PROCESSOR}
fi
if test -d ${WRFHELP}/makedepf90/makedepf90-2.8.8_${CC}_${PROCESSOR}; then
   export MAKEDEPF90=${WRFHELP}/makedepf90/makedepf90-2.8.8_${CC}_${PROCESSOR}
fi

# -----------------------------------------

# mpich2

#if test -d ${WRFHELP}/mpich/mpich2-1.0.3_${FC}_${PROCESSOR}; then
#   export MPICH=${WRFHELP}/mpich/mpich2-1.0.3_${FC}_${PROCESSOR}
#fi
#if test -d ~bray/mpich/mpich2-1.0.3_${FC}_${PROCESSOR}; then
#   export MPICH=~bray/mpich/mpich2-1.0.3_${FC}_${PROCESSOR}
#fi
#if test -d /Volumes/$MACHINE/bray/tools/mpich2-1.0.3_${FC}_${PROCESSOR}; then
#   export MPICH=/Volumes/$MACHINE/bray/tools/mpich2-1.0.3_${FC}_${PROCESSOR}
#fi

if test -d /usr/lpp/ppe.poe; then
   export MPICH=/usr/lpp/ppe.poe
fi

# Lightning

if test $MACHINE = "ln0126en" -o $MACHINE = "ln0127en"; then 
   if test $FC = pathscale; then
      export MPICH=/contrib/2.6/mpich-gm/1.2.6..14a-pathscale-2.4-64
   fi
   if test $FC = pgi; then
      export MPICH=/usr/local/mpich-gm/mpichgm-1.2.6..14a-64
   fi
   if test $FC = ifort; then
      . /contrib/2.6/intel/9.1.036-64/bin/ifortvars.sh
      export MPICH=/contrib/2.6/mpich-gm/1.2.6..14a-intel-9.1.042-64
   fi
fi

# Crayx1

if test $MACHINE = "gold.us.cray.com"; then 
   if test $FC = crayx1; then
      module use /opt/ctl/modulefiles /opt/PE/modulefiles
      module load PrgEnv.56.newest
      module list
      export NETCDF=/ptmp/pjj/netcdf-3.5.1-x1
      export RTTOV=~n12138/rttov/rttov85_crayx1
      export CRTM=~n12138/crtm/crtm_crayx1
      export MPICH=/opt/cpkg/v4/mpich2/1.0
      export WRF_OS=crayx1
   fi
fi

export MPIHOME=$MPICH
export PATH=$MPICH/bin:$MAKEDEPF90:$PATH
export MANPATH=$MPICH/man:$MANPATH

if test $FC = 'g95'; then
   export G95_ENDIAN=BIG
   export G95_FPU_INVALID=${G95_FPU_INVALID:-T}
   export G95_FPU_ZERODIV=${G95_FPU_ZERODIV:-T}
   export G95_FPU_OVERFLOW=${G95_FPU_OVERFLOW:-F}
   export G95_FPU_UNDERFLOW=${G95_FPU_UNDERFLOW:-F}
   export G95_FPU_INEXACT=${G95_FPU_INEXACT:-F}
   export G95_FPU_EXCEPTIONS=${G95_FPU_EXCEPTIONS:-F}
   export G95_UNBUFFERED_ALL=${G95_UNBUFFERED_ALL:-T}
   export G95_MEM_INIT=${G95_MEM_INIT:-0x00}
   export G95_MEM_MAXALLOC=${G95_MEM_MAXALLOC:-F}
   export G95_MEM_SEGMENTS=0
fi

ls -l $MPICH/lib/*.a
ls -l $RTTOV/lib/*.a
ls -l $CRTM/lib/*.a
ls -l $NETCDF/lib/*.a
ls -l $BLAS/*.a
ls -l $LAPACK/*.a
ls -l $FFTPACK5/*.a
ls -l $BUFR/*.a
ls -l $MAKEDEPF90/makedepf90
echo
echo "MPICH       " $MPICH
echo "RTTOV       " $RTTOV
echo "CRTM        " $CRTM
echo "NETCDF      " $NETCDF
echo "BLAS        " $BLAS
echo "LAPACK      " $LAPACK
echo "FFTPACK5    " $FFTPACK5
echo "BUFR        " $BUFR
echo "MAKEDEPF90  " $MAKEDEPF90
