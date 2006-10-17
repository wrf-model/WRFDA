MACHINE=`hostname`
FC=${1:-g95}
CC=${2:-gcc}

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
   if test -d /data7/da/bray/g95; then
      export PATH=/data7/da/bray/g95:$PATH
   fi
   if test -d ~bray/g95; then
      export PATH=~bray/g95:$PATH
   fi
   if test -d /Volumes/$MACHINE/bray/tools/g95; then
      export PATH=/Volumes/$MACHINE/bray/tools/g95:$PATH
   fi
fi


# List options in order of increasing preference

if test -d /data7/da/bray/netcdf/netcdf-3.6.1_${FC}; then
  export NETCDF=/data7/da/bray/netcdf/netcdf-3.6.1_${FC}
fi
if test -d /data7/da/bray/rttov/rttov85_${FC}; then
   export RTTOV=/data7/da/bray/rttov/rttov85_${FC}
fi
if test -d /data7/da/bray/crtm/crtm_${FC}; then
   export CRTM=/data7/da/bray/crtm/crtm_${FC}
fi
if test -d /data7/da/bray/mpich/mpich-1.2.7p1_${FC}; then
   export MPICH=/data7/da/bray/mpich/mpich-1.2.7p1_${FC}
fi
if test -d /data7/da/bray/blas/blas_${FC}; then
   export BLAS=/data7/da/bray/blas/blas_${FC}
fi
if test -d /data7/da/bray/lapack/lapack_${FC}; then
   export LAPACK=/data7/da/bray/lapack/lapack_${FC}
fi
if test -d /data7/da/bray/fftpack5/fftpack5_${FC}; then
   export FFTPACK5=/data7/da/bray/fftpack5/fftpack5_${FC}
fi
if test -d /data7/da/bray/bufr_ncep_nco/bufr_ncep_nco_${FC}; then
   export BUFR=/data7/da/bray/bufr_ncep_nco/bufr_ncep_nco_${FC}
fi
if test -d /data7/da/bray/makedepf90/makedepf90-2.8.8_${CC}; then
   export MAKEDEPF90=/data7/da/bray/makedepf90/makedepf90-2.8.8_${CC}
fi

# -----------------------------------------

if test -d ~bray/netcdf/netcdf-3.6.1_${FC}; then
   export NETCDF=~bray/netcdf/netcdf-3.6.1_${FC}
fi
if test -d ~bray/rttov/rttov85_${FC}; then
   export RTTOV=~bray/rttov/rttov85_${FC}
fi
if test -d ~bray/crtm/crtm_${FC}; then
   export CRTM=~bray/crtm/crtm_${FC}
fi
if test -d ~bray/mpich/mpich-1.2.7p1_${FC}; then
   export MPICH=~bray/mpich/mpich-1.2.7p1_${FC}
fi
if test -d ~bray/lapack/lapack_${FC}; then
   export LAPACK=~bray/lapack/lapack_${FC}
fi
if test -d ~bray/blas/blas_${FC}; then
   export BLAS=~bray/blas/blas_${FC}
fi
if test -d ~bray/fftpack5/fftpack5_${FC}; then
   export FFTPACK5=~bray/fftpack5/fftpack5_${FC}
fi
if test -d ~bray/bufr_ncep_nco/bufr_ncep_nco_${FC}; then
   export BUFR=~bray/bufr_ncep_nco/bufr_ncep_nco_${FC}
fi
if test -d ~bray/makedepf90/makedepf90-2.8.8_${CC}; then
   export MAKEDEPF90=~bray/makedepf90/makedepf90-2.8.8_${CC}
fi

# -----------------------------------------

if test -d ~/netcdf/netcdf-3.6.1_${FC}; then
   export NETCDF=~/netcdf/netcdf-3.6.1_${FC}
fi
if test -d ~/rttov/rttov85_${FC}; then
   export RTTOV=~/rttov/rttov85_${FC}
fi
if test -d ~/crtm/crtm_${FC}; then
   export CRTM=~/crtm/crtm_${FC}
fi
if test -d ~/mpich/mpich-1.2.7p1_${FC}; then
   export MPICH=~/mpich/mpich-1.2.7p1_${FC}
fi
if test -d ~/blas/blas_${FC}; then
   export BLAS=~/blas/blas_${FC}
fi
if test -d ~/lapack/lapack_${FC}; then
   export LAPACK=~/lapack/lapack_${FC}
fi
if test -d ~/fftpack5/fftpack5_${FC}; then
   export FFTPACK5=~/fftpack5/fftpack5_${FC}
fi
if test -d ~/bufr_ncep_nco/bufr_ncep_nco_${FC}; then
   export BUFR=~/bufr_ncep_nco/bufr_ncep_nco_${FC}
fi
if test -d  ~/makedepf90/makedepf90-2.8.8_${CC}; then
   export MAKEDEPF90=~/makedepf90/makedepf90-2.8.8_${CC}
fi

# -----------------------------------------

if test -d /Volumes/$MACHINE/bray/tools/netcdf-3.6.1_${FC}; then
   export NETCDF=/Volumes/$MACHINE/bray/tools/netcdf-3.6.1_${FC}
fi
if test -d /Volumes/$MACHINE/bray/tools/rttov85_${FC}; then
   export RTTOV=/Volumes/$MACHINE/bray/tools/rttov85_${FC}
fi
if test -d /Volumes/$MACHINE/bray/tools/crtm_${FC}; then
   export CRTM=/Volumes/$MACHINE/bray/tools/crtm_${FC}
fi
if test -d /Volumes/$MACHINE/bray/tools/mpich-1.2.7p1_${FC}; then
   export MPICH=/Volumes/$MACHINE/bray/tools/mpich-1.2.7p1_${FC}
fi
if test -d /Volumes/$MACHINE/bray/tools/blas_${FC}; then
   export BLAS=/Volumes/$MACHINE/bray/tools/blas_${FC}
fi
if test -d /Volumes/$MACHINE/bray/tools/lapack_${FC}; then
   export LAPACK=/Volumes/$MACHINE/bray/tools/lapack_${FC}
fi
if test -d /Volumes/$MACHINE/bray/tools/fftpack5_${FC}; then
   export FFTPACK5=/Volumes/$MACHINE/bray/tools/fftpack5_${FC}
fi
if test -d /Volumes/$MACHINE/bray/tools/bufr_ncep_nco_${FC}; then
   export BUFR=/Volumes/$MACHINE/bray/tools/bufr_ncep_nco_${FC}
fi
if test -d  /Volumes/$MACHINE/bray/tools/makedepf90-2.8.8_${CC}; then
   export MAKEDEPF90=/Volumes/$MACHINE/bray/tools/makedepf90-2.8.8_${CC}
fi

# -----------------------------------------

# mpich2

#if test -d /data7/da/bray/mpich/mpich2-1.0.3_${FC}; then
#   export MPICH=/data7/da/bray/mpich/mpich2-1.0.3_${FC}
#fi
#if test -d ~bray/mpich/mpich2-1.0.3_${FC}; then
#   export MPICH=~bray/mpich/mpich2-1.0.3_${FC}
#fi
#if test -d /Volumes/$MACHINE/bray/tools/mpich2-1.0.3_${FC}; then
#   export MPICH=/Volumes/$MACHINE/bray/tools/mpich2-1.0.3_${FC}
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
fi

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
ls -l $MPICH/lib/*.a
ls -l $RTTOV/lib/*.a
ls -l $CRTM/lib/*.a
ls -l $NETCDF/lib/*.a
ls -l $BLAS/*.a
ls -l $LAPACK/*.a
ls -l $FFTPACK5/*.a
ls -l $BUFR/*.a
ls -l $MAKEDEPF90/makedepf90
