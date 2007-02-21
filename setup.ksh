MACHINE=${MACHINE:-`uname -n`}

export EXT_DIR=${EXT_DIR:-~wrfhelp/external}

if ! test -d $EXT_DIR; then
   echo "No directory EXT_DIR=$EXT_DIR"
fi

FC=${1:-g95}
CC=${2:-gcc}

# Search for queuing systems.
# Don't use which, as it always returns 0 on BSD
bjobs > /dev/null 2>&1
if test $? = 0 ; then
   export SUBMIT=LSF
   export LSF_PTILE=${LSF_PTILE:-8}
   export SUBMIT_OPTIONS1="#BSUB -x" # exclusivity
   export SUBMIT_OPTIONS2="#BSUB -a mpich_gm"
   export SUBMIT_OPTIONS3="#BSUB -R span[ptile=$LSF_PTILE]"
   export SUBMIT_OPTIONS4="#BSUB -W 60"
   export SUBMIT_OPTIONS5="#BSUB -P $PROJECT"
else
   llq > /dev/null 2>&1
   if test $? = 0 ; then
      export SUBMIT=LoadLeveller
      export SUBMIT_OPTIONS1='# @ job_type         = parallel'
      export SUBMIT_OPTIONS2='# @ environment      = COPY_ALL'
      export SUBMIT_OPTIONS3='# @ notification     = never'
      export SUBMIT_OPTIONS4='# @ network.MPI      = css0,shared,ip'
      export SUBMIT_OPTIONS5='# @ checkpoint       = no'
      export SUBMIT_OPTIONS6='# @ wall_clock_limit = 01:00:00'
      export SUBMIT_OPTIONS7='# @ class            = share'
      export SUBMIT_OPTIONS8='# @ node_usage       = shared'
   else
      csh -c "which qsub" >/dev/null 2>&1
      # could be SGE of course, so need better way to check
      if test $? = 0; then
         export SUBMIT=PBS
      else
         export SUBMIT=none
      fi
   fi
fi

# Bloody Unix people can't even report processor class properly
# across different machines or between ksh/bash on Linux
# They all need their heads banged together
# This kludge should give powerpc/i686

if test `uname` = "AIX"; then
   # Thanks Aix for reporting a hex string with -m, when
   # all I wanted was powerpc
   export PROCESSOR=${PROCESSOR:-`uname -p`}
else
   # Thanks Linux for either reporting nothing with -n,
   # or different values for ksh and bash, FFS
   export PROCESSOR=${PROCESSOR:-`uname -m`}
fi
if test "$PROCESSOR" = "Power Macintosh"; then
   export PROCESSOR=powerpc
fi

if test $FC = g95; then
   export G95_ENDIAN=BIG
fi

if test -d ${EXT_DIR}/netcdf/netcdf-3.6.1_${FC}_${PROCESSOR}; then
  export NETCDF=${EXT_DIR}/netcdf/netcdf-3.6.1_${FC}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/rttov/rttov85_${FC}_${PROCESSOR}; then
   export RTTOV=${EXT_DIR}/rttov/rttov85_${FC}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/crtm/crtm_${FC}_${PROCESSOR}; then
   export CRTM=${EXT_DIR}/crtm/crtm_${FC}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/mpi/mpich-1.2.7p1_${FC}_${PROCESSOR}; then
   export MPIHOME=${EXT_DIR}/mpi/mpich-1.2.7p1_${FC}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/blas/blas_${FC}_${PROCESSOR}; then
   export BLAS=${EXT_DIR}/blas/blas_${FC}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/lapack/lapack_${FC}_${PROCESSOR}; then
   export LAPACK=${EXT_DIR}/lapack/lapack_${FC}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/fftpack5/fftpack5_${FC}_${PROCESSOR}; then
   export FFTPACK5=${EXT_DIR}/fftpack5/fftpack5_${FC}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/bufr/bufr_ncep_nco_${FC}_${PROCESSOR}; then
   export BUFR=${EXT_DIR}/bufr/bufr_ncep_nco_${FC}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/makedepf90/makedepf90-2.8.8_${CC}_${PROCESSOR}; then
   export MAKEDEPF90=${EXT_DIR}/makedepf90/makedepf90-2.8.8_${CC}_${PROCESSOR}
fi

if test -d /usr/lpp/ppe.poe; then
   export MPIHOME=/usr/lpp/ppe.poe
fi

# Lightning

if test $MACHINE = "lightning"; then 
   if test $FC = pathscale; then
      export MPIHOME=/contrib/2.6/mpich-gm/1.2.6..14a-pathscale-2.4-64
   fi
   if test $FC = pgi; then
      export MPIHOME=/usr/local/mpich-gm/mpichgm-1.2.6..14a-64
   fi
   if test $FC = ifort; then
      . /contrib/2.6/intel/9.1.036-64/bin/ifortvars.sh
      export MPIHOME=/contrib/2.6/mpich-gm/1.2.6..14a-intel-9.1.042-64
   fi
fi

export LINUX_MPIHOME=$MPIHOME
export PATH=$MPIHOME/bin:$MAKEDEPF90:$PATH
export MANPATH=$MPIHOME/man:$MANPATH

echo "PROCESSOR       " $PROCESSOR
echo "MPIHOME         " $MPIHOME
echo "RTTOV           " $RTTOV
echo "CRTM            " $CRTM
echo "NETCDF          " $NETCDF
echo "BLAS            " $BLAS
echo "LAPACK          " $LAPACK
echo "FFTPACK5        " $FFTPACK5
echo "BUFR            " $BUFR
echo "MAKEDEPF90      " $MAKEDEPF90
echo "SUBMIT          " $SUBMIT
if test "$SUBMIT_OPTIONS1." != '.'; then
   echo "SUBMIT_OPTIONS1  $SUBMIT_OPTIONS1"
fi
if test "$SUBMIT_OPTIONS2." != '.'; then
   echo "SUBMIT_OPTIONS2  $SUBMIT_OPTIONS2"
fi
if test "$SUBMIT_OPTIONS3." != '.'; then
   echo "SUBMIT_OPTIONS3  $SUBMIT_OPTIONS3"
fi
if test "$SUBMIT_OPTIONS4." != '.'; then
   echo "SUBMIT_OPTIONS4  $SUBMIT_OPTIONS4"
fi
if test "$SUBMIT_OPTIONS5." != '.'; then
   echo "SUBMIT_OPTIONS5  $SUBMIT_OPTIONS5"
fi
if test "$SUBMIT_OPTIONS6." != '.'; then
   echo "SUBMIT_OPTIONS6  $SUBMIT_OPTIONS6"
fi
if test "$SUBMIT_OPTIONS7." != '.'; then
   echo "SUBMIT_OPTIONS7  $SUBMIT_OPTIONS7"
fi
if test "$SUBMIT_OPTIONS8." != '.'; then
   echo "SUBMIT_OPTIONS8  $SUBMIT_OPTIONS8"
fi
if test "$SUBMIT_OPTIONS9." != '.'; then
   echo "SUBMIT_OPTIONS9  $SUBMIT_OPTIONS9"
fi
if test "$SUBMIT_OPTIONS10." != '.'; then
   echo "SUBMIT_OPTIONS10 $SUBMIT_OPTIONS10"
fi
