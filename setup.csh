if (! $?MACHINE) then
   setenv MACHINE `uname -n`
endif

if (! $?EXT_DIR) then
   setenv EXT_DIR ~wrfhelp/external
endif

if (! -d $EXT_DIR) then
   echo "Cannot find EXT_DIR=$EXT_DIR"
endif

# Search for queuing systems.
# Don't use which, as it always returns 0 on BSD
bjobs >& /dev/null
if ($status == 0) then
   setenv SUBMIT LSF
   if (! $?LSF_PTILE) then
      setenv LSF_PTILE 8
   endif
   setenv SUBMIT_OPTIONS1 "#BSUB -x" # exclusivity
   setenv SUBMIT_OPTIONS2 "#BSUB -a mpich_gm"
   setenv SUBMIT_OPTIONS3 "#BSUB -R span[ptile=$LSF_PTILE]"
   setenv SUBMIT_OPTIONS4 "#BSUB -W 60"
   setenv SUBMIT_OPTIONS5 "#BSUB -P $PROJECT"
else
   llq >& /dev/null
   if ($status == 0) then
      setenv SUBMIT LoadLeveller
      setenv SUBMIT_OPTIONS1 '# @ job_type         = parallel'
      setenv SUBMIT_OPTIONS2 '# @ environment      = COPY_ALL'
      setenv SUBMIT_OPTIONS3 '# @ notification     = never'
      setenv SUBMIT_OPTIONS4 '# @ network.MPI      = css0,shared,ip'
      setenv SUBMIT_OPTIONS5 '# @ checkpoint       = no'
      setenv SUBMIT_OPTIONS6 '# @ wall_clock_limit = 01:00:00'
      setenv SUBMIT_OPTIONS7 '# @ class            = share'
      setenv SUBMIT_OPTIONS8 '# @ node_usage       = shared'
   else
      qsub >& /dev/null
      # could be SGE of course, so might need better way to check
      if ($status == 0) then
         setenv SUBMIT PBS
      else
         setenv SUBMIT none
      endif
   endif
endif

if (! $?PROCESSOR) then
   # Bloody Unix people can't even report processor class properly
   # across different machines or between ksh/bash on Linux
   # They all need their heads banged together
   # This kludge should give powerpc/i686
   if ( `uname` == "AIX" ) then
      # Thanks Aix for reporting a hex string with -m, when
      # all I wanted was powerpc
      setenv PROCESSOR `uname -p`
   else
      # Thanks Linux for either reporting nothing with -n,
      # or different values for ksh and bash, FFS
      setenv PROCESSOR `uname -m`
      if ("$PROCESSOR" == "Power Macintosh") then
         setenv PROCESSOR powerpc
      endif
   endif
endif 

if ( `uname` == "AIX" ) then
   # Brain dead Aix /bin/csh cannot handle arguments to 
   # sourced scripts, so force use of xlf, gcc
   setenv FC xlf
   setenv CC gcc
else
   if ("$1" != "") then
      setenv FC $1
   else
      setenv FC g95
   endif

   if ("$2" != "") then
      setenv CC $2
   else
      setenv CC gcc
   endif
endif

if ($FC == g95) then
   setenv G95_ENDIAN BIG
endif

# List options in order of increasing preference

if (-d ${EXT_DIR}/netcdf/netcdf-3.6.1_${FC}_${PROCESSOR}) then
   setenv NETCDF ${EXT_DIR}/netcdf/netcdf-3.6.1_${FC}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/rttov/rttov85_${FC}_${PROCESSOR}) then
   setenv RTTOV ${EXT_DIR}/rttov/rttov85_${FC}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/crtm/crtm_${FC}_${PROCESSOR}) then
   setenv CRTM ${EXT_DIR}/crtm/crtm_${FC}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/mpi/mpich-1.2.7p1_${FC}_${PROCESSOR}) then
   setenv MPIHOME ${EXT_DIR}/mpi/mpich-1.2.7p1_${FC}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/blas/blas_${FC}_${PROCESSOR}) then
   setenv BLAS ${EXT_DIR}/blas/blas_${FC}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/lapack/lapack_${FC}_${PROCESSOR}) then
   setenv LAPACK ${EXT_DIR}/lapack/lapack_${FC}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/fftpack5/fftpack5_${FC}_${PROCESSOR}) then
   setenv FFTPACK5 ${EXT_DIR}/fftpack5/fftpack5_${FC}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/bufr/bufr_ncep_nco_${FC}_${PROCESSOR}) then
   setenv BUFR ${EXT_DIR}/bufr/bufr_ncep_nco_${FC}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/makedepf90/makedepf90-2.8.8_${CC}_${PROCESSOR}) then
   setenv MAKEDEPF90 ${EXT_DIR}/makedepf90/makedepf90-2.8.8_${CC}_${PROCESSOR}
endif

if (-d /usr/lpp/ppe.poe) then
   setenv MPIHOME /usr/lpp/ppe.poe
endif

# Lightning

if ( $MACHINE == "lightning" ) then 
   if ( $FC == "pathscale" ) then
      setenv MPIHOME /contrib/2.6/mpich-gm/1.2.6..14a-pathscale-2.4-64
   endif
   if ( $FC == "pgi" ) then
      setenv MPIHOME /usr/local/mpich-gm/mpichgm-1.2.6..14a-64
   endif
   if ( $FC == "ifort" ) then
      source /contrib/2.6/intel/9.1.036-64/bin/ifortvars.csh
      setenv MPIHOME /contrib/2.6/mpich-gm/1.2.6..14a-intel-9.1.042-64
   endif
endif

setenv LINUX_MPIHOME $MPIHOME
setenv PATH $MPIHOME/bin:$PATH

if ($?MAKEDEPF90) then
   setenv PATH $MPIHOME/bin\:$MAKEDEPF90\:$PATH
endif

echo
if ($?PROCESSOR) then
   echo "PROCESSOR       " $PROCESSOR
endif
if ($?SUBMIT) then
   echo "SUBMIT          " $SUBMIT
endif
if ($?RUN_CMD) then
   echo "RUN_CMD         " $RUN_CMD
endif
if ($?MPIHOME) then
   echo "MPIHOME         " $MPIHOME
endif
if ($?RTTOV) then
   echo "RTTOV           " $RTTOV
endif
if ($?CRTM) then
   echo "CRTM            " $CRTM
endif
if ($?NETCDF) then
   echo "NETCDF          " $NETCDF
endif
if ($?BLAS) then
   echo "BLAS            " $BLAS
endif
if ($?LAPACK) then
   echo "LAPACK          " $LAPACK
endif
if ($?FFTPACK5) then
   echo "FFTPACK5        " $FFTPACK5
endif
if ($?BUFR) then
   echo "BUFR            " $BUFR
endif
if ($?MAKEDEPF90) then
   echo "MAKEDEPF90      " $MAKEDEPF90
endif
if ($?SUBMIT) then
   echo "SUBMIT          " $SUBMIT
endif
if ($?SUBMIT_OPTIONS1) then
   echo "SUBMIT_OPTIONS1  $SUBMIT_OPTIONS1"
endif
if ($?SUBMIT_OPTIONS2) then
   echo "SUBMIT_OPTIONS2  $SUBMIT_OPTIONS2"
endif
if ($?SUBMIT_OPTIONS3) then
   echo "SUBMIT_OPTIONS3  $SUBMIT_OPTIONS3"
endif
if ($?SUBMIT_OPTIONS4) then
   echo "SUBMIT_OPTIONS4  $SUBMIT_OPTIONS4"
endif
if ($?SUBMIT_OPTIONS5) then
   echo "SUBMIT_OPTIONS5  $SUBMIT_OPTIONS5"
endif
if ($?SUBMIT_OPTIONS6) then
   echo "SUBMIT_OPTIONS6  $SUBMIT_OPTIONS6"
endif
if ($?SUBMIT_OPTIONS7) then
   echo "SUBMIT_OPTIONS7  $SUBMIT_OPTIONS7"
endif
if ($?SUBMIT_OPTIONS8) then
   echo "SUBMIT_OPTIONS8  $SUBMIT_OPTIONS8"
endif
if ($?SUBMIT_OPTIONS9) then
   echo "SUBMIT_OPTIONS9  $SUBMIT_OPTIONS9"
endif
if ($?SUBMIT_OPTIONS10) then
   echo "SUBMIT_OPTIONS10 $SUBMIT_OPTIONS10"
endif
