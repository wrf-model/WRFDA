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
else
   llq >& /dev/null
   if ($status == 0) then
      setenv SUBMIT LoadLeveller
   else
      setenv SUBMIT none
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
   echo "PROCESSOR   " $PROCESSOR
endif
if ($?SUBMIT) then
   echo "SUBMIT      " $SUBMIT
endif
if ($?RUN_CMD) then
   echo "RUN_CMD     " $RUN_CMD
endif
if ($?MPIHOME) then
   echo "MPIHOME     " $MPIHOME
endif
if ($?RTTOV) then
   echo "RTTOV       " $RTTOV
endif
if ($?CRTM) then
   echo "CRTM        " $CRTM
endif
if ($?NETCDF) then
   echo "NETCDF      " $NETCDF
endif
if ($?BLAS) then
   echo "BLAS        " $BLAS
endif
if ($?LAPACK) then
   echo "LAPACK      " $LAPACK
endif
if ($?FFTPACK5) then
   echo "FFTPACK5    " $FFTPACK5
endif
if ($?BUFR) then
   echo "BUFR        " $BUFR
endif
if ($?MAKEDEPF90) then
   echo "MAKEDEPF90  " $MAKEDEPF90
endif
