setenv MACHINE `uname -n`
setenv PROCESSOR `uname -p`

if ( ( `hostname | cut -c 1-2` == bv ) ||  ( `hostname | cut -c 1-2` == bs ) ) then
   # Brain dead Aix /bin/csh cannot handle arguments to 
   # sourced scripts, so force use of xlf, gcc
   setenv FC xlf
   setenv CC xlc
else
   if ($?1) then
      setenv FC $1
   else
      setenv FC g95
   endif

   if ($?2) then
      setenv CC $2
   else
      setenv CC gcc
   endif
endif


# Wait until files on wrfhelp
#if (-d /mmm/users/wrfhelp) then
#   setenv WRFHELP /mmm/users/wrfhelp
#else if (-d /data3/mp/wrfhelp) then
#   setenv WRFHELP /data3/mp/wrfhelp
#else if (-d /users/wrfhelp) then
#   setenv WRFHELP /users/wrfhelp
#else if (-d ~wrfhelp}) then
#   setenv WRFHELP ~wrfhelp
#else if (-d /mmm/users/bray) then
if (-d /mmm/users/bray) then
   setenv WRFHELP /mmm/users/bray
else if (-d /data7/da/bray) then
   setenv WRFHELP /data7/da/bray
else if (-d /users/bray) then
   setenv WRFHELP /users/bray
else if (-d ~bray) then
   setenv WRFHELP=~bray
else
   setenv WRFHELP ~
endif


if ($FC == xlf) then
   if (-d /opt/ibmcmp/xlf/8.1) then
      setenv PATH /opt/ibmcmp/xlf/8.1/bin:$PATH
      setenv MANPATH /opt/ibmcmp/xlf/8.1/man/en_US/man:$MANPATH
   endif
endif

if ($CC == xlc) then
   if (-d /usr/vac/bin) then
      setenv PATH /usr/vac/bin:/usr/vacpp/bin:$PATH
   endif
endif

if ($FC == g95) then
   if (-d ${WRFHELP}/g95/g95) then
      setenv PATH ${WRFHELP}/g95/g95_${PROCESSOR}:$PATH
   endif
endif

# List options in order of increasing preference

if (-d ${WRFHELP}/netcdf/netcdf-3.6.1_${FC}_${PROCESSOR}) then
   setenv NETCDF ${WRFHELP}/netcdf/netcdf-3.6.1_${FC}_${PROCESSOR}
endif
if (-d ${WRFHELP}/rttov/rttov85_${FC}_${PROCESSOR}) then
   setenv RTTOV ${WRFHELP}/rttov/rttov85_${FC}_${PROCESSOR}
endif
if (-d ${WRFHELP}/crtm/crtm_${FC}_${PROCESSOR}) then
   setenv CRTM ${WRFHELP}/crtm/crtm_${FC}_${PROCESSOR}
endif
if (-d ${WRFHELP}/mpich/mpich-1.2.7p1_${FC}_${PROCESSOR}) then
   setenv MPICH ${WRFHELP}/mpich/mpich-1.2.7p1_${FC}_${PROCESSOR}
endif
if (-d ${WRFHELP}/blas/blas_${FC}_${PROCESSOR}) then
   setenv BLAS ${WRFHELP}/blas/blas_${FC}_${PROCESSOR}
endif
if (-d ${WRFHELP}/lapack/lapack_${FC}_${PROCESSOR}) then
   setenv LAPACK ${WRFHELP}/lapack/lapack_${FC}_${PROCESSOR}
endif
if (-d ${WRFHELP}/fftpack5/fftpack5_${FC}_${PROCESSOR}) then
   setenv FFTPACK5 ${WRFHELP}/fftpack5/fftpack5_${FC}_${PROCESSOR}
endif
if (-d ${WRFHELP}/bufr/bufr_ncep_nco_${FC}_${PROCESSOR}) then
   setenv BUFR ${WRFHELP}/bufr/bufr_ncep_nco_${FC}_${PROCESSOR}
endif
if (-d ${WRFHELP}/makedepf90/makedepf90-2.8.8_${CC}_${PROCESSOR}) then
   setenv MAKEDEPF90 ${WRFHELP}/makedepf90/makedepf90-2.8.8_${CC}_${PROCESSOR}
endif



# mpich2

#if ( -d ${WRFHELP}/mpich/mpich2-1.0.3_${FC}_${PROCESSOR}) then

# mpich2

#if ( -d ${WRFHELP}/mpich/mpich2-1.0.3_${FC}_${PROCESSOR}) then
#   setenv MPICH ${WRFHELP}/mpich/mpich2-1.0.3_${FC}_${PROCESSOR}
#endif
#if ( -d ~${WRFHELP}/mpich/mpich2-1.0.3_${FC}_${PROCESSOR}) then
#   setenv MPICH ${WRFHELP}/mpich/mpich2-1.0.3_${FC}_${PROCESSOR}
#endif

if (-d /usr/lpp/ppe.poe) then
   setenv MPICH /usr/lpp/ppe.poe
endif

# Lightning

if ( $MACHINE == "ln0126en" || $MACHINE == "ln0127en" ) then 
   if ( $FC == "pathscale" ) then
      setenv MPICH /contrib/2.6/mpich-gm/1.2.6..14a-pathscale-2.4-64
   endif
   if ( $FC == "pgi" ) then
      setenv MPICH /usr/local/mpich-gm/mpichgm-1.2.6..14a-64
   endif
   if ( $FC == "ifort" ) then
      source /contrib/2.6/intel/9.1.036-64/bin/ifortvars.csh
      setenv MPICH /contrib/2.6/mpich-gm/1.2.6..14a-intel-9.1.042-64
   endif
endif

# Crayx1

if ( $MACHINE == "gold.us.cray.com" ) then
   if ( $FC == "crayx1" ) then
      module use /opt/ctl/modulefiles /opt/PE/modulefiles
      module load PrgEnv.56.newest
      module list
      setenv NETCDF /ptmp/pjj/netcdf-3.5.1-x1
      setenv RTTOV ~n12138/rttov/rttov85_crayx1
      setenv CRTM ~n12138/crtm/crtm_crayx1
      setenv MPICH /opt/cpkg/v4/mpich2/1.0
      setenv WRF_OS crayx1
   endif
endif

setenv MPIHOME $MPICH
setenv PATH $MPICH/bin:$PATH
if ($?MAKEDEPF90) then
   setenv PATH $MPICH/bin\:$MAKEDEPF90\:$PATH
endif

if ($FC == "g95" ) then
   setenv G95_ENDIAN         BIG
   setenv G95_FPU_INVALID    T
   setenv G95_FPU_ZERODIV    T
   setenv G95_FPU_OVERFLOW   F
   setenv G95_FPU_UNDERFLOW  F
   setenv G95_FPU_INEXACT    F
   setenv G95_FPU_EXCEPTIONS F
   setenv G95_UNBUFFERED_ALL T
   setenv G95_MEM_INIT       0x00
   setenv G95_MEM_MAXALLOC   F
   setenv G95_MEM_SEGEMENTS  0
endif

echo
if ($?MPICH) then
   echo "MPICH       " $MPICH
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
