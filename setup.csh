setenv MACHINE `hostname`

if ($MACHINE == bs1201en || $MACHINE == bs1101en || $MACHINE == bv1103en || $MACHINE == bv1203en) then
   # Brain dead Aix /bin/csh cannot handle arguments to 
   # sourced scripts, so force use of xlf
   setenv COMPILER xlf
else
   setenv COMPILER $1
endif

if ($COMPILER == xlf) then
   if (-d /opt/ibmcmp/xlf/8.1) then
      setenv PATH    /opt/ibmcmp/xlf/8.1/bin:$PATH
      setenv MANPATH /opt/ibmcmp/xlf/8.1/man/en_US/man:$MANPATH
   endif
endif

if ($COMPILER == g95) then
   if (-d /data7/da/bray/g95) then
      setenv PATH /data7/da/bray/g95:$PATH
   endif
   if (-d ~bray/g95) then
      setenv PATH ~bray/g95:$PATH
   endif
   if (-d /Volumes/$MACHINE/bray/tools/g95) then
      setenv PATH /Volumes/$MACHINE/bray/tools/g95:$PATH
   endif
endif

# List options in order of increasing preference

# Initially make mpich2 less desirable than mpich1

if ( -d /data7/da/bray/mpich/mpich2-1.0.3_${COMPILER}) then
   setenv MPICH /data7/da/bray/mpich/mpich2-1.0.3_${COMPILER}
endif
if ( -d ~bray/mpich/mpich2-1.0.3_${COMPILER}) then
   setenv MPICH ~bray/mpich/mpich2-1.0.3_${COMPILER}
endif
if ( -d /Volumes/$MACHINE/bray/tools/mpich2-1.0.3_${COMPILER}) then
   setenv MPICH /Volumes/$MACHINE/bray/tools/mpich2-1.0.3_${COMPILER}
endif

if (-d /data7/da/bray/netcdf/netcdf-3.6.1_${COMPILER}) then
   setenv NETCDF /data7/da/bray/netcdf/netcdf-3.6.1_${COMPILER}
endif
if (-d /data7/da/bray/rttov/rttov85_${COMPILER}) then
   setenv RTTOV /data7/da/bray/rttov/rttov85_$COMPILER
endif
if (-d /data7/da/bray/crtm/crtm_${COMPILER}) then
   setenv CRTM /data7/da/bray/crtm/crtm_$COMPILER
endif
if (-d /data7/da/bray/mpich/mpich-1.2.7p1_${COMPILER}) then
   setenv MPICH /data7/da/bray/mpich/mpich-1.2.7p1_${COMPILER}
endif

if (-d ~bray/netcdf/netcdf-3.6.1_${COMPILER}) then
   setenv NETCDF ~bray/netcdf/netcdf-3.6.1_${COMPILER}
endif

if (-d ~bray/rttov/rttov85_$COMPILER) then
   setenv RTTOV ~bray/rttov/rttov85_$COMPILER
endif
if (-d ~bray/crtm/crtm_$COMPILER) then
   setenv CRTM ~bray/crtm/crtm_$COMPILER
endif
if (-d ~bray/mpich/mpich-1.2.7p1_${COMPILER}) then
   setenv MPICH ~bray/mpich/mpich-1.2.7p1_${COMPILER}
endif
if (-d ~bray/blas/blas_${COMPILER}) then
   setenv BLAS ~bray/blas/blas_${COMPILER}
endif
if (-d ~bray/lapack/lapack_${COMPILER}) then
   setenv LAPACK ~bray/lapack/lapack_${COMPILER}
endif
if (-d ~bray/fftpack5/fftpack5_${COMPILER}) then
   setenv FFTPACK5 ~bray/fftpack5/fftpack5_${COMPILER}
endif
if (-d ~bray/bufr_ncep_nco/bufr_ncep_nco_${COMPILER}) then
   setenv BUFR ~bray/bufr_ncep_nco/bufr_ncep_nco_${COMPILER}
endif

if (-d /Volumes/$MACHINE/bray/tools/netcdf-3.6.1_${COMPILER}) then
   setenv NETCDF /Volumes/$MACHINE/bray/tools/netcdf-3.6.1_${COMPILER}
endif
if (-d /Volumes/$MACHINE/bray/tools/rttov85_${COMPILER}) then
   setenv RTTOV /Volumes/$MACHINE/bray/tools/rttov85_${COMPILER}
endif
if (-d /Volumes/$MACHINE/bray/tools/crtm_${COMPILER}) then
   setenv CRTM /Volumes/$MACHINE/bray/tools/crtm_${COMPILER}
endif
if (-d /Volumes/$MACHINE/bray/tools/mpich-1.2.7p1_${COMPILER}) then
   setenv MPICH /Volumes/$MACHINE/bray/tools/mpich-1.2.7p1_${COMPILER}
endif

if (-d /usr/lpp/ppe.poe) then
   setenv MPICH /usr/lpp/ppe.poe
endif

# Lightning

if ( $MACHINE == "ln0126en" || $MACHINE == "ln0127en" ) then 
   if ( $COMPILER == pathscale ) then
      setenv MPICH /contrib/2.6/mpich-gm/1.2.6..14a-pathscale-2.4-64
   endif
   if ( $COMPILER == pgi ) then
      setenv MPICH /usr/local/mpich-gm/mpichgm-1.2.6..14a-64
   endif
   if test $COMPILER == ifort; then
      source /contrib/2.6/intel/9.1.036-64/bin/ifortvars.csh
      setenv MPICH /contrib/2.6/mpich-gm/1.2.6..14a-intel-9.1.042-64
   endif
endif

# Crayx1

if ( $MACHINE == "gold.us.cray.com" ) then
   if ( $COMPILER == crayx1 ) then
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
setenv PATH    $MPICH/bin:$PATH

if ($COMPILER == 'g95') then
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
endif

echo
echo "MPICH       " $MPICH
echo "RTTOV       " $RTTOV
echo "CRTM        " $CRTM
echo "NETCDF      " $NETCDF
echo "BLAS        " $BLAS
echo "LAPACK      " $LAPACK
echo "FFTPACK5    " $FFTPACK5
echo "BUFR        " $BUFR
