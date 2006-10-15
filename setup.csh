setenv MACHINE `hostname`

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

   setenv FC $1
   setenv CC $2

   if ( $FC"." == "." ) then
      setenv FC g95
   endif

   if ( $CC"." == "." ) then
      setenv CC gcc
   endif
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

if (-d /data7/da/bray/netcdf/netcdf-3.6.1_${FC}) then
   setenv NETCDF /data7/da/bray/netcdf/netcdf-3.6.1_${FC}
endif
if (-d /data7/da/bray/rttov/rttov85_${FC}) then
   setenv RTTOV /data7/da/bray/rttov/rttov85_$FC
endif
if (-d /data7/da/bray/crtm/crtm_${FC}) then
   setenv CRTM /data7/da/bray/crtm/crtm_$FC
endif
if (-d /data7/da/bray/mpich/mpich-1.2.7p1_${FC}) then
   setenv MPICH /data7/da/bray/mpich/mpich-1.2.7p1_${FC}
endif
if (-d /data7/da/bray/blas/blas_${FC}) then
   setenv BLAS /data7/da/bray/blas/blas_${FC}
endif
if (-d /data7/da/bray/lapack/lapack_${FC}) then
   setenv lapack /data7/da/bray/lapack/lapack_${FC}
endif
if (-d /data7/da/bray/fftpack5/fftpack5_${FC}) then
   setenv FFTPACK5 /data7/da/bray/fftpack5/fftpack5_${FC}
endif
if (-d /data7/da/bray/bufr_ncep_nco/bufr_ncep_nco_${FC}) then
   setenv BUFR /data7/da/bray/bufr_ncep_nco/bufr_ncep_nco_${FC}
endif
if (-d /data7/da/bray/makedepf90/makedepf90-2.8.8_${CC}) then
   setenv MAKEDEPF90 /data7/da/bray/makedepf90/makedepf90-2.8.8_${CC}
endif

# --------------------------------------------------------------------

if (-d ~bray/netcdf/netcdf-3.6.1_${FC}) then
   setenv NETCDF ~bray/netcdf/netcdf-3.6.1_${FC}
endif
if (-d ~bray/rttov/rttov85_$FC) then
   setenv RTTOV ~bray/rttov/rttov85_$FC
endif
if (-d ~bray/crtm/crtm_$FC) then
   setenv CRTM ~bray/crtm/crtm_$FC
endif
if (-d ~bray/mpich/mpich-1.2.7p1_${FC}) then
   setenv MPICH ~bray/mpich/mpich-1.2.7p1_${FC}
endif
if (-d ~bray/blas/blas_${FC}) then
   setenv BLAS ~bray/blas/blas_${FC}
endif
if (-d ~bray/lapack/lapack_${FC}) then
   setenv LAPACK ~bray/lapack/lapack_${FC}
endif
if (-d ~bray/fftpack5/fftpack5_${FC}) then
   setenv FFTPACK5 ~bray/fftpack5/fftpack5_${FC}
endif
if (-d ~bray/bufr_ncep_nco/bufr_ncep_nco_${FC}) then
   setenv BUFR ~bray/bufr_ncep_nco/bufr_ncep_nco_${FC}
endif
if (-d ~bray/makedepf90/makedepf90-2.8.8_${CC}) then
   setenv MPICH ~bray/makedepf90/makedepf90-2.8.8_${CC}
endif
if (-d ~bray/makedepf90/makedepf90-2.8.8_${CC}) then
   setenv MAKEDEPF90 ~bray/makedepf90/makedepf90-2.8.8_${CC}
endif

# --------------------------------------------------------------------

if (-d ~/netcdf/netcdf-3.6.1_${FC}) then
   setenv NETCDF ~/netcdf/netcdf-3.6.1_${FC}
endif
if (-d ~/rttov/rttov85_${FC}) then
   setenv RTTOV ~/rttov/rttov85_${FC}
endif
if (-d ~/crtm/crtm_${FC}) then
   setenv CRTM ~/crtm/crtm_${FC}
endif
if (-d ~/mpich/mpich-1.2.7p1_${FC}) then
   setenv MPICH ~/mpich/mpich-1.2.7p1_${FC}
endif
if (-d ~/blas/blas_${FC}) then
   setenv BLAS ~/blas/blas_${FC}
endif
if (-d ~/lapack/lapack_${FC}) then
   setenv LAPACK ~/lapack/lapack_${FC}
endif
if (-d ~/fftpack5/fftpack5_${FC}) then
   setenv FFTPACK5 ~/fftpack5/fftpack5_${FC}
endif
if (-d ~/bufr_ncep_nco/bufr_ncep_nco_${FC}) then
   setenv BUFR ~/bufr_ncep_nco/bufr_ncep_nco_${FC}
endif
if (-d ~/makedepf90/makedepf90-2.8.8_${CC}) then
   setenv MPICH ~/makedepf90/makedepf90-2.8.8_${CC}
endif
if (-d ~/makedepf90/makedepf90-2.8.8_${CC}) then
   setenv MAKEDEPF90 ~/makedepf90/makedepf90-2.8.8_${CC}
endif

# --------------------------------------------------------------------

if (-d /Volumes/$MACHINE/bray/tools/netcdf-3.6.1_${FC}) then
   setenv NETCDF /Volumes/$MACHINE/bray/tools/netcdf-3.6.1_${FC}
endif
if (-d /Volumes/$MACHINE/bray/tools/rttov85_${FC}) then
   setenv RTTOV /Volumes/$MACHINE/bray/tools/rttov85_${FC}
endif
if (-d /Volumes/$MACHINE/bray/tools/crtm_${FC}) then
   setenv CRTM /Volumes/$MACHINE/bray/tools/crtm_${FC}
endif
if (-d /Volumes/$MACHINE/bray/tools/mpich-1.2.7p1_${FC}) then
   setenv MPICH /Volumes/$MACHINE/bray/tools/mpich-1.2.7p1_${FC}
endif
if (-d /Volumes/$MACHINE/bray/tools/blas_${FC}) then
   setenv BLAS /Volumes/$MACHINE/bray/tools/blas_${FC}
endif
if (-d /Volumes/$MACHINE/bray/tools/lapack_${FC}) then
   setenv LAPACK /Volumes/$MACHINE/bray/tools/lapack_${FC}
endif
if (-d /Volumes/$MACHINE/bray/tools/fftpack5_${FC}) then
   setenv FFTPACK5 /Volumes/$MACHINE/bray/tools/fftpack5_${FC}
endif
if (-d /Volumes/$MACHINE/bray/tools/bufr_ncep_nco_${FC}) then
   setenv BUFR /Volumes/$MACHINE/bray/tools/bufr_ncep_nco_${FC}
endif
if (-d /Volumes/$MACHINE/bray/tools/makedepf90-2.8.8_${CC}) then
   setenv MAKEDEPF90 /Volumes/$MACHINE/bray/tools/makedepf90-2.8.8_${CC}
endif


# mpich2

#if ( -d /data7/da/bray/mpich/mpich2-1.0.3_${FC}) then

# mpich2

#if ( -d /data7/da/bray/mpich/mpich2-1.0.3_${FC}) then
#   setenv MPICH /data7/da/bray/mpich/mpich2-1.0.3_${FC}
#endif
#if ( -d ~bray/mpich/mpich2-1.0.3_${FC}) then
#   setenv MPICH ~bray/mpich/mpich2-1.0.3_${FC}
#endif
#if ( -d /Volumes/$MACHINE/bray/tools/mpich2-1.0.3_${FC}) then
#   setenv MPICH /Volumes/$MACHINE/bray/tools/mpich2-1.0.3_${FC}
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
