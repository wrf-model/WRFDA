setenv MACHINE `hostname`
setenv COMPILER $1

if ($COMPILER == xlf) then
   if (-d /opt/ibmcmp/xlf/8.1) then
      setenv PATH    /opt/ibmcmp/xlf/8.1/bin:$PATH
      setenv MANPATH /opt/ibmcmp/xlf/8.1/man/en_US/man:$MANPATH
   endif
endif

# List options in order of increasing preference

if (-d /data7/da/bray) then
   setenv NETCDF /data7/da/bray/netcdf/netcdf-3.6.1_${COMPILER}
   setenv RTTOV  /data7/da/bray/rttov/rttov85_${COMPILER}
   setenv MPICH  /data7/da/bray/mpich/mpich-1.2.7p1_${COMPILER}
endif

if (-d ~bray) then
   setenv NETCDF ~bray/netcdf/netcdf-3.6.1_${COMPILER}
   setenv RTTOV  ~bray/rttov/rttov85_${COMPILER}
   setenv MPICH  ~bray/mpich/mpich-1.2.7p1_${COMPILER}
endif

if (-d /Volumes/$MACHINE/bray/tools) then
   setenv NETCDF /Volumes/$MACHINE/bray/tools/netcdf-3.6.1_${COMPILER}
   setenv RTTOV  /Volumes/$MACHINE/bray/tools/rttov85_${COMPILER}
   setenv MPICH  /Volumes/$MACHINE/bray/tools/mpich-1.2.7p1_${COMPILER}
endif

setenv MPIHOME $MPICH
setenv PATH    $MPICH/bin:$PATH
setenv MANPATH $MPICH/man:$MANPATH

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
echo "NETCDF      " $NETCDF
echo "which mpif90" `which mpif90`
