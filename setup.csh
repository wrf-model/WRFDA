setenv MACHINE `hostname`

if ($MACHINE == bs1201en || $MACHINE == bs1101en) then
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

if (-d /data7/da/bray/netcdf/netcdf-3.6.1_${COMPILER}) then
   setenv NETCDF /data7/da/bray/netcdf/netcdf-3.6.1_${COMPILER}
endif
if (-d /data7/da/bray/rttov/rttov85_${COMPILER}) then
echo setting RTTOV to /data7/da/bray/rttov/rttov85_${COMPILER}
   setenv RTTOV /data7/da/bray/rttov/rttov85_$COMPILER
echo 0 $RTTOV
endif
echo 1 $RTTOV
if (-d /data7/da/bray/mpich/mpich-1.2.7p1_${COMPILER}) then
   setenv MPICH /data7/da/bray/mpich/mpich-1.2.7p1_${COMPILER}
endif

if (-d ~bray/netcdf/netcdf-3.6.1_${COMPILER}) then
   setenv NETCDF ~bray/netcdf/netcdf-3.6.1_${COMPILER}
endif
echo oi
if (-d ~bray/rttov/rttov85_$COMPILER) then
   setenv RTTOV ~bray/rttov/rttov85_$COMPILER
echo $RTTOV
endif
if (-d ~bray/mpich/mpich-1.2.7p1_${COMPILER}) then
   setenv MPICH ~bray/mpich/mpich-1.2.7p1_${COMPILER}
endif

echo 2 $RTTOV
if (-d /Volumes/$MACHINE/bray/tools/netcdf-3.6.1_${COMPILER}) then
   setenv NETCDF /Volumes/$MACHINE/bray/tools/netcdf-3.6.1_${COMPILER}
endif
if (-d /Volumes/$MACHINE/bray/tools/rttov85_${COMPILER}) then
   setenv RTTOV /Volumes/$MACHINE/bray/tools/rttov85_${COMPILER}
endif
if (-d /Volumes/$MACHINE/bray/tools/mpich-1.2.7p1_${COMPILER}) then
   setenv MPICH /Volumes/$MACHINE/bray/tools/mpich-1.2.7p1_${COMPILER}
endif

echo 3 $RTTOV
if (-d /usr/lpp/ppe.poe) then
   setenv MPICH /usr/lpp/ppe.poe
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
