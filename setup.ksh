MACHINE=`hostname`
COMPILER=${1:-g95}

if test $COMPILER = xlf; then
   if test -d /opt/ibmcmp/xlf/8.1; then
      export PATH=/opt/ibmcmp/xlf/8.1/bin:$PATH
      export MANPATH=/opt/ibmcmp/xlf/8.1/man/en_US/man:$MANPATH
   fi
fi

if test $COMPILER = g95; then
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

# Initially make mpich2 less desirable than mpich1

if test -d /data7/da/bray/mpich/mpich2-1.0.3_${COMPILER}; then
   export MPICH=/data7/da/bray/mpich/mpich2-1.0.3_${COMPILER}
fi
if test -d ~bray/mpich/mpich2-1.0.3_${COMPILER}; then
   export MPICH=~bray/mpich/mpich2-1.0.3_${COMPILER}
fi
if test -d /Volumes/$MACHINE/bray/tools/mpich2-1.0.3_${COMPILER}; then
   export MPICH=/Volumes/$MACHINE/bray/tools/mpich2-1.0.3_${COMPILER}
fi



if test -d /data7/da/bray/netcdf/netcdf-3.6.1_${COMPILER}; then
  export NETCDF=/data7/da/bray/netcdf/netcdf-3.6.1_${COMPILER}
fi
if test -d /data7/da/bray/rttov/rttov85_${COMPILER}; then
   export RTTOV=/data7/da/bray/rttov/rttov85_${COMPILER}
fi
if test -d /data7/da/bray/mpich/mpich-1.2.7p1_${COMPILER}; then
   export MPICH=/data7/da/bray/mpich/mpich-1.2.7p1_${COMPILER}
fi

if test -d ~bray/netcdf/netcdf-3.6.1_${COMPILER}; then
   export NETCDF=~bray/netcdf/netcdf-3.6.1_${COMPILER}
fi
if test -d ~bray/rttov/rttov85_${COMPILER}; then
   export RTTOV=~bray/rttov/rttov85_${COMPILER}
fi
if test -d ~bray/mpich/mpich-1.2.7p1_${COMPILER}; then
   export MPICH=~bray/mpich/mpich-1.2.7p1_${COMPILER}
fi

if test -d /Volumes/$MACHINE/bray/tools/netcdf-3.6.1_${COMPILER}; then
   export NETCDF=/Volumes/$MACHINE/bray/tools/netcdf-3.6.1_${COMPILER}
fi
if test -d /Volumes/$MACHINE/bray/tools/rttov85_${COMPILER}; then
   export RTTOV=/Volumes/$MACHINE/bray/tools/rttov85_${COMPILER}
fi
if test -d /Volumes/$MACHINE/bray/tools/mpich-1.2.7p1_${COMPILER}; then
   export MPICH=/Volumes/$MACHINE/bray/tools/mpich-1.2.7p1_${COMPILER}
fi

if test -d /usr/lpp/ppe.poe; then
   export MPICH=/usr/lpp/ppe.poe
fi

# Lightning

if test $HOSTNAME == "ln0126en" -o $HOSTNAME == "ln0127en"; then 
   if test $COMPILER == pathscale; then
      export MPICH=/contrib/2.6/mpich-gm/1.2.6..14a-pathscale-2.4-64
   fi
   if test $COMPILER == pgi; then
      export MPICH=/usr/local/mpich-gm/mpichgm-1.2.6..14a-64
   fi
fi

export MPIHOME=$MPICH
export PATH=$MPICH/bin:$PATH
export MANPATH=$MPICH/man:$MANPATH

if test $COMPILER = 'g95'; then
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
echo "NETCDF      " $NETCDF
