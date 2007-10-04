#!/bin/ksh

export WRFNL_DIR=$1

# Extra output for WRFNL
cp Registry/Registry.EM_NL.r2473 $WRFNL_DIR/Registry/Registry.EM

# Allow namelist_script to be automatically generated
# Remove -DNO_NAMELIST_PRINT, as we want them printed
cp arch/configure.defaults       $WRFNL_DIR/arch
