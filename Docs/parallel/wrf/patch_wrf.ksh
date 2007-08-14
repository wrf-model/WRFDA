#!/bin/ksh

export WRF_DIR=$1

cp external/RSL_LITE/module_dm.F $WRF_DIR/external/RSL_LITE
cp main/wrf.F                    $WRF_DIR/main/wrf.F

# Allow namelist_script to be automatically generated
# Remove -DNO_NAMELIST_PRINT, as we want them printed
cp arch/configure.defaults       $WRF_DIR/arch
cp tools/*                       $WRF_DIR/tools
cp frame/module_configure.F      $WRF_DIR/frame
