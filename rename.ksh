#!/bin/ksh
mv da_3dvar da
mv da/src/* da
rm -rf da/src
mkdir doc
mv da/changes doc
mv da/par_util da/da_par_util
mv da/BLAS external/blas
mv da/da_fftpack5 external/fftpack5
mv da/LAPACK external/lapack
mv da/utl da/da_util
mv da/DA_Sound/DA_Sonde_sfc/* da/DA_Sound
rm -rf da/DA_Sound/DA_Sonde_sfc
mkdir da/da_modules
mv da/module_da_gen_be_stats_interface.F da/da_modules
mv da/module_gen_be_esmf_super.F         da/da_modules
mv da/module_gen_be_top.F                da/da_modules
mv da/module_wrf_3dvar_interface.F       da/da_modules/da_wrfvar_interface.inc
mv da/module_wrf_3dvar_io.F              da/da_modules/da_wrfvar_io.f90
mv da/module_wrfvar_esmf_super.F         da/da_modules/da_wrfvar_esmf_super.f90 
mv da/module_wrfvar_top.F                da/da_modules/da_wrfvar_top.f90
mv da/da_solve_v3d da/da_solve
mv da/da_solve/da_solve_v3d.F da/da_solve/da_solve.f90

typeset -l LOWER

cd da
for DIR in *; do
   if test -d $DIR; then
      cd $DIR
      for FILE in *; do
         LOWER=$FILE
         if test -f $FILE; then
            if test $LOWER != $FILE; then
               mv $FILE $LOWER
            fi
         fi
      done
      for FILE in *.f; do
         if test -f $FILE; then
            mv $FILE ${FILE}90
         fi
      done
      cd ..
      LOWER=$DIR
      if test $LOWER != $DIR; then
         mv $DIR $LOWER
      fi
   fi
done
