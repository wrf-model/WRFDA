#! /bin/csh -xvf
#---------------------------------------------------------------
# Purpose : Calculate Scale-Length and Generate output Figuers  
# History : 09/01/2004                              Mi-Seon Lee
#---------------------------------------------------------------

  set Run_Dir = /mmmtmp1/guo/gen_be_run/NMC
  set Src_Dir = /mmmtmp1/guo/wrf3dvar/da_3dvar/utl
  set SHEL    = /mmmtmp1/guo/wrf3dvar/da_3dvar/utl

  cd ${SHEL}

  rm -rf plot_eigen.exe

  if (! -e plot_eigen.exe) then
# DEC_alpha
  ncargf90 -o plot_eigen.exe -convert big_endian ${Src_Dir}/plot_eigen_in_be.f90
# PC Linux:
#  pgf90    -o plot_eigen.exe -byteswapio \
#           -L/usr/local/ncarg/lib -L/usr/X11R6/lib -lncarg -lncarg_gks -lncarg_c \
#           -lX11 -L/usr/pgi/linux86/lib -L/usr/lib -lf2c \
#           ${Src_Dir}/plot_eigen_in_be.f90
  endif

  cd ${Run_Dir}

  ln -sf ${SHEL}/plot_eigen.exe plot_eigen.exe

foreach cv (psi chi_u t_u rh)
#---------------------------------------------------------------
# [2.0] draw eigen vector and scale length
#--------------------------------------------------------------
cat >! namelist.title << EOF
&plot_title
main_title1 = 'TEST WRF BACKGROUND ERROR' ,
main_title2 = 'REGRESSION COEFFICIENTS     ' ,
Km_Resolution = 100.0,
cv_options = 5,
fg_format = 1,
im=89,
jm=89,
km=27,
cvv='${cv}',
be_method = 'NMC',
scale_length =.true. ,
power_spectra = .false.,
infile='gen_be_stage3.${cv}.NMC.dat'/
EOF

./plot_eigen.exe >& plot_eigen.printout.${cv}

mv gmeta gmeta.eigen_${cv}
end
#-----------------------------------------------------------------------
 echo "DA_Run_BE_Graph.csh completed"
 exit (0)
