#! /bin/csh -xvf
#---------------------------------------------------------------
# Purpose : Plot gen_be for cv_options=5  
# History : 
#       06/07/2005                             Yong-Run Guo
#---------------------------------------------------------------
#
# Description of the Namleist:
#
# start_date      : character(len=10) ,  staring time
# end_date        : character(len=10) ,  ending time
# interval        : integer           ,  file interval in hours
# main_title1     : character(len=120),  Title for plots
# main_title2     : character(len=120),  not used.
# Resolution_km   : real              ,  model resolution im km
# be_method       : character(len=  3),  'NMC' or 'ENS'
# Uh_method       : character(len=  8),  'scale' for regional BE, 
#                                        'power' for global (not coded yet).
# NE              : integer           ,  =1 for NMC, 
#                                        =number of ensemble memebers for ENS
# stride          : integer           ,  not used.
# domain_averaged : logical           ,  .true.  use the domain averaged reg. coeff.
#                                     ,  .false. use the local reg. coeff.
# gen_be_dir      : character(len= 80),  gen_be data directory
# code_version    : character(len=  8),  'wrfvar' or 'wrf3dvar'
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    

cat >! namelist.title << EOF
&plot_title
start_date    = '2003081512',
end_date      = '2003091500',
interval      = 12,
main_title1   = 'CWB 45 KM WRF BACKGROUND ERROR' ,
main_title2   = 'REGRESSION COEFFICIENTS     ' ,
Resolution_km =  45.0,
be_method     = 'NMC',
Uh_method     = 'scale',
NE            = 1,
stride        = 1,
domain_averaged = .false., 
gen_be_dir    = '/mmmtmp/guo/wrfvar_cwb.be/cwb_wrf1', 
code_version  = 'wrfvar', /

gen_be_dir    = '/ptmp/guo/wrf3dvar_cwb.be/NMC', 
code_version  = 'wrf3dvar', /

EOF

#
# ------------------------- Compile -----------------------------------------

set OS = `uname -a`
set First_word = `echo $OS | cut -c 1-4`
set palm = `echo $OS | cut -c 7-10`
echo "${First_word} in ${palm}"

if ( -e plot_gen_be.exe ) rm plot_gen_be.exe

if ( $First_word == 'OSF1' ) then
# DEC_alpha
  ncargf90 -o plot_gen_be.exe -convert big_endian -free plot_gen_be.f

else if ( $First_word == 'Linu' && $palm == 'palm' ) then
# PC Linux:
  pgf90    -o plot_gen_be.exe -byteswapio -Mfreeform \
           -L/usr/local/ncarg/lib -L/usr/X11R6/lib64 -lncarg -lncarg_gks -lncarg_c \
           -lX11 -L/usr/pgi/linux86/lib -L/usr/lib64 \
           plot_gen_be.f

else if ( $First_word == 'Linu' ) then
# PC Linux:
  pgf90    -o plot_gen_be.exe -byteswapio -Mfreeform \
           -L/usr/local/ncarg/lib -L/usr/X11R6/lib -lncarg -lncarg_gks -lncarg_c \
           -lX11 -L/usr/pgi/linux86/lib -L/usr/lib -lf2c \
           plot_gen_be.f

else if ( $First_word == 'AIX' ) then
# IBM:
  xlf_r -o plot_gen_be.exe \
      -L/usr/local/lib32/r4i4 -lncarg -lncarg_gks -lncarg_c -lX11 -lm \
      -qfree -qarch=auto -qmaxmem=-1 -qnosave \
      plot_gen_be.f
else if ( $First_word == 'Darw' ) then
# Mac:
   xlf -o plot_gen_be.exe   plot_gen_be.f    \
       -w -qfree -qarch=auto -qspill=20000 -qmaxmem=32767 -qextname \
       -L/usr/local/ncarg/lib -lncarg -lcgm -lncarg_gks -lncarg_c   \
       -L/usr/X11R6/lib -lX11 -lm -L/usr/local/lib                  \
       -L/opt/ibmcmp/xlf/8.1/lib/ -lxlf90 -lg2c 
endif
# -----------------------------------------------------------------------

plot_gen_be.exe >& plot_gen_be_nmc.out

