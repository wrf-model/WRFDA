#!/bin/ksh
#---------------------------------------------------------------
# Purpose : Plot be for cv_options=5  
#---------------------------------------------------------------
#
# Description of the Namelist:
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
# ne              : integer           ,  =1 for NMC, 
#                                        =number of ensemble memebers for ENS
# stride          : integer           ,  not used.
# domain_averaged : logical           ,  .true.  use the domain averaged reg. coeff.
#                                     ,  .false. use the local reg. coeff.
# be_dir          : character(len= 80),   be data directory
# code_version    : character(len=  8),  'wrfvar' or 'wrf3dvar'
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#

export WRFVAR_DIR=${WRFVAR_DIR:-$HOME/code/trunk/wrfvar}
export BE_DIR=${BE_DIR:-$HOME/data/con200/be}

export BE_METHOD=${BE_METHOD:-NMC}
export UH_METHOD=${UH_METHOD:-scale}
export START_DATE=${START_DATE:-2003081512}
export END_DATE=${END_DATE:-2003091500}
export RESOLUTION_KM=${RESOLUTION_KM:-45.0}
export INTERVAL=${INTERVAL:-12}
export MAIN_TITLE1=${MAIN_TITLE1:-Background Error}
export MAIN_TITLE2=${MAIN_TITLE2:-Regression Coefficients}
export NE=${NE:-1}
export STRIDE=${STRIDE:-1}
export DOMAIN_AVERAGED=${DOMAIN_AVERAGED:-.false.}
export CODE_VERSION=${CODE_VERSION:-wrfvar}

export WORK_DIR=${WORK_DIR:-$PWD/plot_be}
rm -rf $WORK_DIR; mkdir -p $WORK_DIR; cd $WORK_DIR

cat > namelist.title << EOF
&plot_title
start_date      = "$START_DATE",
end_date        = "$END_DATE",
interval        =  $INTERVAL,
main_title1     = "$MAIN_TITLE1" ,
main_title2     = "$MAIN_TITLE2" ,
Resolution_km   =  $RESOLUTION_KM,
be_method       = "$BE_METHOD",
Uh_method       = "$UH_METHOD",
NE              =  $NE,
stride          =  $STRIDE,
domain_averaged =  $DOMAIN_AVERAGED, 
be_dir          = "$BE_DIR", 
code_version    = "$CODE_VERSION", /
EOF

$WRFVAR_DIR/build/da_plot_be.exe > plot_be.out 2>&1

