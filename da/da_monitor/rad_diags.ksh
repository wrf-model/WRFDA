#!/bin/ksh
#########################################################################
# Script: rad_diags.ksh  (radiance time-series diagnostics)
#
# Purpose: run inv*/oma* data processing program (rw_rad_diags.f90)
# to generate files in netcdf format and use NCL plotting script
# (plot_rad_diags.ncl and advance_cymdh.ncl) to make plots.
#
# Description:
#
### Data processing:
#
# to compile: xlf -o rw_rad_diags.exe  rw_rad_diags.f90  \
#                 -L/usr/local/netcdf/lib -lnetcdf -lm -I/usr/local/netcdf/include
#             g95 -o rw_rad_diags.exe  rw_rad_diags.f90  \
#                 -L/usr/local/netcdf/lib -lnetcdf -lm -I/usr/local/netcdf/include
# input files: (1)  namelist.rw_rad_diags
#                   &record1
#                    nproc = 16   (the proc numbers WRF-Var used)
#                    instid = 'noaa-17-amsub', 'dmsp-16-ssmis'  (inst names)
#                    file_prefix = "inv"       (either "inv", or "oma")
#                    start_date = '2005082000'
#                    end_date   = '2005083018'
#                    cycle_period  = 6
#                   /
#              (2) inv_* or oma_* from WRF-Var
#                  (note: rw_rad_diags.f90 expects the files are in the date directory
#                   in the program working directory, therefore, it's necessary to link
#                   or copy inv_* or oma_* to the required dirctory structure.)
#
### plotting                   
#
# plot_rad_diags.ncl is the main NCL script.
# A date advancing NCL script, advance_cymdh.ncl, is loaded in plot_rad_diags.ncl.
#
#set -x
#---------------------------------------------------------------------
# user-defined options
#---------------------------------------------------------------------
#
export START_DATE=2006082915
export END_DATE=2006082915
export CYCLE_PERIOD=12
#
export REGION=ernesto4
export EXPT=cloud_04km_k_reff
export NUM_PROCS=16
export WRFVAR_DIR=/ptmp/hclin/code/V2.2/WRFVAR_r2522
export EXP_DIR=/ptmp/hclin/exps/$REGION/$EXPT
export VAR_RUN_DIR1=$EXP_DIR/run
export VAR_RUN_DIR2=wrfvar/working
#
export DIAG_RUN_DIR=$EXP_DIR/diag
export DIAG_SRC_DIR=$HOME/code/plotrad
export FILE_PREFIX=inv    # or oma

set -A INSTIDS noaa-17-amsub noaa-18-amsua noaa-18-mhs
#set -A INSTIDS eos-2-airs noaa-15-amsua noaa-15-amsub noaa-16-amsua noaa-16-amsub noaa-16-hirs noaa-17-amsub noaa-18-amsua  noaa-18-hirs noaa-18-mhs
#
export LINK_DATA=true   # link inv* or oma* files in wrfvar/working directory to be in $DIAG_RUN_DIR/$DATE
export PROC_DATA=true   # collect and convert ascii files to netcdf files
export PROC_PLOT=true   # make plots
#
# environment variables to be passed to the plotting NCL script
#
export OUT_TYPE=ncgm            # ncgm, pdf (pdf will be much slower than ncgm and 
                                #            generate huge output if plots are not splitted)
                                # pdf will generated plots in higher resolution
export PLOT_OPT=all             # all, sea_only, land_only
export PLOT_QCED=false          # true, false.
export PLOT_SPLIT=false         # true, false. Set true to plot one frame in one file.
export PLOT_CLOUDY=true         # true, false. If plotting cloudy points.
                                # cloudy points are defined by the following 
                                # PLOT_CLOUDY_OPT, CLWP_VALUE, SI_VALUE settings.
if $PLOT_CLOUDY; then           # setup cloudy criteria
   export PLOT_CLOUDY_OPT=si    # clwp (cloud liquid water path from model), 
                                # si (scatter index from obs, only amsua, amsub and mhs)
                                # si_clwp
   export CLWP_VALUE=0.2        # only plot points with clwp >= clwp_value (when clwp_value > 0)
                                #                       clwp >  clwp_value (when clwp_value = 0)
   export SI_VALUE=3.0
fi

export DATDIR=$DIAG_RUN_DIR/    # the tailing / is necessary
export PLOTDIR=$DIAG_RUN_DIR/   # the tailing / is necessary
#
# set up mapping info for plotting
#
# for typical application, just set the following
# 3 variables to let the scripts extract mapping info
# from the first-guess file used in WRF-Var

export MAPINFO_FROM_FILE=true   # true, false
export SUBDOMAIN=false          # true, false
export FGFILE=$VAR_RUN_DIR1/$START_DATE/$VAR_RUN_DIR2/fg01

if ! $MAPINFO_FROM_FILE; then   # MAPINFO_FROM_FILE=false
#
# manually set the plotting area here
#
   if ! $SUBDOMAIN; then  # SUBDOMAIN=false, the map is bounded by corner points
      export MAP_PROJ=3     # for now, only 1 (lambert) or 3 (mercator)
      if [[ $MAP_PROJ == 1 ]]; then
         export TRUELAT1=30.
         export TRUELAT2=60.
         export STAND_LON=-98.
      fi
      export LAT_LL=0.1810659   # Lower-Left corner latitude
      export LON_LL=60.26209    # Lower-Left corner longitude
      export LAT_UR=37.62015    # Upper-Right corner latitude
      export LON_UR=100.7379    # Upper-Right corner longitude
   else   # SUBDOMAIN = True, map is defined by lat/lon box 
      export MAXLAT=28.0
      export MINLAT=20.5
      export MAXLON=-81.0
      export MINLON=-89.0
   fi
else   # MAPINFO_FROM_FILE = true
   export MAP_PROJ=`ncdump -h $FGFILE | grep "MAP_PROJ =" | awk '{print $3}'`
   if $SUBDOMAIN; then
      if [[ $MAP_PROJ == 1 ]]; then
         export MAPINFO_FROM_FILE=false # subdomain has to be a latlon box
                                        # Lambert projection (MAP_PROJ=1)
                                        # can not be used to set a subdomain.
                                        # MAXLAT, MINLAT, MAXLON, MINLON have
                                        # to be set.
         export MAXLAT=28.0
         export MINLAT=20.5
         export MAXLON=-81.0
         export MINLON=-89.0
      fi
   fi
fi
#
#---------------------------------------------------------------------
# linking inv/oma data to be in the form required by the data processing program
# for example, 2005082700/inv*
#---------------------------------------------------------------------
#
if $LINK_DATA; then
   DATE=$START_DATE
   while [[ $DATE -le $END_DATE ]]; do
      if [[ ! -d $DIAG_RUN_DIR/$DATE ]]; then
         mkdir -p $DIAG_RUN_DIR/$DATE
      fi
      cd $DIAG_RUN_DIR/$DATE
      ln -sf $VAR_RUN_DIR1/$DATE/$VAR_RUN_DIR2/$FILE_PREFIX* .
      DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe $DATE $CYCLE_PERIOD`
   done
fi

cd $DIAG_RUN_DIR
#
#---------------------------------------------------------------------
# inv/oma data processing section
#---------------------------------------------------------------------
#
if $PROC_DATA; then
#
# fisrt check if rw_rad_diags.exe is available in the working dir
# if not, link or compile rw_rad_diags.exe
#
   if [[ ! -e rw_rad_diags.exe ]]; then
      if [[ -e $DIAG_SRC_DIR/rw_rad_diags.exe ]]; then
         ln -sf $DIAG_SRC_DIR/rw_rad_diags.exe ./rw_rad_diags.exe
      else
         xlf -o rw_rad_diags.exe $DIAG_SRC_DIR/rw_rad_diags.f90  \
              -L/usr/local/netcdf/lib -lnetcdf -lm -I/usr/local/netcdf/include
      fi
   fi
#
# create namelist
#
   n=0
   INSTID=''
   for instID in ${INSTIDS[*]}; do
      let n=$n+1
      INSTID=" ${INSTID} '${instID}', "
   done

   if [[ -e namelist.rw_rad_diags ]]; then
      rm -f namelist.rw_rad_diags
   fi
   cat > namelist.rw_rad_diags << EOF
&record1
nproc = ${NUM_PROCS}
instid = ${INSTID}
file_prefix = '$FILE_PREFIX'
start_date = '$START_DATE'
end_date   = '$END_DATE'
cycle_period  = $CYCLE_PERIOD
/
EOF
#
# run the format convertor
#
   rw_rad_diags.exe

fi
#
#---------------------------------------------------------------------
# plot
#---------------------------------------------------------------------
#
ln -sf $DIAG_SRC_DIR/advance_cymdh.ncl ./advance_cymdh.ncl
cp -p $DIAG_SRC_DIR/plot_rad_diags.ncl ./plot_rad_diags.ncl

if $PROC_PLOT; then
   if $MAPINFO_FROM_FILE; then
      export MAP_PROJ=`ncdump -h $FGFILE | grep "MAP_PROJ =" | awk '{print $3}'`
      export TRUELAT1=`ncdump -h $FGFILE | grep "TRUELAT1 =" | awk '{print $3}'`
      export TRUELAT2=`ncdump -h $FGFILE | grep "TRUELAT2 =" | awk '{print $3}'`
      export STAND_LON=`ncdump -h $FGFILE | grep "STAND_LON =" | awk '{print $3}'`
   fi
   for instID in ${INSTIDS[*]}; do    # loop for instruments
      export INSTRUMENT=$instID
      ncl plot_rad_diags.ncl
   done
fi

exit
