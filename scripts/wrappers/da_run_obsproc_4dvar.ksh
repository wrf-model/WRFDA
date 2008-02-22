#!/bin/ksh
#########################################################################
# Script: da_run_obsproc_4dvar.ksh
#
# Purpose: Provide user-modifiable interface to run obsproc for 4D-Var
#
# Note: This script should be used with the same directory of 3dvar_obs.exe
# 
# You should Change:
# (1)
# BUILD=/ptmp/xinzhang/blueice_ibm_serial/wrfvar/build
# RTOBS=~/data/rtobs
#
# export START_TIME=2007081421
# export END_TIME=2007082200
# export INTERVAL_HOUR=3
# (2)
# doamain configuration in namelist parts
#
# Author: Xin Zhang
# 2/22/2008
#
#########################################################################

#set -x

BUILD=/ptmp/xinzhang/blueice_ibm_serial/wrfvar/build
RTOBS=~/data/rtobs

export START_TIME=2007081421
export END_TIME=2007082200
export INTERVAL_HOUR=3

#   function
function obsproc { 

rm -fr obs_gts.3dvar namelist.3dvar_obs
cat > namelist.3dvar_obs <<EOF
&record1
 obs_gts_filename = 'obs.${y}${m}${d}${h}',
 fg_format        = 'WRF',
 obs_err_filename = 'obserr.txt',
/

&record2
 time_window_min  = '${y}-${m}-${l_day}_${l_hour}:${l_minute}:00',
 time_analysis    = '${y}-${m}-${day}_${hour}:${minute}:00',
 time_window_max  = '${y}-${m}-${r_day}_${r_hour}:${r_minute}:00',
/

&record3
 max_number_of_obs        = 70000,
 fatal_if_exceed_max_obs  = .TRUE.,
/

&record4
 qc_test_vert_consistency = .TRUE.,
 qc_test_convective_adj   = .TRUE.,
 qc_test_above_lid        = .TRUE.,
 remove_above_lid         = .TRUE.,
 domain_check_h           = .true.,
 Thining_SATOB            = .FALSE.,
 Thining_SSMI             = .FALSE.,
 Thining_QSCAT            = .FALSE.,
/

&record5
 print_gts_read           = .TRUE.,
 print_gpspw_read         = .TRUE.,
 print_recoverp           = .TRUE.,
 print_duplicate_loc      = .TRUE.,
 print_duplicate_time     = .TRUE.,
 print_recoverh           = .TRUE.,
 print_qc_vert            = .TRUE.,
 print_qc_conv            = .TRUE.,
 print_qc_lid             = .TRUE.,
 print_uncomplete         = .TRUE.,
/

&record6
 ptop =   1000.,
 base_pres  = 100000.,
 base_temp  =    300.,
 base_lapse =     50.,
/
 ps0  = 100000.,
 ts0  =    300.,
 tlp  =     50.,

&record7
 IPROJ =   3,
 PHIC         =  19.99999,
 XLONC        = -75.00000,
 TRUELAT1= 20.,
 TRUELAT2= 20.,
 MOAD_CEN_LAT =  19.99999,
 STANDARD_LON = -75.0, 
/ 
/

&record8
 IDD    =   1,
 MAXNES =   1,
 NESTIX =   61,  128,  121,  181,  211,
 NESTJX =   55,  222,  133,  196,  211,
 DIS    =   90.,  45.,  15.,  1.1,  1.1,
 NUMC   =    1,    1,   2,     3,    4,
 NESTI  =    1,    1,  29,    35,   45,
 NESTJ  =    1,    1,  27,    65,   55,
 /

&record9
  prepbufr_output_filename='prepbufr_obs_gts.3dvar'
  prepbufr_table_filename='prepbufr_table_filename'
  output_ob_format=2
/
EOF

grep time_ namelist.3dvar_obs
./3dvar_obs.exe 1>/dev/null 2>/dev/null
if [[ ! -a ${y}${m}${day}${hour} ]] ;then
   mkdir ${y}${m}${day}${hour}
fi
if [[ -a obs_gts.3dvar ]]; then
   echo "****mv obs_gts.3dvar ${y}${m}${day}${hour}/ob.ascii${sux}****"
   mv obs_gts.3dvar ${y}${m}${day}${hour}/ob.ascii${sux}
else
   echo "obs_gts.3dvar was not generated"
   exit 8
fi
}


export LOCAL_DATE=$START_TIME

while [[ ${LOCAL_DATE} -le ${END_TIME} ]] ; do

    export y=$(echo ${LOCAL_DATE} | cut -c1-4)
    export m=$(echo ${LOCAL_DATE} | cut -c5-6)
    export d=$(echo ${LOCAL_DATE} | cut -c7-8)
    export h=$(echo ${LOCAL_DATE} | cut -c9-10)

    echo "Processing ${y}${m}${d}${h}...."

    if [[ ! -a ${RTOBS}/${y}${m}${d}${h}/obs.${y}${m}${d}${h}.gz && ! -a ${RTOBS}/${y}${m}${d}${h}/obs.${y}${m}${d}${h} ]]; then
       echo "${RTOBS}/${y}${m}${d}${h}/obs.${y}${m}${d}${h} is not found, copy from MSS"
       mkdir -p ${RTOBS}/${y}${m}${d}${h}
       msrcp mss:/BRESCH/RT/DATA/${y}${m}/obs.${y}${m}${d}${h}.gz ${RTOBS}/${y}${m}${d}${h}/.
    fi
    gunzip ${RTOBS}/${y}${m}${d}${h}/obs.${y}${m}${d}${h}.gz 2>/dev/null
    ln -fs ${RTOBS}/${y}${m}${d}${h}/obs.${y}${m}${d}${h}

    export sux=''

    for x in -1 0 1; do
       TIME=$(${BUILD}/da_advance_time.exe ${y}${m}${d}${h} $x 2>/dev/null)
       TIME_MIN=$(${BUILD}/da_advance_time.exe $TIME -1 2>/dev/null)
       export day=$(echo $TIME | cut -c7-8)
       export hour=$(echo $TIME | cut -c9-10)
       export minute=00
       export l_day=$(echo $TIME_MIN | cut -c7-8)
       export l_hour=$(echo $TIME_MIN | cut -c9-10)
       export l_minute=31
       export r_day=$(echo $TIME | cut -c7-8)
       export r_hour=$(echo $TIME | cut -c9-10)
       export r_minute=30
       obsproc

    done
       
    if [[ ${h} = 03 || ${h} = 09 || ${h} = 15 || ${h} = 21  ]]; then
       TIME=${y}${m}${d}${h}
       TIME_MIN=$(${BUILD}/da_advance_time.exe ${y}${m}${d}${h} -1 2>/dev/null)
       export day=$(echo $TIME | cut -c7-8)
       export hour=$(echo $TIME | cut -c9-10)
       export minute=00
       export l_day=$(echo $TIME_MIN | cut -c7-8)
       export l_hour=$(echo $TIME_MIN | cut -c9-10)
       export l_minute=31
       export r_day=$(echo $TIME | cut -c7-8)
       export r_hour=$(echo $TIME | cut -c9-10)
       export r_minute=00
       export sux='-'
       obsproc

       export l_day=$(echo $TIME | cut -c7-8)
       export l_hour=$(echo $TIME | cut -c9-10)
       export l_minute=00
       export r_minute=30
       export sux='+'
       obsproc
    fi 

    unlink obs.${y}${m}${d}${h}
    gzip ${RTOBS}/${y}${m}${d}${h}/obs.${y}${m}${d}${h} &

    LOCAL_DATE=$(${BUILD}/da_advance_time.exe ${LOCAL_DATE} ${INTERVAL_HOUR} 2>/dev/null)
done

