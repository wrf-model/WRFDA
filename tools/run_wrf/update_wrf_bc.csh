#! /bin/csh -f
#-----------------------------------------------------------------------
# Script update_wrf_bc.csh
#
# Purpose: Update WRF lateral boundary conditions to be consistent with 
# WRF-Var analysis.
#
#-----------------------------------------------------------------------

#set echo

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

if ( ! $?START_DATE )       setenv START_DATE 2004051300
if ( ! $?CYCLING )          setenv CYCLING .FALSE.
if ( ! $?DATA_DISK )        setenv DATA_DISK /data3
if ( ! $?DAT_DIR )          setenv DAT_DIR ${DATA_DISK}/${user}/data
if ( ! $?DOMAIN )           setenv DOMAIN 1
if ( ! $?REG_DIR )          setenv REG_DIR ${DAT_DIR}/${REGION}${DOMAIN}
if ( ! $?EXPT )             setenv EXPT wrftest
if ( ! $?RUN_DIR )          setenv RUN_DIR ${REG_DIR}n/$EXPT/${START_DATE}
if ( ! -d ${RUN_DIR} ) then
   mkdir ${RUN_DIR}
endif

if ( ! $?SRC_DIR )          setenv SRC_DIR ${HOME}/code_development/WRF_V2.1 # Code directory.
if ( ! $?WRF_BC_DIR )       setenv WRF_BC_DIR ${SRC}/WRF_BC
if ( ! $?DA_FIRST_GUESS )   setenv DA_FIRST_GUESS ${RUN_DIR}/wrf_3dvar_input
if ( ! $?DA_ANALYSIS )      setenv DA_ANALYSIS ${RUN_DIR}/wrf_3dvar_output

setenv BDYIN $REG_DIR/wrfbdy_d01.${START_DATE}
setenv BDYOUT $RUN_DIR/wrfbdy_d01

rm -rf ${RUN_DIR}/tmpdir.wrf_bc >&! /dev/null
mkdir  ${RUN_DIR}/tmpdir.wrf_bc
cd ${RUN_DIR}/tmpdir.wrf_bc

cp $BDYIN wrfbdy_d01
ln -sf $DA_ANALYSIS wrf_3dvar_output
ln -sf $DA_FIRST_GUESS wrf_3dvar_input
ln -sf ${WRF_BC_DIR}/update_wrf_bc.exe .

cat > parame.in << EOF
&control_param
 wrf_3dvar_output_file = 'wrf_3dvar_output'
 wrf_bdy_file          = 'wrfbdy_d01'
 wrf_input_from_si     = 'wrf_3dvar_input'

 cycling = ${CYCLING}
 debug   = .true.
 low_bdy_only = .false. /
EOF

./update_wrf_bc.exe >&! /dev/null
mv wrfbdy_d01 $BDYOUT
rm -rf ${RUN_DIR}/tmpdir.wrf_bc

echo ""

exit (0)
