#!/bin/csh -f

 set echo 

 setenv RUNWRF4D   $WRF_TEMP/runvar4d

 rm -rf ${RUNWRF4D}
 mkdir  ${RUNWRF4D}

 cd ${RUNWRF4D}

 cp ${WRF_ROOT}/data/LANDUSE.TBL .
 cp ${WRF_ROOT}/data/RRTM_DATA .

 cp ${WRF_ROOT}/data/wrfbdy_d01.2000-01-25_00:00:00 wrfbdy_d01

 cp ${WRF_ROOT}/data/namelist.var4dnl .
 cp ${WRF_ROOT}/data/namelist.var4dtl .
 cp ${WRF_ROOT}/data/namelist.var4dad .

 cp ${WRF_ROOT}/data/namelist.var4d namelist.3dvar

 cp                  namelist.var4dnl namelist.input

 cp ${WRF_ROOT}/data/wrf_3dvar_input_d01_2000-01-25_00:00:00 wrf_3dvar_input
 cp ${WRF_ROOT}/data/obs2000012500 fgat_ob.01
 cp ${WRF_ROOT}/data/obs2000012501 fgat_ob.02
 cp ${WRF_ROOT}/data/obs2000012502 fgat_ob.03
 cp ${WRF_ROOT}/data/obs2000012503 fgat_ob.04

 cp ${WRF_ROOT}/data/be.cv_2 fort.32

 cp ${WRF_ROOT}/wrf_scr/runvar4dnl.csh .
 cp ${WRF_ROOT}/wrf_scr/runvar4dtl.csh .
 cp ${WRF_ROOT}/wrf_scr/runvar4dad.csh .
 touch stdout.nl
 touch stdout.tl
 touch stdout.ad
 chmod 777 *

 ln -s ${WRF_TEMP}/wrfmodel/main/wrf.exe .
 ln -s ${WRF_TEMP}/wrfvda/main/da_3dvar.exe .

 da_3dvar.exe > stdout.mi

# rm namelist.input namelist.3dvar
# rm wrf_3dvar_input
# rm fgat_fg*

 mv fort.12 DAProg_3DVAR.statistics
 mv fort.47 obsrelated
 mv fort.48 jo
 mv fort.50 obsagain
 mv fort.60 qc
 mv fort.81 DAProg_3DVAR.cost_fn
 mv fort.82 DAProg_3DVAR.grad_fn

