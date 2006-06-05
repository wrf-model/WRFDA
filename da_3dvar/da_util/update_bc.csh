#! /bin/csh

 echo ""
 echo "Running script update_bc.csh"
 echo ""

 rm -f fort.*

 set IC     = /data10/huangwei/New_3DVAR/to_mm5v3/MM5INPUT_3DVAR_DOMAIN1.CAA
 set BC     = /data10/huangwei/CAA_CASE/MM5/Run/BDYOUT_DOMAIN1
 set New_BC = /data10/huangwei/New_3DVAR/to_mm5v3/BDYOUT_3DVAR_DOMAIN1.CAA

 ln -sf $IC     fort.10
 ln -sf $BC     fort.12
 ln -sf $New_BC fort.20

 ./update_bc.exe >&! update_bc.print.out

 echo "update_bc.csh completed"

 exit (0)

