#! /bin/csh

 echo ""
 echo "Running script generate_difference.csh"
 echo ""

#ln -sf /data10/huangwei/CAA_CASE/MM5/Run_little_r_Sat_Wind/MMOUT_DOMAIN1  fort.11
#ln -sf /data10/huangwei/CAA_CASE/MM5/Run/MMOUT_DOMAIN1                    fort.12
#ln -sf /data10/huangwei/GRAPH/3DVAR-LITTLE_R                              fort.20

 foreach dmn ( 1 2 )

    ln -sf /pecan2/huangwei/AMPS/case/3DVAR_DOMAIN${dmn}_00	fort.11
    ln -sf /pecan2/huangwei/AMPS/case/MMOUT_DOMAIN${dmn}_00	fort.12
    ln -sf /pecan3/huangwei/GRAPH/3DVAR_MMOUT_DOMAIN${dmn}_00	fort.20

    ./generate_difference.exe >&! generate_difference.print.out

    echo "generate_difference.csh completed"

    rm -f fort.* generate_difference.print.out
 end

 exit (0)

