#! /bin/csh
##################################################################
#  This deck is used to produce a map of mesoscale observations 
#  sites, and produce the data files for 4DVAR.
#
#                                    Yong-Run Guo 
#                                     07/28/1999
#                                     modified on 06/07/2002
###################################################################
#set echo
 unalias mv rm

 set TIME_WINDOW_MIN = '2002082823'
 set TIME_ANALYSIS   = '2002082900'
 set TIME_WINDOW_MAX = '2002082901'

 set OBSDATA  = ../obs_gts.3dvar

 echo " "

 if (-e $OBSDATA) then

 if ( -e gmeta )         rm gmeta
 if ( -e namelist.file ) rm namelist.file
# =========================================================================
# For domain information, there are 2 options:
#
#   (1) To get it from the file $OBSDATA automatically;
#
#       This is useful with ONE shell script applying to any of
#       $OBSDATA files, no namelist file need to be edited.
#
#   (2) To set it within the namelist file by adding the following 
#       2 records in namelist.file. The information from $OBSDATA
#       will be overwritten.
#
#       This is useful when users want to use a specific domain
#       defined by the namelist for plotting.
#
# Example for AFWA T3 domain:
# -------------------------------------------------------------------------
# &MAPBG
# PHIC  =  51.0 ,
# XLONC =  10.5,
# IEXP  =  0,          ; Domain expanded (1) or not (0) ?
# AEXP  =   360.,      ; Distance of expansion in km on each side of domain
# IPROJ = 'LAMCON',
# ;IPROJ = 'POLSTR',   ; MAP PROJECTION
# ;IPROJ = 'MERCAT',   ; MAP PROJECTION
# TRUELAT1=60.,
# TRUELAT2=30.,
# &
#
# &DOMAINS
# MAXNES =   1,
# NESTIX =  105,  124,  136,  181,  211, 
# NESTJX =  120,  121,  181,  196,  211,
# DIS    =  45.,  10.,  3.3,  1.1,  1.1,
# NUMC   =    1,    1,   2,     3,    4,
# NESTI  =    1,   40,  28,    35,   45,
# NESTJ  =    1,   60,  25,    65,   55,
# &
# --------------------------------------------------------------------------
# 
cat >! namelist.file << EOF

 &TIME_WINDOW
  TIME_WINDOW_MIN = '$TIME_WINDOW_MIN', ; beginning of time window
  TIME_WINDOW_MAX = '$TIME_WINDOW_MAX', ; end of time window
 &

 &SKEW_PLOTS
  skewt_plot = .FALSE.,
 &
EOF

 sed -f no_comment.sed  namelist.file >! tmp.file
 mv tmp.file  namelist.file
# ==========================================================================
# make clean
  make

 ln -s -f $OBSDATA        fort.99
 ln -s -f namelist.file   fort.15

 echo " "
 echo "Map.exe >&! Map.out.${TIME_ANALYSIS}"
     ./Map.exe >&! Map.out.${TIME_ANALYSIS}
 echo " "

 if (-e gmeta) \
 echo "Generate plots in metafile gmeta.${TIME_ANALYSIS}"
 mv gmeta gmeta.${TIME_ANALYSIS}

else
 echo "Cannot find input file $OBSDATA"
endif
 echo " "
