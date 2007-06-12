# Registry file, EM
#                                               
# Order refers to which of the three sets of just-mentioned internal
#                                               
# At the present time this file is managed manually and edited by hand.                                         
#                                               
################################################################################
# Dimension specifications
#
# This section of the Registry file is used to specify the dimensions
# that will be used to define arrays. Dim is the one-letter name of the
# dimension.  How defined can either be "standard_domain", which means
# that the dimension (1) is one of the three spatial dimensions and (2)
# it will be set using the standard namelist mechanism and domain data
# structure dimension fields (e.g. sd31,ed31,sd32...).
#
# Order refers to which of the three sets of just-mentioned internal
# dimension variables the dimension is referred to by in the driver.
# That is, is it the first, second, or third dimension.  The registry
# infers the mapping of its internal dimensions according to the
# combination of Order and Coord-axis that are specified in this table.
# Note that it is all right to more than one dimension name for, say, the
# x dimension.  However, the Order and Coord-axis relationship must be
# consistent throughout.
# 
# Note: these entries do not enforce storage order on a particular field.
# That is determined by the dimension strings for each field. But it does
# relate the dimspec to the internal data structures that the driver uses
# to maintain the three physical domain dimensions.
# 
# "How defined" can also specify the name of a namelist variable from which
# the definition for the dimension will come; this is specified as
# "namelist=<variable name>".  The namelist variable must have been
# defined as an integer and with only one entry in the rconfig table. Or
# a constant can be specified.  The coordinate axis for the dimension is
# either X, Y, Z, or C (for "not a spatial dimension").  The Dimname is
# the descriptive name of the dimension that will be included in the
# metadata in data sets.  Note that the b, f, and t modifiers that appear
# as the last characters of dimension strings used # in state and # i1
# registry definitions are not dimensions and do not need to be declared
# here.
#

#<Table>  <Dim>  <Order> <How defined>         <Coord-axis>  <Dimname in Datasets>
dimspec    i      1     standard_domain           x          west_east
dimspec    j      3     standard_domain           y          south_north
dimspec    k      2     standard_domain           z          bottom_top
dimspec    l      2     namelist=num_soil_layers  z          soil_layers
dimspec    u      2     namelist=num_land_cat     z          land_cat
dimspec    s      2     namelist=num_soil_cat     z          soil_cat
dimspec    m      -     constant=3                c          three_things  # example; not used
dimspec    w      -     namelist=spec_bdy_width   c          spec_bdy_width
dimspec    e      3     namelist=ensdim           z          ensemble dimension


################################################################################
################################################################################
################################################################################

# Lines that start with the word 'state' form a table that is                                           
# used by the script use_registry to generate module_state_descript.F                                           
# and other files.  Also see documentation in use_registry.                                             
#                                               
# table entries are of the form                                         
#<Table> <Type> <Sym>         <Dims>   <Use>   <NumTLev> <Stagger> <IO>     <DNAME>             <DESCRIP>     <UNITS>   
#

# It is reauired that LU_INDEX appears before any variable that is
# interpolated with a mask, as lu_index supplies that mask.
# this next 1 is for the HFSoLE/PET demo; writing these to auxhist1 output over MCEL for coupling
# with wave model, only if compiled with -DMCELIO, JM 2003/05/29
state    real  LU_INDEX         ij      misc        1         -     i02rh0201d=(interp_fcnm)u=(copy_fcnm)   "LU_INDEX"              "LAND USE CATEGORY"         ""
state    real  LU_MASK          ij      misc        1         -     i3h1     "LU_MASK"              "0 land 1 water"         ""


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#                                               
# Variables for Eulerian mass coordinate dynamics                                            
#                                               

# Velocities
#
# U Vel
state    real   u              ikj     dyn_em      2       X     \
     i0123rh02usdf=(bdy_interp:dt,u_b,u_bt)       "U"                      "x-wind component"   "m s-1"
state    real   ru             ikj     dyn_em      1       X      -        "MU_U"        "mu-coupled u"   "Pa m s-1"
state    real   ru_m           ikj     dyn_em      1       X      -        "ru_m"        ""   ""
state    real   ru_tend        ikj     dyn_em      1       X      -        "ru_tend"        ""   ""                                   
i1       real   ru_tendf       ikj     dyn_em      1       X                                          
state    real   u_save         ikj     dyn_em      1       X      -        "u_save"
#                                               
# V Vel
state    real   v              ikj     dyn_em      2       Y     \
     i0123rh02usdf=(bdy_interp:dt,v_b,v_bt)        "V"                     "y-wind component"   "m s-1"
state    real   rv             ikj     dyn_em      1       Y      -        "MU_V"        "mu-coupled v"   "Pa m s-1"
state    real   rv_m           ikj     dyn_em      1       Y      -        "rv_m"
state    real   rv_tend        ikj     dyn_em      1       Y      -        "rv_tend"
i1       real   rv_tendf       ikj     dyn_em      1       Y                                          
state    real   v_save         ikj     dyn_em      1       Y      -        "v_save"                   
#                                               
# Vertical Vel                                          
state    real   w              ikj     dyn_em      2       Z     \
        i023rh02usdf=(bdy_interp:dt,w_b,w_bt)  "w"                          "z-wind component"   "m s-1"
state    real   ww             ikj     dyn_em      1       Z      r         "ww"   "mu-coupled eta-dot"    "Pa s-1"
state    real   rw             ikj     dyn_em      1       Z      -         "rw"   "mu-coupled w"          "Pa m s-1"
i1       real   ww1            ikj     dyn_em      1       Z                                          
i1       real   ww_m           ikj     dyn_em      1       Z                                          
i1       real   wwp            ikj     dyn_em      1         Z                                          
i1       real   rw_tend        ikj     dyn_em      1       Z                                          
i1       real   rw_tendf       ikj     dyn_em      1       Z                                          
i1       real   w_save         ikj     dyn_em      1       Z                                          

# Geopotential
state    real   ph             ikj     dyn_em      2       Z     \
       i023rh02usdf=(bdy_interp:dt,ph_b,ph_bt)   "ph"   "perturbation geopotential"  "m2 s-2"
state    real   phb            ikj     dyn_em      1       Z     irhdus "phb"  "base-state geopotential"  "m2 s-2"
state    real   phb_fine       ikj     dyn_em      1         Z      -     "phb_fine"  "for nesting, temp holding interpolated coarse grid phb"  "m2 s-2"
state    real   ph0            ikj     dyn_em      1         Z      r     "ph0"  "initial geopotential"     "m2 s-2"
state    real   php            ikj     dyn_em      1       Z      r     "php"  "geopotential"             "m2 s-2"
i1       real   ph_tend        ikj     dyn_em      1       Z 
i1       real   ph_tendf       ikj     dyn_em      1       Z 
i1       real   ph_save        ikj     dyn_em      1       Z 

# Potential Temperature
state    real   t              ikj     dyn_em      2       -     \
       i0123rh02usdf=(bdy_interp:dt,t_b,t_bt)   "t"      "perturbation potential temperature (theta-t0)" "K"

state    real   t_init         ikj     dyn_em      1         -      r        "t_init" "initial potential temperature" "K"
i1       real   t_tend         ikj     dyn_em      1       -  
i1       real   t_tendf        ikj     dyn_em      1       -  
state    real   tp             ikj     dyn_em      2         -  
i1       real   t_2save        ikj     dyn_em      1       -   
state    real   t_save         ikj     dyn_em      1       -               "t_save"
#                                               


# Mass
state    real   mu              ij     dyn_em      2       -     \
     i023rh02usdf=(bdy_interp:dt,mu_b,mu_bt)   "mu"  "perturbation dry air mass in column" "Pa"
state    real   mub             ij     dyn_em      1       -     irhdus       "mub" "base state dry air mass in column" "Pa"
state    real   mub_fine        ij     dyn_em      1         -      -           "mub_fine" "nest temp, holds interpolated coarse grid mub" "Pa"
state    real   mu0             ij     dyn_em      1         -     i0123h02rdu       "mu0" "initial dry mass in column" "Pa"
state    real   mudf            ij     dyn_em      1       -      -           "mudf" "" ""
state    real   muu             ij     dyn_em      1       -                 "muu"
i1       real   muus            ij     dyn_em      1       -     
state    real   muv             ij     dyn_em      1       -                 "muv"
i1       real   muvs            ij     dyn_em      1       -     
state    real   mut             ij     dyn_em      1       -                 "mut"
state    real   muts            ij     dyn_em      1       -                 "muts"
i1       real   muave           ij     dyn_em      1       -     
i1       real   mu_save         ij     dyn_em      1       -     
i1       real   mu_tend         ij     dyn_em      1       -     

#diagnostic for looking at nest position in output. A mungy version of terrain height.
state    real   nest_pos        ij     misc        1   -   hu=(mark_domain)  "NEST_POS"


# TKE
state    real   tke            ikj     dyn_em      2         -       r        "tke"          "TURBULENCE KINETIC ENERGY"     "m2 s-2"
i1       real   tke_tend       ikj     dyn_em      1         -      

# Pressure and Density
state    real   p              ikj     dyn_em      1       -      rh02         "p"           "perturbation pressure"         "Pa"
state    real   al             ikj     dyn_em      1       -      rh02         "al"          "inverse perturbation density"  "m3 kg-1"
state    real   alt            ikj     dyn_em      1       -      rh02         "alt"         "inverse density"               "m3 kg-1"
state    real   alb            ikj     dyn_em      1         -      rh02du       "alb"         "inverse base density"          "m3 kg-1"
state    real   zx             ikj     dyn_em      1         X      -         " "  " "  " "    
state    real   zy             ikj     dyn_em      1         Y      -         " "  " "  " "   
state    real   rdz            ikj     dyn_em      1         Z      -         " "  " "  " "   
state    real   rdzw           ikj     dyn_em      1         Z       -        " "  " "  " "   
state    real   pb             ikj     dyn_em      1         -      rhdu      "pb"          "BASE STATE PRESSURE "          "Pa"

#                                               
# Other dyn                                             
#                                               
i1       real   advect_tend    ikj     dyn_em      1       -                                          
state       real   alpha          ikj     dyn_em      1       -    h02
state       real   a              ikj     dyn_em      1       -    h02
state       real   gamma          ikj     dyn_em      1       -    h02
state       real   c2a            ikj     dyn_em      1       -    h02
state       real   rho            ikj     dyn_em      1       -    h02
state       real   phm            ikj     dyn_em      1       -    h02
state       real   cqu            ikj     dyn_em      1       -    h02
state       real   cqv            ikj     dyn_em      1       -    h02
state       real   cqw            ikj     dyn_em      1       -    h02
state       real   pm1            ikj     dyn_em      1       -    h02
i1       real   sr             ij      dyn_em      1         -     -   # dummy array for microphysics driver

state    real    fnm            k       dyn_em      1         -     irh02       "fnm"  "upper weight for vertical stretching"  ""
state    real    fnp            k       dyn_em      1         -     irh02       "fnp"  "lower weight for vertical stretching"  ""
state    real    rdnw           k       dyn_em      1         -     irh02       "rdnw"  "inverse d(eta) values between full (w) levels"   ""
state    real    rdn            k       dyn_em      1         -     irh02       "rdn"  "inverse d(eta) values between half (mass) levels"   ""
state    real    dnw            k       dyn_em      1         -     irh02       "dnw" "d(eta) values between full (w) levels"   ""
state    real    dn             k       dyn_em      1         -     irh02       "dn " "d(eta) values between half (mass) levels"   ""
state    real    znu            k       dyn_em      1         -     irh02     "znu"  "eta values on half (mass) levels"  ""
state    real    znw            k       dyn_em      1         Z     i0123rh02  "znw"  "eta values on full (w) levels"  ""
state    real    t_base         k       dyn_em      1         -     ir        "t_base"               "BASE STATET T IN IDEALIZED CASES"         "K"      
state    real    z              ikj     dyn_em      1       Z     -         " " " " " "
i1       real    mu_3d          ikj     dyn_em      1         -                                          
i1       real    z_at_w         ikj     dyn_em      1       Z 
state    real    cfn            -       misc      -         -     irh02       "cfn"    "extrapolation constant"  ""
state    real    cfn1           -       misc      -         -     irh02       "cfn1"   "extrapolation constant"  ""
state    real    epsts          -       misc      -         -     irh02       "epsts"  "leapfrog time filter constant"  ""
state    integer step_number    -       misc      -         -     ir        "step_number"  ""

# 2m and 10m output diagnostics
state    real   Q2               ij     misc        1         -     irh02       "Q2"                   "QV at 2 M"         "kg kg-1"
state    real   T2               ij     misc        1         -     irh02       "T2"                   "TEMP at 2 M"       "K"
state    real   TH2              ij     misc        1         -     irh02       "TH2"                  "POT TEMP at 2 M"   "K"
state    real   PSFC             ij     misc        1         -      rh       "PSFC"                 "SFC PRESSURE"      "Pa"

# these next 2 are for the HFSoLE/PET demo; writing these to auxhist1 output over MCEL for coupling
# with wave model, only if compiled with -DMCELIO, JM 2003/05/29
state    real   U10              ij     misc        1         -     irh012       "U10"                "U at 10 M"         "m s-1"
state    real   V10              ij     misc        1         -     irh012       "V10"                "V at 10 M"         "m s-1"

# Other
state   real    rdx            -        misc      -         -     irh02       "rdx"                   "INVERSE X GRID LENGTH"         ""      
state   real    rdy            -        misc      -         -     irh02       "rdy"                   "INVERSE Y GRID LENGTH"         ""      
state   real    dts            -        misc      -         -     irh02       "dts"                   "SMALL TIMESTEP"         ""      
state   real    dtseps         -        misc      -         -     irh02       "dtseps"                "TIME WEIGHT CONSTANT FOR SMALL STEPS"         ""      
state   real    resm           -        misc      -         -     irh02       "resm"                  "TIME WEIGHT CONSTANT FOR SMALL STEPS"         ""      
state   real    zetatop        -        misc      -         -     irh02       "zetatop"               "ZETA AT MODEL TOP"         ""      
state   real    cf1            -        misc      -         -     irh02       "cf1"                   "2nd order extrapolation constant"         ""      
state   real    cf2            -        misc      -         -     irh02       "cf2"                   "2nd order extrapolation constant"         ""      
state   real    cf3            -        misc      -         -     irh02       "cf3"                   "2nd order extrapolation constant"         ""      
state   integer number_at_same_level \
                               -        -         -         -     -         "number_at_same_level"  ""         ""      
state   integer itimestep      -        -         -         -     rh        "itimestep"             ""         ""      

# new variables. hans huang.

# for noise diagnoses

state    real   dpsdt           ij     misc      1          -     h        "dpsdt" "surface pressure tendency" "hPa/3h"
state    real   dmudt           ij     misc      1          -     h        "dmudt" "mu tendency" "hPa/3h"
state    real   psfcm           ij     misc      1          -     h        "psfcm" "surface pressure at previous step" "Pa"
state    real   avg_abs_dpsdt   -      misc      -          -     h        "avg_abs_dpsdt" "average absolute surface pressure (psfc) tendency" "hPa/3h"
state    real   avg_abs_dmudt   -      misc      -          -     h        "avg_abs_dmudt" "average absolute mu tendency" "hPa/3h"

# for moisture convergences

state    real   q_conve_3d      ikj    misc      1          -     h         "q_conve_3d" "Water vapour convergence (3d)"   "(g/kg)/3h"
state    real   q_conve_2d      ij     misc      1          -     h         "q_conve_2d" "Water vapour convergence (2d)"   "(g/kg)/s"

# end of new variables. hans huang and james done.

# Output file descriptor for a domain                                           
state   integer oid            -        -          -         -     -         "oid"                   ""         ""      
state   integer auxhist1_oid   -        -          -         -     -         "auxhist1_oid"          ""         ""
state   integer auxhist2_oid   -        -          -         -     -         "auxhist2_oid"          ""         ""
state   integer auxhist3_oid   -        -          -         -     -         "auxhist3_oid"          ""         ""
state   integer auxhist4_oid   -        -          -         -     -         "auxhist4_oid"          ""         ""
state   integer auxhist5_oid   -        -          -         -     -         "auxhist5_oid"          ""         ""
state   integer auxinput1_oid  -        -          -         -     -         "auxinput1_oid"         ""         ""
state   integer auxinput2_oid  -        -          -         -     -         "auxinput2_oid"         ""         ""
state   integer auxinput3_oid  -        -          -         -     -         "auxinput3_oid"         ""         ""
state   integer auxinput4_oid  -        -          -         -     -         "auxinput4_oid"         ""         ""
state   integer auxinput5_oid  -        -          -         -     -         "auxinput5_oid"         ""         ""
# input file descriptor for lbcs on parent domain                                               
state   integer lbc_fid        -        -          -         -     -         "lbc_fid"               ""         ""      
# indicates if tiling has been computed                                         
state   logical tiled          -        -          -         -     -         "tiled"                 ""         ""      
# indicates if patches have been computed                                               
state   logical patched        -        -          -         -     -         "patched"               ""         ""      
# indicates whether to read input from file or generate                                         
#state   logical input_from_file        -        -          -         -     -         "input_from_file"         ""         ""    

# Mask for moving nest interpolations
state    integer imask_nostag         ij      misc     1     -
state    integer imask_xstag          ij      misc     1     X
state    integer imask_ystag          ij      misc     1     Y
state    integer imask_xystag         ij      misc     1     XY 

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Scalar (4D) arrays

# Moist Scalars
#                                               
# The first line ensures that there will be identifiers named moist and                                         
# moist_tend even if there are not any moist scalars (so the essentially                                                
# dry code will will still link properly)                                               
#                                               
state   real    -              ikjft   moist       2       -     -    -                                       
state   real    qv             ikjft   moist       2       -     \
   i0123rh02usdf=(bdy_interp:dt,rqv_b,rqv_bt)  "QVAPOR"           "Water vapor mixing ratio"      "kg kg-1"
state   real    qc             ikjft   moist       2       -     \
   i0123rh02usdf=(bdy_interp:dt,rqc_b,rqc_bt)  "QCLOUD"           "Cloud water mixing ratio"      "kg kg-1"
state   real    qr             ikjft   moist       2       -     \
   i0123rh02usdf=(bdy_interp:dt,rqr_b,rqr_bt)  "QRAIN"            "Rain water mixing ratio"       "kg kg-1"
state   real    qi             ikjft   moist       2       -     \
   i0123rh02usdf=(bdy_interp:dt,rqi_b,rqi_bt)  "QICE"             "Ice mixing ratio"              "kg kg-1"
state   real    qs             ikjft   moist       2       -     \
   i0123rh02usdf=(bdy_interp:dt,rqs_b,rqs_bt)  "QSNOW"            "Snow mixing ratio"             "kg kg-1"
state   real    qg             ikjft   moist       2       -     \
   i0123rh02usdf=(bdy_interp:dt,rqg_b,rqg_bt)  "QGRAUP"           "Graupel mixing ratio"          "kg kg-1"

# Chem Scalars
state   real    -              ikjft   chem        2         -     -    -                                       

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Arrays for Specified LBCs

state    real   u_b            ikjb      dyn_em      -ag        X     b         "ru_b"                  "MU/MSF COUPLED X WIND AT BOUNDARIES"         ""      
state    real   u_bt           ikjb      dyn_em      -ag        X     b         "ru_bt"                 "MU/MSF COUPLED X WIND TENDENCY AT BOUNDARIES"         ""      
state    real   v_b            ikjb      dyn_em      -ag        Y     b         "rv_b"                  "MU/MSF COUPLED Y WIND AT BOUNDARIES"         ""      
state    real   v_bt           ikjb      dyn_em      -ag        Y     b         "rv_bt"                 "MU/MSF COUPLED Y WIND TENDENCY AT BOUNDARIES"         ""      

state    real   w_b            ikjb      dyn_em      -ag        Z     b         "rw_b"                  "MU/MSF COUPLED Z WIND AT BOUNDARIES"         ""
state    real   w_bt           ikjb      dyn_em      -ag        Z     b         "rw_bt"                 "MU/MSF COUPLED Z WIND TENDENCY AT BOUNDARIES"         ""

state    real   ph_b           ikjb      dyn_em      -ag        Z     b         "ph_b"                  "MU COUPLED HEIGHT AT BOUNDARIES"         ""
state    real   ph_bt          ikjb      dyn_em      -ag        Z     b         "ph_bt"                 "MU COUPLED HEIGHT TENDENCY AT BOUNDARIES"         ""
state    real   t_b            ikjb      dyn_em      -ag        -     b         "t_b"                   "MU COUPLED THETA PERTURBATION AT BOUNDARIES"         ""
state    real   t_bt           ikjb      dyn_em      -ag        -     b         "t_bt"                  "MU COUPLED THETA PERTURBATION TENDENCY AT BOUNDARIES"         ""
state    real   mu_b           ijb       dyn_em      -ag        -     b         "mu_b"                  "MU AT BOUNDARIES"         ""
state    real   mu_bt          ijb       dyn_em      -ag        -     b         "mu_bt"                 "MU TENDENCY AT BOUNDARIES"         ""
state    real   rqv_b          ikjb      dyn_em      -ag        -     b         "rqv_b"                 "MU COUPLED WATER VAPOR MIXING RATIO AT BOUNDARIES"         ""
state    real   rqv_bt         ikjb      dyn_em      -ag        -     b         "rqv_bt"                "MU COUPLED WATER VAPOR MIXING RATIO TENDENCY AT BOUNDARIES"         ""

state    real   rqc_b          ikjb      misc        -ag        -     b         "rqc_b"                 "MU COUPLED WATER CLOUD MIXING RATIO AT BOUNDARIES"         ""
state    real   rqc_bt         ikjb      misc        -ag        -     b         "rqc_bt"                "MU COUPLED WATER CLOUD MIXING RATIO TENDENCY AT BOUNDARIES"         ""
state    real   rqr_b          ikjb      misc        -ag        -     b         "rqr_b"                 "MU COUPLED WATER RAIN MIXING RATIO AT BOUNDARIES"         ""
state    real   rqr_bt         ikjb      misc        -ag        -     b         "rqr_bt"                "MU COUPLED WATER RAIN MIXING RATIO TENDENCY AT BOUNDARIES"         ""
state    real   rqi_b          ikjb      misc        -ag        -     b         "rqi_b"                 "MU COUPLED WATER ICE MIXING RATIO AT BOUNDARIES"         ""
state    real   rqi_bt         ikjb      misc        -ag        -     b         "rqi_bt"                "MU COUPLED WATER ICE MIXING RATIO TENDENCY AT BOUNDARIES"         ""
state    real   rqs_b          ikjb      misc        -ag        -     b         "rqs_b"                 "MU COUPLED WATER SNOW MIXING RATIO AT BOUNDARIES"         ""
state    real   rqs_bt         ikjb      misc        -ag        -     b         "rqs_bt"                "MU COUPLED WATER SNOW MIXING RATIO TENDENCY AT BOUNDARIES"         ""
state    real   rqg_b          ikjb      misc        -ag        -     b         "rqg_b"                 "MU COUPLED WATER GRAUPEL MIXING RATIO AT BOUNDARIES"         ""
state    real   rqg_bt         ikjb      misc        -ag        -     b         "rqg_bt"                "MU COUPLED WATER GRAUPEL MIXING RATIO TENDENCY AT BOUNDARIES"         ""


state    real   fcx            w         misc     -       -      irh02      "fcx"                  "RELAXATION TERM FOR BOUNDARY ZONE"         ""
state    real   gcx            w         misc     -       -      irh02      "gcx"                  "2ND RELAXATION TERM FOR BOUNDARY ZONE"         ""
state    real   dtbc            -        misc     -       -      irh02      "dtbc"                 "TIME SINCE BOUNDARY READ"         ""

#-------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------
# Physics Related State Varibles

#-------------------------------------------------------------------------------------------------------------------------------------------
# SI - start variables from netCDF format from Standard Initialization, most eventually for use in LSM schemes
#-------------------------------------------------------------------------------------------------------------------------------------------

state   real   sm000010            ij    misc          1     -     i1      "SM000010"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sm010040            ij    misc          1     -     i1      "SM010040 "     "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sm040100            ij    misc          1     -     i1      "SM040100 "     "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sm100200            ij    misc          1     -     i1      "SM100200 "     "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sm010200            ij    misc          1     -     i1      "SM010200"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm000            ij    misc          1     -     i1      "SOILM000"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm005            ij    misc          1     -     i1      "SOILM005"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm020            ij    misc          1     -     i1      "SOILM020"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm040            ij    misc          1     -     i1      "SOILM040"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm160            ij    misc          1     -     i1      "SOILM160"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm300            ij    misc          1     -     i1      "SOILM300"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sw000010            ij    misc          1     -     i1      "SW000010"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   sw010040            ij    misc          1     -     i1      "SW010040"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   sw040100            ij    misc          1     -     i1      "SW040100"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   sw100200            ij    misc          1     -     i1      "SW100200"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   sw010200            ij    misc          1     -     i1      "SW010200"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw000            ij    misc          1     -     i1      "SOILW000"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw005            ij    misc          1     -     i1      "SOILW005"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw020            ij    misc          1     -     i1      "SOILW020"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw040            ij    misc          1     -     i1      "SOILW040"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw160            ij    misc          1     -     i1      "SOILW160"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw300            ij    misc          1     -     i1      "SOILW300"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   st000010            ij    misc          1     -     i1      "ST000010"      "LAYER SOIL TEMPERATURE" "K"
state   real   st010040            ij    misc          1     -     i1      "ST010040"      "LAYER SOIL TEMPERATURE" "K"
state   real   st040100            ij    misc          1     -     i1      "ST040100"      "LAYER SOIL TEMPERATURE" "K"
state   real   st100200            ij    misc          1     -     i1      "ST100200"      "LAYER SOIL TEMPERATURE" "K"
state   real   st010200            ij    misc          1     -     i1      "ST010200"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt000            ij    misc          1     -     i1      "SOILT000"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt005            ij    misc          1     -     i1      "SOILT005"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt020            ij    misc          1     -     i1      "SOILT020"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt040            ij    misc          1     -     i1      "SOILT040"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt160            ij    misc          1     -     i1      "SOILT160"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt300            ij    misc          1     -     i1      "SOILT300"      "LAYER SOIL TEMPERATURE" "K"
state   real   landmask            ij    misc          1     -     i0123rh02d=(interp_fcnm)u=(copy_fcnm)   "LANDMASK"      "LAND MASK (1 FOR LAND, 0 FOR WATER)"  ""
state   real   topostdv            ij    misc          1     -     i1      "TOPOSTDV"      "ELEVATION STD DEV"  "m"
state   real   toposlpx            ij    misc          1     -     i1      "TOPOSLPX"      "ELEVATION X SLOPE"  ""
state   real   toposlpy            ij    misc          1     -     i1      "TOPOSLPY"      "ELEVATION Y SLOPE"  ""
state   real   shdmax              ij    misc          1     -     i01r    "SHDMAX"        "ANNUAL MAX VEG FRACTION" ""
state   real   shdmin              ij    misc          1     -     i01r    "SHDMIN"        "ANNUAL MIN VEG FRACTION" ""
state   real   snoalb              ij    misc          1     -     i01r    "SNOALB"        "ANNUAL MAX SNOW ALBEDO IN FRACTION" "
state   real   slopecat            ij    misc          1     -     i1      "SLOPECAT"      "SLOPE CATEGORY"  ""
state   real   toposoil            ij    misc          1     -     i1      "SOILHGT"       "ELEVATION OF LSM DATA"  "m"
state   real   landusef            iuj   misc          1     Z     i1      "LANDUSEF"      "LANDUSE FRACTION BY CATEGORY"  ""
state   real   soilctop            isj   misc          1     Z     i1      "SOILCTOP"      "SOIL CAT FRACTION (TOP)"  ""
state   real   soilcbot            isj   misc          1     Z     i1      "SOILCBOT"      "SOIL CAT FRACTION (BOTTOM)"  ""
state   real   soilcat             ij    misc          1     -     i1      "SOILCAT"       "SOIL CAT DOMINANT TYPE" ""
state   real   vegcat              ij    misc          1     -     i1      "VEGCAT"        "VEGETATION CAT DOMINANT TYPE" ""

#---------------------------------------------------------------------------------------------------------------------------------------
# SI - end variables from netCDF format from Standard Initialization
#---------------------------------------------------------------------------------------------------------------------------------------

# soil model variables  (Note that they are marked as staggered in the vertical dimension
# because they are "fully dimensioned" -- they use every element in that dim
state    real   TSLB           ilj       misc      1         Z     irh02d=(interp_mask_land_field:lu_index)u=(copy_fcnm)           "TSLB"     "SOIL TEMPERATURE"   "K"
state    real   ZS              l        misc      -         Z     irh02       "ZS"                    "DEPTHS OF CENTERS OF SOIL LAYERS"         "m"
state    real   DZS             l        misc      -         Z     irh02       "DZS"                   "THICKNESSES OF SOIL LAYERS"               "m"

# lsm State Variables

state    real   SMOIS            ilj     -          1         Z     irh02d=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "SMOIS"            "SOIL MOISTURE"     "m3 m-3"
state    real   SH2O             ilj     -          1         Z     irh02d=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "SH2O"             "SOIL LIQUID WATER" "m3 m-3"
state    real   XICE             ij     misc        1         -     i01rh02d=(interp_mask_water_field:lu_index)u=(copy_fcnm)  "XICE"             "SEA ICE FLAG"  ""
state    real   SMSTAV           ij     misc        1         -      rd=(interp_mask_land_field:lu_index)       "SMSTAV"           "MOISTURE AVAILABILITY" ""
state    real   SMSTOT           ij     misc        1         -      r                                          "SMSTOT"           "TOTAL SOIL MOISTURE" "m3 m-3"

state    real   SFCRUNOFF        ij     misc        1         -      rhd=(interp_mask_land_field:lu_index)      "SFROFF"           "SURFACE RUNOFF"     "mm"
state    real   UDRUNOFF         ij     misc        1         -      rhd=(interp_mask_land_field:lu_index)      "UDROFF"           "UNDERGROUND RUNOFF" "mm"
state  integer  IVGTYP           ij     misc        1         -     irh02d=(interp_fcni)u=(copy_fcni)            "IVGTYP"           "DOMINANT VEGETATION CATEGORY" ""
state  integer  ISLTYP           ij     misc        1         -     irh02d=(interp_fcni)u=(copy_fcni)            "ISLTYP"           "DOMINANT SOIL CATEGORY"       ""
state    real   VEGFRA           ij     misc        1         -     i01rh02d=(interp_mask_land_field:lu_index)u=(copy_fcnm)   "VEGFRA"           "VEGETATION FRACTION" ""
state    real   SFCEVP           ij     misc        1         -      r                                          "SFCEVP"           "SURFACE EVAPORATION" "kg m-2"
state    real   GRDFLX           ij     misc        1         -      rh                                         "GRDFLX"           "GROUND HEAT FLUX" "W m-2"
state    real   SFCEXC           ij     misc        1         -      r                                          "SFCEXC "          "SURFACE EXCHANGE COEFFICIENT"   "m s-1"

state    real   ACSNOW           ij     misc        1         -      rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)       "ACSNOW"           "ACCUMULATED SNOW"         "kg m-2"
state    real   ACSNOM           ij     misc        1         -      rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)       "ACSNOM"           "ACCUMULATED MELTED SNOW"  "kg m-2"
state    real   SNOW             ij     misc        1         -     i01rh2d=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "SNOW"             "SNOW WATER EQUIVALENT"    "kg m-2"
state    real   SNOWH            ij     misc        1         -     i01rh2d=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "SNOWH"            "PHYSICAL SNOW DEPTH"      "m"
state    real   CANWAT           ij     misc        1         -     i01rh2d=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "CANWAT"           "CANOPY WATER"             "kg m-2"
state    real   SST              ij     misc        1         -     i01rh2d=(interp_mask_water_field:lu_index)   "SST"              "SEA SURFACE TEMPERATURE" "K"
state  integer  IFNDSNOWH        -      misc        1         -     i         "FNDSNOWH" "SNOWH_LOGICAL"
state  integer  IFNDSOILW        -      misc        1         -     i         "FNDSOILW" "SOILW_LOGICAL"

# RUC LSM
state    real   SMFR3D           ilj    misc        1         Z      r        "SMFR3D"               "SOIL ICE" ""
state    real   KEEPFR3DFLAG     ilj    misc        1         Z      r        "KEEPFR3DFLAG          "FLAG - 1. FROZEN SOIL YES, 0 - NO"             ""

# MRF PBL variables***25Aug 2002*****
i1      real   PSIM           ij     misc        1         -     -         "PSIM"                "SIMILARITY FUNCTION FOR MOMENTUM"     ""
i1      real   PSIH           ij     misc        1         -     -         "PSIH"                "SIMILARITY FUNCTION FOR HEAT"         ""
i1      real   WSPD           ij     misc        1         -     -         "WSPD"                "Wind speed"                           "m s-1"
i1      real   GZ1OZ0         ij     misc        1         -     -         "GZ1OZ0"              "LOG OF Z1 over Z0"                     ""
i1      real   BR             ij     misc        1         -     -         "BR"                  "Bulk Richardson"                       ""

# MYJ PBL variables
state    real   tke_myj        ikj     misc        1         -      r        "tke_myj"               "TKE FROM MELLOR-YAMADA-JANJIC      "m2 s-2"
state    real   EXCH_H          ikj     misc        1         -     r          "EXCH_H"               "EXCHANGE COEFFICIENTS "
state    real  CT              ij      misc        1         -      r        "CT"                    "COUNTERGRADIENT TERM"    "K"
state   real   THZ0             ij     misc        1         -      r        "THZ0"                  "POTENTIAL TEMPERATURE AT ZNT"                 "K"
state    real  Z0               ij     misc        1         -      r        "Z0"                    "Background ROUGHNESS LENGTH"                  "m"
state   real   QZ0              ij     misc        1         -      r        "QZ0"                   "SPECIFIC HUMIDITY AT ZNT"                     "kg kg-1"
state   real   UZ0              ij     misc        1         -      r        "UZ0"                   "U WIND COMPONENT AT ZNT"                      "m s-1"
state   real   VZ0              ij     misc        1         -      r        "VZ0"                   "V WIND COMPONENT AT ZNT"                      "m s-1"
state   real   QSFC             ij     misc        1         -      r        "QSFC"                  "SPECIFIC HUMIDITY AT LOWER BOUNDARY"          "kg kg-1"
state   real   AKHS             ij     misc        1         -      r        "AKHS"                  "SFC EXCH COEFF FOR HEAT"                      "m s-1"    
state   real   AKMS             ij     misc        1         -      r        "AKMS"                  "SFC EXCH COEFF FOR MOMENTUM"                  "m s-1"    
state   integer KPBL            ij     misc        1         -     r         "KPBL"                  "LEVEL OF PBL TOP"                             ""
i1      real   TSHLTR           ij     misc        1         -     -         "TSHLTR"                "SHELTER THETA FROM MYJ"                       "K"         
i1      real   QSHLTR           ij     misc        1         -     -         "QSHLTR"                "SHELTER SPECIFIC HUMIDITY FROM MYJ"           "kg kg-1"
i1      real   PSHLTR           ij     misc        1         -     -         "PSHLTR"                "SHELTER PRESSURE FROM MYJ"           "Pa"
i1      real   TH10             ij     misc        1         -     -         "TH10"                  "10-M THETA FROM MYJ"                          "K"        
i1      real   Q10              ij     misc        1         -     -         "Q10"                   "10-M SPECIFIC HUMIDITY FROM MYJ"              "kg kg-1"
i1      real   CHKLOWQ          ij     misc        1         -     -         "CHKLOWQ"               "SURFACE SATURATION FLAG"        ""

# gfdl (eta) radiation State Variables
state    real    HTOP            ij     misc        1         -      r        "HTOP"                 "TOP OF CONVECTION LELVEL"         ""
state    real    HBOT            ij     misc        1         -      r        "HBOT"                 "BOT OF CONVECTION LELVEL"         ""
state    REAL      CUPPT         ij     misc        1         -      r        "CUPPT"                "ACCUMULATED CONVECTIVE RAIN SINC LAST CALL TO THE RADIATION"         ""
state  real   totswdn   ij    misc        1    -    i
state  real   totlwdn   ij    misc        1    -    i
state  real   rswtoa    ij    misc        1    -    i
state  real   rlwtoa    ij    misc        1    -    i
state  real   czmean    ij    misc      1    -     i
state  real   cfracl    ij    misc      1    -     i
state  real   cfracm    ij    misc      1    -     i
state  real   cfrach    ij    misc      1    -     i
state  real   acfrst    ij    misc        1    -    i
state integer ncfrst    ij    misc        1    -    i
state  real   acfrcv    ij    misc        1    -    i
state integer ncfrcv    ij    misc        1    -    i

# related to radiation for WRF chem
state    real  aer_dry        ikj     misc        1         -      r        "MASS_AER_DRY"          "dry aerosol mass"                     "?"
state    real  aer_water      ikj     misc        1         -      r        "MASS_AER_WATER"        "aerosol li quid water content"        "?"

# new eta microphpysics State Variables
state    real    F_ICE_PHY       ikj    misc        1         -      rdu      "F_ICE_PHY"            "FRACTION OF ICE"         ""
state    real    F_RAIN_PHY      ikj    misc        1         -      rdu      "F_RAIN_PHY"           "FRACTION OF RAIN "         ""
state    real    F_RIMEF_PHY     ikj    misc        1         -      rdu      "F_RIMEF_PHY"          "MASS RATIO OF RIMED ICE "         ""

# Other Misc State Variables                                            
state   real    h_diabatic     ikj     misc         1         -      r        "h_diabatic"            "PREVIOUS TIMESTEP CONDENSATIONAL HEATING"         ""      
state   real    msft           ij      misc         1         -     i012rh02du=(copy_fcnm)    "MAPFAC_M"         "Map scale factor on mass grid" ""
state   real    msfu           ij      misc         1         X     i012rh02du=(copy_fcnm)    "MAPFAC_U"         "Map scale factor on u-grid" ""
state   real    msfv           ij      misc         1         Y     i012rh02du=(copy_fcnm)    "MAPFAC_V"         "Map scale factor on v-grid" ""
state   real    f              ij      misc         1         -     i012rh02du=(copy_fcnm)    "f"                "Coriolis sine latitude term"  "s-1"
state   real    e              ij      misc         1         -     i012rh02du=(copy_fcnm)    "e"                "Coriolis cosine latitude term"  "s-1"
state   real    sina           ij      misc         1         -     i012rh02du=(copy_fcnm)    "SINALPHA"         "Local sine of map rotation"   ""
state   real    cosa           ij      misc         1         -     i012rh02du=(copy_fcnm)    "COSALPHA"         "Local cosine of map rotation"  ""
state   real    ht             ij      misc         1         -     i012rh02dus  "HGT"              "Terrain Height"   "m"
state   real    ht_fine        ij      misc         1         -     -          "HGT_FINE"         "Fine Terrain Height"   "m"
state   real    ht_int         ij      misc         1         -     -          "HGT_INT"          "Terrain Height Horizontally Interpolated"   "m"
state   real    ht_input       ij      misc         1         -     -          "HGT_INPUT"        "Terrain Height from FG Input File"   "m"

state   real    TSK            ij      misc         1         -     i012rh02du=(copy_fcnm)    "TSK"                   "SURFACE SKIN TEMPERATURE"                  "K"
state   real    TSK_SAVE       ij      misc         1         -     -         "TSK_SAVE" "SURFACE SKIN TEMPERATURE, EXTRA COPY FOR SEA ICE TESTS in REAL"       "K"
state   real    u_base         k       misc         1         -     ir        "u_base"                "BASE STATE X WIND IN IDEALIZED CASES"         ""      
state   real    v_base         k       misc         1         -     ir        "v_base"                "BASE STATE Y WIND IN IDEALIZED CASES"         ""      
state   real    qv_base        k       misc         1         -     ir        "qv_base"               "BASE STATE QV IN IDEALIZED CASES"         ""      
state   real    z_base         k       misc         1         -     ir        "z_base"                "BASE STATE HEIGHT IN IDEALIZED CASES"         ""      
state   real    u_frame        -       misc         1         -     ir        "u_frame"               "FRAME X WIND"         "m s-1"      
state   real    v_frame        -       misc         1         -     ir        "v_frame"               "FRAME Y WIND"         "m s-1"      
# p_top appears as metadata between SI and real but as a state variable in real and WRF
# since it is a scalar and a constant, it makes sense to have it as metadata -- there
# are, however, probably post-processing programs that expect to see it as an I/O record
# another problem: share/input_wrf tries to read this as metadata (fine for real reading
# SI, but with model reading real output, it generates a warning when debug is > 0 in
# namelist and causes repeated questions from users.  A third problem is the potential
# collision between a metadata name and a field record in the I/O data
# resolve this how?  Have the real program throw a switch to tell the code to get it
# from the metadata?  Otherwise it's a field?
state   real    p_top          -       misc         -         -     irh       "p_top"                 "PRESSURE TOP OF THE MODEL"  "Pa"
state   integer imicrogram     -       misc         -         -      r        "imicrogram"            "flag 0/1 0=mixratio, 1=mcrograms/m3"  ""
state   real    lat_ll_t       -       dyn_em       -         -     irh       "lat_ll_t"              "latitude lower left, temp point" "degrees"
state   real    lat_ul_t       -       dyn_em       -         -     irh       "lat_ul_t"              "latitude up left, temp point" "degrees"
state   real    lat_ur_t       -       dyn_em       -         -     irh       "lat_ur_t"              "latitude up right, temp point" "degrees"
state   real    lat_lr_t       -       dyn_em       -         -     irh       "lat_lr_t"              "latitude lower right, temp point" "degrees"
state   real    lat_ll_u       -       dyn_em       -         -     irh       "lat_ll_u"              "latitude lower left, u point" "degrees"
state   real    lat_ul_u       -       dyn_em       -         -     irh       "lat_ul_u"              "latitude up left, u point" "degrees"
state   real    lat_ur_u       -       dyn_em       -         -     irh       "lat_ur_u"              "latitude up right, u point" "degrees"
state   real    lat_lr_u       -       dyn_em       -         -     irh       "lat_lr_u"              "latitude lower right, u point" "degrees"
state   real    lat_ll_v       -       dyn_em       -         -     irh       "lat_ll_v"              "latitude lower left, v point" "degrees"
state   real    lat_ul_v       -       dyn_em       -         -     irh       "lat_ul_v"              "latitude up left, v point" "degrees"
state   real    lat_ur_v       -       dyn_em       -         -     irh       "lat_ur_v"              "latitude up right, v point" "degrees"
state   real    lat_lr_v       -       dyn_em       -         -     irh       "lat_lr_v"              "latitude lower right, v point" "degrees"
state   real    lat_ll_d       -       dyn_em       -         -     irh       "lat_ll_d"              "latitude lower left, massless point" "degrees"
state   real    lat_ul_d       -       dyn_em       -         -     irh       "lat_ul_d"              "latitude up left, massless point" "degrees"
state   real    lat_ur_d       -       dyn_em       -         -     irh       "lat_ur_d"              "latitude up right, massless point" "degrees"
state   real    lat_lr_d       -       dyn_em       -         -     irh       "lat_lr_d"              "latitude lower right, massless point" "degrees"
state   real    lon_ll_t       -       dyn_em       -         -     irh       "lon_ll_t"              "longitude lower left, temp point" "degrees"
state   real    lon_ul_t       -       dyn_em       -         -     irh       "lon_ul_t"              "longitude up left, temp point" "degrees"
state   real    lon_ur_t       -       dyn_em       -         -     irh       "lon_ur_t"              "longitude up right, temp point" "degrees"
state   real    lon_lr_t       -       dyn_em       -         -     irh       "lon_lr_t"              "longitude lower right, temp point" "degrees"
state   real    lon_ll_u       -       dyn_em       -         -     irh       "lon_ll_u"              "longitude lower left, u point" "degrees"
state   real    lon_ul_u       -       dyn_em       -         -     irh       "lon_ul_u"              "longitude up left, u point" "degrees"
state   real    lon_ur_u       -       dyn_em       -         -     irh       "lon_ur_u"              "longitude up right, u point" "degrees"
state   real    lon_lr_u       -       dyn_em       -         -     irh       "lon_lr_u"              "longitude lower right, u point" "degrees"
state   real    lon_ll_v       -       dyn_em       -         -     irh       "lon_ll_v"              "longitude lower left, v point" "degrees"
state   real    lon_ul_v       -       dyn_em       -         -     irh       "lon_ul_v"              "longitude up left, v point" "degrees"
state   real    lon_ur_v       -       dyn_em       -         -     irh       "lon_ur_v"              "longitude up right, v point" "degrees"
state   real    lon_lr_v       -       dyn_em       -         -     irh       "lon_lr_v"              "longitude lower right, v point" "degrees"
state   real    lon_ll_d       -       dyn_em       -         -     irh       "lon_ll_d"              "longitude lower left, massless point" "degrees"
state   real    lon_ul_d       -       dyn_em       -         -     irh       "lon_ul_d"              "longitude up left, massless point" "degrees"
state   real    lon_ur_d       -       dyn_em       -         -     irh       "lon_ur_d"              "longitude up right, massless point" "degrees"
state   real    lon_lr_d       -       dyn_em       -         -     irh       "lon_lr_d"              "longitude lower right, massless point" "degrees"
                                                
# Other physics variables
                                                
state    real  RTHCUTEN        ikj      misc        1         -      r        "RTHCUTEN"              "COUPLED THETA TENDENCY DUE TO CUMULUS SCHEME"     "Pa K s-1"
state    real  RQVCUTEN        ikj      misc        1         -      r        "RQVCUTEN"              "COUPLED Q_V TENDENCY DUE TO CUMULUS SCHEME"       "Pa kg kg-1 s-1"
state    real  RQRCUTEN        ikj      misc        1         -      r        "RQRCUTEN"              "COUPLED Q_R TENDENCY DUE TO CUMULUS SCHEME"       "Pa kg kg-1 s-1"
state    real  RQCCUTEN        ikj      misc        1         -      r        "RQCCUTEN"              "COUPLED Q_C TENDENCY DUE TO CUMULUS SCHEME"       "Pa kg kg-1 s-1"
state    real  RQSCUTEN        ikj      misc        1         -      r        "RQSCUTEN"              "COUPLED Q_S TENDENCY DUE TO CUMULUS SCHEME"       "Pa kg kg-1 s-1"
state    real  RQICUTEN        ikj      misc        1         -      r        "RQICUTEN"              "COUPLED Q_I TENDENCY DUE TO CUMULUS SCHEME"       "Pa kg kg-1 s-1"
state    real  W0AVG           ikj      misc        1         -      r        "W0AVG"                 "AVERAGE VERTICAL VELOCITY FOR KF CUMULUS SCHEME"         "m s-1"
                                                
state    real  RAINC            ij      misc        1         -      rhdu     "RAINC"                 "ACCUMULATED TOTAL CUMULUS PRECIPITATION"                 "mm"      
state    real  RAINNC           ij      misc        1         -      rhdu     "RAINNC"                "ACCUMULATED TOTAL GRID SCALE PRECIPITATION"              "mm"      
state    real  RAINCV           ij      misc        1         -      r        "RAINCV"                "TIME-STEP CUMULUS PRECIPITATION"                         "mm"      
state    real  RAINNCV          ij      misc        1         -      r        "RAINNCV"               "TIME-STEP NONCONVECTIVE PRECIPITATION"                   "mm"
state    real  RAINBL           ij      misc        1         -      r        "RAINBL"                "PBL TIME-STEP TOTAL PRECIPITATION"                       "mm"      
state    real  NCA              ij      misc        1         -      r        "NCA"                   "COUNTER OF THE CLOUD RELAXATION TIME IN KF CUMULUS SCHEME"    ""      
state    integer  LOWLYR        ij      misc        1         -     -         "LOWLYR"                "INDEX OF LOWEST MODEL LAYER ABOVE THE GROUND IN BMJ SCHEME"   ""      
state    real  MASS_FLUX        ij      misc        1         -      r        "MASS_FLUX"             "DOWNDRAFT MASS FLUX FOR IN GRELL CUMULUS SCHEME"  "mb hour-1"
state    real  apr_gr           ij      misc         1         -      r       "APR_GR"                "PRECIP FROM CLOSURE OLD_GRELL"   "mm hour-1"
state    real  apr_w            ij      misc         1         -      r       "APR_W"                 "PRECIP FROM CLOSURE W"           "mm hour-1"
state    real  apr_mc           ij      misc         1         -      r       "APR_MC"                "PRECIP FROM CLOSURE KRISH MV"    "mm hour-1"
state    real  apr_st           ij      misc         1         -      r       "APR_ST"                "PRECIP FROM CLOSURE STABILITY"   "mm hour-1"
state    real  apr_as           ij      misc         1         -      r       "APR_AS"                "PRECIP FROM CLOSURE AS-TYPE"     "mm hour-1"
state    real  apr_capma        ij      misc         1         -      r       "APR_CAPMA"             "PRECIP FROM MAX CAP"             "mm hour-1"
state    real  apr_capme        ij      misc         1         -      r       "APR_CAPME"             "PRECIP FROM MEAN CAP"            "mm hour-1"
state    real  apr_capmi        ij      misc         1         -      r       "APR_CAPMI"             "PRECIP FROM MIN CAP"             "mm hour-1"
state    real  xf_ens           ije     misc        1         Z      r        "XF_ENS"                "MASS FLUX PDF IN GRELL CUMULUS SCHEME"    "mb hour-1"  
state    real  pr_ens           ije     misc        1         Z      r        "PR_ENS"                "PRECIP RATE PDF IN GRELL CUMULUS SCHEME"  "mb hour-1" 
state    real  RTHFTEN          ikj     misc        1         -      r        "RTHFTEN"               "TEMPERATURE TENDENCY USED IN GRELL CUMULUS SCHEME"  "K s-1"      
state    real  RQVFTEN          ikj     misc        1         -      r        "RQVFTEN"               "MOISTURE TENDENCY USED IN GRELL CUMULUS SCHEME"     "kg s-1"

state integer  STEPCU          -        misc        1         -      r        "STEPCU"                "NUMBER OF FUNDAMENTAL TIMESTEPS BETWEEN CONVECTION CALLS"  ""
                                                
state    real  RTHRATEN        ikj      misc        1         -      r        "RTHRATEN"              "COUPLED THETA TENDENCY DUE TO RADIATION"              "Pa K s-1"
state    real  RTHRATENLW      ikj      misc        1         -      r        "RTHRATLW"              "COUPLED THETA TENDENCY DUE TO LONG WAVE RADIATION"    "Pa K s-1"
state    real  RTHRATENSW      ikj      misc        1         -      r        "RTHRATSW"              "COUPLED THETA TENDENCY DUE TO SHORT WAVE RADIATION"   "Pa K s-1"
state    real  CLDFRA          ikj      misc        1         -      r        "CLDFRA"                "CLOUD FRACTION"                                       ""      
                                                
state    real  SWDOWN           ij      misc        1         -      rh       "SWDOWN"                "DOWNWARD SHORT WAVE FLUX AT GROUND SURFACE"           "W m-2"      
state    real  GSW              ij      misc        1         -      r        "GSW"                   "NET SHORT WAVE FLUX AT GROUND SURFACE"           "W m-2"      
state    real  GLW              ij      misc        1         -      rh       "GLW"                   "DOWNWARD LONG WAVE FLUX AT GROUND SURFACE"            "W m-2"      

# these next 2 are for the HFSoLE/PET demo; writing these to auxhist1 output over MCEL for coupling
# with wave model, only if compiled with -DMCELIO, JM 2003/05/29
state    real  XLAT             ij      misc        1         -     i012rh012du=(copy_fcnm)      "XLAT"                "LATITUDE, SOUTH IS NEGATIVE"                                          "degree_north"
state    real  XLONG            ij      misc        1         -     i012rh012du=(copy_fcnm)      "XLONG"               "LONGITUDE, WEST IS NEGATIVE"                                          "degree_east"
state    real  ALBEDO           ij      misc        1         -       r          "ALBEDO"                   "ALBEDO"
state    real  ALBBCK           ij      misc        1         -     i012rh02        "ALBBCK"                   "BACKGROUND ALBEDO"        ""
state    real  EMISS            ij      misc        1         -      r        "EMISS"                 "SURFACE EMISSIVITY"         "" 

state    real  CLDEFI           ij      misc        1         -      r        "CLDEFI"                "precipitation efficiency in BMJ SCHEME"    ""      
state integer  STEPRA          -        misc        1         -      r        "STEPRA"                "NUMBER OF FUNDAMENTAL TIMESTEPS BETWEEN RADIATION CALLS"        ""
                                                
state    real  RUBLTEN         ikj      misc        1         -      r        "RUBLTEN"               "COUPLED X WIND TENDENCY DUE TO PBL PARAMETERIZATION"  "Pa m s-2"
state    real  RVBLTEN         ikj      misc        1         -      r        "RVBLTEN"               "COUPLED Y WIND TENDENCY DUE TO PBL PARAMETERIZATION"  "Pa m s-2"
state    real  RTHBLTEN        ikj      misc        1         -      r        "RTHBLTEN"              "COUPLED THETA TENDENCY DUE TO PBL PARAMETERIZATION"   "Pa K s-1"
state    real  RQVBLTEN        ikj      misc        1         -      r        "RQVBLTEN"              "COUPLED Q_V TENDENCY DUE TO PBL PARAMETERIZATION"     "Pa kg kg-1 s-1"
state    real  RQCBLTEN        ikj      misc        1         -      r        "RQCBLTEN"              "COUPLED Q_C TENDENCY DUE TO PBL PARAMETERIZATION"     "Pa kg kg-1 s-1"
state    real  RQIBLTEN        ikj      misc        1         -      r        "RQIBLTEN"              "COUPLED Q_I TENDENCY DUE TO PBL PARAMETERIZATION"     "Pa kg kg-1 s-1"      
i1       real  th_phy          ikj      misc        1       -                                          
i1       real  pi_phy          ikj      misc        1       -                                          
i1       real  p_phy           ikj      misc        1       -                                          
i1       real  t_phy           ikj      misc        1       -                                          
i1       real  u_phy           ikj      misc        1       -                                          
i1       real  v_phy           ikj      misc        1       -                                          
i1       real  dz8w            ikj      misc        1       Z                                          
i1       real  p8w             ikj      misc        1       Z                                          
i1       real  t8w             ikj      misc        1       Z                                          
i1       real  rho_phy         ikj      misc        1       -                                          
i1    logical  CU_ACT_FLAG     ij       misc        1         -

                                                
state    real  TMN              ij      misc        1         -     i012rh02d=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "TMN"                   "SOIL TEMPERATURE AT LOWER BOUNDARY"           "K"
state    real  XLAND            ij      misc        1         -     i02rh02d=(interp_fcnm)u=(copy_fcnm)       "XLAND"                 "LAND MASK (1 FOR LAND, 2 FOR WATER)"          ""      
state    real  ZNT              ij      misc        1         -     i023rh02     "ZNT"                   "TIME-VARYING ROUGHNESS LENGTH"                "m"      
state    real  UST              ij      misc        1         -      r        "UST"                   "U* IN SIMILARITY THEORY"                      "m s-1"      
i1       real  HOL              ij      misc        1         -      -        "HOL"                   "PBL HEIGHT OVER MONIN-OBUKHOV LENGTH"         ""
state    real  RMOL             ij      misc        1         -     rh       "RMOL"                   "1./Monin Ob. Length"                      ""
state    real  MOL              ij      misc        1         -      r        "MOL"                   "T* IN SIMILARITY THEORY"                      "K"      
state    real  PBLH             ij      misc        1         -      rh       "PBLH"                  "PBL HEIGHT"         "m"      
state    real  CAPG             ij      misc        1         -      r        "CAPG"                  "HEAT CAPACITY FOR SOIL"                       "J K-1 m-3"      
state    real  THC              ij      misc        1         -      r        "THC"                   "THERMAL INERTIA"                              "Cal cm-1 K-1 s-0.5"      
state    real  HFX              ij      misc        1         -      rh       "HFX"                   "UPWARD HEAT FLUX AT THE SURFACE"              "W m-2"      
state    real  QFX              ij      misc        1         -      rh       "QFX"                   "UPWARD MOISTURE FLUX AT THE SURFACE"          "kg m-2 s-1"      
state    real  LH               ij      misc        1         -      rh       "LH"                    "LATENT HEAT FLUX AT THE SURFACE"              "W m-2"
state    real  FLHC             ij      misc        1         -      r        "FLHC"                  "SURFACE EXCHANGE COEFFICIENT FOR HEAT"       ""
state    real  FLQC             ij      misc        1         -      r        "FLQC"                  "SURFACE EXCHANGE COEFFICIENT FOR MOISTURE"   ""
state    real  QSG              ij      misc        1         -      r        "QSG"                   "SURFACE SATURATION WATER VAPOR MIXING RATIO   "kg kg-1"
state    real  QVG              ij      misc        1         -      r        "QVG"                   "WATER VAPOR MIXING RATIO AT THE SURFACE"      "kg kg-1"
state    real  QCG              ij      misc        1         -      r        "QCG"                   "CLOUD WATER MIXING RATIO AT THE SURFACE"      "kg kg-1"
state    real  SOILT1           ij      misc        1         -      r        "SOILT1"                "TEMPERATURE INSIDE SNOW "    "K"
state    real  TSNAV            ij      misc        1         -      r        "TSNAV"                 "AVERAGE SNOW TEMPERATURE "                    "C"
i1       real  REGIME           ij      misc        1         -                                          
state    real  SNOWC            ij      misc        1         -     irhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)      "SNOWC"                 "FLAG INDICATING SNOW COVERAGE (1 FOR SNOW COVER)"         ""
state    real  MAVAIL           ij      misc        1         -      r        "MAVAIL"                "SURFACE MOISTURE AVAILABILITY"                ""
                                                
state   real   tkesfcf          ij      misc        1         -      r        "tkesfcf"               "TKE AT THE SURFACE"                           "m2 s-2"      
                                                
state integer  STEPBL          -        misc        1         -      r        "STEPBL"                "NUMBER OF FUNDAMENTAL TIMESTEPS BETWEEN PBL CALLS" ""      
state    real  taucldi         ikj      misc        1         -      r        "TAUCLDI"               "CLOUD OPTICAL THICKNESS FOR ICE"              ""
state    real  taucldc         ikj      misc        1         -      r        "TAUCLDC"               "CLOUD OPTICAL THICKNESS FOR WATER"            ""
                                                
state   real  defor11          ikj      misc        1         -     -         "defor11"               "DEFORMATION 11"              "s-1"      
state   real  defor22          ikj      misc        1         -     -         "defor22"               "DEFORMATION 22"              "s-1"      
state   real  defor12          ikj      misc        1         -     -         "defor12"               "DEFORMATION 12"              "s-1"      
state   real  defor33          ikj      misc        1         z     -         "defor33"               "DEFORMATION 33"              "s-1"      
state   real  defor13          ikj      misc        1         z     -         "defor13"               "DEFORMATION 13"              "s-1"      
state   real  defor23          ikj      misc        1         z     -         "defor23"               "DEFORMATION 23"              "s-1"      
state   real   xkmv            ikj      misc        1         -     -         "xkmv"                  "VERTICAL EDDY VISCOSITY"     "m2 s-1"      
state   real   xkmh            ikj      misc        1         -     -         "xkmh"                  "HORIZONTAL EDDY VISCOSITY"   "m2 s-1"      
state   real   xkmhd           ikj      misc        1         -     -         "xkmhd"                 "VERTICAL EDDY DIFFUSIVITY OF HEAT (FOR UPPER DAMPING LAYER)"     "m2 s-1"      
state   real   xkhv            ikj      misc        1         -     -         "xkhv"                  "VERTICAL EDDY DIFFUSIVITY OF HEAT"                               "m2 s-1"      
state   real   xkhh            ikj      misc        1         -     -         "xkhh"                  "HORIZONTAL EDDY DIFFUSIVITY OF HEAT"                             "m2 s-1"      
state   real    div            ikj      misc        1         -     -         "div"                   "DIVERGENCE"                                                      "s-1"
state   real    BN2            ikj      misc        1         -     -         "BN2"                   "BRUNT-VAISALA FREQUENCY"                                         "s-2"
state  logical warm_rain        -       misc        1         -     -         "warm_rain"              "WARM_RAIN_LOGICAL"

# flag for nest movement
state  logical moved            -       misc        1         -     -          

#
#---------------------------------------------------------------------------------------------------------------------------------------
#                                               

######                                          
#                                               
# Variables that are set at run-time to control configuration  (namelist-settable)                                              
#                                               
#<Table>  <Type>  <Sym>                   <How set>          <Nentries>   <Default>                                             


# Time Control
rconfig   integer run_days                namelist,time_control		1             0       irh   "run_days"              "NUMBER OF DAYS TO RUN"
rconfig   integer run_hours               namelist,time_control		1             0       irh   "run_hours"             "NUMBER OF HOURS TO RUN"
rconfig   integer run_minutes             namelist,time_control		1             0       irh   "run_minutes"           "NUMBER OF MINUTES TO RUN"
rconfig   integer run_seconds             namelist,time_control		1             0       irh   "run_seconds"           "NUMBER OF SECONDS TO RUN"
rconfig   integer start_year              namelist,time_control 	max_domains    1993    irh   "start_year"            "4 DIGIT YEAR OF START OF MODEL" "YEARS"
rconfig   integer start_month             namelist,time_control		max_domains      03    irh   "start_month"           "2 DIGIT MONTH OF THE YEAR OF START OF MODEL, 1-12" "MONTHS"
rconfig   integer start_day               namelist,time_control		max_domains      13    irh   "start_day"             "2 DIGIT DAY OF THE MONTH OF START OF MODEL, 1-31" "DAYS"
rconfig   integer start_hour              namelist,time_control		max_domains      12    irh   "start_hour"            "2 DIGIT HOUR OF THE DAY OF START OF MODEL, 0-23" "HOURS"
rconfig   integer start_minute            namelist,time_control		max_domains      00    irh   "start_minute"          "2 DIGIT MINUTE OF THE HOUR OF START OF MODEL, 0-59" "MINUTES"
rconfig   integer start_second            namelist,time_control		max_domains      00    irh   "start_second"          "2 DIGIT SECOND OF THE MINUTE OF START OF MODEL, 0-59" "SECONDS"
rconfig   integer end_year                namelist,time_control		max_domains    1993    irh   "end_year"              "4 DIGIT YEAR OF END OF MODEL" "YEARS"
rconfig   integer end_month               namelist,time_control		max_domains      03    irh   "end_month"             "2 DIGIT MONTH OF THE YEAR OF END OF MODEL, 1-12" "MONTHS"
rconfig   integer end_day                 namelist,time_control		max_domains      14    irh   "end_day"               "2 DIGIT DAY OF THE MONTH OF END OF MODEL, 1-31" "DAYS"
rconfig   integer end_hour                namelist,time_control		max_domains      12    irh   "end_hour"              "2 DIGIT HOUR OF THE DAY OF END OF MODEL, 0-23" "HOURS"
rconfig   integer end_minute              namelist,time_control		max_domains      00    irh   "end_minute"            "2 DIGIT MINUTE OF THE HOUR OF END OF MODEL, 0-59" "MINUTES"
rconfig   integer end_second              namelist,time_control		max_domains      00    irh   "end_second"            "2 DIGIT SECOND OF THE MINUTE OF END OF MODEL, 0-59" "SECONDS"
rconfig   integer interval_seconds        namelist,time_control		1             43200    irh   "interval_seconds"      "SECONDS BETWEEN ANALYSIS AND BOUNDARY PERIODS" "SECONDS"
rconfig   logical input_from_file         namelist,time_control		max_domains    .false. irh    "input_from_file"      "T/F INPUT FOR THIS DOMAIN FROM A SEPARATE INPUT FILE"  ""
rconfig   logical input_from_hires        namelist,time_control		max_domains    .false. irh    "input_from_hires"      "T/F INPUT FOR THIS DOMAIN FROM USGS HI RES TERRAIN"  ""
rconfig   integer history_interval        namelist,time_control		max_domains    0       h     "history_interval"       ""      "MINUTES"
rconfig   integer frames_per_outfile      namelist,time_control		max_domains    10      h     "frames_per_outfile"            ""      ""
rconfig   integer frames_per_auxhist1     namelist,time_control         max_domains    10      h     "frames_per_auxhist1"            ""      ""
rconfig   integer frames_per_auxhist2     namelist,time_control         max_domains    1      h     "frames_per_auxhist2"            ""      ""
rconfig   integer frames_per_auxhist3     namelist,time_control         max_domains    1      h     "frames_per_auxhist3"            ""      ""
rconfig   integer frames_per_auxhist4     namelist,time_control         max_domains    10      h     "frames_per_auxhist4"            ""      ""
rconfig   integer frames_per_auxhist5     namelist,time_control         max_domains    10      h     "frames_per_auxhist5"            ""      ""
rconfig   logical restart                 namelist,time_control		1             .false. h    "restart"              ""      ""
rconfig   integer restart_interval        namelist,time_control		1              0       h     "restart_interval"       ""      "MINUTES"
rconfig   integer     io_form_input       namelist,time_control		1              2       h        "io_form_input"                 ""      ""
rconfig   integer     io_form_history     namelist,time_control		1              2       h        "io_form_history"               ""      ""
rconfig   integer     io_form_restart     namelist,time_control		1              2       h        "io_form_restart"               ""      ""
rconfig   integer     io_form_boundary    namelist,time_control		1              2       h        "io_form_boundary"               ""      ""
rconfig   integer debug_level             namelist,time_control		1             0       -      "debug_level"           ""      ""


rconfig   character  history_outname   namelist,time_control		1  "wrfout_d<domain>_<date>"     -     "name of history outfile"  ""      ""
rconfig   character  auxhist1_outname  namelist,time_control		1  "auxhist1_d<domain>_<date>"   -     "name of auxhist1 outfile" ""      ""
rconfig   character  auxhist2_outname  namelist,time_control		1  "auxhist2_d<domain>_<date>"   -     "name of auxhist2 outfile" ""      ""
rconfig   character  auxhist3_outname  namelist,time_control		1  "auxhist3_d<domain>_<date>"   -     "name of auxhist3 outfile" ""      ""
rconfig   character  auxhist4_outname  namelist,time_control		1  "auxhist4_d<domain>_<date>"   -     "name of auxhist4 outfile" ""      ""
rconfig   character  auxhist5_outname  namelist,time_control		1  "auxhist5_d<domain>_<date>"   -     "name of auxhist5 outfile" ""      ""

rconfig   character  history_inname    namelist,time_control		1  "wrfhist_d<domain>_<date>"    -     "name of history infile"  ""      ""
rconfig   character  auxhist1_inname   namelist,time_control		1  "auxhist1_d<domain>_<date>"   -     "name of auxhist1 infile" ""      ""
rconfig   character  auxhist2_inname   namelist,time_control		1  "auxhist2_d<domain>_<date>"   -     "name of auxhist2 infile" ""      ""
rconfig   character  auxhist3_inname   namelist,time_control		1  "auxhist3_d<domain>_<date>"   -     "name of auxhist3 infile" ""      ""
rconfig   character  auxhist4_inname   namelist,time_control		1  "auxhist4_d<domain>_<date>"   -     "name of auxhist4 infile" ""      ""
rconfig   character  auxhist5_inname   namelist,time_control		1  "auxhist5_d<domain>_<date>"   -     "name of auxhist5 infile" ""      ""

rconfig   character  auxinput1_outname  namelist,time_control            1  "auxinput1_d<domain>_<date>"   -     "name of auxinput1 outfile" ""      ""
rconfig   character  auxinput2_outname  namelist,time_control            1  "auxinput2_d<domain>_<date>"   -     "name of auxinput2 outfile" ""      ""
rconfig   character  auxinput3_outname  namelist,time_control            1  "auxinput3_d<domain>_<date>"   -     "name of auxinput3 outfile" ""      ""
rconfig   character  auxinput4_outname  namelist,time_control            1  "auxinput4_d<domain>_<date>"   -     "name of auxinput4 outfile" ""      ""
rconfig   character  auxinput5_outname  namelist,time_control            1  "auxinput5_d<domain>_<date>"   -     "name of auxinput5 outfile" ""      ""

rconfig   character  auxinput1_inname   namelist,time_control            1  "auxinput1_d<domain>"   -     "name of auxinput1 infile" ""      ""
rconfig   character  auxinput2_inname   namelist,time_control            1  "auxinput2_d<domain>"   -     "name of auxinput2 infile" ""      ""
rconfig   character  auxinput3_inname   namelist,time_control            1  "auxinput3_d<domain>"   -     "name of auxinput3 infile" ""      ""
rconfig   character  auxinput4_inname   namelist,time_control            1  "auxinput4_d<domain>"   -     "name of auxinput4 infile" ""      ""
rconfig   character  auxinput5_inname   namelist,time_control            1  "auxinput5_d<domain>"   -     "name of auxinput5 infile" ""      ""

rconfig   integer history_interval_mo     namelist,time_control		max_domains    0       h     "history_interval_mo"       ""      "MONTHS"
rconfig   integer history_interval_d      namelist,time_control		max_domains    0       h     "history_interval_d"       ""      "DAYS"
rconfig   integer history_interval_h      namelist,time_control		max_domains    0       h     "history_interval_h"       ""      "HOURS"
rconfig   integer history_interval_m      namelist,time_control		max_domains    0       h     "history_interval_m"       ""      "MINUTES"
rconfig   integer history_interval_s      namelist,time_control		max_domains    0       h     "history_interval_s"       ""      "SECONDS"

rconfig   integer inputout_interval_mo      namelist,time_control	max_domains    0       h     "inputout_interval_mo"       ""      "MONTHS"
rconfig   integer inputout_interval_d      namelist,time_control	max_domains    0       h     "inputout_interval_d"       ""      "DAYS"
rconfig   integer inputout_interval_h      namelist,time_control	max_domains    0       h     "inputout_interval_h"       ""      "HOURS"
rconfig   integer inputout_interval_m      namelist,time_control	max_domains    0       h     "inputout_interval_m"       ""      "MINUTES"
rconfig   integer inputout_interval_s      namelist,time_control	max_domains    0       h     "inputout_interval_s"       ""      "SECONDS"
rconfig   integer inputout_interval        namelist,time_control	max_domains    0       h     "inputout_interval"       ""      "MINUTES"

rconfig   integer auxhist1_interval_mo      namelist,time_control	max_domains    0       h     "auxhist1_interval_mo"       ""      "MONTHS"
rconfig   integer auxhist1_interval_d      namelist,time_control	max_domains    0       h     "auxhist1_interval_d"       ""      "DAYS"
rconfig   integer auxhist1_interval_h      namelist,time_control	max_domains    0       h     "auxhist1_interval_h"       ""      "HOURS"
rconfig   integer auxhist1_interval_m      namelist,time_control	max_domains    0       h     "auxhist1_interval_m"       ""      "MINUTES"
rconfig   integer auxhist1_interval_s      namelist,time_control	max_domains    0       h     "auxhist1_interval_s"       ""      "SECONDS"
rconfig   integer auxhist1_interval        namelist,time_control	max_domains    0       h     "auxhist1_interval"       ""      "MINUTES"

rconfig   integer auxhist2_interval_mo      namelist,time_control	max_domains    0       h     "auxhist2_interval_mo"       ""      "MONTHS"
rconfig   integer auxhist2_interval_d      namelist,time_control	max_domains    0       h     "auxhist2_interval_d"       ""      "DAYS"
rconfig   integer auxhist2_interval_h      namelist,time_control	max_domains    0       h     "auxhist2_interval_h"       ""      "HOURS"
rconfig   integer auxhist2_interval_m      namelist,time_control	max_domains    0       h     "auxhist2_interval_m"       ""      "MINUTES"
rconfig   integer auxhist2_interval_s      namelist,time_control	max_domains    0       h     "auxhist2_interval_s"       ""      "SECONDS"
rconfig   integer auxhist2_interval        namelist,time_control	max_domains    0       h     "auxhist2_interval"       ""      "MINUTES"

rconfig   integer auxhist3_interval_mo      namelist,time_control	max_domains    0       h     "auxhist3_interval_mo"       ""      "MONTHS"
rconfig   integer auxhist3_interval_d      namelist,time_control	max_domains    0       h     "auxhist3_interval_d"       ""      "DAYS"
rconfig   integer auxhist3_interval_h      namelist,time_control	max_domains    0       h     "auxhist3_interval_h"       ""      "HOURS"
rconfig   integer auxhist3_interval_m      namelist,time_control	max_domains    0       h     "auxhist3_interval_m"       ""      "MINUTES"
rconfig   integer auxhist3_interval_s      namelist,time_control	max_domains    0       h     "auxhist3_interval_s"       ""      "SECONDS"
rconfig   integer auxhist3_interval        namelist,time_control	max_domains    0       h     "auxhist3_interval"       ""      "MINUTES"

rconfig   integer auxhist4_interval_mo     namelist,time_control	max_domains    0       h     "auxhist4_interval_mo"       ""      "MONTHS"
rconfig   integer auxhist4_interval_d      namelist,time_control	max_domains    0       h     "auxhist4_interval_d"       ""      "DAYS"
rconfig   integer auxhist4_interval_h      namelist,time_control	max_domains    0       h     "auxhist4_interval_h"       ""      "HOURS"
rconfig   integer auxhist4_interval_m      namelist,time_control	max_domains    0       h     "auxhist4_interval_m"       ""      "MINUTES"
rconfig   integer auxhist4_interval_s      namelist,time_control	max_domains    0       h     "auxhist4_interval_s"       ""      "SECONDS"
rconfig   integer auxhist4_interval        namelist,time_control	max_domains    0       h     "auxhist4_interval"       ""      "MINUTES"

rconfig   integer auxhist5_interval_mo     namelist,time_control	max_domains    0       h     "auxhist5_interval_mo"       ""      "MONTHS"
rconfig   integer auxhist5_interval_d      namelist,time_control	max_domains    0       h     "auxhist5_interval_d"       ""      "DAYS"
rconfig   integer auxhist5_interval_h      namelist,time_control	max_domains    0       h     "auxhist5_interval_h"       ""      "HOURS"
rconfig   integer auxhist5_interval_m      namelist,time_control	max_domains    0       h     "auxhist5_interval_m"       ""      "MINUTES"
rconfig   integer auxhist5_interval_s      namelist,time_control	max_domains    0       h     "auxhist5_interval_s"       ""      "SECONDS"
rconfig   integer auxhist5_interval        namelist,time_control	max_domains    0       h     "auxhist5_interval"       ""      "MINUTES"

rconfig   integer auxinput1_interval_mo     namelist,time_control	max_domains    0       h     "auxinput1_interval_mo"       ""      "MONTHS"
rconfig   integer auxinput1_interval_d      namelist,time_control	max_domains    0       h     "auxinput1_interval_d"       ""      "DAYS"
rconfig   integer auxinput1_interval_h      namelist,time_control	max_domains    0       h     "auxinput1_interval_h"       ""      "HOURS"
rconfig   integer auxinput1_interval_m      namelist,time_control	max_domains    0       h     "auxinput1_interval_m"       ""      "MINUTES"
rconfig   integer auxinput1_interval_s      namelist,time_control	max_domains    0       h     "auxinput1_interval_s"       ""      "SECONDS"
rconfig   integer auxinput1_interval        namelist,time_control	max_domains    0       h     "auxinput1_interval"       ""      "MINUTES"

rconfig   integer auxinput2_interval_mo     namelist,time_control	max_domains    0       h     "auxinput2_interval_mo"       ""      "MONTHS"
rconfig   integer auxinput2_interval_d      namelist,time_control	max_domains    0       h     "auxinput2_interval_d"       ""      "DAYS"
rconfig   integer auxinput2_interval_h      namelist,time_control	max_domains    0       h     "auxinput2_interval_h"       ""      "HOURS"
rconfig   integer auxinput2_interval_m      namelist,time_control	max_domains    0       h     "auxinput2_interval_m"       ""      "MINUTES"
rconfig   integer auxinput2_interval_s      namelist,time_control	max_domains    0       h     "auxinput2_interval_s"       ""      "SECONDS"
rconfig   integer auxinput2_interval        namelist,time_control	max_domains    0       h     "auxinput2_interval"       ""      "MINUTES"

rconfig   integer auxinput3_interval_mo     namelist,time_control	max_domains    0       h     "auxinput3_interval_mo"       ""      "MONTHS"
rconfig   integer auxinput3_interval_d      namelist,time_control	max_domains    0       h     "auxinput3_interval_d"       ""      "DAYS"
rconfig   integer auxinput3_interval_h      namelist,time_control	max_domains    0       h     "auxinput3_interval_h"       ""      "HOURS"
rconfig   integer auxinput3_interval_m      namelist,time_control	max_domains    0       h     "auxinput3_interval_m"       ""      "MINUTES"
rconfig   integer auxinput3_interval_s      namelist,time_control	max_domains    0       h     "auxinput3_interval_s"       ""      "SECONDS"
rconfig   integer auxinput3_interval        namelist,time_control	max_domains    0       h     "auxinput3_interval"       ""      "MINUTES"

rconfig   integer auxinput4_interval_mo     namelist,time_control	max_domains    0       h     "auxinput4_interval_mo"       ""      "MONTHS"
rconfig   integer auxinput4_interval_d      namelist,time_control	max_domains    0       h     "auxinput4_interval_d"       ""      "DAYS"
rconfig   integer auxinput4_interval_h      namelist,time_control	max_domains    0       h     "auxinput4_interval_h"       ""      "HOURS"
rconfig   integer auxinput4_interval_m      namelist,time_control	max_domains    0       h     "auxinput4_interval_m"       ""      "MINUTES"
rconfig   integer auxinput4_interval_s      namelist,time_control	max_domains    0       h     "auxinput4_interval_s"       ""      "SECONDS"
rconfig   integer auxinput4_interval        namelist,time_control	max_domains    0       h     "auxinput4_interval"       ""      "MINUTES"

rconfig   integer auxinput5_interval_mo     namelist,time_control	max_domains    0       h     "auxinput5_interval_mo"       ""      "MONTHS"
rconfig   integer auxinput5_interval_d      namelist,time_control	max_domains    0       h     "auxinput5_interval_d"       ""      "DAYS"
rconfig   integer auxinput5_interval_h      namelist,time_control	max_domains    0       h     "auxinput5_interval_h"       ""      "HOURS"
rconfig   integer auxinput5_interval_m      namelist,time_control	max_domains    0       h     "auxinput5_interval_m"       ""      "MINUTES"
rconfig   integer auxinput5_interval_s      namelist,time_control	max_domains    0       h     "auxinput5_interval_s"       ""      "SECONDS"
rconfig   integer auxinput5_interval        namelist,time_control	max_domains    0       h     "auxinput5_interval"       ""      "MINUTES"

rconfig   integer restart_interval_mo     namelist,time_control		1              0       h     "restart_interval_mo"       ""      "MONTHS"
rconfig   integer restart_interval_d      namelist,time_control		1              0       h     "restart_interval_d"       ""      "DAYS"
rconfig   integer restart_interval_h      namelist,time_control		1              0       h     "restart_interval_h"       ""      "HOURS"
rconfig   integer restart_interval_m      namelist,time_control		1              0       h     "restart_interval_m"       ""      "MINUTES"
rconfig   integer restart_interval_s      namelist,time_control		1              0       h     "restart_interval_s"       ""      "SECONDS"


rconfig  integer history_begin_y  namelist,time_control		max_domains    0     h   "history_begin_y"    ""   "YEARS from start of run"
rconfig  integer history_begin_mo  namelist,time_control	max_domains    0     h   "history_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer history_begin_d   namelist,time_control	max_domains    0     h   "history_begin_d"    ""   "DAYS from start of run"
rconfig  integer history_begin_h   namelist,time_control	max_domains    0     h   "history_begin_h"    ""   "HOURS from start of run"
rconfig  integer history_begin_m   namelist,time_control	max_domains    0     h   "history_begin_m"    ""   "MINUTES from start of run"
rconfig  integer history_begin_s   namelist,time_control	max_domains    0     h   "history_begin_s"    ""   "SECONDS from start of run"

rconfig  integer inputout_begin_y   namelist,time_control	max_domains    0     h   "inputout_begin_y"    ""   "YEARS from start of run"
rconfig  integer inputout_begin_mo   namelist,time_control	max_domains    0     h   "inputout_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer inputout_begin_d   namelist,time_control	max_domains    0     h   "inputout_begin_d"    ""   "DAYS from start of run"
rconfig  integer inputout_begin_h   namelist,time_control	max_domains    0     h   "inputout_begin_h"    ""   "HOURS from start of run"
rconfig  integer inputout_begin_m   namelist,time_control	max_domains    0     h   "inputout_begin_m"    ""   "MINUTES from start of run"
rconfig  integer inputout_begin_s   namelist,time_control	max_domains    0     h   "inputout_begin_s"    ""   "SECONDS from start of run"

rconfig  integer auxhist1_begin_y   namelist,time_control	max_domains    0     h   "auxhist1_begin_y"    ""   "YEARS from start of run"
rconfig  integer auxhist1_begin_mo   namelist,time_control	max_domains    0     h   "auxhist1_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer auxhist1_begin_d   namelist,time_control	max_domains    0     h   "auxhist1_begin_d"    ""   "DAYS from start of run"
rconfig  integer auxhist1_begin_h   namelist,time_control	max_domains    0     h   "auxhist1_begin_h"    ""   "HOURS from start of run"
rconfig  integer auxhist1_begin_m   namelist,time_control	max_domains    0     h   "auxhist1_begin_m"    ""   "MINUTES from start of run"
rconfig  integer auxhist1_begin_s   namelist,time_control	max_domains    0     h   "auxhist1_begin_s"    ""   "SECONDS from start of run"

rconfig  integer auxhist2_begin_y   namelist,time_control	max_domains    0     h   "auxhist2_begin_y"    ""   "YEARS from start of run"
rconfig  integer auxhist2_begin_mo   namelist,time_control	max_domains    0     h   "auxhist2_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer auxhist2_begin_d   namelist,time_control	max_domains    0     h   "auxhist2_begin_d"    ""   "DAYS from start of run"
rconfig  integer auxhist2_begin_h   namelist,time_control	max_domains    0     h   "auxhist2_begin_h"    ""   "HOURS from start of run"
rconfig  integer auxhist2_begin_m   namelist,time_control	max_domains    0     h   "auxhist2_begin_m"    ""   "MINUTES from start of run"
rconfig  integer auxhist2_begin_s   namelist,time_control	max_domains    0     h   "auxhist2_begin_s"    ""   "SECONDS from start of run"

rconfig  integer auxhist3_begin_y   namelist,time_control	max_domains    0     h   "auxhist3_begin_y"    ""   "YEARS from start of run"
rconfig  integer auxhist3_begin_mo   namelist,time_control	max_domains    0     h   "auxhist3_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer auxhist3_begin_d   namelist,time_control	max_domains    0     h   "auxhist3_begin_d"    ""   "DAYS from start of run"
rconfig  integer auxhist3_begin_h   namelist,time_control	max_domains    0     h   "auxhist3_begin_h"    ""   "HOURS from start of run"
rconfig  integer auxhist3_begin_m   namelist,time_control	max_domains    0     h   "auxhist3_begin_m"    ""   "MINUTES from start of run"
rconfig  integer auxhist3_begin_s   namelist,time_control	max_domains    0     h   "auxhist3_begin_s"    ""   "SECONDS from start of run"

rconfig  integer auxhist4_begin_y  namelist,time_control	max_domains    0     h   "auxhist4_begin_y"    ""   "YEARS from start of run"
rconfig  integer auxhist4_begin_mo  namelist,time_control	max_domains    0     h   "auxhist4_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer auxhist4_begin_d   namelist,time_control	max_domains    0     h   "auxhist4_begin_d"    ""   "DAYS from start of run"
rconfig  integer auxhist4_begin_h   namelist,time_control	max_domains    0     h   "auxhist4_begin_h"    ""   "HOURS from start of run"
rconfig  integer auxhist4_begin_m   namelist,time_control	max_domains    0     h   "auxhist4_begin_m"    ""   "MINUTES from start of run"
rconfig  integer auxhist4_begin_s   namelist,time_control	max_domains    0     h   "auxhist4_begin_s"    ""   "SECONDS from start of run"

rconfig  integer auxhist5_begin_y  namelist,time_control	max_domains    0     h   "auxhist5_begin_y"    ""   "YEARS from start of run"
rconfig  integer auxhist5_begin_mo  namelist,time_control	max_domains    0     h   "auxhist5_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer auxhist5_begin_d   namelist,time_control	max_domains    0     h   "auxhist5_begin_d"    ""   "DAYS from start of run"
rconfig  integer auxhist5_begin_h   namelist,time_control	max_domains    0     h   "auxhist5_begin_h"    ""   "HOURS from start of run"
rconfig  integer auxhist5_begin_m   namelist,time_control	max_domains    0     h   "auxhist5_begin_m"    ""   "MINUTES from start of run"
rconfig  integer auxhist5_begin_s   namelist,time_control	max_domains    0     h   "auxhist5_begin_s"    ""   "SECONDS from start of run"

rconfig  integer auxinput1_begin_y  namelist,time_control	max_domains    0     h   "auxinput1_begin_y"    ""   "YEARS from start of run"
rconfig  integer auxinput1_begin_mo  namelist,time_control	max_domains    0     h   "auxinput1_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer auxinput1_begin_d   namelist,time_control	max_domains    0     h   "auxinput1_begin_d"    ""   "DAYS from start of run"
rconfig  integer auxinput1_begin_h   namelist,time_control	max_domains    0     h   "auxinput1_begin_h"    ""   "HOURS from start of run"
rconfig  integer auxinput1_begin_m   namelist,time_control	max_domains    0     h   "auxinput1_begin_m"    ""   "MINUTES from start of run"
rconfig  integer auxinput1_begin_s   namelist,time_control	max_domains    0     h   "auxinput1_begin_s"    ""   "SECONDS from start of run"

rconfig  integer auxinput2_begin_y  namelist,time_control	max_domains    0     h   "auxinput2_begin_y"    ""   "YEARS from start of run"
rconfig  integer auxinput2_begin_mo  namelist,time_control	max_domains    0     h   "auxinput2_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer auxinput2_begin_d   namelist,time_control	max_domains    0     h   "auxinput2_begin_d"    ""   "DAYS from start of run"
rconfig  integer auxinput2_begin_h   namelist,time_control	max_domains    0     h   "auxinput2_begin_h"    ""   "HOURS from start of run"
rconfig  integer auxinput2_begin_m   namelist,time_control	max_domains    0     h   "auxinput2_begin_m"    ""   "MINUTES from start of run"
rconfig  integer auxinput2_begin_s   namelist,time_control	max_domains    0     h   "auxinput2_begin_s"    ""   "SECONDS from start of run"

rconfig  integer auxinput3_begin_y  namelist,time_control	max_domains    0     h   "auxinput3_begin_y"    ""   "YEARS from start of run"
rconfig  integer auxinput3_begin_mo  namelist,time_control	max_domains    0     h   "auxinput3_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer auxinput3_begin_d   namelist,time_control	max_domains    0     h   "auxinput3_begin_d"    ""   "DAYS from start of run"
rconfig  integer auxinput3_begin_h   namelist,time_control	max_domains    0     h   "auxinput3_begin_h"    ""   "HOURS from start of run"
rconfig  integer auxinput3_begin_m   namelist,time_control	max_domains    0     h   "auxinput3_begin_m"    ""   "MINUTES from start of run"
rconfig  integer auxinput3_begin_s   namelist,time_control	max_domains    0     h   "auxinput3_begin_s"    ""   "SECONDS from start of run"

rconfig  integer auxinput4_begin_y  namelist,time_control	max_domains    0     h   "auxinput4_begin_y"    ""   "YEARS from start of run"
rconfig  integer auxinput4_begin_mo  namelist,time_control	max_domains    0     h   "auxinput4_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer auxinput4_begin_d   namelist,time_control	max_domains    0     h   "auxinput4_begin_d"    ""   "DAYS from start of run"
rconfig  integer auxinput4_begin_h   namelist,time_control	max_domains    0     h   "auxinput4_begin_h"    ""   "HOURS from start of run"
rconfig  integer auxinput4_begin_m   namelist,time_control	max_domains    0     h   "auxinput4_begin_m"    ""   "MINUTES from start of run"
rconfig  integer auxinput4_begin_s   namelist,time_control	max_domains    0     h   "auxinput4_begin_s"    ""   "SECONDS from start of run"

rconfig  integer auxinput5_begin_y  namelist,time_control	max_domains    0     h   "auxinput5_begin_y"    ""   "YEARS from start of run"
rconfig  integer auxinput5_begin_mo  namelist,time_control	max_domains    0     h   "auxinput5_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer auxinput5_begin_d   namelist,time_control	max_domains    0     h   "auxinput5_begin_d"    ""   "DAYS from start of run"
rconfig  integer auxinput5_begin_h   namelist,time_control	max_domains    0     h   "auxinput5_begin_h"    ""   "HOURS from start of run"
rconfig  integer auxinput5_begin_m   namelist,time_control	max_domains    0     h   "auxinput5_begin_m"    ""   "MINUTES from start of run"
rconfig  integer auxinput5_begin_s   namelist,time_control	max_domains    0     h   "auxinput5_begin_s"    ""   "SECONDS from start of run"

rconfig  integer restart_begin_y  namelist,time_control		1              0     h   "restart_begin_y"    ""   "YEARS from start of run"
rconfig  integer restart_begin_mo  namelist,time_control	1              0     h   "restart_begin_mo"    ""   "MONTHS from start of run"
rconfig  integer restart_begin_d   namelist,time_control	1              0     h   "restart_begin_d"    ""   "DAYS from start of run"
rconfig  integer restart_begin_h   namelist,time_control	1              0     h   "restart_begin_h"    ""   "HOURS from start of run"
rconfig  integer restart_begin_m   namelist,time_control	1              0     h   "restart_begin_m"    ""   "MINUTES from start of run"
rconfig  integer restart_begin_s   namelist,time_control	1              0     h   "restart_begin_s"    ""   "SECONDS from start of run"

rconfig integer history_end_y      namelist,time_control	max_domains    0    h  "history_end_y"       ""      "YEARS from start of run"
rconfig integer history_end_mo     namelist,time_control	max_domains    0    h  "history_end_mo"       ""      "MONTHS from start of run"
rconfig integer history_end_d      namelist,time_control	max_domains    0    h  "history_end_d"       ""      "DAYS from start of run"
rconfig integer history_end_h      namelist,time_control	max_domains    0    h  "history_end_h"       ""      "HOURS from start of run"
rconfig integer history_end_m      namelist,time_control	max_domains    0    h  "history_end_m"       ""      "MINUTES from start of run"
rconfig integer history_end_s      namelist,time_control	max_domains    0    h  "history_end_s"       ""      "SECONDS from start of run"

rconfig integer inputout_end_y      namelist,time_control     max_domains    0    h  "inputout_end_y"       ""      "YEARS from start of run"
rconfig integer inputout_end_mo      namelist,time_control     max_domains    0    h  "inputout_end_mo"       ""      "MONTHS from start of run"
rconfig integer inputout_end_d      namelist,time_control     max_domains    0    h  "inputout_end_d"       ""      "DAYS from start of run"
rconfig integer inputout_end_h      namelist,time_control     max_domains    0    h  "inputout_end_h"       ""      "HOURS from start of run"
rconfig integer inputout_end_m      namelist,time_control     max_domains    0    h  "inputout_end_m"       ""      "MINUTES from start of run"
rconfig integer inputout_end_s      namelist,time_control     max_domains    0    h  "inputout_end_s"       ""      "SECONDS from start of run"

rconfig integer auxhist1_end_y      namelist,time_control     max_domains    0    h  "auxhist1_end_y"       ""      "YEARS from start of run"
rconfig integer auxhist1_end_mo      namelist,time_control     max_domains    0    h  "auxhist1_end_mo"       ""      "MONTHS from start of run"
rconfig integer auxhist1_end_d      namelist,time_control     max_domains    0    h  "auxhist1_end_d"       ""      "DAYS from start of run"
rconfig integer auxhist1_end_h      namelist,time_control     max_domains    0    h  "auxhist1_end_h"       ""      "HOURS from start of run"
rconfig integer auxhist1_end_m      namelist,time_control     max_domains    0    h  "auxhist1_end_m"       ""      "MINUTES from start of run"
rconfig integer auxhist1_end_s      namelist,time_control     max_domains    0    h  "auxhist1_end_s"       ""      "SECONDS from start of run"

rconfig integer auxhist2_end_y      namelist,time_control     max_domains    0    h  "auxhist2_end_y"       ""      "YEARS from start of run"
rconfig integer auxhist2_end_mo      namelist,time_control     max_domains    0    h  "auxhist2_end_mo"       ""      "MONTHS from start of run"
rconfig integer auxhist2_end_d      namelist,time_control     max_domains    0    h  "auxhist2_end_d"       ""      "DAYS from start of run"
rconfig integer auxhist2_end_h      namelist,time_control     max_domains    0    h  "auxhist2_end_h"       ""      "HOURS from start of run"
rconfig integer auxhist2_end_m      namelist,time_control     max_domains    0    h  "auxhist2_end_m"       ""      "MINUTES from start of run"
rconfig integer auxhist2_end_s      namelist,time_control     max_domains    0    h  "auxhist2_end_s"       ""      "SECONDS from start of run"

rconfig integer auxhist3_end_y      namelist,time_control     max_domains    0    h  "auxhist3_end_y"       ""      "YEARS from start of run"
rconfig integer auxhist3_end_mo      namelist,time_control     max_domains    0    h  "auxhist3_end_mo"       ""      "MONTHS from start of run"
rconfig integer auxhist3_end_d      namelist,time_control     max_domains    0    h  "auxhist3_end_d"       ""      "DAYS from start of run"
rconfig integer auxhist3_end_h      namelist,time_control     max_domains    0    h  "auxhist3_end_h"       ""      "HOURS from start of run"
rconfig integer auxhist3_end_m      namelist,time_control     max_domains    0    h  "auxhist3_end_m"       ""      "MINUTES from start of run"
rconfig integer auxhist3_end_s      namelist,time_control     max_domains    0    h  "auxhist3_end_s"       ""      "SECONDS from start of run"

rconfig integer auxhist4_end_y     namelist,time_control     max_domains    0    h  "auxhist4_end_y"       ""      "YEARS from start of run"
rconfig integer auxhist4_end_mo     namelist,time_control     max_domains    0    h  "auxhist4_end_mo"       ""      "MONTHS from start of run"
rconfig integer auxhist4_end_d      namelist,time_control     max_domains    0    h  "auxhist4_end_d"       ""      "DAYS from start of run"
rconfig integer auxhist4_end_h      namelist,time_control     max_domains    0    h  "auxhist4_end_h"       ""      "HOURS from start of run"
rconfig integer auxhist4_end_m      namelist,time_control     max_domains    0    h  "auxhist4_end_m"       ""      "MINUTES from start of run"
rconfig integer auxhist4_end_s      namelist,time_control     max_domains    0    h  "auxhist4_end_s"       ""      "SECONDS from start of run"

rconfig integer auxhist5_end_y     namelist,time_control     max_domains    0    h  "auxhist5_end_y"       ""      "YEARS from start of run"
rconfig integer auxhist5_end_mo     namelist,time_control     max_domains    0    h  "auxhist5_end_mo"       ""      "MONTHS from start of run"
rconfig integer auxhist5_end_d      namelist,time_control     max_domains    0    h  "auxhist5_end_d"       ""      "DAYS from start of run"
rconfig integer auxhist5_end_h      namelist,time_control     max_domains    0    h  "auxhist5_end_h"       ""      "HOURS from start of run"
rconfig integer auxhist5_end_m      namelist,time_control     max_domains    0    h  "auxhist5_end_m"       ""      "MINUTES from start of run"
rconfig integer auxhist5_end_s      namelist,time_control     max_domains    0    h  "auxhist5_end_s"       ""      "SECONDS from start of run"

rconfig integer auxinput1_end_y     namelist,time_control     max_domains    0    h  "auxinput1_end_y"       ""      "YEARS from start of run"
rconfig integer auxinput1_end_mo     namelist,time_control     max_domains    0    h  "auxinput1_end_mo"       ""      "MONTHS from start of run"
rconfig integer auxinput1_end_d      namelist,time_control     max_domains    0    h  "auxinput1_end_d"       ""      "DAYS from start of run"
rconfig integer auxinput1_end_h      namelist,time_control     max_domains    0    h  "auxinput1_end_h"       ""      "HOURS from start of run"
rconfig integer auxinput1_end_m      namelist,time_control     max_domains    0    h  "auxinput1_end_m"       ""      "MINUTES from start of run"
rconfig integer auxinput1_end_s      namelist,time_control     max_domains    0    h  "auxinput1_end_s"       ""      "SECONDS from start of run"

rconfig integer auxinput2_end_y     namelist,time_control     max_domains    0    h  "auxinput2_end_y"       ""      "YEARS from start of run"
rconfig integer auxinput2_end_mo     namelist,time_control     max_domains    0    h  "auxinput2_end_mo"       ""      "MONTHS from start of run"
rconfig integer auxinput2_end_d      namelist,time_control     max_domains    0    h  "auxinput2_end_d"       ""      "DAYS from start of run"
rconfig integer auxinput2_end_h      namelist,time_control     max_domains    0    h  "auxinput2_end_h"       ""      "HOURS from start of run"
rconfig integer auxinput2_end_m      namelist,time_control     max_domains    0    h  "auxinput2_end_m"       ""      "MINUTES from start of run"
rconfig integer auxinput2_end_s      namelist,time_control     max_domains    0    h  "auxinput2_end_s"       ""      "SECONDS from start of run"

rconfig integer auxinput3_end_y     namelist,time_control     max_domains    0    h  "auxinput3_end_y"       ""      "YEARS from start of run"
rconfig integer auxinput3_end_mo     namelist,time_control     max_domains    0    h  "auxinput3_end_mo"       ""      "MONTHS from start of run"
rconfig integer auxinput3_end_d      namelist,time_control     max_domains    0    h  "auxinput3_end_d"       ""      "DAYS from start of run"
rconfig integer auxinput3_end_h      namelist,time_control     max_domains    0    h  "auxinput3_end_h"       ""      "HOURS from start of run"
rconfig integer auxinput3_end_m      namelist,time_control     max_domains    0    h  "auxinput3_end_m"       ""      "MINUTES from start of run"
rconfig integer auxinput3_end_s      namelist,time_control     max_domains    0    h  "auxinput3_end_s"       ""      "SECONDS from start of run"

rconfig integer auxinput4_end_y     namelist,time_control     max_domains    0    h  "auxinput4_end_y"       ""      "YEARS from start of run"
rconfig integer auxinput4_end_mo     namelist,time_control     max_domains    0    h  "auxinput4_end_mo"       ""      "MONTHS from start of run"
rconfig integer auxinput4_end_d      namelist,time_control     max_domains    0    h  "auxinput4_end_d"       ""      "DAYS from start of run"
rconfig integer auxinput4_end_h      namelist,time_control     max_domains    0    h  "auxinput4_end_h"       ""      "HOURS from start of run"
rconfig integer auxinput4_end_m      namelist,time_control     max_domains    0    h  "auxinput4_end_m"       ""      "MINUTES from start of run"
rconfig integer auxinput4_end_s      namelist,time_control     max_domains    0    h  "auxinput4_end_s"       ""      "SECONDS from start of run"

rconfig integer auxinput5_end_y     namelist,time_control     max_domains    0    h  "auxinput5_end_y"       ""      "YEARS from start of run"
rconfig integer auxinput5_end_mo     namelist,time_control     max_domains    0    h  "auxinput5_end_mo"       ""      "MONTHS from start of run"
rconfig integer auxinput5_end_d      namelist,time_control     max_domains    0    h  "auxinput5_end_d"       ""      "DAYS from start of run"
rconfig integer auxinput5_end_h      namelist,time_control     max_domains    0    h  "auxinput5_end_h"       ""      "HOURS from start of run"
rconfig integer auxinput5_end_m      namelist,time_control     max_domains    0    h  "auxinput5_end_m"       ""      "MINUTES from start of run"
rconfig integer auxinput5_end_s      namelist,time_control     max_domains    0    h  "auxinput5_end_s"       ""      "SECONDS from start of run"

rconfig   integer     io_form_auxinput1   namelist,time_control		1              2       h        "io_form_auxinput1"                 ""      ""
rconfig   integer     io_form_auxinput2   namelist,time_control		1              2       h        "io_form_auxinput2"                 ""      ""
rconfig   integer     io_form_auxinput3   namelist,time_control		1              2       h        "io_form_auxinput3"                 ""      ""
rconfig   integer     io_form_auxinput4   namelist,time_control		1              2       h        "io_form_auxinput4"                 ""      ""
rconfig   integer     io_form_auxinput5   namelist,time_control		1              2       h        "io_form_auxinput5"                 ""      ""
rconfig   integer     io_form_auxhist1    namelist,time_control		1              2       h        "io_form_auxhist1"               ""      ""
rconfig   integer     io_form_auxhist2    namelist,time_control		1              2       h        "io_form_auxhist2"               ""      ""
rconfig   integer     io_form_auxhist3    namelist,time_control		1              2       h        "io_form_auxhist3"               ""      ""
rconfig   integer     io_form_auxhist4    namelist,time_control		1              2       h        "io_form_auxhist4"               ""      ""
rconfig   integer     io_form_auxhist5    namelist,time_control		1              2       h        "io_form_auxhist5"               ""      ""

rconfig   integer JULYR                   namelist,time_control		max_domains    0       h    "JULYR"                 ""      ""
rconfig   integer JULDAY                  namelist,time_control		max_domains    1       h    "JULDAY"                ""      ""
rconfig   real    GMT                     namelist,time_control		max_domains    0.      h    "GMT"           ""      ""
rconfig   character  input_inname      namelist,time_control		1  "wrfinput_d<domain>"          -     "name of input   infile"   ""      ""
rconfig   character  input_outname     namelist,time_control		1  "wrfinput_d<domain>"          -     "name of input   outfile"  ""      ""
rconfig   character  bdy_inname        namelist,time_control		1  "wrfbdy_d<domain>"            -     "name of boundary infile"  ""      ""
rconfig   character  bdy_outname       namelist,time_control		1  "wrfbdy_d<domain>"            -     "name of boundary outfile" ""      ""
rconfig   character  rst_inname        namelist,time_control		1  "wrfrst_d<domain>_<date>"     -     "name of restrt infile"    ""      ""
rconfig   character  rst_outname       namelist,time_control		1  "wrfrst_d<domain>_<date>"     -     "name of restrt outfile"   ""      ""
rconfig   logical write_input             namelist,time_control		1             .false. -    "write input data for 3dvar etc."              ""      ""
rconfig   logical write_restart_at_0h     namelist,time_control		1             .false. h    "write_restart_at_0h"              ""      ""
# TBH:  Controls to trade off recomputation for memory in the adjoint.  
# TBH:  Put this in a new namelist later...  
rconfig   logical remove_runge_kutta_loor namelist,time_control	        1      .false. rh    "remove_runge_kutta_loor" ""    ""
rconfig   logical remove_small_step       namelist,time_control	        1      .false. rh    "remove_small_step"       ""    ""
rconfig   logical remove_rktend_thru_ss   namelist,time_control	        1      .false. rh    "remove_rktend_thru_ss"   ""    ""
rconfig   logical remove_rktend           namelist,time_control	        1      .false. rh    "remove_rktend"           ""    ""
rconfig   logical remove_rk_step_prep     namelist,time_control	        1      .false. rh    "remove_rk_step_prep"     ""    ""


# Domains
rconfig   integer time_step               namelist,domains	1             -       ih   "time_step"     
rconfig   integer time_step_fract_num     namelist,domains	1             0       ih   "time_step_fract_num"     
rconfig   integer time_step_fract_den     namelist,domains	1             1       ih   "time_step_fract_den"     
rconfig   integer max_dom                 namelist,domains	1             1       irh  "max_dom"               ""      ""
rconfig   integer s_we                    namelist,domains	max_domains    1       irh    "s_we"          ""      ""
rconfig   integer e_we                    namelist,domains	max_domains    32      irh    "e_we"          ""      ""
rconfig   integer s_sn                    namelist,domains	max_domains    1       irh    "s_sn"          ""      ""
rconfig   integer e_sn                    namelist,domains	max_domains    32      irh    "e_sn"          ""      ""
rconfig   integer s_vert                  namelist,domains	max_domains    1       irh    "s_vert"                ""      ""
rconfig   integer e_vert                  namelist,domains	max_domains    31      irh    "e_vert"                ""      ""
rconfig   real    dx                      namelist,domains     max_domains    200     h     "dx"        "X HORIZONTAL RESOLUTION"   "METERS"
rconfig   real    dy                      namelist,domains   	max_domains    200     h     "dy"        "Y HORIZONTAL RESOLUTION"   "METERS"
rconfig   integer grid_id                 namelist,domains	max_domains    1       irh    "id"            ""      ""
rconfig   integer parent_id               namelist,domains	max_domains    0       h     "parent_id"             ""      ""
rconfig   integer level                   namelist,domains	max_domains    1       irh    "level"         ""      ""
rconfig   integer i_parent_start          namelist,domains	max_domains    1       h     "i_parent_start"                ""      ""
rconfig   integer j_parent_start          namelist,domains	max_domains    1       h     "j_parent_start"                ""      ""
rconfig   integer parent_grid_ratio       namelist,domains	max_domains    1       h     "parent_grid_ratio"             ""      ""
rconfig   integer parent_time_step_ratio  namelist,domains	max_domains    1       h     "parent_time_step_ratio"                ""      ""
rconfig   integer feedback                namelist,domains	1    1       h     "feedback"          ""      ""
rconfig   integer smooth_option           namelist,domains	1    2       h     "smooth_option"          ""      ""
rconfig   real    ztop                    namelist,domains	max_domains    15000.  h    "ztop"          ""      ""
rconfig   integer moad_grid_ratio         namelist,domains	max_domains    1       h     "moad_grid_ratio"               ""      ""
rconfig   integer moad_time_step_ratio    namelist,domains	max_domains    1       h     "moad_time_step_ratio"          ""      ""
rconfig   integer shw                     namelist,domains	max_domains    2       h     "stencil_half_width"   "HORIZONTAL INTERPOLATION STENCIL HALF-WIDTH"  "GRID POINTS"
rconfig   integer coral_dist              namelist,domains      max_domains    8
rconfig   integer tile_sz_x               namelist,domains	1             0       -      "tile_sz_x"             ""      ""
rconfig   integer tile_sz_y               namelist,domains	1             0       -      "tile_sz_y"             ""      ""
rconfig   integer numtiles                namelist,domains	1             1       -      "numtiles"              ""      ""
rconfig   integer nproc_x                 namelist,domains	1             -1       -      "nproc_x"              "-1 means not set"      ""
rconfig   integer nproc_y		  namelist,domains	1             -1       -      "nproc_y"              "-1 means not set"      ""
rconfig   integer irand                   namelist,domains	1             0       -      "irand"           ""      ""
rconfig   real    dt                      derived              max_domains    2.      h     "dt"        "TEMPORAL RESOLUTION"      "SECONDS"
rconfig   integer   num_moves       namelist,domains    1                0
rconfig   integer   move_id         namelist,domains    max_moves        0
rconfig   integer   move_interval   namelist,domains    max_moves        999999999
rconfig   integer   move_cd_x       namelist,domains    max_moves        0
rconfig   integer   move_cd_y       namelist,domains    max_moves        0

# VAR_TEST_control, append to domains
rconfig   logical TEST_rk_step_prep       namelist,domains          1      .false. -     "TEST_rk_step_prep"           ""      ""
rconfig   logical TEST_init_zero_tendency namelist,domains          1      .false. -     "TEST_init_zero_tendency"     ""      ""
rconfig   logical TEST_phy_prep           namelist,domains          1      .false. -     "TEST_phy_prep"               ""      ""
rconfig   logical TEST_calculate_km_kh    namelist,domains          1      .false. -     "TEST_calculate_km_kh"        ""      ""
rconfig   logical TEST_rk_tendency        namelist,domains          1      .false. -     "TEST_rk_tendency"            ""      ""
rconfig   logical TEST_surface_drag       namelist,domains          1      .false. -     "TEST_surface_drag"           ""      ""
rconfig   logical TEST_lscond             namelist,domains          1      .false. -     "TEST_lscond"                 ""      ""
rconfig   logical TEST_moist_physics_prep_em  namelist,domains      1      .false. -     "TEST_moist_physics_prep_em
rconfig   logical TEST_advect_scalar      namelist,domains          1      .false. -     "TEST_advect_scalar"          ""      ""
rconfig   logical TEST_relax_bdy_dry      namelist,domains          1      .false. -     "TEST_relax_bdy_dry"          ""      ""
rconfig   logical TEST_rk_addtend_dry     namelist,domains          1      .false. -     "TEST_rk_addtend_dry"         ""      ""
rconfig   logical TEST_spec_bdy_dry       namelist,domains          1      .false. -     "TEST_spec_bdy_dry"           ""      ""
rconfig   logical TEST_small_step_prep    namelist,domains          1      .false. -     "TEST_small_step_prep"        ""      ""
rconfig   logical TEST_calc_p_rho         namelist,domains          1      .false. -     "TEST_calc_p_rho"             ""      ""
rconfig   logical TEST_calc_coef_w        namelist,domains          1      .false. -     "TEST_calc_coef_w"            ""      ""
rconfig   logical TEST_advance_uv         namelist,domains          1      .false. -     "TEST_advance_uv"             ""      ""
rconfig   logical TEST_spec_bdyupdate     namelist,domains          1      .false. -     "TEST_spec_bdyupdate"         ""      ""
rconfig   logical TEST_advance_mu_t       namelist,domains          1      .false. -     "TEST_advance_mu_t"           ""      ""
rconfig   logical TEST_sumflux            namelist,domains          1      .false. -     "TEST_sumflux"                ""      ""
rconfig   logical TEST_advance_w          namelist,domains          1      .false. -     "TEST_advance_w"              ""      ""
rconfig   logical TEST_spec_bdyupdate_ph  namelist,domains          1      .false. -     "TEST_spec_bdyupdate_ph"      ""      ""
rconfig   logical TEST_calc_mu_uv_1       namelist,domains          1      .false. -     "TEST_calc_mu_uv_1"           ""      ""
rconfig   logical TEST_small_step_finish  namelist,domains          1      .false. -     "TEST_small_step_finish"      ""      ""
rconfig   logical TEST_rk_scalar_tend     namelist,domains          1      .false. -     "TEST_rk_scalar_tend"         ""      ""
rconfig   logical TEST_spec_bdy_scalar    namelist,domains          1      .false. -     "TEST_spec_bdy_scalar"        ""      ""
rconfig   logical TEST_rk_update_scalar   namelist,domains          1      .false. -     "TEST_rk_update_scalar"       ""      ""
rconfig   logical TEST_calc_p_rho_phi     namelist,domains          1      .false. -     "TEST_calc_p_rho_phi"         ""      ""
rconfig   logical TEST_diagnose_w         namelist,domains          1      .false. -     "TEST_diagnose_w"             ""      ""

# Physics
rconfig   integer     mp_physics          namelist,physics	max_domains    0       rh       "mp_physics"            ""      ""
rconfig   integer     ra_lw_physics       namelist,physics	max_domains    0       rh       "ra_lw_physics"         ""      ""
rconfig   integer     ra_sw_physics       namelist,physics	max_domains    0       rh       "ra_sw_physics"         ""      ""
rconfig   real    radt                    namelist,physics	max_domains    0       h    "RADT"          ""      ""
rconfig   integer     sf_sfclay_physics   namelist,physics	max_domains    0       rh       "sf_sfclay_physics"             ""      ""
rconfig   integer     sf_surface_physics  namelist,physics	max_domains    0       rh       "sf_surface_physics"            ""      ""
rconfig   integer     bl_pbl_physics      namelist,physics	max_domains    0       rh       "bl_pbl_physics"                ""      ""
rconfig   real    BLDT                    namelist,physics	max_domains    0       h    "BLDT"          ""      ""
rconfig   integer     cu_physics          namelist,physics	max_domains    0       rh       "cu_physics"            ""      ""
rconfig   real    CUDT                    namelist,physics	max_domains    0       h    "CUDT"          ""      ""
rconfig   real    GSMDT                   namelist,physics	max_domains    0       h    "GSMDT"          ""      ""
rconfig   integer ISFFLX                  namelist,physics 	1             1       irh    "ISFFLX"                        ""      ""
rconfig   integer IFSNOW                  namelist,physics	1             0       irh    "IFSNOW"                        ""      ""
rconfig   integer ICLOUD                  namelist,physics	1             1       irh    "ICLOUD"                        ""      ""
rconfig   integer surface_input_source    namelist,physics	1             1       irh    "surface_input_source"          "1=static (fractional), 2=time dependent (dominant), 3=hybrid (not yet implemented)"      ""
rconfig   integer num_soil_layers         namelist,physics	1             5       irh    "num_soil_layers"               ""      ""
rconfig   integer maxiens                 namelist,physics	1             1       irh    "maxiens"                    ""      ""
rconfig   integer maxens                  namelist,physics	1             3       irh    "maxens"                    ""      ""
rconfig   integer maxens2                 namelist,physics	1             3       irh    "maxens2"                    ""      ""
rconfig   integer maxens3                 namelist,physics	1            16       irh    "maxens3"                    ""      ""
rconfig   integer ensdim                  namelist,physics	1            144      irh    "ensdim"                    ""      ""
rconfig   integer     chem_opt            namelist,physics	max_domains    0       rh       "chem_opt"              ""      ""
rconfig   integer num_land_cat            namelist,physics	1            24       -      "num_land_cat"                  ""      ""
rconfig   integer num_soil_cat            namelist,physics	1            16       -      "num_soil_cat"                  ""      ""
rconfig   integer mp_zero_out             namelist,physics	1             0       -      "mp_zero_out"  "microphysics fields set to zero  0=no action taken, 1=all fields but Qv, 2=all fields including Qv"      "flag"
rconfig   real mp_zero_out_thresh         namelist,physics	1          1.e-8      -      "mp_zero_out_thresh"  "minimum threshold for non-Qv moist fields, below are set to zero"  "kg/kg"
rconfig   real    seaice_threshold        namelist,physics	1            271       h    "seaice_threshold"  "tsk below which which water points are set to sea ice for slab scheme"   "K"


# Dynamics
# dynamics option (see package definitions, below)
rconfig   integer dyn_opt                 namelist,dynamics	1             1       irh   "dyn_opt"               ""      ""
rconfig   integer rk_ord                  namelist,dynamics	1             3       irh   "rk_order"               ""      ""
rconfig   integer w_damping               namelist,dynamics	1             0       irh    "w_damping"             ""      ""
# diff_opt 1=old diffusion, 2=new
rconfig   integer diff_opt                namelist,dynamics	1             1       irh    "diff_opt"              ""      ""
# km_opt   1=old coefs, 2=tke, 3=Smagorinksy
rconfig   integer km_opt                  namelist,dynamics	1             1       irh    "km_opt"                ""      ""
rconfig   integer damp_opt                namelist,dynamics	1             1       irh    "damp_opt"              ""      ""
rconfig   real    zdamp                   namelist,dynamics	max_domains    5000.   h    "zdamp"         ""      ""
rconfig   real    dampcoef                namelist,dynamics     max_domains    0.2     h    "dampcoef"              ""      ""
rconfig   real    khdif                   namelist,dynamics	max_domains    0       h    "khdif"         ""      ""
rconfig   real    kvdif                   namelist,dynamics	max_domains    0       h    "kvdif"         ""      ""
rconfig   real    smdiv                   namelist,dynamics	max_domains    0.      h    "smdiv"         ""      ""
rconfig   real    emdiv                   namelist,dynamics	max_domains    0.      h    "emdiv"         ""      ""
rconfig   real    epssm                   namelist,dynamics	max_domains    .1      h    "epssm"         ""      ""
rconfig   logical non_hydrostatic         namelist,dynamics	max_domains  .true.   irh  "non_hydrostatic"    ""   ""
rconfig   integer time_step_sound         namelist,dynamics	max_domains    0       h     "time_step_sound"               ""      ""
rconfig   integer     h_mom_adv_order     namelist,dynamics	max_domains    3       rh       "h_mom_adv_order"               ""      ""
rconfig   integer     v_mom_adv_order     namelist,dynamics	max_domains    3       rh       "v_mom_adv_order"               ""      ""
rconfig   integer     h_sca_adv_order     namelist,dynamics	max_domains    3       rh       "h_sca_adv_order"               ""      ""
rconfig   integer     v_sca_adv_order     namelist,dynamics	max_domains    3       rh       "v_sca_adv_order"               ""      ""
rconfig   logical top_radiation           namelist,dynamics	max_domains    .false. rh    "top_radiation"         ""      ""
rconfig   real    mix_cr_len              namelist,dynamics	max_domains    200.    h    "mix_cr_len"            ""      ""
rconfig   real    tke_upper_bound         namelist,dynamics	max_domains    1000.   h    "tke_upper_bound"            ""      ""
rconfig   real    kh_tke_upper_bound      namelist,dynamics	max_domains    1000.   h    "kh_tke_upper_bound"         ""      ""
rconfig   real    kv_tke_upper_bound      namelist,dynamics	max_domains    1000.   h    "kv_tke_upper_bound"         ""      ""
rconfig   real    tke_drag_coefficient      namelist,dynamics	max_domains    0.      h    "tke_drag_coefficient"       ""      ""
rconfig   real    tke_heat_flux           namelist,dynamics	max_domains    0.      h    "tke_heat_flux"              ""      ""
rconfig   logical pert_coriolis           namelist,dynamics	max_domains  .false.  irh  "pert_coriolis"    ""   ""
rconfig   logical mix_full_fields         namelist,dynamics     max_domains  .false.  irh  "mix_full_field"   ""   ""
rconfig   real    base_pres               namelist,dynamics	1          100000.     h    "base_pres"  "Base state pressure - do not change (10^5 Pa), real only"      "Pa"
rconfig   real    base_temp               namelist,dynamics	1             290.     h    "base_temp"  "Base state sea level temperature, real only"      "K"
rconfig   real    base_lapse              namelist,dynamics	1              50.     h    "base_lapse" "Base state temperature difference between base pres and 1/e of atm depth - do not change, real only"      "K"
rconfig   logical jcdfi_onoff             namelist,dynamics     1              .true.  rh   "jcdfi_onoff"   "JcDFI on/off"      ""
rconfig   real    jcdfi_tauc              namelist,dynamics     1              10800.  rh   "jcdfi_tauc"   "JcDFI cut-off"      ""
rconfig   real    jcdfi_gama              namelist,dynamics     1              1.      rh   "jcdfi_gama"    "Weighting for JcDFI"      ""
rconfig   real    jcdfi_error_wind        namelist,dynamics     1              3.      rh   "jcdfi_error_wind"    "Wind Error for JcDFI"      "m/s"
rconfig   real    jcdfi_error_t           namelist,dynamics     1              1.      rh   "jcdfi_error_t"    "Temp. Error for JcDFI"      "K"
rconfig   real    jcdfi_error_q           namelist,dynamics     1              0.001   rh   "jcdfi_error_q"    "Moist Error for JcDFI"      "kg/kg"
rconfig   real    jcdfi_error_mu          namelist,dynamics     1              1000.   rh   "jcdfi_error_mu"    "Pert. Pres. Error for JcDFI"      "Pa"


# Bdy_control
rconfig   integer spec_bdy_width          namelist,bdy_control		1             5       irh    "spec_bdy_width"                ""      ""
rconfig   integer spec_zone               namelist,bdy_control		1             1       irh    "spec_zone"                     ""      ""
rconfig   integer relax_zone              namelist,bdy_control		1             4       irh    "relax_zone"                    ""      ""
rconfig   logical specified               namelist,bdy_control	max_domains    .false. rh    "specified"             ""      ""
rconfig   logical periodic_x              namelist,bdy_control	max_domains    .false. rh    "periodic_x"            ""      ""
rconfig   logical symmetric_xs            namelist,bdy_control	max_domains    .false. rh    "symmetric_xs"          ""      ""
rconfig   logical symmetric_xe            namelist,bdy_control	max_domains    .false. rh    "symmetric_xe"          ""      ""
rconfig   logical open_xs                 namelist,bdy_control	max_domains    .false. rh    "open_xs"               ""      ""
rconfig   logical open_xe                 namelist,bdy_control	max_domains    .false. rh    "open_xe"               ""      ""
rconfig   logical periodic_y              namelist,bdy_control	max_domains    .false. rh    "periodic_y"            ""      ""
rconfig   logical symmetric_ys            namelist,bdy_control	max_domains    .false. rh    "symmetric_ys"          ""      ""
rconfig   logical symmetric_ye            namelist,bdy_control	max_domains    .false. rh    "symmetric_ye"          ""      ""
rconfig   logical open_ys                 namelist,bdy_control	max_domains    .false. rh    "open_ys"               ""      ""
rconfig   logical open_ye                 namelist,bdy_control	max_domains    .false. rh    "open_ye"               ""      ""
rconfig   logical nested                  namelist,bdy_control	max_domains    .false. rh    "nested"                ""      ""
rconfig   integer real_data_init_type     namelist,bdy_control		1                 1    irh   "real_data_init_type"   "REAL DATA INITIALIZATION OPTIONS: 1=SI, 2=MM5, 3=GENERIC" "PRE-PROCESSOR TYPES"

# NAMELIST DERIVED
rconfig   real    cen_lat                 derived                  max_domains    0       -        "cen_lat"              "center latitude"      "degrees, negative is south"
rconfig   real    cen_lon                 derived                  max_domains    0       -        "cen_lon"              "central longitude"      "degrees, negative is west"
rconfig   real    truelat1                derived                  max_domains    0       -        "true_lat1"             "first standard parallel"      "degrees, negative is south"
rconfig   real    truelat2                derived                  max_domains    0       -        "true_lat2"             "second standard parallel"      "degrees, negative is south"
rconfig   real    moad_cen_lat            derived                  max_domains    0       -        "moad_cen_lat"             "center latitude of the most coarse grid"      "degrees, negative is south"
rconfig   real    stand_lon               derived                  max_domains    0       -        "stand_lon"             "standard longitude, parallel to j-direction, perpendicular to i-direction "      "degrees, negative is west"
rconfig   real    bdyfrq                  derived                  max_domains    0       -        "bdyfrq"               "lateral boundary input frequency"      "seconds"
rconfig   integer iswater                 derived                  max_domains    0       -        "iswater"              "land use index of water"      "index category"
rconfig   integer isice                   derived                  max_domains    0       -        "isice"                "land use index of ice"        "index category"
rconfig   integer isurban                 derived                  max_domains    0       -        "isurban"              "land use index for 'urban and built-up"     "index category"
rconfig   integer isoilwater              derived                  max_domains    0       -        "isoilwater"           "land use index of water for soil"        "index category"
rconfig   integer map_proj                derived                  max_domains    0       -        "map_proj"             "domain map projection"      "0=none, 1=Lambert, 2=polar, 3=Mercator"

rconfig   integer   stdout                  namelist,wrfvar  1  6         - "stdout"                  ""  ""
rconfig   integer   stderr                  namelist,wrfvar  1  0         - "stderr"                  ""  ""
rconfig   integer   trace_unit              namelist,wrfvar  1  7         - "trace_unit"              ""  ""
rconfig   integer   trace_pe                namelist,wrfvar  1  0         - "trace_pe"                ""  ""
rconfig   integer   trace_repeat_head       namelist,wrfvar  1  10        - "trace_repeat_head"       ""  ""
rconfig   integer   trace_repeat_body       namelist,wrfvar  1  10        - "trace_repeat_body"       ""  ""
rconfig   integer   trace_max_depth         namelist,wrfvar  1  30        - "trace_max_depth"         ""  ""
rconfig   logical   trace_use               namelist,wrfvar  1  .true.    - "trace_use"               ""  ""
rconfig   logical   trace_use_frequent      namelist,wrfvar  1  .true.    - "trace_use_frequent"      ""  ""
rconfig   logical   trace_memory            namelist,wrfvar  1  .true.    - "trace_memory"            ""  ""
rconfig   logical   trace_all_pes           namelist,wrfvar  1  .false.   - "trace_all_pes"           ""  ""
rconfig   logical   trace_csv               namelist,wrfvar  1  .false.   - "trace_csv"               ""  ""
rconfig   logical   use_html                namelist,wrfvar  1  .true.    - "use_html"                ""  ""
rconfig   logical   warnings_are_fatal      namelist,wrfvar  1  .false.   - "warnings_are_fatal"      ""  ""
rconfig   character documentation_url       namelist,wrfvar  1  "unknown"                  -  "documentation_url"  ""  ""  

#
#---------------------------------------------------------------------------------------------------------------------------------------
# Package Declarations
# Single dummy declaration to define a nodyn dyn option
state integer nodyn_dummy - dyn_nodyn -  -  -  "" "" ""

#
#------------------------------------------------------------------------------------
# Package Declarations
#

#key      package       associated                package          associated 4d scalars
#         name          namelist choice           state vars
package   dyn_nodyn    dyn_opt==0                   -             -
package   dyn_em       dyn_opt==2                   -             -
package   dyn_em_sn    dyn_opt==102                 -             -
package   dyn_em_tl    dyn_opt==202                 -             -
package   dyn_em_ad    dyn_opt==302                 -             -
package   dyn_em_tst   dyn_opt==402                 -             -
package   dyn_em_var   dyn_opt==502                 -             -

#package   passivec1     chem_opt==0                  -             
package   passiveqv     mp_physics==0                -             moist:qv;a_moist:a_qv;g_moist:g_qv
package   kesslerscheme mp_physics==1                -             moist:qv,qc,qr;a_moist:a_qv,a_qc,a_qr;g_moist:g_qv,g_qc,g_qr
package   linscheme     mp_physics==2                -             moist:qv,qc,qr,qi,qs,qg;a_moist:a_qv,a_qc,a_qr,a_qi,a_qs,a_qg;g_moist:g_qv,g_qc,g_qr,g_qi,g_qs,g_qg
package   wsm3scheme    mp_physics==3                -             moist:qv,qc,qr;a_moist:a_qv,a_qc,a_qr;g_moist:g_qv,g_qc,g_qr
package   wsm5scheme    mp_physics==4                -             moist:qv,qc,qr,qi,qs;a_moist:a_qv,a_qc,a_qr,a_qi,a_qs;g_moist:g_qv,g_qc,g_qr,g_qi,g_qs
package   etampnew      mp_physics==5                -             moist:qv,qc,qr,qi;a_moist:a_qv,a_qc,a_qr,a_qi;g_moist:g_qv,g_qc,g_qr,g_qi
package   wsm6scheme    mp_physics==6                -             moist:qv,qc,qr,qi,qs,qg;a_moist:a_qv,a_qc,a_qr,a_qi,a_qs,a_qg;g_moist:g_qv,g_qc,g_qr,g_qi,g_qs,g_qg
package   ncepcloud3    mp_physics==98               -             moist:qv,qc,qr;a_moist:a_qv,a_qc,a_qr;g_moist:g_qv,g_qc,g_qr
package   ncepcloud5    mp_physics==99               -             moist:qv,qc,qr,qi,qs;a_moist:a_qv,a_qc,a_qr,a_qi,a_qs;g_moist:g_qv,g_qc,g_qr,g_qi,g_qs

package   rrtmscheme    ra_lw_physics==1             -             -
package   gfdllwscheme  ra_lw_physics==99            -             -

package   swradscheme   ra_sw_physics==1             -             -
package   gsfcswscheme  ra_sw_physics==2             -             -
package   gfdlswscheme  ra_sw_physics==99            -             -

package   sfclayscheme   sf_sfclay_physics==1        -             -
package   myjsfcscheme   sf_sfclay_physics==2        -             -
package   slabscheme     sf_surface_physics==1       -             -
package   lsmscheme      sf_surface_physics==2       -             -
package   ruclsmscheme   sf_surface_physics==3       -             -
package   ysuscheme      bl_pbl_physics==1           -             -
package   myjpblscheme   bl_pbl_physics==2           -             -
package   gfsscheme      bl_pbl_physics==3           -             -
package   mrfscheme      bl_pbl_physics==99          -             -

package   kfetascheme    cu_physics==1               -             -
package   bmjscheme      cu_physics==2               -             -
package   gdscheme       cu_physics==3               -             -
package   sasscheme      cu_physics==4               -             -
package   kfscheme       cu_physics==99              -             -
                                                
# only need to specify these once; not for every io_form* variable
package   io_intio    io_form_restart==1                     -             -
package   io_netcdf   io_form_restart==2                     -             -
# Four placeholders for additional packages (we can go beyond zzz 
# but that will entail modifying frame/module_io.F and frame/md_calls.m4)
# Please note these are placeholders; HDF has not been implemented yet.
package   io_hdf      io_form_restart==3                     -             -
package   io_phdf5    io_form_restart==4                     -             -
#package   io_xxx      io_form_restart==4                     -             -
# yyy = grib output from WSI
package   io_yyy      io_form_restart==5                     -             -
package   io_zzz      io_form_restart==6                     -             -
package   io_mcel     io_form_restart==7                     -             -
                                                
#---------------------------------------------------------------------------------------------------------------------------------------
## communications                                               

### 8. Edit the Registry file and create a halo-exchange for x_1.

# Halo Update Communications

halo      HALO_EM_INIT_1 dyn_em 48:u_1,u_2,v_1,v_2,w_1,w_2,ph_1,ph_2
halo      HALO_EM_INIT_2 dyn_em 48:t_1,t_2,mu_1,mu_2,tke_1,tke_2,ww,phb
halo      HALO_EM_INIT_3 dyn_em 48:ph0,php,t_init,mub,mu0,p,al,alt,alb
halo      HALO_EM_INIT_4 dyn_em 48:pb,h_diabatic,msft,msfu,msfv,f,e,sina,cosa,ht
halo      HALO_EM_INIT_5 dyn_em 48:moist_1,moist_2,chem_1,chem_2

halo      HALO_EM_TL_INIT_1 dyn_em 48:g_u_1,g_u_2,g_v_1,g_v_2,g_w_1,g_w_2,g_ph_1,g_ph_2
halo      HALO_EM_TL_INIT_2 dyn_em 48:g_t_1,g_t_2,g_mu_1,g_mu_2,g_ww,g_phb
halo      HALO_EM_TL_INIT_3 dyn_em 48:g_php,g_mub,g_p,g_al,g_alt
halo      HALO_EM_TL_INIT_5 dyn_em 48:g_moist_1,g_moist_2

halo      HALO_EM_AD_INIT_1 dyn_em 48:a_u_1,a_u_2,a_v_1,a_v_2,a_w_1,a_w_2,a_ph_1,a_ph_2
halo      HALO_EM_AD_INIT_2 dyn_em 48:a_t_1,a_t_2,a_mu_1,a_mu_2,a_ww,a_phb
halo      HALO_EM_AD_INIT_3 dyn_em 48:a_php,a_mub,a_p,a_al,a_alt
halo      HALO_EM_AD_INIT_5 dyn_em 48:a_moist_1,a_moist_2,a_ru_tendf,a_rv_tendf,a_rw_tendf,a_ph_tendf,a_t_tendf,a_moist_tend

halo HALO_ADJ_ADVANCE_U dyn_em 48:a_u_2,a_ru_tend,a_v_2,a_rv_tend,p,a_p,pb,ph_2,a_ph_2,\
php,a_php,alt,a_alt,al,a_al,mu_2,a_mu_2,muu,a_muu,cqu,a_cqu,muv,a_muv,cqv,a_cqv,a_mudf


halo      HALO_EM_A dyn_em  8:ru,rv,rw,ww,php,alt,al,p,muu,muv,mut
halo   HALO_EM_TL_A dyn_em  8:g_ru,g_rv,g_rw,g_ww,g_php,g_alt,g_al,g_p,g_muu,g_muv,g_mut
halo   HALO_ADJ_A dyn_em  8:a_ru,a_rv,a_rw,a_ww,a_php,a_alt,a_al,a_p,a_muu,a_muv,a_mut,\

halo      HALO_EM_PHYS_A  dyn_em 4:u_2,v_2
halo   HALO_EM_PHYS_TL_A  dyn_em 4:g_u_2,g_v_2
halo      HALO_EM_PHYS_PBL dyn_em        4:rublten,rvblten
halo      HALO_EM_PHYS_DIFFUSION dyn_em  4:defor11,defor22,defor12,defor13,defor23,div,xkmv,xkmh,xkmhd,xkhv,xkhh,tke_1,tke_2
halo      HALO_EM_TKE_ADVECT_3 dyn_em 24:tke_2
halo      HALO_EM_TKE_ADVECT_5 dyn_em 48:tke_2
halo      HALO_EM_TKE_A dyn_em 4:ph_2,phb
halo   HALO_EM_TKE_TL_A dyn_em 4:g_ph_2
halo      HALO_EM_TKE_B dyn_em 4:z,rdz,rdzw,zx,zy
halo      HALO_EM_TKE_C dyn_em 8:u_2,v_2,z,zx,zy,rdz,rdzw
halo   HALO_EM_TKE_TL_C dyn_em 8:g_u_2,g_v_2,g_z
halo      HALO_EM_TKE_D dyn_em 8:defor11,defor22,defor33,defor12,defor13,defor23,div
halo      HALO_EM_TKE_E dyn_em 8:xkmv,xkmh,xkmhd,xkhv,xkhh,BN2,moist_2
halo   HALO_EM_TKE_TL_E dyn_em 8:moist_2
halo      HALO_EM_TKE_3 dyn_em   24:tke_1,tke_2
halo      HALO_EM_TKE_5 dyn_em   48:tke_1,tke_2
halo      HALO_EM_TKE_F dyn_em   48:tke_1,tke_2
halo      HALO_EM_B dyn_em 4:ph_2,al,p,t_1,t_save,u_save,v_save,mu_1,mu_2,mudf,php,alt,pb
halo   HALO_EM_TL_B dyn_em 4:g_ph_2,g_al,g_p,g_t_1,g_t_save,g_u_save,g_v_save,g_mu_1,g_mu_2,g_mudf,g_php,g_alt
halo      HALO_EM_B2 dyn_em 4:ru_tend,rv_tend
halo   HALO_EM_TL_B2 dyn_em 4:g_ru_tend,g_rv_tend
halo      HALO_EM_C dyn_em    4:u_2,v_2
halo   HALO_EM_TL_C dyn_em    4:g_u_2,g_v_2
halo      HALO_EM_C2 dyn_em    4:ph_2,al,p,mu_2,muts,mudf,muts,muus,muvs
halo   HALO_EM_TL_C2 dyn_em    4:g_ph_2,g_al,g_p,g_mu_2,g_muts,g_mudf
halo      HALO_EM_D dyn_em    4:ru_m,rv_m
halo   HALO_EM_TL_D dyn_em    4:g_ru_m,g_rv_m
halo      HALO_EM_D2_3 dyn_em 24:u_2,v_2,w_2,t_2,ph_2;24:moist_2,chem_2;4:mu_2,al
halo   HALO_EM_TL_D2_3 dyn_em 24:g_u_2,g_v_2,g_w_2,g_t_2,g_ph_2;24:g_moist_2;4:g_mu_2,g_al
halo      HALO_EM_D2_5 dyn_em 48:u_2,v_2,w_2,t_2,ph_2;24:moist_2,chem_2;4:mu_2,al
halo   HALO_EM_TL_D2_5 dyn_em 48:g_u_2,g_v_2,g_w_2,g_t_2,g_ph_2;24:g_moist_2;4:g_mu_2,g_al
halo      HALO_EM_D3_3 dyn_em 24:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,moist_1,moist_2,chem_1,chem_2;4:mu_1,mu_2
halo   HALO_EM_TL_D3_3 dyn_em 24:g_u_1,g_u_2,g_v_1,g_v_2,g_w_1,g_w_2,g_t_1,g_t_2,g_ph_1,g_ph_2,g_moist_1,g_moist_2,;4:g_mu_1,g_mu_2
halo      HALO_EM_D3_5 dyn_em 48:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,moist_1,moist_2,chem_1,chem_2;4:mu_1,mu_2
halo   HALO_EM_TL_D3_5 dyn_em 48:g_u_1,g_u_2,g_v_1,g_v_2,g_w_1,g_w_2,g_t_1,g_t_2,g_ph_1,g_ph_2,g_moist_1,g_moist_2,;4:g_mu_1,g_mu_2
halo      HALO_EM_E_3 dyn_em 24:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,;4:mu_1,mu_2
halo   HALO_EM_TL_E_3 dyn_em 24:g_u_1,g_u_2,g_v_1,g_v_2,g_w_1,g_w_2,g_t_1,g_t_2,g_ph_1,g_ph_2;4:g_mu_1,g_mu_2
halo      HALO_EM_E_5 dyn_em 48:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,;4:mu_1,mu_2
halo   HALO_EM_TL_E_5 dyn_em 48:g_u_1,g_u_2,g_v_1,g_v_2,g_w_1,g_w_2,g_t_1,g_t_2,g_ph_1,g_ph_2;4:g_mu_1,g_mu_2
halo      HALO_EM_MOIST_E_3 dyn_em 24:moist_1,moist_2
halo   HALO_EM_MOIST_TL_E_3 dyn_em 24:g_moist_1,g_moist_2
halo      HALO_EM_MOIST_E_5 dyn_em 48:moist_1,moist_2
halo   HALO_EM_MOIST_TL_E_5 dyn_em 48:g_moist_1,g_moist_2
halo      HALO_EM_CHEM_E_3 dyn_em 24:chem_1,chem_2
halo      HALO_EM_CHEM_E_5 dyn_em 48:chem_1,chem_2

halo      HALO_EM_COUPLE_A   dyn_em 24:mub,mu_1,mu_2
period    PERIOD_EM_COUPLE_A dyn_em 2:mub,mu_1,mu_2
halo      HALO_EM_COUPLE_B   dyn_em 48:ph_1,ph_2,w_1,w_2,t_1,t_2,u_1,u_2,v_1,v_2,\
                                       moist_1,moist_2,chem_1,chem_2
period    PERIOD_EM_COUPLE_B dyn_em 3:ph_1,ph_2,w_1,w_2,t_1,t_2,u_1,u_2,v_1,v_2,\
                                       moist_1,moist_2,chem_1,chem_2

# For moving nests
halo      em_shift_halo_y  dyn_em 48:imask_nostag,imask_xstag,imask_ystag,imask_xystag,u_2,v_2,t_2
halo      em_shift_halo_x  dyn_em 48:imask_nostag,imask_xstag,imask_ystag,imask_xystag,u_2,v_2,t_2

# Periodic Boundary Communications

period    PERIOD_BDY_EM_INIT dyn_em 3:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,t_init,phb,ph0,php,pb,al,alt,alb,mu_1,mu_2,mub,mu0,ht,msft,msfu,msfv,sina,cosa,e,f
period    PERIOD_BDY_EM_MOIST dyn_em 3:moist_1,moist_2
period    PERIOD_BDY_EM_CHEM dyn_em 3:chem_1,chem_2
period    PERIOD_BDY_EM_MOIST2 dyn_em 3:moist_2
period    PERIOD_BDY_EM_CHEM2 dyn_em 3:chem_2
period    PERIOD_BDY_EM_A dyn_em 2:ru,rv,rw,ww,php,alt,p,muu,muv,mut,ph_2
period    PERIOD_BDY_EM_A1  dyn_em 3:rdzw,rdz,z,zx,zy
period    PERIOD_BDY_EM_PHY_BC dyn_em 2:rublten,rvblten,xkmh,xkmhd,xkmv,xkhh,xkhv,div,defor11,defor22,defor12,defor13,defor23,defor33,tke_2
period    PERIOD_BDY_EM_B dyn_em 2:ru_tend,rv_tend,ph_2,al,p,t_1,t_save,u_save,v_save,mu_1,mu_2,mudf,php,alt,pb
period    PERIOD_BDY_EM_B3 dyn_em 2:ph_2,al,p,mu_2,muts,mudf
period    PERIOD_BDY_EM_B2 dyn_em 2:ru_tend,rv_tend
period    PERIOD_BDY_EM_C dyn_em 2:u_2,u_save,v_2,v_save,t_2,t_save,muv,msfv,muu,msfu
period    PERIOD_BDY_EM_D dyn_em 3:u_2,v_2,w_2,t_2,ph_2,mu_2,tke_2
period    PERIOD_BDY_EM_D3 dyn_em 3:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,mu_1,mu_2

##

state    real   a_u              ikj     dyn_em      2       X      i013rh023
state    real   a_ru             ikj     dyn_em      1       X      -        
state    real   a_ru_m           ikj     dyn_em      1       X      -        
state    real   a_ru_tend        ikj     dyn_em      1       X      -        
i1       real   a_ru_tendf       ikj     dyn_em      1       X                                          
state    real   a_u_save         ikj     dyn_em      1       X      -        
state    real   a_v              ikj     dyn_em      2       Y      i013rh023
state    real   a_rv             ikj     dyn_em      1       Y      -        
state    real   a_rv_m           ikj     dyn_em      1       Y      -        
state    real   a_rv_tend        ikj     dyn_em      1       Y      -        
i1       real   a_rv_tendf       ikj     dyn_em      1       Y                                          
state    real   a_v_save         ikj     dyn_em      1       Y      -        
state    real   a_w              ikj     dyn_em      2       Z      i013rh023
state    real   a_ww             ikj     dyn_em      1       Z      r         
state    real   a_rw             ikj     dyn_em      1       Z      -         
i1       real   a_ww1            ikj     dyn_em      1       Z                                          
i1       real   a_ww_m           ikj     dyn_em      1       Z                                          
i1       real   a_rw_tend        ikj     dyn_em      1       Z                                          
i1       real   a_rw_tendf       ikj     dyn_em      1       Z                                          
i1       real   a_w_save         ikj     dyn_em      1       Z                                          
state    real   a_ph             ikj     dyn_em      2       Z     i013rh023
state    real   a_phb            ikj     dyn_em      1       Z     irh
state    real   a_php            ikj     dyn_em      1       Z      r
i1       real   a_ph_tend        ikj     dyn_em      1       Z 
i1       real   a_ph_tendf       ikj     dyn_em      1       Z 
i1       real   a_ph_save        ikj     dyn_em      1       Z 
state    real   a_t              ikj     dyn_em      2       -     i013rh023
i1       real   a_t_tend         ikj     dyn_em      1       -  
i1       real   a_t_tendf        ikj     dyn_em      1       -  
i1       real   a_t_2save        ikj     dyn_em      1       -   
state    real   a_t_save         ikj     dyn_em      1       -     
state    real   a_mu              ij     dyn_em      2       -     i013rh023
state    real   a_mub             ij     dyn_em      1       -     irh
state    real   a_mudf            ij     dyn_em      1       -     
state    real   a_muu             ij     dyn_em      1       -     
i1       real   a_muus            ij     dyn_em      1       -     
state    real   a_muv             ij     dyn_em      1       -     
i1       real   a_muvs            ij     dyn_em      1       -     
state    real   a_mut             ij     dyn_em      1       -     
state    real   a_muts            ij     dyn_em      1       -     
i1       real   a_muave           ij     dyn_em      1       -     
i1       real   a_mu_save         ij     dyn_em      1       -     
i1       real   a_mu_tend         ij     dyn_em      1       -     
state    real   a_p              ikj     dyn_em      1       -      rh02 
state    real   a_al             ikj     dyn_em      1       -      rh02
state    real   a_alt            ikj     dyn_em      1       -      rh02
i1       real   a_advect_tend    ikj     dyn_em      1       -                                          
state       real   a_alpha          ikj     dyn_em      1       -    h02
state       real   a_a              ikj     dyn_em      1       -    h02
state       real   a_gamma          ikj     dyn_em      1       -    h02
state       real   a_c2a            ikj     dyn_em      1       -    h02
state       real   a_rho            ikj     dyn_em      1       -    h02
state       real   a_phm            ikj     dyn_em      1       -    h02
state       real   a_cqu            ikj     dyn_em      1       -    h02
state       real   a_cqv            ikj     dyn_em      1       -    h02
state       real   a_cqw            ikj     dyn_em      1       -    h02
state       real   a_pm1            ikj     dyn_em      1       -    h02
state    real    a_z              ikj     dyn_em      1       Z     -  
i1       real    a_z_at_w         ikj     dyn_em      1       Z 
state   real    -              ikjft   a_moist       2       -     -    -                                       
state   real    a_qv             ikjft   a_moist       2       -     i013rh023  "A_QVAPOR"
state   real    a_qc             ikjft   a_moist       2       -     i013rh023  "A_QCLOUD"
state   real    a_qr             ikjft   a_moist       2       -     i013rh023  "A_QRAIN"
state   real    a_qi             ikjft   a_moist       2       -     i013rh023  "A_QICE"
state   real    a_qs             ikjft   a_moist       2       -     i013rh023  "A_QSNOW"
state   real    a_qg             ikjft   a_moist       2       -     i013rh023  "A_QGRAUP"
i1       real  a_th_phy          ikj      misc        1       -                                          
i1       real  a_pi_phy          ikj      misc        1       -                                          
i1       real  a_p_phy           ikj      misc        1       -                                          
i1       real  a_t_phy           ikj      misc        1       -                                          
i1       real  a_u_phy           ikj      misc        1       -                                          
i1       real  a_v_phy           ikj      misc        1       -                                          
i1       real  a_dz8w            ikj      misc        1       Z                                          
i1       real  a_p8w             ikj      misc        1       Z                                          
i1       real  a_t8w             ikj      misc        1       Z                                          
i1       real  a_rho_phy         ikj      misc        1       -                                          
state   real   a_xkmhd           ikj      misc        1       -     -
state    real   a_fcx            w         misc     -       -      irh02      
state    real   a_gcx            w         misc     -       -      irh02      
state    real   a_dtbc            -        misc     -       -      irh02      

#WARNING 1 : a_mu1 in halo spec HALO_ADJ_C (dyn_em) is not defined in registry.
#WARNING 1 : a_t_2ave in halo spec HALO_ADJ_C (dyn_em) is not defined in registry.
#WARNING 1 : a_mu1 in halo spec  (dyn_em) is not defined in registry.
#WARNING 1 : a_t_2ave in halo spec  (dyn_em) is not defined in registry.
#WARNING 1 : a_mu1 in halo spec  (dyn_em) is not defined in registry.
#WARNING 1 : a_t_2ave in halo spec  (dyn_em) is not defined in registry.
#WARNING 1 : a_mu1 in halo spec  (dyn_em) is not defined in registry.
#WARNING 1 : a_t_2ave in halo spec  (dyn_em) is not defined in registry.
#WARNING 1 : a_mu1 in halo spec  (dyn_em) is not defined in registry.
#WARNING 1 : a_t_2ave in halo spec  (dyn_em) is not defined in registry.
#
#        call a_advance_w( w_2,a_w_2,rw_tend,a_rw_tend,ww,a_ww,u_2,a_u_2,v_2,a_v_2,mu_2,a_mu_2,mut,a_mut,muave,a_muave,muts,a_muts,&
#&t_2save,a_t_2save,t_2,a_t_2,t_save,a_t_save,ph_2,a_ph_2,ph_save,a_ph_save,phb,ph_tend,a_ph_tend,ht,c2a,a_c2a,cqw,a_cqw,&
#&alt,a_alt,alb,a,a_a,alpha,a_alpha,gamma,a_gamma,rdx,rdy,dts,t0,epssm,fnm,fnp,rdnw,rdn,cf1,cf2,cf3,msft,config_flags,ids,&
#&ide,jds,jde,ims,ime,jms,jme,kms,kme, &
#
#subroutine a_advance_w( w, a_w, rw_tend, a_rw_tend, ww, a_ww, u, a_u, v, a_v, mu1, a_mu1, mut, a_mut, muave, a_muave, muts, a_muts,&
#& t_2ave, a_t_2ave, t_2, a_t_2, t_1, a_t_1, ph, a_ph, ph_1, a_ph_1, phb, ph_tend, a_ph_tend, ht, c2a, a_c2a, cqw, a_cqw, alt, &
#&a_alt, alb, a, a_a, alpha, a_alpha, gamma, a_gamma, rdx, rdy, dts, t0, epssm, fnm, fnp, rdnw, rdn, cf1, cf2, cf3, msft, &
#&config_flags, ids, ide, jds, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )
#

halo HALO_ADJ_A1 dyn_em  48:ru,rv,rw,ww,php,alt,al,p,muu,muv,mut
halo HALO_ADJ_A dyn_em 48:msft,ht,a_u_2,a_v_2,a_w_2
halo HALO_ADJ_B dyn_em 48:a_muts,a_muus,a_muvs
halo HALO_ADJ_C dyn_em 48:muu,muv,msfu,msft,ww,a_ww,ww1,u_2,a_u_2,u_save,v_2,a_u_2,v_save,a_t_2,t_save,a_t_save,\
a_a,alpha,a_alpha,a_alt,a_c2a,a_cqw,a_gamma,a_mu_2,a_muave,a_mut,a_muts,a_ph_2,a_ph_tend,a_rw_tend,a_t_1,a_t_2,a_t_2save,a_ww

halo HALO_ADJ_D dyn_em 48:a_al,a_alt,a_cqu,a_cqv,a_mu_2,a_mu_save,a_mudf,a_mut,a_muts,a_muu,a_muus,a_muv,a_muvs,\
a_p,a_ph_2,a_ph_save,a_php,a_ru_tend,a_rv_tend,a_t_2,a_t_save,a_u_2,a_u_save,a_v_2,a_v_save,a_w_2,a_w_save,al,\
alt,cqu,cqv,msft,msfu,msfv,mu_2,mut,muts,muu,muus,muv,muvs,p,pb,ph_2,php,t_2,t_save,u_2,u_save,v_2,v_save,\
w_2,w_save

halo HALO_ADJ_RKUPDATE dyn_em 80:\
a_advect_tend,a_alt,a_moist_1,a_moist_2,a_moist_tend,a_mut,a_ru_m,a_rv_m,a_ww_m,a_xkmhd,alt,\
moist_1,moist_2,msft,msfu,msfv,mut,ru_m,rv_m,ww_m,xkmhd

halo HALO_ADJ_RKTEND dyn_em 80:a_ru_tend,a_rv_tend,a_rw_tend,a_ph_tend,a_t_tend,a_ru_tendf,a_rv_tendf,a_rw_tendf,\
a_t_tendf,a_mu_tend,a_u_save,a_v_save,a_w_save,a_ph_save,a_t_save,ru,a_ru,rv,a_rv,rw,a_rw,ww,a_ww,u_2,a_u_2,v_2,a_v_2,w_2,\
a_w_2,t_2,a_t_2,ph_2,a_ph_2,u_1,a_u_1,v_1,a_v_1,w_1,a_w_1,t_1,a_t_1,ph_1,a_ph_1,phb,t_init,mu_2,a_mu_2,mut,a_mut,muu,a_muu,\
muv,a_muv,mub,al,a_al,alt,a_alt,p,a_p,pb,php,a_php,cqu,a_cqu,cqv,a_cqv,cqw,a_cqw,msfu,msfv,msft,f,e,sina,\
cosa,xkmhd,a_xkmhd,a_moist_tend,a_advect_tend

halo HALO_ADJ_E dyn_em 48:u_1,a_u_1,u_2,a_u_2,v_1,a_v_1,v_2,a_v_2,w_1,a_w_1,w_2,a_w_2,t_1,a_t_1,t_2,a_t_2,a_ph_1,a_ph_2,mub,mu_1,a_mu_1,mu_2,a_mu_2,muu,a_muu,muus,a_muus,muv,a_muv,muvs,a_muvs,mut,a_mut,muts,a_muts,a_mudf,a_u_save,a_v_save,a_w_save,a_t_save,a_ph_save,a_mu_save,a_ww,a_ww1,a_c2a,pb,p,a_p,alt,a_alt,msfu,msfv,msft

halo HALO_ADJ_F dyn_em 48:a_p,a_p8w,a_t_2,a_t8w,a_xkmhd,bn2,defor11,defor12,defor13,defor22,defor23,defor33,div,moist_2,p,p8w,rdz,rdzw,t8w,xkhh,xkhv,xkmh,xkmhd,xkmv

halo HALO_ADJ_G dyn_em 48:u_2,a_u_2,v_2,a_v_2,w_2,a_w_2,a_ph_2,mu_2,a_mu_2,moist_2,a_moist_2,a_ru,a_rv,a_rw,a_ww,a_php,a_alt,muu,a_muu,muv,a_muv,mub,mut,a_mut,a_al,a_cqu,a_cqv,a_cqw,msfu,msfv,msft

halo HALO_ADJ_SAVE1  dyn_em   48:\
xkmhd,ww_m,w_2,w_1,v_2,v_1,u_2,u_1,t_tendf,t_2,t_1,rw_tendf,rv_tendf,rv_m,ru_tendf,ru_m,pm1,\
ph_tendf,ph_2,ph_1,p,mu_2,mu_1,moist_tend,moist_2,moist_1,gamma,alpha,al,a

halo HALO_ADJ_SAVE2  dyn_em   48:\
ww_m,rv_m,ru_m,a,alpha,gamma,xkmhd,w_2,w_1,v_2,v_1,u_2,u_1,t_tendf,t_2,t_1,rw_tendf,rv_tendf,\
ru_tendf,pm1,ph_tendf,ph_2,ph_1,p,mu_2,mu_1,moist_tend,moist_2,moist_1,al,mudf

halo HALO_ADJ_SAVE3  dyn_em   48:\
u_1,u_2,v_1,v_2,w_1,w_2,ph_1,ph_2,t_1,t_2,mu_1,mu_2,p,al,moist_1,moist_2,rv_m,ru_tendf,ru_m,moist_tend

halo HALO_ADJ_SAVE4  dyn_em   48:\
al,mu_2,p,ph_2

halo HALO_ADJ_SAVE5  dyn_em   48:\
ww,u_2,v_2

halo HALO_ADJ_SAVE6  dyn_em   48:\
w_2,ww,u_2,v_2,mu_2,t_2,ph_2,a,t_2save

halo HALO_ADJ_SAVE7  dyn_em   48:\
u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,mu_1,mu_2,p

halo HALO_ADJ_SAVE8  dyn_em   48:\
al,p,ph_2,t_2,mu_2

halo HALO_ADJ_SPEC_BDY_DRY dyn_em 48:\
a_u_2,a_ru_tend,a_v_2,a_rv_tend,p,a_p,pb,ph_2,a_ph_2,\
a_ru_tend,a_rv_tend,a_ph_tend,a_t_tend,a_rw_tend,a_mu_tend

halo HALO_ADJ_ADDTEND_DRY dyn_em 48:\
a_ru_tend,a_rv_tend,a_rw_tend,a_ph_tend,a_t_tend,a_ru_tendf,a_rv_tendf,a_rw_tendf,a_ph_tendf,a_t_tendf,\
a_u_save,a_v_save,a_w_save,a_ph_save,a_t_save,h_diabatic,a_mut,msft,msfu,msfv

halo HALO_ADJ_RELAXBDY_DRY dyn_em 48:\
a_u_save,a_v_save,a_ph_save,a_t_save,a_w_save,a_mu_tend,a_ru,a_rv,ph_2,a_ph_2,t_2,a_t_2,\
w_2,a_w_2,a_mu_2,mut,a_mut

halo HALO_ADJ_ADVANCE_W dyn_em 48:w_2,a_w_2,rw_tend,a_rw_tend,ww,a_ww,u_2,a_u_2,v_2,a_v_2,mu_2,a_mu_2,mut,a_mut,muave,a_muave,muts,a_muts,\
t_2save,a_t_2save,t_2,a_t_2,t_save,a_t_save,ph_2,a_ph_2,ph_save,a_ph_save,phb,p h_tend,a_ph_tend,ht,c2a,a_c2a,cqw,a_cqw,\
alt,a_alt,alb,a,a_a,alpha,a_alpha,gamma,a_gamma,msft



state    real   g_u              ikj     dyn_em      2       X      i013rh023
state    real   g_ru             ikj     dyn_em      1       X      -        
state    real   g_ru_m           ikj     dyn_em      1       X      -        
state    real   g_ru_tend        ikj     dyn_em      1       X      -        
i1       real   g_ru_tendf       ikj     dyn_em      1       X         
state    real   g_u_save         ikj     dyn_em      1       X      -        

state    real   g_v              ikj     dyn_em      2       Y      i013rh023
state    real   g_rv             ikj     dyn_em      1       Y      -        
state    real   g_rv_m           ikj     dyn_em      1       Y      -        
state    real   g_rv_tend        ikj     dyn_em      1       Y      -        
i1       real   g_rv_tendf       ikj     dyn_em      1       Y        
state    real   g_v_save         ikj     dyn_em      1       Y      -        

state    real   g_w              ikj     dyn_em      2       Z      i013rh023
state    real   g_ww             ikj     dyn_em      1       Z      r         
state    real   g_rw             ikj     dyn_em      1       Z      -         
i1       real   g_ww1            ikj     dyn_em      1       Z                                          
i1       real   g_ww_m           ikj     dyn_em      1       Z                                          
i1       real   g_rw_tend        ikj     dyn_em      1       Z                                          
i1       real   g_rw_tendf       ikj     dyn_em      1       Z                                          
i1       real   g_w_save         ikj     dyn_em      1       Z                                          
state    real   g_ph             ikj     dyn_em      2       Z     i013rh023
state    real   g_phb            ikj     dyn_em      1       Z     irh
state    real   g_php            ikj     dyn_em      1       Z      r
i1       real   g_ph_tend        ikj     dyn_em      1       Z 
i1       real   g_ph_tendf       ikj     dyn_em      1       Z 
i1       real   g_ph_save        ikj     dyn_em      1       Z 
state    real   g_t              ikj     dyn_em      2       -     i013rh023
i1       real   g_t_tend         ikj     dyn_em      1       -  
i1       real   g_t_tendf        ikj     dyn_em      1       -  
i1       real   g_t_2save        ikj     dyn_em      1       -   
state    real   g_t_save         ikj     dyn_em      1       -     
state    real   g_mu              ij     dyn_em      2       -     i013rh023
state    real   g_mub             ij     dyn_em      1       -     irh
state    real   g_mudf            ij     dyn_em      1       -     
state    real   g_muu             ij     dyn_em      1       -     
i1       real   g_muus            ij     dyn_em      1       -     
state    real   g_muv             ij     dyn_em      1       -     
i1       real   g_muvs            ij     dyn_em      1       -     
state    real   g_mut             ij     dyn_em      1       -     
state    real   g_muts            ij     dyn_em      1       -     
i1       real   g_muave           ij     dyn_em      1       -     
i1       real   g_mu_save         ij     dyn_em      1       -     
i1       real   g_mu_tend         ij     dyn_em      1       -     
state    real   g_p              ikj     dyn_em      1       -      rh02 
state    real   g_al             ikj     dyn_em      1       -      rh02
state    real   g_alt            ikj     dyn_em      1       -      rh02
i1       real   g_advect_tend    ikj     dyn_em      1       -                                          
state       real   g_alpha          ikj     dyn_em      1       -    h02
state       real   g_a              ikj     dyn_em      1       -    h02
state       real   g_gamma          ikj     dyn_em      1       -    h02
state       real   g_c2a            ikj     dyn_em      1       -    h02
state       real   g_rho            ikj     dyn_em      1       -    h02
state       real   g_phm            ikj     dyn_em      1       -    h02
state       real   g_cqu            ikj     dyn_em      1       -    h02
state       real   g_cqv            ikj     dyn_em      1       -    h02
state       real   g_cqw            ikj     dyn_em      1       -    h02
state       real   g_pm1            ikj     dyn_em      1       -    h02
state    real    g_z              ikj     dyn_em      1       Z     -  
i1       real    g_z_at_w         ikj     dyn_em      1       Z 
state   real    -              ikjft   g_moist       2       -     -    -                                       
state   real    g_qv             ikjft   g_moist       2       -     i013rh023  "G_QVAPOR"
state   real    g_qc             ikjft   g_moist       2       -     i013rh023  "G_QCLOUD"
state   real    g_qr             ikjft   g_moist       2       -     i013rh023  "G_QRAIN"
state   real    g_qi             ikjft   g_moist       2       -     i013rh023  "G_QICE"
state   real    g_qs             ikjft   g_moist       2       -     i013rh023  "G_QSNOW"
state   real    g_qg             ikjft   g_moist       2       -     i013rh023  "G_QGRAUP"
i1       real  g_th_phy          ikj      misc        1       -                                          
i1       real  g_pi_phy          ikj      misc        1       -                                          
i1       real  g_p_phy           ikj      misc        1       -                                          
i1       real  g_t_phy           ikj      misc        1       -                                          
i1       real  g_u_phy           ikj      misc        1       -                                          
i1       real  g_v_phy           ikj      misc        1       -                                          
i1       real  g_dz8w            ikj      misc        1       Z                                          
i1       real  g_p8w             ikj      misc        1       Z                                          
i1       real  g_t8w             ikj      misc        1       Z                                          
i1       real  g_rho_phy         ikj      misc        1       -                                          
state   real   g_xkmhd           ikj      misc        1       -     -
state    real   g_fcx            w         misc     -       -      irh02      
state    real   g_gcx            w         misc     -       -      irh02      
state    real   g_dtbc            -        misc     -       -      irh02      


halo HALO_0376 dyn_em 48:a_al,a_moist_2,a_mu_2,a_p,a_ph_2,a_t_2,a_u_2,a_v_2,a_w_2,al,alb,alt,cosa,e,f,moist_1,moist_2,msft,msfu,msfv,mu0,mu_1,mu_2,mub,p,pb,ph_1,ph_2,phb,php,t_1,t_2,t_init,u_1,u_2,v_1,v_2,w_1,w_2,ww
halo HALO_0394 dyn_em 48:a,alpha,gamma,moist_2,mu_2,ph_2,t_2,u_2,v_2,w_2
halo HALO_0616 dyn_em 48:mut,muu,muv,ru,rv,rw
halo HALO_0626 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_0637 dyn_em 8:mut,muu,muv,php,ru,rv,rw,ww,alt
halo HALO_0648 dyn_em 8:u_2,v_2
halo HALO_0660 dyn_em 8:z
halo HALO_0679 dyn_em 8:moist_2
halo HALO_0718 dyn_em 8:al,mu_1,mu_2,p,pb,ph_2,php,t_1,t_save,u_save,v_save;4:mudf,alt
halo HALO_0736 dyn_em 8:u_2,v_2
halo HALO_0747 dyn_em 48:al,alt,moist_2,mu_1,mu_2,p,pb,ph_1,ph_2,php,t_1,t_2,u_1,u_2,v_1,v_2,w_1,w_2,ww
halo HALO_0790 dyn_em 8:al,mu_2,mudf,muts,muus,muvs,p,ph_2
halo HALO_0821 dyn_em 8:ru_m,rv_m
halo HALO_0871 dyn_em 8:mudf,muts,muus,muvs,p;48:al,moist_1,moist_2,mu_2,ph_2,t_2,u_2,v_2,w_2
halo HALO_1015 dyn_em 48:al,mut,muu,muv,p,ru,rv,rw,ww
halo HALO_1026 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_1031 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_1043 dyn_em 8:u_2,v_2
halo HALO_1073 dyn_em 8:moist_2
halo HALO_1114 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1,t_save,u_save,v_save
halo HALO_1135 dyn_em 8:u_2,v_2
halo HALO_1146 dyn_em 48:al,alt,cosa,e,f,moist_1,moist_2,mu_1,mu_2,p,pb,ph_1,ph_2,phb,php,t_1,t_2,t_init,u_1,u_2,v_1,v_2,w_1,w_2,ww
halo HALO_1189 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1,t_save,u_save,v_save
halo HALO_1219 dyn_em 8:ru_m,rv_m
halo HALO_1278 dyn_em 8:mudf,muts,muus,muvs,p;48:al,moist_1,moist_2,mu_2,ph_2,t_2,u_2,v_2,w_2
halo HALO_1290 dyn_em 48:a,al,alpha,gamma,moist_1,moist_2,mu_1,mu_2,mudf,p,ph_1,ph_2,pm1,ru_m,rv_m,t_1,t_2,u_1,u_2,v_1,v_2,w_1,w_2,ww_m
halo HALO_1373 dyn_em 48:alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_1384 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_1389 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_1394 dyn_em 8:u_2,v_2
halo HALO_1407 dyn_em 8:u_2,v_2
halo HALO_1419 dyn_em 8:z
halo HALO_1441 dyn_em 8:moist_2
halo HALO_1487 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,ph_2,php,t_1,t_save,u_save,v_save
halo HALO_1507 dyn_em 8:u_2,v_2
halo HALO_1518 dyn_em 48:al,alt,moist_2,mu_1,mu_2,p,pb,ph_2,php,t_1,t_2,u_2,v_2,w_2,ww
halo HALO_1560 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1,t_save,u_save,v_save
halo HALO_1592 dyn_em 8:ru_m,rv_m
halo HALO_1654 dyn_em 48:a_al,a_moist_2,a_mu_2,a_p,a_ph_2,a_t_2,a_u_2,a_v_2,a_w_2,al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_1664 dyn_em 48:al,moist_2,mu_2,ph_2,t_2,u_2,v_2,w_2
halo HALO_1748 dyn_em 48:al,p,ww
halo HALO_1757 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_1763 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_1809 dyn_em 8:moist_2
halo HALO_1857 dyn_em 8:al,alt,mu_2,mudf,muts,p,ph_2,php
halo HALO_1880 dyn_em 8:u_2,v_2
halo HALO_1889 dyn_em 48:al,alt,moist_2,mu_1,mu_2,p,pb,ph_2,php,t_1,t_2,u_2,v_2,w_2,ww
halo HALO_1930 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,p,pb,ph_2,php,t_1
halo HALO_1974 dyn_em 48:al,mu_1,mu_2,p,ph_2,ru_m,rv_m,t_1,t_2,u_2,v_2,w_2
halo HALO_2036 dyn_em 8:ru_m,rv_m;48:a_mu_2,a_mudf,a_muts,a_ph_2,a_t_2,al,alt,cqu,cqv,mu_2,muts,muus,muvs,p,pb,ph_2,php,t_2,t_save,u_2,u_save,v_2,v_save,w_2,w_save
halo HALO_2112 dyn_em 48:a_muts,muts,muus,muvs
halo HALO_2154 dyn_em 80:a_advect_tend,a_alt,a_moist_1,a_moist_2,a_moist_tend,alt,moist_1,moist_2,msft,msfu,msfv,mut,ru_m,rv_m,ww_m
halo HALO_2249 dyn_em 48:al,alt,mut,p,ww
halo HALO_2258 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_2264 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_2309 dyn_em 8:moist_2
halo HALO_2358 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1,t_save,u_save,v_save
halo HALO_2386 dyn_em 8:u_2,v_2
halo HALO_2397 dyn_em 48:al,alt,moist_1,moist_2,msft,msfu,msfv,mu_1,mu_2,p,pb,ph_2,php,t_1,t_2,u_2,v_2,w_2,ww
halo HALO_2441 dyn_em 8:mu_1,mudf,ru_m,rv_m,t_1;48:a_mu_2,al,cqu,cqv,mu_2,muts,muus,muvs,p,pb,ph_2,php,t_2,t_save,u_2,u_save,v_2,v_save,w_2,w_save
halo HALO_2474 dyn_em 48:a_muts,a_muus,a_muvs
halo HALO_2527 dyn_em 48:a_moist_1,a_moist_2,a_moist_tend,a_mu_1,a_mu_2,a_ph_2,a_t_2,a_u_2,a_v_2,a_w_2,al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_2588 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_2593 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_2605 dyn_em 8:u_2,v_2
halo HALO_2636 dyn_em 8:moist_2
halo HALO_2676 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1,t_save,u_save,v_save
halo HALO_2696 dyn_em 8:u_2,v_2
halo HALO_2715 dyn_em 48:al,alt,moist_1,moist_2,mu_1,mu_2,p,pb,ph_1,ph_2,php,t_1,t_2,u_1,u_2,v_1,v_2,w_1,w_2,ww
halo HALO_2762 dyn_em 8:al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1
halo HALO_2773 dyn_em 48:al,mu_2,p,ph_2
halo HALO_2784 dyn_em 8:al,mu_2,mudf,p,ph_2
halo HALO_2802 dyn_em 48:u_2,v_2,ww
halo HALO_2810 dyn_em 8:u_2,v_2
halo HALO_2821 dyn_em 48:al,alt,moist_2,mu_1,mu_2,p,pb,ph_2,php,t_1,t_2,u_2,v_2,w_2,ww
halo HALO_2844 dyn_em 48:a,mu_2,ph_2,t_2,t_2save,u_2,v_2,w_2,ww
halo HALO_2881 dyn_em 48:a_moist_2,a_mu_1,a_t_2,a_u_2,a_v_2,a_w_2,al
halo HALO_2903 dyn_em 48:al
halo HALO_2955 dyn_em 48:a_mut,a_muts,a_t_save,al,alpha,t_save,u_2,u_save,v_2,v_save,ww1
halo HALO_2973 dyn_em 48:a,a_mut,a_muts,a_ph_save,a_t_save,alpha,c2a,cqw,gamma,mu_2,muave,muts,ph_2,ph_save,ph_tend,rw_tend,t_2,t_2save,w_2
halo HALO_2991 dyn_em 48:a_alt,a_mu_2,a_ph_2,a_t_2,a_w_2,a_ww
halo HALO_3013 dyn_em 48:a_alt,a_mu_2,a_ph_2,a_t_2,a_u_2,a_v_2,a_w_2,a_ww
halo HALO_3042 dyn_em 48:a_a,a_alpha,a_c2a,a_cqw,a_gamma,a_muave,a_mut,a_muts,a_ph_tend,a_rw_tend,a_t_2save,a_t_save,a_u_2,a_v_2,a_ww,ww,ww1
halo HALO_3072 dyn_em 48:a_mu_2,a_t_2,a_u_2,a_v_2,a_ww,al,ww
halo HALO_3120 dyn_em 48:a_mu_2,a_mu_save,a_mut,a_muts,a_muu,a_muus,a_muv,a_muvs,a_ph_save,a_t_2,a_t_save,a_u_2,a_u_save,a_v_2,a_v_save,a_w_save,a_ww,cqu,cqv,mu_2,muts,muus,muvs,ru_m,rv_m,w_save,ww
halo HALO_3212 dyn_em 48:al,p,ww
halo HALO_3222 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_3227 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_3273 dyn_em 8:moist_2
halo HALO_3313 dyn_em 48:mu_2,t_2,u_2,v_2,w_2
halo HALO_3335 dyn_em 8:al,alt,mu_1,mu_2,mudf,p,pb,ph_2,php,t_1,t_save,u_save,v_save
halo HALO_3341 dyn_em 48:al,mu_2,p,ph_2,t_2
halo HALO_3356 dyn_em 8:mu_2,ph_2,u_2,v_2;48:a_alt,a_mu_2,a_p,a_ph_2,a_php,a_t_2,a_u_2,a_v_2,a_w_2,a_ww,al,alt,p,php,ww
halo HALO_3386 dyn_em 48:a_alt,a_mu_2,a_p,a_ph_2,a_php,al,p
halo HALO_3419 dyn_em 48:a_alt,a_c2a,a_mu_2,a_mu_save,a_mudf,a_mut,a_muts,a_muu,a_muv,a_p,a_ph_2,a_ph_save,a_t_2,a_t_save,a_u_save,a_v_save,a_w_save,a_ww1,mu_1,mu_2,muts,muus,muvs,pb,t_1,t_2,u_2,v_2
halo HALO_3423 dyn_em 48:a_alt,a_mu_2,a_ph_2,a_t_2,al
halo HALO_3510 dyn_em 48:al
halo HALO_3520 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_3525 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_3593 dyn_em 48:a_alt,a_mu_1,a_mu_2,a_p,a_ph_1,a_ph_2,a_t_1,a_t_2,a_u_1,a_u_2,a_v_1,a_v_2,a_w_1,a_w_2,a_ww
halo HALO_3606 dyn_em 48:a_mu_tend,a_p,a_ph_2,a_ph_tend,a_ru_tend,a_rv_tend,a_rw_tend,a_t_tend,a_u_2,a_v_2,ph_2
halo HALO_3619 dyn_em 48:a_mut,a_ph_save,a_ru_tend,a_rv_tend,a_t_save,a_t_tend,a_u_save,a_v_save,a_w_save
halo HALO_3632 dyn_em 48:a_mu_2,a_mu_tend,a_mut,a_ph_2,a_ph_save,a_ru,a_rv,a_t_2,a_t_save,a_u_save,a_v_save,a_w_2,a_w_save,mut,ph_2,t_2,w_2
halo HALO_3723 dyn_em 8:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_3728 dyn_em 48:al,alt,mut,muu,muv,p,php,ru,rv,rw,ww
halo HALO_3783 dyn_em 8:a_alt,a_mu_2,a_mut,a_muu,a_muv,a_p,a_php,a_ru,a_ru_tendf,a_rv,a_rv_tendf,a_rw,a_rw_tendf,a_t_1,a_t_tendf,a_u_1,a_v_1,a_w_1,cqu,cqv,cqw,mub,pb,php,rw,t_init,z,z_at_w,a_z,a_z_at_w,xland
halo HALO_3784 dyn_em 80:a_ph_2,a_ph_tend,a_ru_tend,a_rv_tend,a_rw_tend,a_t_2,a_t_tend,a_u_2,a_v_2,a_w_2,al,muu,muv,p,ph_2,phb,ru,rv,u_2,v_2,w_2
halo HALO_3935 dyn_em 8:a_moist_2
halo HALO_3982 dyn_em 8:a_muu,a_muv,a_rw,a_u_2,a_v_2,a_ww,a_mu_2
halo HALO_3997 dyn_em 48:a_al,a_alt,a_moist_2,a_mu_2,a_ph_2,a_ph_tendf,a_php,a_t_2,a_u_2,a_v_2,a_w_2,a_ww
