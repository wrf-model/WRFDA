#!/usr/bin/ksh

# NOTE:  To ensure reproducible results, must use same number of
#        MPI tasks AND nodes for each run.  blocking=unlimited
#        leads to roundoff differences in mpi_allreduce.

# Env variables used here are: GSI_DIR, OB_DIR, RC_DIR

#set -x

export WORK_DIR=$RUN_DIR/working
export GSI_OUTPUT_DIR=${GSI_OUTPUT_DIR:-$FC_DIR/$DATE}

# Set environment variables for NCEP IBM
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes


# Set environment variables for threads
export SPINLOOPTIME=10000
export YIELDLOOPTIME=40000
export AIXTHREAD_SCOPE=S
export MALLOCMULTIHEAP=true
export XLSMPOPTS="parthds=1:spins=0:yields=0:stack=128000000"


# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite

# Create working  directory
rm -rf $WORK_DIR
mkdir -p $WORK_DIR

date

echo "<HTML><HEAD><TITLE>$EXPT gsi</TITLE></HEAD><BODY>"
echo "<H1>$EXPT gsi</H1><PRE>"
echo 'REL_DIR         <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'GSI_DIR         <A HREF="file:'$GSI_DIR'">'$GSI_DIR'</a>' $WRF_VN
echo 'RUN_DIR         <A HREF="file:'$RUN_DIR'">'$RUN_DIR'</a>'
echo 'WORK_DIR        <A HREF="file:'$WORK_DIR'">'$WORK_DIR'</a>'
echo 'DA_FIRST_GUESS  <A HREF="file:'$DA_FIRST_GUESS'">'$DA_FIRST_GUESS'</a>'
echo 'OB_DIR          <A HREF="file:'$OB_DIR/$DATE'">'$OB_DIR/$DATE'</a>'
echo 'GSI_OUTPUT_DIR  <A HREF="file:'$GSI_OUTPUT_DIR'">'$GSI_OUTPUT_DIR'</a>'
echo "DATE            $DATE"
echo "END_DATE        $END_DATE"
echo "FCST_RANGE      $FCST_RANGE"
echo "DOMAINS         $DOMAINS"

# Set guess/analysis (i/o) file format.  Two option are available:  binary or netcdf
#io_format=binary
io_format=netcdf

NETCDF=.false.
FORMAT=binary
if [[ "$io_format" = "netcdf" ]]; then
   NETCDF=.true.
   FORMAT=netcdf
fi

# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export DELTIM=1200

# Specify GSI fixed field and data directories.
FIXGLOBAL=$GSI_DIR/fix

#   ncp is cp replacement, currently keep as /bin/cp
NCP=/bin/cp

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

# BERROR=${BERROR:-${FIXGLOBAL}/nam_regional_glb_berror.f77
# SATANGL=${SATANGL:-${FIXGLOBAL}/global_satangbias.txt}
BERROR=${FIXGLOBAL}/nam_regional_glb_berror.f77
SATANGL=${FIXGLOBAL}/global_satangbias.txt
SATINFO=${FIXGLOBAL}/global_satinfo.txt
RTMFIX=${FIXGLOBAL}/crtm_gfsgsi

RTMEMIS=${RTMFIX}/EmisCoeff/Big_Endian/EmisCoeff.bin
RTMAERO=${RTMFIX}/AerosolCoeff/Big_Endian/AerosolCoeff.bin
RTMCLDS=${RTMFIX}/CloudCoeff/Big_Endian/CloudCoeff.bin
CONVINFO=${FIXGLOBAL}/global_convinfo.txt
OZINFO=${FIXGLOBAL}/global_ozinfo.txt
PCPINFO=${FIXGLOBAL}/global_pcpinfo.txt

OBERROR=${FIXGLOBAL}/prepobs_errtable.global

# cd run directory 
cd $WORK_DIR

#    # Fixed fields
    ln -sf $BERROR   berror_stats
    ln -sf $SATANGL  satbias_angle
    ln -sf $SATINFO  satinfo
    ln -sf $RTMEMIS  EmisCoeff.bin
    ln -sf $RTMAERO  AerosolCoeff.bin
    ln -sf $RTMCLDS  CloudCoeff.bin
    ln -sf $CONVINFO convinfo
    ln -sf $OZINFO   ozinfo
    ln -sf $PCPINFO  pcpinfo
    ln -sf $OBERROR  errtable

#    # CRTM Spectral and Transmittance coefficients
    nsatsen=`cat satinfo | wc -l`
    isatsen=1
    while [[ $isatsen -le $nsatsen ]]; do
       flag=`head -n $isatsen satinfo | tail -1 | cut -c1-1`
       if [[ "$flag" != "!" ]]; then
          satsen=`head -n $isatsen satinfo | tail -1 | cut -f 2 -d" "`
          spccoeff=${satsen}.SpcCoeff.bin
          if  [[ ! -s $spccoeff ]]; then
             ${NPC:-cp} $RTMFIX/SpcCoeff/Big_Endian/$spccoeff $spccoeff
             ${NPC:-cp} $RTMFIX/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ${satsen}.TauCoeff.bin
          fi
       fi
       isatsen=` expr $isatsen + 1 `
    done

# Only need this file for single obs test
 bufrtable=$FIXGLOBAL/prepobs_prep.bufrtable

# Copy executable and fixed files to current working directory
 ln -sf $GSI_DIR/sorc/global_gsi.fd/global_gsi  ./gsi.x
 ln -sf $bufrtable ./prepobs_prep.bufrtable

# Copy observational data to current working directory
   ln -sf $OB_DIR/$DATE/ob.bufr    ./prepbufr
   ln -sf $OB_DIR/$DATE/amsua.bufr ./amsuabufr

# Copy bias correction, sigma, and surface files
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)

## $ncp $datges/${prefixg}.abias              ./satbias_in
## $ncp $datges/${prefixg}.satang             ./satbias_angle
## $ncp $datges/wrfinput_west_mass_d01_bi     ./wrf_inout
 ${NPC:-cp} $DA_FIRST_GUESS   ./wrf_inout

# cp wrf_inout wrf_ges

# Make gsi namelist
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,
   qoption=1,
   gencode=78,factqmin=0.005,factqmax=0.005,deltim=$DELTIM,
   ndat=59,npred=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.true.,diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.true.,
   regional=.true.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   as=0.6,0.6,0.75,0.75,0.75,0.75,1.0,1.0
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.false.,bkgv_rewgtfct=1.5
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   jcterm=.false.,jcdivt=.false.,bamp_ext1=2.5e12,bamp_ext2=5.0e11,
   bamp_int1=2.5e13,bamp_int2=2.5e12,
   $JCOPTS
 /
 &STRONGOPTS
   jcstrong=.false.,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   jcstrong_option=2,baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=180.0,dmesh(2)=145.0,dmesh(3)=240.0,dmesh(4)=160.0,dmesh(5)=180.0,time_window_max=3.0,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',                  dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',                   dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',                   dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='pw',        dplat(04)=' ',         dsis(04)='pw',                  dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='uv',        dplat(05)=' ',         dsis(05)='uv',                  dval(05)=1.0,  dthin(05)=0,
   dfile(06)='prepbufr',  dtype(06)='spd',       dplat(06)=' ',         dsis(06)='spd',                 dval(06)=1.0,  dthin(06)=0,
   dfile(07)='prepbufr',  dtype(07)='dw',        dplat(07)=' ',         dsis(07)='dw',                  dval(07)=1.0,  dthin(07)=0,
   dfile(08)='radarbufr', dtype(08)='rw',        dplat(08)=' ',         dsis(08)='rw',                  dval(08)=1.0,  dthin(08)=0,
   dfile(09)='prepbufr',  dtype(09)='sst',       dplat(09)=' ',         dsis(09)='sst',                 dval(09)=1.0,  dthin(09)=0,
   dfile(10)='gpsrobufr', dtype(10)='gps_ref',   dplat(10)=' ',         dsis(10)='gps_ref',             dval(10)=1.0,  dthin(10)=0,
   dfile(11)='ssmirrbufr',dtype(11)='pcp_ssmi',  dplat(11)='dmsp',      dsis(11)='pcp_ssmi',            dval(11)=1.0,  dthin(11)=-1,
   dfile(12)='tmirrbufr', dtype(12)='pcp_tmi',   dplat(12)='trmm',      dsis(12)='pcp_tmi',             dval(12)=1.0,  dthin(12)=-1,
   dfile(13)='sbuvbufr',  dtype(13)='sbuv2',     dplat(13)='n16',       dsis(13)='sbuv8_n16',           dval(13)=1.0,  dthin(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n17',       dsis(14)='sbuv8_n17',           dval(14)=1.0,  dthin(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n18',       dsis(15)='sbuv8_n18',           dval(15)=1.0,  dthin(15)=0,
   dfile(16)='hirs2bufr', dtype(16)='hirs2',     dplat(16)='n14',       dsis(16)='hirs2_n14',           dval(16)=6.0,  dthin(16)=1,
   dfile(17)='hirs3bufr', dtype(17)='hirs3',     dplat(17)='n16',       dsis(17)='hirs3_n16',           dval(17)=0.0,  dthin(17)=1,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n17',       dsis(18)='hirs3_n17',           dval(18)=6.0,  dthin(18)=1,
   dfile(19)='hirs4bufr', dtype(19)='hirs4',     dplat(19)='n18',       dsis(19)='hirs4_n18',           dval(19)=0.0,  dthin(19)=1,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='metop-a',   dsis(20)='hirs4_metop-a',       dval(20)=6.0,  dthin(20)=1,
   dfile(21)='gsndrbufr', dtype(21)='sndr',      dplat(21)='g11',       dsis(21)='sndr_g11',            dval(21)=0.0,  dthin(21)=1,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g12',       dsis(22)='sndr_g12',            dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gimgrbufr', dtype(23)='goes_img',  dplat(23)='g11',       dsis(23)='imgr_g11',            dval(23)=0.0,  dthin(23)=1,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g12',       dsis(24)='imgr_g12',            dval(24)=0.0,  dthin(24)=1,
   dfile(25)='airsbufr',  dtype(25)='airs',      dplat(25)='aqua',      dsis(25)='airs281SUBSET_aqua',  dval(25)=20.0, dthin(25)=1,
   dfile(26)='msubufr',   dtype(26)='msu',       dplat(26)='n14',       dsis(26)='msu_n14',             dval(26)=2.0,  dthin(26)=2,
   dfile(27)='amsuabufr', dtype(27)='amsua',     dplat(27)='n15',       dsis(27)='amsua_n15',           dval(27)=10.0, dthin(27)=2,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n16',       dsis(28)='amsua_n16',           dval(28)=0.0,  dthin(28)=2,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n17',       dsis(29)='amsua_n17',           dval(29)=0.0,  dthin(29)=2,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n18',       dsis(30)='amsua_n18',           dval(30)=10.0, dthin(30)=2,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='metop-a',   dsis(31)='amsua_metop-a',       dval(31)=10.0, dthin(31)=2,
   dfile(32)='airsbufr',  dtype(32)='amsua',     dplat(32)='aqua',      dsis(32)='amsua_aqua',          dval(32)=5.0,  dthin(32)=2,
   dfile(33)='amsubbufr', dtype(33)='amsub',     dplat(33)='n15',       dsis(33)='amsub_n15',           dval(33)=3.0,  dthin(33)=3,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n16',       dsis(34)='amsub_n16',           dval(34)=3.0,  dthin(34)=3,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n17',       dsis(35)='amsub_n17',           dval(35)=3.0,  dthin(35)=3,
   dfile(36)='mhsbufr',   dtype(36)='mhs',       dplat(36)='n18',       dsis(36)='mhs_n18',             dval(36)=3.0,  dthin(36)=3,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='metop-a',   dsis(37)='mhs_metop-a',         dval(37)=3.0,  dthin(37)=3,
   dfile(38)='ssmitbufr', dtype(38)='ssmi',      dplat(38)='f13',       dsis(38)='ssmi_f13',            dval(38)=0.0,  dthin(38)=4,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f14',       dsis(39)='ssmi_f14',            dval(39)=0.0,  dthin(39)=4,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f15',       dsis(40)='ssmi_f15',            dval(40)=0.0,  dthin(40)=4,
   dfile(41)='amsrebufr', dtype(41)='amsre_low', dplat(41)='aqua',      dsis(41)='amsre_aqua',          dval(41)=0.0,  dthin(41)=4,
   dfile(42)='amsrebufr', dtype(42)='amsre_mid', dplat(42)='aqua',      dsis(42)='amsre_aqua',          dval(42)=0.0,  dthin(42)=4,
   dfile(43)='amsrebufr', dtype(43)='amsre_hig', dplat(43)='aqua',      dsis(43)='amsre_aqua',          dval(43)=0.0,  dthin(43)=4,
   dfile(44)='ssmisbufr', dtype(44)='ssmis_las', dplat(44)='f16',       dsis(44)='ssmis_f16',           dval(44)=0.0,  dthin(44)=4,
   dfile(45)='ssmisbufr', dtype(45)='ssmis_uas', dplat(45)='f16',       dsis(45)='ssmis_f16',           dval(45)=0.0,  dthin(45)=4,
   dfile(46)='ssmisbufr', dtype(46)='ssmis_img', dplat(46)='f16',       dsis(46)='ssmis_f16',           dval(46)=0.0,  dthin(46)=4,
   dfile(47)='ssmisbufr', dtype(47)='ssmis_env', dplat(47)='f16',       dsis(47)='ssmis_f16',           dval(47)=0.0,  dthin(47)=4,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd1',    dplat(48)='g12',       dsis(48)='sndrD1_g12',          dval(48)=1.5,  dthin(48)=5,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd2',    dplat(49)='g12',       dsis(49)='sndrD2_g12',          dval(49)=1.5,  dthin(49)=5,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd3',    dplat(50)='g12',       dsis(50)='sndrD3_g12',          dval(50)=1.5,  dthin(50)=5,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd4',    dplat(51)='g12',       dsis(51)='sndrD4_g12',          dval(51)=1.5,  dthin(51)=5,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd1',    dplat(52)='g11',       dsis(52)='sndrD1_g11',          dval(52)=1.5,  dthin(52)=5,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd2',    dplat(53)='g11',       dsis(53)='sndrD2_g11',          dval(53)=1.5,  dthin(53)=5,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd3',    dplat(54)='g11',       dsis(54)='sndrD3_g11',          dval(54)=1.5,  dthin(54)=5,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd4',    dplat(55)='g11',       dsis(55)='sndrD4_g11',          dval(55)=1.5,  dthin(55)=5,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd1',    dplat(56)='g13',       dsis(56)='sndrD1_g13',          dval(56)=1.5,  dthin(56)=5,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd2',    dplat(57)='g13',       dsis(57)='sndrD2_g13',          dval(57)=1.5,  dthin(57)=5,
   dfile(58)='gsnd1bufr', dtype(58)='sndrd3',    dplat(58)='g13',       dsis(58)='sndrD3_g13',          dval(58)=1.5,  dthin(58)=5,
   dfile(59)='gsnd1bufr', dtype(59)='sndrd4',    dplat(59)='g13',       dsis(59)='sndrD4_g13',          dval(59)=1.5,  dthin(59)=5,

   $OBSINPUT
 /
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB
 /
EOF

  # Run gsi under Parallel Operating Environment (poe) on NCEP IBM
  ##poe hpmcount $tmpdir/gsi.x < gsiparm.anl > stdout
  mpirun.lsf  ./gsi.x < gsiparm.anl > stdout
  RC=$?

# save analysis result in FC_DIR
  ${NPC:-cp} ./wrf_inout $GSI_OUTPUT_DIR/wrfinput_d01
  cd $RUN_DIR
  echo $(date +'%D %T') "Ended $RC"

# rename diagnostics files with a more meaningful name
#
mv $WORK_DIR/fort.201        stats.ps
mv $WORK_DIR/fort.202        stats.uv
mv $WORK_DIR/fort.203        stats.t
mv $WORK_DIR/fort.204        stats.q
mv $WORK_DIR/fort.205        stats.pw
mv $WORK_DIR/fort.206        stats.sbuv
mv $WORK_DIR/fort.207        stats.radiance
mv $WORK_DIR/fort.208        stats.pcp
mv $WORK_DIR/fort.209        stats.rw
mv $WORK_DIR/fort.210        stats.dw
mv $WORK_DIR/fort.211        stats.srw
mv $WORK_DIR/fort.212        stats.gps
mv $WORK_DIR/fort.213        stats.sst
mv $WORK_DIR/fort.220        costfn
mv $WORK_DIR/stdout          stdout
mv $WORK_DIR/gsiparm.anl     gsiparm.anl
mv $WORK_DIR/satbias_in      satbias_in
mv $WORK_DIR/satbias_out     satbias_out

# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#

loops="01 02 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  02) string=int;;
  03) string=anl;;
   *) string=$loop;;
esac

# Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g10 sndr_g12 sndr_g08_prep sndr_g10_prep sndr_g12_prep sndrd1_g08 sndrd2_g08 sndrd3_g08 sndrd4_g08 sndrd1_g10 sndrd2_g10 sndrd3_g10 sndrd4_g10 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua goes_img_g08 goes_img_g10 goes_img_g11 goes_img_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 amsua_n18 mhs_n18 amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16"
   for type in $listall; do
      count=`ls $WORK_DIR/dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat $WORK_DIR/dir.*/${type}_${loop}* > diag_${type}_${string}
      fi
   done
done

rm -f $WORK_DIR/*.bin # clean CRTM coeffs

if $CLEAN; then
   rm -rf $WORK_DIR
fi

echo "</BODY></HTML>"

# End of script
exit
