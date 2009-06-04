#!/bin/csh

# #BSUB -x                                # exlusive use of node (not_shared)
# #BSUB -a mpich_gm                       # at NCAR: lightning
# #BSUB -R "span[ptile=2]"                # how many tasks per node (1 or 2)
#BSUB -R "span[ptile=32]"                # how many tasks per node (up to 8)
#BSUB -n 32                             # number of total tasks
#BSUB -o reg.out                        # output filename (%J to add job id)
#BSUB -e reg.err                        # error filename
#BSUB -J regtest_wrfda                  # job name
#BSUB -q regular                        # queue
#BSUB -W 6:00                          # wallclock time
#BSUB -P 64000510
##BSUB -P 48500053

# QSUB -q ded_4             # submit to 4 proc
# QSUB -l mpp_p=4           # request 4 processors
# QSUB -lT  21600           # max. job time limit is 6 h
# QSUB -lF 250Mw            # max. job file size limit is 250 Megawords
# QSUB -eo                  # merge error and output into one file
# QSUB -o  reg.out          # output file name
# QSUB                      # there are no further QSUB commands

#	This is a script to test the bit-for-bit reproducibility of
#	the WRFDA system, when comparing single processor serial runs to
#	MPI parallel runs.  There are several regression tests
#	that are performed.  Failed comparisons get reported, but don't
#	stop the script.  Failed builds or forecasts force an exit from 
#	the script.

#	Approximate time for completion of full test suite
#		Compaq 733 MHz   ev67 :  2.5 hours (empty)
#		Intel  1.2 GHz (4-pe) :  3.0 hours (empty)
#		IBM            P4     :  2.0 hours (empty)

#	Do we keep running even when there are BAD failures?

set KEEP_ON_RUNNING = FALSE
set KEEP_ON_RUNNING = TRUE

#	These need to be changed for your particular set of runs.  This is
#	where email gets sent.

if ( ( `uname` == AIX ) && ( `hostname | cut -c 1-2` != be ) ) then
	set FAIL_MAIL = ( ${user}@noaa.gov )
	set GOOD_MAIL = ( ${user}@noaa.gov )

	setenv MP_EAGER_LIMIT 65536
	setenv MP_SHARED_MEMORY yes
	setenv MP_SINGLE_THREAD yes
	setenv MP_LABELIO yes
	setenv MP_STDOUTMODE ordered

	setenv OMP_NUM_THREADS 4
	setenv XLSMPOPTS "parthds=4:spins=0:yields=0:stack=128000000:schedule=static"
	setenv AIXTHREAD_SCOPE S
	setenv AIXTHREAD_MNRATIO 1:1
	setenv SPINLOOPTIME 1000
	setenv YIELDLOOPTIME 1000
else
	set FAIL_MAIL = ( ${user}@ucar.edu )
	set GOOD_MAIL = ( ${user}@ucar.edu )
endif

unalias cd cp rm ls pushd popd mv
if ( ( `uname` == Linux ) || ( `uname` == Darwin ) ) alias banner echo

#	Get the command line input

set therevision = "HEAD"
set thefile = "null"
set thedata = "null"
set clrm = 0          # compile local run mmmtmp, for using clsroom cluster and local disk

#	If this is a batch job (NCAR's IBMs or FSL's Intel and Alpha), we need to muck with the "input"
#	parameters a bit.

if      ( ( `uname` == AIX ) || ( `hostname` == tempest ) || ( `hostname | cut -c 1-2` == ln ) ) then
	set argv = ( -here )
	set argv = ( -ftp )
        set argv = ( -r today )
	set argv = ( -env )
	set WRFDAREGFILE = /mmm/users/xinzhang/wrfda.tar
	if ( ( `uname` == AIX ) && ( `hostname | cut -c 1-2` == be ) ) then
		set argv = ( -f /mmm/users/xinzhang/wrfda.tar )
	endif
else if   ( `hostname` == bay-mmm ) then
	set argv = ( -f /users/xinzhang/CODE/wrfda.tar )
else if   ( `hostname` == karri ) then
	set argv = ( -f /karri/users/xinzhang/regtest/wrfda.tar )
	set argv = ( -r HEAD  )
else if   ( `hostname` == envelope2.rap.ucar.edu ) then
	set argv = ( -f /Volumes/d1/yueliu/XIN/regtest/wrfda.tar )
	set FAIL_MAIL = ( xinzhang@ucar.edu )
	set GOOD_MAIL = ( xinzhang@ucar.edu )
endif

#	Where is the input data located - for a few known NCAR/MMM machines.

if      ( `hostname` == karri ) then
	set WRFDAREGDATAEM = /karri/users/xinzhang/regtest/WRFDA-data-EM
	set WRFDAREGDATANMM = /karri/users/xinzhang/regtest/WRFDA-data-NMM
        set CASEOPTS =	( cv3_guo cwb_ascii afwa_t7_ssmi )
        set CASEOPTS =	( cv3_guo cwb_ascii afwa_t7_ssmi t44_prepbufr cwb_ascii_outerloop_rizvi )
else if   ( `hostname` == bay-mmm ) then
	set WRFDAREGDATAEM = /users/xinzhang/CODE/WRFDA-data-EM
	set WRFDAREGDATANMM = /users/xinzhang/CODE/WRFDA-data-NMM
        set CASEOPTS =	( cwb_ascii )
else if   ( `hostname` == envelope2.rap.ucar.edu ) then
	set WRFDAREGDATAEM = /Volumes/d1/yueliu/XIN/regtest/WRFDA-data-EM
	set WRFDAREGDATANMM = /Volumes/d1/yueliu/XIN/regtest/WRFDA-data-NMM
        set CASEOPTS =	( cwb_ascii cv3_guo afwa_t7_ssmi t44_prepbufr )
        set CASEOPTS =	( t44_prepbufr )
else if ( ( `hostname | cut -c 1-2` == be ) || ( `hostname` == tempest ) || ( `hostname | cut -c 1-2` == ln ) ) then
	set WRFDAREGDATAEM = /mmm/users/xinzhang/WRFDA-data-EM
	set WRFDAREGDATANMM = /mmm/users/xinzhang/WRFDA-data-NMM
        set CASEOPTS =	( tutorial_xinzhang cv3_guo t44_liuz radar_meixu cwb_ascii afwa_t7_ssmi t44_prepbufr ASR_prepbufr cwb_ascii_outerloop_rizvi sfc_assi_2_outerloop_guo )
else
	if      ( ( -d /users/gill/WRF-data-EM ) && ( -d /users/gill/WRF-data-NMM ) ) then
		set WRFDAREGDATAEM = /users/gill/WRF-data-EM
	        set WRFDAREGDATANMM = /mmm/users/xinzhang/WRFDA-data-NMM
	else if ( ( -d /mmm/users/gill/WRF-data-EM ) && ( -d /mmm/users/gill/WRF-data-NMM ) ) then
		set WRFDAREGDATAEM = /mmm/users/gill/WRF-data-EM
	        set WRFDAREGDATANMM = /mmm/users/xinzhang/WRFDA-data-NMM
	else
		echo "stick the WRFDA em and nmm data somewhere, and then fill in the shell vars"
		echo "inside this script, you NEED WRFDAREGDATAEM and WRFDAREGDATANMM set"
		exit ( 1 ) 
	endif
endif
#XINZHANG###################################################
echo XINZHANG em data is located at $WRFDAREGDATAEM
ls -ls $WRFDAREGDATAEM
echo XINZHANG nmm data is located at $WRFDAREGDATANMM
ls -ls $WRFDAREGDATANMM
banner 1
#set ans = "$<"
#XINZHANG###################################################

if ( $#argv == 0 ) then
	echo "Please enter either an revision number  for svn export. ex regtest.csh -r number"
	echo " or a file name containing WRFDA. ex regtest.csh -f tarfile"
	echo " or the -ftp flag for the script to pick code off anon ftp"
	exit ( 2 ) 
endif

set theargs = 0
foreach a ( $argv )
	if ( "$a" == "-r" ) then

		svn ls https://svn-wrf-model.cgd.ucar.edu/trunk >& /dev/null
		if ( $status ) then
			echo "Cannot execute svn ls on https://svn-wrf-model.cgd.ucar.edu/trunk, where the"
			echo "WRF and WRFDA code resides."
			echo "Please check that it is up and that you have permission to check out"
			exit 2
		endif
		setenv SVNROOT https://svn-wrf-model.cgd.ucar.edu/trunk
		
		set acquire_from = "svn"
		set therevision = $argv[2]

	endif

	if ( "$a" == "-f" ) then

		set thefile = $argv[2]
		#	Check for absolute path, if not, make it absolute
		echo $thefile | grep '^/' > /dev/null
		if ( $status != 0 ) set thefile = `pwd`/$thefile
		set acquire_from = "filearg"

	endif

	if ( "$a" == "-ftp" ) then
		set acquire_from = "ftp"
                echo "anon ftp temporarily disabled"
                exit ( 3 )
	endif

	if ( "$a" == "-here" ) then
		set acquire_from = "here"
	endif

	if ( "$a" == "-env" ) then
		set acquire_from = "environment"
		set thefile = $WRFDAREGFILE
	endif
end

#	Start recording everything - for debug purposes.

set echo 
set date

#	And to tell us how long we've spent on this whole regression test,
#	we should remember when we started.

set start = ( `date` )

#####################################################################

#	Initial set up values

#	The default floating point precision is either 4 bytes or 8 bytes.
#	We assume that it is 4 (or the default for the architecture) unless 
#	REAL8 is set to TRUE.

set REAL8 = FALSE
set REAL8 = TRUE

#	Are we shooting for a bit-for-bit run (serial vs MPI), or not?
#	If you want to do a performance-only run, the forecasts are still short, but you
#	get to insure that the optimized code builds and runs.

set REG_TYPE = OPTIMIZED
set REG_TYPE = BIT4BIT

#	For a Mac/Intel, we can run either g95 or PGI.

if ( `hostname` == bay-mmm ) then
	set LINUX_COMP = PGI
else if ( `hostname` == karri ) then
	set LINUX_COMP = PGI
else if ( `hostname` == envelope2.rap.ucar.edu ) then
	set LINUX_COMP = G95
endif

#	A separately installed version of the latest ESMF library (NOT the 
#	ESMF library included in the WRF tarfile) can be tested by setting 
#	"ESMF_LIB" to "TRUE" below.  This test is not supported on all 
#	machines.  

set ESMF_LIB = TRUE
set ESMF_LIB = FALSE

# serial and OMP are not tested with ESMF so always start with env vars cleared
unsetenv ESMFLIB
unsetenv ESMFINC

if ( $ESMF_LIB == TRUE ) then
	if      ( ( `uname` == AIX ) && ( `hostname | cut -c 1-2` == be ) ) then
		echo "A separately installed version of the latest ESMF library"
		echo "(NOT the ESMF library included in the WRF tarfile) will"
		echo "be used for MPI tests"
		setenv OBJECT_MODE 64
#	set ESMFLIBSAVE = /home/bluevista/hender/esmf/esmf_2_2_2r/lib/libO/AIX.default.64.mpi.default
#	set ESMFINCSAVE = /home/bluevista/hender/esmf/esmf_2_2_2r/mod/modO/AIX.default.64.mpi.default
		setenv ESMF_DIR /mmm/users/michalak/esmf
		setenv ESMF_BOPT g
		setenv ESMF_ABI 64
		setenv ESMF_INSTALL_PREFIX $ESMF_DIR/../esmf_install
		setenv ESMFLIB $ESMF_INSTALL_PREFIX/lib/libg/AIX.default.64.mpi.default
		setenv ESMFINC $ESMF_INSTALL_PREFIX/mod/modg/AIX.default.64.mpi.default
		set ESMFLIBSAVE = $ESMFLIB
		set ESMFINCSAVE = $ESMFINC
		echo "Using ESMFLIB = ${ESMFLIBSAVE}"
		echo "Using ESMFINC = ${ESMFINCSAVE}"
	else
		echo "Only the ESMF library included in the WRF tarfile is"
		echo "tested on this machine"
		exit ( 3 ) 
	endif
endif

#	A single WRF output "quilt" server can be tested by setting "QUILT"  to 
#       "TRUE" below.  At the moment, testing of I/O quilt servers is not supported 
#       on all machines.  

set QUILT = TRUE
set QUILT = FALSE

if ( $QUILT == TRUE ) then
	echo "One WRF output quilt server will be used for some tests"
endif

#	Baseline data sets can be generated and archived or compared against.  
#       - To generate and archive, set GENERATE_BASELINE to a pathname that can 
#         be created by this script via "mkdir -p $GENERATE_BASELINE".  This 
#         directory must not already exist.  
#         Set GENERATE_BASELINE = FALSE to avoid baseline generation.  
#       - To compare with a previously archived baseline, set COMPARE_BASELINE 
#         to an existing directory that contains an archived baseline.  
#         Set COMPARE_BASELINE = FALSE to avoid baseline comparison.  
set COMPARE_BASELINE = FALSE
set COMPARE_BASELINE = '/ptmp/xinzhang/BASELINE'
set GENERATE_BASELINE = '/ptmp/xinzhang/BASELINE'
set GENERATE_BASELINE = FALSE
if   ( `hostname` == envelope2.rap.ucar.edu ) then
     set GENERATE_BASELINE = '/Volumes/d1/yueliu/XIN/regtest/BASELINE'
     set COMPARE_BASELINE = '/Volumes/d1/yueliu/XIN/regtest/BASELINE'
     set COMPARE_BASELINE = FALSE
     set GENERATE_BASELINE = FALSE
endif

#	Baseline generation and comparison are only done when BIT4BIT is set.  
if ( $GENERATE_BASELINE != FALSE ) then
	if ( $REG_TYPE != BIT4BIT ) then
		echo "ERROR:  Baseline generation can only be done during BIT4BIT tests."
		exit ( 3 ) 
	endif
	if ( -d $GENERATE_BASELINE ) then
		echo "ERROR:  Baseline directory ${GENERATE_BASELINE} already exists."
		exit ( 3 ) 
	else
		# Archive serial output file to baseline
		mkdir -p $GENERATE_BASELINE || ( echo "ERROR:  cannot mkdir ${GENERATE_BASELINE}"; exit 3 )
	endif
endif
if ( $COMPARE_BASELINE != FALSE ) then
	if ( $REG_TYPE != BIT4BIT ) then
		echo "Baseline comparison can only be done during BIT4BIT tests."
		exit ( 3 ) 
	endif
	if ( ! -d $COMPARE_BASELINE ) then
		echo "${0}: ERROR:  Baseline directory ${COMPARE_BASELINE} does not exist"
		exit ( 3 ) 
	endif
endif

#	Set the input/output format type (currently 1, 2 or 5 OK).
#	Binary		NetCDF				PHDF, IBM	GriB, history only
#	1		2		3		4		5

set IO_FORM = 2
set IO_FORM_NAME = ( io_bin io_netcdf io_dummy io_phdf5 io_grib1 )
set IO_FORM_WHICH =( IO     IO        IO       IO       O        )

set CUR_DIR = `pwd`

#XINZHANG###################################################
banner 2
#set ans = "$<"
#XINZHANG###################################################

#####################################################################

#	Set up info for particular architectures

set ARCH = ( `uname` )

set ZAP_SERIAL          = FALSE
set ZAP_OPENMP          = FALSE
set SERIALRUNCOMMAND	= 
set OMPRUNCOMMAND	= 
set MPIRUNCOMMANDPOST   = 

touch version_info
if ( $ARCH[1] == AIX ) then
	set DEF_DIR             = $home
	set TMPDIR              = /ptmp/$user
	# keep stuff out of $HOME and /ptmp/$USER
	# this allows multiple regressions tests to run simultaneously
	# extend this to other machines later
	if   ( `hostname | cut -c 1-2` == be ) then
		set job_id              = $LSB_JOBID
		set DEF_DIR             = /ptmp/$user/wrfda_regression.${job_id}
		set TMPDIR              = $DEF_DIR
		if ( -d $DEF_DIR ) then
			echo "${0}: ERROR::  Directory ${DEF_DIR} exists, please remove it"
			exit ( 1 ) 
		else
			mkdir -p $DEF_DIR
			echo "See ${DEF_DIR}/wrfdatest.output and other files in ${DEF_DIR} for test results"
		endif
	else if ( ( `hostname | cut -c 1-2` != be ) && ( ! $?LOADL_JOB_NAME ) ) then
		echo "${0}: ERROR::  This batch script must be submitted via"
		echo "${0}:          LoadLeveler on an AIX machine\!"
		exit
	else if   ( ( `hostname | cut -c 1-2` != be ) ) then
		set job_id              = `echo ${LOADL_JOB_NAME} | cut -f2 -d'.'`
		set DEF_DIR             = /ptmp/$user/wrfda_regression.${job_id}
		set TMPDIR              = $DEF_DIR
		if ( -d $DEF_DIR ) then
			echo "${0}: ERROR::  Directory ${DEF_DIR} exists, please remove it"
			exit ( 1 ) 
		else
			mkdir -p $DEF_DIR
			echo "See ${DEF_DIR}/wrfdatest.output and other files in ${DEF_DIR} for test results"
		endif
		set CUR_DIR = ${LOADL_STEP_INITDIR}
	endif
	if ( ! -d $TMPDIR ) mkdir $TMPDIR
	set MAIL                = /usr/bin/mailx
	set COMPOPTS    = ( 1 2 3 )
	set Num_Procs		= 32
        set OPENMP              = $Num_Procs
        setenv MP_PROCS  $Num_Procs
        setenv MP_RMPOOL 1
	if ( `hostname | cut -c 1-2` == be ) then
		set MPIRUNCOMMAND       =  /contrib/mpiruns/be/mpirun.lsf
	else if ( `hostname | cut -c 1-2` != be ) then
		set MPIRUNCOMMAND       =  poe
	endif
# check compiler version, JM
        lslpp -i | grep xlf | grep ' xlfcmp ' | head -1
        set xlfvers=`lslpp -i | grep xlf | grep ' xlfcmp ' | head -1 | awk '{print $2}' | sed 's/\...*$//'`
# end of compiler check, JM
	echo "Compiler version info: " >! version_info
	echo "FORTRAN:        " `lslpp -l | grep xlfrte | head -1 | awk '{print $1 "   " $2}'` >>! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	echo "AIX:            " `lslpp -l | grep bos.mp | head -1 | awk '{print $1 "   " $2}'` >>! version_info
	echo " " >>! version_info
	setenv MP_SHARED_MEMORY yes
else if ( ( $ARCH[1] == Darwin ) && ( `hostname` == envelope2.rap.ucar.edu ) ) then
        set DEF_DIR             = /Volumes/d1/yueliu/XIN/regtest/wrfda_regression
        if ( -d $DEF_DIR ) then
                echo "${0}: ERROR::  Directory ${DEF_DIR} exists, please remove it"
                exit ( 1 )
        else
                mkdir -p $DEF_DIR
                echo "See directory ${DEF_DIR}/ for wrfdatest.output and other test results"
        endif
	set TMPDIR              = $DEF_DIR
	set MAIL		= /usr/bin/mailx
	if      ( $LINUX_COMP == PGI ) then
		set COMPOPTS	= ( 1 2 )
	else if ( $LINUX_COMP == G95 ) then
		set COMPOPTS	= ( 7 8 )
	endif
	set Num_Procs		= 4
	cat >! `pwd`/machfile << EOF
`hostname`
`hostname`
`hostname`
`hostname`
EOF
        set Mach = `pwd`/machfile
	set SERIALRUNCOMMAND	= 
	echo "Compiler version info: " >! version_info
	if      ( $LINUX_COMP == PGI ) then
		set MPIRUNCOMMAND 	= ( /Volumes/d1/yueliu/XIN/external/g95/mpich2-1.0.7/bin/mpirun -np $Num_Procs )
		pgf90 -V | head -2 | tail -1 >>&! version_info
	else if ( $LINUX_COMP == G95 ) then
		set MPIRUNCOMMAND 	= ( /Volumes/d1/yueliu/XIN/external/g95/mpich2-1.0.7/bin/mpirun -np $Num_Procs )
		g95 -v |& grep gcc >>&! version_info
	endif
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
	ps -A | grep mpd | grep -v grep >& /dev/null
	set ok = $status
	if ( $ok != 0 ) then
		echo starting an mpd process
		mpd &
	endif
else if ( ( $ARCH[1] == Linux ) && ( `hostname` == bay-mmm ) ) then
	set DEF_DIR	= /mmmtmp/${user}/`hostname`
	if ( ! -d $DEF_DIR ) mkdir $DEF_DIR
	set TMPDIR		= .
	set MAIL		= /bin/mail
	if      ( $LINUX_COMP == PGI ) then
		set COMPOPTS	= ( 5 6 )
	else if ( $LINUX_COMP == INTEL ) then
		set COMPOPTS	= ( 7  9 11 )
	endif
	set Num_Procs		= 4
	set OPENMP		= $Num_Procs
	cat >! machfile << EOF
`hostname`
`hostname`
`hostname`
`hostname`
EOF
	set Mach		= `pwd`/machfile
	if ( $LINUX_COMP == INTEL ) then
		set ZAP_OPENMP		= TRUE
	endif
	set MPIRUNCOMMAND       = ( mpirun -np $Num_Procs -machinefile $Mach )
	set MPIRUNCOMMAND       = ( mpirun -np $Num_Procs )
	echo "Compiler version info: " >! version_info
	if      ( $LINUX_COMP == PGI ) then
		pgf90 -V >>&! version_info
	else if ( $LINUX_COMP == INTEL ) then
		ifort -v >>&! version_info
	endif
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname | cut -c 1-2` ==  ln ) ) then
	set DEF_DIR	= /ptmp/${user}/wrf_regtest
	if ( ! -d $DEF_DIR ) mkdir $DEF_DIR
	set TMPDIR		= .
	set MAIL		= /bin/mail
	if      ( $LINUX_COMP == PGI ) then
		set COMPOPTS    = ( 4 2 3 )
	endif
	set Num_Procs		= 4
	set OPENMP		= 2
	cat >! machfile << EOF
`hostname`
`hostname`
`hostname`
`hostname`
EOF
	set Mach		= `pwd`/machfile
	set ZAP_OPENMP		= TRUE
	set MPIRUNCOMMAND       =  mpirun.lsf
	echo "Compiler version info: " >! version_info
	if      ( $LINUX_COMP == PGI ) then
		pgf90 -V >>&! version_info
	else if ( $LINUX_COMP == INTEL ) then
		ifort -v >>&! version_info
	endif
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname` == karri ) ) then
	set job_id              = $$
	set DEF_DIR             = /karri/users/${user}/regtest/wrfda_regression
	set TMPDIR              = $DEF_DIR
	if ( -d $DEF_DIR ) then
		echo "${0}: ERROR::  Directory ${DEF_DIR} exists, please remove it"
		exit ( 1 ) 
	else
		mkdir -p $DEF_DIR
		echo "See directory ${DEF_DIR}/ for wrfdatest.output and other test results"
	endif
	set MAIL		= /bin/mail
	if      ( $LINUX_COMP == PGI ) then
		set COMPOPTS	= ( 1 2 )
	else if ( $LINUX_COMP == GFORTRAN ) then
		set COMPOPTS	= ( 7 8 )
	endif
	set Num_Procs		= 4
	set OPENMP		= $Num_Procs
	cat >! machfile << EOF
`hostname`
`hostname`
`hostname`
`hostname`
EOF
	set Mach		= `pwd`/machfile
	if ( $LINUX_COMP == INTEL ) then
		set ZAP_OPENMP		= TRUE
	endif
	set MPIRUNCOMMAND       = ( mpirun -np $Num_Procs -machinefile $Mach )
	set MPIRUNCOMMAND       = ( mpirun -np $Num_Procs )
	echo "Compiler version info: " >! version_info
	if      ( $LINUX_COMP == PGI ) then
		pgf90 -V >>&! version_info
	else if ( $LINUX_COMP == INTEL ) then
		ifort -v >>&! version_info
	endif
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( `hostname` == tempest ) then
	set DEF_DIR	= /ptmp/${user}/wrf_regtest.${QSUB_REQID}
	if ( ! -d $DEF_DIR ) mkdir $DEF_DIR
	set TMPDIR		= .
	set MAIL		= /usr/sbin/Mail
	set COMPOPTS		= ( 1 2 3 )
	set Num_Procs		= 2
	set OPENMP		= $Num_Procs
	set Mach		= `pwd`/machfile
	set ZAP_OPENMP		= TRUE
	set MPIRUNCOMMAND       = ( mpirun -np $Num_Procs )
	echo "Compiler version info: " >! version_info
	f90 -version >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname` == master ) ) then
	set DEF_DIR		= /big6/gill/DO_NOT_REMOVE_DIR
	set TMPDIR		= .
	set MAIL		= /bin/mail
	if      ( $LINUX_COMP == PGI ) then
		set COMPOPTS	= ( 1 3 6 )
	else if ( $LINUX_COMP == INTEL ) then
		set COMPOPTS	= ( 7  9 11 )
	endif
	set Num_Procs		= 4
	set OPENMP		= 2
	cat >! machfile << EOF
node3
node3
node4
node4
EOF
	set Mach		= `pwd`/machfile
	if ( $LINUX_COMP == INTEL ) then
		set ZAP_OPENMP		= TRUE
	endif
	set MPIRUNCOMMAND       = ( mpirun -v -np $Num_Procs -machinefile $Mach -nolocal )
	set MPIRUNCOMMANDPOST   = "< /dev/null"
	echo "Compiler version info: " >! version_info
	if      ( $LINUX_COMP == PGI ) then
		pgf90 -V >>&! version_info
	else if ( $LINUX_COMP == INTEL ) then
		ifort -v >>&! version_info
	endif
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else
	echo "Unrecognized architecture for regression test"  >! error_message
	echo `uname`                                          >> error_message
	echo `hostname`                                       >> error_message
	$MAIL -s "Unknown architecture $ARCH[1] " $FAIL_MAIL   < error_message
	exit ( 1 )
endif

#####################################################################
#XINZHANG###################################################
echo did the arch specific stuff
banner 3
#set ans = "$<"
#XINZHANG###################################################

#	First of all, in which particular directory do we start.

cd $DEF_DIR

#	We want to keep the old regression stuff around

if ( -d regression_test ) then
	if ( -d regression_test.old ) then
		/bin/rm -fr regression_test.old
	endif
	/bin/mv regression_test regression_test.old
endif

#	Go to the regression test directory

mkdir regression_test
set ok = $status
if ( $ok != 0 ) then
	echo "Gee, I cannot make a directory in $DEF_DIR"  >! error_message
	echo `pwd`                                         >> error_message
	echo `\ls -ls`                                     >> error_message
	$MAIL -s "$DEF_DIR not writable $ARCH[1] " $FAIL_MAIL < error_message
	exit ( 1 )
else
	pushd regression_test
endif

if      ( $acquire_from == "svn" ) then

	#	Checkout the most recent version of WRFDA from the NCAR subversion repository,
	#	and pick up the required input data from the anonymous ftp site.

	svn -r $therevision export $SVNROOT WRFDA
	find ./WRFDA -exec touch \{\} \;
#	ftp -n ftp.ucar.edu < ftp_script_data


else if ( $acquire_from == "filearg" ) then

	#	A tar file of the WRF source was provided, so that is used, along with 
	#	the required input data files from the ftp site.

	tar xvf $thefile
	cd WRFDA
        set therevision = (`svnversion`)
	clean -a
	cd ..
#	ftp -n ftp.ucar.edu < ftp_script_data

else if ( $acquire_from == "environment" ) then

	#	A tar file of WRF is assumed to be available.

	tar xvf $thefile

endif

##XINZHANG###################################################
banner 4
#set ans = "$<"
#XINZHANG###################################################

#	John-specific stuff for maple is the else; part of the "using service machines".

if ( ! $clrm ) then
	pushd WRFDA
else
	if ( ! -d $TMPDIR ) then
		echo something wrong 1
	endif
	if ( ! -d $TMPDIR/RUN ) then
		mkdir $TMPDIR/RUN
		/bin/rm -fr $TMPDIR/RUN/*
	endif
	if ( -d $TMPDIR/RUN ) then
		tar cf - ./WRFV3/test ./WRFV3/main | ( cd $TMPDIR/RUN ; tar xvf - )
		pushd WRFV3
	else
		echo something wrong 2
		exit
	endif
endif

#	Here we initialize our output message.

if ( -e ${DEF_DIR}/wrfdatest.output ) rm ${DEF_DIR}/wrfdatest.output
echo "Architecture $ARCH[1]      machine: `hostname`" >>! ${DEF_DIR}/wrfdatest.output
echo "WRFDA source from: $acquire_from " >>! ${DEF_DIR}/wrfdatest.output
echo "WRFDA version: $therevision " >>! ${DEF_DIR}/wrfdatest.output
echo "Number of OpenMP processes to use: $OPENMP" >>! ${DEF_DIR}/wrfdatest.output
echo "Number of MPI    processes to use: $Num_Procs" >>! ${DEF_DIR}/wrfdatest.output
if ( $ARCH[1] == Darwin ) then
	set name = `finger $user | grep "Name:" | awk '{print $4 " " $5}'`
else
	set name = ( `grep ^${user}: /etc/passwd | cut -d: -f5` ) 
endif
echo "Test conducted by $name" >>! ${DEF_DIR}/wrfdatest.output
echo " " >>! ${DEF_DIR}/wrfdatest.output
echo "Test run from directory ${CUR_DIR}" >>! ${DEF_DIR}/wrfdatest.output
echo " " >>! ${DEF_DIR}/wrfdatest.output
if ( $?LOADL_JOB_NAME ) then
	echo "Loadlever job name = ${LOADL_JOB_NAME}" >>! ${DEF_DIR}/wrfdatest.output
	echo " " >>! ${DEF_DIR}/wrfdatest.output
endif
if ( $REG_TYPE == BIT4BIT ) then
	echo "This is a bit-wise (traditional) regression test. " >>! ${DEF_DIR}/wrfdatest.output
	echo " " >>! ${DEF_DIR}/wrfdatest.output
else if ( $REG_TYPE == OPTIMIZED ) then
	echo "This is a fully optimized regression test. " >>! ${DEF_DIR}/wrfdatest.output
	echo "No inter-comparisons are made. " >>! ${DEF_DIR}/wrfdatest.output
	echo " " >>! ${DEF_DIR}/wrfdatest.output
endif
if ( $REAL8 == TRUE ) then
	echo "Floating point precision is 8-bytes" >>! ${DEF_DIR}/wrfdatest.output
	echo " " >>! ${DEF_DIR}/wrfdatest.output
endif
if ( $ESMF_LIB == TRUE ) then
	echo "A separately installed version of the latest ESMF library" >>! ${DEF_DIR}/wrfdatest.output
	echo "(NOT the ESMF library included in the WRF tarfile) will" >>! ${DEF_DIR}/wrfdatest.output
	echo "be used for MPI tests" >>! ${DEF_DIR}/wrfdatest.output
	echo " " >>! ${DEF_DIR}/wrfdatest.output
endif
if ( $QUILT == TRUE ) then
	echo "One WRF output quilt server will be used for some tests" >>! ${DEF_DIR}/wrfdatest.output
	echo " " >>! ${DEF_DIR}/wrfdatest.output
endif
if ( $GENERATE_BASELINE != FALSE ) then
	echo "WRFDA output will be archived in baseline directory ${GENERATE_BASELINE} for some tests" >>! \
	     ${DEF_DIR}/wrfdatest.output
	echo " " >>! ${DEF_DIR}/wrfdatest.output
endif
if ( $COMPARE_BASELINE != FALSE ) then
	echo "WRFDA output will be compared with files in baseline directory ${COMPARE_BASELINE} for some tests" >>! \
	     ${DEF_DIR}/wrfdatest.output
	echo " " >>! ${DEF_DIR}/wrfdatest.output
endif
echo "The selected I/O option is $IO_FORM ($IO_FORM_NAME[$IO_FORM])" >>! ${DEF_DIR}/wrfdatest.output
if      ( $IO_FORM_WHICH[$IO_FORM] == IO ) then
	echo "This option is for both input and history files" >>! ${DEF_DIR}/wrfdatest.output
	echo " " >>! ${DEF_DIR}/wrfdatest.output
else if ( $IO_FORM_WHICH[$IO_FORM] == I  ) then
	echo "This option is for input files only" >>! ${DEF_DIR}/wrfdatest.output
	echo " " >>! ${DEF_DIR}/wrfdatest.output
else if ( $IO_FORM_WHICH[$IO_FORM] ==  O ) then
	echo "This option is for history files only" >>! ${DEF_DIR}/wrfdatest.output
	echo " " >>! ${DEF_DIR}/wrfdatest.output
endif

cat ${CUR_DIR}/version_info >>! ${DEF_DIR}/wrfdatest.output

#	There is only one WRFDA executables to be considered that can run in serial and
#	distributed memory. 

set first_time_in = TRUE
set CORES = wrfda
foreach core ( $CORES )
#XINZHANG###################################################
echo doing core $core
banner 5
#set ans = "$<"
#XINZHANG###################################################

	#	Cores to test.

	setenv WRF_EM_CORE	1
	setenv WRF_NMM_CORE	0
	setenv WRF_COAMPS_CORE	0
	setenv WRF_EXP_CORE	0

	#	Here we are looping over all of the various compilation configurations,
	#	such as serial only, MPI only, etc.  Each architecture
	#	has its own list of these options.  We build each of the executables for
	#	this particular ${core}.
	
	foreach compopt ( $COMPOPTS )
#XINZHANG###################################################
echo doing compile option $compopt
banner 6
#set ans = "$<"
#XINZHANG###################################################

		if ( `uname` == AIX ) goto BUILD_REGARDLESS
	
		#	Did we already build this one?

		if  ( $core == wrfda )  then
                if      ( ( -e var/build/da_wrfvar.exe.$compopt ) && \
                          ( -e var/obsproc/obsproc.exe ) && \
                          ( -e ${DEF_DIR}/regression_test/WRFDA/var/build/diffwrf ) ) then
			goto GOT_THIS_EXEC
		endif

		BUILD_REGARDLESS:
	
		#	The WRF configuration file works with a single integer
		#	input, which is the compiler option.  By convention, option $COMPOPTS[1] is
		#	serial, $COMPOPTS[2] is OMP, and $COMPOPTS[3] is MPI.

		#	Print info about use of separately installed ESMF library.  
		set esmf_lib_str = " - - - - - - - - - - - - - "
		if ( $ESMF_LIB == TRUE ) then
			# only test ESMF with MPI
			if ( $compopt == $COMPOPTS[3] ) then
				echo "A separately installed version of the latest ESMF library" >>! ${DEF_DIR}/wrfdatest.output
				echo "(NOT the ESMF library included in the WRF tarfile) is" >>! ${DEF_DIR}/wrfdatest.output
				echo "being used for this test of $core parallel $compopt..." >>! ${DEF_DIR}/wrfdatest.output
				set esmf_lib_str = "using separate ESMF library"
				echo "Setting ESMFLIB = ${ESMFLIBSAVE}" >>! ${DEF_DIR}/wrfdatest.output
				echo "Setting ESMFINC = ${ESMFINCSAVE}" >>! ${DEF_DIR}/wrfdatest.output
				setenv ESMFLIB $ESMFLIBSAVE
				setenv ESMFINC $ESMFINCSAVE
			else
				unsetenv ESMFLIB
				unsetenv ESMFINC
			endif
		endif
	
#XINZHANG###################################################
echo start build mechanism
banner 7
#set ans = "$<"
#XINZHANG###################################################
		./clean -a

		#	Edit build command for either bit-wise comparison or full optimization.

		if ( $REG_TYPE == BIT4BIT ) then
			set DEBUG_FLAG = -d
		else
			set DEBUG_FLAG =
		endif

		#	Edit build command.  

		if ( `uname` == AIX ) source var/build/setup.csh
		./configure $DEBUG_FLAG wrfda << EOF
$compopt
EOF
	
		#	The configure.wrf file needs to be adjusted as to whether we are requesting real*4 or real*8
		#	as the default floating precision.

#		if ( $REAL8 == TRUE ) then
#			sed -e '/^RWORDSIZE/s/\$(NATIVE_RWORDSIZE)/8/'  configure.wrf > ! foo ; /bin/mv foo configure.wrf
#		endif

                if ((`uname` == AIX) && ($REG_TYPE == BIT4BIT)) then
                     sed -e '/^LDFLAGS_LOCAL/s/-lmass -lmassv//' \
                         -e '/^ARCH_LOCAL/s/-DNATIVE_MASSV//' \
                         configure.wrf > ! foo; /bin/mv foo configure.wrf
                endif
	
		#	Save the configure file.

		cp configure.wrf configure.wrf.core=${core}_build=${compopt}

#XINZHANG###################################################
echo configure built with optim mods removed, ready to compile
banner 8
#set ans = "$<"
#XINZHANG###################################################
	
		# The WRF_SRC_ROOT_DIR hack is only used by the OSF1 build.  
		# It works around the annoying fact that in OSF1 $(PWD) does 
		# not change during execution of regtest.csh, despite the "cd" 
		# and "pushd" commands.  
		setenv WRF_SRC_ROOT_DIR "${DEF_DIR}/regression_test/WRFDA"
		#	Build this executable
		
		./compile all_wrfvar >&! compile_${core}_build=${compopt}.log
#XINZHANG###################################################
echo compile done
banner 9
#set ans = "$<"
#XINZHANG###################################################
	
		#	Did the compile work?  Check the expected executable names and locations.

		set ok = $status
                if ( ! -x var/build/da_wrfvar.exe ) set ok = 1

                if ( ! -x var/build/diffwrf ) set ok = 1

		if ( $ok != 0 ) then
			echo "SUMMARY compilation    for $core           parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrfdatest.output
			$MAIL -s "REGRESSION FAILURE $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrfdatest.output
			exit ( 3 )
		else
			echo "SUMMARY compilation    for $core           parallel $compopt $esmf_lib_str PASS" >>! ${DEF_DIR}/wrfdatest.output
			echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrfdatest.output
			mv var/build/da_wrfvar.exe var/build/da_wrfvar.exe.$compopt
#XINZHANG###################################################
echo exec exists
ls -ls var/build/*.exe*
banner 10
#set ans = "$<"
#XINZHANG###################################################
		endif

		GOT_THIS_EXEC:

		if ( $clrm ) then
			cp var/build/*exe* $TMPDIR/RUN/WRFDA/var/da
		endif
	
	end

	if ( $clrm ) then
		pushd $TMPDIR/RUN/WRFDA
	endif
	
	#	We have all of the executables built, now we run WRFDA.  This is a loop
	#	over all of the various experiment options for this particular
	#	${core}.  Inside the experiment loop, we loop over the parallel options.
	#	This allows us to use the same input files for each of the parallel
	#	choices for a single experiment loop index.

	foreach case_option ( $CASEOPTS )
#XINZHANG###################################################
echo which case option $case_option
banner 11
#set ans = "$<"
#XINZHANG###################################################
	
		#	For each of the executables, we need to run several experiment
		#	options.  

		if (  $core == wrfda )  then

                        ln -fs $WRFDAREGDATAEM/share var/test/.
			foreach compopt ( $COMPOPTS )
#XINZHANG###################################################
echo compopt = $compopt
banner 12
#set ans = "$<"
#XINZHANG###################################################

				#
				#	Create the correct directory for cases.
				#

                                mkdir -p var/test/$case_option
				pushd var/test/$case_option

                                ln -fs $WRFDAREGDATAEM/$case_option/* .

#XINZHANG###################################################
echo built  directory var/test/$case_option for $case_option
banner 13
#set ans = "$<"
#XINZHANG###################################################
                                if ( `uname` != AIX ) then 
                                   set bufr_list=(`ls *.bufr`)
                                   foreach bufr_file ( $bufr_list )
                                      if ( -e $bufr_file) then
                                         ls -la $bufr_file
                                         cwordsh.sh unblk $bufr_file $bufr_file.unblk >& /dev/null
                                         cwordsh.sh block $bufr_file.unblk $bufr_file.block >& /dev/null
                                         rm -f $bufr_file.unblk
                                         ln -fs $bufr_file.block $bufr_file
                                         ls -la $bufr_file
                                      endif
                                   end
                                endif
#XINZHANG###################################################
echo link of data files, we push them elsewhere
ls -ls *
banner 14
#set ans = "$<"
#XINZHANG###################################################

				#	Run the analysis for this core, case and parallel option

				rm $TMPDIR/wrfvar_output.${case_option}.$compopt >& /dev/null
				
				if      ( $compopt == $COMPOPTS[1] ) then
                                        setenv OMP_NUM_THREADS 1
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=1"
					endif
                                        if   ( (`hostname` == envelope2.rap.ucar.edu) && ($case_option == afwa_t7_ssmi) ) then
#XINZHANG######### afwa_t7_ssmi can not run with single processor due to memory limit.
					     mpirun -np 2 ../../build/da_wrfvar.exe.$COMPOPTS[3] $MPIRUNCOMMANDPOST
                                             mv rsl.out.0000 print.out.wrfda_Case=${case_option}_Parallel=${compopt}
                                        else
                                             set start_seconds = (`date +%s`)
					     ../../build/da_wrfvar.exe.$compopt >! print.out.wrfda_Case=${case_option}_Parallel=${compopt}
                                             set end_seconds = (`date +%s`)
                                             @ walltime_seconds = $end_seconds - $start_seconds
                                        endif
                                else if ( $compopt == $COMPOPTS[2] ) then
                                        setenv OMP_NUM_THREADS $OPENMP
                                        if ( `uname` == AIX ) then
                                                setenv XLSMPOPTS "parthds=${OPENMP}:spins=0:yields=0:stack=128000000:schedule=static"
                                        endif
                                        set start_seconds = (`date +%s`)
					../../build/da_wrfvar.exe.$compopt >! print.out.wrfda_Case=${case_option}_Parallel=${compopt}
                                        set end_seconds = (`date +%s`)
                                        @ walltime_seconds = $end_seconds - $start_seconds
				else if ( $compopt == $COMPOPTS[3] ) then
					setenv OMP_NUM_THREADS 1
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=1"
					endif
                                        set start_seconds = (`date +%s`)
					$MPIRUNCOMMAND ../../build/da_wrfvar.exe.$compopt $MPIRUNCOMMANDPOST
                                        set end_seconds = (`date +%s`)
                                        @ walltime_seconds = $end_seconds - $start_seconds
					mv rsl.out.0000 print.out.wrfda_Case=${case_option}_Parallel=${compopt}
				endif
#XINZHANG###################################################
echo ran wrfda compopt = $compopt
banner 17
#set ans = "$<"
#XINZHANG###################################################

				grep "WRF-Var completed successfully" print.out.wrfda_Case=${case_option}_Parallel=${compopt}
				set success = $status

				#	Did making the analysis work, by that, we mean "is there an output file created".

				if ( ( -e wrfvar_output ) && ( $success == 0 ) ) then
					if      ( $IO_FORM_NAME[$IO_FORM] == io_netcdf ) then
						set found_nans = 1
						if      ( `uname` == AIX   ) then
							ncdump wrfvar_output | grep NaN >& /dev/null
							set found_nans = $status
						else if ( `uname` == OSF1  ) then
							ncdump wrfvar_output | grep nan >& /dev/null
					#		set found_nans = $status
						else if ( `uname` == Linux ) then
							ncdump wrfvar_output | grep nan >& /dev/null
							set found_nans = $status
						endif
						if ( $found_nans == 0 ) then
							echo found nans
							set ok = 1
						endif

					endif
					if ( $ok == 0 ) then
						echo "SUMMARY generate ANALYSIS  for $core case $case_option parallel $compopt $esmf_lib_str PASS in $walltime_seconds seconds" >>! ${DEF_DIR}/wrfdatest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrfdatest.output
					else
						echo "SUMMARY generate ANALYSIS  for $core case $case_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrfdatest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrfdatest.output
						$MAIL -s "WRFDA FAIL ANALYSIS $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrfdatest.output
						if ( $KEEP_ON_RUNNING == FALSE ) exit ( 5 )
					endif
				else
					echo "SUMMARY generate ANALYSIS  for $core case $case_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrfdatest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrfdatest.output
					$MAIL -s "WRFDA FAIL ANALYSIS $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrfdatest.output
					if ( $KEEP_ON_RUNNING == FALSE ) exit ( 6 )
				endif
#XINZHANG###################################################
echo success or failure of analysis
banner 18
#set ans = "$<"
#XINZHANG###################################################

				#	We have to save this output file for our biggy comparison after all of the
				#	parallel options have been considered.

				mv wrfvar_output $TMPDIR/wrfvar_output.${case_option}.$compopt

				#       To save space, we move the executables after we are finished with them.

				if ( $case_option == $CASEOPTS[${#CASEOPTS}] ) then
					mv ../../var/build/da_wrfvar.exe.$compopt $TMPDIR/da_wrfvar.exe.$compopt
				endif

				popd

				BYPASS_COMP_LOOP_REAL:

			end

		endif

		#	OK, once more, we gotta check if this is a BIT4BIT run.  If so then there
		#	are a number of comparisons to do.  If this is a an OPTIMIZED run, then the
		#	comparisons will fail the bit-wise comparisons.

		if ( $REG_TYPE == BIT4BIT) then

			if ( ${#COMPOPTS} != 1 ) then
	
			#	If there is only a single parallel option, then we are just trying to
			#	build the serial code.  That implies no comparisons are needed.
	
					All of the analysis for this set of case have been
				#	generated.  We now compare the analysis output files to see
				#	if they are S^2D^2.
		
				pushd ${DEF_DIR}/regression_test/WRFDA/var/test/$case_option
				set DIFFWRF = ${DEF_DIR}/regression_test/WRFDA/var/build/diffwrf
	
				#	Are the files the same size?  If not, then only the initial times
				#	will be compared.  That means, on a failure to run a forecast, the
				#	diffwrf will give a pass.  We need to root out this evil.
	
				if ( ( -e $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[1] ) && \
				     ( -e $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[2] ) && \
				     ( -e $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[3] ) ) then
					set foo1 = ( ` \ls -ls $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[1] `)
					set foo2 = ( ` \ls -ls $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[2] `)
					set foo3 = ( ` \ls -ls $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[3] `)
					set size1 = $foo1[6]
					set size2 = $foo2[6]
					set size3 = $foo3[6]
					if ( ( $size1 == $size2 ) && ( $size1 == $size3 ) ) then
						set RIGHT_SIZE_MPI = TRUE
					else
						set RIGHT_SIZE_MPI = FALSE
					endif
				else
					set RIGHT_SIZE_MPI = FALSE
				endif
	
				#	Serial vs OpenMP
		
				rm fort.88 fort.98 >& /dev/null
				if ( ( -e $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[1] ) && \
				     ( -e $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[2] ) && \
				     ( $RIGHT_SIZE_MPI == TRUE ) ) then
					$DIFFWRF $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[1] \
					         $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[2] >& /dev/null
				else
					touch fort.88 fort.98
				endif
				if ( ! -e fort.88 ) then
				        echo "SUMMARY serial vs OpenMP  for case $case_option $esmf_lib_str            PASS" >>! ${DEF_DIR}/wrfdatest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrfdatest.output
				else
                                        set rmse1=`$DIFFWRF $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[1] $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[2] | sed -e '/Just/d' -e '/Diffing/d' -e '/Next/d' -e '/Field/d' | awk '{print $4}'`
                                        set rmse2=`$DIFFWRF $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[1] $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[2] | sed -e '/Just/d' -e '/Diffing/d' -e '/Next/d' -e '/Field/d' | awk '{print $5}'`
                                        set equal=1
                                        set count=1
                                        while ( $count <= $#rmse1 )
                                                if ( $rmse1[$count] != $rmse2[$count] ) then
                                                     set equal=0
                                                     break
                                                endif
                                                @ count= $count + 1
                                        end
                                        if ( $equal ) then
					     echo "SUMMARY serial vs OpenMP  for case $case_option $esmf_lib_str            PASS" >>! ${DEF_DIR}/wrfdatest.output
					     echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrfdatest.output
                                        else
					     echo "SUMMARY serial vs OpenMP  for case $case_option $esmf_lib_str            FAIL" >>! ${DEF_DIR}/wrfdatest.output
					     echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrfdatest.output
                                        endif
				endif

				#	Serial vs MPI
		
				rm fort.88 fort.98 >& /dev/null
				if ( ( -e $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[1] ) && \
				     ( -e $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[3] ) && \
				     ( $RIGHT_SIZE_MPI == TRUE ) ) then
					$DIFFWRF $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[1] \
					         $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[3] >& /dev/null
				else
					touch fort.88 fort.98
				endif
				if ( ! -e fort.88 ) then
				        echo "SUMMARY serial vs MPI  for case $case_option $esmf_lib_str            PASS" >>! ${DEF_DIR}/wrfdatest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrfdatest.output
				else
                                        set rmse1=`$DIFFWRF $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[1] $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[3] | sed -e '/Just/d' -e '/Diffing/d' -e '/Next/d' -e '/Field/d' | awk '{print $4}'`
                                        set rmse2=`$DIFFWRF $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[1] $TMPDIR/wrfvar_output.${case_option}.$COMPOPTS[3] | sed -e '/Just/d' -e '/Diffing/d' -e '/Next/d' -e '/Field/d' | awk '{print $5}'`
                                        set equal=1
                                        set count=1
                                        while ( $count <= $#rmse1 )
                                                if ( $rmse1[$count] != $rmse2[$count] ) then
                                                     set equal=0
                                                     break
                                                endif
                                                @ count= $count + 1
                                        end
                                        if ( $equal ) then
					     echo "SUMMARY serial vs MPI  for case $case_option $esmf_lib_str            PASS" >>! ${DEF_DIR}/wrfdatest.output
					     echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrfdatest.output
                                        else
					     echo "SUMMARY serial vs MPI  for case $case_option $esmf_lib_str            FAIL" >>! ${DEF_DIR}/wrfdatest.output
					     echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrfdatest.output
                                        endif
				endif
		
				popd
	
			endif

			# Generate and archive baseline or compare against baseline

			if ( $core == wrfda ) then
				if ( $GENERATE_BASELINE != FALSE ) then
					if ( ! -d $GENERATE_BASELINE ) then
						echo "ERROR:  Baseline directory ${GENERATE_BASELINE} does not exist" >>! \
						     ${DEF_DIR}/wrfdatest.output
						exit ( 10 ) 
					else
						# Archive serial output file to baseline
						pushd ${DEF_DIR}/regression_test/WRFDA/var/test/$case_option
						set basefilenm = wrfvar_output.${case_option}
						set basefile = ${GENERATE_BASELINE}/${basefilenm}
						set outfile = $TMPDIR/${basefilenm}.$COMPOPTS[1]
						if ( -e $outfile ) then
							cp $outfile $basefile || \
							  ( echo "ERROR:  cannot copy ${outfile} to ${basefile}" >>! \
							   ${DEF_DIR}/wrfdatest.output; exit 10 )
						else
							echo "ERROR:  Cannot archive baseline, file $outfile does not exist" >>! \
							     ${DEF_DIR}/wrfdatest.output
					echo "SUMMARY baseline archived in ${GENERATE_BASELINE} for case $case_option  FAIL" >>! \
							     ${DEF_DIR}/wrfdatest.output
							echo "-------------------------------------------------------------" >> \
							     ${DEF_DIR}/wrfdatest.output
							exit ( 10 ) 
						endif
					echo "SUMMARY baseline archived in ${GENERATE_BASELINE} for case $case_option  PASS" >>! \
						     ${DEF_DIR}/wrfdatest.output
						echo "-------------------------------------------------------------" >> \
						     ${DEF_DIR}/wrfdatest.output
						popd
					endif
				endif
				if ( $COMPARE_BASELINE != FALSE ) then
					if ( ! -d $COMPARE_BASELINE ) then
						echo "${0}: ERROR::  Baseline directory ${COMPARE_BASELINE} does not exist" >>! \
						     ${DEF_DIR}/wrfdatest.output
						exit ( 10 ) 
					else
						# Compare against baseline output file
						set basefilenm = wrfvar_output.${case_option}
						set basefile = ${COMPARE_BASELINE}/${basefilenm}
						set DIFFWRF = ${DEF_DIR}/regression_test/WRFDA/var/build/diffwrf
						set testdir = ${DEF_DIR}/regression_test/WRFDA/var/test/$case_option
						pushd ${testdir}
						foreach compopt ( $COMPOPTS )
							set cmpfile = $TMPDIR/${basefilenm}.$compopt
							rm fort.88 fort.98 >& /dev/null
							if ( ( -e ${basefile} ) && ( -e ${cmpfile} ) ) then
								#	Are the files the same size?  If not, then only the initial times
								#	will be compared.  That means, on a failure to run a forecast, the
								#	diffwrf will give a pass.  We need to root out this evil.
								set foob = ( ` \ls -ls ${basefile} `)
								set fooc = ( ` \ls -ls ${cmpfile} `)
								set sizeb = $foob[6]
								set sizec = $fooc[6]
								if ( $sizeb == $sizec ) then
									$DIFFWRF ${basefile} ${cmpfile} >& /dev/null
									if ( -e fort.88 ) then
                                                                             set rmse1=`$DIFFWRF ${basefile} ${cmpfile} | sed -e '/Just/d' -e '/Diffing/d' -e '/Next/d' -e '/Field/d' | awk '{print $4}'`
                                                                             set rmse2=`$DIFFWRF ${basefile} ${cmpfile} | sed -e '/Just/d' -e '/Diffing/d' -e '/Next/d' -e '/Field/d' | awk '{print $5}'`
                                                                             set equal=1
                                                                             set count=1
                                                                             while ( $count <= $#rmse1 )
                                                                                     if ( $rmse1[$count] != $rmse2[$count] ) then
                                                                                          set equal=0
                                                                                          break
                                                                                     endif
                                                                                     @ count= $count + 1
                                                                             end
                                                                             if ( $equal ) then
			                                                          rm -f fort.88 fort.98
                                                                             else
			                                                          echo "FAIL:  Baseline comparison, file ${cmpfile} did not match ${basefile} according to diffwrf" >>! \
										        ${DEF_DIR}/wrfdatest.output
									     endif
                                                                        endif
								else
									touch fort.88 fort.98
			echo "FAIL:  Baseline comparison, files ${cmpfile} and ${basefile} have different sizes" $sizeb "  " $sizec >>! \
									${DEF_DIR}/wrfdatest.output
								endif
							else
			echo "FAIL:  Baseline comparison, either ${cmpfile} or ${basefile} does not exist" >>! \
									${DEF_DIR}/wrfdatest.output
								touch fort.88 fort.98
							endif
							if ( ! -e fort.88 ) then
			echo "SUMMARY compare vs baseline ${COMPARE_BASELINE} for case $case_option compopt $compopt $esmf_lib_str  PASS" >>! \
								     ${DEF_DIR}/wrfdatest.output
								echo "-------------------------------------------------------------" >> \
								     ${DEF_DIR}/wrfdatest.output
							else
			echo "SUMMARY compare vs baseline ${COMPARE_BASELINE} for case $case_option compopt $compopt $esmf_lib_str  FAIL" >>! \
								     ${DEF_DIR}/wrfdatest.output
								echo "-------------------------------------------------------------" >> \
								     ${DEF_DIR}/wrfdatest.output
							endif
						end
						popd
					endif
				endif
			endif
			# End of generate and archive baseline or compare against baseline

		endif

		BOTTOM_OF_PHYSICS_LOOP:
		
	end

ALL_SHE_WROTE_FOR_NMM:

	echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrfdatest.output

        if ( $clrm ) then
          popd
        endif
		
end


#	How long did this take.

set end = ( `date` )
echo "Start WRFDA Regression: $start " >> ${DEF_DIR}/wrfdatest.output
echo "End   WRFDA Regression: $end   " >> ${DEF_DIR}/wrfdatest.output

#	We have done all of the tests, and placed the PASS FAIL labels in the
#	output file.  If there are any FAIL messages, we are in trouble.

grep FAIL ${DEF_DIR}/wrfdatest.output
set ok = $status

#	Send email of the status.

if ( $ok == 0 ) then
	$MAIL -s "REGRESSION FAILURE $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrfdatest.output
else
	$MAIL -s "REGRESSION SUCCESS $ARCH[1] " $GOOD_MAIL < ${DEF_DIR}/wrfdatest.output
endif

#	Clean left up detritus

cd $CUR_DIR

