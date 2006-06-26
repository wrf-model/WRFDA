#----------------------------------------------------------------------------
# file:         makefile for HDF-EOS (for internal ECS usage only)
#
# environment:  requires environment established by the ECS .buildrc file
# 
# environment variables dependencies:
#   MAKECMD  make command to be run on make files
#   ARCH     architecture (i.e. hardware/os type)
#
#
# author:  Guru Tej S. Khalsa / Space Applications Corp.
#          Adura Adekunjo    / L3 Communications GSI
#
# history:
#       10-Nov-1997 GTSK Initital version
#       23-Oct-2003 AA   Modified for Toolkit Linux Delivery
#
# notes:
#       1) This file is only for use by ECS Configuration Management (CM)
#	   personnel and software.  This file should not be distributed
#	   as part of the standard HDF-EOS delivery.
#----------------------------------------------------------------------------

# define shell to be used (use ksh so we can use the "." command)

SHELL=/bin/ksh

# define MAKE to be equivalant to $(MAKECMD)

MAKE=$(MAKECMD)

# define the location of the HDF-EOS STAGE area

HE_STAGE=/ecs/formal/STAGE/hdfeos

# define the location of the standard HDF-EOS development area

HE_HOME=/ecs/hdfeos

# define HDF base directory

HDF_BASE=/ecs/cots/hdf

# define current HDF version

CURRENT_HDF=HDF4.2r0

# define SDP Toolkit home directory

TK_HOME=/ecs/formal/TOOLKIT

# name of tar files for delivery
HDFEOS_SOURCE_TAR=HDFEOS_SOURCE.tar
HDFEOS_TESTDRIVERS_TAR=HDFEOS_TESTDRIVERS.tar

# define the default compiler optimization level
OPT_LVL=-g

# define default target

default:
	@echo "This file is for use by ECS Configuration Management processes only."

# define a target to build BOTH variations (or "flavors") of HDF-EOS
# for a given "brand" (i.e. architecture type)

full: opt debug

# define a target to build the "debug" variation (or "flavor") of HDF-EOS
# for a given "brand" (i.e. architecture type) ("debug" means the library
# will be compiled for use in a souce code debugger)

debug:
	@$(MAKE) flavor_dir FLAVOR=$@
	@export use_flavor=1; $(MAKE) $(TARGET) OPT_LVL=-g

# define a target to build the "optimized" variation (or "flavor") of
# HDF-EOS for a given "brand" (i.e. architecture type) ("optimized" means the
# library will be optimized for execution speed by the compiler)

opt:
	@$(MAKE) flavor_dir FLAVOR=$@
	@export use_flavor=1; $(MAKE) $(TARGET) OPT_LVL=-O

# define standard CM targets

# NOTE: The standard targets defined by ECS CM are:
#       BuildImake, ProductHs, ProductLibs, LastPass, Deliver
#
#       Here the above targets are shadowed by the targets:
#       BuildImake2, ProductHs2, ProductLibs2, LastPass2, Deliver2
#       respectively.  The original targets only call the target
#       HE_ENV which sets up the HDF-EOS environment and then
#       builds the shadow target.

BuildImake:
	@$(MAKE) $(MFLAGS) HE_ENV TARGET=$@2

BuildImake2:
	@echo "No Imakefile to build."

ProductHs:
	@$(MAKE) $(MFLAGS) HE_ENV TARGET=$@2

ProductHs2:
	@echo "No header files to generate or promote."

ProductLibs:
	@$(MAKE) $(MFLAGS) HE_ENV TARGET=$@2

ProductLibs2:
	@if [ ! -e $(HDFEOS_HOME)/lib/$(ARCH) ] ; then	\
		cd $(HDFEOS_HOME)/lib;			\
		ln -s `basename $(HDFEOS_LIB)` $(ARCH);		\
	fi
	@cd $(HDFEOS_HOME)/src; $(MAKE) $(MFLAGS) all
	@cd $(TK_HOME)/src/GCT/freeware; $(MAKE) $(MFLAGS) gctp

LastPass:
	@$(MAKE) $(MFLAGS) HE_ENV TARGET=$@2

LastPass2:
	@echo "No utilites to build."

Deliver:
	@$(MAKE) $(MFLAGS) HE_ENV TARGET=$@2

Deliver2:
	tar cvfX $(HE_HOME)/install/$(HDFEOS_SOURCE_TAR) $(HE_HOME)/make_exclude_list -C /ecs ./hdfeos
	tar cvf $(HE_HOME)/install/$(HDFEOS_TESTDRIVERS_TAR) -C /ecs ./hdfeos/testdrivers

DaacDeliver:
	@if [ $(ARCH) = "sgi6n32" ] ; then				    \
		for arch in sgi6n32 sgi6 ; do			            \
			for targ in ProductHs ProductLibs LastPass ; do	    \
				$(MAKECMD) full TARGET=$$targ ARCH=$$arch;  \
			done;						    \
		done;							    \
	elif [ $(ARCH) = "irix65" ] ; then				    \
		for arch in sgi6n32 sgi6 ; do			            \
			for targ in ProductHs ProductLibs LastPass ; do	    \
				$(MAKECMD) full TARGET=$$targ ARCH=$$arch;  \
			done;						    \
		done;                                                       \
	else								    \
		for targ in ProductHs ProductLibs LastPass ; do		    \
			$(MAKECMD) full TARGET=$$targ;			    \
		done;							    \
	fi

LinuxDaacDeliver:  
	@if [ $(ARCH) = "linux" ] ; then					\
		for arch in linux ; do						\
			for targ in ProductHs ProductLibs LastPass ; do		\
			$(MAKECMD) full TARGET=$$targ ARCH=$$arch;		\
		done ;								\
		done ;								\
	fi

# this is a special target used to establish the HDF-EOS environment

HE_ENV:
	@case $(ARCH) in					\
		hp10)    brand=hp;      ;;                      \
		sgi6n32) brand=sgi32; sgi_mode=n32;  ;;         \
		irix65)  brand=irix65; sgi_mode=65   ;;         \
		sgi6)    brand=sgi64; sgi_mode=64;  ;;		\
		sun5.5)  brand=sun5;    ;;			\
		sun5.8)  brand=sun5.8;    ;;			\
		*)       brand=$(ARCH); ;;			\
	esac;							\
	HDFEOS_HOME=$(HE_HOME);					\
	opt_flag=$(OPT_LVL);					\
	export HDFEOS_HOME sgi_mode opt_flag;			\
	. $(HE_HOME)/bin/tmp/hdfeos_env.ksh.tmp;		\
	MAKE="$(MAKECMD)";					\
	AR='ar r';						\
	HDFINC=$(HDF_BASE)/$$brand/$(CURRENT_HDF)/include;	\
	PGSINC=$(TK_HOME)/include;				\
	export MAKE AR HDFINC PGSINC;				\
	$(MAKE) $(MFLAGS) $(TARGET)

flavor_dir:
	@sgi_mode=sgi32;					\
	opt_flag=-O;						\
	postfix="";						\
	case $(FLAVOR) in					\
		*debug)						\
			opt_flag=-g;				\
			postfix=_debug;				\
		;;						\
	esac;							\
	case $(ARCH) in						\
		hp10)						\
			brand=hp;				\
		;;						\
		sgi6n32)					\
			brand=sgi32;				\
			sgi_mode=n32;				\
		;;						\
		irix65)                                         \
			brand=irix65;				\
			sgi_mode=65;				\
		;;						\
		sgi6)						\
			brand=sgi64;				\
			sgi_mode=64;				\
		;;						\
		sun5.5)						\
			brand=sun5;				\
		;;						\
		sun5.8)						\
			brand=sun5.8;				\
		;;						\
		*)						\
			brand=$(ARCH);				\
		;;						\
	esac;							\
	he_obj=$(HE_HOME)/obj/$${brand}$$postfix;		\
	he_lib=$(HE_HOME)/lib/$${brand}$$postfix;		\
	he_bin=$(HE_HOME)/bin/$${brand}$$postfix;		\
	tmpdir=$(HE_HOME)/bin/tmp;				\
	if [ ! -d $$he_obj ] ; then				\
		mkdir $$he_obj;					\
	fi;							\
	if [ ! -d $$he_lib ] ; then				\
		mkdir $$he_lib;					\
	fi;							\
	if [ ! -d $$he_bin ] ; then				\
		mkdir $$he_bin;					\
	fi;							\
	hdflib=$(HDF_BASE)/$${brand}/$(CURRENT_HDF)/lib;	\
	hdfinc=$(HDF_BASE)/$${brand}/$(CURRENT_HDF)/include;	\
	if [ ! -f $$he_bin/hdfeos_env.csh ] ; then			\
		cd $$he_bin;						\
		echo "setenv HDFEOS_HOME $(HE_HOME)" > hdfeos_env.csh;	\
		echo "setenv HDFLIB $$hdflib" >> hdfeos_env.csh;	\
		echo "setenv HDFINC $$hdfinc" >> hdfeos_env.csh;	\
		echo "set sgi_mode=$$sgi_mode" >> hdfeos_env.csh;	\
		echo "set opt_flag=$$opt_flag" >> hdfeos_env.csh;	\
		echo "set use_flavor=1" >> hdfeos_env.csh;		\
		cat $$tmpdir/hdfeos_env.csh.tmp >> hdfeos_env.csh;	\
	fi;								\
	if [ ! -f $$he_bin/hdfeos_env.ksh ] ; then			\
		cd $$he_bin;						\
		echo "HDFEOS_HOME=$(HE_HOME)" > hdfeos_env.ksh;		\
		echo "HDFLIB=$$hdflib" >> hdfeos_env.ksh;		\
		echo "HDFINC=$$hdfinc" >> hdfeos_env.ksh;		\
		echo "sgi_mode=$$sgi_mode" >> hdfeos_env.ksh;		\
		echo "opt_flag=$$opt_flag" >> hdfeos_env.ksh;		\
		echo "use_flavor=1" >> hdfeos_env.ksh;			\
		cat $$tmpdir/hdfeos_env.ksh.tmp >> hdfeos_env.ksh;	\
	fi
