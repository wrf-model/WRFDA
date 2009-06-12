#!/bin/make
 
MODULE		= mpeu

SRCS_mks	= Makefile			\
		  Makefile.conf.AIX		\
		  Makefile.conf.IRIX		\
		  Makefile.conf.IRIX64		\
		  Makefile.conf.OSF1		\
		  Makefile.conf.T3E		\
		  Makefile.conf.UNICOS		\
		  Makefile.dependency		\
		  Makefile.rules		\
		  configure			\
		  unconfigure			\
		  UCupdate.sh			\
		  LCupdate.sh			\
		  externals.mk			\
		  README

SRCS_C		= get_zeits.c

SRCS_F		= m_mpif.F m_random.F

SRCS_f90        = m_FileResolv.f90

SRCS_F90	= m_IndexBin_char.F90		\
		  m_IndexBin_integer.F90	\
		  m_IndexBin_logical.F90	\
		  m_List.F90			\
		  m_MergeSorts.F90		\
		  m_Filename.F90		\
		  m_Group.F90			\
		  m_Permuter.F90		\
		  m_SortingTools.F90		\
		  m_String.F90			\
		  m_StrTemplate.F90		\
		  m_chars.F90			\
		  m_die.F90			\
		  m_dropdead.F90		\
		  m_flow.F90			\
		  m_inpak90.F90			\
		  m_intkinds.F90		\
		  m_ioutil.F90			\
		  m_mall.F90			\
		  m_mergedList.F90		\
		  m_mpif90.F90			\
		  m_mpout.F90			\
		  m_rankMerge.F90		\
		  m_realkinds.F90		\
		  m_showDistrib.F90		\
		  m_stdio.F90			\
		  m_zeit.F90			

SRCS_ALL	= $(SRCS_f)		\
		  $(SRCS_F)		\
		  $(SRCS_f90)		\
		  $(SRCS_F90)		\
		  $(SRCS_C)

OBJS_ALL	= $(SRCS_f:.f=.o)	\
		  $(SRCS_F:.F=.o)	\
		  $(SRCS_f90:.f90=.o)	\
		  $(SRCS_F90:.F90=.o)	\
		  $(SRCS_C:.c=.o)

ARCH_TARGETS	= $(SRCS_mks) $(SRCS_ALL)
ALL_TARGETS	= ../lib/libeu.a ../lib/lib$(MODULE).a $(MODULE)
SUBD_TARGETS	= mpi0
CLEAN_FILES	= mpif.h

#	Try to define _Ddft, _Dsys and _Dnod in proper Makefiles.
#
#		1) _D for the make() time macros, as in
#		make _D="-Dxxx ..."
#
#		2) _Ddft for the default global macros defined
#		in the main Makefile.
#
#		3) _Dsys for the OS dependent macros defined
#		in the main Makefile and can be redefined in
#		the Makefile.conf file.
#
#		4) _Dnod for the OS or the node dependent
#		macros defined in the Makefile.conf file.

_Ddft	=
_Dsys	= -Dsys`uname -s`

MUPDATE		= sh UCupdate.sh
M		= mod
MODS_OBJ        = m_chars.o		\
		  m_die.o		\
		  m_Filename.o		\
                  m_FileResolv.o        \
		  m_flow.o		\
		  m_inpak90.o 		\
		  m_ioutil.o 		\
		  m_List.o 		\
		  m_MergeSorts.o	\
		  m_mall.o		\
		  m_mpif90.o		\
		  m_mpout.o		\
		  m_Permuter.o		\
		  m_realkinds.o		\
		  m_SortingTools.o	\
		  m_String.o		\
		  m_StrTemplate.o	\
		  m_showDistrib.o	\
		  m_stdio.o		\
		  m_zeit.o              \
		  assert.H

include Makefile.rules
include Makefile.conf

_DF	= $(_Ddft) $(_Dsys) $(_Dnod) $(_D)
_I	= $(_Inull) -I. $(_Iconf)

test:
	echo $(FC) $(F90) $(CORELIB) $(COREINC) $(MODULE)

# targets

# fvPSAS: install library and modules
export: $(MODULE) $(CORELIB)/libeu.a $(CORELIB)/libmpeu.a 
	$(MKDIR) $(CORELIB) $(COREINC)/mpeu
	$(CP) libeu.a libmpeu.a     $(CORELIB)
	$(CP) $(MODULE)/* $(COREINC)/mpeu
	$(CP) *.[Mm][Oo][Dd]  $(COREINC)/mpeu
	$(CP) *.H  $(COREINC)/mpeu

# fvPSAS: distclean brings source tree to pre-configuration state
distclean:          
	make clean
	$(RM) /libeu.a libmpeu.a  mpeu/*.[Mm][Oo][Dd]  
	make unconf
	./unconfigure

$(CORELIB)/libeu.a: $(OBJS_ALL) mpi0/libmpi0.a
	$(RM) $@
	cp -p mpi0/libmpi0.a $@
	@ echo $(AR) $@ $(OBJS_ALL)
	@ $(AR) $@ $(OBJS_ALL)

mpi0/libmpi0.a:
	( cd mpi0; $(MAKE) libmpi0.a )


$(CORELIB)/lib$(MODULE).a:	$(OBJS_ALL)
	$(RM) $@
	@ echo $(AR) $@ $(OBJS_ALL)
	@ $(AR) $@ $(OBJS_ALL)

$(MODULE): $(MODS_OBJ) DO_IT_ANYWAY
#	env M=$M $(MUPDATE) $@ $(MODS_OBJ)
	/bin/cp -p *.mod $(COREINC)/mpeu
# dependencies:

include Makefile.dependency

#.
