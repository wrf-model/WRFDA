! $Id: descrip.mms,v 1.1 1993/04/21 21:51:45 chouck Exp $
!
!          MMS file for ncgen
!
!==============================================================================
! ASSUMPTIONS:

!==============================================================================
! CUSTOMIZATIONS (change as necessary):

!Root of the installation:
INSTROOT	= LDMROOT:

!Operating system version:
OS		= vms_5_4

!Compilation options:
COPTS		= /opt/nodebug

!==============================================================================
EXEDIR		= $(INSTROOT)[exe]
LIBRARY		= [-]netcdf.olb
LINKFLAGS	= /nodebug/exec=$(mms$target_name).exe
!LINKFLAGS	= /debug/exec=$(mms$target_name).exe
CFLAGS		= $(COPTS)/include=[-.src]
OBJS 		= \
		    getfill.obj, -
		    close.obj, -
		    derror.obj, -
		    emalloc.obj, -
		    escapes.obj, -
		    generate.obj, -
		    getopt.obj, -
		    init.obj, -
		    load.obj, -
		    main.obj, -
		    ncgentab.obj

all :		ncgen.exe
	@ continue

ncgen.exe :	$(OBJS), $(LIBRARY)
	$(LINK)$(LINKFLAGS) $(OBJS),$(LIBRARY)/lib,[]ncgen/opt

getopt.obj :	[-.util]getopt.c

install :	$(EXEDIR), $(EXEDIR)ncgen.exe
	@ continue

$(EXEDIR) :
	- cre/dir $@

$(EXEDIR)ncgen.exe :	ncgen.exe
	copy ncgen.exe $@
	purge $@

clean :
	purge
	- delete *.obj.*,*.lis.*,*.map.*,*.exe.*

ncgen.obj :	ncgentab.h

ncgentab.obj :	vmstab.c
	$(CC)$(CFLAGS)/obj=$@ $?
