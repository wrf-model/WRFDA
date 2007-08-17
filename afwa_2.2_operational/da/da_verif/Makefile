#	Top-level Makefile for get_diag

#	Macros, these should be generic for all machines

.IGNORE:

AR	=	ar ru
CD	=	cd
LN	=	ln -s
MAKE	=	make -f Makefile
RM	=	/bin/rm -f
RM_LIST	=	*.o core *.i *.mod fort.* *.out namelist.* *~ *.exe

#	Targets for supported architectures

default:	make_rules
		find . -name \*.f -exec rm {} \;
		( $(CD) src ; $(MAKE) get_diag.exe )
		( $(RM) get_diag.exe ;   $(LN) src/get_diag.exe . )

clean:
	$(RM) $(RM_LIST)
	($(CD) src ; $(MAKE) clean )

make_rules:
	uname -a > .tmpfile
	grep OSF .tmpfile								; \
	if [ $$? = 0 ] ; then echo "Compiling for Compaq"				; \
		echo "CPP	= /usr/bin/cpp"			>  compiler_macros	; \
		echo "CPPFLAGS	= -C -P -DDEC"		>> compiler_macros	; \
		echo "FC	= f90"				>> compiler_macros	; \
		echo "FCFLAGS	= -C -free"			>> compiler_macros	; \
		echo "LDFLAGS	= "				>> compiler_macros	; \
		echo "FPPFLAG	= "				>> compiler_macros	; \
		echo "LIBS	= "				>> compiler_macros	; \
	else grep AIX .tmpfile	 							; \
	if [ $$? = 0 ] ; then echo "Compiling for IBM"						; \
		echo "CPP	= /usr/lib/cpp"			>  compiler_macros	; \
		echo "CPPFLAGS	= -C -P -DIBM"		>> compiler_macros	; \
		echo "FC	= xlf90"			>> compiler_macros	; \
		echo "FCFLAGS	= -qlanglvl=90pure -qarch=auto"	>> compiler_macros	; \
		echo "LDFLAGS	= " 				>> compiler_macros	; \
		echo "FPPFLAG	= "				>> compiler_macros	; \
		echo "LIBS	= "				>> compiler_macros	; \
	else grep Darwin .tmpfile	 							; \
	if [ $$? = 0 ] ; then echo "Compiling for Mac OS X"						; \
		echo "CPP	= /usr/bin/cpp"			>  compiler_macros	; \
		echo "CPPFLAGS	= -C -P -DMAC"		>> compiler_macros	; \
		echo "FC	= xlf90"			>> compiler_macros	; \
		echo "FCFLAGS	= -qlanglvl=90pure -qarch=auto"	>> compiler_macros	; \
		echo "LDFLAGS	= " 				>> compiler_macros	; \
		echo "FPPFLAG	= "				>> compiler_macros	; \
		echo "LIBS	= "				>> compiler_macros	; \
	else grep Linux .tmpfile 							; \
	if [ $$? = 0 ] ; then echo "Compiling for Linux"				; \
		echo "CPP	= /lib/cpp"			>  compiler_macros	; \
		echo "CPPFLAGS	= -C -P -DLINUX -traditional"	>> compiler_macros	; \
		echo "FC	= pgf90"			>> compiler_macros	; \
		echo "FCFLAGS	= -Mfreeform"			>> compiler_macros	; \
		echo "LDFLAGS	= " 				>> compiler_macros	; \
		echo "FPPFLAG	= "				>> compiler_macros	; \
		echo "LIBS	= "				>> compiler_macros	; \
	else echo "Do not know how to compile for the `cat .tmpfile` machine." 		; \
	fi ; \
	fi ; \
	fi ; \
	fi

	echo "AR	= $(AR)"	>> compiler_macros
	echo "RM	= $(RM)"	>> compiler_macros
	echo "RM_LIST	= $(RM_LIST)"	>> compiler_macros
	echo "CD	= $(CD)"	>> compiler_macros
	echo "LN	= $(LN)"	>> compiler_macros
	echo "MAKE	= $(MAKE)"	>> compiler_macros
	echo "SHELL	= /bin/sh"	>> compiler_macros
	echo "TOUCH	= touch"	>> compiler_macros

