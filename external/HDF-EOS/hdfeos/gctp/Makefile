#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for GCTP
# 	
# environment variables dependencies:
#   compiler:  CC CFHFLAGS
#  
# author:  Sbe Taaheri 
#
# history:
#	17 Sep-2002 AT Initial version
#       11 Oct-2005 AT updated for Solaris 9 and 10
#----------------------------------------------------------------------------

# force make to use the 'sh' shell
SHELL = /bin/sh

# name of remove utility
RM= /bin/rm
RMFLAGS= -f 

all: finc select_brand dfinc

INCFILES =	\
	src/bcea.h	\
	src/cproj.h	\
	src/isin.h	\
	src/proj.h	\
	src/gctp_prototypes.h \
	src/cproj_prototypes.h

finc: $(INCFILES)

src/bcea.h: include/bcea.h
	cp $? $@

src/cproj.h: include/cproj.h
	cp $? $@

src/isin.h: include/isin.h
	cp $? $@

src/proj.h: include/proj.h
	cp $? $@

src/gctp_prototypes.h: include/gctp_prototypes.h
	cp $? $@

src/cproj_prototypes.h: include/cproj_prototypes.h
	cp $? $@

select_brand:
	@if [ $(BRAND) = "sun5" ] ; then \
		cd src; $(MAKE) -f makesol25 ; \
	elif [ $(BRAND) = "hp" ] ; then \
		cd src; $(MAKE) -f makehp ; \
	elif [ $(BRAND) = "hp11" ] ; then \
		cd src; $(MAKE) -f makehp11 ; \
	elif [ $(BRAND) = "dec" ] ; then \
		cd src; $(MAKE) -f makedec ; \
	elif [ $(BRAND) = "ibm" ] ; then \
		cd src; $(MAKE) -f makeibm ; \
	elif [ $(BRAND) = "irix65" ] ; then \
		cd src; $(MAKE) -f makesgi65_64 ; \
		$(MAKE) -f makegi65_n32 ; \
	elif [ $(BRAND) = "linux" ] ; then \
		cd src; $(MAKE) -f makelinux ; \
	elif [ $(BRAND) = "sgi" ] ; then \
		cd src; $(MAKE) -f makesgi32 ; \
	elif [ $(BRAND) = "sgi32" ] ; then \
		cd src; $(MAKE) -f makesgin32 ; \
	elif [ $(BRAND) = "sgi64" ] ; then \
		cd src; $(MAKE) -f makesgi64 ; \
	elif [ $(BRAND) = "sun5.8" ] ; then \
		cd src; $(MAKE) -f makesol28 ; \
	elif [ $(BRAND) = "sun5.9" ] ; then \
		cd src; $(MAKE) -f makesol29 ; \
	elif [ $(BRAND) = "sun5.10" ] ; then \
		cd src; $(MAKE) -f makesol210 ; \
	else \
		echo " The brand $(BRAND) is not supported. " ; \
		echo " Set the env. vriable BRAND to a supported one then make this makefile"; \
	fi ;

clean:
	$(RM) $(RMFLAGS) core *.o
	$(RM) $(RMFLAGS) src/*.o


dfinc:
	@$(RM) $(RMFLAGS) $(INCFILES)

