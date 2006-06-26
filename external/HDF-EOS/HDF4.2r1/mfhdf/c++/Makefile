# $Id: Makefile,v 1.3 1995/10/02 15:32:52 georgev Exp $
#
#	Development Makefile for netcdf c++ interface
#
srcdir = .
TOP_SRCDIR = ../..
# Version of the library
PACKAGE	 = HDF
VERSION  = 4.0
INCDIR = ../libsrc
LIBDIR = ../libsrc

CCC = CC
INCLUDES = -I$(INCDIR)

CCFLAGS=-g
MANIFEST = $(DISTFILES)
DISTFILES	= Makefile README example.c example.cc example.cdl \
		  nctst.cc ncvalues.cc ncvalues.hh netcdf.cc netcdf.hh \
		  expected nc.txn nc.info
DEFINES =
CPPFLAGS = $(INCLUDES) $(DEFINES)
LIBS = -L$(LIBDIR) -lnetcdf
PURIFY = purify
PRINTER = hpnp

# a test program that uses some of the draft C++ interface
PROG = nctst
PROG_SRCS = netcdf.cc nctst.cc ncvalues.cc
PROG_OBJS = netcdf.o nctst.o ncvalues.o

all: $(PROG)

$(PROG) : $(PROG_OBJS)
	$(LINK.cc) $(PROG_OBJS) $(LIBS) -o $@

test:	expected $(PROG)
	./$(PROG) > test.out
	@cmp expected test.out && \
	    echo "*** $(PROG) test successful ***" ;

nctst.pure:	$(PROG_OBJS)
	$(PURIFY) $(LINK.cc) $(PROG_OBJS) $(LIBS) -o $@
	./$@

clean:
	rm -f $(PROG_OBJS) $(PROG) example.cdf *~

distclean:	clean

netcdf.o: netcdf.hh $(INCDIR)/netcdf.h netcdf.cc
ncvalues.o: ncvalues.hh ncvalues.cc
nctst.o: netcdf.hh nctst.cc

nctst_src: nctst.cc netcdf.cc ncvalues.cc
	#load nctst.cc
	#load netcdf.cc
	#load ncvalues.cc
	#load -L../../lib ../../lib/libnetcdf.a
	#link

nvtst: nvtst.o ncvalues.o
	$(LINK.cc) -g -o $@ nvtst.cc ncvalues.o


nvtst_src: nvtst.cc ncvalues.cc
	#load nvtst.cc
	#load ncvalues.cc
	#link

print: nctst.cc netcdf.hh netcdf.cc ncvalues.hh ncvalues.cc
	vgrind -s11 -lC++ -t nctst.cc netcdf.hh netcdf.cc ncvalues.hh \
				ncvalues.cc | lpr -Phpnp

nc.hc:	nc.ps
	lpr -P$(PRINTER) nc.ps

nc.dvi:	nc.txn
	tex nc.txn
	texindex nc.fn nc.cp
	tex nc.txn

nc.ps:	nc.dvi
	dvips -o nc.ps -r0 nc.dvi

nc.info: nc.txn
	makeinfo nc.txn

MANIFEST.echo:
	@echo $(MANIFEST) | fmt -1

# for distribution creation
distdir = $(TOP_SRCDIR)/$(PACKAGE)-$(VERSION)/$(curdir)
curdir = mfhdf/c++
dist: $(DISTFILES)
	@for file in $(DISTFILES); do \
	  ln $(srcdir)/$$file $(distdir) 2> /dev/null \
	    || { echo copying $$file instead; \
	    cp -p $(srcdir)/$$file $(distdir);}; \
	done
