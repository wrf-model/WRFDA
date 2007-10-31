ALL: mpiman.pdf
SHELL = /bin/sh

INSTALL         = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA    = ${INSTALL} -m 644
prefix          = /sandbox/gropp/tmp/mpi2-inst
docdir          = ${prefix}/doc
srcdir = .

VPATH = .

MPIMANAUXFILES = refman.sty tpage.sty refcmd.tex refmpi.tex
mpiman.dvi: mpiman.tex MPI_Wtime.tex
	-rm -f mpiman.log mpiman.msg
	for file in $(MPIMANAUXFILES) ; do \
	    if [ ! -s $$file ] ; then \
	       if [ -s $(srcdir)/$$file ] ; then \
	            cp $(srcdir)/$$file . ; \
               else \
		    echo "Unable to find file $$file" ; \
		    exit 1; \
               fi ;\
	    fi ; \
	done
	-latex $(srcdir)/mpiman < /dev/null 2>&1 >mpiman.msg
	latex $(srcdir)/mpiman < /dev/null 2>&1 >mpiman.msg

mpiman.pdf: mpiman.dvi
	dvipdfm mpiman

clean:
	rm -f *.dvi *.log *.toc *.aux *.out
distclean: clean

#
# Install these into the various FTP and WWW locations
install: mpiman.pdf
	$(INSTALL_DATA) mpiman.pdf ${docdir}
