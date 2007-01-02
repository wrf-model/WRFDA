# Makefile for 3DVAR Program and its applications

# Lower level Makefile

.IGNORE:
#.SUFFIXES: .F .i .o .f .c .f90
.SUFFIXES: .f90 .o

include ../../configure.wrf

##########################################################################################

all:			to_be_file		\
			just_be			\
			plot_eigen		\
			scale_length		\
			write_sl_2_be		\
			update_bc		\
			convert_to_mm5v3        \
			generate_difference	\
			bufr_little_endian_convert	\
			daprog_diagnostics	\
			daprog_ominusb		\
			tune

##########################################################################################

OBJ_FOR_ALL		=	module_define.o	\
				Utl_IO.o

OBJ_be_file		=	Utl_Trans.o	\
				to_be_file.o

OBJ_just_be		=	Utl_Trans.o	\
				just_be.o

OBJ_plot_eigen		=	Utl_Graph.o	\
				plot_eigen.o

OBJ_scale_length	=	Utl_Graph.o	\
				scale_length.o

OBJ_write_sl_2_be	=	write_sl_2_be.o

OBJ_update_bc		=	update_bc.o

OBJ_convert_to_mm5v3	=	convert_to_mm5v3.o

OBJ_generate_difference =	generate_difference.o

OBJ_bufr_convert	=	grabbufr.o

##########################################################################################

to_be_file:		$(OBJ_FOR_ALL) $(OBJ_be_file)
			$(FC) -o $@.exe $(OBJ_FOR_ALL) $(OBJ_be_file) $(LDUTILS)

just_be:		$(OBJ_FOR_ALL) $(OBJ_just_be)
			$(FC) -o $@.exe $(OBJ_FOR_ALL) $(OBJ_just_be) $(LDUTILS)

plot_eigen:		$(OBJ_FOR_ALL) $(OBJ_plot_eigen)
			$(FC) -o $@.exe $(LDUTILS) $(LOCAL_LIBRARIES) $(OBJ_FOR_ALL) $(OBJ_plot_eigen)

scale_length:		$(OBJ_FOR_ALL) $(OBJ_scale_length)
			$(FC) -o $@.exe $(LDUTILS) $(LOCAL_LIBRARIES) $(OBJ_FOR_ALL) $(OBJ_scale_length)

write_sl_2_be:		$(OBJ_FOR_ALL) $(OBJ_write_sl_2_be)
			$(FC) -o $@.exe $(OBJ_FOR_ALL) $(OBJ_write_sl_2_be) $(LDUTILS)

update_bc:		$(OBJ_FOR_ALL) $(OBJ_update_bc)
			$(FC) -o $@.exe $(OBJ_FOR_ALL) $(OBJ_update_bc) $(LDUTILS)

convert_to_mm5v3:	$(OBJ_FOR_ALL) $(OBJ_convert_to_mm5v3)
			$(FC) -o $@.exe $(OBJ_FOR_ALL) $(OBJ_convert_to_mm5v3) $(LDUTILS)

generate_difference:	$(OBJ_FOR_ALL) $(OBJ_generate_difference)
			$(FC) -o $@.exe $(OBJ_FOR_ALL) $(OBJ_generate_difference) $(LDUTILS)

daprog_ominusb:		daprog_ominusb.o
			$(FC) -o $@.exe daprog_ominusb.o $(LDUTILS)

daprog_diagnostics:	daprog_diagnostics.o
			$(FC) -o $@.exe daprog_diagnostics.o $(LDUTILS)

tune:			tune.o
			$(FC) -o $@.exe tune.o $(LDUTILS)

bufr_little_endian_convert:	\
			$(OBJ_bufr_convert)
			$(FC) -o $@.exe $(OBJ_bufr_convert) ../../da_3dvar/src/da_bufrlib.o

grabbufr:		$(OBJ_FOR_ALL)		\
			grabbufr.f90
			$(FC) -c $(F77FLAGS_BUFR) grabbufr.f90

###########################################################################################

daprog_ominusb.o:	daprog_ominusb.f90
			cp daprog_ominusb.f90  daprog_ominusb.f
			$(FC) -c $(FUFLAGS) daprog_ominusb.f
			$(RM) daprog_ominusb.f

daprog_diagnostics.o:	daprog_diagnostics.f90
			cp daprog_diagnostics.f90  daprog_diagnostics.f
			$(FC) -c $(FUFLAGS) daprog_diagnostics.f
			$(RM) daprog_ominusb.f

tune.o:			tune.f90
			cp tune.f90 tune.f
			$(FC) -c $(FUFLAGS) tune.f
			$(RM) tune.f

to_3dvar_domain.o:	$(OBJ_FOR_ALL)			\
			to_3dvar_domain.f90
			cp to_3dvar_domain.f90 to_3dvar_domain.f
			$(FC) -c $(FUFLAGS) to_3dvar_domain.f
			$(RM) to_3dvar_domain.f

to_be_file.o:		$(OBJ_FOR_ALL)			\
			Utl_Trans.o			\
			to_be_file.f90
			cp to_be_file.f90 to_be_file.f
			$(FC) -c $(FUFLAGS) to_be_file.f
			$(RM) to_be_file.f

just_be.o:		$(OBJ_FOR_ALL)			\
			Utl_Trans.o			\
			just_be.f90
			cp just_be.f90 just_be.f
			$(FC) -c $(FUFLAGS) just_be.f
			$(RM) just_be.f

plot_eigen.o:		$(OBJ_FOR_ALL)			\
			Utl_Graph.o			\
			plot_eigen.f90
			cp plot_eigen.f90 plot_eigen.f
			$(FC) -c $(FUFLAGS) plot_eigen.f
			$(RM) plot_eigen.f

scale_length.o:		$(OBJ_FOR_ALL)			\
			Utl_Graph.o			\
			scale_length.f90
			cp scale_length.f90 scale_length.f
			$(FC) -c $(FUFLAGS) scale_length.f
			$(RM) scale_length.f

write_sl_2_be.o:	$(OBJ_FOR_ALL)			\
			write_sl_2_be.f90
			cp write_sl_2_be.f90 write_sl_2_be.f
			$(FC) -c $(FUFLAGS) write_sl_2_be.f
			$(RM) write_sl_2_be.f

update_bc.o:		$(OBJ_FOR_ALL)			\
			update_bc.f90
			cp update_bc.f90 update_bc.f
			$(FC) -c $(FUFLAGS) update_bc.f
			$(RM) update_bc.f

convert_to_mm5v3.o:	$(OBJ_FOR_ALL)			\
			convert_to_mm5v3.f90
			cp convert_to_mm5v3.f90 convert_to_mm5v3.f
			$(FC) -c $(FUFLAGS) convert_to_mm5v3.f
			$(RM) convert_to_mm5v3.f

generate_difference.o:	$(OBJ_FOR_ALL)			\
			generate_difference.f90
			cp generate_difference.f90 generate_difference.f
			$(FC) -c $(FUFLAGS) generate_difference.f
			$(RM) generate_difference.f

new_terrain.o:		$(OBJ_FOR_ALL)			\
			new_terrain.f90
			cp new_terrain.f90  new_terrain.f
			$(FC) -c $(FUFLAGS) new_terrain.f
			$(RM) new_terrain.f

module_define.o:	Utl_Define/module_define.f90
			cp Utl_Define/module_define.f90 Utl_Define/module_define.f
			$(FC) -c -I./Utl_Define $(FUFLAGS) Utl_Define/module_define.f
			$(RM) Utl_Define/module_define.f

Utl_IO.o:		module_define.o			\
			Utl_IO/cleanRows.inc		\
			Utl_IO/crs_2_dot.inc		\
			Utl_IO/read_mm5_ic.inc		\
			Utl_IO/print_big_header.inc	\
			Utl_IO/print_sub_header.inc	\
			Utl_IO/Utl_IO.f90
			cp Utl_IO/Utl_IO.f90 Utl_IO/Utl_IO.f
			$(FC) -c -I./Utl_IO $(FUFLAGS) Utl_IO/Utl_IO.f
			$(RM) Utl_IO/Utl_IO.f

Utl_Trans.o:		module_define.o			\
			Utl_IO.o			\
			Utl_Trans/make_large.inc	\
			Utl_Trans/get_lliy_lljx.inc	\
			Utl_Trans/Utl_Trans.f90
			cp Utl_Trans/Utl_Trans.f90 Utl_Trans/Utl_Trans.f
			$(FC) -c -I./Utl_Trans $(FUFLAGS) Utl_Trans/Utl_Trans.f
			$(RM) Utl_Trans/Utl_Trans.f

Utl_Graph.o:		module_define.o			\
			Utl_Graph/close_gks.inc		\
			Utl_Graph/open_gks.inc		\
			Utl_Graph/setup_color_table.inc	\
			Utl_Graph/Utl_Graph.f90
			cp Utl_Graph/Utl_Graph.f90 Utl_Graph/Utl_Graph.f
			$(FC) -c -I./Utl_Graph $(FUFLAGS) Utl_Graph/Utl_Graph.f
			$(RM) Utl_Graph/Utl_Graph.f

##############################################################################

clean:
			rm -f *.o *.exe core *.mod

##############################################################################

#-----------------------------------------------------------------------------
#       Don't touch anything below this line
#-----------------------------------------------------------------------------

#.F.i:
#	$(RM) $@
#	$(CPP) $(CPPFLAGS) -D$(CPPOPTION) $*.F > $@
#	mv $*.i $(DEVTOP)/pick/$*.f
#.c.o:
#	$(RM) $@ && \
#	$(CC) -c $(CFLAGS) -D$(CPPOPTION) $*.c
#.F.o:
#	$(RM) $@
#	$(FC) -c $(FUFLAGS) -D$(CPPOPTION) $*.F
#.F.f:
#	$(RM) $@
#	$(CPP) $(CPPFLAGS) -D$(CPPOPTION) $*.F > $@
#.f.o:
#	$(RM) $@
#	$(FC) -c $(FUFLAGS) $*.f

.f90.o:
	$(RM) $@
	$(FC) -c $(FUFLAGS) $*.f90

##############################################################################

MSS_DIR =		New_3DVAR

TAR_FLS =		Makefile			\
			*.f90				\
			Utl_Define/*.f90		\
			Utl_Define/*.inc

tar:
			tar cvf utl_3dvar.tar $(TAR_FLS)
			gzip utl_3dvar.tar
			mswrite -t 1000 utl_3dvar.tar.gz \
					$(MSS_DIR)/utl_3dvar.tar.gz
			$(RM) utl_3dvar.tar.gz
