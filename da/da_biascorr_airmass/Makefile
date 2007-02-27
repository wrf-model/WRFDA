
#FC = f90 -O2 -O -w -g
#FC = f90 -O -g
#FC = f90 -O
#----------------------
# PC Linux
#---------------------
#FC = pgf90 -byteswapio

#----------------------
# IBM XLF
#---------------------
FC = xlf90 -qsuffix=f=F90 -qrealsize=8

all:
	@ ${FC} -o da_sele_bias.exe rad_bias.F90 da_sele_bias.F90
	@ ${FC} -o da_scan_bias.exe rad_bias.F90 da_scan_bias.F90
	@ ${FC} -o da_airmass_bias.exe rad_bias.F90 pythag.F90 tqli.F90 tred2.F90 regress_one.F90 da_airmass_bias.F90
	@ ${FC} -o da_conv_bias.exe rad_bias.F90 da_conv_bias.F90
	@ ${FC} -o da_veri_bias.exe rad_bias.F90 da_veri_bias.F90
clean:
	@ rm *.mod *.exe
	
