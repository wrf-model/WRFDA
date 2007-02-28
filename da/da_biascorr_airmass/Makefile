
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
FC = xlf90 -qsuffix=f=f90 -qrealsize=8

all:
	@ ${FC} -o da_sele_bias.exe rad_bias.f90 da_sele_bias.f90
	@ ${FC} -o da_scan_bias.exe rad_bias.f90 da_scan_bias.f90
	@ ${FC} -o da_airmass_bias.exe rad_bias.f90 pythag.f90 tqli.f90 tred2.f90 regress_one.f90 da_airmass_bias.f90
	@ ${FC} -o da_conv_bias.exe rad_bias.f90 da_conv_bias.f90
	@ ${FC} -o da_veri_bias.exe rad_bias.f90 da_veri_bias.f90
clean:
	@ rm *.mod *.exe
	
