#!/bin/ksh
#
# Hack to rebuild registry and only solve_em_ad().  
#
# Use this when modifying halos in registry.  
#
set -ux

/bin/rm -f main/libwrflib.a

tools/registry -DRSL_LITE -DDM_PARALLEL -DIWORDSIZE=4 -DDWORDSIZE=8 -DRWORDSIZE=8  -DLWORDSIZE=4 -DNETCDF -DTRIEDNTRUE   -DYYY -DINTIO  -DLIMIT_ARGS -DNO_NAMELIST_PRINT -DNATIVE_MASSV -DSTAND_ALONE -DWRFVAR Registry/Registry

cd frame
ar ru ../main/libwrflib.a module_driver_constants.o module_domain.o \
  module_dfi.o module_integrate.o module_integrate_ad.o module_integrate_tl.o \
  module_integrate_tst.o module_timing.o module_configure.o module_tiles.o \
  module_machine.o module_nesting.o module_wrf_error.o \
  module_state_description.o module_sm.o module_io.o module_dm.o \
  module_quilt_outbuf_ops.o module_io_quilt.o wrf_num_bytes_between.o \
  wrf_shutdown.o libmassv.o collect_on_comm.o fast_copy.o microclock.o \
  add_forcing_to_ad.o
ranlib ../main/libwrflib.a

cd ../share
ar ru ../main/libwrflib.a module_bc.o module_bc_time_utilities.o \
  module_io_wrf.o module_date_time.o module_get_file_names.o \
  module_io_domain.o module_model_constants.o module_MPP.o \
  module_wrf_esmf_super.o module_optional_si_input.o module_soil_pre.o \
  mediation_integrate.o mediation_interp_domain.o mediation_force_domain.o \
  mediation_feedback_domain.o mediation_wrfmain.o solve_interface.o \
  solve_interface_ad.o solve_interface_tl.o solve_interface_tst.o \
  start_domain.o init_modules.o set_timekeeping.o Setup_date_string.o \
  interp_fcn.o sint.o input_wrf.o output_wrf.o wrf_ext_write_field.o \
  wrf_ext_read_field.o wrf_inputout.o wrf_auxinput1out.o wrf_auxinput2out.o \
  wrf_auxinput3out.o wrf_auxinput4out.o wrf_auxinput5out.o wrf_histout.o \
  wrf_auxhist1out.o wrf_auxhist2out.o wrf_auxhist3out.o wrf_auxhist4out.o \
  wrf_auxhist5out.o wrf_restartout.o wrf_bdyout.o wrf_inputin.o \
  wrf_auxhist1in.o wrf_auxhist2in.o wrf_auxhist3in.o wrf_auxhist4in.o \
  wrf_auxhist5in.o wrf_auxinput1in.o wrf_auxinput2in.o wrf_auxinput3in.o \
  wrf_auxinput4in.o wrf_auxinput5in.o wrf_bdyin.o wrf_histin.o wrf_restartin.o \
  landread.o
ranlib ../main/libwrflib.a

cd ../phys
ar ru ../main/libwrflib.a module_bl_ysu.o module_bl_mrf.o module_bl_gfs.o \
  module_bl_myjpbl.o module_cu_kf.o module_cu_bmj.o module_cu_kfeta.o \
  module_cu_gd.o module_cu_sas.o module_mp_kessler.o module_mp_ncloud5.o \
  module_mp_lin.o module_mp_ncloud3.o module_mp_wsm3.o module_mp_wsm5.o \
  module_mp_wsm6.o module_mp_etanew.o module_mp_nconvp.o module_ra_sw.o \
  module_ra_gsfcsw.o module_ra_rrtm.o module_ra_gfdleta.o module_sf_sfclay.o \
  module_sf_slab.o module_sf_noahlsm.o module_sf_lsm_nmm.o module_sf_ruclsm.o \
  module_sf_sfcdiags.o module_sf_myjsfc.o module_physics_addtendc.o \
  module_physics_init.o module_gfs_constants.o module_pbl_driver.o \
  module_cumulus_driver.o module_microphysics_driver.o \
  module_radiation_driver.o module_surface_driver.o
ranlib ../main/libwrflib.a


cd ../dyn_em

rm -f solve_em_ad.o
sed -f ../arch/standard.sed solve_em_ad.F > solve_em_ad.b 
/lib/cpp -I../inc -I. -C -P -DRSL_LITE -DDM_PARALLEL -DIWORDSIZE=4 -DDWORDSIZE=8 -DRWORDSIZE=8  -DLWORDSIZE=4 -DNETCDF -DTRIEDNTRUE   -DYYY -DINTIO  -DLIMIT_ARGS -DNO_NAMELIST_PRINT -DNATIVE_MASSV -DSTAND_ALONE -I../external/RSL_LITE `cat ../inc/dm_comm_cpp_flags` -DEM_CORE=1  -DNMM_CORE=0 -DNMM_MAX_DIM=1250  -DCOAMPS_CORE=0  -DEXP_CORE=0  -DWRFVAR  -DNONSTANDARD_SYSTEM -DF90_STANDALONE -DCONFIG_BUF_LEN=8192 -DMAX_DOMAINS_F=5 solve_em_ad.b  > solve_em_ad.f
rm -f solve_em_ad.b
mpxlf90_r -c -qrealsize=8 -qintsize=4 -w -qspill=20000  -qmaxmem=32767 -qinitauto=00  -I../dyn_em          -I../dyn_nmm                        -I../external/io_netcdf -I../external/io_int  -I../external/esmf_time_f90  -I../frame -I../share -I../phys -I../chem -I../inc -qnoopt solve_em_ad.f

ar ru ../main/libwrflib.a module_advect_em.o module_advect_em_ad.o \
  module_advect_em_tl.o module_diffusion_em.o module_diffusion_em_ad.o \
  module_diffusion_em_tl.o module_small_step_em.o module_small_step_em_ad.o \
  module_small_step_em_tl.o module_big_step_utilities_em.o \
  module_big_step_utilities_em_ad.o module_big_step_utilities_em_tl.o \
  module_em.o module_em_ad.o module_em_tl.o module_solvedebug_em.o \
  module_bc_em.o module_bc_em_ad.o module_bc_em_tl.o module_bc_ad.o \
  module_bc_tl.o module_init_utilities.o module_4dvaropt.o init_modules_em.o \
  solve_em.o solve_em_ad.o solve_em_tl.o start_em.o couple_or_uncouple_em.o \
  nest_init_utils.o addroutines.o interp_domain_em.o
ranlib ../main/libwrflib.a

cd ..
make -i -r MODULE_DIRS="-I../dyn_em -I../dyn_nmm -I../external/io_netcdf -I../external/io_int -I../external/esmf_time_f90 -I../frame -I../share -I../phys -I../chem -I../inc" tracing

cd main

rm -f wrf.o
sed -f ../arch/standard.sed wrf.F > wrf.b
/lib/cpp -I../inc -I. -C -P -DRSL_LITE -DDM_PARALLEL -DIWORDSIZE=4 -DDWORDSIZE=8 -DRWORDSIZE=8  -DLWORDSIZE=4 -DNETCDF -DTRIEDNTRUE   -DYYY -DINTIO  -DLIMIT_ARGS -DNO_NAMELIST_PRINT -DNATIVE_MASSV -DSTAND_ALONE -I../external/RSL_LITE `cat ../inc/dm_comm_cpp_flags` -DEM_CORE=1  -DNMM_CORE=0 -DNMM_MAX_DIM=1250  -DCOAMPS_CORE=0  -DEXP_CORE=0  -DWRFVAR  -DNONSTANDARD_SYSTEM -DF90_STANDALONE -DCONFIG_BUF_LEN=8192 -DMAX_DOMAINS_F=5 wrf.b  > wrf.f
rm -f wrf.b
mpxlf90_r -c -O2 -qarch=auto -w -qspill=20000  -qmaxmem=32767 -qinitauto=00  -I../dyn_em          -I../dyn_nmm                        -I../external/io_netcdf -I../external/io_int  -I../external/esmf_time_f90  -I../frame -I../share -I../phys -I../chem -I../inc -qrealsize=8 -qintsize=4 -I.. wrf.f

ranlib libwrflib.a
mpxlf90_r -o wrfplus.exe  wrf.o libwrflib.a -L../external/io_netcdf -lwrfio_nf -L/home/blueice/wrfhelp/external/netcdf/netcdf-3.6.1/xlf_powerpc/lib -lnetcdf -L../external/RSL_LITE -lrsl_lite -lmass -lmassv  -L../external/io_int -lwrfio_int   -L../external/io_grib1 -lio_grib1  ../frame/module_internal_header_util.o ../frame/pack_utils.o -L../external/esmf_time_f90 -lesmf_time 


