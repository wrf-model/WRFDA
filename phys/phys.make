# PHYS

PHYS_MODULES = \
	module_bl_ysu.o \
	module_bl_mrf.o \
	module_bl_gfs.o \
	module_bl_myjpbl.o \
	module_cu_kf.o  \
	module_cu_bmj.o \
	module_cu_kfeta.o \
	module_cu_gd.o \
	module_cu_sas.o \
	module_mp_kessler.o \
	module_mp_ncloud5.o \
	module_mp_lin.o  \
	module_mp_ncloud3.o \
	module_mp_wsm3.o \
	module_mp_wsm5.o \
	module_mp_wsm6.o \
	module_mp_etanew.o \
	module_mp_thompson.o \
	module_ra_sw.o  \
	module_ra_gsfcsw.o \
	module_ra_rrtm.o  \
	module_ra_gfdleta.o \
	module_sf_sfclay.o \
	module_sf_gfs.o \
	module_sf_slab.o  \
	module_sf_noahlsm.o  \
	module_sf_ruclsm.o \
	module_sf_sfcdiags.o \
	module_sf_myjsfc.o \
	module_physics_addtendc.o \
	module_physics_init.o \
	module_gfs_machine.o \
	module_gfs_funcphys.o \
	module_gfs_physcons.o \
	module_progtm.o \
	module_pbl_driver.o \
	module_cumulus_driver.o \
	module_microphysics_driver.o \
	module_microphysics_zero_out.o \
	module_radiation_driver.o \
	module_surface_driver.o
  
# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

module_bl_myjpbl.o: module_model_constants.o

module_bl_gfs.o: module_gfs_machine.o \
		 module_gfs_physcons.o

module_cu_bmj.o: module_model_constants.o

module_cu_kf.o:  module_wrf_error.o

module_cu_kfeta.o: module_wrf_error.o

module_cu_gd.o:

module_gfs_physcons.o: module_gfs_machine.o

module_gfs_funcphys.o: module_gfs_machine.o \
		       module_gfs_physcons.o

module_cu_sas.o: module_gfs_machine.o \
		 module_gfs_funcphys.o \
		 module_gfs_physcons.o

module_ra_gfdleta.o:  module_dm.o

module_ra_rrtm.o: module_wrf_error.o \
		module_dm.o

module_mp_lin.o : module_wrf_error.o

module_sf_lsm_nmm.o: module_model_constants.o \
		module_MPP.o

module_sf_myjsfc.o: module_model_constants.o 

module_sf_gfs.o: module_gfs_machine.o \
		 module_gfs_funcphys.o \
		 module_gfs_physcons.o \
		 module_progtm.o

module_sf_noahlsm.o: module_model_constants.o 

module_sf_ruclsm.o: module_wrf_error.o

module_physics_addtendc.o: \
		module_cu_kf.o			\
		module_cu_kfeta.o		\
		module_state_description.o \
		module_configure.o

module_physics_init.o : \
		module_ra_rrtm.o		\
		module_ra_sw.o			\
		module_ra_gsfcsw.o		\
		module_ra_gfdleta.o		\
		module_sf_sfclay.o		\
		module_sf_slab.o		\
		module_sf_myjsfc.o		\
		module_sf_noahlsm.o		\
		module_sf_ruclsm.o		\
		module_bl_ysu.o			\
		module_bl_mrf.o			\
		module_bl_gfs.o			\
		module_bl_myjpbl.o		\
		module_cu_kf.o			\
		module_cu_kfeta.o		\
		module_cu_bmj.o			\
		module_cu_gd.o			\
		module_cu_sas.o			\
		module_mp_ncloud3.o		\
		module_mp_ncloud5.o		\
		module_mp_wsm3.o		\
		module_mp_wsm5.o		\
		module_mp_wsm6.o		\
		module_mp_etanew.o		\
		module_mp_thompson.o            \
		module_state_description.o \
		module_configure.o \
		module_domain.o \
		module_wrf_error.o \
		module_dm.o \
		module_model_constants.o \
		module_date_time.o	

module_microphysics_driver.o: \
                module_mp_kessler.o module_mp_lin.o \
		module_mp_ncloud3.o module_mp_ncloud5.o \
		module_mp_wsm3.o module_mp_wsm5.o \
		module_mp_wsm6.o module_mp_etanew.o \
		module_mp_thompson.o            \
		module_state_description.o \
		module_wrf_error.o \
		module_configure.o \
		module_model_constants.o 

module_cumulus_driver.o: \
                module_cu_kf.o \
		module_cu_kfeta.o \
		module_cu_bmj.o \
		module_cu_gd.o \
		module_cu_sas.o \
		module_state_description.o \
		module_configure.o \
		module_model_constants.o

module_pbl_driver.o:  \
		module_bl_myjpbl.o \
		module_bl_ysu.o \
		module_bl_mrf.o \
		module_bl_gfs.o \
		module_state_description.o \
		module_configure.o \
		module_model_constants.o 

module_radiation_driver.o: \
		module_ra_sw.o \
		module_ra_gsfcsw.o \
		module_ra_rrtm.o \
		module_ra_gfdleta.o \
		module_state_description.o \
		module_wrf_error.o \
		module_configure.o \
		module_model_constants.o 

module_surface_driver.o: \
		module_sf_sfclay.o		\
		module_sf_slab.o		\
		module_sf_myjsfc.o		\
		module_sf_gfs.o  		\
		module_sf_noahlsm.o		\
		module_sf_ruclsm.o		\
		module_sf_sfcdiags.o		\
		module_state_description.o \
		module_configure.o \
		module_model_constants.o

# DO NOT DELETE
