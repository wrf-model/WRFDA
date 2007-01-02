Change Log for update to WRF 3DVAR code
---------------------------------------

Author: Hans Huang (4dvar), Wei Huang (wrf frame), 
        Qingnong Xiao (wrf sn/tl/ad/tst), ZaiZhong Ma (wrf adjoint testing)

Reviewer: Dale Barker
Date: 04/23/05

Reason for changes (explain before making changes)
--------------------------------------------------

Add 4DVAR capability.

Expected Differences
--------------------

Should have no impact on 3DVAR.

Test results (run on what machines?)
------------------------------------------------------

On zebra.

Case 2000-01-25_01:00:00
 &domains
 e_we                                = 74, 74, 74,
 e_sn                                = 61, 61, 61,
 e_vert                              = 28, 28, 28,
 dx                                  = 30000, 30000, 30000,
 dy                                  = 30000, 30000, 30000,

Using the new version:

  Original gradient is  4.32398451D+02
  For this outer iteration Gradient target is  4.32398451D+00
  Starting Cost function:  6.27877849D+03 Gradient=  4.32398451D+02
 ----------------------------------------------------------

   Iter    Cost Function         Gradient             Step

    1      6.13840509D+03      2.80155327D+02      1.50157330D-03
    2      5.69040888D+03      1.69647011D+02      1.14158056D-02
    3      5.49430408D+03      8.02557033D+01      1.36278016D-02
    4      5.33391009D+03      8.78114467D+01      4.98042336D-02
    5      5.19615983D+03      5.37494173D+01      3.57289424D-02
    6      5.12404836D+03      3.75465696D+01      4.99214077D-02
    7      5.07956382D+03      2.86619840D+01      6.31100529D-02
    8      5.05859566D+03      2.38331228D+01      5.10479062D-02
    9      5.04738268D+03      1.56814131D+01      3.94811032D-02
   10      5.04687171D+03      2.67655479D+01      4.15575701D-03
   11      5.03781073D+03      9.65480388D+00      2.52960666D-02
   12      5.03461311D+03      7.40109580D+00      6.86072034D-02
   13      5.03357425D+03      5.81456245D+00      3.79311441D-02
   14      5.03261649D+03      3.46698827D+00      5.66565380D-02
 ----------------------------------------------------------
  Inner iteration stopped after   14 iterations
   Final cost function :  5.03261649D+03 and Gradient:  3.46698827D+00
 ----------------------------------------------------------

    Final cost function J  =    5032.61649352086

    Total number of obs.    =     1609
    Final 3DVAR value of J  =      5032.61649
    Final 3DVAR value of Jo =      4823.80726
    Final 3DVAR value of Jb =       208.80924
    Final J / total num_obs =         3.12779


Using the wrfvar (co on 04/21/05):

  Original gradient is  4.32398451D+02
  For this outer iteration Gradient target is  4.32398451D+00
  Starting Cost function:  6.27877849D+03 Gradient=  4.32398451D+02
 ----------------------------------------------------------

   Iter    Cost Function         Gradient             Step

    1      6.13840509D+03      2.80155327D+02      1.50157330D-03
    2      5.69040888D+03      1.69647011D+02      1.14158056D-02
    3      5.49430408D+03      8.02557033D+01      1.36278016D-02
    4      5.33391009D+03      8.78114467D+01      4.98042336D-02
    5      5.19615983D+03      5.37494173D+01      3.57289424D-02
    6      5.12404836D+03      3.75465696D+01      4.99214077D-02
    7      5.07956382D+03      2.86619840D+01      6.31100529D-02
    8      5.05859566D+03      2.38331228D+01      5.10479062D-02
    9      5.04738268D+03      1.56814131D+01      3.94811032D-02
   10      5.04687171D+03      2.67655479D+01      4.15575701D-03
   11      5.03781073D+03      9.65480388D+00      2.52960666D-02
   12      5.03461311D+03      7.40109580D+00      6.86072034D-02
   13      5.03357425D+03      5.81456245D+00      3.79311441D-02
   14      5.03261649D+03      3.46698827D+00      5.66565380D-02
 ----------------------------------------------------------
  Inner iteration stopped after   14 iterations
   Final cost function :  5.03261649D+03 and Gradient:  3.46698827D+00
 ----------------------------------------------------------

    Final cost function J  =    5032.61649352086

    Total number of obs.    =     1609
    Final 3DVAR value of J  =      5032.61649
    Final 3DVAR value of Jo =      4823.80726
    Final 3DVAR value of Jb =       208.80924
    Final J / total num_obs =         3.12779

Old code: Mem = 369M, cpu = real 130.9 user 98.9 sys 1.7
New code: Mem = 370M, cpu = real  99.1 user 97.7 sys 1.2

Note on the memory increase: 
The memory increase is not necessary. It is due to the tl (g_) and ad (a_) variables used in the wrf
tl/ad, which are not a part of 3dvar. Work is ongoing to bring the memory dwon. 

Files removed 
--------------

Files removed due to WRF change (there is no more io_diff and
io_quilt directory on WRF

R external/io_diff/diffwrf.F
R external/io_diff/diffwrf.F90
R external/io_diff/diffwrf8.F
R external/io_diff/diffwrf8.F90
R external/io_diff/makefile
R external/io_quilt/collect_on_comm.c
R external/io_quilt/intio_tags.h
R external/io_quilt/io_quilt.F90
R external/io_quilt/makefile
R external/io_quilt/module_quilt_outbuf_ops.F
R external/io_quilt/wrf_io_flags.h
R main/gen_be/Makefile
R main/gen_be/gen_be_cov2d.f
R main/gen_be/gen_be_cov3d.f
R main/gen_be/gen_be_diags.f
R main/gen_be/gen_be_stage1.f
R main/gen_be/gen_be_stage2.f
R main/gen_be/gen_be_stage3.f
R main/gen_be/gen_be_stage4.f
R main/gen_be/gen_spectra.F

Files added:
------------

A da_3dvar/src/DA_Minimisation/DA_Transform_VToY.inc
A da_3dvar/src/DA_Minimisation/DA_Transform_VToY_Adj.inc
A da_3dvar/src/DA_Setup_Structures/da_transfer_wrftltoxa.inc
A da_3dvar/src/DA_Setup_Structures/da_transfer_wrftltoxa_adj.inc
A da_3dvar/src/DA_Setup_Structures/da_transfer_xatowrftl.inc
A da_3dvar/src/DA_Setup_Structures/da_transfer_xatowrftl_adj.inc

4dvar new routines outside da_3dvar

to get some simple statistics
A dyn_em/addroutines.F

wrf sn, tl, ad and tst routines
A dyn_em/module_advect_em_ad.F
A dyn_em/module_advect_em_sn.F
A dyn_em/module_advect_em_tl.F
A dyn_em/module_bc_ad.F
A dyn_em/module_bc_em_ad.F
A dyn_em/module_bc_em_sn.F
A dyn_em/module_bc_em_tl.F
A dyn_em/module_bc_sn.F
A dyn_em/module_bc_tl.F
A dyn_em/module_big_step_utilities_em_ad.F
A dyn_em/module_big_step_utilities_em_sn.F
A dyn_em/module_big_step_utilities_em_tl.F
A dyn_em/module_check.F
A dyn_em/module_diffusion_em_ad.F
A dyn_em/module_diffusion_em_sn.F
A dyn_em/module_diffusion_em_tl.F
A dyn_em/module_em_ad.F
A dyn_em/module_em_sn.F
A dyn_em/module_em_tl.F
A dyn_em/module_small_step_em_ad.F
A dyn_em/module_small_step_em_sn.F
A dyn_em/module_small_step_em_tl.F
A dyn_em/solve_em_ad.F
A dyn_em/solve_em_sn.F
A dyn_em/solve_em_tst.F

4dvar related
A frame/add_forcing_to_ad.F
A frame/module_integrate_ad.F
A frame/module_integrate_tst.F
A share/Setup_ad_date_string.F
A share/mediation_integrate_ad.F
A share/mediation_integrate_sn.F
A share/mediation_integrate_tl.F
A share/solve_em_ad.int
A share/solve_em_tst.int
A share/solve_interface_ad.F
A share/solve_interface_tst.F

scripts called from 4dvar
A scripts/runvar4d.csh
A scripts/runvar4dad.csh
A scripts/runvar4dnl.csh
A scripts/runvar4dtl.csh

Files added (Wei: do we need these. they are not due to 4dvar.):
------------

These are for changing "3dvar" to "wrfvar" (not due to 4dvar):

A arch/Config_wrfvar.pl
A arch/configure.defaults_wrfvar
A arch/postamble_wrfvar
A arch/postamble_wrfvar.crayx1
A arch/postamble_wrfvar.mac_g4
A arch/preamble_wrfvar
A da_3dvar/changes/change_20050215.wh

These are due to WRF's new changes:

A dyn_nmm/BUCKETS.F
A dyn_nmm/module_MPP.F
A dyn_nmm/module_PHYSICS_CALLS.F
A dyn_nmm/module_initialize_real.F
A dyn_nmm/module_si_io_nmm.F
A main/real_nmm.F
A main/wrf_ESMFApp.F
A phys/module_bl_gfs.F
A phys/module_cu_sas.F
A phys/module_gfs_constants.F
A share/landread.c
A share/landread.h
A test/em_real/landFilenames
A test/nmm_real/gribmap.txt
A tools/all_reg.csh

Files modified:
---------------

M da_3dvar/src/Makefile
M da_3dvar/src/DA_Constants/DA_Constants.F
M da_3dvar/src/DA_Minimisation/DA_Minimisation.F
M da_3dvar/src/DA_Minimisation/DA_Minimise_CG.inc
M da_3dvar/src/DA_Minimisation/DA_Minimise_QN.inc
M da_3dvar/src/DA_Minimisation/da_calculate_j.inc
M da_3dvar/src/DA_Minimisation/da_minimise.inc
M da_3dvar/src/da_solve_v3d/da_solve_v3d.F
M da_3dvar/src/DA_Setup_Structures/DA_Setup_Structures.F
M da_3dvar/src/DA_Setup_Structures/DA_Transfer_XatoAnalysis.inc
M da_3dvar/src/DA_Tools/DA_Read_Namelist.inc

Minor changes to break obs operators to time windows

M da_3dvar/src/DA_Airep/DA_Transform_XToY_Airep.inc
M da_3dvar/src/DA_Airep/DA_Transform_XToY_Airep_Adj.inc
M da_3dvar/src/DA_Buoy/DA_Transform_XToY_Buoy.inc
M da_3dvar/src/DA_Buoy/DA_Transform_XToY_Buoy_Adj.inc
M da_3dvar/src/DA_Gpsref/DA_Transform_XToY_Gpsref.inc
M da_3dvar/src/DA_Gpsref/DA_Transform_XToY_Gpsref_Adj.inc
M da_3dvar/src/DA_Metar/DA_Transform_XToY_Metar.inc
M da_3dvar/src/DA_Metar/DA_Transform_XToY_Metar_Adj.inc
M da_3dvar/src/DA_Pilot/DA_Transform_XToY_Pilot.inc
M da_3dvar/src/DA_Pilot/DA_Transform_XToY_Pilot_Adj.inc
M da_3dvar/src/DA_Profiler/DA_Transform_XToY_Profiler.inc
M da_3dvar/src/DA_Profiler/DA_Transform_XToY_Profiler_Adj.inc
M da_3dvar/src/DA_Radar/DA_Transform_XToY_Radar.inc
M da_3dvar/src/DA_Radar/DA_Transform_XToY_Radar_Adj.inc
M da_3dvar/src/DA_Satem/DA_Transform_XToY_Satem.inc
M da_3dvar/src/DA_Satem/DA_Transform_XToY_Satem_Adj.inc
M da_3dvar/src/DA_Satob/DA_Transform_XToY_Satob.inc
M da_3dvar/src/DA_Satob/DA_Transform_XToY_Satob_Adj.inc
M da_3dvar/src/DA_Ships/DA_Transform_XToY_Ships.inc
M da_3dvar/src/DA_Ships/DA_Transform_XToY_Ships_Adj.inc
M da_3dvar/src/DA_Sound/DA_Sonde_sfc/DA_Transform_XToY_Sonde_sfc.inc
M da_3dvar/src/DA_Sound/DA_Sonde_sfc/DA_Transform_XToY_Sonde_sfc_Adj.inc
M da_3dvar/src/DA_Sound/DA_Transform_XToY_Sound.inc
M da_3dvar/src/DA_Sound/DA_Transform_XToY_Sound_Adj.inc
M da_3dvar/src/DA_Synop/DA_Transform_XToY_Synop.inc
M da_3dvar/src/DA_Synop/DA_Transform_XToY_Synop_Adj.inc
M da_3dvar/src/da_qscat/da_transform_xtoy_qscat.inc
M da_3dvar/src/da_qscat/da_transform_xtoy_qscat_adj.inc

4dvar changes outside da_3dvar

M dyn_em/Makefile
M dyn_em/module_big_step_utilities_em.F
M dyn_em/module_em.F
M dyn_em/module_initialize_real.F
M dyn_em/nest_init_utils.F
M dyn_em/solve_em.F
M dyn_em/solve_em_tl.F
M dyn_em/start_em.F

These are due to 4dvar changes:

M Registry/Registry.3DVAR
M Registry/Registry.EM
M frame/module_integrate_tl.F
M share/solve_interface_tl.F

These are due to WRF changes:

M README.NMM
M Registry/Registry.EM.PET
M Registry/Registry.EM_CHEM
M Registry/Registry.NMM
M arch/configure.defaults
M arch/preamble
M arch/preamble_3dvar
M dyn_nmm/Makefile
M dyn_nmm/module_ADVECTION.F
M dyn_nmm/module_BNDRY_COND.F
M dyn_nmm/module_CTLBLK.F
M dyn_nmm/module_DIFFUSION_NMM.F
M dyn_nmm/module_IGWAVE_ADJUST.F
M dyn_nmm/module_MPPINIT.F
M dyn_nmm/module_NONHY_DYNAM.F
M dyn_nmm/read_nmm.F
M dyn_nmm/solve_nmm.F
M dyn_nmm/start_domain_nmm.F
M external/Makefile
M external/RSL/RSL/makefile.core
M external/RSL/RSL/rsl.h
M external/RSL/RSL/rsl_bcast.c
M external/RSL/gen_comms.c
M external/RSL/module_dm.F
M external/RSL_LITE/module_dm.F
M external/RSL_LITE/rsl_lite.h
M external/esmf_time_f90/ESMF_Clock.F90
M external/esmf_time_f90/ESMF_TimeInterval.F90
M external/esmf_time_f90/Meat.F90
M external/io_int/diffwrf.F
M external/io_netcdf/diffwrf.F90
M frame/Makefile
M frame/libmassv.F
M frame/module_domain.F
M main/Makefile
M main/ideal.F
M main/real_em.F
M phys/Makefile
M phys/module_bl_mrf.F
M phys/module_bl_myjpbl.F
M phys/module_bl_ysu.F
M phys/module_cu_bmj.F
M phys/module_cu_gd.F
M phys/module_cu_kf.F
M phys/module_cu_kfeta.F
M phys/module_cumulus_driver.F
M phys/module_mp_etanew.F
M phys/module_pbl_driver.F
M phys/module_physics_init.F
M phys/module_ra_gfdleta.F
M phys/module_radiation_driver.F
M phys/module_sf_lsm_nmm.F
M phys/module_sf_myjsfc.F
M phys/module_sf_sfclay.F
M phys/module_surface_driver.F
M run/README.namelist
M run/gribmap.txt
M run/namelist.3dvar
M run/namelist.input
M share/Makefile
M share/input_wrf.F
M share/mediation_feedback_domain.F
M share/mediation_force_domain.F
M share/mediation_integrate.F
M share/mediation_interp_domain.F
M share/mediation_wrfmain.F
M share/module_date_time.F
M share/module_io_domain.F
M share/module_optional_si_input.F
M share/module_soil_pre.F
M share/solve_interface.F
M share/start_domain.F
M test/em_real/namelist.input
M tools/data.h
M tools/gen_allocs.c
M tools/gen_args.c
M tools/gen_defs.c
M tools/gen_mod_state_descr.c
M tools/gen_scalar_indices.c
M tools/gen_wrf_io.c
M tools/reg_parse.c
M tools/registry.c
M tools/regtest.csh
M tools/testomatic

tag info: in vda
---------------
ml_v_0_5


