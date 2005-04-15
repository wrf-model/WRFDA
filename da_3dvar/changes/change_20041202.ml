Change Log for update to WRF 3DVAR code
---------------------------------------

Author: 	Mi_Seon Lee
Reviewer: 	Wei Huang
Date: 		12/02/04

Reason for changes (explain before making changes)
--------------------------------------------------

Add back "random cv" utility.

Expected Differences
--------------------

No impact on 3D-Var (run as an option).

Test results (run on what machines?)
------------------------------------------------------

Files removed:
--------------

None.

Files added:
------------

 da_3dvar/src/DA_Tools/da_set_randomcv.inc

Files modified:
---------------

 da_3dvar/src/Makefile
 da_3dvar/src/DA_Define_Structures/da_allocate_cv.inc
 da_3dvar/src/DA_Obs/da_add_noise_to_ob.inc
 da_3dvar/src/DA_Tools/DA_Tools.F
 da_3dvar/src/da_solve_v3d/da_solve_v3d.F

Minor-tag:

cvs -r tag random_cv

