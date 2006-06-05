MODULE da_wrfvar_interface

   ! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
   ! Mediation layer modules
   ! Model layer modules
   USE module_model_constants

   ! Registry generated module
   USE module_state_description
   USE da_tracing

CONTAINS

#include "da_wrfvar_interface.inc"

END MODULE da_wrfvar_interface

