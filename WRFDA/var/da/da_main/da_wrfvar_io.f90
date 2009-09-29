module da_wrfvar_io

   use module_configure, only : grid_config_rec_type
   use module_date_time, only : geth_julgmt,current_date, start_date
   use module_domain, only : domain
   use module_io_domain, only : open_r_dataset,close_dataset, &
      input_model_input, open_w_dataset,output_model_input, &
      input_boundary, output_boundary, output_aux_hist4, &
      input_aux_hist6, input_aux_hist4

   use da_control, only : trace_use,ierr,var4d_lbc
   use da_reporting, only : da_error, message, da_message
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace

contains

#include "da_med_initialdata_input.inc"
#include "da_med_initialdata_output.inc"
#include "da_med_initialdata_output_lbc.inc"
#include "da_med_hist_out4.inc"
#include "da_med_hist_in4.inc"
#include "da_med_hist_in6.inc"
#include "da_med_boundary_input.inc"

end module da_wrfvar_io
