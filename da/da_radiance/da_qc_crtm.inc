subroutine da_qc_crtm (ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for radiance data.
   !
   ! METHOD:  seperated QC for each sensor
   !---------------------------------------------------------------------------

   implicit none

   type (y_type),  intent(in)      :: ob         ! Observation structure.
   type (ob_type), intent(inout)   :: iv         ! O-B structure.

   integer :: i, nchan,p,j
   logical   :: amsua, amsub, hirs, msu,airs,hsb

   if (trace_use) call da_trace_entry("da_qc_crtm")

   do i = 1, iv%num_inst
      nchan    = iv%instid(i)%nchan

      amsua = trim(rttov_inst_name(rtminit_sensor(i))) == 'amsua'
      amsub = trim(rttov_inst_name(rtminit_sensor(i))) == 'amsub'
      hirs  = trim(rttov_inst_name(rtminit_sensor(i))) == 'hirs'
      msu   = trim(rttov_inst_name(rtminit_sensor(i))) == 'msu'
      airs  = trim(rttov_inst_name(rtminit_sensor(i))) == 'airs'
      hsb   = trim(rttov_inst_name(rtminit_sensor(i))) == 'hsb'

      if (hirs) then
         ! 1.0 QC for HIRS
         ! call da_qc_hirs(i,nchan,ob,iv)
         call da_warning(__FILE__,__LINE__,(/'QC Not implemented for HIRS'/))
      else if (airs) then
         ! call da_qc_airs(i,nchan,ob,iv)
         call da_warning(__FILE__,__LINE__,(/'QC Not implemented for AIRS'/))
      else if ( hsb ) then
         ! call da_qc_hsb(i,nchan,ob,iv)
         call da_warning(__FILE__,__LINE__,(/'QC Not implemented for HSB'/))
      else if (amsua) then
         call da_qc_amsua(i,nchan,ob,iv)
      else if ( amsub ) then
         call da_qc_amsub(i,nchan,ob,iv)
      else if (msu) then
         ! call da_qc_msu(i,nchan, ob,iv)
         call da_warning(__FILE__,__LINE__,(/'QC Not implemented for MSU'/))
      else
         write(unit=message(1),fmt='(A,A)') &
            "Unrecognized instrument",trim(inst_name(rtminit_sensor(i)))
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

   end do

   if (trace_use) call da_trace_exit("da_qc_crtm")

end subroutine da_qc_crtm


