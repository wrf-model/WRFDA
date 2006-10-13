subroutine da_qc_rad (inst, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! PURPOSE: perform quality control for radiance data.
   !
   ! METHOD:  seperated QC for each sensor
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)             :: inst       ! sensor index.
   INTEGER, INTENT(IN)             :: nchan      ! number of channel
   TYPE (y_type),  INTENT(IN)      :: ob         ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)   :: iv         ! O-B structure.

   logical   :: amsua, amsub, hirs, msu,airs,hsb

#ifdef RTTOV
   if (trace_use) call da_trace_entry("da_qc_rad")
   
   amsua = trim(inst_name(rtminit_sensor(inst))) == 'amsua'
   amsub = trim(inst_name(rtminit_sensor(inst))) == 'amsub'
   hirs  = trim(inst_name(rtminit_sensor(inst))) == 'hirs'
   msu   = trim(inst_name(rtminit_sensor(inst))) == 'msu'
   airs  = trim(inst_name(rtminit_sensor(inst))) == 'airs'
   hsb   = trim(inst_name(rtminit_sensor(inst))) == 'hsb'

   if (hirs) then
      ! 1.0 QC for HIRS
      ! call da_qc_hirs(inst,nchan,ob,iv)
      WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for HIRS' 
   else if (airs) then
      ! call da_qc_airs(inst,nchan,ob,iv)
      WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for AIRS'
   else if ( hsb ) then
      ! call da_qc_hsb(inst,nchan,ob,iv)
      WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for HSB'
   else if (amsua) then
      !  2.0 QC for AMSU-A
      call da_qc_amsua(inst,nchan,ob,iv)
   else if ( amsub ) then
      ! 3.0 QC for AMSU-B
      call da_qc_amsub(inst,nchan,ob,iv)
   else if (msu) then
      ! call da_qc_msu(inst,nchan, ob,iv)
      WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for MSU'
   else
      WRITE(UNIT=errmsg(1),FMT='(A,A)') &
         "Unrecognized Instrument",TRIM(inst_name(rtminit_sensor(inst)))
      call da_error(__FILE__,__LINE__,errmsg(1:1))
   end if

   if (trace_use) call da_trace_exit("da_qc_rad")
#endif

end subroutine da_qc_rad


