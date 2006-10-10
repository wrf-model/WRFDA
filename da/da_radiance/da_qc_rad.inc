!
subroutine da_qc_rad ( inst, nchan, ob, iv )
!------------------------------------------------------------------------------
!  PURPOSE: perform quality control for radiance data.
!
!  METHOD:  seperated QC for each sensor
!
!  HISTORY: 2005/12/14 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)             :: inst       ! sensor index.
   INTEGER, INTENT(IN)             :: nchan      ! number of channel
   TYPE (y_type),  INTENT(IN)      :: ob         ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)   :: iv         ! O-B structure.

! local variables

   logical   :: amsua, amsub, hirs, msu,airs,hsb

#ifdef RTTOV
   
   amsua = trim( inst_name(rtminit_sensor(inst))) == 'amsua'
   amsub = trim( inst_name(rtminit_sensor(inst))) == 'amsub'
   hirs  = trim( inst_name(rtminit_sensor(inst))) == 'hirs'
   msu   = trim( inst_name(rtminit_sensor(inst))) == 'msu'
   airs  = trim( inst_name(rtminit_sensor(inst))) == 'airs'
   hsb   = trim( inst_name(rtminit_sensor(inst))) == 'hsb'

!  1.0 QC for HIRS
!------------------------------
   if ( hirs ) then

!     call da_qc_hirs(inst,nchan,ob,iv)
      WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for HIRS' 

   else if ( airs ) then

!     call da_qc_airs(inst,nchan,ob,iv)
      WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for AIRS'

   else if ( hsb ) then

!     call da_qc_hsb(inst,nchan,ob,iv)
     WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for HSB'

!  2.0 QC for AMSU-A
!------------------------------
   else if ( amsua ) then

      call da_qc_amsua(inst,nchan,ob,iv)

!  3.0 QC for AMSU-B
!------------------------------
   else if ( amsub ) then

      call da_qc_amsub(inst,nchan,ob,iv)

   else if ( msu ) then

!     call da_qc_msu(inst,nchan, ob,iv)
     WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for MSU'

   else

     WRITE(UNIT=errmsg(1),FMT='(A,A)') &
       "Unrecognized Instrument",TRIM(inst_name(rtminit_sensor(inst)))
     call da_error(__FILE__,__LINE__,errmsg(1:1))

   end if

   return
#endif

end subroutine da_qc_rad
