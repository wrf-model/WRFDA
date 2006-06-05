!
SUBROUTINE da_qc_rad ( inst, nchan, tb_ob, iv )
!------------------------------------------------------------------------------
!  PURPOSE: perform quality control for radiance data.
!
!  METHOD:  seperated QC for each sensor
!
!  HISTORY: 2005/12/14 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   USE module_wrf_error

   IMPLICIT NONE

   INTEGER, INTENT(IN)             :: inst       ! sensor index.
   INTEGER, INTENT(IN)             :: nchan      ! number of channel
   REAL,    INTENT(IN)             :: tb_ob(nchan)  ! Observation Tb.
   TYPE (rad_type), INTENT(INOUT)  :: iv         ! O-B structure.

! local variables

   logical   :: amsua, amsub, hirs, msu,airs,hsb
   
     amsua = trim( inst_name(rtminit_sensor(inst))) == 'amsua'
     amsub = trim( inst_name(rtminit_sensor(inst))) == 'amsub'
     hirs  = trim( inst_name(rtminit_sensor(inst))) == 'hirs'
     msu   = trim( inst_name(rtminit_sensor(inst))) == 'msu'
     airs  = trim( inst_name(rtminit_sensor(inst))) == 'airs'
     hsb   = trim( inst_name(rtminit_sensor(inst))) == 'hsb'

!  1.0 QC for HIRS
!------------------------------
   if ( hirs ) then

   !   call da_qc_hirs(inst,nchan,tb_ob,iv)
!     print *, 'QC Not impemented for HIRS' 
     return
   else if ( airs ) then

   !   call da_qc_airs(inst,nchan,tb_ob,iv)
!     print *, 'QC Not impemented for AIRS'
      return

   else if ( hsb ) then

   !   call da_qc_hsb(inst,nchan,tb_ob,iv)
!     print *, 'QC Not impemented for HSB'
      return

!  2.0 QC for AMSU-A
!------------------------------
   else if ( amsua ) then

      call da_qc_amsua(inst,nchan,tb_ob,iv)

!  3.0 QC for AMSU-B
!------------------------------
   else if ( amsub ) then

      call da_qc_amsub(inst,nchan,tb_ob,iv)

   else if ( msu ) then

   !   call da_qc_msu(inst,nchan,tb_ob,iv)
      WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for MSU'

   else
     WRITE(UNIT=errmsg(1),FMT='(A,A)') &
       "Unrecognized Instrument",TRIM(inst_name(rtminit_sensor(inst)))
     CALL wrf_error_fatal(errmsg(1))
   end if

   return

END SUBROUTINE da_qc_rad
