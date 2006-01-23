!
SUBROUTINE da_biascorr_rad ( inst, nchan, tb_ob, iv )
!------------------------------------------------------------------------------
!  PURPOSE: perform bias correction for radiance data.
!
!  METHOD:  Tb*(xb)=a+b*Tb(xb)
!
!  HISTORY: 2005/12/13 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)             :: inst       ! sensor index.
   INTEGER, INTENT(IN)             :: nchan      ! number of channel
   REAL,    INTENT(IN)             :: tb_ob(nchan)  ! Observation Tb.
   TYPE (rad_type), INTENT(INOUT)  :: iv         ! O-B structure.

! Local variables

   INTEGER   :: i,iscan
   REAL      :: tb_xb

   iscan = iv%scanpos
   do i=1,nchan
     tb_xb = satinfo(inst)%a(iscan,i) + satinfo(inst)%b(iscan,i)*iv%tb_xb(i)
     iv%tb(i)%inv = tb_ob(i) - tb_xb
   end do

   return

END SUBROUTINE da_biascorr_rad
