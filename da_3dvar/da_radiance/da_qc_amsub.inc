!
SUBROUTINE da_qc_amsub ( inst, nchan, tb_ob, rad )
!------------------------------------------------------------------------------
!  PURPOSE: perform quality control for amsub data.
!
!  METHOD:  
!
!  HISTORY: 2005/12/14 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)             :: inst       ! sensor index.
   INTEGER, INTENT(IN)             :: nchan      ! number of channel
   REAL,    INTENT(IN)             :: tb_ob(nchan)  ! Observation Tb.
   TYPE (rad_type), INTENT(INOUT)  :: rad         ! O-B structure.

! local variables
   integer   :: scanpos,k,isflg
   logical   :: lmix
   real      :: satzen,si

!  0.0  initialise QC flags by assuming good obs
!---------------------------------------------
      rad%tb(:)%qc = 1

!  a.  reject all channels over mixture surface type
!------------------------------------------------------
      isflg = rad%isflg 
      lmix  = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
      if (lmix) rad%tb(:)%qc  =  -1

!  b.  reject channels 1~2 over land/sea-ice/snow
!------------------------------------------------------
      if ( isflg > 0 ) rad%tb(1:2)%qc    = -1

!    reject limb obs 
!------------------------------------------------------
       scanpos = rad%scanpos
!      if ( scanpos <= 8 .or. scanpos >= 83 ) iv%instid(inst)%rad(n)%tb(:)%qc  =  -1

!      satzen  = rad%satzen
!      if ( abs(satzen) > 45. ) iv%instid(inst)%rad(n)%tb(:)%qc  =  -1

!  d. check cloud/precipitation 
!-----------------------------------------------------------
     if ( tb_ob(1) > 0. .and. tb_ob(2) > 0.) then
       si = tb_ob(1) - tb_ob(2)
       if ( si >= 3. ) rad%tb(:)%qc = -1 
     end if

     if ( rad%clwp >= 0.2 ) rad%tb(:)%qc = -1

!  e. check surface height/pressure
!-----------------------------------------------------------
!      sfchgt = ivrad%rad(i)%info%elv
!      if ( sfchgt >= ) then
!
!      else 

!      end if

       if ( (isflg .ne. 0) .and. (rad%ps < 800.) ) rad%tb(5)%qc  = -1

!  f. check innovation
!-----------------------------------------------------------
         do k = 1, nchan
          if ( abs(rad%tb(k)%inv) > 15. ) then
            rad%tb(k)%qc  = -1
          end if

        if (lbiascorr) then
          if ( abs(rad%tb(k)%inv) > 3.*satinfo(inst)%std(scanpos,k) ) then
            rad%tb(k)%qc  = -1
          end if
        else
          if ( abs(rad%tb(k)%inv) > 3.*satinfo(inst)%rms(scanpos,k) ) then
            rad%tb(k)%qc  = -1
          end if
        end if

          if (rad%tb(k)%qc == -1) then
            rad%tb(k)%error = 500.0
          else
            if (lbiascorr) then
               rad%tb(k)%error = satinfo(inst)%std(scanpos,k)
            else
               rad%tb(k)%error = satinfo(inst)%rms(scanpos,k)
            end if
          end if
         end do

   return

END SUBROUTINE da_qc_amsub
