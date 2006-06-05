!
SUBROUTINE da_qc_amsua ( inst, nchan, tb_ob, rad )
!------------------------------------------------------------------------------
!  PURPOSE: perform quality control for amsua data.
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

!  0.0  initialise QC by flags assuming good obs
!---------------------------------------------
      rad%tb(:)%qc = 1

!  a.  reject all channels over mixture surface type
!------------------------------------------------------
      isflg = rad%isflg 
      lmix  = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
      if (lmix) rad%tb(:)%qc  =  -1

!  b.  reject channels 1~4 over land/sea-ice/snow
!------------------------------------------------------
      if ( isflg > 0 ) rad%tb(1:4)%qc    = -1 

!  c.  reject channels 13,14(above top model 10mb),15 
!------------------------------------------------------
        rad%tb(13:15)%qc    = -1

!    reject limb obs 
!------------------------------------------------------
       scanpos = rad%scanpos
!      if ( scanpos <= 3 .or. scanpos >= 28 ) iv%instid(inst)%rad(n)%tb(:)%qc  =  -1

!      satzen  = rad%satzen
!      if ( abs(satzen) > 45. ) iv%instid(inst)%rad(n)%tb(:)%qc  =  -1

!  d. check precipitation 
!-----------------------------------------------------------
     if ( tb_ob(1) > 0. .and. tb_ob(15) > 0.) then
       si = tb_ob(1) - tb_ob(15)
       if ( si >= 3. ) rad%tb(:)%qc = -1 
     end if

     if ( rad%clwp >= 0.2 ) rad%tb(:)%qc = -1

!   3.1 Estimate Cloud Liquid Water (CLW) in mm over sea
!       ( Grody etal. 2001, JGR, Equation 5b,7c,7d,9 )
!---------------------------------------------------------
! if (isflg == 0) then
!    coszen =  cos(iv%instid(inst)%rad(n)%satzen)
!    d0     =  8.24-(2.622-1.846*coszen)*coszen
!    d1     =  0.754
!    d2     =  -2.265
!    ts     =  iv%instid(inst)%rad(n)%ts
!    tb1    =  ob%instid(inst)%rad(n)%tb(1)
!    tb2    =  ob%instid(inst)%rad(n)%tb(2)
!    clw    =  coszen*(d0+d1*log(ts-tb1)+d2*log(ts-tb2))
!    clw    =  clw - 0.03
! end if


!  e. check surface height/pressure
!-----------------------------------------------------------
!      sfchgt = ivrad%rad(i)%info%elv
!      if ( sfchgt >= ) then
!
!      else 

!      end if

       if ( (isflg .ne. 0) .and. (rad%ps < 850.) ) rad%tb(5)%qc  = -1

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

END SUBROUTINE da_qc_amsua
