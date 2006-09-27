  subroutine da_biasprep(inst,ob,iv)

  IMPLICIT NONE

! ACCUMULATE STATS FROM FEEDBACK FILES FOR BIAS CORRECTION PROGS
!--------------------------------------------------------------------------

  INTEGER       ,  INTENT(IN)      :: inst
  TYPE (y_type),  INTENT(IN)       :: ob         ! O structure.
  TYPE (ob_type),  INTENT(INOUT)   :: iv         ! O-B structure.

!---------------------------------------------------------------------------
! INITIALIZE CONSTANTS AND VARIABLES.

  INTEGER  :: j,jx,npred,nchan, biasprepunit
  CHARACTER(len=80)  :: filename
  CHARACTER(len=1)   :: s1
  REAL        :: pred(6), q(43)
  TYPE(bias)  :: radbias

  biasprepunit = 211

  write(filename,'(a)') 'biasprep_'//trim(iv%instid(inst)%rttovid_string)//'.bin'
  open(UNIT=biasprepunit,FILE=filename,FORM='unformatted')

!---------------------------------------------------------------------------
   npred = 4
   nchan = iv%instid(inst)%nchan 

   DO j=1,iv%instid(inst)%num_rad

     q(1:43) = iv%instid(inst)%rad(j)%mr(1:43)/q2ppmv

     call da_predictor(pred(1:npred), npred, iv%instid(inst)%rad(j)%t(1:43), q(1:43), iv%instid(inst)%rad(j)%ts)

     !iv%instid(inst)%rad(j)%pred(1:npred)=pred(1:npred)
     
! transfer information to bias structure
     radbias%platform_id  = iv%instid(inst)%platform_id
     radbias%satellite_id = iv%instid(inst)%satellite_id
     radbias%sensor_id    = iv%instid(inst)%sensor_id

     read(iv%instid(inst)%rad(j)%info%date_char,'(i4,5(a1,i2))') &
                                radbias%year,s1, radbias%month,s1, radbias%day, &
                                s1,radbias%hour, s1,radbias%min, s1,radbias%sec
     if (j < 10) then
        print *, iv%instid(inst)%rad(j)%info%date_char
        print *, radbias%year, radbias%month, radbias%day, &
                 radbias%hour, radbias%min, radbias%sec
     endif

     radbias%scanline     = iv%instid(inst)%rad(j)%scanline    ! not available
     radbias%scanpos      = iv%instid(inst)%rad(j)%scanpos
     radbias%landmask     = iv%instid(inst)%rad(j)%landsea_mask
     radbias%elevation    = iv%instid(inst)%rad(j)%info%elv
     radbias%lat          = iv%instid(inst)%rad(j)%info%lat
     radbias%lon          = iv%instid(inst)%rad(j)%info%lon
     radbias%ps           = iv%instid(inst)%rad(j)%ps
     radbias%t2m          = iv%instid(inst)%rad(j)%t2m
     radbias%q2m          = iv%instid(inst)%rad(j)%mr2m/q2ppmv
     radbias%tsk          = iv%instid(inst)%rad(j)%ts

     radbias%nchan        = nchan 
     radbias%tb(1:nchan)  = ob%instid(inst)%rad(j)%tb(1:nchan)
     radbias%omb(1:nchan) = iv%instid(inst)%rad(j)%tb(1:nchan)%inv

     radbias%npred         = npred
     radbias%pred(1:npred) = pred(1:npred)

!    set missing data and bad data to missing
     DO jx=1,nchan   
       IF(radbias%tb(jx) < 150. .or. radbias%tb(jx) > 400. ) THEN
          radbias%tb(jx)   = missing_r
          radbias%omb(jx)  = missing_r 
       ENDIF
     ENDDO

     write(UNIT=biasprepunit) radbias

   ENDDO
  
   close(UNIT=biasprepunit)

 end subroutine da_biasprep
