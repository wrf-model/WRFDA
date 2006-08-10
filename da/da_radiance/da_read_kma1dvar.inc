subroutine da_read_kma1dvar (inst,iv, ob, xp, inunit, infile)
!------------------------------------------------------------------------------
!  PURPOSE: read in kma 1dvar innovation output to innovation and obs structure
!
!   METHOD: use F90 sequantial data structure to avoid read file twice  
!            so that da_scan_bufrtovs is not necessary any more.
!            1. read radiance data in sequential data structure
!            2. assign sequential data structure to innovation structure
!                  and deallocate sequential data structure
!
!  HISTORY: 2005/12/14 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

  implicit none

! subroutine argument
  integer           ,  intent (in)    :: inst
  character(20)     ,  intent (in)    :: infile
  integer           ,  intent (in)    :: inunit
  type (xpose_type) ,  intent (in)    :: xp
  type (y_type)     ,  intent (inout) :: ob
  type (ob_type)    ,  intent (inout) :: iv

! local variables
  integer          :: iost, i, j

! Declare local variables
  logical outside,inside_halo
  character(20)  :: instrument

! pixel information
   integer   ::  idate, npass, scanpos
   real      ::  rlat, rlon                         !  lat/lon in degrees   for Anfovs
   real      ::  satzen      !  scan angles for Anfovs
   integer   ::  landsea_mask
   real      ::  srf_height
   integer   ::  nchanl,knchan                      !  number of channels

   real      :: fastem(5)
   real , allocatable :: &         !  bright temperatures
                         otb(:),oerr(:),inov(:), emiss(:), &
                         tb(:), inv(:),err(:)
   integer, allocatable :: kchan(:),qc(:)

  type (datalink), pointer    :: head, p, current

  integer                        ::  size, error
  type(info_type)                ::  info
  type(model_loc_type)           ::  loc

!**************************************************************************
! Initialize variables

  nchanl = iv%instid(inst)%nchan

  allocate ( kchan(nchanl) )
  allocate ( otb(nchanl) )
  allocate ( oerr(nchanl) )
  allocate ( inov(nchanl) )
  allocate ( emiss(nchanl) )

  allocate ( tb(nchanl) )
  allocate ( inv(nchanl) )
  allocate ( err(nchanl) )
  allocate ( qc(nchanl) )

! 0.0  Open unit for kma 1dvar file and read file header
!--------------------------------------------------------------

  open(unit=inunit,file=trim(infile),form='formatted', &
       iostat = iost, status = 'old')
  if (iost /= 0) then
    call da_error(__FILE__,__LINE__,&
      (/"Cannot open file "//TRIM(infile)/))
  end if

  read(UNIT=inunit,FMT='(A,I10,I12)') instrument,idate,npass

! Loop to read pixel and assign information to a sequential structure
!-------------------------------------------------------------------------

  allocate ( head )
  nullify  ( head % next )
  p => head
  size = 0

  OUT_LOOP: DO j=1,npass
    read(inunit,'(2F12.4)')    rlat,rlon
    read(inunit,'(I5,20I5)')   knchan,(kchan(i),i=1,knchan)
! landsea_mask: 0:land ; 1:sea (same as RTTOV)
    read(inunit,'(I5,F12.4,I5,F12.4)')  scanpos,satzen,landsea_mask,srf_height
    read(inunit,'(20F12.4)') (oerr(i),i=1,knchan)
    read(inunit,'(20F12.4)') (emiss(i),i=1,knchan)
    read(inunit,'(5F12.4)')  (fastem(i),i=1,5)
    read(inunit,'(20F12.4)') (otb(i),i=1,knchan)
    read(inunit,'(20F12.4)') (inov(i),i=1,knchan)

!  1.0     Extract observation location and other required information
!          juge if data is in the domain, read next record if not
!-------------------------------------------------------------------------

        info%lat  =  rlat
        info%lon  =  rlon
        call da_ll_to_xy (info, loc, xp, outside, inside_halo )
        if (outside) cycle

        info%elv = srf_height
        write(UNIT=info%date_char, FMT='(i10)') idate 

        tb(1:nchanl) = missing_r
        inv(1:nchanl) = missing_r
        err(1:nchanl) = missing_r
        qc(1:nchanl) = -1
        do i=1,knchan
           tb(kchan(i)) = otb(i)
          inv(kchan(i)) = inov(i)
          err(kchan(i)) = oerr(i)
           qc(kchan(i)) = 1
        end do 

!  2.0   assign information to sequential radiance structure
!--------------------------------------------------------------------------
   allocate ( p % rad % tb (1:nchanl) )
   allocate ( p % rad_ob % tb (1:nchanl) )
   p%rad%info             = info
   p%rad%loc              = loc
   p%rad%landsea_mask     = landsea_mask
   p%rad%scanpos          = scanpos
   p%rad%satzen           = satzen
   p%rad%satazi           = missing_r
   p%rad%solzen           = missing_r
   p%rad%tb(1:nchanl)%inv = inv(1:nchanl)
   p%rad%tb(1:nchanl)%error = err(1:nchanl)
   p%rad%tb(1:nchanl)%qc  = qc(1:nchanl)
   p%rad_ob%tb(1:nchanl)  = tb(1:nchanl)
   p%sensor_index         = inst

   size = size + 1
   allocate ( p%next, stat=error)   ! add next data
   if (error /= 0 ) then
     call da_error(__FILE__,__LINE__, &
       (/"Cannot allocate radiance structure"/))
   end if

   p => p%next
   nullify (p%next)

  ENDDO OUT_LOOP

   
   iv%total_obs = iv%total_obs + size
   iv%total_rad_pixel   = iv%total_rad_pixel + size
   iv%total_rad_channel = iv%total_rad_channel + size*nchanl

  deallocate ( kchan )
  deallocate ( otb )
  deallocate ( oerr )
  deallocate ( inov )
  deallocate ( emiss )

  deallocate ( tb )
  deallocate ( inv )
  deallocate ( err )
  deallocate ( qc )

   close(inunit)

!  3.0 allocate innovation and obs radiance structure
!----------------------------------------------------------------
      iv%instid(inst)%num_rad = size
      ob%instid(inst)%num_rad = size
      
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)')  ' allocating space for radiance innov structure', &
                             i, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad
      allocate( iv%instid(inst)%rad(1:iv%instid(inst)%num_rad), stat = error )
      If( error /= 0 ) Then
        call da_error(__FILE__,__LINE__, &
          (/"mem allocation error to iv%instid(inst)%rad"/))
      End If

      allocate( ob%instid(inst)%rad(1:ob%instid(inst)%num_rad), stat = error )
      If( error /= 0 ) Then
        call da_error(__FILE__,__LINE__, &
          (/"mem allocation error to ob%instid(inst)%rad"/))
      End If

!  4.0 assign sequential structure to innovation structure
!-------------------------------------------------------------
  p => head
  do i = 1, size
   iv%instid(inst)%rad(i)%info   =  p%rad%info
   iv%instid(inst)%rad(i)%loc    =  p%rad%loc

   iv%instid(inst)%rad(i)%landsea_mask = p%rad%landsea_mask
   iv%instid(inst)%rad(i)%scanpos      = p%rad%scanpos
   iv%instid(inst)%rad(i)%satzen = p%rad%satzen
   iv%instid(inst)%rad(i)%satazi = p%rad%satazi
   iv%instid(inst)%rad(i)%solzen = p%rad%solzen
   allocate ( iv%instid(inst)%rad(i)%tb(1:nchanl), stat = error )
   If( error /= 0 ) Then
        call da_error(__FILE__,__LINE__, &
          (/"mem allocation error to iv%instid(inst)%rad(i)%tb"/))
   End If
   iv%instid(inst)%rad(i)%tb(1:nchanl)%inv   = p%rad%tb(1:nchanl)%inv
   iv%instid(inst)%rad(i)%tb(1:nchanl)%error = p%rad%tb(1:nchanl)%error
   iv%instid(inst)%rad(i)%tb(1:nchanl)%qc    = p%rad%tb(1:nchanl)%qc

   allocate ( ob%instid(inst)%rad(i)%tb(1:nchanl), stat = error )
   If( error /= 0 ) Then
        call da_error(__FILE__,__LINE__, &
          (/"mem allocation error to ob%instid(inst)%rad(i)%tb"/))
   End If
   ob%instid(inst)%rad(i)%tb(1:nchanl) = p%rad_ob%tb(1:nchanl)

!   WRITE(UNIT=stdout,*) inst, nread, iv%instid(inst)%rad(i)%tb(1:nchanl)%inv
   current => p
   p => p%next

! free current data
   deallocate ( current % rad % tb )
   deallocate ( current % rad_ob % tb )
   deallocate ( current )

 end do

! check if sequential structure has been freed
!
!  p => head
!  do i = 1, size
!   print *, i, p%rad%tb(1:nchanl)%inv
!   p => p%next
!  end do

end subroutine da_read_kma1dvar
