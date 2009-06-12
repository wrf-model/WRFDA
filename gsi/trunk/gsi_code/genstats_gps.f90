subroutine genstats_gps(bwork,awork,super_gps,toss_gps,conv_diagsave,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genstats_gps    generate statistics for gps observations
!   prgmmr: treadon          org: np20                date: 2005-12-21
!
! abstract:  For gps observations, this routine
!              a) collects statistics for runtime diagnostic output
!              f) adjusts observation error ratio based on superobs factor
!
! program history log:
!   2005-12-21  treadon
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-09-20  cucurull - replace superobs factor for obs in a top (non-full) layer 
!   2007-03-01  treadon - add array toss_gps
!   2007-06-21 cucurull - add conv_diagsave and mype in argument list; 
!                         modify qc and output for diagnostic file based on toss_gps
!                         print out diagnostic files if requested
!                         add wgtlim and huge_single in constants module
!
!   input argument list:
!     super_gps   - array of superob factors
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use obsmod, only: gps_allhead,gps_allptr,nprof_gps,&
       destroy_genstats_gps,gpsptr
  use gridmod, only: nsig
  use constants, only: tiny_r_kind,half,izero,ione,wgtlim,one,two,zero,five,&
                       huge_single,r1000
  use qcmod, only: npres_print,ptop,pbot
  use jfunc, only: last
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  implicit none

! Declare local parameters

! Declare passed variables
  logical,intent(in):: conv_diagsave
  integer(i_kind) :: mype
  real(r_kind),dimension(100+7*nsig),intent(inout):: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout):: bwork
  real(r_kind),dimension(nsig,max(1,nprof_gps)),intent(in):: super_gps
  real(r_kind),dimension(max(1,nprof_gps)),intent(in):: toss_gps

! Declare local variables
  logical:: luse,muse
  integer(i_kind):: k,itype,jsig,i,khgt,kprof,ikx,nn,j,nchar,nreal,ii
  real(r_kind):: pressure,arg,wgross,wgt,term,cg_gps,valqc,ressw2
  real(r_kind):: scale,ress,val,ratio_errors,val2
  real(r_kind):: exp_arg,data_ikx,data_rinc,cg_term,rat_err2,elat
  real(r_kind):: wnotgross,data_ipg,data_ier,data_ib,factor,super_gps_up,rhgt
  real(r_kind),allocatable,dimension(:,:)::rdiag
  real(r_single),allocatable,dimension(:,:)::sdiag
  character(8),allocatable,dimension(:):: cdiag
  
  real(r_kind),parameter:: r30 = 30.0_r_kind
  real(r_kind),parameter:: seven = 7.0_r_kind
  real(r_kind),parameter:: eight = 8.0_r_kind

!*******************************************************************************
! Initialize variables
  scale = one
  ii = izero ! counter for obs with luse
   i = izero ! counter for all obs

! Loop over data to be processed to get dimensions
  gps_allptr => gps_allhead
  do while (associated(gps_allptr))
      i=i+1
      luse = gps_allptr%luse
      if(luse) then
         ii=ii+1
      endif
      gps_allptr => gps_allptr%llpoint
  end do
  nreal =19
  nchar = 1
  allocate(cdiag(i),rdiag(nreal,i),sdiag(nreal,i))

  i=izero
  gps_allptr => gps_allhead
  do while (associated(gps_allptr))

!    Load local work variables
     ratio_errors = gps_allptr%ratio_err
     data_ier     = gps_allptr%obserr
     luse         = gps_allptr%luse
     muse         = gps_allptr%muse
     val          = gps_allptr%dataerr
     data_ipg     = gps_allptr%pg
     data_ib      = gps_allptr%b
     khgt         = gps_allptr%loc
     kprof        = gps_allptr%kprof
     data_ikx     = gps_allptr%type
     pressure     = gps_allptr%rdiag(6)
     data_rinc    = gps_allptr%rdiag(5)
     elat         = gps_allptr%rdiag(3)
     ikx          = nint(data_ikx)
     gpsptr       => gps_allptr%mmpoint

     if(conv_diagsave) then
        i=i+1
        cdiag(i) = gps_allptr%cdiag
        do j=1,nreal
           rdiag(j,i)= gps_allptr%rdiag(j)
        enddo
     endif

!    Determine model level to which observation is mapped to
     k=min(max(1,khgt),nsig)

!    Normalize ratio_errors by superobs factor.  Update ratio_error 
!    term used in minimization
     super_gps_up=zero

     if (super_gps(k,kprof)>tiny_r_kind) then
        do j=min(k+1,nsig),nsig
          super_gps_up = max(super_gps_up,super_gps(j,kprof))
        enddo

        if (super_gps_up >tiny_r_kind) then
            factor = one / sqrt(super_gps(k,kprof))
        else
            factor = one / sqrt(max(super_gps(k-1,kprof),super_gps(k,kprof)))
        endif
        ratio_errors = ratio_errors * factor
        if(conv_diagsave) then
           if(rdiag(16,i)< huge_single) rdiag(16,i)=ratio_errors*data_ier
        endif

!       Adjust error ratio for observations used in inner loop
        if (associated(gpsptr)) then
           gpsptr%raterr2 = ratio_errors **2
        endif
     endif


!    For given profile, check if observation level is below level at 
!    which profile data is tossed.   If so, set error parameter to 
!    zero (effectively tossing the obs).

     rhgt = gps_allptr%loc
     if (rhgt<toss_gps(kprof)) then
        if(ratio_errors*data_ier > tiny_r_kind) then ! obs was good
            if(conv_diagsave) then
               if(rdiag(7,i) > r30*r1000) then
                 rdiag(10,i) = eight
               else
                 rdiag(10,i) = seven
               endif
               rdiag(12,i) = -one
            endif
            if (luse) then
               if(elat > r30) then
                  awork(22) = awork(22)+one
               else if(elat< -r30)then
                  awork(23) = awork(23)+one
               else
                  awork(24) = awork(24)+one
               end if
            endif
        endif
        ratio_errors = zero
        if (associated(gpsptr)) then
           gpsptr%raterr2 = ratio_errors **2
        endif
     endif

        
!    Compute penalty terms
     if (ratio_errors*data_ier <= tiny_r_kind) muse = .false.
     if(luse)then
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (data_ipg > tiny_r_kind) then
           cg_gps=cg_term/data_ib
           wnotgross= one-data_ipg
           wgross   = data_ipg*cg_gps
           arg      = exp(exp_arg)
           term     = log(wnotgross*arg+wgross)
           wgt      = wnotgross*arg/(wnotgross*arg+wgross)
        else
           term = exp_arg
           wgt  = one
        endif
        if(conv_diagsave) rdiag(13,i) = wgt/wgtlim
        valqc = -two*rat_err2*term
        

!       Accumulate statistics for obs belonging to this task
        if(muse)then
           if(wgt < wgtlim) awork(21) = awork(21)+one
  
!          Accumulate values for penalty and data count
           jsig=max(ione,khgt)
           awork(jsig+3*nsig+100)=awork(jsig+3*nsig+100)+valqc
           awork(jsig+5*nsig+100)=awork(jsig+5*nsig+100)+one
           awork(jsig+6*nsig+100)=awork(jsig+6*nsig+100)+val2*rat_err2
        endif
          

!       Loop over pressure level groupings and obs to accumulate
!       statistics as a function of observation type.
        do k = 1,npres_print
           if(pressure>=ptop(k) .and. pressure<=pbot(k))then
              ress=data_rinc*scale
              ressw2=ress*ress
              nn=1
              if (.not. muse) then
                 nn=2
                 if(ratio_errors*data_ier >=tiny_r_kind)nn=3
              end if
              
              bwork(k,ikx,1,nn)  = bwork(k,ikx,1,nn)+one           ! count
              bwork(k,ikx,2,nn)  = bwork(k,ikx,2,nn)+ress          ! (o-g)
              bwork(k,ikx,3,nn)  = bwork(k,ikx,3,nn)+ressw2        ! (o-g)**2
              bwork(k,ikx,4,nn)  = bwork(k,ikx,4,nn)+val2*rat_err2 ! penalty
              bwork(k,ikx,5,nn)  = bwork(k,ikx,5,nn)+valqc         ! nonlin qc penalty
              
           end if
        end do
     end if

     gps_allptr => gps_allptr%llpoint

! End loop over observations

  end do

! If requested write information to diagnostic file
  if(conv_diagsave)then
     i  = izero
     ii = izero
     gps_allptr => gps_allhead
     do while (associated(gps_allptr))
       i=i+1
       luse = gps_allptr%luse
       if(luse)then
          ii=ii+1
          do j=1,nreal
            sdiag(j,ii)=rdiag(j,i)
          end do
       end if
       gps_allptr => gps_allptr%llpoint
     end do
     write(7)'gps',nchar,nreal,ii,mype
     write(7)cdiag(1:ii),sdiag(:,1:ii)
     deallocate(cdiag,rdiag,sdiag)
  endif

! Destroy arrays holding gps data
  call destroy_genstats_gps

end subroutine genstats_gps
