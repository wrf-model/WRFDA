SUBROUTINE DA_Setup_Background_Errors3( xb, be, xp )

!------------------------------------------------------------------------------
!  PURPOSE: Define and allocate components of background errors for cv_option 3.
!
!  METHOD:  Allocate components in turn.
!
!  HISTORY: 08/02/2002 - Creation of F90 version.           Wan-Shu Wu
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)  :: xb                    ! First guess structure.
   TYPE (be_type), INTENT(OUT) :: be                    ! Back. errors structure.
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers.

   INTEGER                     :: kts , kte  
   INTEGER                     :: its,ite,jts,jte
   INTEGER                     :: ij,ijk                ! Scalar.
   INTEGER                     :: i, j, k, ic, jc       ! Loop counters.
   INTEGER                     :: ier                   ! error index


! added for AVN
   integer                     :: nlath
   integer                     :: nsig
!   integer, parameter          :: nlath=96
!   integer, parameter          :: nsig=31
   integer                     :: m,n,m1,n1,n4            ! loop counter
   integer                     :: msig,mlath,nmdszh,kcap  ! dummy variables
   REAL, ALLOCATABLE           :: corz_kz(:,:)
   REAL, ALLOCATABLE           :: cord_kz(:,:)
   REAL, ALLOCATABLE           :: corh_kz(:,:)
   REAL, ALLOCATABLE           :: corq_kz(:,:)
   REAL, ALLOCATABLE           :: corz_avn(:,:)
   REAL, ALLOCATABLE           :: cord_avn(:,:)
   REAL, ALLOCATABLE           :: corh_avn(:,:)
   REAL, ALLOCATABLE           :: corq_avn(:,:)
   REAL, ALLOCATABLE           :: corp_avn(:)
   REAL, ALLOCATABLE           :: clat_avn(:),sigma_avn(:)
   REAL, ALLOCATABLE           :: hwll_avn(:,:,:),hwllp_avn(:),hwll_kz(:,:,:)
   REAL, ALLOCATABLE           :: vztdq_avn(:,:,:),vztdq_kz(:,:,:)

   REAL, ALLOCATABLE           :: agv_avn(:,:,:),agv_kz(:,:,:)
   REAL, ALLOCATABLE           :: bv_avn(:,:),wgv_avn(:,:),bv_kz(:,:),wgv_kz(:,:)
   REAL, ALLOCATABLE           :: dsh(:),turn(:,:)
   REAL, DIMENSION(xp%kts:xp%kte, xp%kts:xp%kte) :: vv
   REAL, DIMENSION(xp%ids:xp%ide, xp%jds:xp%jde, xp%kds:xp%kde, 1:4) :: hwll
   REAL, DIMENSION(xp%ids:xp%ide, xp%jds:xp%jde)                     :: hwllp, &
                                                                        coef1, coef2, &
                                                                        global_lat, global_fac
   INTEGER, DIMENSION(xp%ids:xp%ide, xp%jds:xp%jde)                  :: mlat

   INTEGER :: nta,ndeg,ku,kz
   real    :: samp,s2u,tin,as(5),slim

   write (6,'(A)') ' ----------------------------------------------------------'
   write (6,'(A,I3)') ' [3.0] Set up background errors (be) for cv_option:', cv_options
   write (6,'(A)') ' ----------------------------------------------------------'
   write (6,*)

   IF(cv_options /= 3) THEN
      write(unit=*, fmt='(a, i2)') &
           'cv_options:', cv_options

      CALL wrf_shutdown

      STOP "Stopped for Wrong CV_OPTION."
   ENDIF

   its = xp%its
   ite = xp%ite
   jts = xp%jts
   jte = xp%jte
   kts = xp%kts
   kte = xp%kte
   
   be % cv % option = cv_options
!!!!!!!!! cv_options=3
      be % v1 % name = 'psi  '           ! Streamfunction
      be % v2 % name = 'chi_u'           ! Uncorrelated velocity potential.
      be % v3 % name = 't_u'             ! Unbalanced temperature.
      be % v4 % name = 'q/qsg'
      be % v5 % name = 'psfc'            ! surface pressure
   write(6,'(3x,A)')' DA_Setup_Background_Errors: 3DVAR dry control variables are:'
   write(6,'(4x,7A)')TRIM(be % v1 % name), ', ', TRIM(be % v2 % name), ', ', &
                  TRIM(be % v3 % name), ' and ', TRIM(be % v5 % name)

   write(6,'(3x,A,A)')' DA_Setup_Background_Errors: 3DVAR humidity control variable is ',&
                     TRIM(be % v4 % name)

   write(6,*)

   be % mix = xp%ide - xp%ids + 1
   be % mjy = xp%jde - xp%jds + 1

   ij = (xp%ite-xp%its+1)*(xp%jte-xp%jts+1)
   ijk = ij * (xp%kte-xp%kts+1)

   be % v1 % mz = xp%kde-xp%kds+1
   be % v2 % mz = xp%kde-xp%kds+1
   be % v3 % mz = xp%kde-xp%kds+1
   be % v4 % mz = xp%kde-xp%kds+1
   be % v5 % mz = 1           

   be % cv % size1  = ijk
   be % cv % size2  = ijk
   be % cv % size3  = ijk
   be % cv % size4  = ijk
   be % cv % size5  = ij

   be % cv % size = be % cv % size1 + be % cv % size2 + be % cv % size3 + &
                    be % cv % size4 + be % cv % size5

   cv_size = be % cv % size


     rewind(be_cv_3_unit)

     read(be_cv_3_unit) nsig,nlath

!     print*,'number of vertical level for stats =', nsig
!     print*,'number of latitude           nlath =', nlath

   kz = xp%kde-xp%kds+1

   if(nsig.ne.kz)then
     print*,'number of vertical level = ',kz
     print*,'number of vertical level for stats =',nsig
   end if

   ALLOCATE ( corz_kz(1:2*nlath+1,1:kz),cord_kz(1:2*nlath+1,1:kz) )
   ALLOCATE ( corh_kz(1:2*nlath+1,1:kz),corq_kz(1:2*nlath+1,1:kz) )
   ALLOCATE ( hwll_kz(0:nlath*2+1,1:kz,1:4)                      )
   ALLOCATE ( vztdq_kz(1:kz,0:nlath*2+1,1:4)                     )
   ALLOCATE ( agv_kz(0:nlath*2+1,1:kz,1:kz)                      )
   ALLOCATE ( bv_kz(0:nlath*2+1,1:kz),wgv_kz(0:nlath*2+1,1:kz)   )
   ALLOCATE ( corz_avn(1:2*nlath+1,1:nsig),cord_avn(1:2*nlath+1,1:nsig) )
   ALLOCATE ( corh_avn(1:2*nlath+1,1:nsig),corq_avn(1:2*nlath+1,1:nsig) )
   ALLOCATE ( corp_avn(1:2*nlath+1),clat_avn(1:2*nlath),sigma_avn(1:nsig) )
   ALLOCATE ( hwll_avn(0:nlath*2+1,1:nsig,1:4),hwllp_avn(0:nlath*2+1) )
   ALLOCATE ( vztdq_avn(1:nsig,0:nlath*2+1,1:4)                     )
   ALLOCATE ( agv_avn(0:nlath*2+1,1:nsig,1:nsig)                    )
   ALLOCATE ( bv_avn(0:nlath*2+1,1:nsig),wgv_avn(0:nlath*2+1,1:nsig) )

   ALLOCATE ( be % corz(its:ite,jts:jte,kts:kte,1:4) )
   ALLOCATE ( be % corp(its:ite,jts:jte) )
   ALLOCATE ( be % vz(kts:kte,its:ite,jts:jte,1:4) )
   ALLOCATE ( be % agvz(its:ite,jts:jte,kts:kte,kts:kte) )
   ALLOCATE ( be % bvz(its:ite,jts:jte,kts:kte) )
   ALLOCATE ( be % wgvz(its:ite,jts:jte,kts:kte) )
!

   ij = (xp%ide-xp%ids+1)*(xp%jde-xp%jds+1)

!  Collect xb component of lat into global buffer.
   call local_to_global( xp, xb%lat, global_lat, 2 )
   call wrf_dm_bcast_real( global_lat, ij )
!  Collect xb component of fac into global buffer.
   call local_to_global( xp, xb%map_factor, global_fac, 2 )
   call wrf_dm_bcast_real( global_fac, ij )


   read(be_cv_3_unit) clat_avn,(sigma_avn(k),k=1,nsig)

   m=2*nlath+1
   read (be_cv_3_unit) &
                   ((corz_avn(i,k),i=1,m),k=1,nsig),   &
                   ((cord_avn(i,k),i=1,m),k=1,nsig),   &
                   ((corh_avn(i,k),i=1,m),k=1,nsig),   &
                   ((corq_avn(i,k),i=1,m),k=1,nsig),corp_avn

   read(be_cv_3_unit) (((hwll_avn(i,k,m),i=0,nlath*2+1),k=1,nsig),m=1,4),   &
                     hwllp_avn
   read(be_cv_3_unit) (((vztdq_avn(k,i,m),k=1,nsig),i=0,nlath*2+1),m=1,4)
   read(be_cv_3_unit) (((agv_avn(i,k,m),i=0,nlath*2+1),k=1,nsig),m=1,nsig), &
                         ((bv_avn(i,k),i=0,nlath*2+1),k=1,nsig), &
                         ((wgv_avn(i,k),i=0,nlath*2+1),k=1,nsig)

    as(1)=as1(2)
    as(2)=as2(2)
    as(3)=as3(2)
    as(4)=as4(2)
      do m=1,4
       do k=1,nsig
        do i=0,nlath*2+1
      hwll_avn(i,k,m)=hwll_avn(i,k,m)*as(m)
        enddo
       enddo
      enddo
        do i=0,nlath*2+1
      hwllp_avn(i)=hwllp_avn(i)*as5(2)
        enddo

    as(1)=as1(3)
    as(2)=as2(3)
    as(3)=as3(3)
    as(4)=as4(3)
      do m=1,4
        do i=0,nlath*2+1
       do k=1,nsig
      vztdq_avn(k,i,m)=vztdq_avn(k,i,m)*as(m)
        enddo
       enddo
      enddo

     
    ku=xp%kde+1
  k_loop: do k=xp%kds+1,xp%kde
    if(xb%sigmah(k-1)>0.15 .and. xb%sigmah(k)<=0.15)then
    ku=k
     exit k_loop
    endif
  end do k_loop

  call da_chgvres(nlath,nsig,kz,xb%sigmah,sigma_avn,&
 corz_avn,cord_avn,corh_avn,corq_avn,hwll_avn,vztdq_avn,agv_avn,bv_avn,wgv_avn,&
 corz_kz, cord_kz, corh_kz, corq_kz, hwll_kz, vztdq_kz, agv_kz, bv_kz, wgv_kz)

  do j=xp%jds, xp%jde
  do i=xp%ids, xp%ide
  if(global_lat(i,j).ge.clat_avn(2*nlath))then
     mlat(i,j)=nlath*2-1
     coef1(i,j)=0.
     coef2(i,j)=1.
  else
     do m=1,2*nlath-1
       m1=m+1
       if((global_lat(i,j).ge.clat_avn(m)).and.  &
          (global_lat(i,j).lt.clat_avn(m1)))then
         mlat(i,j)=m
         exit
       end if
     end do

     coef2(i,j)=(global_lat(i,j)-clat_avn(m))/(clat_avn(m1)-clat_avn(m))
     coef1(i,j)=1.-coef2(i,j)
  endif
   end do
   end do

! 3-D covariance
  do j=jts,jte
   do i=its,ite
     m=mlat(i,j)
     m1=m+1
     be%corp(i,j)=corp_avn(m)*coef1(i,j)+corp_avn(m1)*coef2(i,j)
   enddo
  enddo
 print *,'be_corp=',be%corp(its+5,jts+2)

 do k=kts,kte
  do j=jts,jte
   do i=its,ite
     m=mlat(i,j)
     m1=m+1
       be%corz(i,j,k,1)=corz_kz(m,k)*coef1(i,j)+corz_kz(m1,k)*coef2(i,j)
       be%corz(i,j,k,2)=cord_kz(m,k)*coef1(i,j)+cord_kz(m1,k)*coef2(i,j)
       be%corz(i,j,k,3)=corh_kz(m,k)*coef1(i,j)+corh_kz(m1,k)*coef2(i,j)
       be%corz(i,j,k,4)=corq_kz(m,k)*coef1(i,j)+corq_kz(m1,k)*coef2(i,j)
     end do
   end do
  end do
  
   print *, 'size(be % corz, dim=1)=', size(be % corz, dim=1)
   print *, 'size(be % corz, dim=2)=', size(be % corz, dim=2)
   print *, 'size(be % corz, dim=3)=', size(be % corz, dim=3)
   print *, 'size(be % corz, dim=4)=', size(be % corz, dim=4)

   ic = 1 + (xp%ide-xp%ids)/2
   jc = 1 + (xp%jde-xp%jds)/2

   do j=xp%jds, xp%jde
   do i=xp%ids, xp%ide
     m=mlat(i,j)
     m1=m+1
         hwllp(i,j)=hwllp_avn(m)*coef1(i,j)+hwllp_avn(m1)*coef2(i,j)
   end do
   end do
 print *,'hwllp=',hwllp(ic/2,jc/2)

! 3-D covariance
 do n4=1,4
  do k=xp%kds, xp%kde
   do j=xp%jds, xp%jde
   do i=xp%ids, xp%ide
     m=mlat(i,j)
     m1=m+1
 hwll(i,j,k,n4)=hwll_kz(m,k,n4)*coef1(i,j)+hwll_kz(m1,k,n4)*coef2(i,j)
   end do
   end do
  end do
 end do

 do n4=1,4
   do j=jts,jte
   do i=its,ite
     m=mlat(i,j)
     m1=m+1
    do k=kts,kte
 be%vz(k,i,j,n4)=vztdq_kz(k,m,n4)*coef1(i,j)+vztdq_kz(k,m1,n4)*coef2(i,j)
    end do
   end do
   end do
 end do
   print *,'vz=',(be%vz(10,its+2,jts+5,n4),n4=1,4)

 do k=kts,kte
  do n=kts,kte
   do j=jts,jte
   do i=its,ite
     m=mlat(i,j)
     m1=m+1
       be%agvz(i,j,n,k)=agv_kz(m,n,k)*coef1(i,j)+agv_kz(m1,n,k)*coef2(i,j)
   end do
   end do
 end do
 end do

 do k=kts,kte
   do j=jts,jte
   do i=its,ite
     m=mlat(i,j)
     m1=m+1
       be%bvz(i,j,k)=bv_kz(m,k)*coef1(i,j)+bv_kz(m1,k)*coef2(i,j)
   end do
   end do
 end do

 do k=kts,kte
   do j=jts,jte
   do i=its,ite
     m=mlat(i,j)
     m1=m+1
       be%wgvz(i,j,k)=wgv_kz(m,k)*coef1(i,j)+wgv_kz(m1,k)*coef2(i,j)
   end do
   end do
 end do

   DEALLOCATE ( corz_avn,cord_avn )
   DEALLOCATE ( corh_avn,corq_avn )
   DEALLOCATE ( corp_avn,clat_avn,sigma_avn )
   DEALLOCATE ( corz_kz,cord_kz )
   DEALLOCATE ( corh_kz,corq_kz )

   DEALLOCATE ( hwll_avn,hwllp_avn )
   DEALLOCATE ( vztdq_avn )
   DEALLOCATE ( agv_avn )
   DEALLOCATE ( bv_avn,wgv_avn )
   DEALLOCATE ( hwll_kz )
   DEALLOCATE ( vztdq_kz )
   DEALLOCATE ( agv_kz )
   DEALLOCATE ( bv_kz,wgv_kz )

!
!     call da_prerf(xb,be)
!
!!!!!!!!!!!!! init
      as(1)=sqrt(as1(1))
      as(2)=sqrt(as2(1))
      as(3)=sqrt(as3(1))
      as(4)=sqrt(as4(1))
      as(5)=sqrt(as5(1))
      nta=5600
      be%nta=nta
      ndeg=4
      be%ndeg=ndeg
      be%swidth=10.
  allocate (dsh(1:nta)        )
   ALLOCATE ( turn (1:ndeg,1:ndeg) )

   ALLOCATE ( be % sljpy (xp%ipsy:xp%ipey,xp%jpsy:xp%jpey) )
   ALLOCATE ( be % sljy (xp%ipsy:xp%ipey,xp%jpsy:xp%jpey,xp%kpsy:xp%kpey,1:4) )
   ALLOCATE ( be % slipx (xp%ipsx:xp%ipex,xp%jpsx:xp%jpex) )
   ALLOCATE ( be % slix (xp%ipsx:xp%ipex,xp%jpsx:xp%jpex,xp%kpsx:xp%kpex,1:4) )
   ALLOCATE ( be % be (1:ndeg) )
   ALLOCATE ( be % rate (1:ndeg) )
   ALLOCATE ( be % table (1:nta,1:ndeg) )
   s2u= 1./xb % ds
      hwll=hwll*s2u
      hwllp=hwllp*s2u
      CALL RFDPAR1(be%BE,be%RATE,ndeg)
      CALL RFDPAR2(be%BE,be%RATE,TURn,SAMP,ndeg)

!!!!!!!!!!

       tin=be%swidth/float(nta)
       do i=1,nta
       dsh(i)=float(i-1)*tin
       enddo
      call  RFDPARV(DSH,be%RATE,be%table,nta,ndeg )

  deallocate (dsh )
  deallocate (turn )

!
!cccccccccc sli in scale  unit (map_factor come with ds )
!           variance* amp for 3d/2d RF
    do n=1,4
      do j=jts,jte
      do i=its,ite
      vv=0.
    do k=kts,kte
    vv(k,k)=1.
    enddo
   call da_rfz0(vv,kz,kz,be%ndeg,&
     be%vz(kts,i,j,n),be%be,be%table,be%nta,be%swidth)
     do k=kts,kte
      be % corz(i,j,k,n)=be % corz(i,j,k,n)*as(n) &
        *samp/hwll(i,j,k,n)/ &
         vv(k,k)/global_fac(i,j)
      enddo
     enddo
     enddo
    enddo
      be % corp(its:ite,jts:jte)=be % corp(its:ite,jts:jte)*as(5) &
       *samp/hwllp(its:ite,jts:jte)/global_fac(its:ite,jts:jte)
!cccc set up scales  ccccccccccccc
    do n=1,4
     do k=xp%kpsy,xp%kpey
      do j=xp%jpsy,xp%jpey
      do i=xp%ipsy,xp%ipey
      be%sljy(i,j,k,n)=1./global_fac(i,j)/hwll(i,j,k,n)
      enddo
      enddo
     enddo
    enddo

    do n=1,2
    do k=max(ku,xp%kpsy),xp%kpey
      slim=1./global_fac(ic,jc)/hwll(ic,jc,k,n)
      do j=xp%jpsy,xp%jpey
      do i=xp%ipsy,xp%ipey
        be%sljy(i,j,k,n)=slim
      enddo
      enddo
    enddo
    enddo

   do j=xp%jpsy,xp%jpey
   do i=xp%ipsy,xp%ipey
      be%sljpy(i,j)=1./global_fac(i,j)/hwllp(i,j)
   enddo
   enddo

   do n=1,4
   do k=xp%kpsx,xp%kpex
      do j=xp%jpsx,xp%jpex
      do i=xp%ipsx,xp%ipex
      be%slix(i,j,k,n)=1./global_fac(i,j)/hwll(i,j,k,n)
      enddo
      enddo
   enddo
   enddo

   do n=1,2
   do k=max(ku,xp%kpsx),xp%kpex
      slim=1./global_fac(ic,jc)/hwll(ic,jc,k,n)
      do j=xp%jpsx,xp%jpex
      do i=xp%ipsx,xp%ipex
      be%slix(i,j,k,n)=slim
      enddo
      enddo
   enddo
   enddo

   do j=xp%jpsx,xp%jpex
   do i=xp%ipsx,xp%ipex
      be%slipx(i,j)=1./global_fac(i,j)/hwllp(i,j)
   enddo
   enddo

   close(be_cv_3_unit)

END SUBROUTINE DA_Setup_Background_Errors3

