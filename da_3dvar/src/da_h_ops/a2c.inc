!						***************************
!						*  a2c.inc                *
!						*  wu     2001            *
! 						***************************
SUBROUTINE a2c(u,v,mix,mjx,mkx)
!----------------------------------------------------------------------------
! Set m*m matrix p to the nth power of m*m matrix a using binary combination
! of factors.
!----------------------------------------------------------------------------
!use module_cpt_consts
!use module_hops2i
!use module_hops2j
   IMPLICIT NONE
   INTEGER,                      INTENT(IN)    :: mix,mjx,mkx
   REAL, DIMENSION(mix,mjx,mkx), INTENT(INOUT) :: u,v
!----------------------------------------------------------------------------
   INTEGER,PARAMETER :: nhalo=1    ! a halo of 2 is needed for 8th-order

   INTEGER :: ids,ide,its,ite,ims,ime,k,i,j
   INTEGER :: jds,jde,jts,jte,jms,jme

   REAL, DIMENSION(1-nhalo:mjx+nhalo,1-nhalo:mix+nhalo) :: ux
!----------------------------------------------------------------------------
   ids=1 ; ide=mjx ; ims=1-nhalo ; ime=mjx+nhalo ; its=1 ; ite=mjx
   jds=1 ; jde=mix ; jms=1-nhalo ; jme=mix+nhalo ; jts=1 ; jte=mix

   write(unit=*, fmt='(3a,4x,i8)') &
        'File:', __FILE__, 'Line:', __LINE__
   write(unit=*, fmt='(a,2i8)') &
        'its,jts=', its,jts, &
        'ite,jte=', ite,jte, &
        'ids,jds=', ids,jds, &
        'ide,jde=', ide,jde, &
        'ims,jms=', ims,jms, &
        'ime,jme=', ime,jme, &
        'mix,mjx=', mix,mjx, &
        'nhalo,mkx=', nhalo,mkx

   do k=1,mkx
     ux=0.
     do i=1,mix
     do j=1,mjx
       ux(j,i)=u(i,j,k)
     enddo
     enddo

     CALL hg4ti2i(ux,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )

     do i=1,mix
     do j=1,mjx
       u(i,j,k)=ux(j,i)
     enddo
     enddo

     ux=0.
     do i=1,mix
     do j=1,mjx
       ux(j,i)=v(i,j,k)
     enddo
     enddo

     CALL hg4ti2j(ux,                                                       &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )

     do i=1,mix
     do j=1,mjx
       v(i,j,k)=ux(j,i)
     enddo
     enddo
   enddo

END SUBROUTINE a2c        

