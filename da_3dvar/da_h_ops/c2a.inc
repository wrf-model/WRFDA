!						***************************
!						*  c2a.inc                *
!						*  wu     2001            *
! 						***************************
SUBROUTINE c2a(u,v,mix,mjx,mkx,inijim)
!----------------------------------------------------------------------------
! Set m*m matrix p to the nth power of m*m matrix a using binary combination
! of factors.
!----------------------------------------------------------------------------
!use module_cpt_consts
!use module_hops2i
!use module_hops2j
IMPLICIT NONE
INTEGER,PARAMETER:: nhalo=1    ! a halo of 2 is needed for 8th-order
INTEGER,                      INTENT(IN) :: mix,mjx,mkx,inijim
REAL, DIMENSION(mix,mjx,mkx), INTENT(INOUT) :: u,v
!----------------------------------------------------------------------------
INTEGER ids,ide,its,ite,ims,ime,k,i,j
INTEGER jds,jde,jts,jte,jms,jme
INTEGER kds,kde,kts,kte,kms,kme
REAL, DIMENSION(1-nhalo:mjx+nhalo,1-nhalo:mix+nhalo):: ux
!----------------------------------------------------------------------------
if(inijim==1)then
! Set intended order of boundary extrapolation polynomial filters
call set_cpt_consts
call set_cpt_wextsall(2) ! Redefine extrapolation weights consistent
                          ! with desired order np. (Overwrite earlier defaults)
endif

    ids=1 ; ide=mjx ; ims=1-nhalo ; ime=mjx+nhalo ; its=1 ; ite=mjx
    jds=1 ; jde=mix ; jms=1-nhalo ; jme=mix+nhalo ; jts=1 ; jte=mix
    kds=1 ; kde=mkx ; kms=1-nhalo ; kme=mkx+nhalo ; kts=1 ; kte=mkx

   do k=1,mkx

ux=0.
     do i=1,mix
     do j=1,mjx
ux(j,i)=u(i,j,k)
     enddo
     enddo
   CALL hg4t2i(ux,                                                          &
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
   CALL hg4t2j(ux,                                                          &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
     do i=1,mix
     do j=1,mjx
v(i,j,k)=ux(j,i)
     enddo
     enddo

   enddo
END SUBROUTINE c2a        
