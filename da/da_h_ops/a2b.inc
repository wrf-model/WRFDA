!                                               ***************************
!                                               *  a2b.inc                *
!                                               *  wu     2001            *
!                                               ***************************
SUBROUTINE a2b(u,mix,mjx,mkx,ub)
!----------------------------------------------------------------------------
! Set m*m matrix p to the nth power of m*m matrix a using binary combination
! of factors.
!----------------------------------------------------------------------------
!use module_cpt_consts
!use module_hops2i
!use module_hops2j
IMPLICIT NONE
INTEGER,PARAMETER:: nhalo=1    ! a halo of 2 is needed for 8th-order
INTEGER,                      INTENT(IN) :: mix,mjx,mkx
REAL, DIMENSION(mix,mjx,mkx), INTENT(IN) :: u
REAL, DIMENSION(mix,mjx,mkx), INTENT(OUT):: ub
!----------------------------------------------------------------------------
INTEGER ids,ide,its,ite,ims,ime,k,i,j
INTEGER jds,jde,jts,jte,jms,jme
INTEGER kds,kde,kts,kte,kms,kme
REAL, DIMENSION(1-nhalo:mix+nhalo,1-nhalo:mjx+nhalo):: ux
!----------------------------------------------------------------------------
    ids=1 ; ide=mix ; ims=1-nhalo ; ime=mix+nhalo ; its=1 ; ite=mix
    jds=1 ; jde=mjx ; jms=1-nhalo ; jme=mjx+nhalo ; jts=1 ; jte=mjx
    kds=1 ; kde=mkx ; kms=1-nhalo ; kme=mkx+nhalo ; kts=1 ; kte=mkx

   do k=1,mkx
     ux=0.
     do i=1,mix
     do j=1,mjx
        ux(i,j)=u(i,j,k)          
     enddo
     enddo
   CALL hg4t2i(ux,                                                          &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
   CALL hg4t2j(ux,                                                          &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
     do j=1,mjx
     do i=1,mix
        ub(i,j,k)=ux(i,j)          
     enddo
     enddo

   enddo

END SUBROUTINE a2b        
