subroutine penal(xhat,xhatuv,mype)
!$$$  subprogram documentation block
! subprogram:    penal        calculate the trace and Jo to get adjusted observation error factor 
!   prgmmr: wu               org: np23                date: 2005-08-26
! abstract: apply observation operator to calculate Jo and trace 
! program history log:
!   2005-08-15  wu - oberror tuning
!   2007-03-02  su - modify the Wu's code and update the new gsi version

! usage: penal(xhat,xhatuv,mype)
!   input argument list:
!     xhat     - increment in grid space
!     xhatuv   - increment in grid space
!     mype     - pe id
!
!   output argument list: 


!ttributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero
  use obsmod, only: thead,tptr,psptr,pshead,qptr,qhead,whead,wptr,perturb_fact
  use gridmod, only: latlon1n,latlon11,lat2,lon2,nsig,nsig1o
  use jfunc, only: nclen,nclen1,nclen2,nrclen,nsclen,&
                   npclen,ncw,np,nt,nsst,noz,nq,nst,nvp,nu,nv,&
                   nuvlen,jiterstart,jiter
  use converr, only:etabl
  use mpimod, only: ierror,mpi_comm_world,mpi_sum,mpi_rtype,levs_id
  implicit none

! Declare passed var1iables
  integer(i_kind),intent(in):: mype
  real(r_kind),dimension(nclen),intent(in):: xhat
  real(r_kind),dimension(nuvlen):: xhatuv
 
! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  integer(i_kind) ntm,npm,num,nvm,itype,nq180
  integer(i_kind) n,k,l,ntk1,npk1,nwk1,nqk1
  real(r_kind) valu,valv,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) val,sosum
  real(r_kind),dimension(320) ::  tpenalty,ttrace,so,cat_num,tcat_num
  real(r_kind),save,dimension(320) ::  fpenalty,trace
  real(r_kind),dimension(2) :: sumqz,tsumqz 
  real(r_kind),dimension(latlon1n) :: xhat_q


! Set local variables
  ntm = nt-1
  npm = np-1
  num = nu-1
  nvm = nv-1

! Convert input normalized RH to q
  call normal_rh_to_q(xhat(nq),xhat(nt),xhat(np),xhat_q)


  if(jiter==jiterstart)then
     trace=zero
     fpenalty=zero
  else
    tpenalty=zero
    ttrace=zero
    cat_num=zero 
    tcat_num=zero 
    so=one
  endif


! Temperature
  tptr => thead
  do while (associated(tptr))
     ntk1=tptr%tk1 
     itype=ntk1/100
     if(itype==120 .or. itype==132) then
        n=mod(ntk1,100)
        if(n >28) n=28
     else if(itype==130) then
        n=34
     else if(itype==131)then
        n=35
     else if(itype==133)then
        n=36
     else if(itype==180 .or. itype==182)then
        n=37
     endif
     j1=tptr%ij(1)
     j2=tptr%ij(2)
     j3=tptr%ij(3)
     j4=tptr%ij(4)
     j5=tptr%ij(5)
     j6=tptr%ij(6)
     j7=tptr%ij(7)
     j8=tptr%ij(8)
     w1=tptr%wij(1)
     w2=tptr%wij(2)
     w3=tptr%wij(3)
     w4=tptr%wij(4)
     w5=tptr%wij(5)
     w6=tptr%wij(6)
     w7=tptr%wij(7)
     w8=tptr%wij(8)
     
     val=w1*xhat(j1+ntm)+w2*xhat(j2+ntm)+w3*xhat(j3+ntm)&
          +w4*xhat(j4+ntm)+w5*xhat(j5+ntm)+w6*xhat(j6+ntm)&
          +w7*xhat(j7+ntm)+w8*xhat(j8+ntm)

!     if (mype ==0 ) then
!       write(6,*) ntk1,n,val,tptr%tpertb,tptr%err2
!    endif

     if(jiter == jiterstart) then
        trace(n)=trace(n)-tptr%tpertb*val*tptr%err2/(perturb_fact*perturb_fact)
        fpenalty(n)=fpenalty(n)+(val-tptr%res)**2*tptr%err2

!     if (itype == 120 ) then
!       write(6,*) ntk1,n,val,tptr%tpertb,tptr%err2,trace(n),fpenalty(n)
!    endif

     else
        trace(n)=trace(n)+tptr%tpertb*val*tptr%err2/(perturb_fact*perturb_fact)
        cat_num(n)=cat_num(n)+one

!     if (itype == 120 ) then
!       write(6,*) ntk1,n,val,tptr%tpertb,tptr%err2,trace(n),cat_num(n),fpenalty(n)
!    endif

     endif
     
     tptr => tptr%llpoint
     
  enddo

! Humidity

  qptr => qhead
  do while (associated(qptr))
     nqk1=qptr%qk1
     itype=nqk1/100
     if(itype==120 .or. itype==132)then
        n=mod(nqk1,100)+38
     else if(itype ==180 .or. itype ==182) then
        n=72
     endif
     j1=qptr%ij(1)
     j2=qptr%ij(2)
     j3=qptr%ij(3)
     j4=qptr%ij(4)
     j5=qptr%ij(5)
     j6=qptr%ij(6)
     j7=qptr%ij(7)
     j8=qptr%ij(8)
     w1=qptr%wij(1)
     w2=qptr%wij(2)
     w3=qptr%wij(3)
     w4=qptr%wij(4)
     w5=qptr%wij(5)
     w6=qptr%wij(6)
     w7=qptr%wij(7)
     w8=qptr%wij(8)
     val=w1*xhat_q(j1)+w2*xhat_q(j2)+w3*xhat_q(j3)&
          +w4*xhat_q(j4)+w5*xhat_q(j5)+w6*xhat_q(j6)&
          +w7*xhat_q(j7)+w8*xhat_q(j8)
     if( jiter == jiterstart) then
        trace(n)=trace(n)-qptr%qpertb*val*qptr%err2/(perturb_fact*perturb_fact)
        fpenalty(n)=fpenalty(n)+(val-qptr%res)**2*qptr%err2
!     if (itype == 120 ) then
!       write(6,*) nqk1,n,val,qptr%qpertb,qptr%err2,trace(n),fpenalty(n)
!    endif
     else
        trace(n)=trace(n)+qptr%qpertb*val*qptr%err2/(perturb_fact*perturb_fact)
        cat_num(n)=cat_num(n)+one
!     if (itype == 120 ) then
!       write(6,*) nqk1,n,val,qptr%qpertb,qptr%err2,trace(n),fpenalty(n)
!    endif
     endif

     qptr => qptr%llpoint

  enddo
 
! Wind
  wptr => whead
  do while(associated(wptr))
     nwk1=wptr%wk1  
     itype=nwk1/100
     if(itype>209  .and. itype<223 .or. itype == 229) then
        n=mod(nwk1,100)+72
        if (n >100) n=100
     else if(itype==223  .and. itype==224) then
        n=mod(nwk1,100)+105
        if(n >133) n=133
     else if(itype == 230) then
        n=139
     else if(itype == 231) then
        n=140
     else if(itype ==  232) then
        n=141
     else if(itype ==  233) then
        n=142
!     else if(itype == 242 .or. itype == 243) then  
!       n=143
!     else if(itype == 245 .or. itype == 246) then 
!      n=mod(nwk1,100)+143
!     else if(itype == 252 .or. itype == 253) then
!      n=177
!     else if(itype == 257 .or. itype == 258 .or. itype == 259) then
!      n=mod(nwk1,100)+177
     else if(itype == 280 .or. itype ==282 ) then
        n=211
!     else if(itype ==  285 ) then
!      n=212
     endif
      
     j1=wptr%ij(1)
     j2=wptr%ij(2)
     j3=wptr%ij(3)
     j4=wptr%ij(4)
     j5=wptr%ij(5)
     j6=wptr%ij(6)
     j7=wptr%ij(7)
     j8=wptr%ij(8)
     w1=wptr%wij(1)
     w2=wptr%wij(2)
     w3=wptr%wij(3)
     w4=wptr%wij(4)
     w5=wptr%wij(5)
     w6=wptr%wij(6)
     w7=wptr%wij(7)
     w8=wptr%wij(8)
     
     val=w1*xhatuv(j1+num)+w2*xhatuv(j2+num)&
          +w3*xhatuv(j3+num)+w4*xhatuv(j4+num)&
          +w5*xhatuv(j5+num)+w6*xhatuv(j6+num)&
          +w7*xhatuv(j7+num)+w8*xhatuv(j8+num)
!     if (mype ==0) then
!       write(6,*) nwk1,n,val,wptr%upertb
!    endif
     if(jiter == jiterstart ) then
        trace(n)=trace(n)-wptr%upertb*val*wptr%err2/(perturb_fact*perturb_fact) 
        fpenalty(n)=fpenalty(n)+(val-wptr%ures)**2*wptr%err2
!     if (mype ==0) then
!       write(6,*) nwk1,n,val,wptr%upertb,trace(n),fpenalty(n),wptr%err2
!     endif
     else 
        trace(n)=trace(n)+wptr%upertb*val*wptr%err2/(perturb_fact*perturb_fact)
        cat_num(n)=cat_num(n)+one
!       write(6,*) nwk1,n,val,wptr%upertb,trace(n),fpenalty(n),cat_num(n)
     endif
     val=w1*xhatuv(j1+nvm)+w2*xhatuv(j2+nvm)&
          +w3*xhatuv(j3+nvm)+w4*xhatuv(j4+nvm)&
          +w5*xhatuv(j5+nvm)+w6*xhatuv(j6+nvm)&
          +w7*xhatuv(j7+nvm)+w8*xhatuv(j8+nvm) 
     if(jiter == jiterstart ) then
        trace(n)=trace(n)-wptr%vpertb*val*wptr%err2/(perturb_fact*perturb_fact) 
        fpenalty(n)=fpenalty(n)+(val-wptr%vres)**2*wptr%err2
     else 
        trace(n)=trace(n)+wptr%vpertb*val*wptr%err2/(perturb_fact*perturb_fact)
        cat_num(n)=cat_num(n)+one
     endif
     
     wptr => wptr%llpoint

  enddo


! Suface pressure
  psptr => pshead
  do while (associated(psptr))
     npk1=psptr%pk1
     itype=npk1/100     
     if(itype == 120) then
        n=213
     else if(itype == 180  .or. itype ==182) then
        n=214
     else if(itype == 181 ) then
        n=215
     else if(itype == 187 ) then
        n=216
     endif

     j1=psptr%ij(1)
     j2=psptr%ij(2)
     j3=psptr%ij(3)
     j4=psptr%ij(4)
     w1=psptr%wij(1)
     w2=psptr%wij(2)
     w3=psptr%wij(3)
     w4=psptr%wij(4)

     val=w1*xhat(j1+npm)+w2*xhat(j2+npm)&
          +w3*xhat(j3+npm)+w4*xhat(j4+npm)
     if( jiter == jiterstart ) then
        trace(n)=trace(n)-psptr%ppertb*val*psptr%err2/(perturb_fact*perturb_fact)
        fpenalty(n)=fpenalty(n)+(val-psptr%res)**2*psptr%err2
        if(mype ==0) then
           write(6,*) itype,npk1,psptr%ppertb,val,psptr%err2,trace(n),fpenalty(n)
        endif
     else
        trace(n)=trace(n)+psptr%ppertb*val*psptr%err2/(perturb_fact*perturb_fact)
        cat_num(n)=cat_num(n)+one
     endif
     
     psptr => psptr%llpoint
     
  enddo
       
  if (jiter /= jiterstart ) then
     trace(1:320)=cat_num(1:320)-trace(1:320) 
!       if (mype == 0) then
!          write(6,300) (trace(i),i=1,8),cat_num(1:8)
!          write(6,*) 'PENAL: start to call mpi_allreduce'
!       endif
      
     call mpi_allreduce(trace,ttrace,320,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce(fpenalty,tpenalty,320,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror) 
     
     call mpi_allreduce(cat_num,tcat_num,320,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror) 

!          write(6,*) 'PENAL: start to call mpi_allreduce'
     do i=1,320
        if(ttrace(i) /=  zero) so(i)=tpenalty(i)/ttrace(i)
        if(so(i) >zero ) then
           so(i)=sqrt(so(i))
        else
           so(i)=one
        endif
     enddo

     if (mype ==0) then
        write(450,300) (ttrace(i),i=1,216)
        write(450,*)
        write(450,300) (tpenalty(i),i=1,216)
        write(450,*)
        write(450,300) (tcat_num(i),i=1,216)
        write(450,*)
        write(450,310) (so(i),i=1,216)
     endif
300  format(10e8.1)
310  format(10f8.5)

!   endif

!    Update the error table
     if(mype==3)then
        open(59,file='errtable_out',form='formatted')
        rewind 59
        do l=1,300
           if(etabl(l,1,1)==1100.)then
              write(59,100)l
100           format(1x,i3,' OBSERVATION TYPE')
              if(l == 120)then
                 do k=1,33
                    if( etabl(l,k,2) < 1.e8) etabl(l,k,2)=etabl(l,k,2)*so(k)
                    if( etabl(l,k,3) < 1.e8) etabl(l,k,3)=etabl(l,k,3)*so(k+38)
                    if( etabl(l,k,5) < 1.e8) etabl(l,k,5)=etabl(l,k,5)*so(213)
                 end do
              else if( l == 132) then 
                 do k=1,33
                    if( etabl(l,k,2) < 1.e8) etabl(l,k,2)=etabl(l,k,2)*so(k)
                    if( etabl(l,k,3) < 1.e8) etabl(l,k,3)=etabl(l,k,3)*so(k+38)
                 enddo
              else if( l == 130) then
                 do k=1,33
                    if( etabl(l,k,2) < 1.e8) etabl(l,k,2)=etabl(l,k,2)*so(34)
                 enddo
              else if( l == 131) then
                 do k=1,33
                    if( etabl(l,k,2) < 1.e8) etabl(l,k,2)=etabl(l,k,2)*so(35)
                 enddo
              else if( l == 133) then
                 do k=1,33
                    if( etabl(l,k,2) < 1.e8) etabl(l,k,2)=etabl(l,k,2)*so(36)
                 enddo
              else if( l == 180 .or. l == 182) then
                 do k=1,33
                    if( etabl(l,k,2) < 1.e8) etabl(l,k,2)=etabl(l,k,2)*so(37)
                    if( etabl(l,k,3) < 1.e8) etabl(l,k,3)=etabl(l,k,3)*so(72)
                    if( etabl(l,k,5) < 1.e8) etabl(l,k,5)=etabl(l,k,5)*so(214)
                 enddo
              else if( l == 181) then
                 do k=1,33
                    if( etabl(l,k,5) < 1.e8) etabl(l,k,5)=etabl(l,k,5)*so(215)
                 enddo
              else if( l == 187) then
                 do k=1,33
                    if( etabl(l,k,5) < 1.e8) etabl(l,k,5)=etabl(l,k,5)*so(216)
                 enddo
              else if ( l >209 .and. l <223 .or.  l == 229 ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(k+72) 
                 enddo
              else if ( l == 223 .and. l == 224 ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(k+105) 
                 enddo
              else if ( l == 230 ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(139) 
                 enddo
              else if ( l == 231 ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(140) 
                 enddo
              else if ( l == 232 ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(141) 
                 enddo
              else if ( l == 233 ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(142) 
                 enddo
              else if ( l == 242 .or. l == 243 ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(143) 
                 enddo
              else if ( l == 245 .or. l == 246 ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(k+143) 
                 enddo
              else if ( l == 252 .or. l == 253 ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(177) 
                 enddo
              else if ( l == 257 .or. l == 258 ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(k+177) 
                 enddo
              else if ( l == 280 .or. l == 282 ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(211) 
                 enddo
              else if ( l == 285  ) then
                 do k=1,33
                    if( etabl(l,k,4) < 1.e8) etabl(l,k,4)=etabl(l,k,4)*so(212) 
                 enddo
              endif
              do k=1,33
                 write(59,110)(etabl(l,k,i),i=1,6)
              end do
110           format(1x,6e12.5)
           endif
        enddo
        close(59)
     endif ! mype
  endif


  return 
end subroutine penal




      

