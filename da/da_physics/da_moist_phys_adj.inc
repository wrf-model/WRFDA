subroutine da_moist_phys_adj(grid, xb, xa, xp)

   !---------------------------------------------------------------------------
   !  Purpose: Partition of the hydrometeors via the moist explicit scheme.
   !           A warm rain process is used in this subroutine. 
   !           This is the adjoint code of the scheme.
   !
   !  Method: The warm rain process is according to Hsie and Anthes (1984)
   !          and Dudhia (1989)
   !
   !  Assumptions: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)               :: grid

   type (xb_type), intent(in)    :: xb           ! First guess structure.
   type (x_type), intent(inout)  :: xa           ! increment structure.
   type (xpose_type), intent(inout) :: xp        ! Dimensions and xpose buffers.

   real, dimension(ims:ime,jms:jme,kms:kme) :: T_OLD,T_NEW
   real, dimension(ims:ime,jms:jme,kms:kme) :: Q_OLD,Q_NEW
   real, dimension(ims:ime,jms:jme,kms:kme) :: QCW_OLD,QCW_NEW
   real, dimension(ims:ime,jms:jme,kms:kme) :: QRN_OLD,QRN_NEW

   real, dimension(kms:kme)                 :: EES, QVSS
   real, dimension(kms:kme)                 :: EES9, QVSS9

   real, dimension(kms:kme)                   :: DT
   real, dimension(kms:kme)                   :: QVT,QCT,QRT,TTT
   real, dimension(kms:kme)                   :: QVT9,QCT9,QRT9,TTT9
   real, dimension(kms:kme) :: SCR2,SCR3,SCR4,SCR5,SCR6
   real, dimension(kms:kme) :: DUM31
   real, dimension(kms:kme) :: PRA,PRC,PRD,PRE
   real, dimension(kms:kme) :: SCR31,SCR42,SCR71
   real, dimension(kms:kme) :: DUM112,DUM113,DUM211,DUM411
   real, dimension(kms:kme) :: PRA2,PRC2

   real, dimension(kms:kme) :: SCR29,SCR39,SCR49,SCR59,SCR69
   real, dimension(kms:kme) :: DUM319
   real, dimension(kms:kme) :: PRA9,PRC9,PRD9,PRE9
   real, dimension(kms:kme) :: SCR319,SCR429,SCR719
   real, dimension(kms:kme) :: DUM1129,DUM1139,DUM2119,DUM4119
   real, dimension(kms:kme) :: TMP

   integer :: i, j, k

   if (trace_use) call da_trace_entry("da_moist_phys_adj")

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            QRN_NEW(i,j,k) = xa % qrn (i,j,k) 
            QRN_OLD(i,j,k) = xa % qrn (i,j,k)
            QCW_NEW(i,j,k) = xa % qcw (i,j,k)
            QCW_OLD(i,j,k) = xa % qcw (i,j,k)
            Q_NEW(i,j,k) = xa % q (i,j,k)
            Q_OLD(i,j,k) = xa % q (i,j,k)
            T_NEW(i,j,k) = xa % t (i,j,k)
            T_OLD(i,j,k) = xa % t (i,j,k)
         end do
      end do
   end do

   call da_filter_adj(grid,t_new, xp)
   call da_filter_adj(grid,q_new, xp)
   call da_filter_adj(grid,qcw_new, xp)
   call da_filter_adj(grid,qrn_new, xp)

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            xa % qrn (i,j,k) = QRN_NEW(i,j,k)
            QRN_OLD(i,j,k) = QRN_OLD(i,j,k) - QRN_NEW(i,j,k)
            xa % qcw (i,j,k) = QCW_NEW(i,j,k)
            QCW_OLD(i,j,k) = QCW_OLD(i,j,k) - QCW_NEW(i,j,k)
            xa % q (i,j,k) = Q_NEW(i,j,k)
            Q_OLD(i,j,k) = Q_OLD(i,j,k) - Q_NEW(i,j,k)
            xa % t (i,j,k) = T_NEW(i,j,k)
            T_OLD(i,j,k) = T_OLD(i,j,k) - T_NEW(i,j,k)
         end do
      end do
   end do

   do j=jts,jte
      do i=its,ite

         ! Preparation

         do K=kts,kte
            DT(k) = xb%delt(i,j,k)
         end do

         do K=kts,kte

            if (DT(k) <= 0.) cycle

            if( xb%t(I,J,K) > TO )then
               EES(K)=SVP1*EXP(SVP2*(xb%t(I,J,K)-SVPT0)/(xb%t(I,J,K)-SVP3))
            else
               EES(K)=.611*EXP(22.514-6.15E3/xb%t(I,J,K))
            end if

            QVSS(K)=622.*EES(K)/(xb%p(I,J,K)-EES(K))

            SCR4(K)=xb%q(I,J,K)/QVSS(K)

         end do

         do K=kts,kte

            if (DT(k) <= 0.) cycle

            if(xb%qcw(I,J,K) > 0.) then
               SCR2(K)=xb%qcw(I,J,K)
            else
               SCR2(K)=0.
            end if
            if(xb%qrn(I,J,K) > 1.E-25) then
               SCR3(K)=xb%qrn(I,J,K)
            else
               SCR3(K)=1.E-25
            end if
            SCR5(K)=xb%q(I,J,K)/SCR4(K)

            SCR6(K)=xb%p(I,J,K)/(gas_constant*xb%t(I,J,K))

            DUM31(K)=3.1484E6-XLV1*xb%t(I,J,K)

         end do

         call da_autoc(DT,SCR2,PRC,kts,kte,kms,kme)

         do k=kts,kte
            PRC2(K)=PRC(K)
         end do

         call da_accre(DT,SCR2,SCR3,PRA,kts,kte,kms,kme)

         do k=kts,kte
            PRA2(K)=PRA(K)
         end do

         call da_evapo(DT,SCR3,SCR5,xb%q(I,J,:),PRE,SCR6,kts,kte,kms,kme)

         do K=kts, kte

            if (DT(k) <= 0.) cycle

            !  Readjust

            DUM112(K)=(PRC(k)+PRA(k))*DT(k)
            if (DUM112(K) > SCR2(k)) then
               PRC(k)=SCR2(K)*PRC(K)/DUM112(K)
               PRA(k)=SCR2(K)*PRA(K)/DUM112(K)
            end if
            QVT(K)=-PRE(K)
            QCT(K)=-PRC(K)-PRA(K)
            QRT(K)=PRC(K)+PRA(K)+PRE(K)
            if(xb%t(I,J,K).GT.TO)then
               DUM411(K)=DUM31(K)
            else
               DUM411(K)=XLS
            end if
            PRD(K)=cp*(1.+0.887*xb%q(I,J,K))
            TTT(K)=-DUM411(K)*QVT(K)/PRD(K)

            DUM113(K)=xb%q(I,J,K)+DT(K)*QVT(K)
            if(DUM113(K) > 1.E-12 ) then
               SCR42(K)=DUM113(K)
            else
               SCR42(K)=1.E-12
            end if
            DUM211(K)=xb%qcw(I,J,K)+QCT(K)*DT(k)
            if(DUM211(K) > 0.) then
               SCR31(K)=DUM211(K)
            else
               SCR31(K)=0.
            end if
            SCR71(K)=xb%t(I,J,K)+TTT(K)*DT(k)
         end do

         call da_condens_adj(DT,SCR31,SCR42,SCR71,DUM31,PRD,       &
                             QVT,QCT,QRT,TTT,                      &
                             xb%p(I,J,:),xb%t(I,J,:),xb%q(I,J,:),  &
                             xb%qcw(I,J,:),xb%qrn(I,J,:),          &
                             SCR319,SCR429,SCR719,DUM319,PRD9,     &
                             QVT9,QCT9,QRT9,TTT9,                  &
                             xa%p(I,J,:),xa%t(I,J,:),xa%q(I,J,:),  &
                             xa%qcw(I,J,:),xa%qrn(I,J,:),kts,kte)

         do K=kts, kte
            if (DT(k) <= 0.) cycle

            !  Readjust

            xa%t(I,J,K)=xa%t(I,J,K)+SCR719(K)
            TTT9(K)=TTT9(K)+DT(k)*SCR719(K)
            DUM2119(K)=0.
            if(DUM211(K) > 0.) then
               DUM2119(K)=SCR319(K)
            end if
            xa%qcw(I,J,K)=xa%qcw(I,J,K)+DUM2119(K)
            QCT9(K)=QCT9(K)+DT(K)*DUM2119(K)
            DUM1139(K)=0.
            if(DUM113(K) > 1.E-12 ) then
               DUM1139(K)=SCR429(K)
            end if
            xa%q(I,J,K)=xa%q(I,J,K)+DUM1139(K)
            QVT9(K)=QVT9(K)+DT(K)*DUM1139(K)
            PRD9(K)=PRD9(K)+DUM411(K)*QVT(K)/(PRD(K)*PRD(K))*TTT9(K)
            QVT9(K)=QVT9(K)-DUM411(K)/PRD(K)*TTT9(K)
            DUM4119(K)=-QVT(K)/PRD(K)*TTT9(K)
            xa%q(I,J,K)=xa%q(I,J,K)+cp*0.887*PRD9(K)

            if(xb%t(I,J,K).GT.TO)then
               DUM319(K)=DUM319(K)+DUM4119(K)
            end if
            PRC9(K)=QRT9(K)
            PRA9(K)=QRT9(K)
            PRE9(K)=QRT9(K)
            PRC9(K)=PRC9(K)-QCT9(K)
            PRA9(K)=PRA9(K)-QCT9(K)
            PRE9(K)=PRE9(K)-QVT9(K)

            SCR29(K)=0.
            DUM1129(K)=0.
            if (DUM112(K) > SCR2(k)) then
               DUM1129(K)=-SCR2(K)*PRA2(K)/(DUM112(K)*DUM112(K))*PRA9(K)
               SCR29(K)=PRA2(K)/DUM112(K)*PRA9(K)
               PRA9(K)=PRA9(K)*SCR2(K)/DUM112(K)
               DUM1129(K)=DUM1129(K)-SCR2(K)*PRC2(K)/(DUM112(K)*DUM112(K))*PRC9(K)
               SCR29(K)=SCR29(K)+PRC2(K)/DUM112(K)*PRC9(K)
               PRC9(K)=PRC9(K)*SCR2(K)/DUM112(K)
            end if
            PRC9(K)=PRC9(K)+DT(K)*DUM1129(K)
            PRA9(K)=PRA9(K)+DT(K)*DUM1129(K)

         end do

         call da_evapo_adj(DT,SCR3,SCR5,xb%q(I,J,:),PRE,SCR6,  &
                           SCR39,SCR59,xa%q(I,J,:),PRE9,SCR69, &
                           kts,kte,kms,kme)

         call da_accre_adj(DT,SCR2,SCR3,PRA,SCR29,SCR39,PRA9,kts,kte,kms,kme)

         call da_autoc_adj(DT,SCR2,PRC,SCR29,PRC9,kts,kte,kms,kme)

         !  Preparation

         do K=kts,kte

            if (DT(k) <= 0.) cycle

            xa%t(I,J,K)=xa%t(I,J,K)-XLV1*DUM319(K)

            xa%p(I,J,K)=xa%p(I,J,K)+SCR69(K)/(gas_constant*xb%t(I,J,K))
            xa%t(I,J,K)=xa%t(I,J,K)-xb%p(I,J,K)/  &
                        (gas_constant*xb%t(I,J,K)**2)*SCR69(K)
            xa%q(I,J,K)=xa%q(I,J,K)+SCR59(K)/SCR4(K)
            SCR49(K)=-xb%q(I,J,K)/SCR4(K)**2*SCR59(K)

            if(xb%qrn(I,J,K) > 1.E-25) then
               xa%qrn(I,J,K)=xa%qrn(I,J,K)+SCR39(K)
            end if
            if(xb%qcw(I,J,K) > 0.) then
               xa%qcw(I,J,K)=xa%qcw(I,J,K)+SCR29(K)
            end if

         end do

         do K=kts,kte

            if (DT(k) <= 0.) cycle

            xa%q(I,J,K)=xa%q(I,J,K)+SCR49(K)/QVSS(K)
            QVSS9(K)=-xb%q(I,J,K)/QVSS(K)**2*SCR49(K)
            TMP(K)=622./((xb%p(I,J,K)-EES(K))**2)
            EES9(K)=TMP(K)*xb%p(I,J,K)*QVSS9(K)
            xa%p(I,J,K)=xa%p(I,J,K)-TMP(K)*EES(K)*QVSS9(K)
            if( xb%t(I,J,K) > TO )then
               xa%t(I,J,K)=xa%t(I,J,K)+EES(K)*SVP2*(SVPT0-SVP3)/   &
                           ((xb%t(I,J,K)-SVP3)*(xb%t(I,J,K)-SVP3))*EES9(K)
            else
               xa%t(I,J,K)=xa%t(I,J,K)+EES(K)*6.15E3/(xb%t(I,J,K)*   &
                           xb%t(I,J,K))*EES9(K)
            end if

         end do

         do K=kts,kte
            xa % qt (i,j,k) = xa % qt (i,j,k) + xa % q(i,j,k)
            xa % qcw(i,j,k) = xa % qcw(i,j,k) - xa % q(i,j,k)
            xa % qrn(i,j,k) = xa % qrn(i,j,k) - xa % q(i,j,k)
         end do
      end do
   end do

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            xa % qrn (i,j,k) = xa % qrn (i,j,k) + QRN_OLD(i,j,k)
            xa % qcw (i,j,k) = xa % qcw (i,j,k) + QCW_OLD(i,j,k)
            xa % q (i,j,k) = xa % q (i,j,k) + Q_OLD(i,j,k)
            xa % t (i,j,k) = xa % t (i,j,k) + T_OLD(i,j,k)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_moist_phys_adj")

end subroutine da_moist_phys_adj


