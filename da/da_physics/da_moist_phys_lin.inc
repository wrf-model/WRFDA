subroutine da_moist_phys_lin(grid)

   !---------------------------------------------------------------------------
   !  Purpose: Partition of the hydrometeors via the moist explicit scheme.
   !           A warm rain process is used in this subroutine.
   !           This is the tangent linear code of the scheme.
   !
   !  Method: The warm rain process is according to Hsie and Anthes (1984)
   !          and Dudhia (1989)
   !
   !  Assumptions: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)               :: grid

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

   real, dimension(kms:kme) :: SCR29,SCR39,SCR49,SCR59,SCR69
   real, dimension(kms:kme) :: DUM319
   real, dimension(kms:kme) :: PRA9,PRC9,PRD9,PRE9
   real, dimension(kms:kme) :: SCR319,SCR429,SCR719
   real, dimension(kms:kme) :: DUM1129,DUM1139,DUM2119,DUM4119
   real, dimension(kms:kme) :: TMP

   integer :: i, j, k

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            T_OLD(i,j,k) = grid%xa % t (i,j,k)
            Q_OLD(i,j,k) = grid%xa % q (i,j,k)
            QCW_OLD(i,j,k) = grid%xa % qcw (i,j,k)
            QRN_OLD(i,j,k) = grid%xa % qrn (i,j,k)
         end do
      end do
   end do

   do j=jts,jte
      do i=its,ite

      !  Preparation

         do K=kts,kte
            grid%xa % q(i,j,k) =grid%xa % qt(i,j,k) - grid%xa % qcw(i,j,k) - grid%xa %qrn(i,j,k)
         end do

         do K=kts,kte
            DT(k) = grid%xb%delt(i,j,k)
         end do

         do K=kts,kte

            if (DT(k) <= 0.) cycle

            if ( grid%xb%t(I,J,K) > TO )then
               EES(K)=SVP1*EXP(SVP2*(grid%xb%t(I,J,K)-SVPT0)/(grid%xb%t(I,J,K)-SVP3))
               EES9(K)=EES(K)*SVP2*(SVPT0-SVP3)/((grid%xb%t(I,J,K)-SVP3)*  &
                       (grid%xb%t(I,J,K)-SVP3))*grid%xa%t(I,J,K)
            else
               EES(K)=.611*EXP(22.514-6.15E3/grid%xb%t(I,J,K))
               EES9(K)=EES(K)*6.15E3/(grid%xb%t(I,J,K)*grid%xb%t(I,J,K))*grid%xa%t(I,J,K)
            end if

            TMP(K)=622./((grid%xb%p(I,J,K)-EES(K))**2)
            QVSS9(K)=TMP(K)*grid%xb%p(I,J,K)*EES9(K)  &
                  -TMP(K)*EES(K)*grid%xa%p(I,J,K)
            QVSS(K)=622.*EES(K)/(grid%xb%p(I,J,K)-EES(K))


            SCR49(K)=grid%xa%q(I,J,K)/QVSS(K)-grid%xb%q(I,J,K)/QVSS(K)**2*QVSS9(K)
            SCR4(K)=grid%xb%q(I,J,K)/QVSS(K)
         end do

         do K=kts,kte

            if (DT(k) <= 0.) cycle

            if (grid%xb%qcw(I,J,K) > 0.) then
               SCR29(K)=grid%xa%qcw(I,J,K)
               SCR2(K)=grid%xb%qcw(I,J,K)
            else
               SCR29(K)=0.
               SCR2(K)=0.
            end if
            if (grid%xb%qrn(I,J,K) > 1.E-25) then
               SCR39(K)=grid%xa%qrn(I,J,K)
               SCR3(K)=grid%xb%qrn(I,J,K)
            else
               SCR39(K)=0.
               SCR3(K)=1.E-25
            end if
            SCR59(K)=grid%xa%q(I,J,K)/SCR4(K)-grid%xb%q(I,J,K)/SCR4(K)**2*SCR49(K)
            SCR5(K)=grid%xb%q(I,J,K)/SCR4(K)

            SCR69(K)=grid%xa%p(I,J,K)/(gas_constant*grid%xb%t(I,J,K))-grid%xb%p(I,J,K)/  &
                     (gas_constant*grid%xb%t(I,J,K)**2)*grid%xa%t(I,J,K)
            SCR6(K)=grid%xb%p(I,J,K)/(gas_constant*grid%xb%t(I,J,K))

            DUM319(K)=-XLV1*grid%xa%t(I,J,K) 
            DUM31(K)=3.1484E6-XLV1*grid%xb%t(I,J,K)

         end do

         call da_autoc_lin(DT,SCR2,PRC,SCR29,PRC9,kts,kte,kms,kme)

         call da_accre_lin(DT,SCR2,SCR3,PRA,SCR29,SCR39,PRA9,kts,kte,kms,kme)

         call da_evapo_lin(DT,SCR3,SCR5,grid%xb%q(I,J,:),PRE,SCR6,  &
                           SCR39,SCR59,grid%xa%q(I,J,:),PRE9,SCR69, &
                           kts,kte,kms,kme)

         do K=kts, kte

            if (DT(k) <= 0.) cycle

            !  Readjust

            DUM1129(K)=(PRC9(k)+PRA9(k))*DT(k)
            DUM112(K)=(PRC(k)+PRA(k))*DT(k)
            if (DUM112(K) > SCR2(k)) then
               PRC9(K)=SCR29(K)*PRC(K)/DUM112(K)  &
                      +PRC9(K)*SCR2(K)/DUM112(K)  &
                      -SCR2(K)*PRC(K)/(DUM112(K)*DUM112(K))*DUM1129(K)
               PRC(k)=SCR2(K)*PRC(K)/DUM112(K)
               PRA9(K)=SCR29(K)*PRA(K)/DUM112(K)  &
                      +PRA9(K)*SCR2(K)/DUM112(K)  &
                      -SCR2(K)*PRA(K)/(DUM112(K)*DUM112(K))*DUM1129(K)
               PRA(k)=SCR2(K)*PRA(K)/DUM112(K)
            end if
            QVT9(K)=-PRE9(K)
            QVT(K)=-PRE(K)
            QCT9(K)=-PRC9(K)-PRA9(K)
            QCT(K)=-PRC(K)-PRA(K)
            QRT9(K)=PRC9(K)+PRA9(K)+PRE9(K)
            QRT(K)=PRC(K)+PRA(K)+PRE(K)
            if (grid%xb%t(I,J,K).GT.TO)then
               DUM4119(K)=DUM319(K)
               DUM411(K)=DUM31(K)
            else
               DUM4119(K)=0.
               DUM411(K)=XLS
            end if
            PRD9(K)=cp*0.887*grid%xa%q(I,J,K)
            PRD(K)=cp*(1.+0.887*grid%xb%q(I,J,K))
            TTT9(K)=-DUM4119(K)*QVT(K)/PRD(K)  &
                   -QVT9(K)*DUM411(K)/PRD(K)  &
                   +DUM411(K)*QVT(K)/(PRD(K)*PRD(K))*PRD9(K)
            TTT(K)=-DUM411(K)*QVT(K)/PRD(K)

            DUM1139(K)=grid%xa%q(I,J,K)+DT(K)*QVT9(K)
            DUM113(K)=grid%xb%q(I,J,K)+DT(K)*QVT(K)
            if (DUM113(K) > 1.E-12 ) then
               SCR429(K)=DUM1139(K)
               SCR42(K)=DUM113(K)
            else
               SCR429(K)=0.
               SCR42(K)=1.E-12
            end if
            DUM2119(K)=grid%xa%qcw(I,J,K)+QCT9(K)*DT(k)
            DUM211(K)=grid%xb%qcw(I,J,K)+QCT(K)*DT(k)
            if (DUM211(K) > 0.) then
               SCR319(K)=DUM2119(K)
               SCR31(K)=DUM211(K)
            else
               SCR319(K)=0.
               SCR31(K)=0.
            end if
            SCR719(K)=grid%xa%t(I,J,K)+TTT9(K)*DT(k)
            SCR71(K)=grid%xb%t(I,J,K)+TTT(K)*DT(k)
         end do

         call da_condens_lin(DT,SCR31,SCR42,SCR71,DUM31,PRD,         &
                             QVT,QCT,QRT,TTT,                        &
                             grid%xb%p(I,J,:),grid%xb%t(I,J,:),grid%xb%q(I,J,:),    &
                             grid%xb%qcw(I,J,:),grid%xb%qrn(I,J,:),            &
                             SCR319,SCR429,SCR719,DUM319,PRD9,       &
                             QVT9,QCT9,QRT9,TTT9,                    &
                             grid%xa%p(I,J,:),grid%xa%t(I,J,:),grid%xa%q(I,J,:),    &
                             grid%xa%qcw(I,J,:),grid%xa%qrn(I,J,:),kts,kte)
      end do
   end do

   do k=kds,kde
      do j=jts,jte
         do i=its,ite
            T_NEW(i,j,k) = grid%xa % t (i,j,k) - T_OLD(i,j,k)
            Q_NEW(i,j,k) = grid%xa % q (i,j,k) - Q_OLD(i,j,k)
            QCW_NEW(i,j,k) = grid%xa % qcw (i,j,k) - QCW_OLD(i,j,k)
            QRN_NEW(i,j,k) = grid%xa % qrn (i,j,k) - QRN_OLD(i,j,k)
         end do
      end do
   end do

   call da_filter(grid, t_new, grid%xp)
   call da_filter(grid, q_new, grid%xp)
   call da_filter(grid, qcw_new, grid%xp)
   call da_filter(grid, qrn_new, grid%xp)

   do k=kds,kde
      do j=jts,jte
         do i=its,ite
            grid%xa % t (i,j,k) = T_NEW(i,j,k) + T_OLD(i,j,k)
            grid%xa % q (i,j,k) = Q_NEW(i,j,k) + Q_OLD(i,j,k)
            grid%xa % qcw (i,j,k) = QCW_NEW(i,j,k) + QCW_OLD(i,j,k)
            grid%xa % qrn (i,j,k) = QRN_NEW(i,j,k) + QRN_OLD(i,j,k)
         end do
      end do
   end do

end subroutine da_moist_phys_lin


