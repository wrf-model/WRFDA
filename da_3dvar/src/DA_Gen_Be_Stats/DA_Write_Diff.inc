SUBROUTINE DA_Write_Diff (unit, file_date, number, diff, xp,   &
                        ids,ide, jds,jde, kds,kde,  &
                        ims,ime, jms,jme, kms,kme,  &
                        its,ite, jts,jte, kts,kte, xb )

!-------------------------------------------------------------------------
!  PURPOSE: Write Xprime (Difference)
!
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

  type (statistics_type), INTENT(IN) :: diff
  type (xpose_type),      INTENT(IN) :: xp         ! Dimensions and xpose buf
  type (xb_type), INTENT(IN), optional :: xb        ! Dimensions and xpose buf
  
  INTEGER,      INTENT(IN)        :: unit, number

  INTEGER,      INTENT(IN)        :: ids,ide, jds,jde, kds,kde   ! domain dims.
  INTEGER,      INTENT(IN)        :: ims,ime, jms,jme, kms,kme   ! memory dims.
  INTEGER,      INTENT(IN)        :: its,ite, jts,jte, kts,kte   ! tile  dims.

  character(*), intent(in)        :: file_date
  character(len=80)               :: filename
  character(len=10)               :: time, head
  character(len=2)                :: mem
  integer                         :: k
  real                            :: dphi !Latitude interval (not used)
!--------------------------------------------------------------------
  head = 'diff.'
!
  IF (BACKGROUND_OPTION == 1) then
  filename = trim(head)//file_date(1:19)
  ELSEIF (BACKGROUND_OPTION == 2) then
  if (number.lt.10) write(mem,'(I1)') number
  if (number.ge.10.and.number.lt.100) write(mem,'(I2)') number
  filename = trim(head)//file_date(1:19)//'.'//trim(mem)
  ENDIF

!--------------------------------------------------------------------
  open(unit,file=filename,form='unformatted')

  time(1:10)=file_date(1:4)//file_date(6:7)//file_date(9:10)//file_date(12:13)

  write(unit)  time,  ide, jde, kde    

!
  write(unit) diff%psi(its:ite,jts:jte,kts:kte)

  write(unit) diff%chi(its:ite,jts:jte,kts:kte)
  write(unit) diff%t(its:ite,jts:jte,kts:kte)
  write(unit) diff%rh(its:ite,jts:jte,kts:kte)
  write(unit) diff%psfc(its:ite,jts:jte)

  write(unit) xb%h(its:ite,jts:jte,kts:kte)
  write(unit) xb%lat(its:ite,jts:jte)

  close(unit)

  END SUBROUTINE DA_Write_Diff
