subroutine open_gks(fname)

   implicit none

   character(len=80), intent(in) :: fname

   character(len=80) :: cdum

   integer           :: idum

   call gopks(6,idum)

   call gesc(-1391,1,fname,1,idum,cdum)
   call gopwk(2,9,3)
   call gopwk(1,3,1)
   call gacwk(1)

   call gsclip(0)

end subroutine open_gks

