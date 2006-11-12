subroutine da_read_ssmi_info (iunit, ob, xb, xbx)

   !---------------------------------------------------------------------------
   ! Read the header of a SSMI GTS observation file
   !---------------------------------------------------------------------------

   implicit none

   integer,        intent (in)  :: iunit
   type (xb_type), intent (in)  :: xb
   type (xbx_type),intent (in)  :: xbx     ! Header & non-gridded vars.
   type (ob_type), intent (out) :: ob


   integer                      :: iost, i, ii
   character (LEN = 120)        :: char_ned
   logical                      :: connected

   integer                      :: nssmis,nothers
   integer                      :: ixc, jxc, iproj, idd, maxnes
   integer,  dimension(10)      :: nestix, nestjx, numnc, nesti, nestj
   real                         :: phic   , xlonc  , &
                                   truelat1_3dv, truelat2_3dv, &
                                   ts0    , ps0    , tlp     , ptop
   real   ,  dimension(10)      :: dis

   logical                      :: CHECK_WRONG, CHECK_SUBdoMAin

   iost = -99999

   ! 1. OPEN FILE
   !    ---------

   if( use_ssmiretrievalobs .or. use_ssmitbobs .or. &
       use_ssmt1obs .or. use_ssmt2obs ) then
      OPEN ( unit   = iunit,     &
           FORM   = 'FORMATTED',  &
           ACCESS = 'SEQUENTIAL', &
           iostat =  iost,     &
           STATUS = 'OLD')

      if (iost /= 0) then
         Use_SsmiRetrievalObs = .false.
         Use_SsmiTbObs  = .false.
         use_ssmt1obs   = .false.
         use_ssmt2obs   = .false.

         return
      end if
   else
      return
   end if

   REWinD (unit = iunit)


   ! 2.  read HEADER
   ! ===============

   ! 2.1 read FIRST LINE
   !     ---------------

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

   ! 2.2 PROCESS ERROR
   !     -------------

   if (iost /= 0) then
      Use_SsmiRetrievalObs = .false.
      Use_SsmiTbObs  = .false.
      use_ssmt1obs   = .false.
      use_ssmt2obs   = .false.

      write(unit=stderr, fmt='(/a,i3/)') &
           'ERROR readinG FILE unit:', iunit, &
           'ERROR CODE:', iost

      return
   else
      write(unit=stdout, fmt='(/2a/)') &
           'in da_read_ssmi_info.inc, char_ned=', trim(char_ned)
   end if

   ! 2.3 read NUMBER OF REPORTS
   !     ---------------------

   do
 
      do i = 0, 120-14

         select case ( char_ned (I+1:I+5) )
         ! Number of observations
            case ('SSMI ') ; 
               if (Use_SsmiRetrievalObs) &
                  read (char_ned (I+9:I+14),'(I6)', iostat = iost) &
                     ob%num_ssmi_retrieval
               if (Use_SsmiTbObs) then
                  read (char_ned (I+9:I+14),'(I6)', iostat = iost) ob%num_ssmi_tb
               end if
            case ('OTHER') ; 
               read (char_ned (I+9:I+14),'(I6)', iostat = iost) nothers

               ! Geographic area and reference atmosphere definition

            case ('MISS.') ; 
                read (char_ned (I+8:I+15),'(F8.0)', iostat = iost) ob % missing
            case ('PHIC ') ; 
                read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) phic
            case ('XLONC') ; 
                read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) xlonc
            case ('true1') ; 
                read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) truelat1_3dv
            case ('true2') ; 
                read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) truelat2_3dv
            case ('TS0  ') ; 
                read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) ts0
            case ('TLP  ') ; 
                read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) tlp
            case ('PTOP ') ; 
                read (char_ned (I+8:I+14),'(F7.0)', iostat = iost) ptop
            case ('PS0  ') ; 
                read (char_ned (I+8:I+14),'(F7.0)', iostat = iost) ps0
            case ('IXC  ') ; 
                read (char_ned (I+8:I+14),'(I7)', iostat = iost) ixc
            case ('JXC  ') ; 
                read (char_ned (I+8:I+14),'(I7)', iostat = iost) jxc
            case ('IPROJ') ; 
                read (char_ned (I+8:I+14),'(I7)', iostat = iost) iproj
            case ('IDD  ') ; 
                read (char_ned (I+8:I+14),'(I7)', iostat = iost) idd
            case ('MAXNE') ; 
                read (char_ned (I+8:I+14),'(I7)', iostat = iost) maxnes
            case default ; read (char_ned (I+9:I+14),'(I6)', iostat = iost) nssmis
         end select
      end do

      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

      if (iost /= 0) then
         Use_SsmiRetrievalObs = .false.
         Use_SsmiTbObs  = .false.
         use_ssmt1obs   = .false.
         use_ssmt2obs   = .false.

         write(unit=stderr, fmt='(/a,i3/)') &
            'ERROR readinG FILE unit:', iunit, &
            'ERROR CODE:', iost

         return
      end if
      if (char_ned(1:6) == 'NESTIX') exit

   end do

   do
      select case ( char_ned (1:6) )

         ! Model domains definition

         case ('NESTIX') ;
            call da_read_ssmi_integer_array(nestix, maxnes, 8, 9)
         case ('NESTJX') ; 
            call da_read_ssmi_integer_array(nestjx, maxnes, 8, 9)
         case ('NUMC  ') ; 
            call da_read_ssmi_integer_array(numnc , maxnes, 8, 9)
         case ('DIS   ') ; 
            call da_read_ssmi_real_array   (dis   , maxnes, 8, 9)
         case ('NESTI ') ; 
            call da_read_ssmi_integer_array(nesti , maxnes, 8, 9)
         case ('NESTJ ') ; 
            call da_read_ssmi_integer_array(nestj , maxnes, 8, 9)
      end select

      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
      if (char_ned(1:6) == 'inFO  ') exit

   end do

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

contains

#include "da_read_ssmi_integer_array.inc"
#include "da_read_ssmi_real_array.inc"

end subroutine da_read_ssmi_info
