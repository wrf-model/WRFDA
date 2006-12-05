subroutine da_max_error_qc (it, info, field, max_error, ix, ixf, iunit, var, &
                         failed, print)

   implicit none

   integer          ,   intent (in)   :: it
   type (info_type) ,   intent(in)    :: info
   type (field_type),   intent(inout) :: field
   real             ,   intent(in)    :: max_error
   integer          ,   intent(inout) :: ix, ixf
   integer          ,   intent(in)    :: iunit
   character (LEN=*),   intent(in)    :: var
   logical          ,   intent(out)   :: failed
   logical          ,   intent(in)    :: print

   real                               :: err, err_max
   integer                            :: qc_flag

   failed = .false.

   qc_flag = field % qc
   err_max = field % error * max_error
   err     = field % inv
   err     = ABS (err)

   ix     = ix + 1
   if( it > 1 .and. qc_flag == fails_error_max ) field%qc = 0
   if (err > err_max) then
      if (field % qc > fails_error_max) field % qc = fails_error_max 
      !rizvi  field % inv = 0.0

      ixf = ixf + 1
      failed = .true.

      if (print .and. failed) then
         if (err_max .LE. 0.) then
            write (unit=iunit , fmt = '(A,3(F8.1,1X),A,A,A,A5,A,A,3f10.2)')   &
                   "Err_max < 0 ==> ",err,err_max,max_error, " for ", var, &
                   " OBS ID: ", info % id, info % platform,     &
                   " LA/LON/ELV:", info % lat, info % lon, info % elv
            ! call da_error(__FILE__,__LINE__,(/"Erk"/))
         end if

         write (unit=iunit , fmt = '(A,A5,A,A,I5,A,I3,A,F4.1,A,A,A,2e13.6)') &
                "Err_max failed:ID=", info % id, info % platform,&
                "Ix=", ix, " Ixf=", ixf, " Err_max ratio =",err/err_max, &
                " for ", var, " inv, error:",field % inv, field % error
      end if
      field % inv = 0.0
   end if

end subroutine da_max_error_qc

