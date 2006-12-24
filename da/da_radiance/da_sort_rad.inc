subroutine da_sort_rad (iv)
!------------------------------------------------------------------------------
!  PURPOSE: sorting radiance innovation to FGAT time bins
!
!   METHOD: using a temporary radaince variable  
!
!  HISTORY: 04/26/2006 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

  implicit none

! subroutine argument
  type (ob_type)    ,  intent (inout) :: iv

! local variables
  integer                   ::  inst, i,j, n, error
  type(rad_type), pointer   ::  rad(:)

!**************************************************************************

    if (num_fgat_time == 1) then
      do inst=1,rtminit_nsensor
        iv%ob_numb(:)%radiance(inst) = 0
        iv%ob_numb(num_fgat_time)%radiance(inst) = iv%instid(inst)%num_rad
      end do
      return
    end if

  do inst=1,rtminit_nsensor
     iv%ob_numb(:)%radiance(inst) = 0
     if (iv%instid(inst)%num_rad < 1) cycle

     allocate(rad(iv%instid(inst)%num_rad), stat=error)
     if (error /= 0) then
       call da_error(__FILE__,__LINE__, &
         (/"Cannot allocate memory to rad"/))
     end if

     j = 0
  do i = 1,num_fgat_time
  do n = 1,iv%instid(inst)%num_rad
     if (iv%instid(inst)%rad(n)%ifgat /= i) cycle
          j = j + 1
          rad(j)%ifgat       =iv%instid(inst)%rad(n)%ifgat
          rad(j)%info        =iv%instid(inst)%rad(n)%info
          rad(j)%loc         =iv%instid(inst)%rad(n)%loc
          rad(j)%landsea_mask=iv%instid(inst)%rad(n)%landsea_mask 
          rad(j)%scanpos     =iv%instid(inst)%rad(n)%scanpos 
          rad(j)%satzen      =iv%instid(inst)%rad(n)%satzen 
          rad(j)%satazi      =iv%instid(inst)%rad(n)%satazi
          rad(j)%solzen      =iv%instid(inst)%rad(n)%solzen
          allocate (rad(j)%tb(iv%instid(inst)%nchan), stat=error)
          if (error /= 0) then
             call da_error(__FILE__,__LINE__, &
               (/"Cannot allocate memory to rad(j)%tb"/))
          end if
          rad(j)%tb(1:iv%instid(inst)%nchan)%inv=iv%instid(inst)%rad(n)%tb(1:iv%instid(inst)%nchan)%inv 
  end do
          iv%ob_numb(i)%radiance(inst) = j
  end do

   write(unit=stdout,fmt='(a,2x,a,2x,10i7)') &
      ' FGAT: ',iv%instid(inst)%rttovid_string, iv%ob_numb(1:num_fgat_time)%radiance(inst)

  do n = 1,iv%instid(inst)%num_rad
       iv%instid(inst)%rad(n)%ifgat = rad(n)%ifgat
       iv%instid(inst)%rad(n)%info  = rad(n)%info 
       iv%instid(inst)%rad(n)%loc   = rad(n)%loc 
       iv%instid(inst)%rad(n)%landsea_mask = rad(n)%landsea_mask
       iv%instid(inst)%rad(n)%scanpos = rad(n)%scanpos
       iv%instid(inst)%rad(n)%satzen = rad(n)%satzen
       iv%instid(inst)%rad(n)%satazi = rad(n)%satazi
       iv%instid(inst)%rad(n)%solzen = rad(n)%solzen
       iv%instid(inst)%rad(n)%tb(1:iv%instid(inst)%nchan)%inv = &
                       rad(n)%tb(1:iv%instid(inst)%nchan)%inv
       deallocate(rad(n)%tb)
  end do
       deallocate(rad)
  end do

end subroutine da_sort_rad


