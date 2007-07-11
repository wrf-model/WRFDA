subroutine da_get_innov_vector_gpspw( it, xb, xp, ob, iv)

   !----------------------------------------------------------------
   ! Purpose: TBD
   !----------------------------------------------------------------

   implicit none

   integer, intent(in)           :: it      ! External iteration.
   type(xb_type), intent(in)    :: xb      ! first guess state.
   type(xpose_type), intent(in) :: xp      ! Domain decomposition vars.
   type(y_type),  intent(inout) :: ob      ! Observation structure.
   type(ob_type), intent(inout) :: iv      ! O-B structure.

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  ! Interpolation weights.
   real                         :: dy, dym  ! Interpolation weights.
   real                         :: model_tpw! Model value u at oblocation.
   integer           :: ittpw,ittpwf

   !--------------------------------------------------------------------------
   integer                :: k            ! Index dimension
   real                   :: dpw, ddpw    ! adjustment pw [mm]
   real                   :: obs_terr     ! real terrain height at GPS site [m]
   real                   :: model_terr   ! model terrain height at GPS site[m]
   real,dimension(kts:kte):: model_q ! model q at GPS site [kg/kg]
   real,dimension(kts:kte):: model_z ! model z at GPS site [m]
   real,dimension(kts:kte):: model_rho ! model rho at GPS site [kg/m^3]

   ! unit_gps = myproc + 140
   !---------------------------------------------------------------------------

   !
   ! GPS TPW Pseudo OBS test:
   !
   if (pseudo_var(1:3) == 'tpw' .and. num_pseudo > 0) then
      ! Deallocate:
      if (iv%num_gpspw > 0) then
         write(unit=stdout, fmt='(a,i4)') &
            'iv%num_gpspw =', iv%num_gpspw
         deallocate(iv % gpspw)
         deallocate(ob % gpspw)
      end if

      use_gpspwobs = .true.

      ! Allocate:
      iv % num_gpspw  = num_pseudo
      iv % ob_numb(1)%gpspw = num_pseudo
      iv % num_pseudo = 0

      allocate(iv % gpspw(1:num_pseudo))
      allocate(ob % gpspw(1:num_pseudo))

      write(unit=stdout,fmt='(a,i2)') &
        '==> GPS TPW pseudo OBS test: num_pseudo=',num_pseudo

      iv % gpspw(1) % loc % x   = pseudo_x
      iv % gpspw(1) % loc % y   = pseudo_y
      iv % gpspw(1) % loc % i   = int(pseudo_x)
      iv % gpspw(1) % loc % j   = int(pseudo_y)
      iv % gpspw(1) % loc % dx  = pseudo_x-real(iv % gpspw(1) % loc % i)
      iv % gpspw(1) % loc % dy  = pseudo_y-real(iv % gpspw(1) % loc % j)
      iv % gpspw(1) % loc % dxm = 1.0 - iv % gpspw(1) % loc % dx
      iv % gpspw(1) % loc % dym = 1.0 - iv % gpspw(1) % loc % dy

      iv % gpspw(1) % tpw % inv   = pseudo_val
      iv % gpspw(1) % tpw % qc    = 0
      iv % gpspw(1) % tpw % error = pseudo_err

      ! To consider the site elevation effect, set the model terrain height
      ! to elevation for pseudo OBS:

      i = iv%gpspw(1)%loc%i
      j = iv%gpspw(1)%loc%j
      dx = iv%gpspw(1)%loc%dx
      dy = iv%gpspw(1)%loc%dy
      dxm = iv%gpspw(1)%loc%dxm
      dym = iv%gpspw(1)%loc%dym

      iv%gpspw(1)%info%elv = dym*(dxm*xb%terr(i,j)   + dx*xb%terr(i+1,j)) + & 
                             dy *(dxm*xb%terr(i,j+1) + dx*xb%terr(i+1,j+1))

      ! Set halo:
      if ((iv%gpspw(1)%loc%i < xp%its-1) .or.(iv%gpspw(1)%loc%i > xp%ite) .or. & 
          (iv%gpspw(1)%loc%j < xp%jts-1) .or.(iv%gpspw(1)%loc%j > xp%jte)) then 
         call da_error(__FILE__,__LINE__,(/"Should never have obs outside halo by now"/))
         ! iv%gpspw(1)%loc%proc_domain_with_halo = .false. 
         iv%gpspw(1)%loc%proc_domain = .false. 
      else 
         ! iv%gpspw(1)%loc%proc_domain_with_halo = .true.  
         iv%gpspw(1)%loc%proc_domain = .false.  

         if (iv%gpspw(1)%loc%i >= xp%its .and. iv%gpspw(1)%loc%i <= xp%ite .and. &  
             iv%gpspw(1)%loc%j >= xp%jts .and. iv%gpspw(1)%loc%j <= xp%jte) then  
            iv%gpspw(1)%loc%proc_domain = .true.  
         end if  
      end if 

      write(unit=stdout,fmt='(a4,2f15.5)') pseudo_var, pseudo_val, pseudo_err
      write(unit=stdout,fmt='(3f15.5)')    pseudo_x,  pseudo_y,  pseudo_z
      write(unit=stdout,fmt='(a,f8.2)')    'iv%gpspw: elv=',iv%gpspw(1)%info%elv
   end if 

   if (iv % num_gpspw > 0) then

      ittpw   = 0
      ittpwf  = 0

      ! write(unit=unit_gps,fmt='(3x,a3,10a10)') ' n ','     lat  ',  &
      !   '     lon  ', '  obs ght ', '  mdl ght ',  &
      !   ' obsh-mdlh', '   obs pw ', '  model pw',  &
      !   '   O-B pw ', '    Dpw   ', '  O-B+Dpw '

      do n=iv%ob_numb(iv%current_ob_time-1)%gpspw + 1, iv%ob_numb(iv%current_ob_time)%gpspw

         ! [1.1] Get horizontal interpolation weights:

         i = iv%gpspw(n)%loc%i
         j = iv%gpspw(n)%loc%j
         dx = iv%gpspw(n)%loc%dx
         dy = iv%gpspw(n)%loc%dy
         dxm = iv%gpspw(n)%loc%dxm
         dym = iv%gpspw(n)%loc%dym

         model_tpw = dym*(dxm*xb%tpw(i,j)   + dx*xb%tpw(i+1,j)) + &
                     dy *(dxm*xb%tpw(i,j+1) + dx*xb%tpw(i+1,j+1))

         if (pseudo_var(1:3) == 'tpw' .and. num_pseudo > 0) then
            ! To compute the 'ob':
            ob % gpspw(n) % tpw = iv % gpspw(n) % tpw % inv + model_tpw
         else
            ! To compute the 'inv':
            iv % gpspw(n) % tpw % inv = 0.0
            if (ob % gpspw(n) % tpw > missing_r .AND. &
                iv % gpspw(n) % tpw % qc >= obs_qc_pointer) then
               dpw = 0.0
               obs_terr   = iv%gpspw(n)%info%elv
               model_terr = dym*(dxm*xb%terr(i,j)   + dx*xb%terr(i+1,j)) + &
                          dy *(dxm*xb%terr(i,j+1) + dx*xb%terr(i+1,j+1))

               if (obs_terr <= model_terr) then
                  model_q(1) = dym*(dxm*xb%q(i,j,1)   + dx*xb%q(i+1,j,1)) + &
                             dy *(dxm*xb%q(i,j+1,1) + dx*xb%q(i+1,j+1,1))
                  model_rho(1) = dym*(dxm*xb%rho(i,j,1) + dx*xb%rho(i+1,j,1)) + &
                             dy *(dxm*xb%rho(i,j+1,1) + dx*xb%rho(i+1,j+1,1))
                  dpw = model_rho(1) * model_q(1) *( obs_terr - model_terr)
               else
                  model_z(1) = dym*(dxm*xb%hf(i,j,1)   + dx*xb%hf(i+1,j,1)) + &
                            dy *(dxm*xb%hf(i,j+1,1) + dx*xb%hf(i+1,j+1,1))
                  do k = kts, kte
                     if (model_z(k) >= obs_terr) exit

                     model_z(k+1) = dym*(dxm*xb%hf(i,j,k+1)   + dx*xb%hf(i+1,j,k+1)) + &
                                dy *(dxm*xb%hf(i,j+1,k+1) + dx*xb%hf(i+1,j+1,k+1))
                     model_q(k) = dym*(dxm*xb%q(i,j,k)   + dx*xb%q(i+1,j,k)) + &
                              dy *(dxm*xb%q(i,j+1,k) + dx*xb%q(i+1,j+1,k))
                     model_rho(k) = dym*(dxm*xb%rho(i,j,k)   + dx*xb%rho(i+1,j,k)) + &
                                dy *(dxm*xb%rho(i,j+1,k) + dx*xb%rho(i+1,j+1,k))

                     if (model_z(k+1) <= obs_terr) then
                        ddpw = model_rho(k) * model_q(k) *( model_z(k+1) - model_z(k))
                     else
                        ddpw = model_rho(k) * model_q(k) *( obs_terr - model_z(k))
                     end if

                     dpw = dpw + ddpw
                  end do
               end if

               iv % gpspw(n) % tpw % inv = ob % gpspw(n) % tpw - model_tpw &
                                         + 0.1*dpw
            end if
         end if

         !------------------------------------------------------------------------
         ! [5.0] Perform optional maximum error check:
         !------------------------------------------------------------------------

         if ( check_max_iv) then
            call da_check_max_iv_gpspw(it, iv % gpspw(n), ittpw,ittpwf)
         end if

         ! write(unit=unit_gps, fmt='(i4,10f10.3)') n, &
         !    iv%gpspw(n)%info%lat, iv%gpspw(n)%info%lon, obs_terr, &
         !    model_terr, obs_terr - model_terr, ob%gpspw(n)%tpw,   &
         !    model_tpw , ob%gpspw(n)%tpw-model_tpw, 0.1*dpw,       &
         !    ob%gpspw(n)%tpw-model_tpw+0.1*dpw
      end do

      if (rootproc .and. check_max_iv_print) then
         write(unit = check_max_iv_unit, fmt ='(A,i5,A)')'For outer iteration ',it, &
            ', Total Rejections for Gpspw follows:'
         write(unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
            'Number of failed TPW-observations:     ',ittpwf, ' on ',ittpw, &
            'Finally all Gpspw rejections ',ittpwf,' on ',ittpw
      end if
   end if

end subroutine da_get_innov_vector_gpspw


