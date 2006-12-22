subroutine da_obs_sfc_correction(sfc_obs, xb )

   !--------------------------------------------------------------------
   ! This subroutine is used to correct the surface measurements (wind, 
   ! temperature, and pressure) from the observed height to the WRF     
   ! model's lowest half-zeta level before going to the minimization.   
   !                                                                    
   !   Wind       : based on the similarity theory                      
   !   Temperature: Frank Ruggiero's (1996) method                      
   !   Pressure   : Hydrostatic correction                              
   !                                                                    
   ! The order of the vertical index is "kts=1(bottom) and kte(top)".   
   ! With cv_options=2 and sfc_assi_option=1, this procedure must be    
   ! gone through, otherwise unrealistic results may be obtained.   
   !--------------------------------------------------------------------

   implicit none

   type(synop_type),       intent(inout) :: sfc_obs
   type(xb_type),          intent(in)    :: xb

   real                                  :: roughness, psfc, &
                                            mslp, dx, dxm, dy, dym,    &
                                            ho, po, to, qo,   &
                                            hm, pm, tm, qm, um, vm, correc, val

   integer                               :: i, j, k

   real, dimension(xb%kts:xb%kte)        :: t_mdl, q_mdl, u_mdl, v_mdl, &
                                            height, pressure


   ! 1. Check if it needs to do the surface correction at the first level
 
   ! 1.1 Surface repots located at far below the lowest model level
 
   ! 2. Model profile at OBS site for surface correction

   i = sfc_obs%loc%i
   j = sfc_obs%loc%j
   dx = sfc_obs%loc%dx
   dy = sfc_obs%loc%dy
   dxm = sfc_obs%loc%dxm
   dym = sfc_obs%loc%dym

   ! Model roughness at the obs site

   roughness = dym*(dxm*xb%rough(i,j)   + dx*xb%rough(i+1,j)) &
             + dy *(dxm*xb%rough(i,j+1) + dx*xb%rough(i+1,j+1))

   do k = xb%kts, xb%kte

      pressure(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                  + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))

      height(k)   = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                  + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))

      t_mdl(k)    = dym*(dxm*xb%t(i,j  ,k) + dx*xb% t(i+1,j  ,k)) &
                  + dy *(dxm*xb%t(i,j+1,k) + dx*xb% t(i+1,j+1,k))

      q_mdl(k)    = dym*(dxm*xb%q(i,j  ,k) + dx*xb% q(i+1,j  ,k)) &
                  + dy *(dxm*xb%q(i,j+1,k) + dx*xb% q(i+1,j+1,k))

      u_mdl(k)    = dym*(dxm*xb%u(i,j  ,k) + dx*xb% u(i+1,j  ,k)) &
                  + dy *(dxm*xb%u(i,j+1,k) + dx*xb% u(i+1,j+1,k))

      v_mdl(k)    = dym*(dxm*xb%v(i,j  ,k) + dx*xb% v(i+1,j  ,k)) &
                  + dy *(dxm*xb%v(i,j+1,k) + dx*xb% v(i+1,j+1,k))

   end do 

   ! 3. OBS data and recover the surface pressure from the
   ! mean sea level pressure (mslp)

   ho   = sfc_obs % h
   po   = sfc_obs % p % inv 
   to   = sfc_obs % t % inv
   qo   = sfc_obs % q % inv

   ! 3.1 Compute the surface OBS pressure from mean sea level pressure

   mslp = sfc_obs % loc % slp % inv
   if (abs(mslp - missing_r) > 1.) then
      psfc = missing_r
      if (abs(ho - missing_r) > 1.) then
         if (abs(to - missing_r) > 1.) then
            call da_sfcprs (xb%kts, xb%kte, pressure, t_mdl, q_mdl, &
                         height, psfc, mslp, ho, to)
         else
            call da_sfcprs (xb%kts, xb%kte, pressure, t_mdl, q_mdl, &
                         height, psfc, mslp, ho)
         end if
      end if
      sfc_obs % p % inv = psfc
      ! YRG: to allow assimlate the Psfc from mslp:
      sfc_obs % p % qc  = 0
   end if

   if (sfc_obs % p % inv < 1.0) then
      sfc_obs % p % qc  = missing
   end if

   po = sfc_obs % p % inv

   ! 3.2 Check that obs pressure and height are present
   !     ----------------------------------------------

   if (abs(po - missing_r) < 1.  .OR. &
       abs(ho - missing_r) < 1.) then

      return

      ! write(unit=message(1), fmt='(/3(1x,a))') &
      !    'MISSinG HEIGHT OR PRESSURE OBSERVATION ID ', &
      !    trim (sfc_obs%info % id), trim (sfc_obs%info % name)

      ! write(unit=message(2), fmt='(2(A,F12.3,/))') &
      !                         ' height   = ',ho,&
      !                         ' pressure = ',po
      ! call da_error(__FILE__,__LINE__,message(1:2))

   end if

   ! 4.  Bring surface observation below model levels with good height quality
   ! ================================================

   if ( sfc_obs % h < height(xb%kts)) then

      ! 2.3 Make use of local variables for model  
      !     -------------------------------------

      um = u_mdl(xb%kts)
      vm = v_mdl(xb%kts)
      tm = t_mdl(xb%kts)
      pm = pressure (xb%kts)
      qm = q_mdl(xb%kts)
      hm = height(xb%kts)

      ! 3.2 Correction wind based on similarity laws
      !     -------------------------------------------

      if ((abs(sfc_obs % u % inv - missing_r) > 1.).AND.&
          (abs(sfc_obs % v % inv - missing_r) > 1.))then

         ! 3.2.1 Correction factor

         ! temperature and moisture at sigma level:

         if (abs(to - missing_r) < 1.0) then
            correc = da_mo_correction( ho, po, tm, qo, &
                                     hm, pm, tm, qm, um ,vm, roughness )
         else
            correc = da_mo_correction( ho, po, to, qo, &
                                    hm, pm, tm, qm, um ,vm, roughness )
         end if

         ! 3.2.2 Wind correction 
         !       ---------------

         !  Correct data and replace any previous wind qcs
         !  with surface correction flag

         sfc_obs % u % inv = correc * sfc_obs % u % inv 
         sfc_obs % u % qc  = surface_correction

         sfc_obs % v % inv = correc * sfc_obs % v % inv
         sfc_obs % v % qc  = surface_correction
      end if

      ! 3.4 Correct pressure
      !     ----------------


      if (sfc_obs % p % qc >= 0) then

         !  Correct data

         if (abs(to  - missing_r) > 1. .and. &
            abs(qo - missing_r) > 1.) then
            call da_intpsfc_prs (val, ho, po, hm, tm, qm, to, qo)
         else if (abs(to  - missing_r) > 1.) then
            call da_intpsfc_prs (val, ho, po, hm, tm, qm, to)
         else
            call da_intpsfc_prs (val, ho, po, hm, tm, qm)
         end if

         !  Replace any previous pressure qc by the surface correction

         sfc_obs % p % inv = val
         sfc_obs % p % qc  = surface_correction
      end if

      ! 3.5 Correct temperature
      !     -------------------

      if (abs(sfc_obs % t % inv - missing_r) > 1.) then

         !  Correct data

         call da_intpsfc_tem (val, ho, po, to, height, pressure, t_mdl, &
            xb%kts, xb%kte)

         sfc_obs % t % inv = val

         !  Replace any previous temperature qc by the surface correction

         sfc_obs % t % qc  = surface_correction

      end if

      ! 3.6 Assign model lowest level height + 1m to observation
      !      ----------------------------------------------------- 

      ! sfc_obs % h = height(xb%kts) + 1.

      ! 3.7 Height QC
      !     ---------
 
      ! sfc_obs % height_qc = surface_correction

   end if

end  subroutine da_obs_sfc_correction


