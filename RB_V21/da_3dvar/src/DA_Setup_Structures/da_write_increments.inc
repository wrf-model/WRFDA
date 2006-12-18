SUBROUTINE DA_Write_Increment ( xp, xb, xa, q_cgrid, mu_cgrid, ph_cgrid )

   IMPLICIT NONE

   TYPE(xb_type)                               :: xb
   TYPE(x_type)                                :: xa
   TYPE(xpose_type)                            :: xp
   REAL, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme)   :: q_cgrid, ph_cgrid
   REAL, dimension(xp%ims:xp%ime,xp%jms:xp%jme)                 :: mu_cgrid

! .. Arrays for write out increments:
   integer                                          :: ix, jy, kz
   real, dimension(1:xb%mix,1:xb%mjy)               ::     gbuf_2d
   real, dimension(1:xb%mix+1,1:xb%mjy+1)           ::     gbuf_2dd
   real, dimension(1:xb%mix,1:xb%mjy,1:xb%mkz)      ::     gbuf
   real, dimension(1:xb%mix,1:xb%mjy,1:xb%mkz+1)    ::    wgbuf

   real, dimension(:,:,:), allocatable :: u_global, v_global, w_global, &
                                          p_global, t_global, q_global, &
                                         ph_global
   real, dimension(:,:)  , allocatable :: mu_global, psfc_global, &
                       psac_global, tgrn_global, terr_global, snow_global,&
                        lat_global,  lon_global, lanu_global,             &
                 map_factor_global, cori_global, landmask_global

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

! ---------------------------------------------------------------------------

! This analysis increment data file ("ANALYSIS_INCREMENT") can be converted 
! to MM5V3 MMINPUT file ("MMINPUT_ANALYSIS_INCREMENT") by the program 
! "da_3dvar/utl/convert_to_mm5v3.f90" with a template file named as 
! "MMIMPUT_template", which is any of the available MMINPUT files 
! (for details see the internal documentation inside the program
! "da_3dvar/utl/convert_to_mm5v3.f90").
!
! Then, MM5/GRAPH can be used to plot the horizontal maps and cross-section 
! maps of the increments fileds with the file: "MMINPUT_ANALYSIS_INCREMENT".
!
!                                        Yong-Run Guo 07/20/2004

! .. Dimension of the domain:
      ix = xb%mix
      jy = xb%mjy
      kz = xb%mkz

#ifdef DM_PARALLEL
 
! .. 3-d and 2-d increments:

      ALLOCATE (    p_global (1:ix+1,1:jy+1,1:kz+1) )
      ALLOCATE (    t_global (1:ix+1,1:jy+1,1:kz+1) )
      ALLOCATE (    q_global (1:ix+1,1:jy+1,1:kz+1) )
      ALLOCATE (    u_global (1:ix+1,1:jy+1,1:kz+1) )
      ALLOCATE (    v_global (1:ix+1,1:jy+1,1:kz+1) )
      ALLOCATE (    w_global (1:ix+1,1:jy+1,1:kz+1) )
      ALLOCATE (   ph_global (1:ix+1,1:jy+1,1:kz+1) )
      ALLOCATE ( psfc_global (1:ix+1,1:jy+1) )
      ALLOCATE (   mu_global (1:ix+1,1:jy+1) )
      call local_to_global( xp, xa % p, gbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        p_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
      END IF 
      call local_to_global( xp, xa % t, gbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        t_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
      END IF 
      call local_to_global( xp, q_cgrid, gbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        q_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
      END IF 
      call local_to_global( xp, xa % u, gbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        u_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
      END IF 
      call local_to_global( xp, xa % v, gbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        v_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
      END IF

! One more level for w and ph:
      xp%kde = xp%kde+1
      call local_to_global( xp, xa % w, wgbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        w_global(1:ix,1:jy,1:kz+1) = wgbuf(1:ix,1:jy,1:kz+1) 
      END IF 
      call local_to_global( xp, ph_cgrid, wgbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        ph_global(1:ix,1:jy,1:kz+1) = wgbuf(1:ix,1:jy,1:kz+1) 
      END IF 
      xp%kde = xp%kde-1
! 
      call local_to_global( xp, xa % psfc, gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        psfc_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF 
      call local_to_global( xp, mu_cgrid, gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        mu_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF 

! .. 2d constant fields:

      ALLOCATE (       psac_global (1:ix+1,1:jy+1) )
      ALLOCATE (       tgrn_global (1:ix+1,1:jy+1) )
      ALLOCATE (       terr_global (1:ix+1,1:jy+1) )
      ALLOCATE (       snow_global (1:ix+1,1:jy+1) )
      ALLOCATE (        lat_global (1:ix+1,1:jy+1) )
      ALLOCATE (        lon_global (1:ix+1,1:jy+1) )
      ALLOCATE (       lanu_global (1:ix+1,1:jy+1) )
      ALLOCATE ( map_factor_global (1:ix+1,1:jy+1) )
      ALLOCATE (       cori_global (1:ix+1,1:jy+1) )
      ALLOCATE (   landmask_global (1:ix+1,1:jy+1) )

      call local_to_global( xp, xb%psac, gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        psac_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF
      call local_to_global( xp, xb%tgrn, gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        tgrn_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF
      call local_to_global( xp, xb%terr, gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        terr_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF
      call local_to_global( xp, xb%snow, gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        snow_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF
      call local_to_global( xp, xb%lat , gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
         lat_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF
      call local_to_global( xp, xb%lon , gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
         lon_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF
      call local_to_global( xp, xb%lanu, gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        lanu_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF
      call local_to_global( xp, xb%map_factor, gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        map_factor_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF
      call local_to_global( xp, xb%cori, gbuf_2dd, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        cori_global(1:ix+1,1:jy+1) = gbuf_2dd(1:ix+1,1:jy+1) 
      END IF
      call local_to_global( xp, xb%landmask, gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        landmask_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF

#endif

      if ( wrf_dm_on_monitor() ) then
        open(unit=141, file='ANALYSIS_INCREMENT', form='unformatted')

         write (141) ANALYSIS_DATE

         write (141) 1, ix, 1, jy, 1, kz 

! .. Map projection information:
         write (141) &
          map_projection, coarse_ix, coarse_jy, coarse_ds, start_x, start_y, &
          phic, xlonc, cone_factor, truelat1_3dv, truelat2_3dv, pole, dsm,   &
          psi1, c2, ptop, ps0, ts0, tlp, tis0

!    .. 1d constant fields:

         write (141) xb%sigmah, xb%sigmaf

#ifdef DM_PARALLEL

!    .. 3d- and 2d-increments:
         write (141) u_global, v_global, w_global, p_global, t_global, &
                   q_global, ph_global, mu_global, psfc_global

!    .. 2d-constant fields:
         write (141) psac_global, tgrn_global, terr_global, snow_global,  &
                  lat_global, lon_global, lanu_global, map_factor_global, &
                  cori_global, landmask_global

!     print '(/"MAXVAL:: u, v, w, p, t, qv, ph, mu, psfc:",9e13.5)',  &
!      maxval(u_global),maxval(v_global),maxval(w_global ),maxval(p_global ), &
!      maxval(t_global),maxval(q_global),maxval(ph_global),maxval(mu_global), & 
!      maxval(psfc_global)
!     print '(/"MINVAL:: u, v, w, p, t, qv, ph, mu, psfc:",9e13.5)',  &
!      minval(u_global),minval(v_global),minval(w_global ),minval(p_global ), &
!      minval(t_global),minval(q_global),minval(ph_global),minval(mu_global), &
!      minval(psfc_global)
 
#else

!    .. 3d- and 2d-increments:
        write (141) xa%u(1:ix+1,1:jy+1,1:kz+1), &
                    xa%v(1:ix+1,1:jy+1,1:kz+1), &
                    xa%w(1:ix+1,1:jy+1,1:kz+1), &
                    xa%p(1:ix+1,1:jy+1,1:kz+1), &
                    xa%t(1:ix+1,1:jy+1,1:kz+1), &
                    q_cgrid(1:ix+1,1:jy+1,1:kz+1), &
                    ph_cgrid(1:ix+1,1:jy+1,1:kz+1), &
                    mu_cgrid(1:ix+1,1:jy+1), &
                    xa%psfc(1:ix+1,1:jy+1)

!    .. 2d-constant fields:
        write (141) xb%psac(1:ix+1,1:jy+1), &
                    xb%tgrn(1:ix+1,1:jy+1), &
                    xb%terr(1:ix+1,1:jy+1), &
                    xb%snow(1:ix+1,1:jy+1), &
                    xb%lat(1:ix+1,1:jy+1), &
                    xb%lon(1:ix+1,1:jy+1), &
                    xb%lanu(1:ix+1,1:jy+1), &
                    xb%map_factor(1:ix+1,1:jy+1), &
                    xb%cori(1:ix+1,1:jy+1), &
                    xb%landmask(1:ix+1,1:jy+1)

!      print '(/"MAXVAL:: u, v, w, p, t, qv, ph, mu, psfc:",9e13.5)',  &
!         maxval(xa%u),maxval(xa%v),   maxval(xa%w ),   maxval(xa%p ), &
!         maxval(xa%t),maxval(q_cgrid),maxval(ph_cgrid),maxval(mu_cgrid),&
!         maxval(xa%psfc)
!      print '(/"MINVAL:: u, v, w, p, t, qv, ph, mu, psfc:",9e13.5)',  &
!         minval(xa%u),minval(xa%v),   minval(xa%w ),   minval(xa%p ), &
!         minval(xa%t),minval(q_cgrid),minval(ph_cgrid),minval(mu_cgrid),&
!         minval(xa%psfc)

#endif

      endif

END SUBROUTINE DA_Write_Increment 
