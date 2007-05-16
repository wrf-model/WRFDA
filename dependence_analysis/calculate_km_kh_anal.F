    SUBROUTINE calculate_km_kh( config_flags, dt,                        &
                                dampcoef, zdamp, damp_opt,               &
                                xkmh, xkmhd, xkmv, xkhh, xkhv,           &
                                BN2, khdif, kvdif, div,                  &
                                defor11, defor22, defor33,               &
                                defor12, defor13, defor23,               &
                                tke, p8w, t8w, theta, t, p, moist,       &
                                dn, dnw, dx, dy, rdz, rdzw, cr_len,      &
                                n_moist, cf1, cf2, cf3, warm_rain,       &
                                kh_tke_upper_bound, kv_tke_upper_bound,  &
                                ids, ide, jds, jde, kds, kde,            &
                                ims, ime, jms, jme, kms, kme,            &
                                its, ite, jts, jte, kts, kte             )

! History:     Sep 2003  Changes by George Bryan and Jason Knievel, NCAR
!              Oct 2001  Converted to mass core by Bill Skamarock, NCAR
!              ...       ...

! Purpose:     This routine calculates exchange coefficients for the TKE
!              scheme.

! References:  Klemp and Wilhelmson (JAS 1978)
!              Deardorff (B-L Meteor 1980)
!              Chen and Dudhia (NCAR WRF physics report 2000)

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags   

    INTEGER, INTENT( IN )  &
    :: n_moist, damp_opt,             & 
       ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte 

    LOGICAL, INTENT( IN )  &
    :: warm_rain

    REAL, INTENT( IN )  &
    :: cr_len, dx, dy, zdamp, dt, dampcoef, cf1, cf2, cf3, khdif, kvdif

    REAL, DIMENSION( kms:kme ), INTENT( IN )  &
    :: dnw, dn

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ), INTENT( INOUT )  &
    :: moist

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: xkmv, xkmh, xkmhd, xkhv, xkhh, BN2  

    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),  INTENT( IN )  &
    :: defor11, defor22, defor33, defor12, defor13, defor23,      &
       div, rdz, rdzw, p8w, t8w, theta, t, p

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: tke

    REAL, INTENT( IN )  &
    :: kh_tke_upper_bound, kv_tke_upper_bound

! Local variables.

    INTEGER  &
    :: i_start, i_end, j_start, j_end, ktf, i, j, k

! End declarations.
!-----------------------------------------------------------------------

    ktf     = MIN( kte, kde-1 )
    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

! TBH:  OVERWRITE BN2
! TBH:  HORIZONTAL DEPENDENCE:  NONE
    CALL calculate_N2( config_flags, BN2, moist,           &
                       theta, t, p, p8w, t8w,              &
                       dnw, dn, rdz, rdzw,                 &
                       n_moist, cf1, cf2, cf3, warm_rain,  &
                       ids, ide, jds, jde, kds, kde,       &
                       ims, ime, jms, jme, kms, kme,       &
                       its, ite, jts, jte, kts, kte        )

! Select a scheme for calculating diffusion coefficients.

    km_coef: SELECT CASE( config_flags%km_opt )

      CASE (1)
! TBH:  OVERWRITE xkmh,xkmhd,xkmv,xkhh,xkhv
! TBH:  HORIZONTAL DEPENDENCE:  NONE
            CALL isotropic_km( config_flags, xkmh, xkmhd, xkmv,         &
                               xkhh, xkhv, khdif, kvdif,                &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               its, ite, jts, jte, kts, kte             )
      CASE (2)  
! TBH:  OVERWRITE xkmh,xkmhd,xkmv,xkhh,xkhv
! TBH:  HORIZONTAL DEPENDENCE:  NONE
            CALL tke_km(       config_flags, xkmh, xkmhd, xkmv,         &
                               xkhh, xkhv, BN2, tke, p8w, t8w, theta,   &
                               rdz, rdzw, dx, dy, cr_len,               &
                               kh_tke_upper_bound, kv_tke_upper_bound,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               its, ite, jts, jte, kts, kte             )
      CASE (3)  
! TBH:  OVERWRITE xkmh,xkmhd,xkmv,xkhh,xkhv
! TBH:  HORIZONTAL DEPENDENCE:  defor12  ( [    i,i+1], [    j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  defor13  ( [    i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  defor23  ( [    i    ], [    j,j+1] )
            CALL smag_km(      config_flags, xkmh, xkmhd, xkmv,         &
                               xkhh, xkhv, BN2, div,                    &
                               defor11, defor22, defor33,               &
                               defor12, defor13, defor23,               &
                               rdzw, dx, dy, cr_len,                    &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               its, ite, jts, jte, kts, kte             )
      CASE (4)  
! TBH:  OVERWRITE xkmh,xkmhd,xkmv,xkhh,xkhv
! TBH:  HORIZONTAL DEPENDENCE:  NONE
            CALL smag2d_km(    config_flags, xkmh, xkmhd, xkmv,         &
                               xkhh, xkhv, defor11, defor22, defor12,   &
                               rdzw, dx, dy,                            &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               its, ite, jts, jte, kts, kte             )
      CASE DEFAULT
            CALL wrf_error_fatal( 'Please choose diffusion coefficient scheme' )

    END SELECT km_coef

    IF ( damp_opt .eq. 1 ) THEN
! TBH:  OVERWRITE xkmh,xkmhd,xkmv,xkhh,xkhv
! TBH:  HORIZONTAL DEPENDENCE:  NONE
      CALL cal_dampkm( config_flags, xkmhd, xkmh, xkhh, xkmv, xkhv,   &
                       dx, dy, dt, dampcoef, rdz, rdzw, zdamp,  &
                       ids, ide, jds, jde, kds, kde,            &
                       ims, ime, jms, jme, kms, kme,            &
                       its, ite, jts, jte, kts, kte             )
    END IF

    END SUBROUTINE calculate_km_kh
