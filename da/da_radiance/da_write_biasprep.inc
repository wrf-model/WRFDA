subroutine da_write_biasprep(radbias)

   type(bias), intent(in)  :: radbias
   write(unit=biasprep_unit) radbias%nchan,radbias%npred
   write(unit=biasprep_unit) radbias%platform_id , &
                             radbias%satellite_id, &
                             radbias%sensor_id,    &
                             radbias%year,radbias%month,&
                             radbias%day, radbias%hour, &
                             radbias%min, radbias%sec,  &
                             radbias%scanline, &
                             radbias%scanpos,  &
                             radbias%landmask, &
                             radbias%elevation,&
                             radbias%lat,radbias%lon, &
                             radbias%ps,radbias%t2m, &
                             radbias%q2m,radbias%tsk, &
                             radbias%tb(1:radbias%nchan), &
                             radbias%omb(1:radbias%nchan), &
                             radbias%bias(1:radbias%nchan), &
                             radbias%pred(1:radbias%npred), &
                             radbias%qc_flag(1:radbias%nchan), &
                             radbias%cloud_flag(1:radbias%nchan), &
                             radbias%surf_flag, radbias%clwp

end subroutine da_write_biasprep


