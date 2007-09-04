
CALL write_field_global3d ( g_a, &
                          'g_a', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_advect_tend, &
                          'g_advect_tend', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_alpha, &
                          'g_alpha', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
!CALL write_field_global3d ( g_bn2h, &
!                          'g_bn2h', filecount, min_filecount, max_filecount, 'm', &
!                          ids, ide, jds, jde, kds, kde, &
!                          ims, ime, jms, jme, kms, kme, &
!                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_c2a, &
                          'g_c2a', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_cqu, &
                          'g_cqu', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_cqv, &
                          'g_cqv', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_cqw, &
                          'g_cqw', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_gamma, &
                          'g_gamma', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_al, &
                          'g_al', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_alt, &
                          'g_alt', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_moist_1(ims,kms,jms,2), &
                          'g_moist_1', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_moist_2(ims,kms,jms,2), &
                          'g_moist_2', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_moist_tend(ims,kms,jms,2), &
                          'g_moist_tend', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_mu_1, &
                          'g_mu_1', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_mu_2, &
                          'g_mu_2', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_mu_save, &
                          'g_mu_save', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_mub, &
                          'g_mub', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_mu_tend, &
                          'g_mu_tend', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_muave, &
                          'g_muave', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_mut, &
                          'g_mut', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_muts, &
                          'g_muts', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_muu, &
                          'g_muu', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_muus, &
                          'g_muus', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_muv, &
                          'g_muv', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( g_muvs, &
                          'g_muvs', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_p, &
                          'g_p', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_p8w, &
                          'g_p8w', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_p_phy, &
                          'g_p_phy', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_ph_1, &
                          'g_ph_1', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_ph_2, &
                          'g_ph_2', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_ph_save, &
                          'g_ph_save', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_phb, &
                          'g_phb', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_ph_tend, &
                          'g_ph_tend', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_ph_tendf, &
                          'g_ph_tendf', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
!CALL write_field_global3d ( g_pi_phyh, &
!                          'g_pi_phyh', filecount, min_filecount, max_filecount, 'm', &
!                          ids, ide, jds, jde, kds, kde, &
!                          ims, ime, jms, jme, kms, kme, &
!                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_pm1, &
                          'g_pm1', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_ru_m, &
                          'g_ru_m', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_ru_tend, &
                          'g_ru_tend', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_ru_tendf, &
                          'g_ru_tendf', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_rv_m, &
                          'g_rv_m', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_rv_tend, &
                          'g_rv_tend', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_rv_tendf, &
                          'g_rv_tendf', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_rw_tend, &
                          'g_rw_tend', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_rw_tendf, &
                          'g_rw_tendf', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_t8w, &
                          'g_t8w', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_t_2save, &
                          'g_t_2save', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_t_phy, &
                          'g_t_phy', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_t_1, &
                          'g_t_1', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_t_2, &
                          'g_t_2', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_t_save, &
                          'g_t_save', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_t_tend, &
                          'g_t_tend', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_t_tendf, &
                          'g_t_tendf', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_th_phy, &
                          'g_th_phy', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_u_1, &
                          'g_u_1', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_u_2, &
                          'g_u_2', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_u_save, &
                          'g_u_save', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_v_1, &
                          'g_v_1', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_v_2, &
                          'g_v_2', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_v_save, &
                          'g_v_save', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_w_1, &
                          'g_w_1', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_w_2, &
                          'g_w_2', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_w_save, &
                          'g_w_save', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_ww1, &
                          'g_ww1', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_ww_m, &
                          'g_ww_m', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
!CALL write_field_global3d ( g_z_at_wh, &
!                          'g_z_at_wh', filecount, min_filecount, max_filecount, 'w', &
!                          ids, ide, jds, jde, kds, kde, &
!                          ims, ime, jms, jme, kms, kme, &
!                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( z, &
                          'z', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_z, &
                          'g_z', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( z_at_w, &
                          'z_at_w', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( g_z_at_w, &
                          'g_z_at_w', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )

