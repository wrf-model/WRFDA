
CALL write_field_global3d ( u_1, &
                          'u_1', filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )

CALL write_field_global3d ( u_2, &
                          'u_2', filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )

CALL write_field_global3d ( a_a, &
                          'a_a', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_advect_tend, &
                          'a_advect_tend', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_alpha, &
                          'a_alpha', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_bn2h, &
                          'a_bn2h', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_c2a, &
                          'a_c2a', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_cqu, &
                          'a_cqu', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_cqv, &
                          'a_cqv', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_cqw, &
                          'a_cqw', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_gamma, &
                          'a_gamma', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_al, &
                          'a_al', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_alt, &
                          'a_alt', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_moist_1(ims,kms,jms,2), &
                          'a_moist_1', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_moist_2(ims,kms,jms,2), &
                          'a_moist_2', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_moist_tend(ims,kms,jms,2), &
                          'a_moist_tend', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_mu_1, &
                          'a_mu_1', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_mu_2, &
                          'a_mu_2', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_mu_save, &
                          'a_mu_save', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_mub, &
                          'a_mub', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_mu_tend, &
                          'a_mu_tend', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_muave, &
                          'a_muave', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_mut, &
                          'a_mut', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_muts, &
                          'a_muts', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_muu, &
                          'a_muu', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_muus, &
                          'a_muus', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_muv, &
                          'a_muv', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( a_muvs, &
                          'a_muvs', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_p, &
                          'a_p', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_p8w, &
                          'a_p8w', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_p_phy, &
                          'a_p_phy', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_ph_1, &
                          'a_ph_1', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_ph_2, &
                          'a_ph_2', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_ph_save, &
                          'a_ph_save', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_phb, &
                          'a_phb', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_ph_tend, &
                          'a_ph_tend', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_ph_tendf, &
                          'a_ph_tendf', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_pi_phyh, &
                          'a_pi_phyh', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_pm1, &
                          'a_pm1', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_ru_m, &
                          'a_ru_m', filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_ru_tend, &
                          'a_ru_tend', filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_ru_tendf, &
                          'a_ru_tendf', filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_rv_m, &
                          'a_rv_m', filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_rv_tend, &
                          'a_rv_tend', filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_rv_tendf, &
                          'a_rv_tendf', filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_rw_tend, &
                          'a_rw_tend', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_rw_tendf, &
                          'a_rw_tendf', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_t8w, &
                          'a_t8w', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_t_2save, &
                          'a_t_2save', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_t_phy, &
                          'a_t_phy', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_t_1, &
                          'a_t_1', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_t_2, &
                          'a_t_2', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_t_save, &
                          'a_t_save', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_t_tend, &
                          'a_t_tend', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_t_tendf, &
                          'a_t_tendf', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_th_phy, &
                          'a_th_phy', filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_u_1, &
                          'a_u_1', filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_u_2, &
                          'a_u_2', filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_u_save, &
                          'a_u_save', filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_v_1, &
                          'a_v_1', filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_v_2, &
                          'a_v_2', filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_v_save, &
                          'a_v_save', filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_w_1, &
                          'a_w_1', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_w_2, &
                          'a_w_2', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_w_save, &
                          'a_w_save', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_ww1, &
                          'a_ww1', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_ww_m, &
                          'a_ww_m', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( a_z_at_wh, &
                          'a_z_at_wh', filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )

