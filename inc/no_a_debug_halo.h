
CALL write_field_global3d ( ru_tend, &
                          'ru_tend', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( rv_tend, &
                          'rv_tend', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( rw_tend, &
                          'rw_tend', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )

CALL write_field_global3d ( ru_tendf, &
                          'ru_tendf', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( rv_tendf, &
                          'rv_tendf', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( rw_tendf, &
                          'rw_tendf', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( ph_tendf, &
                          'ph_tendf', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( t_tendf, &
                          't_tendf', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( mu_tend, &
                          'mu_tend', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( u_save, &
                          'u_save', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( v_save, &
                          'v_save', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( w_save, &
                          'w_save', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( ph_save, &
                          'ph_save', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( t_save, &
                          't_save', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( mu_save, &
                          'mu_save', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( rthften, &
                          'rthften', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( ru, &
                          'ru', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( rv, &
                          'rv', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( rw, &
                          'rw', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( ww, &
                          'ww', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( h_diabatic, &
                          'h_diabatic', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )


CALL write_field_global3d ( p, &
                          'p', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( pb, &
                          'pb', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( php, &
                          'php', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( phb, &
                          'phb', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( alt, &
                          'alt', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( al, &
                          'al', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( mu_2, &
                          'mu_2', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( mut, &
                          'mut', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( muu, &
                          'muu', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( muv, &
                          'muv', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( mub, &
                          'mub', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( cqu, &
                          'cqu', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( muv, &
                          'muv', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( cqv, &
                          'cqv', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( cqw, &
                          'cqw', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( mudf, &
                          'mudf', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( t_init, &
                          't_init', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( xkmhd, &
                          'xkmhd', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )

CALL write_field_global2d ( msft, &
                          'msft', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( msfu, &
                          'msfu', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( msfv, &
                          'msfv', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( f, &
                          'f', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( e, &
                          'e', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( sina, &
                          'sina', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global2d ( cosa, &
                          'cosa', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )

CALL write_field_global3d ( t_1, &
                          't_1', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( t_2, &
                          't_2', filecount, min_filecount, max_filecount, 'm', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( u_1, &
                          'u_1', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( u_2, &
                          'u_2', filecount, min_filecount, max_filecount, 'u', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( v_1, &
                          'v_1', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( v_2, &
                          'v_2', filecount, min_filecount, max_filecount, 'v', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( w_1, &
                          'w_1', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( w_2, &
                          'w_2', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )

CALL write_field_global3d ( ph_1, &
                          'ph_1', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )
CALL write_field_global3d ( ph_2, &
                          'ph_2', filecount, min_filecount, max_filecount, 'w', &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe )

