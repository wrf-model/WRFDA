SUBROUTINE copy_dims(grid, xp, &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe )
!------------------------------------------------------------------------------
!  PURPOSE: Copy dimensioning information from grid structure.
!
!------------------------------------------------------------------------------

   TYPE(domain), INTENT(IN)         :: grid
   TYPE (xpose_type),INTENT(INOUT)  :: xp      ! Transpose variables.

   INTEGER,      INTENT(OUT)        :: ids,ide, jds,jde, kds,kde   ! domain dims.
   INTEGER,      INTENT(OUT)        :: ims,ime, jms,jme, kms,kme   ! memory dims.
   INTEGER,      INTENT(OUT)        :: ips,ipe, jps,jpe, kps,kpe   ! patch  dims.

!--De-reference dimension information stored in the grid data structure.

   ids = grid%sd31 
   ide = grid%ed31 - 1
   jds = grid%sd32 
   jde = grid%ed32 - 1
   kds = grid%sd33 
   kde = grid%ed33 - 1

   ims = grid%sm31 
   ime = grid%em31 
   jms = grid%sm32 
   jme = grid%em32 
   kms = grid%sm33 
   kme = grid%em33 

   ips = grid%sp31 
   ipe = grid%ep31 
   jps = grid%sp32 
   jpe = grid%ep32 
   kps = grid%sp33 
   kpe = grid%ep33 

!--Indices for yz decomposition

   xp%idsx = grid%sd31
   xp%idex = grid%ed31 - 1
   xp%jdsx = grid%sd32
   xp%jdex = grid%ed32 - 1
   xp%kdsx = grid%sd33
   xp%kdex = grid%ed33 - 1

   xp%imsx = grid%sm31x
   xp%imex = grid%em31x
   xp%jmsx = grid%sm32x
   xp%jmex = grid%em32x
   xp%kmsx = grid%sm33x
   xp%kmex = grid%em33x

   xp%itsx = grid%sp31x
   xp%itex = grid%ep31x
   xp%jtsx = grid%sp32x
   xp%jtex = grid%ep32x
   xp%ktsx = grid%sp33x
   xp%ktex = grid%ep33x

   xp%ipsx = grid%sp31x
   xp%ipex = grid%ep31x
   xp%jpsx = grid%sp32x
   xp%jpex = grid%ep32x
   xp%kpsx = grid%sp33x
   xp%kpex = grid%ep33x

!--Indices for xz decomposition

   xp%idsy = grid%sd31
   xp%idey = grid%ed31 - 1
   xp%jdsy = grid%sd32
   xp%jdey = grid%ed32 - 1
   xp%kdsy = grid%sd33
   xp%kdey = grid%ed33 - 1

   xp%imsy = grid%sm31y
   xp%imey = grid%em31y
   xp%jmsy = grid%sm32y
   xp%jmey = grid%em32y
   xp%kmsy = grid%sm33y
   xp%kmey = grid%em33y

   xp%itsy = grid%sp31y
   xp%itey = grid%ep31y
   xp%jtsy = grid%sp32y
   xp%jtey = grid%ep32y
   xp%ktsy = grid%sp33y
   xp%ktey = grid%ep33y

   xp%ipsy = grid%sp31y
   xp%ipey = grid%ep31y
   xp%jpsy = grid%sp32y
   xp%jpey = grid%ep32y
   xp%kpsy = grid%sp33y
   xp%kpey = grid%ep33y

   if(ipe > ide) ipe = ide
   if(jpe > jde) jpe = jde
   if(kpe > kde) kpe = kde

   ! Indices for yz decomposition

   if(xp%itex > ide) xp%itex = ide
   if(xp%jtex > jde) xp%jtex = jde
   if(xp%ktex > kde) xp%ktex = kde

   if(xp%ipex > ide) xp%ipex = ide
   if(xp%jpex > jde) xp%jpex = jde
   if(xp%kpex > kde) xp%kpex = kde

   ! Indices for xz decomposition

   if(xp%itey > ide) xp%itey = ide
   if(xp%jtey > jde) xp%jtey = jde
   if(xp%ktey > kde) xp%ktey = kde

   if(xp%ipey > ide) xp%ipey = ide
   if(xp%jpey > jde) xp%jpey = jde
   if(xp%kpey > kde) xp%kpey = kde

!  Copy xpose dimensions from grid structure to xp structure.

!--Indices for xy decomposition

   xp%ids = ids
   xp%ide = ide
   xp%jds = jds
   xp%jde = jde
   xp%kds = kds
   xp%kde = kde

   xp%ims = ims
   xp%ime = ime
   xp%jms = jms
   xp%jme = jme
   xp%kms = kms
   xp%kme = kme

   xp%ips = ips
   xp%ipe = ipe
   xp%jps = jps
   xp%jpe = jpe
   xp%kps = kps
   xp%kpe = kpe

   if(print_detail > 0) then
      write(unit=*, fmt='(2(a, i4, 5x))') &
           'xp%ids =', xp%ids , 'xp%ide =', xp%ide , &
           'xp%jds =', xp%jds , 'xp%jde =', xp%jde , &
           'xp%kds =', xp%kds , 'xp%kde =', xp%kde
      write(unit=*, fmt='(//)')

      write(unit=*, fmt='(2(a, i4, 5x))') &
           'xp%ims =', xp%ims , 'xp%ime =', xp%ime , &
           'xp%jms =', xp%jms , 'xp%jme =', xp%jme , &
           'xp%kms =', xp%kms , 'xp%kme =', xp%kme
      write(unit=*, fmt='(//)')

      write(unit=*, fmt='(2(a, i4, 5x))') &
           'xp%ips =', xp%ips , 'xp%ipe =', xp%ipe , &
           'xp%jps =', xp%jps , 'xp%jpe =', xp%jpe , &
           'xp%kps =', xp%kps , 'xp%kpe =', xp%kpe
      write(unit=*, fmt='(//)')

      write(unit=*, fmt='(2(a, i4, 5x))') &
           'xp%imsx =', xp%imsx, 'xp%imex=', xp%imex, &
           'xp%jmsx =', xp%jmsx, 'xp%jmex=', xp%jmex, &
           'xp%kmsx =', xp%kmsx, 'xp%kmex=', xp%kmex
      write(unit=*, fmt='(//)')

      write(unit=*, fmt='(2(a, i4, 5x))') &
           'xp%ipsx =', xp%ipsx, 'xp%ipex=', xp%ipex, &
           'xp%jpsx =', xp%jpsx, 'xp%jpex=', xp%jpex, &
           'xp%kpsx =', xp%kpsx, 'xp%kpex=', xp%kpex
      write(unit=*, fmt='(//)')

      write(unit=*, fmt='(2(a, i4, 5x))') &
           'xp%imsy =', xp%imsy, 'xp%imey=', xp%imey, &
           'xp%jmsy =', xp%jmsy, 'xp%jmey=', xp%jmey, &
           'xp%kmsy =', xp%kmsy, 'xp%kmey=', xp%kmey
      write(unit=*, fmt='(//)')

      write(unit=*, fmt='(2(a, i4, 5x))') &
           'xp%ipsy =', xp%ipsy, 'xp%ipey=', xp%ipey, &
           'xp%jpsy =', xp%jpsy, 'xp%jpey=', xp%jpey, &
           'xp%kpsy =', xp%kpsy, 'xp%kpey=', xp%kpey
   endif

END SUBROUTINE copy_dims

