load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin

 xaf    = addfile("xaf.nc","r")    ; Open input file "Xaf"
 xbf    = addfile("xbf.nc","r")    ; Open input file "Xbf"
 tl     = addfile("tl.nc","r")     ; Open input file "tl"
; anainc = addfile("anainc.nc","r") ; Open input file "anainc"

 u_tl = tl->G_U
 v_tl = tl->G_V
 t_tl = tl->G_T
 p_tl = tl->G_P

 u_tl2 = tl->G_U
 v_tl2 = tl->G_V
 t_tl2 = tl->G_T
 p_tl2 = tl->G_P

 u_tl3 = tl->G_U
 v_tl3 = tl->G_V
 t_tl3 = tl->G_T
 p_tl3 = tl->G_P

 u_xaf = xaf->U
 v_xaf = xaf->V
 t_xaf = xaf->T
 p_xaf = xaf->P

 u_xbf = xbf->U
 v_xbf = xbf->V
 t_xbf = xbf->T
 p_xbf = xbf->P

 u_tl3=u_xaf-u_xbf
 v_tl3=v_xaf-v_xbf
 t_tl3=t_xaf-t_xbf
 p_tl3=p_xaf-p_xbf

; u_anainc = anainc->U
; v_anainc = anainc->V
; t_anainc = anainc->T
; p_anainc = anainc->P

 ist = istart
 jst = jstart
 ied = iend
 jed = jend
 kst = kstart
 ked = kend

 u_tl2 = 0.0 
 v_tl2 = 0.0 
 t_tl2 = 0.0 
 p_tl2 = 0.0 
    do k = kst-1, ked-1
       do j = jst-1, jed-1
          do i = ist-1, ied-1
            u_tl2(0,k,j,i)=1.0
            v_tl2(0,k,j,i)=1.0
            t_tl2(0,k,j,i)=1.0
            p_tl2(0,k,j,i)=1.0
          end do
       end do
    end do
 u_tl3=u_tl3*u_tl2
 v_tl3=v_tl3*v_tl2
 t_tl3=t_tl3*t_tl2
 p_tl3=p_tl3*p_tl2

 u_tl=u_tl*u_tl2
 v_tl=v_tl*v_tl2
 t_tl=t_tl*t_tl2
 p_tl=p_tl*p_tl2

; delta_e_nl
 delta_e_nl =  sum(u_tl3^2) + sum(v_tl3^2) + (9.8/3)^2 * sum(t_tl3^2) + (1./300.)^2 * sum(p_tl3^2)

; delta_e_tl
 delta_e_tl =  sum(u_tl^2)  + sum(v_tl^2)  + (9.8/3)^2 * sum(t_tl^2)  + (1./300.)^2 * sum(p_tl^2) 

 print ("---> Non-Linear Forecast Difference = "+delta_e_nl * 1E-5)
 print ("---> Linear estimate                = "+delta_e_tl * 1E-5)
 print ("---> Percentage of Error            = "+ 100.0*(delta_e_nl-delta_e_tl)/delta_e_nl)
 end
