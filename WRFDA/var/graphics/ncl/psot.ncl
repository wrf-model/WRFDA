;#########################################################################
; Script: psot.ncl
;
; Purpose: Ploting (XY, XZ and YZ cross-sections) script for single obs test
;  (Works both for GSI & WRFDA)
; Author : Syed RH Rizvi, MMM/ESSL/NCAR,  Date:04/15/2009
;
;#########################################################################
; Description:
;
;  It is automativcally driven by var/scripts/da_plot_psot_gsi.ksh
;  To run it independently, it needs following environment variable
;  works  -- Work
;  expt   -- Typical character string to name the plot
;  kl     -- Vertical sigma level
;  xlon   -- Co-ordinate in WE-direction
;  xlat   -- Co-ordinale in SN direction
;  bakfile - Full path of First Guess file
;  analfile- Full path of Analysis file
;  GRAPHICS_DIR -- Full path GRAPHICS_DIR path 
;#########################################################################
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "$GRAPHICS_DIR/WRF_contributed.ncl.test"
begin
;*************************************************************************
; open file 
;*************************************************************************
  first_guess     = addfile(bakfile+".nc", "r")
  analysis        = addfile(analfile+".nc", "r")
  ixlat=floattointeger(xlat)
  ixlon=floattointeger(xlon)

  znu = first_guess->ZNU(0,:)
;*************************************************************************
;
   t0 =300.0
   base_pres=100000.00
   gas_constant = 287.0
   cp = 7.0*gas_constant/2.0
   kappa = gas_constant / cp

;*************************************************************************
  xy_ug = first_guess->U(0,kl-1,:,:)
  xy_ua = analysis->U(0,kl-1,:,:)
  xydf_u = xy_ua
  xydf_u = xy_ua - xy_ug

  xy_vg = first_guess->V(0,kl-1,:,:)
  xy_va = analysis->V(0,kl-1,:,:)
  xydf_v = xy_va
  xydf_v = xy_va - xy_vg

   dimU  = dimsizes(xy_ug)
   latU = dimU(0)
   lonU = dimU(1)
   dimV  = dimsizes(xy_vg)
   latV = dimV(0)
   lonV = dimV(1)

;--------------------------------------------------------------------
;  Currently pressure increments are not updated in GSI
;  So we cannot plot Temperature increments using analysis file
;
   PB = first_guess->PB(0,kl-1,:,:)
   PPg = first_guess->P(0,kl-1,:,:)
   PPg = PPg + PB
   PPa = analysis->P(0,kl-1,:,:)
   PPa = PPa + PB

  ThetaG = first_guess->T(0,kl-1,:,:)
  ThetaA = analysis->T(0,kl-1,:,:)

;  xydf_t = ThetaA - ThetaG

  ThetaG = ThetaG + t0
  ThetaA = ThetaA + t0

  xydf_t = ThetaG
  xydf_t = exp( log(ThetaA) + kappa * log(PPa/base_pres) ) - \
           exp( log(ThetaG) + kappa * log(PPg/base_pres) )
;

;--------------------------------------------------------------------

  xy_qg = first_guess->QVAPOR(0,kl-1,:,:)
  xy_qa = analysis->QVAPOR(0,kl-1,:,:)
  xydf_q = xy_qa
  xydf_q = (xy_qa - xy_qg) * 1000.

   xydf_u@units = "mps"
   xydf_v@units = "mps"
   xydf_t@units = "Kelvin"
   xydf_q@units = "g/Kg"

   xy_u = xy_qa
   xy_v = xy_qa
   xy_u = 0.5*(xydf_u(:,0:lonU-2)+xydf_u(:,1:lonU-1))
   xy_v = 0.5*(xydf_v(0:latV-2,:)+xydf_v(1:latV-1,:))
  print (" For XY-plot max/min U  = "+max(xydf_u)+" "+min(xydf_u))
  print (" For XY plot max/min V  = "+max(xydf_v)+" "+min(xydf_v))
  print (" For XY plot max/min T  = "+max(xydf_t)+" "+min(xydf_t))
  print (" For XY plot max/min Q  = "+max(xydf_q)+" "+min(xydf_q))

;#########################################################################
; Now create XY cross-sesction                               
;#########################################################################

  plts                      = new (4,"graphic")

  wks = gsn_open_wks(works,expt)          ; ps,pdf,x11,ncgm,eps
  gsn_define_colormap(wks,"gui_default")           ; select color map

; resource for U and V (vector and scalar overlaid)
  res                       = True            ; plot mods desired
  res@gsnMaximize           = True            ; uncomment to maximize size

  res@cnFillOn              = True            ; turn on color
  res@cnLineLabelsOn        = False           ; turn off contour line labels
  res@gsnSpreadColors       = True            ; use entire color map
;  res@gsnScalarContour      = True            ; contours desired

  WRF_map_c(first_guess,res,0)                ; set map resources

  res@gsnDraw               = False           ; (a) do not draw
  res@gsnFrame              = False           ; (b) do not advance 'frame'

  res@mpGridAndLimbOn       = True            ; trun on lat/lon lines
  res@pmTickMarkDisplayMode = "Always"        ; turn on tickmarks
  res@tmXTOn = False                          ; turn off top   labels 
  res@tmYROn = False                          ; turn off right labels
  res@lbLabelStride = 2                       ; Color bar value writing


;  res@vcGlyphStyle       = "CurlyVector"      ; curly vectors
;  res@vcRefMagnitudeF    = 0.2                ; define vector ref mag
;  res@vcRefLengthF       = 0.045              ; define length of vec ref
;  res@gsnRightString     = " "                ; turn off right string
;  res@gsnLeftString      = " "                ; turn off left string
;  res@tiXAxisString      = " "                ; turn off axis label 
;  res@vcRefAnnoOrthogonalPosF = -.535         ; move ref vector into plot
;  res@vcMinDistanceF        = 0.02            ; larger means sparser
;
; resource for T and QV (contour)
;
  res1                       = True           ; plot mods desired
  res1@gsnMaximize           = True           ; uncomment to maximize size

  res1@cnFillOn              = True           ; turn on color
  res1@cnLineLabelsOn        = False          ; turn off contour line labels
  res1@gsnSpreadColors       = True           ; use entire color map

  WRF_map_c(first_guess,res1,0)                ; set map resources

  res1@gsnDraw               = False          ; (a) do not draw
  res1@gsnFrame              = False          ; (b) do not advance 'frame'

  res1@mpGridAndLimbOn       = True           ; trun on lat/lon lines
  res1@pmTickMarkDisplayMode = "Always"       ; turn on tickmarks
  res1@tmXTOn = False                         ; turn off top   labels
  res1@tmYROn = False                         ; turn off right labels
  res1@lbLabelStride = 2                      ; Color bar value writing


  res@gsnLeftString         = "U("+kl+")"
  res@gsnRightString        = xydf_u@units
  plts(0)                   = gsn_csm_contour_map(wks,xydf_u,res) 

  res@gsnLeftString         = "V("+kl+")"
  res@gsnRightString        = xydf_v@units
  plts(1)                   = gsn_csm_contour_map(wks,xydf_v,res) 

  res1@gsnLeftString         = "T("+kl+")"
  res1@gsnRightString        = xydf_t@units
  plts(2)                   = gsn_csm_contour_map(wks,xydf_t,res1)

  res1@gsnLeftString         = "Q("+kl+")"
  res1@gsnRightString        = xydf_q@units
  plts(3)                   = gsn_csm_contour_map(wks,xydf_q,res1)


;#########################################################################
; create panel: panel plots have their own set of resources
;#########################################################################

  resP1                  = True                   ; modify the panel plot
  resP1@gsnFrame         = False                  ; don't advance panel plot
  resP1@gsnMaximize      = True                   ; maximize panel area
  resP1@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
  gsn_panel(wks,plts,(/2,2/),resP1)               ; now draw as one plot
  frame(wks)

   delete ( plts )
   delete ( res  )
   delete ( res1  )
   delete ( resP1 )
   delete( PB )
   delete( PPg )
   delete( PPa )
   delete( ThetaG )
   delete( ThetaA )

;*************************************************************************
; Now create XZ cross-sesction                               
;*************************************************************************
;
  xlong    = first_guess->XLONG(0,ixlat-1,:)
  xlong@units = "degrees_east"
  xlong_u    = first_guess->XLONG_U(0,ixlat-1,:)
  xlong_u@units = "degrees_east"
  xlong_v    = first_guess->XLONG_V(0,ixlat-1,:)
  xlong_v@units = "degrees_east"

  xz_ug = first_guess->U(0,:,ixlat-1,:)              ; import data + meta info
  xz_ua = analysis->U(0,:,ixlat-1,:)             ; import data + meta info

  xzdf_u = xz_ua - xz_ug

  xzdf_u!0 = "lev"
  xzdf_u&lev = znu
  xzdf_u!1 = "lon"
  xzdf_u&lon = xlong_u

  xz_vg1 = first_guess->V(0,:,ixlat-1,:)          ; import data + meta info
  xz_vg2 = first_guess->V(0,:,ixlat,:)          ; import data + meta info
  xz_vg = 0.5*(xz_vg1 + xz_vg2)

  xz_va1 = analysis->V(0,:,ixlat-1,:)          ; import data + meta info
  xz_va2 = analysis->V(0,:,ixlat,:)          ; import data + meta info
  xz_va = 0.5*(xz_va1 + xz_va2)

  xzdf_v = xz_va - xz_vg
  xzdf_v!0 = "lev"
  xzdf_v&lev = znu
  xzdf_v!1 = "lon"
  xzdf_v&lon = xlong

;--------------------------------------------------------------------
;  Currently pressure increments are not updated in GSI
;  So we cannot plot Temperature increments using analysis file
;
   PB = first_guess->PB(0,:,ixlat-1,:)      ; Base State Pressure
   PPg = first_guess->P(0,:,ixlat-1,:)       ; Perturbation Pressure
   PPg = PPg + PB
   PPa = analysis->P(0,:,ixlat-1,:)       ; Perturbation Pressure
   PPa = PPa + PB
;
  ThetaG = first_guess->T(0,:,ixlat-1,:)           ; import data + meta info
  ThetaA = analysis->T(0,:,ixlat-1,:)             ; import data + meta info
;
  ThetaG = ThetaG + t0
  ThetaA = ThetaA + t0
;
  xzdf_t = ThetaG
;  xzdf_t = ThetaA - ThetaG
  xzdf_t = exp( log(ThetaA) + kappa * log(PPa/base_pres) ) - \
           exp( log(ThetaG) + kappa * log(PPg/base_pres) )
;
  xzdf_t!0 = "lev"
  xzdf_t&lev = znu
  xzdf_t!1 = "lon"
  xzdf_t&lon = xlong
;-----------------------------------------------------------

  xz_qg = first_guess->QVAPOR(0,:,ixlat-1,:)          ; import data + meta info
  xz_qa = analysis->QVAPOR(0,:,ixlat-1,:)             ; import data + meta info
  xzdf_q = xz_qg
  xzdf_q = (xz_qa - xz_qg) * 1000.0
  xzdf_q!0 = "lev"
  xzdf_q&lev = znu
  xzdf_q!1 = "lon"
  xzdf_q&lon = xlong

  print (" For XZ plot max/min U  = "+max(xzdf_u)+" "+min(xzdf_u))
  print ("For XZ plot max/min V  = "+max(xzdf_v)+" "+min(xzdf_v))
  print ("For XZ plot max/min T  = "+max(xzdf_t)+" "+min(xzdf_t))
  print ("For XZ plot max/min Q  = "+max(xzdf_q)+" "+min(xzdf_q))

   xzdf_u@units = "mps"
   xzdf_v@units = "mps"
   xzdf_t@units = "Kelvin"
   xzdf_q@units = "g/Kg"

;*************************************************************************
; create plots
;*************************************************************************

  plts                      = new (4,"graphic")

  gsn_define_colormap(wks,"gui_default")           ; select color map

  res                       = True            ; plot mods desired
  res@gsnMaximize           = True            ; uncomment to maximize size

  res@cnFillOn              = True            ; turn on color
  res@gsnSpreadColors       = True            ; use entire color map
  res@lbLabelAutoStride     = True            ; let NCL determine label spacing
  res@trYReverse            = True            ; reverse y axis
  res@tmXBTickSpacingF      = 15.              ; force labeling every 5 deg lon

  res@gsnDraw               = False            ; (a) do not draw
  res@gsnFrame              = False            ; (b) do not advance 'frame'


  res@gsnLeftString         = "U"
  res@gsnRightString        = xzdf_u@units
  plts(0)                   = gsn_csm_contour(wks,xzdf_u,res)

  res@gsnLeftString         = "V"
  res@gsnRightString        = xzdf_v@units
  plts(1)                   = gsn_csm_contour(wks,xzdf_v,res)

  res@gsnLeftString         = "T"
  res@gsnRightString        = xzdf_t@units
  plts(2)                   = gsn_csm_contour(wks,xzdf_t,res)

  res@gsnLeftString         = "Q"
  res@gsnRightString        = xzdf_q@units
  plts(3)                   = gsn_csm_contour(wks,xzdf_q,res)

;*************************************************************************
; create panel: panel plots have their own set of resources
;*************************************************************************
  resP1                  = True                   ; modify the panel plot
  resP1@gsnFrame         = False                  ; don't advance panel plot
  resP1@gsnMaximize      = True                   ; maximize panel area
  resP1@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
  gsn_panel(wks,plts,(/2,2/),resP1)               ; now draw as one plot

  frame(wks)
 
   delete ( plts )
   delete( PB )
   delete( PPg )
   delete( PPa )
   delete( ThetaG )
   delete( ThetaA )

;*************************************************************************
; Now create YZ cross-sesction                               
;*************************************************************************
  xlatt    = first_guess->XLAT(0,:,ixlon-1)
  xlatt@units = "degrees_north"
  xlatt_u    = first_guess->XLAT_U(0,:,ixlon-1)
  xlatt_u@units = "degrees_north"
  xlatt_v    = first_guess->XLAT_V(0,:,ixlon-1)
  xlatt_v@units = "degrees_north"

;*************************************************************************
; Read W at Time=6, south_north=0, convert to cm/sec
;*************************************************************************


  yz_ug1 = first_guess->U(0,:,:,ixlon)
  yz_ug2 = first_guess->U(0,:,:,ixlon-1)
  yz_ug1 = 0.5*(yz_ug1+yz_ug2)
  yz_ua1 = analysis->U(0,:,:,ixlon)
  yz_ua2 = analysis->U(0,:,:,ixlon-1)
  yz_ua1 = 0.5*(yz_ua1+yz_ua2)
  yzdf_u = yz_ua1 - yz_ug1

  yzdf_u!0 = "lev"
  yzdf_u&lev = znu
  yzdf_u!1 = "lat"
  yzdf_u&lat = xlatt_u

;  yz_va = analysis->V(0,:,:,ixlon-1)              ; import data + meta info
;  yz_vg = first_guess->V(0,:,:,ixlon-1)              ; import data + meta info
;  yzdf_v = yz_va - yz_vg

  yz_va1 = analysis->V(0,:,:,ixlon-1)              ; import data + meta info
  yz_va2 = analysis->V(0,:,:,ixlon-2)              ; import data + meta info
  yz_va1 = 0.5*(yz_va1+yz_va2)
  yz_vg1 = first_guess->V(0,:,:,ixlon-1)              ; import data + meta info
  yz_vg2 = first_guess->V(0,:,:,ixlon-2)              ; import data + meta info
  yz_vg1 = 0.5*(yz_vg1+yz_vg2)
  yzdf_v = yz_va1 - yz_vg1

  yzdf_v!0 = "lev"
  yzdf_v&lev = znu
  yzdf_v!1 = "lat"
  yzdf_v&lat = xlatt_v

;--------------------------------------------------------------------
;  Currently pressure increments are not updated in GSI
;  So we cannot plot Temperature increments using analysis file
;
   PB = first_guess->PB(0,:,:,ixlon-1)      ; Base State Pressure
   PPg = first_guess->P(0,:,:,ixlon-1)       ; Perturbation Pressure
   PPg = PPg + PB
   PPa = analysis->P(0,:,:,ixlon-1)       ; Perturbation Pressure
   PPa = PPa + PB

  ThetaG = first_guess->T(0,:,:,ixlon-1)              ; import data + meta info
  ThetaA = analysis->T(0,:,:,ixlon-1)             ; import data + meta info

  ThetaG = ThetaG + t0
  ThetaA = ThetaA + t0
;
  yzdf_t = ThetaG
;  yzdf_t = ThetaA - ThetaG

  yzdf_t =  exp( log(ThetaA) + kappa * log(PPa/base_pres) ) - \
            exp( log(ThetaG) + kappa * log(PPg/base_pres) )
;
  yzdf_t!0 = "lev"
  yzdf_t&lev = znu
  yzdf_t!1 = "lat"
;
;--------------------------------------------------------------------
  yzdf_t&lat = xlatt

  yz_qg = first_guess->QVAPOR(0,:,:,ixlon-1)              ; import data + meta info
  yz_qa = analysis->QVAPOR(0,:,:,ixlon-1)             ; import data + meta info
  yzdf_q = yz_qg
  yzdf_q = (yz_qa - yz_qg ) * 1000.0
  yzdf_q!0 = "lev"
  yzdf_q&lev = znu
  yzdf_q!1 = "lat"
  yzdf_q&lat = xlatt

  print ("For YZ plot max/min U  = "+max(yzdf_u)+" "+min(yzdf_u))
  print ("For YZ plot max/min V  = "+max(yzdf_v)+" "+min(yzdf_v))
  print ("For YZ plot max/min T  = "+max(yzdf_t)+" "+min(yzdf_t) )
  print ("For YZ plot max/min Q  = "+max(yzdf_q)+" "+min(yzdf_q) )

   yzdf_u@units = "mps"
   yzdf_v@units = "mps"
   yzdf_t@units = "Kelvin"
   yzdf_q@units = "g/Kg"

;*************************************************************************
; create plots  
;*************************************************************************

  plts                      = new (4,"graphic")

  gsn_define_colormap(wks,"gui_default")           ; select color map

  res                       = True            ; plot mods desired
  res@gsnMaximize           = True            ; uncomment to maximize size

  res@cnFillOn              = True            ; turn on color
  res@gsnSpreadColors       = True            ; use entire color map
  res@lbLabelAutoStride     = True            ; let NCL determine label spacing
  res@trYReverse            = True            ; reverse y axis
  res@tmXBTickSpacingF      = 10.              ; force labeling every 5 deg lon

  res@gsnDraw               = False            ; (a) do not draw
  res@gsnFrame              = False            ; (b) do not advance 'frame'


  res@gsnLeftString         = "U"
  res@gsnRightString        = yzdf_u@units
  plts(0)                   = gsn_csm_contour(wks,yzdf_u,res)

  res@gsnLeftString         = "V"
  res@gsnRightString        = yzdf_v@units
  plts(1)                   = gsn_csm_contour(wks,yzdf_v,res)

  res@gsnLeftString         = "T"
  res@gsnRightString        = yzdf_t@units
  plts(2)                   = gsn_csm_contour(wks,yzdf_t,res)

  res@gsnLeftString         = "Q"
  res@gsnRightString        = yzdf_q@units
  plts(3)                   = gsn_csm_contour(wks,yzdf_q,res)

;*************************************************************************
; create panel: panel plots have their own set of resources
;*************************************************************************
  resP3                  = True                   ; modify the panel plot
  resP3@gsnFrame         = False                  ; don't advance panel plot
  resP3@gsnMaximize      = True                   ; maximize panel area
  resP3@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
  gsn_panel(wks,plts,(/2,2/),resP3)               ; now draw as one plot

  frame(wks)

   delete ( plts )
   delete( PB )
   delete( PPg )
   delete( PPa )
   delete( ThetaG )
   delete( ThetaA )

end
