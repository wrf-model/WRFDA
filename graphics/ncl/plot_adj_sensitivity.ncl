;=========================================================;
; Purpose: Plots forcing to Adjoint Sensitivity 
; (i.e. the sensitivity of forecast error to final state)
;=========================================================;
;
; 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "./WRF_contributed.ncl.test"

; Note:  Make sure following.
;         a) You have NCL Version: 4.2.0.a.034 upward
;         b) "wrf_user_fortran_util_0.so" file is created by using 
;            "WRAPIT wrf_user_fortran_util_0.f" command
;****************************************************************************
;****************Change lines below *****************************************
;field = addfile("/users/auligne/Expt/tutorial_asr180/run/2007010100/sensitivity/wrf_xb/final_sens_d01_2007-01-01_06:00:00"+".nc", "r")
;plot_option = "final"

field = addfile("/users/auligne/Expt/tutorial_asr180/run/2007010100/sensitivity/ad_d01_2007010100"+".nc", "r")
plot_option = "initial"

;****************************************************************************
begin
   ;***************************************************************************
   ; open file 
   ;***************************************************************************

   works = "pdf"
if (plot_option .eq. "final") then
   wks = gsn_open_wks(works,"final_sensitivity")          ; ps,pdf,x11,ncgm,eps
   Title = "Final Sensitivity"
else
   if (plot_option .eq. "initial") then
      wks = gsn_open_wks(works,"initial_sensitivity")          ; ps,pdf,x11,ncgm,eps
      Title = "Initial Sensitivity"
   else
      print("plot_option must be either 'initial' or 'final'")      
      return
   end if
end if

   thinning_factor = 0.025
;   vector_magnitude = 1.0
   vector_length    = 0.05

   unit_Q="Pa"

   do sigma_lev=1,27,5

   T = field->T(0,sigma_lev-1,:,:)      ; Theta
   U = field->U(0,sigma_lev-1,:,:)      ; u-wind compt.
   V = field->V(0,sigma_lev-1,:,:)      ; v-wind compt.
   P = field->PHB(0,sigma_lev-1,:,:)      ;        

   U@units = U@units
   V@units = V@units
   T@units = T@units
   P@units = P@units

if (plot_option .eq. "final") then
   T = field->G_T(0,sigma_lev-1,:,:)      ; Theta
   U = field->G_U(0,sigma_lev-1,:,:)      ; u-wind compt.
   V = field->G_V(0,sigma_lev-1,:,:)      ; v-wind compt.
   P = field->G_P(0,sigma_lev-1,:,:)      ;        
else
   if (plot_option .eq. "initial") then
      T = field->A_T(0,sigma_lev-1,:,:)      ; Theta
      U = field->A_U(0,sigma_lev-1,:,:)      ; u-wind compt.
      V = field->A_V(0,sigma_lev-1,:,:)      ; v-wind compt.
      P = field->A_P(0,sigma_lev-1,:,:)      ;        
   end if
end if

  print ("min Theeta = "+min(T))
  print ("max Theeta = "+max(T))
  print ("min U = "+min(U))
  print ("max U = "+max(U))
  print ("min V = "+min(V))
  print ("max V = "+max(V))
  print ("min P = "+min(P))
  print ("max P = "+max(P))

  vector_magnitude=max(U)

   lat = field->XLAT(0,:,:)
   lon = field->XLONG(0,:,:)
   dims = dimsizes(lat)

   dimU  = dimsizes(U)
   lonU = dimU(1)

   dimV  = dimsizes(V)
   latV = dimV(0)

   UU = 0.5*(U(:,0:lonU-2)+U(:,1:lonU-1))
   VV = 0.5*(V(0:latV-2,:)+V(1:latV-1,:))

   UU@units = U@units
   VV@units = V@units


   ;**********************No need change lines here onward **********************
   ; create plots
   ;********************************************************

   gsn_define_colormap(wks,"gui_default")           ; select color map

   plts                      = new (4,"graphic")


   res                       = True            ; plot mods desired
   res@gsnDraw               = False                ; (a) do not draw
   res@gsnFrame              = False                ; (b) do not advance 'frame'

   res@gsnMaximize           = True            ; uncomment to maximize size

  res@gsnMaximize           = True            ; uncomment to maximize size

  res@cnFillOn              = True            ; turn on color
  res@cnLineLabelsOn        = False           ; turn off contour line labels
  res@gsnSpreadColors       = True            ; use entire color map
  res@gsnScalarContour      = True            ; contours desired


  res@mpGridAndLimbOn       = True            ; trun on lat/lon lines
  res@pmTickMarkDisplayMode = "Always"        ; turn on tickmarks
  res@tmXTOn = False                          ; turn off top   labels
  res@tmYROn = False                          ; turn off right labels

  res@vcGlyphStyle       = "CurlyVector"      ; curly vectors
  res@vcRefMagnitudeF    = vector_magnitude   ; define vector ref mag
  res@vcRefLengthF       = vector_length      ; define vector ref length
  res@vcMinDistanceF        = thinning_factor  ; larger means sparser

   WRF_map_c(field,res,0)                ; set map resources

   ;
   ; Select portion of the map to view.
   ;
   res@mpLimitMode           = get_res_value_keep(res, "mpLimitMode","Corners")
   res@mpLeftCornerLatF      = get_res_value_keep(res, "mpLeftCornerLatF",lat(0,0))
   res@mpLeftCornerLonF      = get_res_value_keep(res, "mpLeftCornerLonF",lon(0,0))
   res@mpRightCornerLatF     = get_res_value_keep(res, "mpRightCornerLatF", lat(dims(0)-1,dims(1)-1))
   res@mpRightCornerLonF     = get_res_value_keep(res, "mpRightCornerLonF", lon(dims(0)-1,dims(1)-1))
   res@pmTickMarkDisplayMode       = get_res_value_keep(res, "pmTickMarkDisplayMode","Always")

   ;
   ; Tick mark resources
   ;
;   res@tmXBMajorLengthF      = get_res_value(res, "tmXBMajorLengthF",0.01)
;   res@tmYLMajorLengthF      = get_res_value(res, "tmYLMajorLengthF",0.01)
;   res@tmXTOn                = get_res_value(res,"tmXTOn",False)
;   res@tmYROn                = get_res_value(res,"tmYROn",False)
;   res@tmYRLabelsOn          = get_res_value(res,"tmYRLabelsOn",True)
;   res@tmXBBorderOn          = get_res_value(res,"tmXBBorderOn",True)
;   res@tmXTBorderOn          = get_res_value(res,"tmXTBorderOn",True)
;   res@tmYLBorderOn          = get_res_value(res,"tmYLBorderOn",True)
;   res@tmYRBorderOn          = get_res_value(res,"tmYRBorderOn",True)
   res@lbLabelStride = 2           ; Color bar value writing

   res@gsnSpreadColors       = True           ; use entire color map
   res@gsnLeftString         = "U"
   res@gsnRightString        = UU@units
   plts(0)                   = gsn_csm_vector_scalar_map(wks,UU,VV,UU,res) ;vector overlaid
   res@gsnLeftString         = "V"
   res@gsnRightString        = VV@units
   res@cnFillOn              = True           ; turn on color
   plts(1)                   = gsn_csm_vector_scalar_map(wks,UU,VV,VV,res) ;vector overlaid

; resource for T and QV (contour)
  res1                       = True           ; plot mods desired
  res1@gsnDraw               = False          ; (a) do not draw
  res1@gsnFrame              = False          ; (b) do not advance 'frame'
  res1@gsnMaximize           = True           ; uncomment to maximize size
  res1@lbLabelStride = 2                      ; Color bar value writing

  res1@cnFillOn              = True           ; turn on color
  res1@cnLinesOn             = False 
  res1@cnLineLabelsOn        = False          ; turn off contour line labels
  res1@gsnSpreadColors       = True           ; use entire color map

  WRF_map_c(field,res1,0)                ; set map resources


  res1@mpGridAndLimbOn       = True           ; trun on lat/lon lines
  res1@pmTickMarkDisplayMode = "Always"       ; turn on tickmarks
  res1@tmXTOn = False                         ; turn off top   labels
  res1@tmYROn = False                         ; turn off right labels




   res1@gsnLeftString         = "Theta"
   res1@gsnRightString        = T@units
   plts(2)                   = gsn_csm_contour_map(wks,T,res1)

   res1@gsnLeftString         = "P"
   res1@gsnRightString        = P@units 
   plts(3)                   = gsn_csm_contour_map(wks,P,res1)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@gsnFrame         = False                  ; don't advance panel plot
   resP@txString         = Title   
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot
  txres               = True
  txres@txFontHeightF = 0.012

  gsn_text_ndc(wks,"Horizontal Cross-section at sigma_level="+sigma_lev ,0.5,0.94,txres)
  frame(wks)
  end do
end
