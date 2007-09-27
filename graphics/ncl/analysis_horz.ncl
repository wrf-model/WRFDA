;********************************************************
;The scripts read "analysis" file from wrfvar 
;Plot Horz. cross-section 
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
; Copyright (C) 2006
; University Corporation for Atmospheric Research
; All Rights Reserved
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
; Author: Ashish Routray , S.R.H. Rizvi and Ananda Kumar Das
;         National Center for Atmospheric Research
;         Boulder, CO 80301
;
; Date: 01/17/07
;=========================================================;
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;=========================================================;
; Note:  Make sure following.
;         a) You have NCL Version: 4.2.0.a.034 upward
;         b) "wrf_user_fortran_util_0.so" file is created by using 
;            "RWRAPIT wrf_user_fortran_util_0.f" command
;****************************************************************************
;
;****************Change lines below *****************************************
;
load "/ptmp/rizvi/trunk/graphics/ncl/WRF_contributed.ncl.test"
load "/ptmp/rizvi/trunk/graphics/ncl/WRFUserARW.ncl"
external wrf_user_fortaran_util_0 "/ptmp/rizvi/trunk/graphics/ncl/wrf_user_fortran_util_0.so"
;
begin
   ;***************************************************************************
   ; open file 
   ;***************************************************************************

   works = "x11"
   sigma_lev=25
   analysis   = addfile("/mmm/mmmtmp/rizvi/data/india_30km/run_cv_options_hum_rh_md1/fc/2005080500/wrfinput_d01.nc", "r")    ; analysis file name

   var_U="U"
   var_V="V"
   var_T="T"
   var_Q="Q"

   unit_Q="gm/Kg"

   ;****************************************
   ; Read variables from "analysis" file
   ;***************************************


   T = analysis->T(0,sigma_lev-1,:,:)      ; Theta
   U= analysis->U(0,sigma_lev-1,:,:)       ; u-wind compt.
   V= analysis->V(0,sigma_lev-1,:,:)       ; v-wind compt.
   Q= analysis->QVAPOR(0,sigma_lev-1,:,:)  ; qvapor    

   ;**********************No need change lines here onward **********************
   ; create plots  
   ;********************************************************

   plts                      = new (4,"graphic")

   wks = gsn_open_wks(works,"psot_h.")          ; ps,pdf,x11,ncgm,eps
   gsn_define_colormap(wks,"gui_default")           ; select color map

   res                       = True            ; plot mods desired
   res@gsnDraw               = False                ; (a) do not draw
   res@gsnFrame              = False                ; (b) do not advance 'frame'

   res@gsnMaximize           = True            ; uncomment to maximize size

   res@cnFillOn              = True            ; turn on color
   res@gsnSpreadColors       = True            ; use entire color map
   res@cnLevelSelectionMode      = "ManualLevels"
   WRF_map_c(analysis,res,0)                ; set map resources

   lat = analysis->XLAT(0,:,:)
   lon = analysis->XLONG(0,:,:)
   dims = dimsizes(lat)
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
   res@tmXBMajorLengthF      = get_res_value(res, "tmXBMajorLengthF",0.01)
   res@tmYLMajorLengthF      = get_res_value(res, "tmYLMajorLengthF",0.01)
   res@tmXTOn                = get_res_value(res,"tmXTOn",False)
   res@tmYROn                = get_res_value(res,"tmYROn",False)
   res@tmYRLabelsOn          = get_res_value(res,"tmYRLabelsOn",True)
   res@tmXBBorderOn          = get_res_value(res,"tmXBBorderOn",True)
   res@tmXTBorderOn          = get_res_value(res,"tmXTBorderOn",True)
   res@tmYLBorderOn          = get_res_value(res,"tmYLBorderOn",True)
   res@tmYRBorderOn          = get_res_value(res,"tmYRBorderOn",True)
   res@lbLabelStride = 2           ; Color bar value writing

   res@gsnLeftString         = var_U
   plts(0)                   = gsn_csm_contour_map(wks,U,res)
   res@gsnLeftString         = var_V
   plts(1)                   = gsn_csm_contour_map(wks,V,res)
   res@gsnLeftString         = var_T
   plts(2)                   = gsn_csm_contour_map(wks,T,res)

   res@gsnLeftString         = var_Q+"("+unit_Q+")"     
   plts(3)                   = gsn_csm_contour_map(wks,Q*1000.,res)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@txString         = "Horizontal cross section at sigma level "+sigma_lev
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

end
