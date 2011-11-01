;
; Author: Xiaoyan Zhang   MMM/NCAR
;
; Purpose: plot the individul obs impact from FSO
; Input :  obs_impact.
; 8/30/2011: code only availabe for U,V,T,P,Q,GPS
;            and conventional observation instrument
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;============================================================
; Main code
;============================================================
begin
  filename  = "obs_impact"                ; ASCII file to read.
  print(filename)
  read_data = asciiread(filename,-1,"string")
  print(dimsizes(read_data))
  header1    = read_data(0)        ; Header. Use for variable names.
  header2    = read_data(1)        ; Header. Use for variable names.
  delimiter = "    " 
  nvars   = str_fields_count(header2,delimiter)
  print(nvars)
  var_names = new(nvars,string)
  do i=0,nvars-1
    var_names(i) = str_get_field(header2,i+1,delimiter)
  end do
  print(var_names)

  data1      = read_data(2:2)       ; Get the third line which is the impact for u,v,t,p,q,gps.
  var_vals = new(nvars,string)
  var_impact = new(nvars,float)
  do i=0,nvars-1
    var_vals(i) = str_get_field(data1,i+1,delimiter)
    print(var_vals(i))
    var_impact(i) = stringtofloat(var_vals(i))
  end do
  scale = 100000  ; for getting the integer X-Aix lable
  max_var_impact = floattointeger(max(var_impact)/scale)  ;to  the maximum X-Aix lable
  min_var_impact = floattointeger(min(var_impact)/scale)  ;to  the minimum X-Aix lable

  header3    = read_data(3)        ; Header. Use for observation types.
  nobs_types = str_fields_count(header3,delimiter)
  obs_types = new(nobs_types,string)
  obs_types_vals = new(nobs_types,string)
  impact = new(nobs_types,float)
  do i=0,nobs_types-1
    obs_types(i) = str_get_field(header3,i+1,delimiter)
  end do
  print(obs_types)
  data2      = read_data(4:4)       ; Get rid of first line which is a header.
  do i=0,nobs_types-1
    obs_types_vals(i) = str_get_field(data2,i+1,delimiter)
    print(obs_types_vals(i))
    impact(i) = stringtofloat(obs_types_vals(i))
  end do

  max_impact = floattointeger(max(impact)/scale)  ;to  the maximum X-Aix lable for the 2nd plot
  min_impact = floattointeger(min(impact)/scale)  ;to  the minimum X-Aix lable for the 2nd plot

  print(impact)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Start to creat bar plot ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 wks = gsn_open_wks("pdf","obs_impact")
;  wks = gsn_open_wks("x11","bar")
;
; Set resources for blank plot. Be sure to set axes limits
; that represent data that will be added later via polygons.
;
  res                             = True
  res@gsnMaximize                 = True    ; Maximize plot in frame
  res@gsnDraw                     = False   ; Don't draw
  res@gsnFrame                    = False   ; Don't advance frame
  res@vpWidthF                    = 0.8     ; Make long and
  res@vpHeightF                   = 0.4     ; narrow
;---Set axes limits. Add extra space for X max.
  res@trXMinF                     = min(var_impact)
  res@trXMaxF                     = max(var_impact)
  res@trYMinF                     = 0
  res@trYMaxF                     = nvars + 1
  res@gsnTickMarksPointOutward    = True
;---Put variable labels on Y axis
  res@tmYUseLeft                  = False   ; Make right axis independent of left
  res@tmYLOn                      = False   ; Turn off left tickmarks
  res@tmYROn                      = True    ; Turn on right tickmarks
  res@tmYLLabelsOn                = False   ; Turn off left labels
  res@tmYRLabelsOn                = True    ; Turn on right labels
  res@tmYRMinorOn                 = False   ; Turn off minor ticks on Y axis
  res@tmYRLabelFontHeightF        = 0.015   ; Increase font height
  res@tmYRLabelDeltaF             = 0.1     ; Increase space b/w ticks and labels
;  res@tmYRLabelJust               = "CenterRight"   ; right-justify labels
  res@tmYRMode                    = "Explicit"
  res@tmYRValues                  = ispan(1,nvars,1)
  res@tmYRLabels                  = var_names

;---Put variable labels on Y axis
;  res@tmXBMode                    = "Explicit"
;  res@tmXBLabelsOn                = False    ; Turn on bottom labels
;  res@tmXBValues                  = ispan(min_var_impact*scale,max_var_impact*scale,scale)
;  res@tmXBLabels                  = res@tmXBValues    ; Turn on bottom labels
;  res@tmXBMinorOn                 = True   ; Turn off minor ticks on Y axis
;  res@tmXBMinorValues             = res@tmXBValues
  res@tmXBLabelFontHeightF        = 0.012   ; Increase font height
  res@tmXBLabelDeltaF             = 0.1     ; Increase space b/w ticks and labels
  res@tmXBMajorLengthF            = 0.01
;  res@tmXTOn                      = False   ; Turn off top tickmarks
  res@tiXAxisString = "Forecast error contribution (J/kg)"  ; xaxis string
  res@tiXAxisFontHeightF          = 0.02
;---Create blank plot without X grid lines
  plot_without_xgrid = gsn_blank_plot(wks,res)

;---Create blank plot with X grid lines
;  res@tmXMinorGrid                = True
;  res@tmXMinorGridLineDashPattern = 2       ; Dashed lines
;  res@tmXMinorGridThicknessF      = 1.0     ; default is 2 (?)
  res@tmXMajorGrid                = True    ; Turn on grid lines
  res@tmXMajorGridLineDashPattern = 2       ; Dashed lines
  res@tmXMajorGridThicknessF      = 1.0     ; default is 2 (?)
  plot_with_xgrid = gsn_blank_plot(wks,res)  

;---Arrays to hold the bar values and the bars themselves.
  xbar = new(5,float)
  ybar = new(5,float)
  dum1 = new(nvars,graphic)
  dum2 = new(nvars,graphic)
  dum3 = new(nobs_types,graphic)

;---Set some resources for the bars.
  bres                            = True
  bres@gsEdgesOn                  = True      ; Outline the polygons (bars)
;
; Loop through each city and attach bar to plot that
; doesn't have grid lines.
;

;;;; plot the impact for U,V,T,P,Q,GPS


  do i=1,nvars

;---Do longer bar first.
    bres@gsFillColor = "red"
    xbar      = (/0,var_impact(i-1),var_impact(i-1),0,0/)
    print (xbar)
    ybar       =(/i-0.25,i-0.25,i+0.25,i+0.25,i-0.25/)
    dum2(i-1) = gsn_add_polygon(wks,plot_without_xgrid,xbar,ybar,bres)
  end do
  draw(plot_with_xgrid)
  draw(plot_without_xgrid)
  frame(wks)

;;;; plot the impact for each instrument
  res1                            = True
  res1@gsnMaximize                = True    ; Maximize plot in frame
  res1@gsnDraw                    = False   ; Don't draw
  res1@gsnFrame                   = False   ; Don't advance frame
  res1@vpWidthF                   = 0.6     ; Make long and
  res1@vpHeightF                  = 0.6     ; narrow
;---Set axes limits. Add extra space for X max.
  res1@trXMinF                    = min(impact)
  res1@trXMaxF                    = max(impact)
  res1@trYMinF                    = 0
  res1@trYMaxF                    = nobs_types + 1
  res1@gsnTickMarksPointOutward   = True
;---Put variable labels on Y axis
  res1@tmYUseLeft                 = False   ; Make right axis independent of left
  res1@tmYLOn                     = False   ; Turn off left tickmarks
  res1@tmYROn                     = True    ; Turn on right tickmarks
  res1@tmYLLabelsOn               = False   ; Turn off left labels
  res1@tmYRLabelsOn               = True    ; Turn on right labels
  res1@tmYRMinorOn                = False   ; Turn off minor ticks on Y axis
  res1@tmYRLabelFontHeightF       = 0.01   ; Increase font height
  res1@tmYRLabelDeltaF            = 0.1     ; Increase space b/w ticks and labels
;  res1@tmYRLabelJust              = "CenterRight"   ; right-justify labels
  res1@tmYRMode                   = "Explicit"
  res1@tmYRValues                 = ispan(1,nobs_types,1)
  res1@tmYRLabels                 = obs_types

;---Put variable labels on X axis
;  res1@tmXBMode                    = "Explicit"
;  res1@tmXBLabelsOn                = True    ; Turn on bottom labels
;  res1@tmXBValues                  = ispan(min_impact*scale,max_impact*scale,scale)
;  res1@tmXBLabels                  = res@tmXBValues    ; Turn on bottom labels
;  res1@tmXBMinorOn                 = True   ; Turn off minor ticks on Y axis
;  res1@tmXBMinorValues             = res@tmXBValues
  res1@tmXBLabelFontHeightF        = 0.012   ; Increase font height
  res1@tmXBLabelDeltaF             = 0.1     ; Increase space b/w ticks and labels
  res1@tmXBMajorLengthF            = 0.01
;  res1@tmXTOn                      = False   ; Turn off top tickmarks
  res1@tiXAxisString = "Forecast error contribution (J/kg)"  ; xaxis string
  res1@tiXAxisFontHeightF          = 0.02
;--Turn off Y axis tickmarks
  res1@tmYLMajorLengthF           = 0.
  res1@tmYLMajorOutwardLengthF    = 0.  
  res1@tmXBMajorLengthF           = 0.01
  res1@tmXBMajorOutwardLengthF    = 0.01
  res1@tmXTOn                     = False   ; Turn off top tickmarks
;---Create blank plot without X grid lines

  plot_without_xgrid = gsn_blank_plot(wks,res1)

;---Create blank plot with X grid lines
  res1@tmXMajorGrid               = True    ; Turn on grid lines
  res1@tmXMajorGridLineDashPattern= 2       ; Dashed lines
  res1@tmXMajorGridThicknessF     = 1.0     ; default is 2 (?)
  plot_with_xgrid = gsn_blank_plot(wks,res1)  

  do i=1,nobs_types

;---Do longer bar first.
    bres@gsFillColor = "red"
    xbar      = (/0,impact(i-1),impact(i-1),0,0/)
    print (xbar)
    ybar       =(/i-0.25,i-0.25,i+0.25,i+0.25,i-0.25/)
    dum3(i-1) = gsn_add_polygon(wks,plot_without_xgrid,xbar,ybar,bres)
  end do

;
; Drawing the plot will draw all the attached bars. Have to draw
; the plot twice: one with the X grid lines and one without, in
; order to make sure the X grid lines are behind the bars.
;
; Couldn't see how else to do this.
;
  draw(plot_with_xgrid)
  draw(plot_without_xgrid)
  frame(wks)
end
