;
; Author: Xiaoyan Zhang   MMM/NCAR
;
; Purpose: plot the obs average and time series impact from FSO
;          i.e. WRFDA output (obs_impact).
; Input:   obs_impact
; 8/30/2011: code only availabe for U,V,T,P,Q,GPS
;            and conventional observation instrument
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;============================================================
; Main code
;============================================================
  start_date   = "2010060106"
  end_date     = "2010060818"
  cycle_period = 6
  expt_dir     = "/ptmp/xzhang/RUN_FSO/CONUS/run/"
  obsimpact_dir= "/obsimpact/working/obs_impact"
;---------------------------------------------------------------------------
; functions and procedures
;---------------------------------------------------------------------------

undef("change_date")
function change_date(ccyy:integer, mm:integer, dd:integer, delta:integer)
local mmday, newday
begin
   mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)
   if ( isleapyear(ccyy) ) then
      mmday(1) = 29
   end if
   dd = dd + delta
   if ( dd .eq. 0 ) then
      mm = mm - 1
      if ( mm .eq. 0 ) then
         mm = 12
         ccyy = ccyy - 1
      end if
      dd = mmday(mm-1)
   else
      if ( dd .gt. mmday(mm-1) ) then
         dd = 1
         mm = mm + 1
         if ( mm .gt. 12 ) then
            mm = 1
            ccyy = ccyy + 1
         end if
      end if
   end if
   newday = ccyy*10000 + mm*100 + dd
   ;   newday = sprinti("%0.4i",ccyy)+sprinti("%0.2i",mm)+sprinti("%0.2i",dd)
   return(newday)
end


function advance_cymdh(currentdatestr:string, dh:integer)
local ccyy, mm, dd, hh, newday, newdatestr
begin
   currentdate = stringtochar(currentdatestr)
   ccyy = stringtointeger((/currentdate(0:3)/))
   mm   = stringtointeger((/currentdate(4:5)/))
   dd   = stringtointeger((/currentdate(6:7)/))
   hh   = stringtointeger((/currentdate(8:9)/))
   hh = hh + dh
   newday = ccyy*10000 + mm*100 + dd
   ;   newday = sprinti("%0.4i",ccyy)+sprinti("%0.2i",mm)+sprinti("%0.2i",dd)
   do while (hh .lt. 0)
      hh = hh + 24
      newday = change_date( ccyy, mm, dd, -1 )
   end do
   do while (hh .gt. 23)
      hh = hh - 24
      newday = change_date( ccyy, mm, dd, 1 )
   end do
   ;   newdate = newday*100 + hh
   newdatestr = sprinti("%0.8i",newday) + sprinti("%0.2i",hh)
   return(newdatestr)
end

;---------------------------------------------------------------------------
; main procedures
;---------------------------------------------------------------------------
begin
  date = start_date
  ndate = 0
  do while ( date .le. end_date )  ; big date loop
    ndate = ndate +1
    date = advance_cymdh(date,cycle_period)
  end do  ; end of big date loop

  filename  = expt_dir+start_date+obsimpact_dir  ;ASCII file to read.
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

  var_vals = new(nvars,string)
  var_impact = new(nvars,float)
  avg_var_impact = new(nvars,float)
  all_var_impact = new((/ndate,nvars/),float)
 
  header3    = read_data(3)        ; Header. Use for observation types.
  nobs_types = str_fields_count(header3,delimiter)
  obs_types = new(nobs_types,string)
  obs_types_vals = new(nobs_types,string)
  impact = new(nobs_types,float)
  avg_impact = new(nobs_types,float)
  all_impact = new((/ndate,nobs_types/),float)
  do i=0,nobs_types-1
    obs_types(i) = str_get_field(header3,i+1,delimiter)
  end do
  print(obs_types)
;;; main loop times to read
  times = new(ndate,typeof(date))
  date = start_date
  ncount = 0
  do while ( date .le. end_date )  ; big date loop
    times(ncount) = date        ; for time series plot xaix lable
    filename  = expt_dir+date+obsimpact_dir  ;ASCII file to read.
    read_data = asciiread(filename,-1,"string")
    data1      = read_data(2:2)       ; Get the third line which is the impact for u,v,t,p,q,gps.
    do i=0,nvars-1
      var_vals(i) = str_get_field(data1,i+1,delimiter)
      print(var_vals(i))
      var_impact(i) = stringtofloat(var_vals(i))
      all_var_impact(ncount,i) = var_impact(i) ; save all dates obs impact for each variables
    end do

;;;; start to do same thing for each obs types
    data2      = read_data(4:4)       ; Get rid of first line which is a header.
    do i=0,nobs_types-1
      obs_types_vals(i) = str_get_field(data2,i+1,delimiter)
      print(obs_types_vals(i))
      impact(i) = stringtofloat(obs_types_vals(i))
      all_impact(ncount,i) = impact(i) ; save all dates obs impact for each obs types
    end do
    
    date = advance_cymdh(date,cycle_period)
    ncount = ncount +1
  end do  ; end of big date loop

;;;;; start to do average
   do i = 0,nvars-1
     avg_var_impact(i)=sum(all_var_impact(:,i))/ncount
   end do

   do i = 0,nobs_types-1
     avg_impact(i)=sum(all_impact(:,i))/ncount
   end do

  scale = 10000  ; for getting the integer X-Aix lable
  max_var_impact = floattointeger(max(avg_var_impact)/scale)  ;to  the maximum X-Aix lable
  min_var_impact = floattointeger(min(avg_var_impact)/scale)  ;to  the minimum X-Aix lable


  max_impact = floattointeger(max(impact)/scale)  ;to  the maximum X-Aix lable for the 2nd plot
  min_impact = floattointeger(min(impact)/scale)  ;to  the minimum X-Aix lable for the 2nd plot

  print(impact)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Start to creat bar plot ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 wks = gsn_open_wks("pdf","avg_obs_impact_bar."+2010060812)
;  wks = gsn_open_wks("x11","bar")
;
; Set resources for blank plot. Be sure to set axes limits
; that represent data that will be added later via polygons.
;
  res                             = True
  res@gsnMaximize                 = True    ; Maximize plot in frame
  res@gsnDraw                     = False   ; Don't draw
  res@gsnFrame                    = False   ; Don't advance frame
  res@vpWidthF                    = 0.6     ; Make long and
  res@vpHeightF                   = 0.6     ; narrow
;---Set axes limits. Add extra space for X max.
  res@trXMinF                     = min(avg_var_impact/1E+4)
  res@trXMaxF                     = max(avg_var_impact/1E+4)
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

;---Put variable labels on X axis
;  res@tmXBMode                    = "Explicit"
;  res@tmXBLabelsOn                = True    ; Turn on bottom labels
;  res@tmXBValues                  = ispan(min_var_impact*scale,max_var_impact*scale,scale)
;  res@tmXBLabels                  = res@tmXBValues    ; Turn on bottom labels
;  res@tmXBMinorOn                 = True   ; Turn off minor ticks on Y axis
;  res@tmXBMinorValues             = res@tmXBValues
  res@tmXBLabelFontHeightF        = 0.012   ; Increase font height
  res@tmXBLabelDeltaF             = 0.1     ; Increase space b/w ticks and labels
  res@tmXBMajorLengthF            = 0.01
;  res@tmXTOn                      = False   ; Turn off top tickmarks
  res@tiXAxisString = "Forecast error contribution ( x10000 J/kg)"  ; xaxis string
  res@tiXAxisFontHeightF          = 0.02
  res@tiMainString                = "Average between "+start_date+" - "+end_date 
  res@tiMainFontHeightF           = 0.015
;---Create blank plot without X grid lines
  plot_without_xgrid = gsn_blank_plot(wks,res)

;---Create blank plot with X grid lines
;  res@tmXMinorGrid                = True
  res@tmXMinorGridLineDashPattern = 2       ; Dashed lines
  res@tmXMinorGridThicknessF      = 1.0     ; default is 2 (?)
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
    xbar      = (/0,avg_var_impact(i-1)/1E+4,avg_var_impact(i-1)/1E+4,0,0/)
    print (xbar)
;    ybar       =(/i-0.25,i-0.25,i+0.25,i+0.25,i-0.25/)
    ybar       =(/i-0.15,i-0.15,i+0.15,i+0.15,i-0.15/)
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
  res1@trXMinF                    = min(avg_impact/1E+4)
  res1@trXMaxF                    = max(avg_impact/1E+4)
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
  res1@tiXAxisString = "Forecast error contribution ( x10000 J/kg)"  ; xaxis string
  res1@tiXAxisFontHeightF          = 0.02
;--Turn off Y axis tickmarks
  res1@tmYLMajorLengthF           = 0.
  res1@tmYLMajorOutwardLengthF    = 0.  
  res1@tmXBMajorLengthF           = 0.01
  res1@tmXBMajorOutwardLengthF    = 0.01
  res1@tmXTOn                     = False   ; Turn off top tickmarks

  res1@tiMainString                = "Average between "+start_date+" - "+end_date 
  res1@tiMainFontHeightF           = 0.015
;---Create blank plot without X grid lines

  plot_without_xgrid = gsn_blank_plot(wks,res1)

;---Create blank plot with X grid lines
  res1@tmXMajorGrid               = True    ; Turn on grid lines
  res1@tmXMajorGridLineDashPattern= 2       ; Dashed lines
  res1@tmXMajorGridThicknessF     = 1.0     ; default is 2 (?)
  plot_with_xgrid = gsn_blank_plot(wks,res1)  

  do i=1,nobs_types

;---Do longer bar first.
    bres@gsFillColor = "blue"
    xbar      = (/0,avg_impact(i-1)/1E+4,avg_impact(i-1)/1E+4,0,0/)
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

;;; plot the time series for 
  res2                            = True
;---Put variable labels on X axis
  res2@tmXBMode                    = "Explicit"
  res2@tmXBLabelsOn                = True    ; Turn on bottom labels
  res2@tmXBValues                  = ispan(0,ncount-1,4)
  dates = new(ncount/4+1,string)
  do i = 0, ncount-1,4
     dates(i/4) = times(i)
  end do
  res2@tmXBLabels                  = dates ;times    ; Turn on bottom labels
;  res2@tmXBMinorOn                 = True   ; Turn off minor ticks on Y axis
;  res2@tmXBMinorValues             = res@tmXBValues
  res2@tmXBLabelFontHeightF        = 0.012   ; Increase font height
  res2@tmXBLabelDeltaF             = 0.1     ; Increase space b/w ticks and labels
  res2@tmXBMajorLengthF            = 0.01
  res2@tmXBLabelAngleF             = -45
  res2@tmXTOn                      = False   ; Turn off top tickmarks
;  res2@trYAxisType                   = "LogAxis"
  res2@tiYAxisString = "Forecast error contribution ( x10000 J/kg)"  ; xaxis string
  res2@tiYAxisFontHeightF          = 0.02
  res2@tiXAxisString = "Dates"  ; xaxis string
  res2@tiXAxisFontHeightF          = 0.02
  res2@tiYAxisAngleF               = 90
  res2@xyLineThicknesses           = 2.0               ; make 2nd lines thicker
  res2@xyLineColors                = "blue"          ; change line color

  res2@gsnYRefLine           = 0.              ; reference line   
  res2@gsnXYBarChart         = True            ; create bar chart 
  res2@gsnXYBarChartBarWidth = 0.75                 ; change bar widths
  res2@gsnAboveYRefLineColor = "red"           ; above ref line fill red
  res2@gsnBelowYRefLineColor = "blue"          ; below ref line fill blue


  x = new(ncount,integer)
  do n = 0,ncount-1
    x(n) = n
  end do
  do i = 0,nobs_types-1
  res2@tiMainString               = "Time Series Impact of "+obs_types(i)
  if (avg_impact(i) .ne. 0.0) then
  print(all_impact(:,i))
;  plot = gsn_csm_xy(wks,x,all_impact(:,i),res2)         ; Draw an XY plot with 1 curve.
  plot = gsn_csm_xy(wks,x,all_impact(:,i)/1E+4,res2)         ; Draw an XY plot with 1 curve.
  end if
  end do
  print(obs_types)
  draw(plot)
end
