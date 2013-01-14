;*******************************************************
; unique_5.ncl
;
; Concepts illustrated:
;   - Drawing multiple bar charts on a page
;   - Drawing three custom legends outside a bar chart
;   - Using bar charts to draw standard deviations for four timeseries
;*******************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

     walltime = new((/6,3/),"float")
     walltime(:,0) = (/6.52,3.37,2.04,1.50,1.13,0.93/)    ; FWM
     walltime(:,1) = (/15.80,7.78,4.70,3.24,2.43,1.80/)   ; TLM
     walltime(:,2) = (/33.20,16.63,10.12,7.20,5.53,3.50/) ; ADM
;======================================================================================	
     wks = gsn_open_wks("eps","lynxtime")
     newcolor = NhlNewColor(wks,.6,.85,1.)
     newcolor = NhlNewColor(wks,0.42,0.42,0.42)

     sres = True
     sres@vpWidthF = 0.7
     sres@vpHeightF = 0.5
     sres@vpXF = .15
     sres@trXMinF = 0.5
     sres@trXMaxF = 6.5
     sres@trYMinF = 0.0
     sres@trYMaxF = 34.0
     sres@gsnDraw = True
     sres@gsnFrame = False
     sres@gsnXYBarChart = True
     sres@gsnXYBarChartBarWidth = 0.25           ; change bar widths
     sres@tmXBMode          = "Explicit"         ; explicit labels
     sres@tmXBValues        = (/1,2,3,4,5,6/)
     sres@tmXBLabels = (/"16","32","64","128","256","512"/)
     sres@tmXBLabelFontHeightF = 0.0205
     sres@tmXTLabelFontHeightF = 0.0205
     sres@tmYLLabelFontHeightF = 0.0205
     sres@tiMainFontHeightF = 0.020
     sres@tiMainFont = "helvetica"
     sres@tiMainString = "Wallclock for one-time step integration on Lynx"
     sres@gsnRightString = ""
     sres@tiYAxisString = "Seconds"
     sres@tiXAxisString = "Processor Count"
        
     sres@gsnXYBarChartColors = (/"red"/)	
     plot1 = gsn_csm_xy(wks,fspan(0.75,5.75,6),walltime(:,0),sres)		; draw each timeseries
     sres@gsnXYBarChartColors = (/"green"/)					; seperately, not
     plot2 = gsn_csm_xy(wks,fspan(1.0,6.0,6),walltime(:,1),sres)		; advancing the frame
     sres@gsnXYBarChartColors = (/"blue"/)					; but tweaking where
     plot3 = gsn_csm_xy(wks,fspan(1.25,6.25,6),walltime(:,2),sres)		; each timeseries is

; Create some labelbars for a legend
     lbres                    = True          ; labelbar only resources
     lbres@vpWidthF           = 0.20          ; labelbar width
     lbres@vpHeightF          = 0.15          ; labelbar height
     lbres@lbBoxMajorExtentF  = 0.20          ; puts space between color boxes
     lbres@lbFillColors       = (/"blue","green","red"/)
     lbres@lbMonoFillPattern  = True          ; Solid fill pattern
     lbres@lbLabelFontHeightF = 0.015         ; font height. default is small
     lbres@lbLabelJust        = "CenterLeft"  ; left justify labels
     lbres@lbPerimOn          = False
     lbres@lgPerimColor          = "white"
     labels = (/"ADM","TLM","FWM"/)
     gsn_labelbar_ndc(wks,3,labels,0.62,0.73,lbres)     ; draw right labelbar column

; Create some labelbars for a legend
;    lbres                    = True          ; labelbar only resources
;    lbres@vpWidthF           = 0.25          ; labelbar width
;    lbres@vpHeightF          = 0.1           ; labelbar height
;    lbres@lbBoxMajorExtentF  = 0.25          ; puts space between color boxes
;    lbres@lbMonoFillPattern  = True          ; Solid fill pattern
;    lbres@lbLabelFontHeightF = 0.015         ; font height. default is small
;    lbres@lbLabelJust        = "CenterLeft"  ; left justify labels
;    lbres@lbPerimOn          = False

;    colors = (/"red",  "green",  "blue"/)
;    labels = (/"FWM", "TLM",    "ADM"/)
;    xpos   = (/0.15,    0.4,     0.65/)

;    do i=0,2
;      lbres@lbFillColors = colors(i)
;      gsn_labelbar_ndc(wks,1,labels(i),xpos(i),0.20,lbres)
;    end do	

;
     frame(wks)      
end
