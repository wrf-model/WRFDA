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

     walltime = new((/8,3/),"float")
     walltime(:,0) = (/4.99,2.49,1.32,0.68,0.42,0.28,0.15,0.11/)    ; FWM
     walltime(:,1) = (/10.42,5.28,2.74,1.40,0.86,0.49,0.29,0.20/)   ; TLM
     walltime(:,2) = (/25.91,12.93,6.73,3.55,2.13,1.20,0.94,0.50/)  ; ADM
;======================================================================================	
     wks = gsn_open_wks("X11","bluefiretime")
     newcolor = NhlNewColor(wks,.6,.85,1.)
     newcolor = NhlNewColor(wks,0.42,0.42,0.42)

     sres = True
     sres@vpWidthF = 0.7
     sres@vpHeightF = 0.5
     sres@vpXF = .15
     sres@trXMinF = 0.5
     sres@trXMaxF = 8.5
     sres@trYMinF = 0.0
     sres@trYMaxF = 26.0
     sres@gsnDraw = True
     sres@gsnFrame = False
     sres@gsnXYBarChart = True
     sres@gsnXYBarChartBarWidth = 0.25           ; change bar widths
     sres@tmXBMode          = "Explicit"         ; explicit labels
     sres@tmXBValues        = (/1,2,3,4,5,6,7,8/)
     sres@tmXBLabels = (/"16","32","64","128","256","512","1024","2048"/)
     sres@tmXBLabelFontHeightF = 0.0205
     sres@tmXTLabelFontHeightF = 0.0205
     sres@tmYLLabelFontHeightF = 0.0205
     sres@tiMainFontHeightF = 0.020
     sres@tiMainFont = "helvetica"
     sres@tiMainString = "Wallclock for one-time step integration on Bluefire"
     sres@gsnRightString = ""
     sres@tiYAxisString = "Seconds"
     sres@tiXAxisString = "Processor Count"
        
     sres@gsnXYBarChartColors = (/"red"/)	
     plot1 = gsn_csm_xy(wks,fspan(.75,7.75,8),walltime(:,0),sres)		; draw each timeseries
     sres@gsnXYBarChartColors = (/"green"/)					; seperately, not
     plot2 = gsn_csm_xy(wks,fspan(1.0,8.0,8),walltime(:,1),sres)		; advancing the frame
     sres@gsnXYBarChartColors = (/"blue"/)					; but tweaking where
     plot3 = gsn_csm_xy(wks,fspan(1.25,8.25,8),walltime(:,2),sres)		; each timeseries is

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
     lbres@lgPerimColor 	 = "white"
     labels = (/"ADM","TLM","FWM"/)
     gsn_labelbar_ndc(wks,3,labels,0.62,0.73,lbres)	; draw right labelbar column
	
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
;    xpos   = (/0.35,    0.5,     0.65/)

;    do i=0,2
;      lbres@lbFillColors = colors(i)
;      gsn_labelbar_ndc(wks,1,labels(i),xpos(i),0.75,lbres)
;    end do	

;
     frame(wks)      
end
