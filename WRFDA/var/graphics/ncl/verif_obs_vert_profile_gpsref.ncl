;============================================================;
;  vert_profile_gpsref.ncl
;============================================================;
; Purpose:
; plot vertical profile of BIAS, ABIAS or RMSE against GPS refractivity
; (GPSRF).
; History:
; Created: Date: 08/23/2007   
; Author : Syed RH Rizvi & Hui Shao , NCAR
;
; Note:
;
; (1) This script was modified based on vert_profile.ncl, which can be found
; in wrfvar repository trunk (in da/da_verif/scripts). 
; The original script was defined to plot
; u, v, t, q (and Ps for surface) for upper air/surface forecast verification
; against conventional data with a veritcal coordinate in pressure.
; For verification against GPS refractivity with a vertical coor in height, 
; work has been done in seting up veritcal coordinate, removing
; the dimension related multi-variables, etc. And also the way to plot
; two different y-axes has also been changed for a better display.
;
; (2) Note that the veritcal height levels have been hard-wired here as
; 0.2-30km with 0.2 vertical interval (150 levels totally). 
;
; (3)Data Format:  In each row total (1 + 150 x 4 = 601 = ncol) values for gpsref
; One value for date, 150 for total level number and
; for each level, there are 4 types of number, i.e.,
; NUM of obs at that level, BIAS, Absolute BIAS & RMSE
;
;=============================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
;-------------------------------------------------------------
begin
 Fill_ValueF = -99.99
 Fill_ValueI = -99
;-----------------------------------------------------------
;=============================================================;
 htop = 29.8    ; users can specifiy the top level (in km) of the plot
                ; current upper limit is 29.8km
 plot_error_bar = False
run_dir = run_dir+"/"
plot_dir = run_dir
read_headers = asciiread(run_dir+"header_main",-1,"string")
;----------------------------------------------------------
;-----------------------------------------------------------
Title = read_headers(0)
diag_var1 = read_headers(1)
diag_var2 = read_headers(2)
num_expt = stringtointeger(read_headers(3))
pdat_dirs = (/read_headers(4 : 3+num_expt)/)
if (get_omboma_plots .eq. "True" .or. \
    get_omboma_plots .eq. "true" .or. \
    get_omboma_plots .eq. ".True." .or. \
    get_omboma_plots .eq. ".true."      ) then
omboma_string="omboma_"
omboma=1
nexp=2*num_expt
legend_line_cols=new((/2*num_expt/),"string")
new_legends=new((/2*num_expt/),"string")
do i=1,num_expt
legend_line_cols(2*i-2)=exp_line_cols(i-1)
legend_line_cols(2*i-1)= legend_line_cols(2*i-2)
new_legends(2*i-2)=exp_legends(i-1)+" omb"
new_legends(2*i-1)= exp_legends(i-1)+" oma"
end do
else
omboma_string=""
omboma=0
nexp=num_expt
legend_line_cols=new((/num_expt/),"string")
new_legends=new((/num_expt/),"string")
do i=1,num_expt
legend_line_cols(i-1)=exp_line_cols(i-1)
new_legends(i-1)     = exp_legends(i-1)
end do
end if

; the following is for having num_obs
;num_legend=num_expt + 1
;new_legends=new((/num_legend/),"string")
;new_legends(0:num_legend-2)=exp_legends(0:num_legend-2)
;new_legends(num_legend-1)="Obs Num"
;-----------------------------------------------------------
num_d_levs=dimsizes(select_levs)
num_d_scor=dimsizes(select_scores)
;-----------------------------------------------------------
num_obs = dimsizes(read_headers) - (num_expt + 4)
fnames_obs = new((/num_obs/),"string")
do iob = 1, num_obs
fnames_obs(iob-1)= read_headers(num_expt+4+iob-1)
end do
;-----------------------------------------------------------

 gpsref_file="fnames_ggpsref"
 do iob = 1, num_obs
  if ( fnames_obs(iob-1) .eq. gpsref_file ) then
      file_names =  fnames_obs(iob-1)
  end if 
 end do
 nlev = 150   ; number of vertial levels (hard-wired for future improvement)
              ; it should be consistent with the nstdh value in 
              ; var/da/da_verif_obs/da_verif_obs_control.f90
 hlevels = fspan(0.2,30,150) ; veritcal levels (height, km) (hard-wired for improvement)
                             ; 0.2-30 km, 150 levels , 0.2 km interval
 htopid = floattointeger((htop-0.2)/0.2+1)

;----------------------------------------------------------
 varheads = asciiread(run_dir+file_names,-1,"string")
 nrow = stringtointeger(varheads(0))
 ncol = stringtointeger(varheads(1))
 varnames = (/varheads(2 : :)/)

 num_vars = dimsizes(varnames)/3
 varfiles = new((/2,num_vars/),"string")
 varunits = new((/num_vars/),"string")

do iv = 1, num_vars
  varfiles(0,iv-1)= varnames(3*(iv-1))
  varfiles(1,iv-1)= varnames(3*(iv-1)+1)
  varunits(iv-1)  = varnames(3*(iv-1)+2)
end do

;-----------------------------------------------------------

 out_name = "Profile_GPSREF_"+omboma_string
 nv = num_vars

 data_omb = new ((/nv,nexp,ncol/),"float",Fill_ValueF)
 data_oma = new ((/nv,nexp,ncol/),"float",Fill_ValueF)

 do iexp = 1, num_expt
 do iv = 1, nv
    data_omb((iv-1),(iexp-1),:)=asciiread(pdat_dirs(iexp-1)+varfiles(0,iv-1),(/ncol/),"float")
    data_oma((iv-1),(iexp-1),:)=asciiread(pdat_dirs(iexp-1)+varfiles(1,iv-1),(/ncol/),"float")
 end do
 end do

 diag_types=(/"BIAS","ABIAS","RMSE"/)
 diag_names=(/"Bias","Absolute Bias","RMSE"/)

;---------------------------------------
do idtype =1, num_d_scor ;loop for different diag_typs
  do ii = 1, 3
    if ( select_scores(idtype-1) .eq. diag_types(ii-1) ) then
      Type = ii
    end if
  end do
  diag_name=diag_names(Type-1)
;
 x = new((/nv,nexp,htopid+1/),"float",Fill_ValueF)
 number = new((/nv,nexp,htopid+1/),"float",Fill_ValueF)
 bar = new((/nv,nexp,htopid+1/),"float",Fill_ValueF)
 sigt = new((/nv,nexp,htopid+1/),"float",Fill_ValueF)
 ob_tot = new((/nv,htopid+1/),"float",Fill_ValueF)

do iv = 1, nv
  do iexp = 1, num_expt
    do ilev = 1 , htopid+1
     number((iv-1),(iexp-1),(ilev-1))  = data_omb(iv-1,iexp-1,1+(ilev-1)*6)
     if( omboma .eq. 1) then
      x(iv-1,2*iexp-2,ilev-1)    = data_omb(iv-1,iexp-1,1+(ilev-1)*6+Type)
      bar(iv-1,2*iexp-2,ilev-1)  = data_omb(iv-1,iexp-1,5+(ilev-1)*6)
      sigt(iv-1,2*iexp-2,ilev-1) = data_omb(iv-1,iexp-1,6+(ilev-1)*6)

      x(iv-1,2*iexp-1,ilev-1)    = data_oma(iv-1,iexp-1,1+(ilev-1)*6+Type)
      bar(iv-1,2*iexp-1,ilev-1)  = data_oma(iv-1,iexp-1,5+(ilev-1)*6)
      sigt(iv-1,2*iexp-1,ilev-1) = data_oma(iv-1,iexp-1,6+(ilev-1)*6)
     else
      x(iv-1,iexp-1,ilev-1)    = data_omb(iv-1,iexp-1,1+(ilev-1)*6+Type)
      bar(iv-1,iexp-1,ilev-1)  = data_omb(iv-1,iexp-1,5+(ilev-1)*6)
      sigt(iv-1,iexp-1,ilev-1) = data_omb(iv-1,iexp-1,6+(ilev-1)*6)
     end if
    end do
  end do
end do
do iv = 1, nv
    do ilev = 1 , htopid+1
     ob_tot((iv-1),(ilev-1)) = sum(data_omb((iv-1),0,(1+(ilev-1)*6)))
   end do
end do
;
; instead of the lowest height level (=0.2), start from 0km
; instead of the top standard height level (25km), end at htop (20km)
;
 hlevel =new((/htopid+1/),"string")

 levid=htopid-1
 y = ispan(0,htopid,1)
 ;hnumber(0)="0"
 hlevel(0)="0"
 ;hcl hlevel(1:)=flt2string(hlevels(:levid))
 hlevel(1:)= sprintf("%4.1f", (hlevels(:levid)))

 ylabels = new((/htopid+1/),"string")
 ylabels(0) = "0"
 ylabels(1:) = flt2string(number(0,0,:levid))
 
do iv =1, nv   ;loop for variable

 centers=new((/nexp,htopid+1/),graphic)
 error_bar = new((/nexp,htopid+1/),graphic)

pnumber = flt2string(ob_tot(iv-1,:))

;
; Open workstation
;
 wks   = gsn_open_wks (wksdev,plot_dir+out_name+diag_types(Type-1))  
 plts                      = new (nv,"graphic")
;
; Resources
;
 res          = True                              ; plot mods desired
 res@xyLineThicknesses = (/2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0/)            ; make 2nd lines thicker
 res@xyLineColors      = legend_line_cols                               ; change line color
 if ( omboma .eq. 1 ) then
    res@xyDashPatterns    = (/0,1,0,0,0,0,0,0,0,0/) 
 else
    res@xyDashPatterns    = (/0,0,0,0,0,0,0,0,0,0/) 
 end if
 res@xyMarkers         = (/2,2,2,2,2,2,2,2/)                            ; (none, dot, asterisk)
 res@xyMarkerColors    = res@xyLineColors             ; Marker colors
 res@xyMarkerSizeF          = 0.01                    ; Marker size (default is 0.01)
 res@xyLineLabelFontHeightF = 0.02                    ; Font size and color
 res@xyLineLabelFontColor   = 2                       ; for line labels

 res@vpXF            = 0.2                           ; this controls the size and location of the plot
 res@vpWidthF        = 0.5         
 res@vpYF            = 0.83
 res@vpHeightF       = 0.5

 res@tmXTBorderOn           = True                    ; set up the top border
 res@tmXTOn                 = False                   ; set the top tickmark

; res@tmYRBorderOn           = False                   ; set up the Right border
 res@tmYRLabelsOn           = True
 res@tmYROn                 = True                    ; set the Right tickmark
 res@tmYRMode               = "Explicit"
 res@tmYLMode               = "Explicit"
 ; res@tmLabelAutoStride      = True
 res@tmLabelAutoStride      = False
 res@tmYRLabelStride        = 5      ; to make one label per km
 res@tmYLLabelStride        = 5      ; to make one label per km
 res@tmYUseLeft             = False
 res@tmYLValues             = y
 res@tmYRValues             = y
 res@tmYRLabels             = ylabels(:)
 res@tmYRMajorLengthF       = 0.0
 res@tmYLMajorLengthF       = 0.0

 res@tiXAxisFont            = "Times-roman"           ; Font for X axis label
 res@tiYAxisFont            = "Times-roman"           ; Font for Y axis label
 res@tiXAxisFontThicknessF  = 3.0
 res@tiYAxisFontThicknessF  = 3.0
 res@tiXAxisFontHeightF     = 0.02                    ; Change the font size.
 res@tiYAxisFontHeightF     = 0.02
 res@tiXAxisFontAspectF     = 1.5
 res@tiYAxisFontAspectF     = 1.5
 res@tiYAxisString          = " Height (km)"              ; Label for the Y axis
 if ( Type .ne. 3) then
 res@gsnXRefLine            = 0.0                    ; reference line for x
 res@gsnXRefLineDashPattern = 11                     ; reference line style for x
 end if

 res@gsnDraw               = False                   ; (a) do not draw
 res@gsnFrame              = False                    ; (b) do not advance 'frame'
; res@tiMainString   = diag_name+" Profiles : "+Title
 res@tiMainString   = diag_name+" Profiles "+Title

 res@tmYLValues  = y                              ; Assign y-axis labels
 res@tmYRValues  = y
; res@tmYRLabels  = hnumber
 res@tmYLLabels  = hlevel
 res@tmYLLabelFontHeightF = 0.015
 res@tmYRLabelFontHeightF = 0.015

 res@tiXAxisString   = varunits             ; Label for the X axis

; res@gsnRightString = "Obs Num"
; res@gsnRightStringParallelPosF = 1.15
; res@gsnRightStringOrthogonalPosF = 0.03
; res@trYMaxF = nlev
;
; Plot
;
 plts(iv-1) = gsn_csm_xy(wks,x(iv-1,:,:),y,res)

; add ploymarkers
  polyres = True
  polyres@gsMarkerIndex     = 1                          ; index for circle
  polyres@gsMarkerSizeF     = .02                        ; size
  polyres@gsMarkerThicknessF     = 5                          ; thickness

 do iexp=1,nexp
 do ilev=1,htopid+1
 if (.not. ismissing(x(iv-1,iexp-1,ilev-1))) then
 polyres@gsMarkerColor     = legend_line_cols(iexp-1)                         ; color
 polyres@gsLineColor = legend_line_cols(iexp-1)
 if ( plot_error_bar ) then
 error_bar(iexp-1,ilev-1)=gsn_add_polyline(wks,plts, \
  (/x(iv-1,iexp-1,ilev-1)+bar(iv-1,iexp-1,ilev-1),x(iv-1,iexp-1,ilev-1)-bar(iv-1,iexp-1,ilev-1)/),\
  (/y(ilev-1) ,y(ilev-1)/), polyres)
 end if
;removed the center significance marker since the error bar already indicate that 
;markers at each level make the figure crowded. Hui 04/01/2008
; if(sigt(iv-1,iexp-1,ilev-1) .gt. 0 ) then
;   centers(iexp-1,ilev-1)=\
;   gsn_add_polymarker(wks,plts,x(iv-1,iexp-1,ilev-1),y(ilev-1), polyres)
; end if
end if
 end do
 end do

draw ( plts(iv-1) )


; plts = gsn_csm_xy(wks,x(:,:),hlevel(:),res)
;
; Add legend
;
 legend = create "Legend" legendClass wks
  "vpXF"                     : 0.20                    ; position on virtual page
  "vpYF"                     : 0.2
  "vpWidthF"                 : 0.6                     ; width
  "vpHeightF"                : 0.1                     ; height
  "lgPerimOn"                : False                   ; no perimeter
  "lgItemCount"              : nexp                    ; how many
  "lgLabelsOn"               : False                   ; no default lables
  "lgLineLabelFontHeightF"   : 0.012                   ; font height
  "lgMonoLineThickness"      : True
  "lgLineThicknessF"         : 2.0
  "lgDashIndexes"            : (/0,1,0,1,0,1,0,1,0,1,0,1/)             ; line paters
  "lgLineLabelStrings"       : new_legends
  "lgLineColors"             : legend_line_cols
  "lgMonoLineLabelFontColor" : True                    ; one label color
 end create
   if ( omboma .eq. 0 ) then
      setvalues legend
         "lgDashIndexes"       : (/0,0,0,0,0,0,0,0,0,0,0,0/)             ; line paters
      end setvalues
   end if

 draw(legend)
frame(wks)
delete(centers)
delete(error_bar)
;delete(plts(iv-1)

;----------------------------------------------
end do  ;loop for variable

;
; Close workstation
;
; frame(wks)
 delete(wks)

end do ;loop for different diag_types
;-------------------------------------

delete(varheads)
delete(varnames)
delete(varfiles)
delete(varunits)
delete(x)
;delete(ob_tot)
delete(y)
delete(data_omb)
delete(data_oma)
delete(plts)

end
