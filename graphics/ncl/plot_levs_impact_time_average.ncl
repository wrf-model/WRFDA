;,
; Author: Xiaoyan Zhang   MMM/NCAR
;
; Purpose: plot the overage over time of obs impact on vertical levels
; Method:  Read the gts_omb_oma, pick the inovation value (that is replaced by obs impact after FSO);
;          sum the total impact on specific level (1st plot) and divided it by obs number on this leveli(2nd plot);
;          finally do the average over time
; Expression 1: sum[total impact(u,v,t,q)_on_each_level] /date_count
;            2: sum[total impact(u,v,t,q)_on_each_level/obs_number_on_each_level] /date_count
; Input_data: gts_omb_oma_01.
; 11/01/2011: code only well tested for sound and tamdar instrument
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;============================================================
; Main code
;============================================================

;  start_date   = "2010010200"
;  end_date     = "2010013000"
  start_date   = "2010060118"
  end_date     = "2010060218"
  cycle_period = 24
  date         = start_date
  datdir1      = "/glade/scratch/xzhang/fso_run/"
;  datdir2      = "/obsimpact/working/"                      ; the path after  DATE
  datdir2      = "/obsimpact/"                      ; the path after  DATE
  filename_gts   = "gts_omb_oma_01"

;---------------------------------------------------------------------------
; functions and procedures
;---------------------------------------------------------------------------
function read_info(ndata:integer, idata:integer, cdata:character, nlevel:integer, ilevel:integer, type:string)
local info, ibeg, iend, n, is, ie
begin ; begin of function read_info
   if ( type .eq. "id" ) then
      ibeg = 16
      iend = 20
   end if
   if ( type .eq. "lat" ) then
      ibeg = 21
      iend = 29
   end if
   if ( type .eq. "lon" ) then
      ibeg = 30
      iend = 38
   end if
   if ( type .eq. "pre" .or. type .eq. "hgt" ) then
      ibeg = 39
      iend = 55
   end if
   if ( type .eq. "id" ) then
      info = new(sum(nlevel),string)
   else
      info = new(sum(nlevel),float)
   end if
   do n = 0, ndata-1
      if ( nlevel(n) .gt. 0 ) then
         if ( n .eq. 0 ) then
            is = 0
            ie = nlevel(n) - 1
         else
            is = ie + 1
            ie = is + nlevel(n) - 1
         end if
         if ( type .eq. "id" ) then
            info(is:ie) = charactertostring(cdata(ilevel(n)+1:ilevel(n)+nlevel(n),ibeg:iend))
         else
            info(is:ie) = stringtofloat(charactertostring(cdata(ilevel(n)+1:ilevel(n)+nlevel(n),ibeg:iend)))
         end if
      end if
   end do
   return(info)
end ; end of function read_info

function read_data(ndata:integer, idata:integer, cdata:character, nlevel:integer, ilevel:integer, \
                   otype:string, field:string)
local data, k, ibeg, iend, n, is, ie
begin ; begin of function read_data
   data = new((/sum(nlevel),5/),float)
   data@_FillValue = -888888
   if ( otype .eq. "synop" .or. otype .eq. "metar" .or. otype .eq. "ships" .or. \
        otype .eq. "buoy"  .or. otype .eq. "sonde_sfc" ) then
      if ( field .eq. "u" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "v" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
      if ( field .eq. "t" )  then
         ibeg = (/208,225,242,250,267/)
         iend = (/224,241,249,266,283/)
      end if
      if ( field .eq. "p" )  then
         ibeg = (/284,301,318,326,343/)
         iend = (/300,317,325,342,359/)
      end if
      if ( field .eq. "q" )  then
         ibeg = (/360,377,394,402,419/)
         iend = (/376,393,401,418,435/)
      end if
   end if
   if ( otype .eq. "geoamv" .or. otype .eq. "polaramv" .or.   \
        otype .eq. "pilot"  .or. otype .eq. "profiler" .or.   \
        otype .eq. "qscat"  .or. otype .eq. "bogus" ) then
      if ( field .eq. "u" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "v" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
   end if
   if ( otype .eq. "gpspw" ) then
      if ( field .eq. "tpw" ) then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
   end if
   if ( otype .eq. "ssmir" ) then
      if ( field .eq. "spd" ) then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
   end if
   if ( otype .eq. "gpsref" ) then
      if ( field .eq. "ref" ) then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
   end if
   if ( otype .eq. "airsr" ) then
      if ( field .eq. "t" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "q" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
   end if
   if ( otype .eq. "sound" ) then
      if ( field .eq. "u" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "v" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
      if ( field .eq. "t" )  then
         ibeg = (/208,225,242,250,267/)
         iend = (/224,241,249,266,283/)
      end if
      if ( field .eq. "q" )  then
         ibeg = (/284,301,318,326,343/)
         iend = (/300,317,325,342,359/)
      end if
   end if
   if ( otype .eq. "airep" ) then
      if ( field .eq. "u" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "v" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
      if ( field .eq. "t" )  then
         ibeg = (/208,225,242,250,267/)
         iend = (/224,241,249,266,283/)
      end if
   end if
   if ( otype .eq. "tamdar" ) then
      if ( field .eq. "u" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "v" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
      if ( field .eq. "t" )  then
         ibeg = (/208,225,242,250,267/)
         iend = (/224,241,249,266,283/)
      end if
      if ( field .eq. "q" )  then
         ibeg = (/284,301,318,326,343/)
         iend = (/300,317,325,342,359/)
      end if
   end if
   if ( otype .eq. "radar" ) then
      if ( field .eq. "rv" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "rf" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
   end if

   do n = 0, ndata-1
      if ( nlevel(n) .gt. 0 ) then
         if ( n .eq. 0 ) then
            is = 0
            ie = nlevel(n) - 1
         else
            is = ie + 1
            ie = is + nlevel(n) - 1
         end if
         do k = 0, 4   ; obs, inv, qc, err, inc
            data(is:ie,k) = stringtofloat(charactertostring(cdata(ilevel(n)+1:ilevel(n)+nlevel(n),ibeg(k):iend(k))))
         end do
      end if
   end do
   return(data)
end ; end of function read_data
;plot_impact_divide_by_obs(wks,obs_types,impact_by_obs,start_date,end_date,ndate)
undef("plot_impact_divide_by_obs")
procedure plot_impact_divide_by_obs(wks:graphic, otype:string, all_impact:float, start_date:string, end_date:string, ndate:integer,mainstring:string)
begin
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Start to creat bar plot ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; plot the obs types series for
  res2                            = True
;---Put variable labels on X axis
  res2@trXMinF                     = 0
  res2@trXMaxF                     = dimsizes(otype)
;  res2@trYMinF                     = 0
;  res2@trYMaxF                     = nlevs 
  res2@trYReverse                  = False
  res2@tmXUseBottom                = False
  res2@tmXTMode                    = "Explicit"
  res2@tmXTOn                      = True    ; Turn on Top labels
  res2@tmXTLabelsOn                = True    ; Turn on Top labels
  res2@tmXTLabels                  = otype    ; Turn on bottom labels
  res2@tmXTValues                  = ispan(0,dimsizes(otype)-1,1)
  res2@tmXTLabelFontHeightF        = 0.012   ; Increase font height
  res2@tmXTLabelDeltaF             = 0.1     ; Increase space b/w ticks and labels
  res2@tmXTMajorLengthF            = 0.01
  res2@tmXTLabelAngleF             = 45
  res2@tmXBOn                      = False   ; Turn off top tickmarks
  res2@tmXBLabelsOn                = False    ; Turn on Top labels
  res2@tiYAxisString = "Forecast error contribution (x10000 J/kg)"  ; yaxis string
  res2@tiYAxisFontHeightF          = 0.02
;  res2@tiXAxisString = "Obs Types"  ; xaxis string
  res2@tiXAxisString = "Impact of observation on 24h WRF 60km forecast error - dry energy norm"  ; xaxis string
  res2@tiXAxisFontHeightF          = 0.02
  res2@tiYAxisAngleF               = 90
  res2@xyLineThicknesses           = 2.0               ; make 2nd lines thicker
  res2@xyLineColors                = "blue"          ; change line color
;---Put variable labels on Y axis
;  res2@tmYLMode                    = "Explicit"
;  res2@tmYLOn                      = True   ; Turn off left tickmarks
;  res2@tmYLLabelsOn                = True   ; Turn off left labels
;  res2@tmYLLabelFontHeightF        = 0.015   ; Increase font height
;  res2@tmYLLabelDeltaF             = 0.1     ; Increase space b/w ticks and labels
;  res2@tmYLValues                  = ispan(1,nlevs(0),1)
;  res2@tmYLLabels                  = pressure
  res2@gsnYRefLine           = 0.              ; reference line
  res2@gsnXYBarChart         = True            ; create bar chart
  res2@gsnXYBarChartBarWidth = 0.55                 ; change bar widths
  res2@gsnAboveYRefLineColor = "red"           ; above ref line fill red
  res2@gsnBelowYRefLineColor = "lightblue"          ; below ref line fill blue
  x = new(dimsizes(otype),integer) 
  do i = 0,dimsizes(otype)-1
    x(i) = i
  end do
  res2@tiMainString                = mainstring 
  res2@tiMainFontHeightF           = 0.020
  plot = gsn_csm_xy(wks,x(:),all_impact(:)/10000,res2) 
;  draw(plot)
;  frame(wks)
end ; end of function plot_impact_divide_by_obs

undef("plot_levs_impact")
procedure plot_levs_impact(wks:graphic,mainstring:string, otype:string, vars:string, pressure:float, nlevs:integer, levels_sum:float)
begin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Start to creat bar plot ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  wks = gsn_open_wks("pdf","multi_levs_obs_impact")
  res                             = True
  res@gsnMaximize                 = True    ; Maximize plot in frame
  res@gsnDraw                     = False   ; Don't draw
  res@gsnFrame                    = False   ; Don't advance frame
  res@vpWidthF                    = 0.6     ; Make long and
  res@vpHeightF                   = 0.6     ; narrow
;---Set axes limits. Add extra space for X max.
  res@trXMinF                     = min(levels_sum(:))
  res@trXMaxF                     = max(levels_sum(:))
;  res@tiMainString                = otype+"  "+vars
  res@tiMainString                = "("+vars+")"+" - "+mainstring
  res@tiMainFontHeightF           = 0.015
  res@trYMinF                     = 0
  res@trYMaxF                     = nlevs + 1
  res@tiYAxisString               = "Pressur (hPa)"  ; yaxis string
  res@tiYAxisSide                 = "right"
  res@tiYAxisFontHeightF          = 0.02
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
  res@tmYRMode                    = "Explicit"
  res@tmYRValues                  = ispan(1,nlevs(0),1)
  res@tmYRLabels                  = pressure
;---Put variable labels on X axis
  res@tmXBLabelFontHeightF        = 0.012   ; Increase font height
  res@tmXBLabelDeltaF             = 0.1     ; Increase space b/w ticks and labels
  res@tmXBMajorLengthF            = 0.01
  res@tiXAxisString = "Forecast error contribution (x10000 J/kg)"  ; xaxis string
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
  dum1 = new(nlevs(0),graphic)
  dum2 = new(nlevs(0),graphic)

;---Set some resources for the bars.
  bres                            = True
  bres@gsEdgesOn                  = True      ; Outline the polygons (bars)
  bres@gsEdgeThicknessF           = 2.0
  bres@gsEdgeColor                = "black"
;
;;;; plot the impact for U,V,T,Q, & total
 
  do i=1,nlevs(0)

;---Do longer bar first.
    bres@gsFillColor = "orange4"
    xbar      = (/0,levels_sum(i-1),levels_sum(i-1),0,0/)
    ybar       =(/i-0.25,i-0.25,i+0.25,i+0.25,i-0.25/)
    dum2(i-1) = gsn_add_polygon(wks,plot_without_xgrid,xbar,ybar,bres)
  end do
  draw(plot_with_xgrid)
  draw(plot_without_xgrid)
  frame(wks)
end ; end of function plot_levs_impact

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
  plevs  = (/ (/1005.,995./),(/855.,845/),(/705.,695./),(/505.,495./),\
              (/305.,295./),(/205.,195./),(/105.,95./), \
             (/55.,45./), (/45.,0./)  /)
  
;  plevs    = (/ (/1050.,0./),(/-999.,-999./) /)   ; a (/-999.,-999./) pair is required if only
                                                  ; one bounded pair is desired

  mid_plevs = (plevs(:,0)+plevs(:,1))/2
  plevs = plevs * 100.
  print(plevs)
  plevs@_FillValue = -999.
  nplevs = dimsizes(plevs)

; set height levels for plotting RADAR and GPSREF
; method 1: set a 2-D array of bounded height levels in meters. Recommended for GPSREF
;hlevs  = (/ (/2000., 5000./), (/5000., 8500./) /)
hlevs  = (/ (/0., 25000./), (/-999., -999./) /)  ; (/-999.,-999./) pair is required if only
                                                  ; one bounded pair is desired
;
; method 2: set a 1-D array of fixed height levels in meters
;  hlevs  = (/ 1000., 2000., 3000., 4000., 5000., 6000., 7000., 8000., 10000. /)

  hlevs@_FillValue = -999.
  nhlevs = dimsizes(hlevs)

  print(mid_plevs)
  proc_sound     = True
  proc_tamdar    = True
  proc_geoamv    = False ;True
  proc_polaramv  = False ;True
  proc_airep     = False
  proc_pilot     = False ;True
  proc_profiler  = False ;True
  proc_bogus     = False ;True
  proc_airsr     = False ;True
  proc_gpsref    = False ;True
 


  obs_types = (/"Sound","Tamdar"/)
 
  i_sound        =0
  i_tamdar       =1
  i_geoamv       =2
  i_polaramv     =3
  i_airep        =4
  i_gpsref       =5
  i_synop        =6
  i_metar        =7
  i_sonde_sfc    =8
  i_ships        =9
  i_buoy         =10
  i_gpspw        =11
  i_pilot        =12
  i_profiler     =13
  i_bogus        =14
  i_airsr        =15

  impact_total=new(dimsizes(obs_types),float)  ; total obs types
  impact_total(:)=0.0

  impact_by_obs=new(dimsizes(obs_types),float)  ; total obs types
  impact_by_obs(:)=0.0

  levs_impact_total=new((/dimsizes(obs_types),nplevs(0),5/),float)  ;u,v,t,q & total on each level 
  levs_impact_total(:,:,:)=0.0

  levs_impact_by_obs=new((/dimsizes(obs_types),nplevs(0),5/),float)  ; total obs types
  levs_impact_by_obs(:,:,:)=0.0

  ndate = 0
do while ( date .le. end_date )  ; big date loop

  gts_fullname = datdir1+date+datdir2+filename_gts

  if ( .not. isfilepresent(gts_fullname) ) then ; big loop
     print("Can not find the file "+gts_fullname+"  Will skip it")
  else
     if ( stringtointeger(systemfunc("wc -l "+gts_fullname)) .eq. 0 ) then ; 2nd big loop
        print("no data found in "+gts_fullname+"  Will skip it")
     else
        print("Reading data from "+gts_fullname)
        print("This may take a while......")
        data  = asciiread(gts_fullname, -1, "string")    ; -1 means read all rows.
        cdata = stringtochar(data)
        nline = dimsizes(cdata(:,0))

        nsound     = 0
        ntamdar    = 0
        nairep      = 0
        npilot     = 0
        nprofiler  = 0
        ngpsref    = 0
        ngeoamv    = 0
        npolaramv  = 0
        nbogus     = 0
        nairsr     = 0

        nsynop = 0
        nmetar = 0
        nships = 0
        nbuoy  = 0
        nsonde_sfc = 0
        ngpspw = 0


;;;;;;;;processing sounding data
        if ( proc_sound ) then
           ob_string = "               sound"
           isound = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           print(isound)
           if ( .not.ismissing(isound) ) then
              nsound = stringtointeger(charactertostring(cdata(isound,20:27))) ; number of obs
              print(nsound)
              if ( nsound .gt. 0 ) then
                 nlevel_sound = new(nsound,integer)
                 ilevel_sound = new(nsound,integer)
                 ilevel_sound(0) = isound+1
                 nlevel_sound(0) = stringtointeger(charactertostring(cdata(ilevel_sound(0),0:7)))
                 do k = 1, nsound-1
                   ilevel_sound(k) = ilevel_sound(k-1)+nlevel_sound(k-1)+1
                   nlevel_sound(k) = stringtointeger(charactertostring(cdata(ilevel_sound(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_sound .and. nsound .gt. 0 ) then ;sounding plot
          sound_id  = read_info(nsound, isound, cdata, nlevel_sound, ilevel_sound, "id")
          sound_lat = read_info(nsound, isound, cdata, nlevel_sound, ilevel_sound, "lat")
          sound_lon = read_info(nsound, isound, cdata, nlevel_sound, ilevel_sound, "lon")
          sound_pre = read_info(nsound, isound, cdata, nlevel_sound, ilevel_sound, "pre")
          sound_u   = read_data(nsound, isound, cdata, nlevel_sound, ilevel_sound, "sound", "u")
          sound_v   = read_data(nsound, isound, cdata, nlevel_sound, ilevel_sound, "sound", "v")
          sound_t   = read_data(nsound, isound, cdata, nlevel_sound, ilevel_sound, "sound", "t")
          sound_q   = read_data(nsound, isound, cdata, nlevel_sound, ilevel_sound, "sound", "q")
          levs_sum  = new((/nplevs(0),5/),float)
          nsound_used = new((/nplevs(0),5/),integer)
          levs_sum  = 0.0
          average  = new((/4/),float)
          pres      = sound_pre  
          print(dimsizes(sound_u))
          print(dimsizes(pres))
           do k = 0, nplevs(0)-1
           if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
             u_inv = mask(sound_u(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
;             print("plevs(k,0) ="+plevs(k,0)+"u_inv="+u_inv)
             v_inv = mask(sound_v(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             t_inv = mask(sound_t(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             q_inv = mask(sound_q(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u_qc = mask(sound_u(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_qc = mask(sound_v(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             t_qc = mask(sound_t(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             q_qc = mask(sound_q(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u = mask(u_inv(:), u_qc(:).ge.0,True)
             v = mask(v_inv(:), v_qc(:).ge.0,True)
             t = mask(t_inv(:), t_qc(:).ge.0,True)
             q = mask(q_inv(:), q_qc(:).ge.0,True)
             nsound_used(k,0) = num(.not.ismissing(u)) 
             nsound_used(k,1) = num(.not.ismissing(v)) 
             nsound_used(k,2) = num(.not.ismissing(t)) 
             nsound_used(k,3) = num(.not.ismissing(q)) 
             nsound_used(k,4) = sum(nsound_used(k,0:3))
              
             levs_sum(k,0) = levs_sum(k,0)+sum(u)
             levs_sum(k,1) = levs_sum(k,1)+sum(v)
             levs_sum(k,2) = levs_sum(k,2)+sum(t)
             levs_sum(k,3) = levs_sum(k,3)+sum(q)
             levs_sum(k,4) = sum(levs_sum(k,0:3))
             print(k)
             levs_impact_total(i_sound,k,0) = levs_impact_total(i_sound,k,0)+levs_sum(k,0)
             levs_impact_total(i_sound,k,1) = levs_impact_total(i_sound,k,1)+levs_sum(k,1)
             levs_impact_total(i_sound,k,2) = levs_impact_total(i_sound,k,2)+levs_sum(k,2)
             levs_impact_total(i_sound,k,3) = levs_impact_total(i_sound,k,3)+levs_sum(k,3)
             levs_impact_total(i_sound,k,4) = levs_impact_total(i_sound,k,4)+levs_sum(k,4)

             if (nsound_used(k,0) .ne.0) then
             levs_impact_by_obs(i_sound,k,0) = levs_impact_by_obs(i_sound,k,0)+levs_sum(k,0)/nsound_used(k,0)
             end if
             if (nsound_used(k,1) .ne.0) then
             levs_impact_by_obs(i_sound,k,1) = levs_impact_by_obs(i_sound,k,1)+levs_sum(k,1)/nsound_used(k,1)
             end if
             if (nsound_used(k,2) .ne.0) then
             levs_impact_by_obs(i_sound,k,2) = levs_impact_by_obs(i_sound,k,2)+levs_sum(k,2)/nsound_used(k,2)
             end if
             if (nsound_used(k,3) .ne.0) then
             levs_impact_by_obs(i_sound,k,3) = levs_impact_by_obs(i_sound,k,3)+levs_sum(k,3)/nsound_used(k,3)
             end if
             if (nsound_used(k,4) .ne.0) then
             levs_impact_by_obs(i_sound,k,4) = levs_impact_by_obs(i_sound,k,4)+levs_sum(k,4)/nsound_used(k,4)
             end if

           end if
           end do
           do i = 0, dimsizes(average)-1
             if (sum(nsound_used(:,i)) .ne. 0) then
              average(i) = sum(levs_sum(:,i))/sum(nsound_used(:,i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(nsound_used(:,i)) )
              impact_total(i_sound)=impact_total(i_sound)+sum(levs_sum(:,i)) ; total sum (u+v+t+q)
              impact_by_obs(i_sound)=impact_by_obs(i_sound)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
           end do
        delete([/pres, average, u, v, t, q, levs_sum, u_inv, v_inv, t_inv, q_inv, u_qc, v_qc, t_qc, q_qc/])
        delete([/nsound_used,ilevel_sound, nlevel_sound, sound_id, sound_lat, sound_lon, sound_pre, sound_u, sound_v, sound_t, sound_q/])
        end if ; end of sounding plot
;;;;;;;;End of processing sounding data
;---------------------------------------------------------------------------------------
;;;;;;;;processing tamdar data
        if ( proc_tamdar ) then
           ob_string = "              tamdar"
           itamdar = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           if ( .not.ismissing(itamdar) ) then
              ntamdar = stringtointeger(charactertostring(cdata(itamdar,20:27))) ; number of obs
              print(ntamdar)
              if ( ntamdar .gt. 0 ) then
                 nlevel_tamdar = new(ntamdar,integer)
                 ilevel_tamdar = new(ntamdar,integer)
                 ilevel_tamdar(0) = itamdar+1
                 nlevel_tamdar(0) = stringtointeger(charactertostring(cdata(ilevel_tamdar(0),0:7)))
                 do k = 1, ntamdar-1
                   ilevel_tamdar(k) = ilevel_tamdar(k-1)+nlevel_tamdar(k-1)+1
                   nlevel_tamdar(k) = stringtointeger(charactertostring(cdata(ilevel_tamdar(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_tamdar .and. ntamdar .gt. 0 ) then ;tamdaring plot
          tamdar_id  = read_info(ntamdar, itamdar, cdata, nlevel_tamdar, ilevel_tamdar, "id")
          tamdar_lat = read_info(ntamdar, itamdar, cdata, nlevel_tamdar, ilevel_tamdar, "lat")
          tamdar_lon = read_info(ntamdar, itamdar, cdata, nlevel_tamdar, ilevel_tamdar, "lon")
          tamdar_pre = read_info(ntamdar, itamdar, cdata, nlevel_tamdar, ilevel_tamdar, "pre")
          tamdar_u   = read_data(ntamdar, itamdar, cdata, nlevel_tamdar, ilevel_tamdar, "tamdar", "u")
          tamdar_v   = read_data(ntamdar, itamdar, cdata, nlevel_tamdar, ilevel_tamdar, "tamdar", "v")
          tamdar_t   = read_data(ntamdar, itamdar, cdata, nlevel_tamdar, ilevel_tamdar, "tamdar", "t")
          tamdar_q   = read_data(ntamdar, itamdar, cdata, nlevel_tamdar, ilevel_tamdar, "tamdar", "q")
          ntamdar_used = new((/nplevs(0),5/),integer)
          levs_sum  = new((/nplevs(0),5/),float)
          levs_sum  = 0.0
          average  = new((/4/),float)
          pres      = tamdar_pre  
          print(dimsizes(tamdar_u))
          print(dimsizes(pres))
           do k = 0, nplevs(0)-1
           if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
             u_inv = mask(tamdar_u(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_inv = mask(tamdar_v(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             t_inv = mask(tamdar_t(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             q_inv = mask(tamdar_q(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u_qc = mask(tamdar_u(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_qc = mask(tamdar_v(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             t_qc = mask(tamdar_t(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             q_qc = mask(tamdar_q(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u = mask(u_inv(:), u_qc(:).ge.0,True)
             v = mask(v_inv(:), v_qc(:).ge.0,True)
             t = mask(t_inv(:), t_qc(:).ge.0,True)
             q = mask(q_inv(:), q_qc(:).ge.0,True)
             nn = num(.not.ismissing(u))
             n1 = num(.not.ismissing(u_inv))
             print("k = " +k+" no_qc = "+n1+" qc ="+nn)
             ntamdar_used(k,0) = num(.not.ismissing(u))
             ntamdar_used(k,1) = num(.not.ismissing(v))
             ntamdar_used(k,2) = num(.not.ismissing(t))
             ntamdar_used(k,3) = num(.not.ismissing(q))
             ntamdar_used(k,4) = sum(ntamdar_used(k,0:3))
 
             levs_sum(k,0) = levs_sum(k,0)+sum(u)
             levs_sum(k,1) = levs_sum(k,1)+sum(v)
             levs_sum(k,2) = levs_sum(k,2)+sum(t)
             levs_sum(k,3) = levs_sum(k,3)+sum(q)
             levs_sum(k,4) = sum(levs_sum(k,0:3))
                           
             levs_impact_total(i_tamdar,k,0) = levs_impact_total(i_tamdar,k,0)+levs_sum(k,0)
             levs_impact_total(i_tamdar,k,1) = levs_impact_total(i_tamdar,k,1)+levs_sum(k,1)
             levs_impact_total(i_tamdar,k,2) = levs_impact_total(i_tamdar,k,2)+levs_sum(k,2)
             levs_impact_total(i_tamdar,k,3) = levs_impact_total(i_tamdar,k,3)+levs_sum(k,3)
             levs_impact_total(i_tamdar,k,4) = levs_impact_total(i_tamdar,k,4)+levs_sum(k,4)

             if (ntamdar_used(k,0) .ne.0) then
             levs_impact_by_obs(i_tamdar,k,0) = levs_impact_by_obs(i_tamdar,k,0)+levs_sum(k,0)/ntamdar_used(k,0)
             end if
             if (ntamdar_used(k,1) .ne.0) then
             levs_impact_by_obs(i_tamdar,k,1) = levs_impact_by_obs(i_tamdar,k,1)+levs_sum(k,1)/ntamdar_used(k,1)
             end if
             if (ntamdar_used(k,2) .ne.0) then
             levs_impact_by_obs(i_tamdar,k,2) = levs_impact_by_obs(i_tamdar,k,2)+levs_sum(k,2)/ntamdar_used(k,2)
             end if
             if (ntamdar_used(k,3) .ne.0) then
             levs_impact_by_obs(i_tamdar,k,3) = levs_impact_by_obs(i_tamdar,k,3)+levs_sum(k,3)/ntamdar_used(k,3)
             end if
             if (ntamdar_used(k,4) .ne.0) then
             levs_impact_by_obs(i_tamdar,k,4) = levs_impact_by_obs(i_tamdar,k,4)+levs_sum(k,4)/ntamdar_used(k,4)
             end if
           end if
           end do

        do i = 0, dimsizes(average)-1
             if (sum(ntamdar_used(:,i)) .ne. 0) then
              average(i) = sum(levs_sum(:,i))/sum(ntamdar_used(:,i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(ntamdar_used(:,i)) )
              impact_total(i_tamdar)=impact_total(i_tamdar)+sum(levs_sum(:,i)) ; total sum (u+v+t+q)
              impact_by_obs(i_tamdar)=impact_by_obs(i_tamdar)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
           end do
        delete([/pres, average, u, v, t, q, levs_sum, u_inv, v_inv, t_inv, q_inv, u_qc, v_qc, t_qc, q_qc/])
        delete([/ntamdar_used,ilevel_tamdar, nlevel_tamdar, tamdar_id, tamdar_lat, tamdar_lon, tamdar_pre, tamdar_u, tamdar_v, tamdar_t, tamdar_q/])

        end if ; end of tamdar plot
;;;;;;;;End of processing tamdar data
;------------------------------------------------------------------------------------------------------
;;;;;;;;processing airep data
        if ( proc_airep ) then
           ob_string = "               airep"
           iairep = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           if ( .not.ismissing(iairep) ) then
              nairep = stringtointeger(charactertostring(cdata(iairep,20:27))) ; number of obs
              print(nairep)
              if ( nairep .gt. 0 ) then
                 nlevel_airep = new(nairep,integer)
                 ilevel_airep = new(nairep,integer)
                 ilevel_airep(0) = iairep+1
                 nlevel_airep(0) = stringtointeger(charactertostring(cdata(ilevel_airep(0),0:7)))
                 do k = 1, nairep-1
                   ilevel_airep(k) = ilevel_airep(k-1)+nlevel_airep(k-1)+1
                   nlevel_airep(k) = stringtointeger(charactertostring(cdata(ilevel_airep(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_airep .and. nairep .gt. 0 ) then ;airep plot
          airep_id  = read_info(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "id")
          airep_lat = read_info(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "lat")
          airep_lon = read_info(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "lon")
          airep_pre = read_info(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "pre")
          airep_u   = read_data(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "airep", "u")
          airep_v   = read_data(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "airep", "v")
          airep_t   = read_data(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "airep", "t")
          nairep_used = new((/nplevs(0),4/),integer) ; for U, V, T, and Total=U+V+T
          levs_sum  = new((/nplevs(0),4/),float)
          levs_sum  = 0.0
          average  = new((/3/),float)
          pres      = airep_pre  
          print(dimsizes(airep_u))
          print(dimsizes(pres))
           do k = 0, nplevs(0)-1
           if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
             u_inv = mask(airep_u(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_inv = mask(airep_v(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             t_inv = mask(airep_t(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u_qc = mask(airep_u(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_qc = mask(airep_v(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             t_qc = mask(airep_t(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u = mask(u_inv(:), u_qc(:).ge.0,True)
             v = mask(v_inv(:), v_qc(:).ge.0,True)
             t = mask(t_inv(:), t_qc(:).ge.0,True)
             nn = num(.not.ismissing(u))
             n1 = num(.not.ismissing(u_inv))
             print("k = " +k+" no_qc = "+n1+" qc ="+nn)
             nairep_used(k,0) = num(.not.ismissing(u))
             nairep_used(k,1) = num(.not.ismissing(v))
             nairep_used(k,2) = num(.not.ismissing(t))
             nairep_used(k,3) = sum(nairep_used(k,0:2))
 
             levs_sum(k,0) = levs_sum(k,0)+sum(u)
             levs_sum(k,1) = levs_sum(k,1)+sum(v)
             levs_sum(k,2) = levs_sum(k,2)+sum(t)
             levs_sum(k,3) = sum(levs_sum(k,0:2))
                           
             levs_impact_total(iairep,k,0) = levs_impact_total(iairep,k,0)+levs_sum(k,0)
             levs_impact_total(iairep,k,1) = levs_impact_total(iairep,k,1)+levs_sum(k,1)
             levs_impact_total(iairep,k,2) = levs_impact_total(iairep,k,2)+levs_sum(k,2)
             levs_impact_total(iairep,k,3) = levs_impact_total(iairep,k,3)+levs_sum(k,3)

             levs_impact_by_obs(iairep,k,0) = levs_impact_by_obs(iairep,k,0)+levs_sum(k,0)/nairep_used(k,0)
             levs_impact_by_obs(iairep,k,1) = levs_impact_by_obs(iairep,k,1)+levs_sum(k,1)/nairep_used(k,1)
             levs_impact_by_obs(iairep,k,2) = levs_impact_by_obs(iairep,k,2)+levs_sum(k,2)/nairep_used(k,2)
             levs_impact_by_obs(iairep,k,3) = levs_impact_by_obs(iairep,k,3)+levs_sum(k,3)/nairep_used(k,3)
           end if
           end do

        do i = 0, dimsizes(average)-1
             if (sum(nairep_used(:,i)) .ne. 0) then
              average(i) = sum(levs_sum(:,i))/sum(nairep_used(:,i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(nairep_used(:,i)) )
              impact_total(i_airep)=impact_total(i_airep)+sum(levs_sum(:,i)) ; total sum (u+v+t+q)
              impact_by_obs(i_airep)=impact_by_obs(i_airep)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
           end do
        delete([/pres, average, u, v, t, levs_sum, u_inv, v_inv, t_inv, u_qc, v_qc, t_qc/])
        delete([/nairep_used,ilevel_airep, nlevel_airep, airep_id, airep_lat, airep_lon, airep_pre, airep_u, airep_v, airep_t/])

        end if ; end of airep plot
;;;;;;;;End of processing airep data

;-----------------------------------------------------------------------------------------------------------------
;;;;;;;;processing pilot data
        if ( proc_pilot ) then
           ob_string = "               pilot"
           ipilot = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           if ( .not.ismissing(ipilot) ) then
              npilot = stringtointeger(charactertostring(cdata(ipilot,20:27))) ; number of obs
              print(npilot)
              if ( npilot .gt. 0 ) then
                 nlevel_pilot = new(npilot,integer)
                 ilevel_pilot = new(npilot,integer)
                 ilevel_pilot(0) = ipilot+1
                 nlevel_pilot(0) = stringtointeger(charactertostring(cdata(ilevel_pilot(0),0:7)))
                 do k = 1, npilot-1
                   ilevel_pilot(k) = ilevel_pilot(k-1)+nlevel_pilot(k-1)+1
                   nlevel_pilot(k) = stringtointeger(charactertostring(cdata(ilevel_pilot(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_pilot .and. npilot .gt. 0 ) then ;airep plot
          pilot_id  = read_info(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "id")
          pilot_lat = read_info(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "lat")
          pilot_lon = read_info(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "lon")
          pilot_pre = read_info(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "pre")
          pilot_u   = read_data(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "pilot", "u")
          pilot_v   = read_data(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "pilot", "v")
          npilot_used = new((/nplevs(0),3/),integer) ; for U, V,  and Total=U+V
          levs_sum  = new((/nplevs(0),3/),float)
          levs_sum  = 0.0
          average  = new((/2/),float)
          pres      = pilot_pre  
          print(dimsizes(pilot_u))
          print(dimsizes(pres))
           do k = 0, nplevs(0)-1
           if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
             u_inv = mask(pilot_u(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_inv = mask(pilot_v(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u_qc = mask(pilot_u(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_qc = mask(pilot_v(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u = mask(u_inv(:), u_qc(:).ge.0,True)
             v = mask(v_inv(:), v_qc(:).ge.0,True)
             nn = num(.not.ismissing(u))
             n1 = num(.not.ismissing(u_inv))
             print("k = " +k+" no_qc = "+n1+" qc ="+nn)
             npilot_used(k,0) = num(.not.ismissing(u))
             npilot_used(k,1) = num(.not.ismissing(v))
             npilot_used(k,2) = sum(npilot_used(k,0:1))
 
             levs_sum(k,0) = levs_sum(k,0)+sum(u)
             levs_sum(k,1) = levs_sum(k,1)+sum(v)
             levs_sum(k,2) = sum(levs_sum(k,0:1))
          
             levs_impact_total(ipilot,k,0) = levs_impact_total(ipilot,k,0)+levs_sum(k,0)
             levs_impact_total(ipilot,k,1) = levs_impact_total(ipilot,k,1)+levs_sum(k,1)
             levs_impact_total(ipilot,k,2) = levs_impact_total(ipilot,k,2)+levs_sum(k,2)

             levs_impact_by_obs(ipilot,k,0) = levs_impact_by_obs(ipilot,k,0)+levs_sum(k,0)/npilot_used(k,0)
             levs_impact_by_obs(ipilot,k,1) = levs_impact_by_obs(ipilot,k,1)+levs_sum(k,1)/npilot_used(k,1)
             levs_impact_by_obs(ipilot,k,2) = levs_impact_by_obs(ipilot,k,2)+levs_sum(k,2)/npilot_used(k,2)
                           
           end if
           end do
           do i = 0, dimsizes(average)-1
             if (sum(npilot_used(:,i)) .ne. 0) then
              average(i) = sum(levs_sum(:,i))/sum(npilot_used(:,i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(npilot_used(:,i)) )
              impact_total(i_pilot)=impact_total(i_pilot)+sum(levs_sum(:,i)) ; total sum (u+v+t+q)
              impact_by_obs(i_pilot)=impact_by_obs(i_pilot)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
           end do
        delete([/pres, average, u, v, levs_sum, u_inv, v_inv, u_qc, v_qc/])
        delete([/npilot_used,ilevel_pilot, nlevel_pilot, pilot_id, pilot_lat, pilot_lon, pilot_pre, pilot_u, pilot_v/])

        end if ; end of pilot plot
;;;;;;;;End of processing pilot data
;------------------------------------------------------------------------------------------------------
;;;;;;;;processing profiler data
        if ( proc_profiler ) then
           ob_string = "            profiler"
           iprofiler = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           if ( .not.ismissing(iprofiler) ) then
              nprofiler = stringtointeger(charactertostring(cdata(iprofiler,20:27))) ; number of obs
              print(nprofiler)
              if ( nprofiler .gt. 0 ) then
                 nlevel_profiler = new(nprofiler,integer)
                 ilevel_profiler = new(nprofiler,integer)
                 ilevel_profiler(0) = iprofiler+1
                 nlevel_profiler(0) = stringtointeger(charactertostring(cdata(ilevel_profiler(0),0:7)))
                 do k = 1, nprofiler-1
                   ilevel_profiler(k) = ilevel_profiler(k-1)+nlevel_profiler(k-1)+1
                   nlevel_profiler(k) = stringtointeger(charactertostring(cdata(ilevel_profiler(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_profiler .and. nprofiler .gt. 0 ) then ;profiler plot
          profiler_id  = read_info(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "id")
          profiler_lat = read_info(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "lat")
          profiler_lon = read_info(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "lon")
          profiler_pre = read_info(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "pre")
          profiler_u   = read_data(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "profiler", "u")
          profiler_v   = read_data(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "profiler", "v")
          nprofiler_used = new((/nplevs(0),3/),integer) ; for U, V,  and Total=U+V
          levs_sum  = new((/nplevs(0),3/),float)
          levs_sum  = 0.0
          average  = new((/2/),float)
          pres      = profiler_pre  
          print(dimsizes(profiler_u))
          print(dimsizes(pres))
           do k = 0, nplevs(0)-1
           if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
             u_inv = mask(profiler_u(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_inv = mask(profiler_v(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u_qc = mask(profiler_u(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_qc = mask(profiler_v(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u = mask(u_inv(:), u_qc(:).ge.0,True)
             v = mask(v_inv(:), v_qc(:).ge.0,True)
             nn = num(.not.ismissing(u))
             n1 = num(.not.ismissing(u_inv))
             print("k = " +k+" no_qc = "+n1+" qc ="+nn)
             nprofiler_used(k,0) = num(.not.ismissing(u))
             nprofiler_used(k,1) = num(.not.ismissing(v))
             nprofiler_used(k,2) = sum(nprofiler_used(k,0:1))
 
             levs_sum(k,0) = levs_sum(k,0)+sum(u)
             levs_sum(k,1) = levs_sum(k,1)+sum(v)
             levs_sum(k,2) = sum(levs_sum(k,0:1))
             
             levs_impact_total(iprofiler,k,0) = levs_impact_total(iprofiler,k,0)+levs_sum(k,0)
             levs_impact_total(iprofiler,k,1) = levs_impact_total(iprofiler,k,1)+levs_sum(k,1)
             levs_impact_total(iprofiler,k,2) = levs_impact_total(iprofiler,k,2)+levs_sum(k,2)

             levs_impact_by_obs(iprofiler,k,0) = levs_impact_by_obs(iprofiler,k,0)+levs_sum(k,0)/nprofiler_used(k,0)
             levs_impact_by_obs(iprofiler,k,1) = levs_impact_by_obs(iprofiler,k,1)+levs_sum(k,1)/nprofiler_used(k,1)
             levs_impact_by_obs(iprofiler,k,2) = levs_impact_by_obs(iprofiler,k,2)+levs_sum(k,2)/nprofiler_used(k,2)
                           
           end if
           end do
        do i = 0, dimsizes(average)-1
             if (sum(nprofiler_used(:,i)) .ne. 0) then
              average(i) = sum(levs_sum(:,i))/sum(nprofiler_used(:,i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(nprofiler_used(:,i)) )
              impact_total(i_profiler)=impact_total(i_profiler)+sum(levs_sum(:,i)) ; total sum (u+v+t+q)
              impact_by_obs(i_profiler)=impact_by_obs(i_profiler)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
           end do
        delete([/pres, average, u, v, levs_sum, u_inv, v_inv, u_qc, v_qc/])
        delete([/nprofiler_used,ilevel_profiler, nlevel_profiler, profiler_id, profiler_lat, profiler_lon, profiler_pre, profiler_u, profiler_v/])

        end if ; end of profiler plot

;;;;;;;;End of processing profiler data
;------------------------------------------------------------------------------------------------------
;;;;;;;;processing geoamv data
        if ( proc_geoamv ) then
           ob_string = "              geoamv"
           igeoamv = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           if ( .not.ismissing(igeoamv) ) then
              ngeoamv = stringtointeger(charactertostring(cdata(igeoamv,20:27))) ; number of obs
              print(ngeoamv)
              if ( ngeoamv .gt. 0 ) then
                 nlevel_geoamv = new(ngeoamv,integer)
                 ilevel_geoamv = new(ngeoamv,integer)
                 ilevel_geoamv(0) = igeoamv+1
                 nlevel_geoamv(0) = stringtointeger(charactertostring(cdata(ilevel_geoamv(0),0:7)))
                 do k = 1, ngeoamv-1
                   ilevel_geoamv(k) = ilevel_geoamv(k-1)+nlevel_geoamv(k-1)+1
                   nlevel_geoamv(k) = stringtointeger(charactertostring(cdata(ilevel_geoamv(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_geoamv .and. ngeoamv .gt. 0 ) then ;geoamv plot
          geoamv_id  = read_info(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "id")
          geoamv_lat = read_info(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "lat")
          geoamv_lon = read_info(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "lon")
          geoamv_pre = read_info(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "pre")
          geoamv_u   = read_data(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "geoamv", "u")
          geoamv_v   = read_data(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "geoamv", "v")
          ngeoamv_used = new((/nplevs(0),3/),integer) ; for U, V,  and Total=U+V
          levs_sum  = new((/nplevs(0),3/),float)
          levs_sum  = 0.0
          average  = new((/2/),float)
          pres      = geoamv_pre  
;          print(dimsizes(geoamv_u))
          print(dimsizes(pres))
           do k = 0, nplevs(0)-1
           if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
             u_inv = mask(geoamv_u(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_inv = mask(geoamv_v(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u_qc = mask(geoamv_u(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_qc = mask(geoamv_v(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u = mask(u_inv(:), u_qc(:).ge.0,True)
             v = mask(v_inv(:), v_qc(:).ge.0,True)
             nn = num(.not.ismissing(u))
             n1 = num(.not.ismissing(u_inv))
             print("k = " +k+" no_qc = "+n1+" qc ="+nn)
             ngeoamv_used(k,0) = num(.not.ismissing(u))
             ngeoamv_used(k,1) = num(.not.ismissing(v))
             ngeoamv_used(k,2) = sum(ngeoamv_used(k,0:1))
 
             levs_sum(k,0) = levs_sum(k,0)+sum(u)
             levs_sum(k,1) = levs_sum(k,1)+sum(v)
             levs_sum(k,2) = sum(levs_sum(k,0:1))
             
             levs_impact_total(igeoamv,k,0) = levs_impact_total(igeoamv,k,0)+levs_sum(k,0)
             levs_impact_total(igeoamv,k,1) = levs_impact_total(igeoamv,k,1)+levs_sum(k,1)
             levs_impact_total(igeoamv,k,2) = levs_impact_total(igeoamv,k,2)+levs_sum(k,2)

             levs_impact_by_obs(igeoamv,k,0) = levs_impact_by_obs(igeoamv,k,0)+levs_sum(k,0)/ngeoamv_used(k,0)
             levs_impact_by_obs(igeoamv,k,1) = levs_impact_by_obs(igeoamv,k,1)+levs_sum(k,1)/ngeoamv_used(k,1)
             levs_impact_by_obs(igeoamv,k,2) = levs_impact_by_obs(igeoamv,k,2)+levs_sum(k,2)/ngeoamv_used(k,2)
                           
           end if
           end do
        do i = 0, dimsizes(average)-1
             if (sum(ngeoamv_used(:,i)) .ne. 0) then
              average(i) = sum(levs_sum(:,i))/sum(ngeoamv_used(:,i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(ngeoamv_used(:,i)) )
              impact_total(i_geoamv)=impact_total(i_geoamv)+sum(levs_sum(:,i)) ; total sum (u+v+t+q)
              impact_by_obs(i_geoamv)=impact_by_obs(i_geoamv)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
           end do
        delete([/pres, average, u, v, levs_sum, u_inv, v_inv, u_qc, v_qc /])
        delete([/ngeoamv_used,ilevel_geoamv, nlevel_geoamv, geoamv_id, geoamv_lat, geoamv_lon, geoamv_pre, geoamv_u, geoamv_v/])

        end if ; end of geoamv plot
;;;;;;;;End of processing geoamv data
;------------------------------------------------------------------------------------------------------
;;;;;;;;processing polaramv data
        if ( proc_polaramv ) then
           ob_string = "            polaramv"
           ipolaramv = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           if ( .not.ismissing(ipolaramv) ) then
              npolaramv = stringtointeger(charactertostring(cdata(ipolaramv,20:27))) ; number of obs
              print(npolaramv)
              if ( npolaramv .gt. 0 ) then
                 nlevel_polaramv = new(npolaramv,integer)
                 ilevel_polaramv = new(npolaramv,integer)
                 ilevel_polaramv(0) = ipolaramv+1
                 nlevel_polaramv(0) = stringtointeger(charactertostring(cdata(ilevel_polaramv(0),0:7)))
                 do k = 1, npolaramv-1
                   ilevel_polaramv(k) = ilevel_polaramv(k-1)+nlevel_polaramv(k-1)+1
                   nlevel_polaramv(k) = stringtointeger(charactertostring(cdata(ilevel_polaramv(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_polaramv .and. npolaramv .gt. 0 ) then ;polaramv plot
          polaramv_id  = read_info(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "id")
          polaramv_lat = read_info(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "lat")
          polaramv_lon = read_info(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "lon")
          polaramv_pre = read_info(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "pre")
          polaramv_u   = read_data(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "polaramv", "u")
          polaramv_v   = read_data(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "polaramv", "v")
          npolaramv_used = new((/nplevs(0),3/),integer) ; for U, V,  and Total=U+V
          levs_sum  = new((/nplevs(0),3/),float)
          levs_sum  = 0.0
          average  = new((/2/),float)
          pres      = polaramv_pre  
          print(dimsizes(polaramv_u))
          print(dimsizes(pres))
           do k = 0, nplevs(0)-1
           if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
             u_inv = mask(polaramv_u(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_inv = mask(polaramv_v(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u_qc = mask(polaramv_u(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_qc = mask(polaramv_v(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u = mask(u_inv(:), u_qc(:).ge.0,True)
             v = mask(v_inv(:), v_qc(:).ge.0,True)
             nn = num(.not.ismissing(u))
             n1 = num(.not.ismissing(u_inv))
             print("k = " +k+" no_qc = "+n1+" qc ="+nn)
             npolaramv_used(k,0) = num(.not.ismissing(u))
             npolaramv_used(k,1) = num(.not.ismissing(v))
             npolaramv_used(k,2) = sum(npolaramv_used(k,0:1))
 
             levs_sum(k,0) = levs_sum(k,0)+sum(u)
             levs_sum(k,1) = levs_sum(k,1)+sum(v)
             levs_sum(k,2) = sum(levs_sum(k,0:1))
                           
             levs_impact_total(ipolarmv,k,0) = levs_impact_total(ipolarmv,k,0)+levs_sum(k,0)
             levs_impact_total(ipolarmv,k,1) = levs_impact_total(ipolarmv,k,1)+levs_sum(k,1)
             levs_impact_total(ipolarmv,k,2) = levs_impact_total(ipolarmv,k,2)+levs_sum(k,2)

             levs_impact_by_obs(ipolarmv,k,0) = levs_impact_by_obs(ipolarmv,k,0)+levs_sum(k,0)/npolarmv_used(k,0)
             levs_impact_by_obs(ipolarmv,k,1) = levs_impact_by_obs(ipolarmv,k,1)+levs_sum(k,1)/npolarmv_used(k,1)
             levs_impact_by_obs(ipolarmv,k,2) = levs_impact_by_obs(ipolarmv,k,2)+levs_sum(k,2)/npolarmv_used(k,2)
           end if
           end do
        do i = 0, dimsizes(average)-1
             if (sum(npolaramv_used(:,i)) .ne. 0) then
              average(i) = sum(levs_sum(:,i))/sum(npolaramv_used(:,i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(npolaramv_used(:,i)) )
              impact_total(i_polaramv)=impact_total(i_polaramv)+sum(levs_sum(:,i)) ; total sum (u+v+t+q)
              impact_by_obs(i_polaramv)=impact_by_obs(i_polaramv)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
           end do
        delete([/pres, average, u, v, levs_sum, u_inv, v_inv, u_qc, v_qc/])
        delete([/npolaramv_used,ilevel_polaramv, nlevel_polaramv, polaramv_id, polaramv_lat, polaramv_lon, polaramv_pre, polaramv_u, polaramv_v /])

        end if ; end of polaramv plot
;;;;;;;;End of processing polaramv data
;------------------------------------------------------------------------------------------------------
;;;;;;;;processing bogus data
        if ( proc_bogus ) then
           ob_string = "               bogus"
           ibogus = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           if ( .not.ismissing(ibogus) ) then
              nbogus = stringtointeger(charactertostring(cdata(ibogus,20:27))) ; number of obs
              print(nbogus)
              if ( nbogus .gt. 0 ) then
                 nlevel_bogus = new(nbogus,integer)
                 ilevel_bogus = new(nbogus,integer)
                 ilevel_bogus(0) = ibogus+1
                 nlevel_bogus(0) = stringtointeger(charactertostring(cdata(ilevel_bogus(0),0:7)))
                 do k = 1, nbogus-1
                   ilevel_bogus(k) = ilevel_bogus(k-1)+nlevel_bogus(k-1)+1
                   nlevel_bogus(k) = stringtointeger(charactertostring(cdata(ilevel_bogus(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_bogus .and. nbogus .gt. 0 ) then ;bogus plot
          bogus_id  = read_info(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "id")
          bogus_lat = read_info(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "lat")
          bogus_lon = read_info(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "lon")
          bogus_pre = read_info(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "pre")
          bogus_u   = read_data(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "bogus", "u")
          bogus_v   = read_data(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "bogus", "v")
          nbogus_used = new((/nplevs(0),3/),integer) ; for U, V,  and Total=U+V
          levs_sum  = new((/nplevs(0),3/),float)
          levs_sum  = 0.0
          average  = new((/2/),float)
          pres      = bogus_pre  
          print(dimsizes(bogus_u))
          print(dimsizes(pres))
           do k = 0, nplevs(0)-1
           if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
             u_inv = mask(bogus_u(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_inv = mask(bogus_v(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u_qc = mask(bogus_u(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             v_qc = mask(bogus_v(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             u = mask(u_inv(:), u_qc(:).ge.0,True)
             v = mask(v_inv(:), v_qc(:).ge.0,True)
             nn = num(.not.ismissing(u))
             n1 = num(.not.ismissing(u_inv))
             print("k = " +k+" no_qc = "+n1+" qc ="+nn)
             nbogus_used(k,0) = num(.not.ismissing(u))
             nbogus_used(k,1) = num(.not.ismissing(v))
             nbogus_used(k,3) = sum(nbogus_used(k,0:1))
 
             levs_sum(k,0) = levs_sum(k,0)+sum(u)
             levs_sum(k,1) = levs_sum(k,1)+sum(v)
             levs_sum(k,2) = sum(levs_sum(k,0:1))
                           
           end if
           end do
        do i = 0, dimsizes(average)-1
             if (sum(nbogus_used(:,i)) .ne. 0) then
              average(i) = sum(levs_sum(:,i))/sum(nbogus_used(:,i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(nbogus_used(:,i)) )
              impact_total(i_bogus)=impact_total(i_bogus)+sum(levs_sum(:,i)) ; total sum (u+v+t+q)
              impact_by_obs(i_bogus)=impact_by_obs(i_bogus)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
           end do
        delete([/pres, average, u, v, levs_sum, u_inv, v_inv,  u_qc, v_qc/])
        delete([/nbogus_used,ilevel_bogus, nlevel_bogus, bogus_id, bogus_lat, bogus_lon, bogus_pre, bogus_u, bogus_v/])

        end if ; end of bogus plot
;;;;;;;;End of processing bogus data
;------------------------------------------------------------------------------------------------------
;;;;;;;;processing gpsref data
        if ( proc_gpsref ) then
           ob_string = "              gpsref"
           igpsref = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           if ( .not.ismissing(igpsref) ) then
              ngpsref = stringtointeger(charactertostring(cdata(igpsref,20:27))) ; number of obs
              print(ngpsref)
              if ( ngpsref .gt. 0 ) then
                 nlevel_gpsref = new(ngpsref,integer)
                 ilevel_gpsref = new(ngpsref,integer)
                 ilevel_gpsref(0) = igpsref+1
                 nlevel_gpsref(0) = stringtointeger(charactertostring(cdata(ilevel_gpsref(0),0:7)))
                 do k = 1, ngpsref-1
                   ilevel_gpsref(k) = ilevel_gpsref(k-1)+nlevel_gpsref(k-1)+1
                   nlevel_gpsref(k) = stringtointeger(charactertostring(cdata(ilevel_gpsref(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_gpsref .and. ngpsref .gt. 0 ) then ;gpsref plot
          gpsref_id  = read_info(ngpsref, igpsref, cdata, nlevel_gpsref, ilevel_gpsref, "id")
          gpsref_lat = read_info(ngpsref, igpsref, cdata, nlevel_gpsref, ilevel_gpsref, "lat")
          gpsref_lon = read_info(ngpsref, igpsref, cdata, nlevel_gpsref, ilevel_gpsref, "lon")
          gpsref_hgt = read_info(ngpsref, igpsref, cdata, nlevel_gpsref, ilevel_gpsref, "hgt")
          gpsref_ref = read_data(ngpsref, igpsref, cdata, nlevel_gpsref, ilevel_gpsref, "gpsref", "ref")
          hgt      = gpsref_hgt  
          print(dimsizes(gpsref_ref))
          print(dimsizes(hgt))
          ngpsref_used = new((nhlevs(0)),integer) ; for U, V,  and Total=U+V
          levs_sum  = new((nhlevs(0)),float)
          levs_sum  = 0.0
           do k = 0, nhlevs(0)-1
           if ( .not.ismissing(hlevs(k,0)) .and. .not.ismissing(hlevs(k,1)) ) then
             ref_inv = mask(gpsref_ref(:,1), hgt(:).ge.hlevs(k,0).and.hgt(:).le.hlevs(k,1), True)
             ref_qc = mask(gpsref_ref(:,2), hgt(:).ge.hlevs(k,0).and.hgt(:).le.hlevs(k,1), True)
             ref = mask(ref_inv(:), ref_qc(:).ge.0,True)
             nn = num(.not.ismissing(ref))
             n1 = num(.not.ismissing(ref_inv))
             print("k = " +k+" no_qc = "+n1+" qc ="+nn)
             ngpsref_used(k) = num(.not.ismissing(ref))
             levs_sum(k) = levs_sum(k)+sum(ref)
           end if
           end do

        average = 0.0
        if (sum(ngpsref_used(:)) .ne. 0) then
          average = sum(levs_sum(:))/sum(ngpsref_used(:))
          print("Average: "+ average+"  Obs_number: "+sum(ngpsref_used(:)) )
          impact_total(i_gpsref)=impact_total(i_gpsref)+sum(levs_sum(:)) ; total sum (u+v+t+q)
          impact_by_obs(i_gpsref)=impact_by_obs(i_gpsref)+average ; total sum(u,v,t,q)/obs_number
        end if
        delete([/hgt,average, ref, levs_sum, ref_inv, ref_qc/])
        delete([/ngpsref_used,ilevel_gpsref, nlevel_gpsref, gpsref_id, gpsref_lat, gpsref_lon, gpsref_hgt, gpsref_ref/])

        end if ; end of gpsref plot
;;;;;;;;End of processing gpsref data
;------------------------------------------------------------------------------------------------------
;;;;;;;;processing airsr data
        if ( proc_airsr ) then
           ob_string = "              airsr"
           iairep = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           if ( .not.ismissing(iairsr) ) then
              nairsr = stringtointeger(charactertostring(cdata(iairsr,20:27))) ; number of obs
              print(nairsr)
              if ( nairsr .gt. 0 ) then
                 nlevel_airsr = new(nairsr,integer)
                 ilevel_airsr = new(nairsr,integer)
                 ilevel_airsr(0) = iairsr+1
                 nlevel_airsr(0) = stringtointeger(charactertostring(cdata(ilevel_airsr(0),0:7)))
                 do k = 1, nairsr-1
                   ilevel_airsr(k) = ilevel_airsr(k-1)+nlevel_airsr(k-1)+1
                   nlevel_airsr(k) = stringtointeger(charactertostring(cdata(ilevel_airsr(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_airsr .and. nairsr .gt. 0 ) then ;airsring plot
          airsr_id  = read_info(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "id")
          airsr_lat = read_info(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "lat")
          airsr_lon = read_info(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "lon")
          airsr_pre = read_info(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "pre")
          airsr_t   = read_data(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "airsr", "t")
          airsr_q   = read_data(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "airsr", "q")
          nairsr_used = new((/nplevs(0),3/),integer)
          levs_sum  = new((/nplevs(0),3/),float)
          levs_sum  = 0.0
          average  = new((/2/),float)
          pres      = airsr_pre  
          print(dimsizes(airsr_t))
          print(dimsizes(pres))
           do k = 0, nplevs(0)-1
           if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
             t_inv = mask(airsr_t(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             q_inv = mask(airsr_q(:,1), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             t_qc = mask(airsr_t(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             q_qc = mask(airsr_q(:,2), pres(:).le.plevs(k,0).and.pres(:).ge.plevs(k,1), True)
             t = mask(t_inv(:), t_qc(:).ge.0,True)
             q = mask(q_inv(:), q_qc(:).ge.0,True)
             nn = num(.not.ismissing(u))
             n1 = num(.not.ismissing(u_inv))
             print("k = " +k+" no_qc = "+n1+" qc ="+nn)
             nairsr_used(k,0) = num(.not.ismissing(t))
             nairsr_used(k,1) = num(.not.ismissing(q))
             nairsr_used(k,2) = sum(nairsr_used(k,0:1))
 
             levs_sum(k,0) = levs_sum(k,0)+sum(t)
             levs_sum(k,1) = levs_sum(k,1)+sum(q)
             levs_sum(k,2) = sum(levs_sum(k,0:1))
                           
           end if
           end do
        do i = 0, dimsizes(average)-1
             if (sum(nairsr_used(:,i)) .ne. 0) then
              average(i) = sum(levs_sum(:,i))/sum(nairsr_used(:,i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(nairsr_used(:,i)) )
              impact_total(i_airsr)=impact_total(i_airsr)+sum(levs_sum(:,i)) ; total sum (u+v+t+q)
              impact_by_obs(i_airsr)=impact_by_obs(i_airsr)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
           end do
        delete([/pres, average, u, v, t, levs_sum, u_inv, v_inv, t_inv, u_qc, v_qc, t_qc/])
        delete([/nairsr_used,ilevel_airsr, nlevel_airsr, airsr_id, airsr_lat, airsr_lon, airsr_pre, airsr_u, airsr_v, airsr_t/])

        end if ; end of airsr plot
;;;;;;;;End of processing airsr data
;;;;;;;;processing synop data
        if ( proc_synop ) then
           ob_string = "               synop"
           isynop = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           print(isynop)
           if ( .not.ismissing(isynop) ) then
              nsynop = stringtointeger(charactertostring(cdata(isynop,20:27))) ; number of obs
              print(nsynop)
              if ( nsynop .gt. 0 ) then
                 nlevel_synop = new(nsynop,integer)
                 ilevel_synop = new(nsynop,integer)
                 ilevel_synop(0) = isynop+1
                 nlevel_synop(0) = stringtointeger(charactertostring(cdata(ilevel_synop(0),0:7)))
                 do k = 1, nsynop-1
                   ilevel_synop(k) = ilevel_synop(k-1)+nlevel_synop(k-1)+1
                   nlevel_synop(k) = stringtointeger(charactertostring(cdata(ilevel_synop(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_synop .and. nsynop .gt. 0 ) then ;synoping plot
          synop_id  = read_info(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "id")
          synop_lat = read_info(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "lat")
          synop_lon = read_info(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "lon")
          synop_pre = read_info(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "pre")
          synop_u   = read_data(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "synop", "u")
          synop_v   = read_data(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "synop", "v")
          synop_t   = read_data(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "synop", "t")
          synop_p   = read_data(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "synop", "p")
          synop_q   = read_data(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "synop", "q")
          print(dimsizes(synop_u))
          u = mask(synop_u(:,1), synop_u(:,2).ge.0,True) ;;inovation(obs impact at qc>0)
          v = mask(synop_v(:,1), synop_v(:,2).ge.0,True)
          t = mask(synop_t(:,1), synop_t(:,2).ge.0,True)
          q = mask(synop_q(:,1), synop_q(:,2).ge.0,True)
          p = mask(synop_p(:,1), synop_p(:,2).ge.0,True)
          nn = num(.not.ismissing(u))
          n1 = num(.not.ismissing(synop_u(:,1)))
          print("k = " +k+" no_qc = "+n1+" qc ="+nn)

          levs_sum  = new((/6/),float)
          levs_sum  = 0.0
          nsynop_used = new((/6/),integer)
          average  = new((/5/),float)

          nsynop_used(0) = num(.not.ismissing(u)) 
          nsynop_used(1) = num(.not.ismissing(v)) 
          nsynop_used(2) = num(.not.ismissing(t)) 
          nsynop_used(3) = num(.not.ismissing(q)) 
          nsynop_used(4) = num(.not.ismissing(p)) 
          nsynop_used(5) = sum(nsynop_used(0:4))
          
          levs_sum(0) = levs_sum(0)+sum(u)
          levs_sum(1) = levs_sum(1)+sum(v)
          levs_sum(2) = levs_sum(2)+sum(t)
          levs_sum(3) = levs_sum(3)+sum(q)
          levs_sum(4) = levs_sum(4)+sum(p)
          levs_sum(5) = sum(levs_sum(0:4)) 
              
        do i = 0, dimsizes(average)-1
             if (sum(nsynop_used(i)) .ne. 0) then
              average(i) = sum(levs_sum(i))/sum(nsynop_used(i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(nsynop_used(i)) )
              impact_total(i_synop)=impact_total(i_synop)+sum(levs_sum(i)) ; total sum (u+v+t+q)
              impact_by_obs(i_synop)=impact_by_obs(i_synop)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
        end do
        delete([/average, u, v, t, q,p, levs_sum/])
        delete([/nsynop_used,ilevel_synop, nlevel_synop, synop_id, synop_lat, synop_lon, synop_pre, synop_u, synop_v, synop_t, synop_q,synop_p/])

        end if ; end of synoping plot
;;;;;;;;End of processing synop data
;;;;;;;;processing metar data
        if ( proc_metar ) then
           ob_string = "               metar"
           imetar = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           print(imetar)
           if ( .not.ismissing(imetar) ) then
              nmetar = stringtointeger(charactertostring(cdata(imetar,20:27))) ; number of obs
              print(nmetar)
              if ( nmetar .gt. 0 ) then
                 nlevel_metar = new(nmetar,integer)
                 ilevel_metar = new(nmetar,integer)
                 ilevel_metar(0) = imetar+1
                 nlevel_metar(0) = stringtointeger(charactertostring(cdata(ilevel_metar(0),0:7)))
                 do k = 1, nmetar-1
                   ilevel_metar(k) = ilevel_metar(k-1)+nlevel_metar(k-1)+1
                   nlevel_metar(k) = stringtointeger(charactertostring(cdata(ilevel_metar(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_metar .and. nmetar .gt. 0 ) then ;metaring plot
          metar_id  = read_info(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "id")
          metar_lat = read_info(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "lat")
          metar_lon = read_info(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "lon")
          metar_pre = read_info(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "pre")
          metar_u   = read_data(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "metar", "u")
          metar_v   = read_data(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "metar", "v")
          metar_t   = read_data(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "metar", "t")
          metar_p   = read_data(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "metar", "p")
          metar_q   = read_data(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "metar", "q")
          print(dimsizes(metar_u))
          u = mask(metar_u(:,1), metar_u(:,2).ge.0,True) ;;inovation(obs impact at qc>0)
          v = mask(metar_v(:,1), metar_v(:,2).ge.0,True)
          t = mask(metar_t(:,1), metar_t(:,2).ge.0,True)
          q = mask(metar_q(:,1), metar_q(:,2).ge.0,True)
          p = mask(metar_p(:,1), metar_p(:,2).ge.0,True)
          nn = num(.not.ismissing(u))
          n1 = num(.not.ismissing(metar_u(:,1)))
          print("k = " +k+" no_qc = "+n1+" qc ="+nn)
          levs_sum  = new((/6/),float)
          levs_sum  = 0.0
          nmetar_used = new((/6/),integer)
          average  = new((/5/),float)

          nmetar_used(0) = num(.not.ismissing(u))
          nmetar_used(1) = num(.not.ismissing(v))
          nmetar_used(2) = num(.not.ismissing(t))
          nmetar_used(3) = num(.not.ismissing(q))
          nmetar_used(4) = num(.not.ismissing(p))
          nmetar_used(5) = sum(nmetar_used(0:4))

          levs_sum(0) = levs_sum(0)+sum(u)
          levs_sum(1) = levs_sum(1)+sum(v)
          levs_sum(2) = levs_sum(2)+sum(t)
          levs_sum(3) = levs_sum(3)+sum(q)
          levs_sum(4) = levs_sum(4)+sum(p)
          levs_sum(5) = sum(levs_sum(0:4))

        do i = 0, dimsizes(average)-1
             if (sum(nmetar_used(i)) .ne. 0) then
              average(i) = sum(levs_sum(i))/sum(nmetar_used(i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(nmetar_used(i)) )
              impact_total(i_metar)=impact_total(i_metar)+sum(levs_sum(i)) ; total sum (u+v+t+q)
              impact_by_obs(i_metar)=impact_by_obs(i_metar)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
        end do
        delete([/average, u, v, t, q,p, levs_sum/])
        delete([/nmetar_used,ilevel_metar, nlevel_metar, metar_id, metar_lat, metar_lon, metar_pre, metar_u, metar_v, metar_t, metar_q,metar_p/])

        end if ; end of metar plot
;;;;;;;;End of processing metar data
;;;;;;;;processing sonde_sfc data
        if ( proc_sonde_sfc ) then
           ob_string = "           sonde_sfc"
           isonde_sfc = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           print(isonde_sfc)
           if ( .not.ismissing(isonde_sfc) ) then
              nsonde_sfc = stringtointeger(charactertostring(cdata(isonde_sfc,20:27))) ; number of obs
              print(nsonde_sfc)
              if ( nsonde_sfc .gt. 0 ) then
                 nlevel_sonde_sfc = new(nsonde_sfc,integer)
                 ilevel_sonde_sfc = new(nsonde_sfc,integer)
                 ilevel_sonde_sfc(0) = isonde_sfc+1
                 nlevel_sonde_sfc(0) = stringtointeger(charactertostring(cdata(ilevel_sonde_sfc(0),0:7)))
                 do k = 1, nsonde_sfc-1
                   ilevel_sonde_sfc(k) = ilevel_sonde_sfc(k-1)+nlevel_sonde_sfc(k-1)+1
                   nlevel_sonde_sfc(k) = stringtointeger(charactertostring(cdata(ilevel_sonde_sfc(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_sonde_sfc .and. nsonde_sfc .gt. 0 ) then ;sonde_sfcing plot
          sonde_sfc_id  = read_info(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "id")
          sonde_sfc_lat = read_info(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "lat")
          sonde_sfc_lon = read_info(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "lon")
          sonde_sfc_pre = read_info(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "pre")
          sonde_sfc_u   = read_data(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "sonde_sfc", "u")
          sonde_sfc_v   = read_data(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "sonde_sfc", "v")
          sonde_sfc_t   = read_data(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "sonde_sfc", "t")
          sonde_sfc_p   = read_data(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "sonde_sfc", "p")
          sonde_sfc_q   = read_data(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "sonde_sfc", "q")
          print(dimsizes(sonde_sfc_u))
          u = mask(sonde_sfc_u(:,1), sonde_sfc_u(:,2).ge.0,True) ;;inovation(obs impact at qc>0)
          v = mask(sonde_sfc_v(:,1), sonde_sfc_v(:,2).ge.0,True)
          t = mask(sonde_sfc_t(:,1), sonde_sfc_t(:,2).ge.0,True)
          q = mask(sonde_sfc_q(:,1), sonde_sfc_q(:,2).ge.0,True)
          p = mask(sonde_sfc_p(:,1), sonde_sfc_p(:,2).ge.0,True)
          nn = num(.not.ismissing(u))
          n1 = num(.not.ismissing(sonde_sfc_u(:,1)))
          levs_sum  = new((/6/),float)
          levs_sum  = 0.0
          nsonde_sfc_used = new((/6/),integer)
          average  = new((/5/),float)

          nsonde_sfc_used(0) = num(.not.ismissing(u))
          nsonde_sfc_used(1) = num(.not.ismissing(v))
          nsonde_sfc_used(2) = num(.not.ismissing(t))
          nsonde_sfc_used(3) = num(.not.ismissing(q))
          nsonde_sfc_used(4) = num(.not.ismissing(p))
          nsonde_sfc_used(5) = sum(nsonde_sfc_used(0:4))

          levs_sum(0) = levs_sum(0)+sum(u)
          levs_sum(1) = levs_sum(1)+sum(v)
          levs_sum(2) = levs_sum(2)+sum(t)
          levs_sum(3) = levs_sum(3)+sum(q)
          levs_sum(4) = levs_sum(4)+sum(p)
          levs_sum(5) = sum(levs_sum(0:4))

          do i = 0, dimsizes(average)-1
             if (sum(nsonde_sfc_used(i)) .ne. 0) then
              average(i) = sum(levs_sum(i))/sum(nsonde_sfc_used(i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(nsonde_sfc_used(i)) )
              impact_total(i_sonde_sfc)=impact_total(i_sonde_sfc)+sum(levs_sum(i)) ; total sum (u+v+t+q)
              impact_by_obs(i_sonde_sfc)=impact_by_obs(i_sonde_sfc)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
          end do
         delete([/average, u, v, t, q,p, levs_sum/])
         delete([/nsonde_sfc_used,ilevel_sonde_sfc, nlevel_sonde_sfc, sonde_sfc_id, sonde_sfc_lat, sonde_sfc_lon, sonde_sfc_pre, sonde_sfc_u, sonde_sfc_v, sonde_sfc_t, sonde_sfc_q,sonde_sfc_p/])

        end if ; end of sonde_sfc plot
;;;;;;;;End of processing sonde_sfc data
;;;;;;;;processing ships data
        if ( proc_ships ) then
           ob_string = "               ships"
           iships = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           print(iships)
           if ( .not.ismissing(iships) ) then
              nships = stringtointeger(charactertostring(cdata(iships,20:27))) ; number of obs
              print(nships)
              if ( nships .gt. 0 ) then
                 nlevel_ships = new(nships,integer)
                 ilevel_ships = new(nships,integer)
                 ilevel_ships(0) = iships+1
                 nlevel_ships(0) = stringtointeger(charactertostring(cdata(ilevel_ships(0),0:7)))
                 do k = 1, nships-1
                   ilevel_ships(k) = ilevel_ships(k-1)+nlevel_ships(k-1)+1
                   nlevel_ships(k) = stringtointeger(charactertostring(cdata(ilevel_ships(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_ships .and. nships .gt. 0 ) then ;shipsing plot
          ships_id  = read_info(nships, iships, cdata, nlevel_ships, ilevel_ships, "id")
          ships_lat = read_info(nships, iships, cdata, nlevel_ships, ilevel_ships, "lat")
          ships_lon = read_info(nships, iships, cdata, nlevel_ships, ilevel_ships, "lon")
          ships_pre = read_info(nships, iships, cdata, nlevel_ships, ilevel_ships, "pre")
          ships_u   = read_data(nships, iships, cdata, nlevel_ships, ilevel_ships, "ships", "u")
          ships_v   = read_data(nships, iships, cdata, nlevel_ships, ilevel_ships, "ships", "v")
          ships_t   = read_data(nships, iships, cdata, nlevel_ships, ilevel_ships, "ships", "t")
          ships_p   = read_data(nships, iships, cdata, nlevel_ships, ilevel_ships, "ships", "p")
          ships_q   = read_data(nships, iships, cdata, nlevel_ships, ilevel_ships, "ships", "q")
          print(dimsizes(ships_u))
          u = mask(ships_u(:,1), ships_u(:,2).ge.0,True) ;;inovation(obs impact at qc>0)
          v = mask(ships_v(:,1), ships_v(:,2).ge.0,True)
          t = mask(ships_t(:,1), ships_t(:,2).ge.0,True)
          q = mask(ships_q(:,1), ships_q(:,2).ge.0,True)
          p = mask(ships_p(:,1), ships_p(:,2).ge.0,True)
          nn = num(.not.ismissing(u))
          n1 = num(.not.ismissing(ships_u(:,1)))
          levs_sum  = new((/6/),float)
          levs_sum  = 0.0
          nships_used = new((/6/),integer)
          average  = new((/5/),float)

          nships_used(0) = num(.not.ismissing(u))
          nships_used(1) = num(.not.ismissing(v))
          nships_used(2) = num(.not.ismissing(t))
          nships_used(3) = num(.not.ismissing(q))
          nships_used(4) = num(.not.ismissing(p))
          nships_used(5) = sum(nships_used(0:4))

          levs_sum(0) = levs_sum(0)+sum(u)
          levs_sum(1) = levs_sum(1)+sum(v)
          levs_sum(2) = levs_sum(2)+sum(t)
          levs_sum(3) = levs_sum(3)+sum(q)
          levs_sum(4) = levs_sum(4)+sum(p)
          levs_sum(5) = sum(levs_sum(0:4))

          do i = 0, dimsizes(average)-1
             if (sum(nships_used(i)) .ne. 0) then
              average(i) = sum(levs_sum(i))/sum(nships_used(i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(nships_used(i)) )
              impact_total(i_ships)=impact_total(i_ships)+sum(levs_sum(i)) ; total sum (u+v+t+q)
              impact_by_obs(i_ships)=impact_by_obs(i_ships)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
          end do
          delete([/average, u, v, t, q,p, levs_sum/])
          delete([/nships_used,ilevel_ships, nlevel_ships, ships_id, ships_lat, ships_lon, ships_pre, ships_u, ships_v, ships_t , ships_q,ships_p/])

        end if ; end of ships plot
;;;;;;;;End of processing ships data
;;;;;;;;processing buoy data
        if ( proc_buoy ) then
           ob_string = "                buoy"
           ibuoy = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           print(ibuoy)
           if ( .not.ismissing(ibuoy) ) then
              nbuoy = stringtointeger(charactertostring(cdata(ibuoy,20:27))) ; number of obs
              print(nbuoy)
              if ( nbuoy .gt. 0 ) then
                 nlevel_buoy = new(nbuoy,integer)
                 ilevel_buoy = new(nbuoy,integer)
                 ilevel_buoy(0) = ibuoy+1
                 nlevel_buoy(0) = stringtointeger(charactertostring(cdata(ilevel_buoy(0),0:7)))
                 do k = 1, nbuoy-1
                   ilevel_buoy(k) = ilevel_buoy(k-1)+nlevel_buoy(k-1)+1
                   nlevel_buoy(k) = stringtointeger(charactertostring(cdata(ilevel_buoy(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_buoy .and. nbuoy .gt. 0 ) then ;buoying plot
          buoy_id  = read_info(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "id")
          buoy_lat = read_info(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "lat")
          buoy_lon = read_info(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "lon")
          buoy_pre = read_info(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "pre")
          buoy_u   = read_data(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "buoy", "u")
          buoy_v   = read_data(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "buoy", "v")
          buoy_t   = read_data(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "buoy", "t")
          buoy_p   = read_data(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "buoy", "p")
          buoy_q   = read_data(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "buoy", "q")
          print(dimsizes(buoy_u))
          u = mask(buoy_u(:,1), buoy_u(:,2).ge.0,True) ;;inovation(obs impact at qc>0)
          v = mask(buoy_v(:,1), buoy_v(:,2).ge.0,True)
          t = mask(buoy_t(:,1), buoy_t(:,2).ge.0,True)
          q = mask(buoy_q(:,1), buoy_q(:,2).ge.0,True)
          p = mask(buoy_p(:,1), buoy_p(:,2).ge.0,True)
          nn = num(.not.ismissing(u))
          n1 = num(.not.ismissing(buoy_u(:,1)))
          levs_sum  = new((/6/),float)
          levs_sum  = 0.0
          nbuoy_used = new((/6/),integer)
          average  = new((/5/),float)

          nbuoy_used(0) = num(.not.ismissing(u))
          nbuoy_used(1) = num(.not.ismissing(v))
          nbuoy_used(2) = num(.not.ismissing(t))
          nbuoy_used(3) = num(.not.ismissing(q))
          nbuoy_used(4) = num(.not.ismissing(p))
          nbuoy_used(5) = sum(nbuoy_used(0:4))

          levs_sum(0) = levs_sum(0)+sum(u)
          levs_sum(1) = levs_sum(1)+sum(v)
          levs_sum(2) = levs_sum(2)+sum(t)
          levs_sum(3) = levs_sum(3)+sum(q)
          levs_sum(4) = levs_sum(4)+sum(p)
          levs_sum(5) = sum(levs_sum(0:4))

          do i = 0, dimsizes(average)-1
             if (sum(nbuoy_used(i)) .ne. 0) then
              average(i) = sum(levs_sum(i))/sum(nbuoy_used(i))
              print("Average: "+ average(i)+"  Obs_number: "+sum(nbuoy_used(i)) )
              impact_total(i_buoy)=impact_total(i_buoy)+sum(levs_sum(i)) ; total sum (u+v+t+q)
              impact_by_obs(i_buoy)=impact_by_obs(i_buoy)+average(i) ; total sum(u,v,t,q)/obs_number
             end if
          end do
          delete([/average, u, v, t, q,p, levs_sum/])
         delete([/nbuoy_used,ilevel_buoy, nlevel_buoy, buoy_id, buoy_lat, buoy_lon, buoy_pre, buoy_u, buoy_v, buoy_t , buoy_q,buoy_p/])

        end if ; end of buoying plot
;;;;;;;;End of processing buoy data
;;;;;;;;processing gpspw data
        if ( proc_gpspw ) then
           ob_string = "               gpspw"
           igpspw = ind(chartostring(cdata(:,0:19)).eq.ob_string)
           print(igpspw)
           if ( .not.ismissing(igpspw) ) then
              ngpspw = stringtointeger(charactertostring(cdata(igpspw,20:27))) ; number of obs
              print(ngpspw)
              if ( ngpspw .gt. 0 ) then
                 nlevel_gpspw = new(ngpspw,integer)
                 ilevel_gpspw = new(ngpspw,integer)
                 ilevel_gpspw(0) = igpspw+1
                 nlevel_gpspw(0) = stringtointeger(charactertostring(cdata(ilevel_gpspw(0),0:7)))
                 do k = 1, ngpspw-1
                   ilevel_gpspw(k) = ilevel_gpspw(k-1)+nlevel_gpspw(k-1)+1
                   nlevel_gpspw(k) = stringtointeger(charactertostring(cdata(ilevel_gpspw(k),0:7)))
                 end do
              end if
           end if
        end if

        if ( proc_gpspw .and. ngpspw .gt. 0 ) then ;gpspwing plot
          gpspw_id  = read_info(ngpspw, igpspw, cdata, nlevel_gpspw, ilevel_gpspw, "id")
          gpspw_lat = read_info(ngpspw, igpspw, cdata, nlevel_gpspw, ilevel_gpspw, "lat")
          gpspw_lon = read_info(ngpspw, igpspw, cdata, nlevel_gpspw, ilevel_gpspw, "lon")
          gpspw_tpw = read_data(ngpspw, igpspw, cdata, nlevel_gpspw, ilevel_gpspw, "gpspw", "tpw")
          print(dimsizes(gpspw_tpw))
          tpw = mask(gpspw_tpw(:,1), gpspw_tpw(:,2).ge.0,True) ;;inovation(obs impact at qc>0)
          nn = num(.not.ismissing(tpw))
          n1 = num(.not.ismissing(gpspw_tpw(:,1)))
          n_tpw = num(.not.ismissing(tpw)) 
           if (n_tpw .ne. 0) then
             avg_tpw=sum(tpw)/n_tpw
           end if
           print(n_tpw)
           print(avg_tpw)
           impact_total(i_gpspw)=impact_total(i_gpspw)+sum(tpw)
           impact_by_obs(i_gpspw)=impact_by_obs(i_gpspw)+avg(tpw)
        delete(tpw)
        delete(avg_tpw)
        delete(ilevel_gpspw)
        delete(nlevel_gpspw)
        delete(gpspw_id)
        delete(gpspw_lat)
        delete(gpspw_lon)
        delete(gpspw_tpw)
        end if ; end of gpspwing plot
;;;;;;;;End of processing gpspw data

       delete(data)
       delete(cdata)

    end if ; 2nd big loop
  end if  ; big loop

date = advance_cymdh(date,cycle_period)
ndate = ndate +1
end do  ; end of big date loop
  do n = 0,dimsizes(obs_types)-1
     impact_total(n)=impact_total(n)/ndate  ;; averaget over the the total time
     impact_by_obs(n)=impact_by_obs(n)/ndate  ;; averaget over the the total time and divided by obs number
     levs_impact_total(n,:,:)=levs_impact_total(n,:,:)/ndate
     levs_impact_by_obs(n,:,:)=levs_impact_by_obs(n,:,:)/ndate
  end do
;  wks = gsn_open_wks("pdf","impact_by_obs"+start_date+"-"+end_date)
;  mainstring="JAN 30-Day Average Impact by Instrument Type (18Z) "
;  plot_impact_divide_by_obs(wks,obs_types,impact_total,start_date,end_date,ndate,mainstring)
;  mainstring="JAN 30-Day Average Impact/Obs_num by Instrument Type (18Z) "
;  plot_impact_divide_by_obs(wks,obs_types,impact_by_obs,start_date,end_date,ndate,mainstring)
;;;;;; ploting the vertical level impact
  wks = gsn_open_wks("pdf","impact_by_levs"+start_date+"-"+end_date)
  variables = (/"U","V","T","Q","Total"/)
  do n = 0,dimsizes(obs_types)-1
  do i = 0,dimsizes(variables)-1
     mainstring="JUN 25-Day Average Impact on Vertical Levels (18Z) "
     plot_levs_impact(wks,mainstring, obs_types(n),variables(i),mid_plevs,nplevs,levs_impact_total(n,:,i)/10000.0)
     mainstring="JUN 25-Day Average Impact/Obs_num on Vertical Levels (18Z) "
     plot_levs_impact(wks,mainstring, obs_types(n),variables(i),mid_plevs,nplevs,levs_impact_by_obs(n,:,i)/10000.0)
  end do
  end do


;  frame(wks)
end
