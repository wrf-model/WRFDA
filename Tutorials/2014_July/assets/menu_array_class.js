/*
 Milonic DHTML Menu
 Written by Andy Woolley
 Copyright 2002 (c) Milonic Solutions. All Rights Reserved.
 Plase vist http://www.milonic.co.uk/menu or e-mail menu3@milonic.com
 You may use this menu on your web site free of charge as long as you place prominent links to http://www.milonic.co.uk/menu and
 your inform us of your intentions with your URL AND ALL copyright notices remain in place in all files including your home page
 Comercial support contracts are available on request if you cannot comply with the above rules.

 Please note that major changes to this file have been made and is not compatible with earlier versions..

 You no longer need to number your menus as in previous versions.
 The new menu structure allows you to name the menu instead. This means that you can remove menus and not break the system.
 The structure should also be much easier to modify, add & remove menus and menu items.

 If you are having difficulty with the menu please read the FAQ at http://www.milonic.co.uk/menu/faq.php before contacting us.

 Please note that the above text CAN be erased if you wish as long as copyright notices remain in place.
*/

//The following line is critical for menu operation, and MUST APPEAR ONLY ONCE.
menunum=0;menus=new Array();_d=document;function addmenu(){menunum++;menus[menunum]=menu;}function dumpmenus(){mt="<script language=JavaScript>";for(a=1;a<menus.length;a++){mt+=" menu"+a+"=menus["+a+"];"}mt+="<\/script>";_d.write(mt)}
//Please leave the above line intact. The above also needs to be enabled if it not already enabled unless you have more than one _array.js file


////////////////////////////////////
// Editable properties START here //
////////////////////////////////////

timegap=300                   // The time delay for menus to remain visible
followspeed=5                 // Follow Scrolling speed
followrate=40                 // Follow Scrolling Rate
suboffset_top=10              // Sub menu offset Top position
suboffset_left=10             // Sub menu offset Left position



PlainStyle=[                  // PlainStyle is an array of properties. You can have as many property arrays as you need
"ebe3de",                     // Mouse Off Font Color
"99BB66",                     // Mouse Off Background Color (use zero for transparent in Netscape 6)
"ffffff",                     // Mouse On Font Color
"99BB66",                     // Mouse On Background Color
"99BB66",                     // Menu Border Color
"12PT",                        // Font Size (default is px but you can specify mm, pt or a percentage)
"normal",                     // Font Style (italic or normal)
"bold",                       // Font Weight (bold or normal)
"georgia, sans-serif",            // Font Name
3,                            // Menu Item Padding or spacing
"/wrf/users/images/arrow.gif",                  // Sub Menu Image (Leave this blank if not needed)
0,                            // 3D Border & Separator bar
"ebe3de",                     // 3D High Color
"ebe3de",                     // 3D Low Color
,                             // Current Page Item Font Color (leave this blank to disable)
,                             // Current Page Item Background Color (leave this blank to disable)
,                             // Top Bar image (Leave this blank to disable)
,                             // Menu Header Font Color (Leave blank if headers are not needed)
,                             // Menu Header Background Color (Leave blank if headers are not needed)
"99BB66",                     // Menu Item Separator Color
]

PlainStyle2=[                 // PlainStyle is an array of properties. You can have as many property arrays as you need
"ffffff",                     // Mouse Off Font Color
"99BB66",                     // Mouse Off Background Color (use zero for transparent in Netscape 6)
"333333",                     // Mouse On Font Color
"ffffff",                     // Mouse On Background Color
"999999",                     // Menu Border Color
"10pt",                         // Font Size (default is px but you can specify mm, pt or a percentage)
"normal",                     // Font Style (italic or normal)
"bold",                       // Font Weight (bold or normal)
"arial,helvetica",          // Font Name
3,                            // Menu Item Padding or spacing
"http://www.mmm.ucar.edu/wrf/OnLineTutorial/images/arrow.gif",                  // Sub Menu Image (Leave this blank if not needed)
0,                            // 3D Border & Separator bar
"ffff00",                     // 3D High Color
"ebe3de",                     // 3D Low Color
"333333",                             // Current Page Item Font Color (leave this blank to disable)
"ffffff",                             // Current Page Item Background Color (leave this blank to disable)
,                             // Top Bar image (Leave this blank to disable)
,                             // Menu Header Font Color (Leave blank if headers are not needed)
,                             // Menu Header Background Color (Leave blank if headers are not needed)
"eeeeee",                     // Menu Item Separator Color
]

WWE=[                         // WWE is an array of properties. You can have as many property arrays as you need
"ffffff",                     // Mouse Off Font Color
"99BB66",                     // Mouse Off Background Color (use zero for transparent in Netscape 6)
"999999",                     // Mouse On Font Color
"ffffff",                     // Mouse On Background Color
"999999",                     // Menu Border Color
"8pt",                          // Font Size (default is px but you can specify mm, pt or a percentage)
"normal",                     // Font Style (italic or normal)
"bold",                       // Font Weight (bold or normal)
"Arial",                      // Font Name
,                             // Menu Item Padding or spacing
"wwelogo4.jpg",               // Sub Menu Image (Leave this blank if not needed)
1,                            // 3D Border & Separator bar
"FF0033",                     // 3D High Color
"000000",                     // 3D Low Color
"FF0033",                     // Current Page Item Font Color (leave this blank to disable)
"0000ff",                     // Current Page Item Background Color (leave this blank to disable)
"wwelogo4.jpg",               // Top Bar image (Leave this blank to disable)
"FF0033",                     // Menu Header Font Color (Leave blank if headers are not needed)
"000000",                     // Menu Header Background Color (Leave blank if headers are not needed)
"FFFFFF",                     // Menu Item Separator Color
]


addmenu(menu=[
"Main",                       // Menu Name - This is needed in order for this menu to be called
0,                            // Menu Top - The Top position of this menu in pixels
25,                           // Menu Left - The Left position of this menu in pixels
397,                          // Menu Width - Menus width in pixels
0,                            // Menu Border Width
,                             // Screen Position - here you can use "center;left;right;middle;top;bottom" or a combination of "center:middle"
WWE,                          // Properties Array - this array is declared higher up as you can see above
0,                            // Always Visible - allows this menu item to be visible at all time (1=on or 0=off)
,                             // Alignment - sets this menu elements text alignment, values valid here are: left, right or center
,                             // Filter - Text variable for setting transitional effects on menu activation - see above for more info
0,                            // Follow Scrolling Top Position - Tells this menu to follow the user down the screen on scroll placing the menu at the value specified.
0,                            // Horizontal Menu - Tells this menu to display horizontaly instead of top to bottom style (1=on or 0=off)
0,                            // Keep Alive - Keeps the menu visible until the user moves over another menu or clicks elsewhere on the page (1=on or 0=off)
,                             // Position of TOP sub image left:center:right
,                             // Set the Overall Width of Horizontal Menu to specified width or 100% and height to a specified amount
0,                            // Right To Left - Used in Hebrew for example. (1=on or 0=off)
0,                            // Open the Menus OnClick - leave blank for OnMouseover (1=on or 0=off)
,                             // ID of the div you want to hide on MouseOver (useful for hiding form elements)
,                             // Background image for menu Color must be set to transparent for this to work
0,                            // Scrollable Menu
,                             // Miscellaneous Menu Properties
])


// START OF MENUS   


addmenu(menu=[
"Mainmenu",
134,								//  top position of menu
4,
160,							// width of buttons
,
"left",							// left, center, right postition
PlainStyle,
1,
"center",
,
0,
1,
0,
,
,
0,
0,
,
,
0,
,
,"         "," ",,,0
,"Helpful links","show-menu=helpful",,,0
,"Exercises","show-menu=Exercises",,,0
,"Data&Code","show-menu=Data&Code",,,0
,"Feedback ","../survey.html",,,0
,"WRF Basic","http://www.mmm.ucar.edu/wrf/OnLineTutorial/Class/index.html",,,0
])


addmenu(menu=[
"helpful",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
,
,"Home","../index.html",,,0
,"WRFDA Home Page","../../..",,,0
,"WRFDA User's Guide","../../../usersguide.html",,,0
,"Tutorial schedule","../Tutorial_Agenda.html",,,1
])


addmenu(menu=[
"Data&Code",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
, 
,"Download code","../class/wrfda_code.html",,,1
,"Compiling WRFDA ","../class/compile.html",,,1
,"Download data","../class/wrfda_testdata.html",,,1
])


addmenu(menu=[
"Tutorial Case",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
, 
,"Overview ","../class/wrfda_sessions.html",,,1
,"OBSPROC ","../class/obsproc.html",,,1
,"WRFVar-Real ","../class/3dvar.html",,,1
,"GEN_BE ","../class/genbe.html",,,1
,"WRFVar-PSOT ","../class/psot.html",,,1
,"Radiance ","../class/radiance.html",,,0
])

addmenu(menu=[
"Exercises",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
, 
,"Overview ","../class/wrfda_sessions.html",,,1
,"Basic ","show-menu=wrfda-basic","../class/wrfda_sessions.html#session_basic",,1
,"Optional ","show-menu=wrfda-optional","../class/wrfda_sessions.html#session_optional",,1
,"Advanced ","show-menu=wrfda-advanced","../class/wrfda_sessions.html#session_advanced",,1
])

addmenu(menu=[
"wrfda-basic",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
, 
,"OBSPROC ","../class/obsproc.html",,,1
,"3DVAR-realdata","../class/3dvar.html",,,1
,"3DVAR-PSOT ","../class/psot.html",,,1
,"GEN_BE ","../class/genbe.html",,,1
,"Compiling WRFDA ","../class/compile.html",,,0
])

addmenu(menu=[
"wrfda-optional",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
, 
,"Radiance DA ","../class/radiance.html",,,1
,"4DVAR ","../class/4dvar.html",,,1
,"Hybrid DA ","../class/hybrid.html",,,1
,"Radar DA ","../class/radar.html",,,1
,"Rainfall DA ","../class/rainfall.html",,,0
])

addmenu(menu=[
"wrfda-advanced",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
, 
,"OBSPROC ","../class/obsproc.html#advance",,,1
,"3DVAR-realdata ","../class/3dvar.html#advance",,,1
,"Radiance ","../class/radiance.html#advance",,,1
,"4D-VAR ","../class/4dvar.html#advance",,,1
,"WRFDA/WRF Cycling ","../class/cycling.html",,,1
,"FSO ","../class/fso.html",,,0
])

//    MENU ITEM - ARW
addmenu(menu=[
"Feedback",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
, 
,"Overview ","http://www.mmm.ucar.edu/wrf/OnLineTutorial/Class/arw_overview.htm",,,1
//,"Configure WRF-ARW ","http://www.mmm.ucar.edu/wrf/OnLineTutorial/Class/configure_wrf.htm",,,1
//,"Compile for Real case","http://www.mmm.ucar.edu/wrf/OnLineTutorial/Class/compile_real.htm",,,0
//,"Configure & Compile WPS ","http://www.mmm.ucar.edu/wrf/OnLineTutorial/Class/configure_compile_wps.htm",,,1
//,"Compile for Idealized cases","http://www.mmm.ucar.edu/wrf/OnLineTutorial/Class/compile_ideal.htm",,,1
,"Case Studies ","http://www.mmm.ucar.edu/wrf/OnLineTutorial/Class/wps_arw.htm",,,0
])


//    MENU ITEM - SI
addmenu(menu=[
"SI",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
,
,"Getting Started","http://www.mmm.ucar.edu/wrf/OnLineTutorial/WRFSI/index.htm",,,0
,"Source Code","http://www.mmm.ucar.edu/wrf/OnLineTutorial/WRFSI/si_code.htm",,,0
,"Environment Variables","http://www.mmm.ucar.edu/wrf/OnLineTutorial/WRFSI/si_setenv.htm",,,1
,"Run SI - Manually","show-menu=runSI","http://www.mmm.ucar.edu/wrf/OnLineTutorial/WRFSI/si_run1.htm",,0
,"Run SI - with GUI","http://www.mmm.ucar.edu/wrf/OnLineTutorial/WRFSI/si_runGUI.htm",,,0
])

addmenu(menu=[
"runSI",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
,
,"Step 1 - Localization","http://www.mmm.ucar.edu/wrf/OnLineTutorial/WRFSI/si_run1.htm",,,0
,"Step 2 - deGrib Input","http://www.mmm.ucar.edu/wrf/OnLineTutorial/WRFSI/si_run2.htm",,,0
,"Step 3 - Interpolation","http://www.mmm.ucar.edu/wrf/OnLineTutorial/WRFSI/si_run3.htm",,,0
])


//    MENU ITEM - ARW Real
addmenu(menu=[
"ARW_Real",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
,
,"General","http://www.mmm.ucar.edu/wrf/OnLineTutorial/WRF_real/index.htm",,,0
,"Run real.exe","http://www.mmm.ucar.edu/wrf/OnLineTutorial/WRF_real/arw_real_1dom.htm",,,0
,"Run wrf.exe","http://www.mmm.ucar.edu/wrf/OnLineTutorial/WRF_real/arw_wrf_1dom.htm",,,0
])

addmenu(menu=[
"ARW_Ideal",
,
,
190,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
,
//,"Pubs & Docs Overview","http://www.mmm.ucar.edu/wrf/users/pub-doc.html",,,0
//,"WRF dynamics","http://www.mmm.ucar.edu/wrf/users/docs/wrf-dyn.html",,,0
//,"WRF physics document","http://www.mmm.ucar.edu/wrf/users/docs/wrf-phy.html",,,0
//,"Tutorial Presentation","http://www.mmm.ucar.edu/wrf/tutorial/2003/tutorial-2003.html",,,1
])


addmenu(menu=[
"Graphics",
,
,
210,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
,
//,"WRF Links Overview","http://www.mmm.ucar.edu/wrf/users/links.html",,,0
//,"NCAR Graphics","http://ngwww.ucar.edu/",,,0
//,"NCL example page","http://www.cgd.ucar.edu/csm/support/CSM_Graphics/index_bycat.shtml",,,1
//,"NCAR SCD","show-menu=SCD",,,1
//,"MMM Web Site","http://www.mmm.ucar.edu/",,,0
//,"NCAR Web Site","http://www.ncar.ucar.edu/ncar/",,,0
//,"UCAR Web Site","http://www.ucar.edu/ucar/",,,0
])


addmenu(menu=[
"Tools",
,
,
160,
1,
,
PlainStyle2,
0,
,
"Fade(duration=0.5);Shadow(color=777777, Direction=135, Strength=5)",
0,
0,
0,
,
,
0,
0,
,
,
0,
,
//,"Forecast Overview","http://www.mmm.ucar.edu/wrf/users/forecasts.html",,,0
//,"MMM Real-time WRF","http://rain.mmm.ucar.edu/wrf/",,,0
//,"Other Sites","http://rain.mmm.ucar.edu/mm5/pages/sites.html",,,0
])

dumpmenus();
	
