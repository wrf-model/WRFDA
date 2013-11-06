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
"000033",                     // Mouse Off Font Color
"AADDAA",                     // Mouse Off Background Color (use zero for transparent in Netscape 6)
"FFFFFF",                     // Mouse On Font Color
"77AA77",                     // Mouse On Background Color
"FFFFFF",                     // Menu Border Color
"8PT",                        // Font Size (default is px but you can specify mm, pt or a percentage)
"normal",                     // Font Style (italic or normal)
"bold",                       // Font Weight (bold or normal)
"ARIAL,helvetica",            // Font Name
3,                            // Menu Item Padding or spacing
"/wrf/users/images/arrow.gif",                  // Sub Menu Image (Leave this blank if not needed)
0,                            // 3D Border & Separator bar
"ffff00",                     // 3D High Color
"ccffff",                     // 3D Low Color
,                             // Current Page Item Font Color (leave this blank to disable)
,                             // Current Page Item Background Color (leave this blank to disable)
,                             // Top Bar image (Leave this blank to disable)
,                             // Menu Header Font Color (Leave blank if headers are not needed)
,                             // Menu Header Background Color (Leave blank if headers are not needed)
"AADDAA",                     // Menu Item Separator Color
]

PlainStyle2=[                 // PlainStyle is an array of properties. You can have as many property arrays as you need
"ffffff",                     // Mouse Off Font Color
"77AA77",                     // Mouse Off Background Color (use zero for transparent in Netscape 6)
"77AA77",                     // Mouse On Font Color
"ffffff",                     // Mouse On Background Color
"999999",                     // Menu Border Color
"8pt",                         // Font Size (default is px but you can specify mm, pt or a percentage)
"normal",                     // Font Style (italic or normal)
"bold",                       // Font Weight (bold or normal)
"arial,helvetica",          // Font Name
3,                            // Menu Item Padding or spacing
"http://www.mmm.ucar.edu/wrf/OnlineTutorial/images/arrow.gif",                  // Sub Menu Image (Leave this blank if not needed)
0,                            // 3D Border & Separator bar
"ffff00",                     // 3D High Color
"ccffff",                     // 3D Low Color
"337733",                             // Current Page Item Font Color (leave this blank to disable)
"DDFFDD",                             // Current Page Item Background Color (leave this blank to disable)
,                             // Top Bar image (Leave this blank to disable)
,                             // Menu Header Font Color (Leave blank if headers are not needed)
,                             // Menu Header Background Color (Leave blank if headers are not needed)
"eeeeee",                     // Menu Item Separator Color
]

WWE=[                         // WWE is an array of properties. You can have as many property arrays as you need
"ffffff",                     // Mouse Off Font Color
"009966",                     // Mouse Off Background Color (use zero for transparent in Netscape 6)
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
72,								//  top position of menu
,
109,							// width of buttons
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
,"Home","show-menu=Home","index.html",,1
,"Introduction","show-menu=INTRO","Introduction/index.html",,1
,"Tutorials","show-menu=TUT","Introduction/start.html",,1
,"Help","show-menu=HELP","Help/index.html",,1
//,"Graphics","show-menu=Graphics","Graphics/index.html",,1
//,"Tools","show-menu=Tools","Tools/index.html",,1
//,"Data","DATA/index.html",,,1
//,"Feedback","http://survey.ucar.edu/opinio/s?s=3302",,,0
//,"WRF SI","show-menu=SI","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRFSI/index.htm",,0
])

//    MENU ITEM - HOME
addmenu(menu=[
"Home",
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
,"WRFDA Online Tutorial Front Page ","index.html",,,0
,"WRFDA Users Page ","http://www.mmm.ucar.edu/wrf/users/wrfda",,,0
,"ARW Online Tutorial Home ","http://www.mmm.ucar.edu/wrf/OnLineTutorial/index.htm",,,0
,"WRF Model Home Page ","http://www.wrf-model.org",,,0
])


//    MENU ITEM - INTRO
addmenu(menu=[
"INTRO",
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
,"Introduction","Introduction/index.html",,,0
,"Getting Started ","Introduction/start.html",,,0
])


//    MENU ITEM - TUT
addmenu(menu=[
"TUT",
,
,
170,
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
,"3DVAR ","show-menu=3DVAR","3dvar/index.html",,1
//,"3DVAR Advanced ","show-menu=3DVAR_ADV","Compile/3dvar_adv.html",,1
//,"Hybrid ","show-menu=Hybrid","Compile/hybrid.html",,1
//,"4DVAR ","show-menu=4DVAR","Compile/4dvar.html",,1
//,"FSO ","show-menu=FSO","Compile/fso.html",,1
])


addmenu(menu=[
"3DVAR",
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
,"Configure WRFDA ","3dvar/configure.html",,,1
,"Compile WRFDA ","3dvar/compile.html",,,1
,"Test Case ","3dvar/testcase.html",,,0
])

addmenu(menu=[
"3DVAR_ADV",
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
,"Configure WRFDA ","Compile/3dvar_adv_configure.html",,,1
,"Compile WRFDA ","Compile/3dvar_adv_compile.html",,,0
])

addmenu(menu=[
"HYBRID",
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
,"Configure WRFDA ","Compile/hybrid_configure.html",,,1
,"Compile WRFDA ","Compile/hybrid_compile.html",,,0
])

addmenu(menu=[
"4DVAR",
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
,"Configure WRFDA ","Compile/4dvar_configure.html",,,1
,"Compile WRFDA ","Compile/4dvar_compile.html",,,0
])

addmenu(menu=[
"FSO",
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
,"Configure WRFDA ","Compile/fso_configure.html",,,1
,"Compile WRFDA ","Compile/fso_compile.html",,,0
])


//    MENU ITEM - CASES
addmenu(menu=[
"CASES",
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
,"Cases ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/index.html",,,1
,"Default Case ","show-menu=JAN00","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/JAN00/index.html",,0
,"Single Domain Case ","show-menu=d01","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/SingleDomain/index.html",,0
,"Restart Run ","show-menu=RESTART","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/Restart/index.html",,0
,"SST Input ","show-menu=SST","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/SST/index.html",,0
,"Nested Model Runs ","show-menu=NEST","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/index.html",,0
,"Analysis Nudging ","show-menu=FDDA","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/FDDA/index.html",,1
,"Idealized Cases ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/Ideal/index.html",,,0
])



addmenu(menu=[
"JAN00",
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
,"Case","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/JAN00/index.html",,,0
,"geogrid ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/JAN00/geogrid.htm",,,0
,"ungrib ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/JAN00/ungrib.htm",,,0
,"metgrid ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/JAN00/metgrid.htm",,,0
,"wrf ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/JAN00/wrf.htm",,,0
])


addmenu(menu=[
"d01",
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
,"Case","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/SingleDomain/index.html",,,0
,"ungrib ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/SingleDomain/ungrib.htm",,,0
,"geogrid ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/SingleDomain/geogrid.htm",,,0
,"metgrid ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/SingleDomain/metgrid.htm",,,0
,"wrf ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/SingleDomain/wrf.htm",,,0
])

addmenu(menu=[
"RESTART",
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
,"Case","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/Restart/index.html",,,0
,"wrf ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/Restart/wrf.htm",,,0
])

addmenu(menu=[
"NEST",
,
,
200,
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
,"Overview","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/index.html",,,1
,"2-way with 1 input file","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/2way1input.htm",,,0
,"wrf ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/wrf_2way1input.htm",,,1
,"2-way with 2 input files","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/2way2inputs.htm",,,0
,"geogrid ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/geogrid_2way2inputs.htm",,,0
,"metgrid ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/metgrid_2way2inputs.htm",,,0
,"wrf - using all input","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/wrf_2way2inputs.htm",,,0
,"wrf - using only static input","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/wrf_2way2inputs_static.htm",,,1
,"1-way using ndown","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/ndown.htm",,,0
//,"Overview","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/ndown1-1.htm",,,0
,"STEP 1 - geogrid","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/ndown1-2.htm",,,0
,"STEP 1 - metgrid","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/ndown1-3.htm",,,0
,"STEP 2 - wrf for D01","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/ndown2.htm",,,0
,"STEP 3 - real for D02","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/ndown3.htm",,,0
,"STEP 4/5 - ndown and wrf for D02","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/NestRuns/ndown4.htm",,,0
])

addmenu(menu=[
"SST",
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
,"Overview","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/SST/index.html",,,1
,"metgrid - constant SST","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/SST/metgrid.htm",,,1
,"metgrid - varying SST","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/SST/metgrid2.htm",,,0
,"wrf - varying SST","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/SST/wrf.htm",,,0
])

addmenu(menu=[
"FDDA",
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
,"Case","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/FDDA/index.html",,,0
,"wrf ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/CASES/FDDA/wrf.htm",,,0
])





//    MENU ITEM - Graphics
addmenu(menu=[
"Graphics",
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
,"Overview ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/index.html",,,1
,"NCL ","show-menu=NCL","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/NCL/index.html",,0
,"RIP4 ","show-menu=RIP4","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/RIP4/index.html",,0
,"ARWpost ","show-menu=ARWpost","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/ARWpost/index.html",,1
])


addmenu(menu=[
"NCL",
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
,"Overview ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/NCL/index.html",,,0
,"Basics ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/NCL/NCL_basics.htm",,,0
,"Creating Scripts ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/NCL/NCL_scripts.htm",,,0
,"Sample Scripts ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/NCL/NCL_examples.htm",,,1
,"NCL WRF Functions ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/NCL/NCL_functions.htm",,,1
,"FORTRAN in NCL ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/NCL/NCL_Fortran.htm",,,0
])






addmenu(menu=[
"RIP4",
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
,"Overview ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/RIP4/index.html",,,0
,"Code ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/RIP4/start.htm",,,0
,"Environment ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/RIP4/env.htm",,,0
,"Compile ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/RIP4/compile.htm",,,0
,"ripdp ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/RIP4/ripdp.htm",,,0
,"rip namelist ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/RIP4/namelist.htm",,,0
,"rip ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/RIP4/rip.htm",,,0
,"Examples ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/RIP4/Examples/index.html",,,0
])






addmenu(menu=[
"ARWpost",
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
,"Overview ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/ARWpost/index.html",,,0
,"Code ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/ARWpost/start.htm",,,0
,"Environment ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/ARWpost/env.htm",,,0
,"Compile ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/ARWpost/compile.htm",,,0
,"Run ARWpost ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/ARWpost/arwpost.htm",,,0
,"Examples ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Graphics/ARWpost/Examples/index.html",,,0
])





//    MENU ITEM - Tools
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
,"Overview ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Tools/index.html",,,1
,"read_wrf_nc ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Tools/read_wrf_nc.htm",,,0
,"iowrf ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Tools/iowrf.htm",,,0
,"p_interp ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Tools/p_interp.htm",,,0
,"diffwrf ","http://www.mmm.ucar.edu/wrf/users/utilities/diffwrf.html",,,0
,"Data & Graphics ","http://www.mmm.ucar.edu/wrf/OnlineTutorial/Tools/data_file_tools.htm",,,0
])



// //     --------------------------------------------------------------------------------------------------

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
,"Getting Started","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRFSI/index.htm",,,0
,"Source Code","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRFSI/si_code.htm",,,0
,"Environment Variables","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRFSI/si_setenv.htm",,,1
,"Run SI - Manually","show-menu=runSI","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRFSI/si_run1.htm",,0
,"Run SI - with GUI","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRFSI/si_runGUI.htm",,,0
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
,"Step 1 - Localization","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRFSI/si_run1.htm",,,0
,"Step 2 - deGrib Input","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRFSI/si_run2.htm",,,0
,"Step 3 - Interpolation","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRFSI/si_run3.htm",,,0
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
,"General","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRF_real/index.htm",,,0
,"Run real.exe","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRF_real/arw_real_1dom.htm",,,0
,"Run wrf.exe","http://www.mmm.ucar.edu/wrf/OnlineTutorial/WRF_real/arw_wrf_1dom.htm",,,0
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
	
