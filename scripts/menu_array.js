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
"009933",                     // Mouse Off Background Color (use zero for transparent in Netscape 6)
"FFFFFF",                     // Mouse On Font Color
"009933",                     // Mouse On Background Color
"FFFFFF",                     // Menu Border Color
"8PT",                        // Font Size (default is px but you can specify mm, pt or a percentage)
"normal",                     // Font Style (italic or normal)
"bold",                       // Font Weight (bold or normal)
"ARIAL,helvetica",            // Font Name
3,                            // Menu Item Padding or spacing
"../images/arrow.gif",                  // Sub Menu Image (Leave this blank if not needed)
0,                            // 3D Border & Separator bar
"ffff00",                     // 3D High Color
"ccffff",                     // 3D Low Color
,                             // Current Page Item Font Color (leave this blank to disable)
,                             // Current Page Item Background Color (leave this blank to disable)
,                             // Top Bar image (Leave this blank to disable)
,                             // Menu Header Font Color (Leave blank if headers are not needed)
,                             // Menu Header Background Color (Leave blank if headers are not needed)
"009933",                     // Menu Item Separator Color
]

PlainStyle2=[                 // PlainStyle is an array of properties. You can have as many property arrays as you need
"ffffff",                     // Mouse Off Font Color
"003300",                     // Mouse Off Background Color (use zero for transparent in Netscape 6)
"999999",                     // Mouse On Font Color
"ffffff",                     // Mouse On Background Color
"999999",                     // Menu Border Color
"8pt",                         // Font Size (default is px but you can specify mm, pt or a percentage)
"normal",                     // Font Style (italic or normal)
"bold",                       // Font Weight (bold or normal)
"arial,helvetica",          // Font Name
3,                            // Menu Item Padding or spacing
"../images/arrow.gif",                  // Sub Menu Image (Leave this blank if not needed)
0,                            // 3D Border & Separator bar
"ffff00",                     // 3D High Color
"ccffff",                     // 3D Low Color
,                             // Current Page Item Font Color (leave this blank to disable)
,                             // Current Page Item Background Color (leave this blank to disable)
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


addmenu(menu=[
"Join",
0,
25,
397,
0,
,
WWE,
0,
,
,
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
])

addmenu(menu=[
"Mainmenu",
72,								//  top position of menu
,
100,							// width of buttons
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
,"<div style='margin-top:6px'>Home</div>","show-menu=Home",,,1
,"<div style='margin-top:6px'>System</div>","show-menu=Analysis",,,1
,"<div style='margin-top:6px'>User Support</div>","show-menu=Support",,,1
,"<div style='margin-top:6px'>Download</div>","show-menu=Download",,,1
,"Publications & Documentation","show-menu=Pub",,,1
,"<div style='margin-top:6px'>Links</div>","show-menu=Links",,,1
,"<div style='margin-top:6px'>Internal</div>","http://www.mmm.ucar.edu/wrf/users/wrfda/internal.html",,,1
])


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
//,"WRF Main Home Page","http://www.wrf-model.org",,,0
,"WRFDA Users Home Page","http://www.mmm.ucar.edu/wrf/users/wrfda/index.html",,,0
,"WRFDA News","http://www.mmm.ucar.edu/wrf/users/wrfda/news.html",,,0
,"Public Domain Notice","http://www.mmm.ucar.edu/wrf/users/wrfda/public.html",,,0
,"Contact Us!","http://www.mmm.ucar.edu/wrf/users/wrfda/contact.html",,,0
])


addmenu(menu=[
"Analysis",
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
,"System Overview","http://www.mmm.ucar.edu/wrf/users/wrfda/system.html",,,1
,"WRFDA V3.6.1","http://www.mmm.ucar.edu/wrf/users/wrfda/updates-3.6.1.html",,,0
,"WRFDA V3.6","http://www.mmm.ucar.edu/wrf/users/wrfda/updates-3.6.html",,,0
,"WRFDA V3.5.1","http://www.mmm.ucar.edu/wrf/users/wrfda/updates-3.5.1.html",,,0
,"WRFDA V3.5","http://www.mmm.ucar.edu/wrf/users/wrfda/updates-3.5.html",,,0
,"WRFDA V3.4.1","http://www.mmm.ucar.edu/wrf/users/wrfda/updates-3.4.1.html",,,0
,"WRFDA V3.4","http://www.mmm.ucar.edu/wrf/users/wrfda/updates-3.4.html",,,0
,"WRFDA V3.3.1","http://www.mmm.ucar.edu/wrf/users/wrfda/updates-3.3.1.html",,,0
,"WRFDA V3.3","http://www.mmm.ucar.edu/wrf/users/wrfda/updates-3.3.html",,,0
,"WRFDA V3.2.1","http://www.mmm.ucar.edu/wrf/users/wrfda/updates-3.2.1.html",,,0
,"WRFDA V3.2","http://www.mmm.ucar.edu/wrf/users/wrfda/updates-3.2.html",,,0
,"Older versions","http://www.mmm.ucar.edu/wrf/users/wrfda/updates-older.html",,,1
,"WRFDA Tutorials","http://www.mmm.ucar.edu/wrf/users/wrfda/tutorial.html",,,1
,"WRFDA Tools","http://www.mmm.ucar.edu/wrf/users/wrfda/download/tools.html",,,0
])

addmenu(menu=[
"Support",
,
,
180,
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
,"WRFDA community support","http://www.mmm.ucar.edu/wrf/users/wrfda/support.html",,,0
,"System requirements","http://www.mmm.ucar.edu/wrf/users/wrfda/requirements.html",,,0
,"Known problems","http://www.mmm.ucar.edu/wrf/users/wrfda/known-prob.html",,,0
,"wrfhelp","http://www.mmm.ucar.edu/wrf/users/wrfda/wrfhelp.html",,,0
,"wrf-news","http://www.mmm.ucar.edu/wrf/users/wrfda/wrfnews.html",,,0
,"wrf-users","http://www.mmm.ucar.edu/wrf/users/wrfda/wrfusers.html",,,0
,"Become a registered user","http://www.mmm.ucar.edu/wrf/users/wrfda/regist.html",,,1
,"Workshop","http://www.mmm.ucar.edu/wrf/users/wrfda/workshop.html",,,0
,"Tutorial","http://www.mmm.ucar.edu/wrf/users/wrfda/tutorial.html",,,1
,"WRFDA User's Guide","show-menu=userguide",,,0
//,"Tutorial","show-menu=Tutorials",,,0
//,"On-Line tutorial","http://www.mmm.ucar.edu/mm5/mm5v3/tutorial/teachyourself.html target=new",,,1
//,"Users information","http://www.mmm.ucar.edu/mm5/institutes.html",,,0
])


addmenu(menu=[
"Pub",
100,
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
,"WRFDA Publications","http://www.mmm.ucar.edu/wrf/users/wrfda/pub-doc.html",,,1
,"Technical Notes","http://www.mmm.ucar.edu/wrf/users/wrfda/technotes.html",,,1
,"WRFDA User's Guide","show-menu=userguide",,,0
,"WRFDA Online Tutorial","http://www.mmm.ucar.edu/wrf/users/wrfda/OnlineTutorial/index.html",,,1
,"How to compile WRFDA","http://www.mmm.ucar.edu/wrf/users/wrfda/how_to_compile.html",,,1
,"WRFDA on Yellowstone","http://www.mmm.ucar.edu/wrf/users/wrfda/yellowstone.html",,,1
,"Reading BUFR files","http://www.mmm.ucar.edu/wrf/users/wrfda/readBUFR.html",,,1
//,"Tutorial Presentation","http://www.mmm.ucar.edu/wrf/users/wrfda/tutorial.html",,,0
//,"NCAR Tech Notes","http://www.mmm.ucar.edu/wrf/users/doc1.html",,,0

])

addmenu(menu=[
"userguide",
,
,
120,
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
,"WRFDA V3.6.1","http://www.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.6.1/WRFDA_Users_Guide.html",,,0
,"WRFDA V3.6","http://www.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.6/WRFDA_Users_Guide.html",,,0
,"WRFDA V3.5.1","http://www.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.5.1/WRFDA_Users_Guide.html",,,0
,"WRFDA V3.5","http://www.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.5/WRFDA_Users_Guide.html",,,0
,"WRFDA V3.4.1","http://www.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.4.1/users_guide_chap6.htm",,,0
,"WRFDA V3.4","http://www.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.4/users_guide_chap6.htm",,,0
,"WRFDA V3.3","http://www.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.3/users_guide_chap6.htm",,,0
,"WRFDA V3.2.1","http://www.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.2.1/users_guide_chap6.htm",,,0
,"WRFDA V3.2","http://www.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.2/users_guide_chap6.htm",,,0
,"WRFDA V3.1.1","http://www.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.1.1/users_guide_chap6.htm",,,0
,"WRFDA V3.1","http://www.mmm.ucar.edu/wrf/users/wrfda/Docs/user_guide_V3.1/users_guide_chap6.htm",,,0
])


addmenu(menu=[
"Links",
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
,"WRFDA Links Overview","http://www.mmm.ucar.edu/wrf/users/wrfda/links.html",,,1
,"NCAR Graphics","http://ngwww.ucar.edu/",,,0
,"NCAR Command Language (NCL)","http://www.ncl.ucar.edu/",,,1
,"Unidata (netCDF)","http://www.unidata.ucar.edu/",,,1
,"CISL NCAR Computing Support","http://www2.cisl.ucar.edu/cisl-support",,,1
,"MMM website","http://www.mmm.ucar.edu/",,,0
,"NCAR website","http://www.ncar.ucar.edu/ncar/",,,0
,"UCAR website","http://www.ucar.edu/ucar/",,,0
])

addmenu(menu=[
"Download",
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
,"Downloads Overview","http://www.mmm.ucar.edu/wrf/users/wrfda/downloads.html",,,1
,"WRFDA","http://www.mmm.ucar.edu/wrf/users/wrfda/download/get_source.html",,,1
,"WRFPLUS","http://www.mmm.ucar.edu/wrf/users/wrfda/download/wrfplus.html",,,1
,"Test Data","http://www.mmm.ucar.edu/wrf/users/wrfda/download/testdata.html",,,1
,"FSO","http://www.mmm.ucar.edu/wrf/users/wrfda/download/fso.html",,,1
,"MADIS2LITTLER","http://www.mmm.ucar.edu/wrf/users/wrfda/download/madis.html",,,1
,"TOOLS","http://www.mmm.ucar.edu/wrf/users/wrfda/download/tools.html",,,1
,"Free Input Data","http://www.mmm.ucar.edu/wrf/users/wrfda/download/free_data.html",,,0
])


dumpmenus();
	
