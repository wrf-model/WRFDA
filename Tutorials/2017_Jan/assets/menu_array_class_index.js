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
"http://www2.mmm.ucar.edu/wrf/OnLineTutorial/images/arrow.gif",                  // Sub Menu Image (Leave this blank if not needed)
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
,"Data&Code","show-menu=Data&Code",,,0
,"Feedback ","survey.html",,,0
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
,"Home","index.html",,,0
,"WRFDA Home Page","../..",,,0
,"WRFDA User's Guide","../../usersguide.html",,,1
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
,"Download code","wrfda_code.html",,,1
,"Compiling WRFDA ","compile.html",,,1
,"Download data","wrfda_testdata.html",,,0
])

dumpmenus();
	
