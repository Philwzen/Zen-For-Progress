/* control.i */
/* pre processor definitions for use in getctrl */
/* Zen Specific stuff */
/* need pre DB conection */
/* need pre DB conection */
&if defined(ini-filename) = 0 &then
&glob ini-filename                  zen.ini
&glob ini-path                      {&core}
&endif
/* control file definitions */
&glob clv                               ClientVar
&glob LoadLibsAsSupers                  false
&glob ActivityLog    			ActivityLog
&glob AccountsFieldList			AccountsFieldList
&glob AsyncCurrentJobs AsyncCurrentJobs
&glob AsyncMaxJobs    AsyncMaxJobs
&glob AuditSystemStatus			AuditSystemStatus
&glob AuditExcludeList			AuditExcludeList
&glob AutoKeyGen			AutoKeyGen
&glob Data-Extract-Dir              Data-Extracts-Dir
&glob msb if getsysvar('{&clv}user') = 'phil' then message  
&glob dbt view-as alert-box title 'Debug in ' + program-name(1)
&glob SonicBroker SonicBroker
&glob debug-mode                    debug
&glob menu-grey                     menu-grey
&glob exitcheck                     exitcheck
&glob general-icon                  general-icon
&glob Change-Title                  Change-Title
&glob CompanyTitle                  CompanyTitle
&glob CheckReadonly 		    CheckReadonly
&glob DisabledTextColour            DisabledTextColour
&glob ExcludeFromDefaultsID 999999
&glob InputField		    InputField
&glob LookupHighlight		    LookupHighlight	
&glob editablefield                 editableField
&glob displayfield		    displayfield
&glob screenfontnumber              screenfontnumber
&glob screenfontname                screenfontname
&glob SystemPropath	    	    SystemPropath
&glob PrinterFontNumber		    PrinterFontNumber
&glob Opsystype				opsys		
&glob centre-windows                centre-windows
&glob char-run-once                 char-run-once
&glob Store-code	             Store-code
&glob doc-save-path                 doc-save-path
&glob WordTemplatePath	            WordTemplatePath
&glob ScratchPath			ScratchPath
&glob image-save-path               image-save-path
&glob image-system-pf               image-system-pf
&glob LogMessages		    LogMessages
&glob LogAppserver              LogAppserver
&glob pause-on-error                pause-on-error
&glob lanquage                      eng
&glob Required-OCXs                 Required-OCXs
&glob read-only	                  read-only
&glob maxfavourites                 maxfavourites
&glob uselookupbuttons			uselookupbuttons
&glob SystemDynamic                 SystemDynamic
&glob SystemVersion                 SystemVersion
&glob ZenVersion                    ZenVersion
&glob PopulateLabels                PopulateLabels
&glob MainMenuProgram	            MainMenuProgram
&glob HelpExtension                 HelpExtension
&glob centerbuts			      centerbuts
&glob systemname				systemname
&glob systemMode				sys-mode
&glob defbuttons 				defbutlist
&glob defbuthoriz				defbuthoriz
&glob defbutx 				defbutx
&glob defbuty 				defbuty
&glob defbutwidth 			defbutwidth
&glob defbutheight 			defbutheight
&glob TabExtraInfo		    ''
&glob winoffsetx	winoffsetx
&glob winoffsety	winoffsety
/* not used */
&glob linktype  Linktype
&glob addrtype  addrtype
&glob debug-file	                  debug-file
&glob large-icon-path               large-icon-path
&glob progress-userids              progress-userids
&glob Proper-Form                   Proper-Form
&glob security-on                   security-on
&glob Spoolpath                     Spoolpath
&glob status-bar                    status-bar

/* end not used */

&glob usezenmenus 		    true
&glob UseProPrint                   UseProPrint

/* country codes */
&glob uk        44
&GLOB deu       49 

/* General Text Strings */
&if "{&OPSYS}" = "unix" 
    &then &glob DirDelim \
    &else &glob DirDelim /
&endif

&glob LookupDelim ' '
&glob all-text                  All
&glob Null-String			  ?,''
&glob none-text                 None
&glob select-text               Please Select
&glob log-true			  yes,True,Y,Ok,Pass,1
&Glob log-False			  no,False,N,Fail,0
&glob BackSlash             chr(92)
&glob crlf		    chr(13) + chr(10)
&Glob slash                 che(47)
&glob ComboDelim                ^
&glob Delim2        |
&glob Delim3        @
&glob Delim4	:

/* zen button defaults */
&glob defbutlist 	New^ttf,Edit^ftf,Save^ftf,Undo^ftf,Delete^ftf,Audit^ftf,Query^ttf^Get All,Print^ttf,Crystal^ttf^Crystal Report,Export^ttf,Defaults^ttf,Help^ttf,Exit^ttf{&Delim2}
&glob nosavenew  /* by default we dont want a save new button */
&glob btnhorizontal     false
&glob btnflat           false
&glob btnstartcol       5
&glob btnstartrow       10
&glob btnheight         30   
&glob btnwidth          30
&glob btncenter         false

&glob AllowUpScrolling true
&glob GraphicButtons True
&glob KeyInTriggers    	/* Use triggers to create unique key */
&glob usemoreimage
&Glob NoImmediateQuery true
&glob MaximiseLicenses /* maximise broker licenses MUST use appserverconnect.p and zen-context */ 
&glob loginscreens login.w,autorun.w
&glob mainmenu mainmenu.w
&glob alwayscenterlist {&mainmenu},{&loginscreens}
&glob DisableParents DisableParents

/* dont forget to check {{&sys}overrides.i} */
/* for extra or overridden values !!! */

{{&tsk}taskservercodes.i} 
{{&core}classcodes.i}

