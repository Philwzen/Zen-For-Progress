/* global definitions for MS VB constants */
&global-define TH32CS_SNAPPROCESS 2
&global-define PROCESS_QUERY_INFORMATION 1024
&global-define process_all_access 2035711
&global-define PROCESS_VM_READ 16
&global-define MAX_PATH 260
&global-define PROCESS_TERMINATE 1
&global-define wdformletters 0
&global-define wdOpenFormatAuto 0
&global-define WdFirstRecord -4
&global-define WdlastRecord -5
&global-define WdNextRecord -2
&global-define wdFirstDataSourceRecord -6
&global-define WdLastDataSourceRecord -7
&global-define WdNextDataSourcerecord -8
&global-define wdDoNotSaveChanges 0
&global-define wdDialogFilePrint 88
&global-define wdPrintAllDocument 0
&global-define wdToggle 9999998 
&global-define wdLineStyleSingle 1 
&global-define wdLineWidth050pt 4 
&global-define wdBorderLeft -2 
&global-define wdBorderRight -4 
&global-define wdBorderTop -1 
&global-define wdBorderBottom -3 
&global-define wdBorderHorizontal -5 
&global-define wdBorderVertical -6 
&global-define wdBorderDiagonalDown -7 
&global-define wdBorderDiagonalUp -8 
&GLOBAL-DEFINE wdSeekCurrentPageHeader 9
           /* view types */
&GLOBAL-DEFINE wdMasterView 5
&GLOBAL-DEFINE wdNormalView 1
&GLOBAL-DEFINE wdOutlineView 2
&GLOBAL-DEFINE wdPrintPreview 4
&GLOBAL-DEFINE wdPrintView 3
&GLOBAL-DEFINE wdReadingView 7
&GLOBAL-DEFINE wdWebView 6
&glob wdPageFitBestFit 2                                       
&glob wdPageFitFullPage 1
&glob wdPageFitTextFit 3
/* doc protection */
&Glob wdAllowOnlyRevisions  0
&Glob wdAllowOnlyComments  1
&Glob wdAllowOnlyFormFields  2
&Glob wdAllowOnlyReading  3
&Glob wdNoProtection    -1
/* doc save formats */
&Glob wdFormatDocument	0 
&Glob wdFormatDOSText	4  
&Glob wdFormatDOSTextLineBreaks	5 
&Glob wdFormatEncodedText	7  
&Glob wdFormatFilteredHTML	10 
&Glob wdFormatHTML	8	
&Glob wdFormatRTF	6  
&Glob wdFormatTemplate	1	
&Glob wdFormatText	2  
&Glob wdFormatTextLineBreaks	3  
&Glob wdFormatUnicodeText	7	
&Glob wdFormatWebArchive	9  
&Glob wdFormatXML	11  
&Glob wdFormatDocument97	0   
&Glob wdFormatDocumentDefault	16 
&Glob wdFormatPDF	17 
&Glob wdFormatTemplate97	1	
&Glob wdFormatXMLDocument	12
&Glob wdFormatXMLDocumentMacroEnabled	13 
&Glob wdFormatXMLTemplate	14	
&Glob wdFormatXMLTemplateMacroEnabled	15 
&Glob wdFormatXPS	18	

&global-define wdLineStyleNone 0 
&global-define wdColorAutomatic -16777216
&global-define wdColorBlack 0
&global-define wdTexture20Percent 200
&global-define wdwindowstatenormal 0
&global-define wdwindowstatemaximize 1
&global-define wdwindowstateminimize 2
&global-define wdWithInTable 12 
&global-define WdAlignParagraphRight 2
&global-define wdStory 6
&global-define wdExtend 1
&global-define WdSendToNewDocument 0
&global-define xlInsertDeleteCells 1
&global-define xlDelimited 1
&global-define xlTextQualifierDoubleQuote 1
/* WINAPI printer constants */
&global-define PRINTER_ENUM_DEFAULT      1
&global-define PRINTER_ENUM_LOCAL        2
&global-define PRINTER_ENUM_CONNECTIONS  4
&global-define PRINTER_ENUM_FAVORITE     4
&global-define PRINTER_ENUM_NAME         8
&global-define PRINTER_ENUM_REMOTE       16
&global-define PRINTER_ENUM_SHARED       32
&global-define PRINTER_ENUM_NETWORK      64
&global-define PRINTER_ENUM_EXPAND       16384
&global-define PRINTER_ENUM_CONTAINER    32768
&global-define PRINTER_ENUM_ICONMASK     16711680
&global-define PRINTER_ENUM_ICON1        65536
&global-define PRINTER_ENUM_ICON2        131070
&global-define PRINTER_ENUM_ICON3        262144
&global-define PRINTER_ENUM_ICON4        524288
&global-define PRINTER_ENUM_ICON5        1048576
&global-define PRINTER_ENUM_ICON6        2097152
&global-define PRINTER_ENUM_ICON7        4194304
&global-define PRINTER_ENUM_ICON8        8388608
&global-define PRINTER_INFO_1            1
&global-define PRINTER_INFO_2            2
&global-define PRINTER_INFO_3            3
&global-define PRINTER_INFO_4            4
&global-define PRINTER_INFO_5            5
&global-define PRINTER_INFO_6            6
&global-define PRINTER_INFO_7            7

/* preprocessors for windows dlls etc 
so we can deal with 16 and 32 or even 64 bit versions */
&IF "{&OPSYS}":U="WIN32":U &THEN
   /* 32-bit definitions, Progress 8.2+ */

   &global-define A A
   /* data types */
   &global-define HWND long
   &global-define BOOL long
   &global-define HINSTANCE long
   &global-define INT long
   &global-define INTSIZE 4
   /* libraries */
   &global-define USER     "user32"
   &global-define KERNEL   "kernel32"
   &global-define SHELL    "shell32"
   &global-define MAPI     "mapi32"
   &global-define GDI      "gdi32"
   &global-define MMEDIA   "winmm"
   &global-define WINSPOOL "winspool.drv"
   &global-define ADVAPI   "advapi32"
   &global-define A A

&ELSE
   /* 16-bit definitions, Progress 7 to 8.1 */

   /* data types */
   &global-define HWND short
   &global-define BOOL short
   &global-define HINSTANCE short
   &global-define INT short
   &global-define INTSIZE 2

   /* libraries */
   &global-define USER   "user.exe"
   &global-define KERNEL "kernel.exe"
   &global-define SHELL  "shell.dll"
   &global-define MAPI   "mapi.dll"
   &global-define GDI    "gdi.exe"
   &global-define A

&ENDIF


/* messages */
&global-define WM_PAINT 15
&global-define WM_HSCROLL 276
&global-define WM_VSCROLL 277
&global-define WM_LBUTTONDOWN 513
&global-define WM_LBUTTONUP 514
&global-define WM_RBUTTONDOWN 516
&global-define WM_RBUTTONUP 517
&global-define WM_USER 1024

/* mouse buttons */
&global-define MK_LBUTTON 1
&global-define MK_RBUTTON 2

/* scrollbars */
&global-define SB_HORZ 0
&global-define SB_VERT 1
&global-define SB_BOTH 3
&global-define SB_THUMBPOSITION 4

/* editors */
&IF "{&OPSYS}":U="WIN32":U &THEN
   &global-define EM_SETPASSWORDCHAR 204
&ELSE
    &global-define EM_SETPASSWORDCHAR {&WM_USER} + 28
&ENDIF

/* some window styles */
&global-define GWL_STYLE -16
&global-define WS_MAXIMIZEBOX 65536
&global-define WS_MINIMIZEBOX 131072
&global-define WS_THICKFRAME  262144
&global-define WS_CAPTION 12582912
&global-define WS_BORDER 8388608

/* some extended window styles */
&global-define GWL_EXSTYLE -20
&global-define WS_EX_CONTEXTHELP 1024
&global-define WS_EX_PALETTEWINDOW 392

/* system commands/menu */
&global-define SC_SIZE      61440  
&global-define SC_MINIMIZE  61472
&global-define SC_MAXIMIZE  61488  
&global-define MF_BYCOMMAND 0

/* placement order (Z-order) */
&global-define HWND_TOPMOST -1
&global-define HWND_NOTOPMOST -2
 
/* window-positioning flags */
&global-define SWP_NOSIZE 1
&global-define SWP_NOMOVE 2
&global-define SWP_NOZORDER 4
&global-define SWP_NOACTIVATE 16 
&global-define SWP_FRAMECHANGED 32
&global-define SWP_SHOWWINDOW 64
&global-define WM_SETREDRAW 11

/* registry */
&scoped-define HKEY_CLASSES_ROOT     -2147483648
&scoped-define HKEY_CURRENT_USER     -2147483647
&scoped-define HKEY_LOCAL_MACHINE    -2147483646
&scoped-define HKEY_USERS            -2147483645
&scoped-define HKEY_PERFORMANCE_DATA -2147483644
&scoped-define HKEY_CURRENT_CONFIG   -2147483643
&scoped-define HKEY_DYN_DATA         -2147483642

&scoped-define KEY_ALL_ACCESS 983103
&scoped-define REG_OPTION_NON_VOLATILE 0
&scoped-define REG_OPTION_VOLATILE 1
&scoped-define REG_CREATED_NEW_KEY 1
&scoped-define REG_OPENED_EXISTING_KEY 2


&global-define ERROR_SUCCESS 0
&global-define ERROR_NO_MORE_ITEMS 259

&global-define MAX_PATH 260

/* File Operations */
&global-define FO_MOVE   1
&global-define FO_COPY   2
&global-define FO_DELETE 3
&global-define FO_RENAME 4
&global-define FOF_SILENT              4
&global-define FOF_RENAMEONCOLLISION   8
&global-define FOF_NOCONFIRMATION     16
&global-define FOF_ALLOWUNDO          64
&global-define FOF_SIMPLEPROGRESS    256
&global-define FOF_NOCONFIRMMKDIR	512
&global-define FOF_NOERRORUI      1024


/* results from WaitForSingleObject */
&global-define WAIT_ABANDONED 128
&global-define WAIT_OBJECT_0 0

/* menu manipulation */
&global-define MF_BYPOSITION 1024
&global-define MF_REMOVE     256


/* printer dialog dev mode */
&global-define PD_ALLPAGES 0
&global-define PD_SELECTION 1
&global-define PD_PAGENUMS 2
&global-define PD_NOSELECTION 4
&global-define PD_NOPAGENUMS 8
&global-define PD_COLLATE 16
&global-define PD_PRINTTOFILE 32 /* should the PrintToFiletoggle box show */
&global-define PD_PRINTSETUP 64 /* display PrintSetup dialogvs Printer Dialog */
&global-define PD_NOWARNING 128
&global-define PD_RETURNDC 256 /* return the Device Contextfor the selected printer */
&global-define PD_RETURNIC 512
&global-define PD_RETURNDEFAULT 1024 /* returns hDevMod andhDevName without displaying dialog */
&global-define PD_SHOWHELP 2048 /* should help button showup */
&global-define PD_ENABLEPRINTHOOK 4096
&global-define PD_ENABLESETUPHOOK 8192
&global-define PD_ENABLEPRINTTEMPLATE 16384
&global-define PD_ENABLESETUPTEMPLATE 32768
&global-define PD_ENABLEPRINTTEMPLATEHANDLE 65536
&global-define PD_ENABLESETUPTEMPLATEHANDLE 131072
&global-define PD_USEDEVMODECOPIES 262144
&global-define PD_USEDEVMODECOPIESANDCOLLATE 524288
&global-define PD_DISABLEPRINTTOFILE 524288
&global-define PD_HIDEPRINTTOFILE 1048576
&global-define PD_NONETWORKBUTTON 2097152
&global-define DEVMOD_NUMCOPIESOFFSET 23
&global-define CCHDEVICENAME 32
&global-define DM_COPIES 256

