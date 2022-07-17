&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME win-main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS win-main 
/******************************************************************************/
/*  PROGRAM ID.     : ????????                                                */
/*  PROGRAM TITLE   : ????????                                                */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    : Zennor Computing LTD                                    */
/*  VERSION NO.     : 1.0                                                     */
/******************************************************************************/
/******************************************************************************/
/*                          PATCH HISTORY                                     */
/*                                                                            */
/*           PATCH  USR     Job                                               */
/* DATE      NO.    ID      REF DESCRIPTION                                   */
/* -------------------------------------------------------------------------- */
/* ??/??/??  P00    Philw   00  initial release                               */
/******************************************************************************/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{app-paths.i}
&glob title-text "?????"
&glob table-name ?????
&glob unique-key t-recid
/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-from lv-to lv-subject lv-text lv-doc ~
lv-app BUTTON-1 BUTTON-3 BUTTON-4 BUTTON-2 
&Scoped-Define DISPLAYED-OBJECTS lv-from lv-to lv-subject lv-text lv-doc ~
lv-app 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-from lv-to lv-subject lv-text lv-doc lv-app 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "run" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "printers" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "Button 3" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-4 
     LABEL "Button 4" 
     SIZE 15 BY 1.14.

def var lv-doc as char FORMAT "X(256)":U INITIAL "c:~\temp~\asst-visits-cry.doc" 
     LABEL "Doc" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

def var lv-from as char FORMAT "X(256)":U INITIAL "Zen@realemail.co.uk" 
     LABEL "From" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

def var lv-subject as char FORMAT "X(256)":U INITIAL "Test Message" 
     LABEL "Subject" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

def var lv-text as char FORMAT "X(256)":U INITIAL "testing testing 123" 
     LABEL "Text" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

def var lv-to as char FORMAT "X(256)":U INITIAL "phil.white@ensign-group.com" 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

def var lv-app as char 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Word", "word.application",
"Excel", "excel.application",
"Outlook", "outlook.application"
     SIZE 12 BY 3 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     lv-from AT ROW 3.38 COL 15 COLON-ALIGNED
     lv-to AT ROW 4.57 COL 15 COLON-ALIGNED
     lv-subject AT ROW 5.76 COL 15 COLON-ALIGNED
     lv-text AT ROW 6.95 COL 15 COLON-ALIGNED
     lv-doc AT ROW 8.14 COL 15 COLON-ALIGNED
     lv-app AT ROW 9.33 COL 10 NO-LABEL
     BUTTON-1 AT ROW 9.57 COL 24
     BUTTON-3 AT ROW 9.57 COL 44
     BUTTON-4 AT ROW 11 COL 43
     BUTTON-2 AT ROW 11.24 COL 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 59.6 BY 12.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW win-main ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 21.6
         ROW                = 7.62
         HEIGHT             = 12.1
         WIDTH              = 59.6
         MAX-HEIGHT         = 12.1
         MAX-WIDTH          = 112
         VIRTUAL-HEIGHT     = 12.1
         VIRTUAL-WIDTH      = 112
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
/* SETTINGS FOR RADIO-SET lv-app IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-doc IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-from IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-subject IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-text IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-to IN FRAME f-main
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(win-main)
THEN win-main:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 win-main
ON CHOOSE OF BUTTON-1 IN FRAME f-main /* run */
DO:

def var lv-applhandle as com-handle no-undo.

assign frame {&frame-name}
    {&list-1}.

lv-applhandle = MSOpenApplication(lv-app,'visible').

message MsApplicationname(lv-applhandle) view-as alert-box.

/* MsMergeToOutlook(lv-applhandle,'from',lv-to,                                    */
/*                 'OfficeServer','File:' + GetFullPath(lv-doc),                   */
/*                  GetFullPath(lv-doc),                                           */
/*                  string(today + 3,'99/99/9999') + string(time,'hh:mm:ss am')).  */

MSCloseApplication(lv-applhandle).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 win-main
ON CHOOSE OF BUTTON-2 IN FRAME f-main /* printers */
DO:
  message '**' WapiGetPrinters() '**' view-as alert-box.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 win-main
ON CHOOSE OF BUTTON-3 IN FRAME f-main /* Button 3 */
DO:

  assign frame {&frame-name}
        {&list-1}.

  def var lv-share as char no-undo init "~\" format 'x(30)'.
  def var lv-destfile  as char no-undo.
  def var h-applhandle as com-handle no-undo.

  lv-share = WapiGetShareName('o').
  lv-destfile = 'ofsrv\officeserver.w'. 

message lv-share skip
        lv-destfile    skip
        getfullpath(lv-destfile) skip
        search(lv-destfile)
view-as alert-box.

    h-applhandle = MSOpenApplication("outlook.application",'visible').
    MsMergeToOutlook(h-applhandle,'',lv-to,
                    'OfficeServer','Youre Job Has Completed File:' +  lv-share + '\' + lv-destfile,
                     '',
                     string(today + 3,'99/99/9999') + string(time,'hh:mm:ss am')).

    MSCloseApplication(h-applhandle).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 win-main
ON CHOOSE OF BUTTON-4 IN FRAME f-main /* Button 4 */
DO:
  
  def var h-applhandle as com-handle no-undo.

  h-applhandle = MSOpenApplication("write.application",'visible').

message 'a' view-as alert-box.
    MSCloseApplication(h-applhandle).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK win-main 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
/* this dont work in v8 for my style of windows 
if int(substring(proversion,1,1)) >= 7 then
{&WINDOW-NAME}:parent         = current-window.      
*/
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

PROCEDURE WNetGetConnectionA EXTERNAL "mpr.dll" :
  DEFINE INPUT        PARAMETER lpDrive    AS CHAR.
  DEFINE OUTPUT       PARAMETER lpUNCName  AS CHAR.
  DEFINE INPUT-OUTPUT PARAMETER lpnLength  AS LONG.
  DEFINE RETURN       PARAMETER RetBool    AS LONG.
END PROCEDURE.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
oN CLOSE OF THIS-PROCEDURE do:
    if not lv-exited then
        run exit-trigger in this-procedure no-error.
    run disable_ui.             
end.

ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
    run exit-trigger in this-procedure no-error.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
    run exit-trigger in this-procedure no-error.
END.
    
{&window-name}:hidden = false.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    run initialise.
IF NOT THIS-PROCEDURE:PERSISTENT OR SESSION:DISPLAY-TYPE ne "GUI":U then
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI win-main  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(win-main)
  THEN DELETE WIDGET win-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI win-main  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY lv-from lv-to lv-subject lv-text lv-doc lv-app 
      WITH FRAME f-main IN WINDOW win-main.
  ENABLE lv-from lv-to lv-subject lv-text lv-doc lv-app BUTTON-1 BUTTON-3 
         BUTTON-4 BUTTON-2 
      WITH FRAME f-main IN WINDOW win-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exit-trigger win-main 
PROCEDURE exit-trigger :
def var lv-exit as log no-undo.
def var x as int no-undo.
    
    lv-exited = true.
    APPLY "CLOSE":U TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise win-main 
PROCEDURE initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var lv-buttonparams as char no-undo. 
   
    lv-buttonparams = "Ok^ttf,Exit^ttf|" + 
                      string(this-procedure) + "," +
                      string({&window-name}:handle) + "," +
                      string(frame {&frame-name}:handle) + "," + 
                      "true,{&btnflat},{&btnstartcol},{&btnstartrow},{&btnheight},{&btnwidth},true".                       
                
    &if defined(NoNew)    ne 0 &then lv-buttonparams = replace(lv-buttonparams,"new^","^").    &endif  
    &if defined(NoEdit)   ne 0 &then lv-buttonparams = replace(lv-buttonparams,"edit^","^").   &endif
    &if defined(NoSave)   ne 0 &then lv-buttonparams = replace(lv-buttonparams,"save^","^").   &endif
    &if defined(NoDelete) ne 0 &then lv-buttonparams = replace(lv-buttonparams,"delete^","^"). &endif
    &if defined(NoUndo)   ne 0 &then lv-buttonparams = replace(lv-buttonparams,"undo^","^").   &endif
    &if defined(NoQuery)  ne 0 &then lv-buttonparams = replace(lv-buttonparams,"query^","^").  &endif
    &if defined(NoAudit)  ne 0 &then lv-buttonparams = replace(lv-buttonparams,"audit^","^").  &endif
    &if defined(NoExit)   ne 0 &then lv-buttonparams = replace(lv-buttonparams,"exit^","^").   &endif
    &if defined(NoHelp)   ne 0 &then lv-buttonparams = replace(lv-buttonparams,"help^","^").   &endif
    &if defined(NoPrint)  ne 0 &then lv-buttonparams = replace(lv-buttonparams,"print^","^").  &endif     

    CreateButs(lv-buttonparams).  
 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok-trigger win-main 
PROCEDURE ok-trigger :
def var lv-applhandle as com-handle no-undo.
assign frame {&frame-name}
    {&list-1}.

lv-applhandle = MSOpenApplication("outlook.application",'visible').

message MsApplicationname(lv-applhandle) view-as alert-box.

MsMergeToOutlook(lv-applhandle,lv-from,lv-to,lv-subject,lv-text,lv-doc,'').

MSCloseApplication(lv-applhandle).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh win-main 
PROCEDURE refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen win-main 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
/* validate the screen widgets etc if any fail 
    display message put focus on that widget and return 'failed' 
 e.g.

  IF lv-new THEN
      IF CAN-FIND(FIRST b-{&Table-name} WHERE b-{&Table-name}.keyfield = t-{&Table-name}.keyfield) 
      THEN DO:
          MESSAGE msg (120,"","","","") VIEW-AS ALERT-BOX.
          RETURN STRING(t-{&Table-name}.keyfield:HANDLE in frame one).
      END.
   else return 'passed'.
*/
    return 'passed'.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

