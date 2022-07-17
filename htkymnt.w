&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-f-hotkey NO-UNDO LIKE zen-f-hotkey.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                                                         */
/*  PROGRAM TITLE   :                                                         */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    :                                                         */
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
/****************************************************************************** 
 this is a semi generic routine you only need to paint the screen widgets
 then edit pc-save for any fields not on the screen ie time stamps etc
 and edit validate-screen for any validation logic you require to allow record 
 to be saved 
********************************************************************************/

CREATE WIDGET-POOL.
&glob title-text Hotkey Maint
&glob table-name zen-f-hotkey
&glob unique-key    {&table-name}TableId
{app-paths.i}


&glob KeepRefreshButton
&glob suppresswindow  
&Glob ImmediateQuery
&undefine getallonquery

/* ***************************  Definitions  ************************** */
&glob nohelp
/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */

def var lv-sysman as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frame-maint
&Scoped-define BROWSE-NAME br-maint

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-zen-f-hotkey

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-f-hotkey.f-desc ~
t-zen-f-hotkey.duser 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-f-hotkey NO-LOCK ~
    BY t-zen-f-hotkey.duser ~
       BY t-zen-f-hotkey.shortcut
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-f-hotkey NO-LOCK ~
    BY t-zen-f-hotkey.duser ~
       BY t-zen-f-hotkey.shortcut.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-f-hotkey
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-f-hotkey


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-user br-maint 
&Scoped-Define DISPLAYED-FIELDS t-zen-f-hotkey.duser t-zen-f-hotkey.f-desc ~
t-zen-f-hotkey.f-program t-zen-f-hotkey.shortcut 
&Scoped-define DISPLAYED-TABLES t-zen-f-hotkey
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-f-hotkey
&Scoped-Define DISPLAYED-OBJECTS lv-user 

/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-zen-f-hotkey.duser t-zen-f-hotkey.f-desc ~
t-zen-f-hotkey.f-program t-zen-f-hotkey.shortcut 
&Scoped-define Edit-List t-zen-f-hotkey.duser t-zen-f-hotkey.f-desc ~
t-zen-f-hotkey.f-program t-zen-f-hotkey.shortcut 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE lv-user AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "User" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-f-hotkey SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-f-hotkey.f-desc FORMAT "x(30)":U WIDTH 66.4
      t-zen-f-hotkey.duser WIDTH 18.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 83.8 BY 12.38 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     lv-user AT ROW 1.1 COL 16.2 COLON-ALIGNED
     br-maint AT ROW 2.19 COL 11.2 HELP
          "Select the record to edit."
     t-zen-f-hotkey.duser AT ROW 14.67 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 51 BY 1
     t-zen-f-hotkey.f-desc AT ROW 15.86 COL 20 COLON-ALIGNED FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 77 BY 1
     t-zen-f-hotkey.f-program AT ROW 17.05 COL 20 COLON-ALIGNED
          LABEL "Program" FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 77 BY 1
     t-zen-f-hotkey.shortcut AT ROW 18.24 COL 20 COLON-ALIGNED FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 50.8 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 103 BY 19.76
         TITLE "Jumps Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-f-hotkey T "?" NO-UNDO schadm zen-f-hotkey
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Browse Maintenance"
         HEIGHT             = 26.67
         WIDTH              = 103
         MAX-HEIGHT         = 45.33
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.33
         VIRTUAL-WIDTH      = 256
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         FONT               = 9
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN window-maint = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW window-maint
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frame-maint
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint lv-user frame-maint */
/* SETTINGS FOR FILL-IN t-zen-f-hotkey.duser IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-f-hotkey.f-desc IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-zen-f-hotkey.f-program IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT                                   */
/* SETTINGS FOR FILL-IN t-zen-f-hotkey.shortcut IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-f-hotkey"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _OrdList          = "Temp-Tables.t-zen-f-hotkey.duser|yes,Temp-Tables.t-zen-f-hotkey.shortcut|yes"
     _FldNameList[1]   > Temp-Tables.t-zen-f-hotkey.f-desc
"t-zen-f-hotkey.f-desc" ? "x(30)" "" ? ? ? ? ? ? no ? no no "66.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-f-hotkey.duser
"t-zen-f-hotkey.duser" ? ? "character" ? ? ? ? ? ? no ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME lv-user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-user window-maint
ON LEAVE OF lv-user IN FRAME frame-maint /* User */
DO:
  assign frame {&frame-name} lv-user.
/*   run openquery. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*******************
    DONT FORGET TO CHECK DIR LOCATION FOR BELOW INCLUDE !!!!!!!
********************/
lv-user = getsysvar('{&clv}user').
{{&core}commonmaint.i &path = "{&core}{&srv}"
                      &extraparams = "lv-user,"}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
 
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {{&core}sec-chk.i}
  RUN enable_UI.
  {{&core}wid-chk.i}
  {{&core}focus.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-validate window-maint 
PROCEDURE delete-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if lookup(t-zen-f-hotkey.duser,'*') ne 0
then do:
   message 'This Will Delete a Global Entry!' skip
           'Are you sure You want to do this!?'
   view-as alert-box question buttons yes-no update lv-ok as log.
   if not lv-ok then return 'failed'.
end.


    Return "passed".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI window-maint  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME frame-maint.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disp-wids window-maint 
PROCEDURE Disp-wids :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 if not lv-sysman and 
        lookup(t-zen-f-hotkey.duser,'*') ne 0
then setsensitive(false,'inc','btn-edit,btn-delete',frame {&frame-name}:handle).
else setsensitive(true,'inc','btn-edit,btn-delete',frame {&frame-name}:handle).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI window-maint  _DEFAULT-ENABLE
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
  DISPLAY lv-user 
      WITH FRAME frame-maint.
  IF AVAILABLE t-zen-f-hotkey THEN 
    DISPLAY t-zen-f-hotkey.duser t-zen-f-hotkey.f-desc t-zen-f-hotkey.f-program 
          t-zen-f-hotkey.shortcut 
      WITH FRAME frame-maint.
  ENABLE lv-user br-maint 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-afterinitialise window-maint 
PROCEDURE local-afterinitialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run disp-wids.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialise window-maint 
PROCEDURE Local-Initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 lv-sysman =  systemmanager(GetUserID()).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen window-maint 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/


  return 'passed'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

