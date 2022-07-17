&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          schadm       PROGRESS
*/
&Scoped-define WINDOW-NAME win-main


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-f-hotkey NO-UNDO LIKE zen-f-hotkey.



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
&glob KeepRefreshButton
/* Local Variable Definitions ---                                       */
{app-paths.i}

&glob table-name zen-f-hotkey
&glob unique-key    {&table-name}TableId
&glob title-text HotKeys /* message box title */

&glob btnhorizontal     true
&glob NoChangedCheck    /* disable changed onleave check */
&glob NoExitCheck       /* disble exit question */
&glob Nobuttons
&Glob ImmediateQuery    /* force open query */

/* ***************************  Definitions  ************************** */
def var lv-user as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main
&Scoped-define BROWSE-NAME BROWSE-20

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-zen-f-hotkey

/* Definitions for BROWSE BROWSE-20                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-20 t-zen-f-hotkey.f-desc ~
t-zen-f-hotkey.f-program 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-20 
&Scoped-define QUERY-STRING-BROWSE-20 FOR EACH t-zen-f-hotkey NO-LOCK ~
    BY t-zen-f-hotkey.duser ~
       BY t-zen-f-hotkey.shortcut INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-20 OPEN QUERY BROWSE-20 FOR EACH t-zen-f-hotkey NO-LOCK ~
    BY t-zen-f-hotkey.duser ~
       BY t-zen-f-hotkey.shortcut INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-20 t-zen-f-hotkey
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-20 t-zen-f-hotkey


/* Definitions for FRAME f-main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-main ~
    ~{&OPEN-QUERY-BROWSE-20}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-20 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-20 FOR 
      t-zen-f-hotkey SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-20 win-main _STRUCTURED
  QUERY BROWSE-20 NO-LOCK DISPLAY
      t-zen-f-hotkey.f-desc FORMAT "X(30)":U WIDTH 49.8
      t-zen-f-hotkey.f-program FORMAT "X(255)":U WIDTH 76
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 131 BY 10
          FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     BROWSE-20 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 132 BY 12.1
         .


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
  CREATE WINDOW win-main ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 10.2
         ROW                = 18.33
         HEIGHT             = 12.1
         WIDTH              = 132
         MAX-HEIGHT         = 12.1
         MAX-WIDTH          = 132
         VIRTUAL-HEIGHT     = 12.1
         VIRTUAL-WIDTH      = 132
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 9
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN win-main = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-20 1 f-main */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-20
/* Query rebuild information for BROWSE BROWSE-20
     _TblList          = "Temp-Tables.t-zen-f-hotkey"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.t-zen-f-hotkey.duser|yes,Temp-Tables.t-zen-f-hotkey.shortcut|yes"
     _FldNameList[1]   > Temp-Tables.t-zen-f-hotkey.f-desc
"t-zen-f-hotkey.f-desc" ? ? "character" ? ? ? ? ? ? no ? no no "49.8" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.t-zen-f-hotkey.f-program
"t-zen-f-hotkey.f-program" ? "X(255)" "character" ? ? ? ? ? ? no ? no no "76" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-20 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-20
&Scoped-define SELF-NAME BROWSE-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-20 win-main
ON RETURN OF BROWSE-20 IN FRAME f-main
DO:
  run ok-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK win-main 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/* main core logic */
{{&core}commonmaint.i &path = "{&core}{&srv}"
                      &extraparams = "lv-user," }
/* &extraparams  = "whatever you want,"}*/

if transaction then do:
    message msg(161,'','','','')
    view-as alert-box information.
    run exit-trigger.
end.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {{&core}sec-chk.i}   /* screen security check */
    RUN enable_UI.
   {{&core}wid-chk.i}   /* widgetlevel security check */
   {{&core}focus.i}     /* set focus to first enabled widget */
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
  /* Hide all frames. */
  HIDE FRAME f-main.
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
  ENABLE BROWSE-20 
      WITH FRAME f-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-br-msdblclick-trigger win-main 
PROCEDURE local-br-msdblclick-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run ok-trigger.
return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialise win-main 
PROCEDURE Local-Initialise :
/* allow customising of initialisation 
return 'overide' to stop default processing
*/
CreateButs("Ok^ttf,Exit^ttf{&Delim2}" + 
          string(this-procedure) + "," +
          string({&window-name}:handle) + "," +
          string(frame {&frame-name}:handle) + "," + 
          "{&btnhorizontal},{&btnflat},{&btnstartcol},215,{&btnheight},{&btnwidth},true").                       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ok-trigger win-main 
PROCEDURE Ok-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if avail t-zen-f-hotkey then do:
    if search(t-zen-f-hotkey.f-program) ne ? or 
       search(substring(t-zen-f-hotkey.f-program,1,length(t-zen-f-hotkey.f-program) - 1) + 'r')
     ne ? then do:
          runchild(t-zen-f-hotkey.f-program,widget-handle(getsysvar('{&clv}top-window'))).
          apply 'close' to this-procedure.
    end.
  end.
  else message msg(160,'','','','')
       view-as alert-box.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh win-main 
PROCEDURE Refresh :
/*------------------------------------------------------------------------------
  Purpose: called from parent to refesh itself    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
lv-user = getsysvar('{&clv}user').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

