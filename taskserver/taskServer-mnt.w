&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-Tserver NO-UNDO LIKE Zen-Tserver.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                                                         */
/*  PROGRAM TITLE   :    mnt-zone.w (formerly - sg210)                        */ 
/*                                                                            */
/*  CREATE DATE     :    22/09/00                                             */
/*                                                                            */
/*  COMPANY NAME    :                                                         */
/*  VERSION NO.     :    1.0                                                  */
/******************************************************************************/
/******************************************************************************/
/*                          PATCH HISTORY                                     */
/*                                                                            */
/*           PATCH  USR                 Job                                   */
/* DATE      NO.    ID                  REF DESCRIPTION                       */
/* -------------------------------------------------------------------------- */
/* 22/09/00  v1               00  initial release                   */
/******************************************************************************/


CREATE WIDGET-POOL.
&glob title-text    Task Server SetUp
&glob table-name    zen-tserver
&glob unique-key   {&table-name}tableid
&global-define suppresswindow
&glob KeepRefreshButton
&Glob ImmediateQuery
&global-define KeepRefreshButton
/* ***************************  Definitions  ************************** */
{app-paths.i}

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
&Scoped-define INTERNAL-TABLES t-Zen-Tserver

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-Zen-Tserver.StartUser ~
string(starttime,"HH:MM am") t-Zen-Tserver.StartDate t-Zen-Tserver.TStatus ~
t-Zen-Tserver.started t-Zen-Tserver.TaskServer 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-Zen-Tserver NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-Zen-Tserver NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-Zen-Tserver
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-Zen-Tserver


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint 
&Scoped-Define DISPLAYED-FIELDS t-Zen-Tserver.taskserver ~
t-Zen-Tserver.DBParameters t-Zen-Tserver.serverprog t-Zen-Tserver.logfile ~
t-Zen-Tserver.twait t-Zen-Tserver.tdefault t-Zen-Tserver.catchup ~
t-Zen-Tserver.AutoStart 
&Scoped-define DISPLAYED-TABLES t-Zen-Tserver
&Scoped-define FIRST-DISPLAYED-TABLE t-Zen-Tserver


/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-Zen-Tserver.taskserver t-Zen-Tserver.DBParameters ~
t-Zen-Tserver.serverprog t-Zen-Tserver.logfile t-Zen-Tserver.twait ~
t-Zen-Tserver.tdefault t-Zen-Tserver.catchup t-Zen-Tserver.AutoStart 
&Scoped-define Edit-List t-Zen-Tserver.taskserver ~
t-Zen-Tserver.DBParameters t-Zen-Tserver.serverprog t-Zen-Tserver.logfile ~
t-Zen-Tserver.twait t-Zen-Tserver.tdefault t-Zen-Tserver.catchup ~
t-Zen-Tserver.AutoStart 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Stopped window-maint 
FUNCTION Stopped RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-Zen-Tserver SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-Zen-Tserver.StartUser FORMAT "x(12)":U WIDTH 22.8
      string(starttime,"HH:MM am") COLUMN-LABEL "Start Time" FORMAT "x(8)":U
      t-Zen-Tserver.StartDate
      t-Zen-Tserver.TStatus FORMAT "x(12)":U WIDTH 8
      t-Zen-Tserver.started
      t-Zen-Tserver.TaskServer FORMAT "x(40)":U WIDTH 24.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 92 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     br-maint AT ROW 1.24 COL 13 HELP
          "Select the record to edit."
     t-Zen-Tserver.taskserver AT ROW 10.05 COL 26.2 COLON-ALIGNED FORMAT "X(100)"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     t-Zen-Tserver.DBParameters AT ROW 11.24 COL 28.2 NO-LABEL WIDGET-ID 6
          VIEW-AS EDITOR
          SIZE 68 BY 5 TOOLTIP "Use same connections as appserver (generally)"
     t-Zen-Tserver.serverprog AT ROW 16.43 COL 26.2 COLON-ALIGNED FORMAT "X(100)"
          VIEW-AS FILL-IN 
          SIZE 68 BY 1 TOOLTIP "Controlling program which will fire up routine on server."
     t-Zen-Tserver.logfile AT ROW 17.43 COL 26.2 COLON-ALIGNED FORMAT "X(100)"
          VIEW-AS FILL-IN 
          SIZE 68 BY 1 TOOLTIP "If blank, uses logs/[name of task server].log"
     t-Zen-Tserver.twait AT ROW 18.43 COL 26.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1 TOOLTIP "Wait ___ seconds between checking for new jobs."
     t-Zen-Tserver.tdefault AT ROW 18.62 COL 38.2
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY 1 TOOLTIP "Use this as default if none specified (on tasks)"
     t-Zen-Tserver.catchup AT ROW 18.62 COL 55.2
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY 1 TOOLTIP "If task is set to run at 9:00, but server doesn't start till 10:00, do task?"
     t-Zen-Tserver.AutoStart AT ROW 18.62 COL 74.2 WIDGET-ID 8
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY 1 TOOLTIP "Start this up when appserver broker is started."
     "DB Parameters" VIEW-AS TEXT
          SIZE 15 BY .71 AT ROW 11.24 COL 11 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 107.8 BY 19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-Zen-Tserver T "?" NO-UNDO schadm Zen-Tserver
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Zone Maintenance"
         COLUMN             = 155.6
         ROW                = 16.48
         HEIGHT             = 22.48
         WIDTH              = 108
         MAX-HEIGHT         = 45.33
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.33
         VIRTUAL-WIDTH      = 256
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         THREE-D            = yes
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frame-maint
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint TEXT-1 frame-maint */
/* SETTINGS FOR TOGGLE-BOX t-Zen-Tserver.AutoStart IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-Zen-Tserver.catchup IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR EDITOR t-Zen-Tserver.DBParameters IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Tserver.logfile IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-Zen-Tserver.serverprog IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-Zen-Tserver.taskserver IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR TOGGLE-BOX t-Zen-Tserver.tdefault IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Tserver.twait IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-Zen-Tserver"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-Zen-Tserver.StartUser
"t-Zen-Tserver.StartUser" ? "x(12)" "character" ? ? ? ? ? ? no ? no no "22.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"string(starttime,""HH:MM am"")" "Start Time" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.t-Zen-Tserver.StartDate
     _FldNameList[4]   > Temp-Tables.t-Zen-Tserver.TStatus
"t-Zen-Tserver.TStatus" ? "x(12)" "" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.t-Zen-Tserver.started
     _FldNameList[6]   > Temp-Tables.t-Zen-Tserver.TaskServer
"t-Zen-Tserver.TaskServer" ? "x(40)" "character" ? ? ? ? ? ? no ? no no "24.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME br-maint

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*******************
    DONT FORGET TO CHECK DIR LOCATION FOR BELOW INCLUDE !!!!!!!
********************/
 
{{&core}commonmaint.i &path = "{&tsk}{&srv}"}   

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateExtraFields window-maint 
PROCEDURE CreateExtraFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

      t-Zen-Tserver.serverprog = '{&tsk}{&srv}tserver.p'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-validate window-maint 
PROCEDURE delete-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
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
  IF AVAILABLE t-Zen-Tserver THEN 
    DISPLAY t-Zen-Tserver.taskserver t-Zen-Tserver.DBParameters 
          t-Zen-Tserver.serverprog t-Zen-Tserver.logfile t-Zen-Tserver.twait 
          t-Zen-Tserver.tdefault t-Zen-Tserver.catchup t-Zen-Tserver.AutoStart 
      WITH FRAME frame-maint.
  ENABLE br-maint 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-trigger window-maint 
PROCEDURE local-delete-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

if not stopped() then return 'override'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-edit-trigger window-maint 
PROCEDURE local-edit-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if not stopped() then return 'override'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen window-maint 
PROCEDURE validate-screen :
/* ----------------------------------------------------------------
  Purpose:      Checks the zone ref and description are entered
  Parameters:   None
  Notes:        Puts up error messages if invalid and stops 
                processing.
-----------------------------------------------------------------*/

  return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Stopped window-maint 
FUNCTION Stopped RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

IF AVAIL t-zen-tserver AND t-zen-tserver.tstatus <> '{&tstopped}' 
THEN DO:
   MESSAGE msg(228,t-zen-tserver.taskserver,t-zen-tserver.tstatus,'{&tstopped}','') VIEW-AS ALERT-BOX ERROR.
   /* The TaskServer #1 is currently #2, unable to carry out any action on this record
      until the TaskServer is #3 */
   RETURN false.
END.

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

