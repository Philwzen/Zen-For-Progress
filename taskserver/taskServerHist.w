&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          schadm       PROGRESS
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-TaskHist NO-UNDO LIKE Zen-TaskHist.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                              */
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

CREATE WIDGET-POOL.
&glob KeepRefreshButton
/* ***************************  Definitions  ************************** */
{app-paths.i}
&global-define nobuttons
&global-define suppresswindow
&global-define nobrowse
DEF VAR vr-row  like zen-taskhist.zen-taskhisttableid NO-UNDO. /* rowid of task server we are on */
DEF VAR h-timer AS COM-HANDLE NO-UNDO.

DEF VAR lv-server AS CHAR NO-UNDO.

DEF VAR vr-taskhist like zen-taskhist.zen-taskhisttableid NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frame-maint
&Scoped-define BROWSE-NAME br-history

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-Zen-TaskHist

/* Definitions for BROWSE br-history                                    */
&Scoped-define FIELDS-IN-QUERY-br-history t-Zen-TaskHist.TaskId ~
t-Zen-TaskHist.TStatus t-Zen-TaskHist.cycle t-Zen-TaskHist.TaskType ~
t-Zen-TaskHist.SubmitDate string(t-Zen-TaskHist.SubmitTime,"hh:mm am") ~
t-Zen-TaskHist.SubmitUser t-Zen-TaskHist.RunDate ~
string(t-Zen-TaskHist.RunTime, "hh:mm am") t-Zen-TaskHist.StartDate ~
string(t-Zen-TaskHist.StartTime, "hh:mm am") t-Zen-TaskHist.EndDate ~
string(t-Zen-TaskHist.EndTime,"hh:mm am") 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-history 
&Scoped-define QUERY-STRING-br-history FOR EACH t-Zen-TaskHist NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-history OPEN QUERY br-history FOR EACH t-Zen-TaskHist NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-history t-Zen-TaskHist
&Scoped-define FIRST-TABLE-IN-QUERY-br-history t-Zen-TaskHist


/* Definitions for FRAME frame-maint                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-frame-maint ~
    ~{&OPEN-QUERY-br-history}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-Exit RECT-16 br-history b-refresh ~
b-delete b-restore b-details b-clear-hist vd-date refresh-hist ~
lv-prior-date 
&Scoped-Define DISPLAYED-OBJECTS vd-date refresh-hist lv-prior-date 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-clear-hist 
     LABEL "Clear Entire History" 
     SIZE 21 BY 1.14.

DEFINE BUTTON b-delete 
     LABEL "Delete History" 
     SIZE 16.8 BY 1.14.

DEFINE BUTTON b-details 
     LABEL "Details" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-refresh 
     LABEL "Refresh" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-restore 
     LABEL "Restore Task" 
     SIZE 17.8 BY 1.14.

DEFINE BUTTON btn-Exit 
     IMAGE-UP FILE "{&core}grafix/exit-su.ico":U NO-FOCUS
     LABEL "E&xit":L 
     SIZE 5.6 BY 1.33 TOOLTIP "Exit".

def var vd-date AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 174 BY 12.38.

def var lv-prior-date as log INITIAL no 
     LABEL "Before Date" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

def var refresh-hist as log INITIAL yes 
     LABEL "Refresh History" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-history FOR 
      t-Zen-TaskHist SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-history
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-history window-maint _STRUCTURED
  QUERY br-history NO-LOCK DISPLAY
      t-Zen-TaskHist.TaskId FORMAT "999999":U
      t-Zen-TaskHist.TStatus FORMAT "X(8)":U
      t-Zen-TaskHist.cycle FORMAT "99999":U
      t-Zen-TaskHist.TaskType FORMAT "X(15)":U WIDTH 17.2
      t-Zen-TaskHist.SubmitDate FORMAT "99/99/9999":U
      string(t-Zen-TaskHist.SubmitTime,"hh:mm am") COLUMN-LABEL "At" FORMAT "x(8)":U
            WIDTH 9.8
      t-Zen-TaskHist.SubmitUser FORMAT "X(8)":U WIDTH 20.8
      t-Zen-TaskHist.RunDate FORMAT "99/99/9999":U
      string(t-Zen-TaskHist.RunTime, "hh:mm am") COLUMN-LABEL "At" FORMAT "x(8)":U
            WIDTH 9.8
      t-Zen-TaskHist.StartDate FORMAT "99/99/9999":U
      string(t-Zen-TaskHist.StartTime, "hh:mm am") COLUMN-LABEL "At" FORMAT "x(8)":U
            WIDTH 9.6
      t-Zen-TaskHist.EndDate FORMAT "99/99/9999":U
      string(t-Zen-TaskHist.EndTime,"hh:mm am") COLUMN-LABEL "At" FORMAT "x(8)":U
            WIDTH 21.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 171 BY 10.24 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     btn-Exit AT ROW 12.91 COL 4 HELP
          "Exit this maintenance program"
     br-history AT ROW 2.67 COL 4
     b-refresh AT ROW 13 COL 34
     b-delete AT ROW 13 COL 57.6
     b-restore AT ROW 13 COL 74.4
     b-details AT ROW 13 COL 92.2
     b-clear-hist AT ROW 13 COL 115
     vd-date AT ROW 13 COL 152.8 COLON-ALIGNED NO-LABEL
     refresh-hist AT ROW 13.14 COL 13.2
     lv-prior-date AT ROW 13.14 COL 137.8
     RECT-16 AT ROW 1.95 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 177.4 BY 14.14.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-Zen-TaskHist T "?" NO-UNDO schadm Zen-TaskHist
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Maintenance"
         HEIGHT             = 14.95
         WIDTH              = 180.2
         MAX-HEIGHT         = 30.95
         MAX-WIDTH          = 180.2
         VIRTUAL-HEIGHT     = 30.95
         VIRTUAL-WIDTH      = 180.2
         SHOW-IN-TASKBAR    = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
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
   FRAME-NAME                                                           */
/* BROWSE-TAB br-history RECT-16 frame-maint */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-history
/* Query rebuild information for BROWSE br-history
     _TblList          = "Temp-Tables.t-Zen-TaskHist"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.t-Zen-TaskHist.TaskId
     _FldNameList[2]   = Temp-Tables.t-Zen-TaskHist.TStatus
     _FldNameList[3]   = Temp-Tables.t-Zen-TaskHist.cycle
     _FldNameList[4]   > Temp-Tables.t-Zen-TaskHist.TaskType
"TaskType" ? "X(15)" "character" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.t-Zen-TaskHist.SubmitDate
     _FldNameList[6]   > "_<CALC>"
"string(t-Zen-TaskHist.SubmitTime,""hh:mm am"")" "At" "x(8)" ? ? ? ? ? ? ? no ? no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.t-Zen-TaskHist.SubmitUser
"SubmitUser" ? ? "character" ? ? ? ? ? ? no ? no no "20.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = Temp-Tables.t-Zen-TaskHist.RunDate
     _FldNameList[9]   > "_<CALC>"
"string(t-Zen-TaskHist.RunTime, ""hh:mm am"")" "At" "x(8)" ? ? ? ? ? ? ? no ? no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = Temp-Tables.t-Zen-TaskHist.StartDate
     _FldNameList[11]   > "_<CALC>"
"string(t-Zen-TaskHist.StartTime, ""hh:mm am"")" "At" "x(8)" ? ? ? ? ? ? ? no ? no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   = Temp-Tables.t-Zen-TaskHist.EndDate
     _FldNameList[13]   > "_<CALC>"
"string(t-Zen-TaskHist.EndTime,""hh:mm am"")" "At" "x(8)" ? ? ? ? ? ? ? no ? no no "21.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-history */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME b-clear-hist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-clear-hist window-maint
ON CHOOSE OF b-clear-hist IN FRAME frame-maint /* Clear Entire History */
DO:
   DEF VAR lv-ok AS LOG NO-UNDO.
   /* Are you sure you want to #1 task history #2 */
   IF lv-prior-date:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'No' THEN
   MESSAGE msg(229,'remove all','','','') 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lv-ok.
   ELSE
   do:
      IF date(vd-date:SCREEN-VALUE) <> ? THEN
      MESSAGE msg(229,'remove all','before ' + vd-date:SCREEN-VALUE,'','') 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lv-ok.
      ELSE
      DO:
         MESSAGE msg(33,'','','','') VIEW-AS ALERT-BOX ERROR.
         lv-ok = NO.
      END.
   END.

   IF lv-ok THEN
   DO:
       
       {{&core}run.i &program   = "zen-taskhist.p"
                    &path      = "{&tsk}{&srv}"
                    &noper     = true
                    &Appsrv    = "System"
                    &procedure = "clear-history"
                    &params    = "(lv-server,
                                   lv-prior-date:SCREEN-VALUE,
                                   date(vd-date:SCREEN-VALUE))"}
       
       RUN open-taskhist-query.

   END. /* delete */ 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-delete window-maint
ON CHOOSE OF b-delete IN FRAME frame-maint /* Delete History */
DO:
   DEF VAR lv-ok AS LOG NO-UNDO.

   MESSAGE msg(34,'you want to delete this task history','','','') 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lv-ok.

   IF lv-ok THEN
   DO:
       
       {{&core}run.i &program   = "zen-taskhist.p"
                    &path      = "{&tsk}{&srv}"
                    &noper     = true
                    &Appsrv    = "System"
                    &procedure = "delete-record"
                    &params    = "(t-zen-taskhist.zen-taskhisttableid, input-output table t-zen-taskhist)"}
       
       RUN open-taskhist-query.

   END. /* delete */ 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-details
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-details window-maint
ON CHOOSE OF b-details IN FRAME frame-maint /* Details */
DO:
    IF AVAIL t-zen-taskhist 
        THEN RUN {&tsk}taskhist-details.w(TABLE t-zen-taskhist, t-zen-taskhist.taskid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-refresh window-maint
ON CHOOSE OF b-refresh IN FRAME frame-maint /* Refresh */
DO:
   RUN open-taskhist-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-restore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-restore window-maint
ON CHOOSE OF b-restore IN FRAME frame-maint /* Restore Task */
DO:
   DEF VAR lv-task AS INT NO-UNDO.
   DEF VAR lv-ok AS LOG NO-UNDO.

   MESSAGE msg(56,'','','','')
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lv-ok.

   IF lv-ok THEN
   DO:
 
      {{&core}run.i  &program   = "zen-taskhist.p"
                    &path      = "{&tsk}{&srv}"
                    &noper     = true
                    &Appsrv    = "System"
                    &procedure = "restore-task"
                    &params    = "(t-zen-taskhist.zen-taskhisttableid, input table t-zen-taskhist,
                                   OUTPUT lv-task)"}
      
      IF lv-task = 0 THEN MESSAGE msg(57,'','','','') VIEW-AS ALERT-BOX ERROR.
      ELSE MESSAGE msg(58,string(lv-task),'','','') VIEW-AS ALERT-BOX INFO.
      
   END. /* ok to restore task from history */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-history
&Scoped-define SELF-NAME br-history
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-history window-maint
ON VALUE-CHANGED OF br-history IN FRAME frame-maint
DO:
  ASSIGN vr-taskhist = IF AVAIL t-zen-taskhist THEN t-zen-taskhist.zen-taskhisttableid ELSE ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Exit window-maint
ON CHOOSE OF btn-Exit IN FRAME frame-maint /* Exit */
do:    
   RUN Exit-trigger.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-prior-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-prior-date window-maint
ON VALUE-CHANGED OF lv-prior-date IN FRAME frame-maint /* Before Date */
DO:
   vd-date:SENSITIVE = self:SCREEN-VALUE = 'yes'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{{&core}commonmaint.i}
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
  DISPLAY vd-date refresh-hist lv-prior-date 
      WITH FRAME frame-maint.
  ENABLE btn-Exit RECT-16 br-history b-refresh b-delete b-restore b-details 
         b-clear-hist vd-date refresh-hist lv-prior-date 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-taskhist-query window-maint 
PROCEDURE open-taskhist-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   empty temp-table t-zen-taskhist.

   {{&core}run.i &program   = "zen-taskhist.p"
                &path      = "{&tsk}{&srv}"
                &noper     = true
                &Appsrv    = "System"
                &procedure = "open-query"
                &params    = "(lv-server, output table t-zen-taskhist)"} 

   {&open-query-br-history}

   IF br-history:NUM-ITERATIONS IN FRAME {&FRAME-NAME} >= 1 THEN 
   ASSIGN b-delete:SENSITIVE  = YES
          b-restore:SENSITIVE = YES
          b-details:SENSITIVE = YES
          vr-taskhist         = IF vr-taskhist = ? THEN t-zen-taskhist.zen-taskhisttableid ELSE vr-taskhist.
   ELSE ASSIGN b-delete:SENSITIVE  = NO
               b-restore:SENSITIVE = NO
               b-details:SENSITIVE = NO
               vr-taskhist = ?.

  IF vr-taskhist <> ? THEN
  do:
      FIND FIRST t-zen-taskhist WHERE t-zen-taskhist.zen-taskhisttableid = vr-taskhist NO-LOCK NO-ERROR.
       
      IF AVAIL t-zen-taskhist THEN REPOSITION br-history TO ROWID ROWID(t-zen-taskhist) NO-ERROR.
      ELSE 
      do:
    
         IF br-history:NUM-ITERATIONS >= 1 THEN
         DO:
            REPOSITION br-history TO ROW 1.
            APPLY 'value-changed' TO br-history.
         END.
         ELSE ASSIGN vr-taskhist = t-zen-taskhist.zen-taskhisttableid.
    
      END. /* avail t-zen-taskhist */

  END. /* recid <> ? */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh window-maint 
PROCEDURE refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pv-server AS CHAR NO-UNDO.

   ASSIGN lv-server = pv-server.

   IF refresh-hist:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'yes' THEN
   RUN open-taskhist-query.
    
   APPLY 'value-changed' TO lv-prior-date IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

