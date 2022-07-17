&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-Task NO-UNDO LIKE Zen-Task.
DEFINE TEMP-TABLE t-Zen-Tserver NO-UNDO LIKE Zen-Tserver.



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

&glob table-name zen-tserver
&glob unique-key {&table-name}tableid
&global-define suppresswindow
&global-define nobuttons

DEF VAR vr-tserver like zen-tserver.zen-tservertableid NO-UNDO. /* rowid of task server we are on */
DEF VAR vr-task    like zen-task.zen-tasktableid NO-UNDO. /* rowid of task we are on */

def var ch-timerservers as com-handle no-undo.
def var ch-timertasks as com-handle no-undo.
def var ch-spinservers as com-handle no-undo.
def var ch-spintasks as com-handle no-undo.

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
&Scoped-define INTERNAL-TABLES t-Zen-Tserver t-Zen-Task

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-Zen-Tserver.TaskServer ~
t-Zen-Tserver.AutoStart t-Zen-Tserver.Started t-Zen-Tserver.TStatus ~
t-Zen-Tserver.catchup t-Zen-Tserver.StartDate ~
string(t-Zen-Tserver.StartTime, "hh:mm am") t-Zen-Tserver.StartUser ~
t-Zen-Tserver.pid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-Zen-Tserver NO-LOCK ~
    BY t-Zen-Tserver.TaskServer
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-Zen-Tserver NO-LOCK ~
    BY t-Zen-Tserver.TaskServer.
&Scoped-define TABLES-IN-QUERY-br-maint t-Zen-Tserver
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-Zen-Tserver


/* Definitions for BROWSE br-task                                       */
&Scoped-define FIELDS-IN-QUERY-br-task t-Zen-Task.TaskId ~
string(t-Zen-Task.SubmitTime,'hh:mm am') t-Zen-Task.SubmitUser ~
t-Zen-Task.SubmitDate t-Zen-Task.TStatus t-Zen-Task.RunDate ~
string(t-Zen-Task.RunTime,'hh:mm am') t-Zen-Task.pid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-task 
&Scoped-define QUERY-STRING-br-task FOR EACH t-Zen-Task NO-LOCK ~
    BY t-Zen-Task.RunDate ~
       BY t-Zen-Task.RunTime INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-task OPEN QUERY br-task FOR EACH t-Zen-Task NO-LOCK ~
    BY t-Zen-Task.RunDate ~
       BY t-Zen-Task.RunTime INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-task t-Zen-Task
&Scoped-define FIRST-TABLE-IN-QUERY-br-task t-Zen-Task


/* Definitions for FRAME frame-maint                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-frame-maint ~
    ~{&OPEN-QUERY-br-task}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-16 RECT-17 br-maint lv-force btn-help ~
b-select b-refreshservers refresh-lines br-task b-schedule b-details ~
b-delete b-new b-history b-refreshtasks btn-now lv-forcedelete btn-Exit ~
lv-server-title lv-task-title 
&Scoped-Define DISPLAYED-OBJECTS lv-force refresh-lines lv-forcedelete ~
lv-server-title lv-task-title 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-delete 
     LABEL "Delete Task" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-details 
     LABEL "Details" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-history 
     LABEL "Task History" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-logfile 
     LABEL "Log" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-new 
     LABEL "New Task" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-refreshservers 
     LABEL "Refresh" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-refreshtasks 
     LABEL "Refresh" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-schedule 
     LABEL "Schedule" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-select 
     LABEL "Start/Stop" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-Exit 
     IMAGE-UP FILE "{&core}grafix/exit-su.ico":U NO-FOCUS
     LABEL "E&xit":L 
     SIZE 5.6 BY 1.33 TOOLTIP "Exit".

DEFINE BUTTON btn-help 
     IMAGE-UP FILE "{&core}grafix/help-su.ico":U NO-FOCUS
     LABEL "help":L 
     SIZE 5.6 BY 1.33 TOOLTIP "Exit".

DEFINE BUTTON btn-now 
     LABEL "Run Now" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-server-title AS CHARACTER FORMAT "X(256)":U INITIAL "Servers" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE lv-task-title AS CHARACTER FORMAT "X(256)":U INITIAL "Current Tasks" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 135 BY 6.14.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 134 BY 13.29.

DEFINE VARIABLE lv-force AS LOGICAL INITIAL no 
     LABEL "Force" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .76 NO-UNDO.

DEFINE VARIABLE lv-forcedelete AS LOGICAL INITIAL no 
     LABEL "force delete" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.6 BY 1 NO-UNDO.

DEFINE VARIABLE refresh-lines AS LOGICAL INITIAL yes 
     LABEL "Refresh Lines" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .67 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-Zen-Tserver SCROLLING.

DEFINE QUERY br-task FOR 
      t-Zen-Task SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-Zen-Tserver.TaskServer FORMAT "X(20)":U WIDTH 20.2
      t-Zen-Tserver.AutoStart
      t-Zen-Tserver.Started
      t-Zen-Tserver.TStatus
      t-Zen-Tserver.catchup
      t-Zen-Tserver.StartDate COLUMN-LABEL "Started On" WIDTH 13.2
      string(t-Zen-Tserver.StartTime, "hh:mm am") COLUMN-LABEL "Start Time" FORMAT "x(8)":U
      t-Zen-Tserver.StartUser COLUMN-LABEL "User" WIDTH 13.6
      t-Zen-Tserver.pid
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 110 BY 4.95.

DEFINE BROWSE br-task
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-task window-maint _STRUCTURED
  QUERY br-task NO-LOCK DISPLAY
      t-Zen-Task.TaskId
      string(t-Zen-Task.SubmitTime,'hh:mm am') COLUMN-LABEL "Submit Time" FORMAT "x(8)":U
            WIDTH 13.6
      t-Zen-Task.SubmitUser WIDTH 13.6
      t-Zen-Task.SubmitDate
      t-Zen-Task.TStatus
      t-Zen-Task.RunDate
      string(t-Zen-Task.RunTime,'hh:mm am') COLUMN-LABEL "Run Time" FORMAT "x(8)":U
      t-Zen-Task.pid WIDTH 29.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 111 BY 12.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     br-maint AT ROW 1.52 COL 23 HELP
          "Select the record to edit."
     lv-force AT ROW 1.95 COL 6
     btn-help AT ROW 19.1 COL 14 HELP
          "Exit this maintenance program" WIDGET-ID 2
     b-select AT ROW 2.91 COL 5
     b-logfile AT ROW 4.05 COL 5
     b-refreshservers AT ROW 5.19 COL 5
     refresh-lines AT ROW 6.48 COL 4
     br-task AT ROW 8.24 COL 23
     b-schedule AT ROW 9.24 COL 5
     b-details AT ROW 10.38 COL 5
     b-delete AT ROW 11.52 COL 5
     b-new AT ROW 12.67 COL 5
     b-history AT ROW 13.76 COL 5
     b-refreshtasks AT ROW 14.91 COL 5
     btn-now AT ROW 16.14 COL 4.8 WIDGET-ID 4
     lv-forcedelete AT ROW 17.43 COL 4
     btn-Exit AT ROW 19.1 COL 5 HELP
          "Exit this maintenance program"
     lv-server-title AT ROW 1 COL 2 COLON-ALIGNED NO-LABEL
     lv-task-title AT ROW 7.43 COL 2 COLON-ALIGNED NO-LABEL
     RECT-16 AT ROW 1.29 COL 1
     RECT-17 AT ROW 7.67 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 137 BY 21.91
         TITLE "TaskServers And Tasks".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-Zen-Task T "?" NO-UNDO schadm Zen-Task
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
         TITLE              = "Maintenance"
         HEIGHT             = 23.33
         WIDTH              = 141.4
         MAX-HEIGHT         = 33.14
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.14
         VIRTUAL-WIDTH      = 204.8
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
/* BROWSE-TAB br-maint RECT-17 frame-maint */
/* BROWSE-TAB br-task refresh-lines frame-maint */
/* SETTINGS FOR BUTTON b-logfile IN FRAME frame-maint
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-Zen-Tserver"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _OrdList          = "Temp-Tables.t-Zen-Tserver.TaskServer|yes"
     _FldNameList[1]   > Temp-Tables.t-Zen-Tserver.TaskServer
"t-Zen-Tserver.TaskServer" ? "X(20)" "character" ? ? ? ? ? ? no ? no no "20.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.t-Zen-Tserver.AutoStart
     _FldNameList[3]   = Temp-Tables.t-Zen-Tserver.Started
     _FldNameList[4]   = Temp-Tables.t-Zen-Tserver.TStatus
     _FldNameList[5]   = Temp-Tables.t-Zen-Tserver.catchup
     _FldNameList[6]   > Temp-Tables.t-Zen-Tserver.StartDate
"t-Zen-Tserver.StartDate" "Started On" ? "date" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"string(t-Zen-Tserver.StartTime, ""hh:mm am"")" "Start Time" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.t-Zen-Tserver.StartUser
"t-Zen-Tserver.StartUser" "User" ? "character" ? ? ? ? ? ? no ? no no "13.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = Temp-Tables.t-Zen-Tserver.pid
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-task
/* Query rebuild information for BROWSE br-task
     _TblList          = "Temp-Tables.t-Zen-Task"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.t-Zen-Task.RunDate|yes,Temp-Tables.t-Zen-Task.RunTime|yes"
     _FldNameList[1]   = Temp-Tables.t-Zen-Task.TaskId
     _FldNameList[2]   > "_<CALC>"
"string(t-Zen-Task.SubmitTime,'hh:mm am')" "Submit Time" "x(8)" ? ? ? ? ? ? ? no ? no no "13.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.t-Zen-Task.SubmitUser
"t-Zen-Task.SubmitUser" ? ? "character" ? ? ? ? ? ? no ? no no "13.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.t-Zen-Task.SubmitDate
     _FldNameList[5]   = Temp-Tables.t-Zen-Task.TStatus
     _FldNameList[6]   = Temp-Tables.t-Zen-Task.RunDate
     _FldNameList[7]   > "_<CALC>"
"string(t-Zen-Task.RunTime,'hh:mm am')" "Run Time" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.t-Zen-Task.pid
"t-Zen-Task.pid" ? ? "integer" ? ? ? ? ? ? no ? no no "29.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-task */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME b-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-delete window-maint
ON CHOOSE OF b-delete IN FRAME frame-maint /* Delete Task */
DO:
   DEF VAR lv-ok AS LOG NO-UNDO.

   MESSAGE msg(34,'you want to delete this task','','','') 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lv-ok.

/*    RUN open-task-query.  */

   IF lv-ok THEN
       IF t-zen-task.tstatus <> '{&trunning}' or
          lv-forcedelete:CHECKED 
       THEN DO:
           {{&core}run.i &program   = "zen-task.p"
                        &path      = "{&tsk}{&srv}"
                        &noper     = true
                        &Appsrv    = "System"
                        &procedure = "delete-record"
                        &params    = "(t-zen-task.zen-tasktableid)"}
           
           RUN open-task-query.
       END. /* delete */ 
       ELSE MESSAGE msg(174,'Running','Delete.','','') VIEW-AS ALERT-BOX INFO.
            /* Task is #1, unable to #2 */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-details
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-details window-maint
ON CHOOSE OF b-details IN FRAME frame-maint /* Details */
DO:
   IF AVAIL t-zen-task THEN 
    RUN {&tsk}task-details.w (TABLE t-zen-task, t-zen-task.taskid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-history
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-history window-maint
ON CHOOSE OF b-history IN FRAME frame-maint /* Task History */
DO:
   runchild('{&tsk}taskserverhist.w',this-procedure).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-logfile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-logfile window-maint
ON CHOOSE OF b-logfile IN FRAME frame-maint /* Log */
DO:

  /* close the output on taskserver - copy log - then open again with append
     means we can see the actual taskserver.log file when the taskserver is running */

  {{&core}run.i &program   = "zen-tserver.p"
               &path      = "{&tsk}{&srv}"
               &noper     = true
               &Appsrv    = "System"
               &procedure = "log-file"
               &params    = "(t-zen-tserver.taskserver)"}
               


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-new
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-new window-maint
ON CHOOSE OF b-new IN FRAME frame-maint /* New Task */
DO: 
   runchild('{&tsk}submit-task-np.w',this-procedure).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-refreshservers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-refreshservers window-maint
ON CHOOSE OF b-refreshservers IN FRAME frame-maint /* Refresh */
DO:
 run refreshservers.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-refreshtasks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-refreshtasks window-maint
ON CHOOSE OF b-refreshtasks IN FRAME frame-maint /* Refresh */
DO:
  APPLY 'value-changed' TO br-maint IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-schedule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-schedule window-maint
ON CHOOSE OF b-schedule IN FRAME frame-maint /* Schedule */
DO:
   runchild('{&tsk}schedule.w',this-procedure).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-select window-maint
ON CHOOSE OF b-select IN FRAME frame-maint /* Start/Stop */
DO:
/*    APPLY 'value-changed' TO br-maint.  */
   IF AVAIL t-zen-tserver AND 
            t-zen-tserver.tstatus <> '{&tstopped}' AND
            t-zen-tserver.started = NO and 
            not lv-force:checked in frame {&frame-name}
    THEN DO: /* TaskServer must be #1 before #2 */
      MESSAGE msg(230,'stopped','it can be started','','').
      RETURN NO-APPLY.
   END.
   ELSE DO:
      if not RunRemote() then do:
         message 'This Server will stop when you log off' skip
                 'Use an Appserver Session to start "permanant" servers' skip
                 'Continue ? '
         view-as alert-box question buttons yes-no update lv-ok as log.
         if not lv-ok then return no-apply.
      end.
      vr-tserver = IF AVAIL t-zen-tserver THEN t-zen-tserver.{&unique-key} 
                                          ELSE ?.
       {{&core}run.i &program   = "zen-tserver.p"
                    &path      = "{&tsk}{&srv}"
                    &Appsrv    = "System"
                    &noper     = true
                    &procedure = "start-stop"
                    &params    = "(t-zen-tserver.taskserver,
                                   lv-force:checked)"}
       
       RUN open-tserver-query.

       FIND FIRST t-zen-tserver WHERE t-zen-tserver.zen-tservertableid = vr-tserver
                                NO-LOCK NO-ERROR.
       IF AVAIL t-zen-tserver 
       THEN REPOSITION br-maint TO ROWID rowid(t-zen-tserver).

       RUN set-server-buttons.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&Scoped-define SELF-NAME br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-maint window-maint
ON VALUE-CHANGED OF br-maint IN FRAME frame-maint
DO:
   vr-tserver = IF AVAIL t-zen-tserver THEN t-zen-tserver.{&unique-key} 
                                       ELSE ?.
   RUN set-server-buttons.
   RUN open-task-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-task
&Scoped-define SELF-NAME br-task
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-task window-maint
ON VALUE-CHANGED OF br-task IN FRAME frame-maint
DO:
    
 vr-task = IF AVAIL t-zen-task THEN t-zen-task.zen-tasktableid ELSE ?.
   
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


&Scoped-define SELF-NAME btn-help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-help window-maint
ON CHOOSE OF btn-help IN FRAME frame-maint /* help */
do:    
    RUN help-trigger.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-now
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-now window-maint
ON CHOOSE OF btn-now IN FRAME frame-maint /* Run Now */
DO:
errorclear().

  {{&core}run.i &program   = "zen-task.p"
             &path      = "{&tsk}{&srv}"
             &Appsrv    = "System"
             &noper     = true
             &procedure = "update-schedule"
             &params    = "(t-zen-task.zen-tasktableid,
                            'asap',  
                            today, 
                            time)"} 
if anyerrors() then .
                            
apply 'choose' to b-refreshtasks.                            

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

{{&core}commonmaint.i &path = "{&tsk}{&srv}"}   

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
 
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {{&core}sec-chk.i}
  RUN enable_UI.

  RUN initialise.
  {{&core}wid-chk.i}
  {{&core}focus.i}
 
  APPLY "VALUE-CHANGED":U TO {&BROWSE-NAME} IN FRAME {&frame-name}.
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
  DISPLAY lv-force refresh-lines lv-forcedelete lv-server-title lv-task-title 
      WITH FRAME frame-maint.
  ENABLE RECT-16 RECT-17 br-maint lv-force btn-help b-select b-refreshservers 
         refresh-lines br-task b-schedule b-details b-delete b-new b-history 
         b-refreshtasks btn-now lv-forcedelete btn-Exit lv-server-title 
         lv-task-title 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls window-maint 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* assign                                        */
/*    ch-timerservers = chCtrlFrame:PSTimer      */
/*    ch-timertasks   = chCtrlFrame-2:Tasktimer  */
/*    ch-spinservers  = chCtrlFrame-3:CSSpin     */
/*    ch-spintasks    = chCtrlFrame-4:CSSpin.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn window-maint 
PROCEDURE Local-ChildReturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-from as char no-undo.
def var h-from as handle no-undo.

h-from = widget-handle(pv-from).

case h-from:private-data:
   when '{&tsk}schedule.w' then do:
      def var lv-sched as char no-undo.
      run SendResult in widget-handle(pv-from) (output lv-sched).
      if lv-sched ne 'quit' 
      then RUN schedule-refresh (lv-sched).
   end.
   when '{&tsk}submit-task-np.w' 
   then RUN open-task-query.
end case.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise window-maint 
PROCEDURE local-initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN open-tserver-query.
   RUN set-server-buttons.
   RUN open-task-query.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-child-procedures window-maint 
PROCEDURE local-update-child-procedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-child as handle no-undo.

case pv-child:private-data:
   when '{&tsk}taskserverhist.w' or 
   when '{&tsk}submit-task-np.w' then 
      RUN refresh IN pv-child (t-zen-tserver.taskserver).
   when '{&tsk}schedule.w' then
      RUN schedule-to-screen IN pv-child (t-zen-task.schedule).
end case.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-task-query window-maint 
PROCEDURE open-task-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF AVAIL t-zen-tserver 
THEN DO:
   empty temp-table t-zen-task.
   {{&core}run.i &program   = "zen-task.p"
                &path      = "{&tsk}{&srv}"
                &noper     = true
                &Appsrv    = "System"
                &procedure = "open-query"
                &params    = "(input t-zen-tserver.taskserver,
                               output table t-zen-task)"} 
END.
    
{&open-query-br-task}

IF br-task:NUM-ITERATIONS IN FRAME {&FRAME-NAME} >= 1 
THEN ASSIGN b-delete:SENSITIVE   = YES
            b-schedule:SENSITIVE = YES
            b-details:SENSITIVE  = YES
            vr-task = IF vr-task = ? THEN t-zen-task.zen-tasktableid ELSE vr-task.
ELSE ASSIGN b-delete:SENSITIVE   = NO
            b-schedule:SENSITIVE = NO
            b-details:SENSITIVE  = NO
            vr-task = ?.

IF vr-task <> ? 
THEN do:
   FIND FIRST t-zen-task WHERE t-zen-task.zen-tasktableid = vr-task NO-LOCK NO-ERROR.
   IF AVAIL t-zen-task 
   THEN REPOSITION br-task TO ROWID ROWID(t-zen-task) NO-ERROR.
   ELSE do:
      IF br-task:NUM-ITERATIONS >= 1 
      THEN DO:
         REPOSITION br-task TO ROW 1.
         APPLY 'value-changed' TO br-task.
      END.
      ELSE vr-task = t-zen-task.zen-tasktableid.
   END. /* avail t-zen-task */
END. /* recid <> ? */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-tserver-query window-maint 
PROCEDURE open-tserver-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

empty temp-table t-zen-tserver.

{{&core}run.i &program   = "zen-tserver.p"
             &path      = "{&tsk}{&srv}"
             &noper     = true
             &Appsrv    = "System"
             &procedure = "open-query"
             &params    = "(output table t-zen-tserver)"} 

{&open-query-br-maint}

IF br-maint:NUM-ITERATIONS IN FRAME {&FRAME-NAME} >= 1 
THEN DO:
   IF vr-tserver <> ? 
   THEN FIND FIRST t-zen-tserver WHERE t-zen-tserver.zen-tservertableid = vr-tserver NO-LOCK NO-ERROR.
   IF AVAIL t-zen-tserver THEN REPOSITION br-maint TO ROWID ROWID(t-zen-tserver).
END.
ELSE ASSIGN b-select:SENSITIVE  = NO
            b-new:SENSITIVE     = NO
            b-history:SENSITIVE = NO
            b-refreshtasks:SENSITIVE = NO.

/* IF NOT SESSION:PARAMETER BEGINS 'appserver' THEN ASSIGN b-select:SENSITIVE = NO.  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshServers window-maint 
PROCEDURE RefreshServers :
IF AVAIL t-zen-tserver AND (t-zen-tserver.started OR 
                            t-zen-tserver.tstatus = '{&tstopping}') 
THEN vr-tserver = IF AVAIL t-zen-tserver THEN t-zen-tserver.zen-tservertableid 
                                         ELSE ?.
RUN open-tserver-query.
if vr-tserver ne ? 
then FIND FIRST t-zen-tserver WHERE t-zen-tserver.zen-tservertableid = vr-tserver 
                     NO-LOCK NO-ERROR.
else find first t-zen-tserver no-error.

IF AVAIL t-zen-tserver 
THEN REPOSITION br-maint TO ROWID rowid(t-zen-tserver) NO-ERROR.

RUN set-server-buttons.

IF REFRESH-lines:checked IN FRAME {&FRAME-NAME} 
THEN RUN open-task-query.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE schedule-refresh window-maint 
PROCEDURE schedule-refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER v-schedule AS CHAR NO-UNDO.

DEF var lv-new-sched  AS CHAR    NO-UNDO. /* schedule line to screen or record format */
def var lv-run-date   as date    no-undo. /* next run date and time */
def var lv-run-time   as int no-undo. 
def var lv-ended      as log no-undo. /* has the schedule ended */ 
def var lv-problem    as log no-undo. /* problem interpriting schedule line */
def var lv-ending     as log no-undo. /* will this schedule end itself? */
DEF var lv-iterate    as log NO-UNDO. /* is there a time iteration ncrease input time */
DEF VAR lv-ok AS LOG NO-UNDO.

MESSAGE msg(34,'you want to update this tasks schedule','','','') 
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lv-ok.

RUN open-task-query.

IF lv-ok 
THEN IF t-zen-task.tstatus <> '{&trunning}' 
     THEN DO:
          {{&core}run.i &program   = "schedule-parser.p"
                       &path      = "{&tsk}"
                       &Appsrv    = "System"
                       &noper     = true
                       &procedure = "refresh"
                       &params    = "(v-schedule,
                                  TODAY,
                                  TIME,
                                  NO,
                                  OUTPUT lv-new-sched,
                                  OUTPUT lv-run-date,
                                  OUTPUT lv-run-time,
                                  OUTPUT lv-ended,
                                  OUTPUT lv-problem,
                                  OUTPUT lv-ending,
                                  OUTPUT lv-iterate)"}
/*           /* for speed */      */
/*           PAUSE 1 NO-MESSAGE.  */
          
          MESSAGE 'Next Run Date: ' lv-run-date SKIP
                  'Next Run Time: ' STRING(lv-run-time,'hh:mm am') 
          VIEW-AS ALERT-BOX INFO.
      
          run UpdateSchedule(t-zen-task.zen-tasktableid, 
                             v-schedule, lv-run-date, lv-run-time).
          
          RUN open-task-query.
      END. /* update schedule on task */
      ELSE MESSAGE msg(174,'Running','Update Schedule.','','') 
           VIEW-AS ALERT-BOX INFO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-server-buttons window-maint 
PROCEDURE set-server-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF AVAIL t-zen-tserver 
THEN DO:
   IF t-zen-tserver.started 
      THEN b-select:LABEL IN FRAME {&FRAME-NAME} = 'Stop'.
      ELSE b-select:LABEL = 'Start'.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateSchedule window-maint 
PROCEDURE UpdateSchedule :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-tasktableid as dec no-undo.
def input param pv-schedule as char no-undo.
def input param pv-run-date as date no-undo.
def input param pv-run-time  as int no-undo.

    {{&core}run.i &program   = "zen-task.p"
                 &path      = "{&tsk}{&srv}"
                 &Appsrv    = "System"
                 &noper     = true
                 &procedure = "update-schedule"
                 &params    = "(pv-tasktableid, 
                                pv-schedule, 
                                pv-run-date, 
                                pv-run-time)"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

