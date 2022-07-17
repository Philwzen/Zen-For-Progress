&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME window-maint
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
def var lv-type as char no-undo.
def var lv-params as char no-undo.

   def var lv-schedule as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frame-maint

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Cancel RECT-17 RECT-3 RECT-4 lv-submit ~
cb-taskserver vd-date lv-time lv-quick lv-detailed b-schedule b-submit ~
lv-title lv-title2 
&Scoped-Define DISPLAYED-OBJECTS lv-submit cb-taskserver vd-date lv-time ~
lv-quick lv-detailed vd-r-date lv-r-time lv-title lv-title2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-submit cb-taskserver vd-date lv-time lv-quick ~
lv-detailed vd-r-date lv-r-time 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-schedule 
     LABEL "Detailed Schedule" 
     SIZE 23.2 BY 1.14.

DEFINE BUTTON b-submit AUTO-GO  NO-FOCUS
     LABEL "Submit" 
     SIZE 17.2 BY 1.14 TOOLTIP "Ok"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancel AUTO-GO 
     IMAGE-UP FILE "{&core}grafix/exit-su.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 6.4 BY 1.24 TOOLTIP "Exit"
     BGCOLOR 8 .

def var cb-taskserver as char FORMAT "X(256)":U 
     LABEL "Task Server" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

def var lv-r-time as char FORMAT "xx:xx":U 
     LABEL "Estimated Run Time" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

def var lv-submit as char FORMAT "X(256)":U 
     LABEL "Submit Task" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

def var lv-time as char FORMAT "xx:xx":U 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

def var lv-title as char FORMAT "X(256)":U INITIAL "Quick Schedule" 
      VIEW-AS TEXT 
     SIZE 15.8 BY .62 NO-UNDO.

def var lv-title2 as char FORMAT "X(256)":U INITIAL "Current Run Info" 
      VIEW-AS TEXT 
     SIZE 17 BY .62 NO-UNDO.

def var vd-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

def var vd-r-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Estimated Run Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 3.29.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 1.91.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 3.33.

def var lv-detailed as log INITIAL no 
     LABEL "Detailed Schedule" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

def var lv-quick as log INITIAL no 
     LABEL "Quick Schedule" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     Btn_Cancel AT ROW 16.33 COL 3
     lv-submit AT ROW 1.43 COL 15 COLON-ALIGNED
     cb-taskserver AT ROW 2.81 COL 15 COLON-ALIGNED
     vd-date AT ROW 5.91 COL 15 COLON-ALIGNED
     lv-time AT ROW 6.86 COL 15 COLON-ALIGNED
     lv-quick AT ROW 9.05 COL 17
     lv-detailed AT ROW 9.86 COL 17
     vd-r-date AT ROW 12.48 COL 25.4 COLON-ALIGNED
     lv-r-time AT ROW 13.48 COL 25.4 COLON-ALIGNED
     b-schedule AT ROW 16.38 COL 30.8
     b-submit AT ROW 16.38 COL 13.6
     lv-title AT ROW 4.71 COL 4.2 COLON-ALIGNED NO-LABEL
     lv-title2 AT ROW 11.43 COL 4.2 COLON-ALIGNED NO-LABEL
     RECT-17 AT ROW 11.81 COL 5
     RECT-3 AT ROW 15.95 COL 11.6
     RECT-4 AT ROW 5.1 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.6 BY 16.86.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Maintenance"
         HEIGHT             = 17.1
         WIDTH              = 64
         MAX-HEIGHT         = 30.95
         MAX-WIDTH          = 159.8
         VIRTUAL-HEIGHT     = 30.95
         VIRTUAL-WIDTH      = 159.8
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME frame-maint
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX cb-taskserver IN FRAME frame-maint
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-detailed IN FRAME frame-maint
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-quick IN FRAME frame-maint
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-r-time IN FRAME frame-maint
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-submit IN FRAME frame-maint
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-time IN FRAME frame-maint
   1                                                                    */
/* SETTINGS FOR FILL-IN vd-date IN FRAME frame-maint
   1                                                                    */
/* SETTINGS FOR FILL-IN vd-r-date IN FRAME frame-maint
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(window-maint)
THEN window-maint:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME b-schedule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-schedule window-maint
ON CHOOSE OF b-schedule IN FRAME frame-maint /* Detailed Schedule */
DO:
   runchild('{&tsk}schedule.w',this-procedure).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-submit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-submit window-maint
ON CHOOSE OF b-submit IN FRAME frame-maint /* Submit */
DO:
   /* at the moment is simple need to do a bit more checking -
      works 'bare-bones' though */
    aSSIGN FRAME {&FRAME-NAME} {&list-1}.

    DEF VAR lv-task-no AS INT NO-UNDO.

    IF lv-quick THEN DO: /* build up a basic eg. quick schedule line for the task */
        IF date(vd-date:SCREEN-VALUE IN frame {&FRAME-NAME}) = ? AND
           TRIM(lv-time:SCREEN-VALUE) = ':' 
        THEN lv-schedule = ''.
        ELSE IF date(vd-date:SCREEN-VALUE) <> ? 
             THEN DO:
                lv-schedule = 's-dates ' + vd-date:SCREEN-VALUE.
                IF TRIM(lv-time:SCREEN-VALUE) <> ':' 
                THEN lv-schedule = lv-schedule + '{&Delim2}' + 's-time ' + lv-time:SCREEN-VALUE.
             END. /* date and ?time? */
             ELSE lv-schedule = 's-time ' + lv-time:SCREEN-VALUE.
 
       run ScheduleParser(lv-schedule).
       case return-value:
          when 'problem' THEN DO:
             MESSAGE 'There is a problem with the Syntax of the Schedule' VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
          END.
          when 'ended'  THEN do:
             MESSAGE 'This schedule has already ended!' VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
          END.
          otherwise do:
            MESSAGE 'Syntax is correct' VIEW-AS ALERT-BOX INFO.
          end.
       end case.
    END. /* set quick schedule to lv-schedule */

   {{&core}run.i &program   = "zen-task.p"
                &path      = "{&tsk}{&srv}"
                &noper     = true
                &Appsrv    = "System"
                &procedure = "submit-task"
                &params    = "(lv-submit:private-data,
                               cb-taskserver:SCREEN-VALUE,
                               vd-r-date,
                               lv-r-time,
                               lv-schedule,
                               lv-type,
                               lv-params,
                               output lv-task-no)"} 

   /* task number #1 has been submitted to the taskserver #2 */   
   MESSAGE msg(173,string(lv-task-no),string('"' + cb-taskserver:SCREEN-VALUE + '"'),'','') VIEW-AS ALERT-BOX INFO.
   
   APPLY 'close' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel window-maint
ON CHOOSE OF Btn_Cancel IN FRAME frame-maint /* Cancel */
DO:
run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-detailed window-maint
ON VALUE-CHANGED OF lv-detailed IN FRAME frame-maint /* Detailed Schedule */
DO:
   IF lv-detailed:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'no' THEN
   ASSIGN lv-detailed:SENSITIVE = NO
          lv-quick:SCREEN-VALUE    = 'yes'
          lv-quick:SENSITIVE = YES
          b-schedule:SENSITIVE = NO
          vd-date:SENSITIVE = YES
          lv-time:SENSITIVE = YES
          b-submit:SENSITIVE = YES.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-quick
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-quick window-maint
ON VALUE-CHANGED OF lv-quick IN FRAME frame-maint /* Quick Schedule */
DO:
   IF lv-quick:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'no' THEN
   ASSIGN lv-detailed:SCREEN-VALUE = 'YES'
          lv-detailed:SENSITIVE = YES
          lv-quick:SCREEN-VALUE    = 'no'
          lv-quick:SENSITIVE = NO
          b-schedule:SENSITIVE = YES
          b-submit:SENSITIVE = NO
          vd-date:SENSITIVE = NO
          lv-time:SENSITIVE = NO.
   ELSE
   ASSIGN lv-detailed:SENSITIVE = NO
          b-schedule:SENSITIVE = NO.
          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-time window-maint
ON LEAVE OF lv-time IN FRAME frame-maint /* Time */
DO:
   
   ASSIGN lv-r-time:SCREEN-VALUE IN FRAME {&FRAME-NAME} = lv-time:SCREEN-VALUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vd-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vd-date window-maint
ON LEAVE OF vd-date IN FRAME frame-maint /* Date */
DO:

   ASSIGN vd-r-date:SCREEN-VALUE IN FRAME {&FRAME-NAME} = vd-date:SCREEN-VALUE.
          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */
 
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*{{&sys}btn-help.i}*/

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{{&core}commonmaint.i}

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
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(window-maint)
  THEN DELETE WIDGET window-maint.
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
  DISPLAY lv-submit cb-taskserver vd-date lv-time lv-quick lv-detailed vd-r-date 
          lv-r-time lv-title lv-title2 
      WITH FRAME frame-maint IN WINDOW window-maint.
  ENABLE Btn_Cancel RECT-17 RECT-3 RECT-4 lv-submit cb-taskserver vd-date 
         lv-time lv-quick lv-detailed b-schedule b-submit lv-title lv-title2 
      WITH FRAME frame-maint IN WINDOW window-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
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

def var lv-sched as char no-undo.

run SendResult in widget-handle(pv-from) (output lv-sched).

RUN scheduleparser (lv-sched).

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

BuildCombo(cb-taskserver:handle in frame {&FRAME-NAME},
           "zen-tserver",
           "tdefault",
           "taskserver",
           "",
           "",
           NO,
           No).

ASSIGN cb-taskserver:screen-value in frame {&FRAME-NAME} = setcombovalue('YES',cb-taskserver:handle)
       lv-quick:SCREEN-VALUE = 'YES'
       vd-r-date:SCREEN-VALUE = STRING(TODAY,'99/99/9999')
       lv-r-time:SCREEN-VALUE = STRING(TIME,'hh:mm').

APPLY 'VALUE-CHANGED' TO lv-quick IN FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Update-Child-Procedures window-maint 
PROCEDURE Local-Update-Child-Procedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param pv-child as handle no-undo.

case pv-child:private-data:
   when '{&tsk}schedule.w' then do:
       RUN schedule-to-screen IN pv-child (lv-schedule).
   end.
end case.
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

DEFINE INPUT PARAMETER lv-prog     AS CHAR NO-UNDO. /* description of program being submitted */
DEFINE INPUT PARAMETER lv-runprog  AS CHAR NO-UNDO. /* .p to run in extractdata in */
DEFINE INPUT PARAMETER lv-progtype AS CHAR NO-UNDO. /* type of program eg. report etc. */
DEFINE INPUT PARAMETER pv-params   AS CHAR NO-UNDO. /* program parameters */

ASSIGN lv-submit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = lv-prog
       lv-submit:PRIVATE-DATA = lv-runprog
       lv-type   = lv-progtype
       lv-params = pv-params.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScheduleParser window-maint 
PROCEDURE ScheduleParser :
def input param pv-sched as char no-undo.
                 
DEF var lv-new-sched  AS CHAR    NO-UNDO. /* schedule line to screen or record format */
def var lv-run-date   as date    no-undo. /* next run date and time */
def var lv-run-time   as int no-undo. 
def var lv-ended      as log no-undo. /* has the schedule ended */ 
def var lv-problem    as log no-undo. /* problem interpriting schedule line */
def var lv-ending     as log no-undo. /* will this schedule end itself? */
DEF var lv-iterate    as log NO-UNDO. /* is there a time iteration ncrease input time */


{{&core}run.i &program   = "schedule-parser.p"
             &path      = "{&tsk}"
             &noper     = true
             &Appsrv    = "System"
             &procedure = "refresh"
             &params    = "(pv-sched,
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

ASSIGN vd-r-date:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(lv-run-date,'99/99/9999')
       lv-r-time:SCREEN-VALUE = string(lv-run-time,'hh:mm').

if lv-problem then return 'problem'.
if lv-ending  then return 'ending'.
if lv-ended   then return 'ended'.

b-submit:SENSITIVE = YES.
return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

