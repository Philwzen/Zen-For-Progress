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
&glob KeepRefreshButton
&glob title-text Print Submit       /* message box title */
&glob suppresswindow
&glob nobuttons
&glob nobrowse

def input param pv-path as char no-undo.
def input param pv-prog as char no-undo.
def input param pv-proc as char no-undo.
def input param pv-printerparams as char no-undo.
def input param pv-dpgmparams as char no-undo.
def input param pv-extras as longchar no-undo.


DEF VAR lv-schedule AS CHAR   NO-UNDO.
def var lv-run-date   as date    no-undo. /* next run date and time */
def var lv-run-time   as int no-undo. 
DEF VAR lv-task-no AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-Cancel RECT-1 lv-quick lv-on lv-at ~
btn-Submit 
&Scoped-Define DISPLAYED-OBJECTS lv-quick lv-on lv-at 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-quick lv-on lv-at 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Cancel  NO-FOCUS
     LABEL "Cancel" 
     SIZE 16 BY 1.24 TOOLTIP "Cancel"
     BGCOLOR 8 .

DEFINE BUTTON btn-Schedule  NO-FOCUS
     LABEL "Schedule" 
     SIZE 16 BY 1.24 TOOLTIP "Ok"
     BGCOLOR 8 .

DEFINE BUTTON btn-Submit  NO-FOCUS
     LABEL "Submit" 
     SIZE 16 BY 1.24 TOOLTIP "Submit"
     BGCOLOR 8 .

def var lv-at as char FORMAT "x(10)":U 
     LABEL "At" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

def var lv-on AS DATE FORMAT "99/99/9999":U 
     LABEL "On" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 8.1.

def var lv-quick as log INITIAL yes 
     LABEL "Immediate" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     Btn-Cancel AT ROW 8.14 COL 3 WIDGET-ID 2
     lv-quick AT ROW 2.67 COL 11 WIDGET-ID 12
     lv-on AT ROW 3.62 COL 13 COLON-ALIGNED WIDGET-ID 24
     lv-at AT ROW 4.81 COL 13 COLON-ALIGNED WIDGET-ID 26
     btn-Schedule AT ROW 8.14 COL 41 WIDGET-ID 4
     btn-Submit AT ROW 8.14 COL 22.4 HELP
          "Submit" WIDGET-ID 22
     RECT-1 AT ROW 1.48 COL 2 WIDGET-ID 16
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 62.4 BY 9.29
          WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW win-main ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 19.2
         ROW                = 7.24
         HEIGHT             = 9.38
         WIDTH              = 62.6
         MAX-HEIGHT         = 46.52
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.52
         VIRTUAL-WIDTH      = 256
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN win-main = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btn-Schedule IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-at IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-on IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-quick IN FRAME f-main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cancel win-main
ON CHOOSE OF Btn-Cancel IN FRAME f-main /* Cancel */
DO:
   run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Schedule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Schedule win-main
ON CHOOSE OF btn-Schedule IN FRAME f-main /* Schedule */
DO:
assign lv-quick lv-schedule = trim(lv-schedule).

IF not lv-quick:checked 
tHEN do:
   IF lv-schedule <> '' 
   THEN DO:
      MESSAGE msg(232,'previous','','','') 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lv-ok AS LOG.
      IF lv-ok THEN apply 'choose' to btn-submit.
   end.
   runchild("{&tsk}schedule.w",this-procedure).
END.

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Submit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Submit win-main
ON CHOOSE OF btn-Submit IN FRAME f-main /* Submit */
DO:

MESSAGE msg(231,'submit task now','','','') 
VIEW-AS ALERT-BOX QUESTION 
BUTTONS YES-NO UPDATE lv-ok AS LOG.

IF lv-ok 
THEN DO:
   assign {&list-1}.
   IF lv-quick THEN lv-schedule = ''.

   RUN submit-task(lv-quick,lv-schedule).
    /* task number #1 has been submitted to the taskserver #2 */   
    MESSAGE msg(173,string(lv-task-no),entry(7,pv-printerparams,'{&Delim2}'),'','')
    VIEW-AS ALERT-BOX INFO.
END.

run exit-trigger.

eND.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-quick
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-quick win-main
ON VALUE-CHANGED OF lv-quick IN FRAME f-main /* Immediate */
DO:
   assign lv-quick.
   IF lv-quick:checked THEN lv-schedule = ''.
assign
   lv-on:sensitive = self:checked
   lv-at:sensitive = self:checked
   btn-schedule:sensitive = not lv-quick:checked.
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
{{&core}commonmaint.i}
/* &extraparams  = "whatever you want,"}*/
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {{&core}sec-chk.i}   /* screen security check */
lv-on = today.
lv-at = string(time + 300,'hh:mm').
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
  DISPLAY lv-quick lv-on lv-at 
      WITH FRAME f-main.
  ENABLE Btn-Cancel RECT-1 lv-quick lv-on lv-at btn-Submit 
      WITH FRAME f-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn win-main 
PROCEDURE Local-ChildReturn :
/*------------------------------------------------------------------------------
  Purpose: run after a child exits     
  Parameters:  pv-from is name of child procedure
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-from as char no-undo.

DEF var lv-new-sched  AS CHAR    NO-UNDO. /* schedule line to screen or record format */
def var lv-ended      as log no-undo. /* has the schedule ended */ 
def var lv-problem    as log no-undo. /* problem interpriting schedule line */
def var lv-ending     as log no-undo. /* will this schedule end itself? */
DEF var lv-iterate    as log NO-UNDO. /* is there a time iteration ncrease input time */

run SendResult in widget-handle(pv-from) (output lv-schedule).

{{&core}run.i &program   = "schedule-parser.p"
                &path      = "{&tsk}"
                &noper     = true
                &Appsrv    = "System"
                &procedure = "refresh"
                &params    = "(lv-schedule,
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

message 'Scheduling Information Created' 
view-as alert-box.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Update-Child-Procedures win-main 
PROCEDURE Local-Update-Child-Procedures :
/*------------------------------------------------------------------------------
  Purpose: refesh any child procedures    
  Parameters: pv-to handle of child requesting refresh
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Submit-Task win-main 
PROCEDURE Submit-Task :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param lv-immidiate AS LOG NO-UNDO.
def input param pv-sched as char no-undo.

message pv-path + pv-prog skip
                            pv-proc skip
                            entry(7,pv-printerparams,'{&Delim2}') skip
                            lv-on lv-run-date skip
                            integertime(lv-at) lv-run-time skip
                            pv-sched skip
                            '{&treport}' skip
                            pv-printerparams + '{&ComboDelim}' + pv-dpgmparams + '{&ComboDelim}' + pv-extras.
                            

{{&core}run.i &program   = "zen-task.p"
             &path      = "{&tsk}{&srv}"
             &Appsrv    = "System"
             &noper     = true
             &procedure = "submit-task"
             &params    = "(pv-path + pv-prog,
                            pv-proc,
                            entry(7,pv-printerparams,'{&Delim2}'), 
                            IF lv-quick THEN lv-on ELSE lv-run-date,
                            IF lv-quick THEN integertime(lv-at) ELSE lv-run-time,
                            pv-sched, /* quick schedule */
                            '{&treport}', /* task type */
                            pv-printerparams + '{&ComboDelim}' + pv-dpgmparams + '{&ComboDelim}' + pv-extras, /* parameters */
                            output lv-task-no)"} 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

