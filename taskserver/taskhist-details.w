&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-TaskHist NO-UNDO LIKE Zen-TaskHist.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
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

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&glob KeepRefreshButton
/* Local Variable Definitions ---                                       */
{app-paths.i}



DEFINE INPUT PARAMETER TABLE FOR t-zen-taskhist.
DEFINE INPUT PARAMETER lv-task AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-Zen-TaskHist

/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define FIELDS-IN-QUERY-DIALOG-1 t-Zen-TaskHist.TaskServer ~
t-Zen-TaskHist.TaskId t-Zen-TaskHist.TaskType t-Zen-TaskHist.TStatus ~
t-Zen-TaskHist.tmessage t-Zen-TaskHist.dependancy-taskid ~
t-Zen-TaskHist.cycle t-Zen-TaskHist.SubmitDate t-Zen-TaskHist.StartDate ~
t-Zen-TaskHist.RunDate t-Zen-TaskHist.EndDate t-Zen-TaskHist.SubmitUser ~
t-Zen-TaskHist.taskpgm t-Zen-TaskHist.Parameters 
&Scoped-define OPEN-QUERY-DIALOG-1 OPEN QUERY DIALOG-1 FOR EACH t-Zen-TaskHist SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-1 t-Zen-TaskHist
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-1 t-Zen-TaskHist


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK RECT-51 RECT-52 RECT-53 RECT-54 
&Scoped-Define DISPLAYED-FIELDS t-Zen-TaskHist.TaskServer ~
t-Zen-TaskHist.TaskId t-Zen-TaskHist.TaskType t-Zen-TaskHist.TStatus ~
t-Zen-TaskHist.tmessage t-Zen-TaskHist.dependancy-taskid ~
t-Zen-TaskHist.cycle t-Zen-TaskHist.SubmitDate t-Zen-TaskHist.StartDate ~
t-Zen-TaskHist.RunDate t-Zen-TaskHist.EndDate t-Zen-TaskHist.SubmitUser ~
t-Zen-TaskHist.taskpgm t-Zen-TaskHist.Parameters 
&Scoped-Define DISPLAYED-OBJECTS lv-sub-time lv-sta-time lv-run-time ~
lv-end-time 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 t-Zen-TaskHist.TaskServer t-Zen-TaskHist.TaskId ~
t-Zen-TaskHist.TaskType t-Zen-TaskHist.TStatus t-Zen-TaskHist.tmessage ~
t-Zen-TaskHist.dependancy-taskid t-Zen-TaskHist.cycle ~
t-Zen-TaskHist.SubmitDate lv-sub-time t-Zen-TaskHist.StartDate lv-sta-time ~
t-Zen-TaskHist.RunDate lv-run-time t-Zen-TaskHist.EndDate lv-end-time ~
t-Zen-TaskHist.SubmitUser t-Zen-TaskHist.taskpgm t-Zen-TaskHist.Parameters 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO  NO-FOCUS
     LABEL "OK" 
     SIZE 18 BY 1.24 TOOLTIP "Ok"
     BGCOLOR 8 .

def var lv-end-time as char FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

def var lv-run-time as char FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

def var lv-sta-time as char FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

def var lv-sub-time as char FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY 2.14.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 135 BY 3.81.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 135 BY 4.05.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 135 BY 5.95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-1 FOR 
      t-Zen-TaskHist SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     Btn_OK AT ROW 21.48 COL 58
     t-Zen-TaskHist.TaskServer AT ROW 2.67 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     t-Zen-TaskHist.TaskId AT ROW 5.52 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     t-Zen-TaskHist.TaskType AT ROW 5.52 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     t-Zen-TaskHist.TStatus AT ROW 6.48 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     t-Zen-TaskHist.tmessage AT ROW 6.48 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 88 BY 1
     t-Zen-TaskHist.dependancy-taskid AT ROW 7.43 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     t-Zen-TaskHist.cycle AT ROW 7.43 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     t-Zen-TaskHist.SubmitDate AT ROW 10.76 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lv-sub-time AT ROW 10.76 COL 34 COLON-ALIGNED NO-LABEL
     t-Zen-TaskHist.StartDate AT ROW 10.76 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lv-sta-time AT ROW 10.76 COL 96 COLON-ALIGNED NO-LABEL
     t-Zen-TaskHist.RunDate AT ROW 11.71 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lv-run-time AT ROW 11.71 COL 34 COLON-ALIGNED NO-LABEL
     t-Zen-TaskHist.EndDate AT ROW 11.71 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lv-end-time AT ROW 11.71 COL 96 COLON-ALIGNED NO-LABEL
     t-Zen-TaskHist.SubmitUser AT ROW 12.67 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     t-Zen-TaskHist.taskpgm AT ROW 16 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     t-Zen-TaskHist.Parameters AT ROW 17.05 COL 19 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 115 BY 3.57
     RECT-51 AT ROW 1.95 COL 1
     RECT-52 AT ROW 5.05 COL 1
     RECT-53 AT ROW 10.05 COL 1
     RECT-54 AT ROW 15.29 COL 1
     "Task Server" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.48 COL 2
     "Task" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 4.57 COL 2
     "Task Information" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 9.57 COL 2
     "Task Program Information" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 14.81 COL 2
     "Parameters:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 17.1 COL 6.8
     SPACE(117.79) SKIP(5.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Task Server History Details".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: t-Zen-TaskHist T "?" NO-UNDO sigmstr Zen-TaskHist
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
                                                                        */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN t-Zen-TaskHist.cycle IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.dependancy-taskid IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.EndDate IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR EDITOR t-Zen-TaskHist.Parameters IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.RunDate IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.StartDate IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.SubmitDate IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.SubmitUser IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.TaskId IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.taskpgm IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.TaskServer IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.TaskType IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.tmessage IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-TaskHist.TStatus IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-end-time IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-run-time IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-sta-time IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-sub-time IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _TblList          = "Temp-Tables.t-Zen-TaskHist"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK DIALOG-1
ON CHOOSE OF Btn_OK IN FRAME DIALOG-1 /* OK */
DO:
  return string("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {{&core}sec-chk.i}
  RUN enable_UI.
  
  FIND FIRST t-zen-taskhist WHERE t-zen-taskhist.taskid = lv-task NO-LOCK NO-ERROR.
  ASSIGN lv-sub-time = STRING(t-zen-taskhist.submittime,'hh:mm am')
         lv-sta-time = STRING(t-zen-taskhist.starttime,'hh:mm am')
         lv-run-time = STRING(t-zen-taskhist.runtime,'hh:mm am')
         lv-end-time = STRING(t-zen-taskhist.EndTIME,'hh:mm am').
  DISPLAY {&list-1}
  WITH FRAME {&FRAME-NAME}.
  
  

  {{&core}wid-chk.i}
  {{&core}focus.i}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY lv-sub-time lv-sta-time lv-run-time lv-end-time 
      WITH FRAME DIALOG-1.
  IF AVAILABLE t-Zen-TaskHist THEN 
    DISPLAY t-Zen-TaskHist.TaskServer t-Zen-TaskHist.TaskId 
          t-Zen-TaskHist.TaskType t-Zen-TaskHist.TStatus t-Zen-TaskHist.tmessage 
          t-Zen-TaskHist.dependancy-taskid t-Zen-TaskHist.cycle 
          t-Zen-TaskHist.SubmitDate t-Zen-TaskHist.StartDate 
          t-Zen-TaskHist.RunDate t-Zen-TaskHist.EndDate 
          t-Zen-TaskHist.SubmitUser t-Zen-TaskHist.taskpgm 
          t-Zen-TaskHist.Parameters 
      WITH FRAME DIALOG-1.
  ENABLE Btn_OK RECT-51 RECT-52 RECT-53 RECT-54 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

