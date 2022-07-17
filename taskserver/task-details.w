&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-task NO-UNDO LIKE zen-task.



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



DEFINE INPUT PARAMETER TABLE FOR t-zen-task.
DEFINE INPUT PARAMETER lv-task AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK RECT-51 RECT-52 RECT-53 RECT-54 
&Scoped-Define DISPLAYED-FIELDS t-zen-task.taskserver t-zen-task.taskid ~
t-zen-task.tasktype t-zen-task.tstatus t-zen-task.tmessage ~
t-zen-task.dependancy-taskid t-zen-task.cycle t-zen-task.submitdate ~
t-zen-task.submituser t-zen-task.taskpgm t-zen-task.taskproc ~
t-zen-task.parameters 
&Scoped-define DISPLAYED-TABLES t-zen-task
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-task
&Scoped-Define DISPLAYED-OBJECTS lv-sub-time 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 t-zen-task.taskserver t-zen-task.taskid ~
t-zen-task.tasktype t-zen-task.tstatus t-zen-task.tmessage ~
t-zen-task.dependancy-taskid t-zen-task.cycle t-zen-task.submitdate ~
t-zen-task.submituser lv-sub-time t-zen-task.taskpgm t-zen-task.taskproc ~
t-zen-task.parameters 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO  NO-FOCUS
     LABEL "OK" 
     SIZE 18 BY 1.24 TOOLTIP "Ok"
     BGCOLOR 8 .

DEFINE VARIABLE lv-sub-time AS CHARACTER FORMAT "X(256)":U 
     LABEL "Submit Time" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 1.57.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 135 BY 3.57.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 2.62.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 135 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     Btn_OK AT ROW 17.91 COL 56
     t-zen-task.taskserver AT ROW 1.86 COL 18 COLON-ALIGNED FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     t-zen-task.taskid AT ROW 4.1 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     t-zen-task.tasktype AT ROW 4.1 COL 50 COLON-ALIGNED FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 83 BY 1
     t-zen-task.tstatus AT ROW 5.05 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     t-zen-task.tmessage AT ROW 5.05 COL 50 COLON-ALIGNED FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 83 BY 1
     t-zen-task.dependancy-taskid AT ROW 6 COL 18 COLON-ALIGNED
          LABEL "Dependent Task"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     t-zen-task.cycle AT ROW 6 COL 50 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     t-zen-task.submitdate AT ROW 8.43 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     t-zen-task.submituser AT ROW 8.43 COL 50 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     lv-sub-time AT ROW 9.38 COL 18 COLON-ALIGNED
     t-zen-task.taskpgm AT ROW 11.76 COL 18 COLON-ALIGNED
          LABEL "Run Program" FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 104 BY 1
     t-zen-task.taskproc AT ROW 12.95 COL 18 COLON-ALIGNED WIDGET-ID 2 FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 104 BY 1
     t-zen-task.parameters AT ROW 14.14 COL 20 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 112 BY 3.1
     "Task Server" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.1 COL 2
     "Task Program Information" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 11 COL 2
     "User Information" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 7.67 COL 2
     "Task" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 3.38 COL 2
     "Parameters:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 15.76 COL 7
     RECT-51 AT ROW 1.57 COL 1
     RECT-52 AT ROW 3.86 COL 1
     RECT-53 AT ROW 8.14 COL 1
     RECT-54 AT ROW 11.48 COL 1
     SPACE(0.59) SKIP(1.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Task Details".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-task T "?" NO-UNDO schadm zen-task
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN t-zen-task.cycle IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-task.dependancy-taskid IN FRAME DIALOG-1
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN lv-sub-time IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR EDITOR t-zen-task.parameters IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-task.submitdate IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-task.submituser IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-task.taskid IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-task.taskpgm IN FRAME DIALOG-1
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN t-zen-task.taskproc IN FRAME DIALOG-1
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN t-zen-task.taskserver IN FRAME DIALOG-1
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN t-zen-task.tasktype IN FRAME DIALOG-1
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN t-zen-task.tmessage IN FRAME DIALOG-1
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN t-zen-task.tstatus IN FRAME DIALOG-1
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
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
  
  FIND FIRST t-zen-task WHERE t-zen-task.taskid = lv-task NO-LOCK NO-ERROR.
  lv-sub-time = STRING(t-zen-task.submittime,'hh:mm am').
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
  DISPLAY lv-sub-time 
      WITH FRAME DIALOG-1.
  IF AVAILABLE t-zen-task THEN 
    DISPLAY t-zen-task.taskserver t-zen-task.taskid t-zen-task.tasktype 
          t-zen-task.tstatus t-zen-task.tmessage t-zen-task.dependancy-taskid 
          t-zen-task.cycle t-zen-task.submitdate t-zen-task.submituser 
          t-zen-task.taskpgm t-zen-task.taskproc t-zen-task.parameters 
      WITH FRAME DIALOG-1.
  ENABLE Btn_OK RECT-51 RECT-52 RECT-53 RECT-54 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

