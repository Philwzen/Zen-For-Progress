&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  vars:
      <none>

  vars:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
&glob KeepRefreshButton
/* ***************************  Definitions  ************************** */

{app-paths.i}
&global-define suppresswindow
&global-define nobuttons
/* Parameters Definitions ---                                           */
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME schedule-frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-Exit RECT-11 RECT-12 RECT-13 RECT-14 ~
RECT-15 lv-monday lv-weekdays lv-first lv-first-day lv-last lv-last-day ~
lv-tuesday lv-weekends lv-f-monday lv-l-monday lv-wednesday lv-everyday ~
lv-f-tuesday lv-l-tuesday lv-thursday lv-f-wednesday lv-l-wednesday ~
lv-friday lv-f-thursday lv-l-thursday lv-saturday lv-f-friday lv-l-friday ~
lv-sunday lv-f-saturday lv-l-saturday lv-f-sunday lv-l-sunday ~
Lv-UseStartDays lv-UseStartDates lv-startdays lv-startdates lv-starttime ~
lv-repeat lv-UseStartTime lv-userepeat lv-end-date lv-end-time lv-ending ~
lv-schedule lv-iterations b-return b-show b-syntax b-run-time b-forecast 
&Scoped-Define DISPLAYED-OBJECTS lv-monday lv-weekdays lv-first ~
lv-first-day lv-last lv-last-day lv-tuesday lv-weekends lv-f-monday ~
lv-l-monday lv-wednesday lv-everyday lv-f-tuesday lv-l-tuesday lv-thursday ~
lv-f-wednesday lv-l-wednesday lv-friday lv-f-thursday lv-l-thursday ~
lv-saturday lv-f-friday lv-l-friday lv-sunday lv-f-saturday lv-l-saturday ~
lv-f-sunday lv-l-sunday Lv-UseStartDays lv-UseStartDates lv-startdays ~
lv-startdates lv-starttime lv-repeat lv-UseStartTime lv-userepeat ~
lv-end-date lv-end-time lv-ending lv-schedule lv-iterations 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-monday lv-weekdays lv-first lv-first-day lv-last ~
lv-last-day lv-tuesday lv-weekends lv-f-monday lv-l-monday lv-wednesday ~
lv-everyday lv-f-tuesday lv-l-tuesday lv-thursday lv-f-wednesday ~
lv-l-wednesday lv-friday lv-f-thursday lv-l-thursday lv-saturday ~
lv-f-friday lv-l-friday lv-sunday lv-f-saturday lv-l-saturday lv-f-sunday ~
lv-l-sunday Lv-UseStartDays lv-UseStartDates lv-startdays lv-startdates ~
lv-starttime lv-repeat lv-UseStartTime lv-userepeat lv-end-date lv-end-time ~
lv-ending lv-schedule lv-iterations 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-forecast 
     LABEL "Forecast >>" 
     SIZE 16 BY 1.14.

DEFINE BUTTON b-return 
     LABEL "Set + Exit" 
     SIZE 13.8 BY 1.14.

DEFINE BUTTON b-run-time 
     LABEL "Date/Time" 
     SIZE 14 BY 1.14.

DEFINE BUTTON b-show 
     LABEL "Show" 
     SIZE 12 BY 1.14.

DEFINE BUTTON b-syntax 
     LABEL "Syntax" 
     SIZE 12 BY 1.14.

DEFINE BUTTON btn-Exit 
     IMAGE-UP FILE "{&core}grafix/exit-su.ico":U NO-FOCUS
     LABEL "E&xit":L 
     SIZE 5.6 BY 1.33 TOOLTIP "Exit".

DEFINE VARIABLE lv-end-date AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lv-end-time AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ending Time" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE lv-iterations AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 30 
     LABEL "Ahead" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE lv-repeat AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-schedule AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 108.8 BY 1 NO-UNDO.

DEFINE VARIABLE lv-startdates AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE lv-startdays AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE lv-starttime AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 5.24.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 8.33.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 2.62.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 118 BY 3.33.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 118 BY 1.91.

DEFINE VARIABLE lv-ending AS LOGICAL INITIAL no 
     LABEL "Ending Date" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-everyday AS LOGICAL INITIAL no 
     LABEL "Everyday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-f-friday AS LOGICAL INITIAL no 
     LABEL "Friday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-f-monday AS LOGICAL INITIAL no 
     LABEL "Monday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-f-saturday AS LOGICAL INITIAL no 
     LABEL "Saturday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-f-sunday AS LOGICAL INITIAL no 
     LABEL "Sunday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-f-thursday AS LOGICAL INITIAL no 
     LABEL "Thursday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-f-tuesday AS LOGICAL INITIAL no 
     LABEL "Tuesday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-f-wednesday AS LOGICAL INITIAL no 
     LABEL "Wednesday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-first AS LOGICAL INITIAL no 
     LABEL "First" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE lv-first-day AS LOGICAL INITIAL no 
     LABEL "Day" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-friday AS LOGICAL INITIAL no 
     LABEL "Friday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-l-friday AS LOGICAL INITIAL no 
     LABEL "Friday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-l-monday AS LOGICAL INITIAL no 
     LABEL "Monday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-l-saturday AS LOGICAL INITIAL no 
     LABEL "Saturday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-l-sunday AS LOGICAL INITIAL no 
     LABEL "Sunday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-l-thursday AS LOGICAL INITIAL no 
     LABEL "Thursday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-l-tuesday AS LOGICAL INITIAL no 
     LABEL "Tuesday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-l-wednesday AS LOGICAL INITIAL no 
     LABEL "Wednesday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-last AS LOGICAL INITIAL no 
     LABEL "Last" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.6 BY .81 NO-UNDO.

DEFINE VARIABLE lv-last-day AS LOGICAL INITIAL no 
     LABEL "Day" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-monday AS LOGICAL INITIAL no 
     LABEL "Monday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-saturday AS LOGICAL INITIAL no 
     LABEL "Saturday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-sunday AS LOGICAL INITIAL no 
     LABEL "Sunday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-thursday AS LOGICAL INITIAL no 
     LABEL "Thursday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-tuesday AS LOGICAL INITIAL no 
     LABEL "Tuesday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-userepeat AS LOGICAL INITIAL no 
     LABEL "Repeat" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE lv-UseStartDates AS LOGICAL INITIAL no 
     LABEL "Start Dates" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE Lv-UseStartDays AS LOGICAL INITIAL no 
     LABEL "Start Days" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE lv-UseStartTime AS LOGICAL INITIAL no 
     LABEL "Start Time" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE lv-wednesday AS LOGICAL INITIAL no 
     LABEL "Wednesday" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-weekdays AS LOGICAL INITIAL no 
     LABEL "Weekdays" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-weekends AS LOGICAL INITIAL no 
     LABEL "Weekends" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME schedule-frame
     btn-Exit AT ROW 17.48 COL 5 HELP
          "Exit this maintenance program"
     lv-monday AT ROW 2.19 COL 11
     lv-weekdays AT ROW 2.19 COL 36
     lv-first AT ROW 2.19 COL 57
     lv-first-day AT ROW 2.19 COL 72
     lv-last AT ROW 2.19 COL 91.4
     lv-last-day AT ROW 2.19 COL 103
     lv-tuesday AT ROW 2.86 COL 11
     lv-weekends AT ROW 2.86 COL 36
     lv-f-monday AT ROW 2.86 COL 72
     lv-l-monday AT ROW 2.86 COL 103
     lv-wednesday AT ROW 3.48 COL 11
     lv-everyday AT ROW 3.48 COL 36
     lv-f-tuesday AT ROW 3.48 COL 72
     lv-l-tuesday AT ROW 3.48 COL 103
     lv-thursday AT ROW 4.1 COL 11
     lv-f-wednesday AT ROW 4.1 COL 72
     lv-l-wednesday AT ROW 4.1 COL 103
     lv-friday AT ROW 4.71 COL 11
     lv-f-thursday AT ROW 4.71 COL 72
     lv-l-thursday AT ROW 4.71 COL 103
     lv-saturday AT ROW 5.33 COL 11
     lv-f-friday AT ROW 5.33 COL 72
     lv-l-friday AT ROW 5.33 COL 103
     lv-sunday AT ROW 5.95 COL 11
     lv-f-saturday AT ROW 5.95 COL 72
     lv-l-saturday AT ROW 5.95 COL 103
     lv-f-sunday AT ROW 6.57 COL 72
     lv-l-sunday AT ROW 6.57 COL 103
     Lv-UseStartDays AT ROW 7.57 COL 72
     lv-UseStartDates AT ROW 7.71 COL 6.6
     lv-startdays AT ROW 8.57 COL 70 COLON-ALIGNED NO-LABEL
     lv-startdates AT ROW 8.67 COL 4.6 COLON-ALIGNED NO-LABEL
     lv-starttime AT ROW 11.33 COL 27.2 COLON-ALIGNED NO-LABEL
     lv-repeat AT ROW 11.33 COL 82.2 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     lv-UseStartTime AT ROW 11.57 COL 8.2
     lv-userepeat AT ROW 11.57 COL 71.8
     lv-end-date AT ROW 12.52 COL 27.2 COLON-ALIGNED NO-LABEL
     lv-end-time AT ROW 12.52 COL 82.2 COLON-ALIGNED
     lv-ending AT ROW 12.67 COL 8.2
     lv-schedule AT ROW 15.62 COL 9.4 COLON-ALIGNED NO-LABEL
     lv-iterations AT ROW 17.52 COL 98.2 COLON-ALIGNED
     b-return AT ROW 17.57 COL 15.4
     b-show AT ROW 17.57 COL 32.8
     b-syntax AT ROW 17.57 COL 44.8
     b-run-time AT ROW 17.57 COL 56.8
     b-forecast AT ROW 17.57 COL 74.2
     "Schedule" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 14.67 COL 7.2
     "Monthly" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.38 COL 58
     "Schedules" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 17.76 COL 109.2
     "24 hr  HH:MM" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.71 COL 104.2
     "Weekly" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.43 COL 7
     "Minutes" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.57 COL 95.2
     "Eg: 01/01/01^02/02/02" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 7.71 COL 25
     "Start and End" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 10.52 COL 7.2
     "(24 hr - HH:MM)" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 11.52 COL 48.2
     "Specific" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 6.95 COL 7
     "Eg:  1^10^20" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 7.57 COL 100.2
     RECT-11 AT ROW 1.71 COL 5
     RECT-12 AT ROW 1.71 COL 56
     RECT-13 AT ROW 7.43 COL 5
     RECT-14 AT ROW 10.81 COL 5.2
     RECT-15 AT ROW 15.14 COL 5.2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123.2 BY 18.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,DB-Fields,Window
   Frames: 0
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Generate Schedule"
         HEIGHT             = 21.95
         WIDTH              = 123
         MAX-HEIGHT         = 33.14
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.14
         VIRTUAL-WIDTH      = 204.8
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
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME schedule-frame
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN lv-end-date IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-end-time IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-ending IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-everyday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-f-friday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-f-monday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-f-saturday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-f-sunday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-f-thursday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-f-tuesday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-f-wednesday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-first IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-first-day IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-friday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-iterations IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-l-friday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-l-monday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-l-saturday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-l-sunday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-l-thursday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-l-tuesday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-l-wednesday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-last IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-last-day IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-monday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-repeat IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-saturday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-schedule IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-startdates IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-startdays IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-starttime IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-sunday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-thursday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-tuesday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-userepeat IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-UseStartDates IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX Lv-UseStartDays IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-UseStartTime IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-wednesday IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-weekdays IN FRAME schedule-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-weekends IN FRAME schedule-frame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME schedule-frame
/* Query rebuild information for FRAME schedule-frame
     _Query            is NOT OPENED
*/  /* FRAME schedule-frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generate Schedule */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generate Schedule */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-forecast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-forecast C-Win
ON CHOOSE OF b-forecast IN FRAME schedule-frame /* Forecast >> */
DO:
    run scheduleparser (lv-schedule:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

    IF return-value = 'passed' 
    THEN run scheduleforecast(INT(lv-iterations:SCREEN-VALUE),
                             lv-schedule:SCREEN-VALUE).
    ELSE MESSAGE 'There is a problem with the Syntax of this Schedule' 
         VIEW-AS ALERT-BOX ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-return
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-return C-Win
ON CHOOSE OF b-return IN FRAME schedule-frame /* Set + Exit */
DO:
 RUN ASSIGN_frame.
 RUN build_schedule_line.
 run ScheduleParser(lv-schedule:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 case return-value:
    when 'problem' THEN DO:
       MESSAGE 'There is a problem with the Syntax of the Schedule' VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    when 'ended'  THEN do:
       MESSAGE 'This schedule has already ended!' VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
/*     otherwise do:                                          */
/*       MESSAGE 'Syntax is correct' VIEW-AS ALERT-BOX INFO.  */
/*     end.                                                   */
 end case.
 IF return-value ne 'ended' and 
    return-value ne 'problem' and 
    return-value ne 'ending' 
 THEN MESSAGE 'This schedule will run continueously' VIEW-AS ALERT-BOX INFO.

 run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-run-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-run-time C-Win
ON CHOOSE OF b-run-time IN FRAME schedule-frame /* Date/Time */
DO:

     run ScheduleParser(lv-schedule:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

     IF return-value = 'problem'
     THEN MESSAGE 'There is a problem with the Syntax of the Schedule' VIEW-AS ALERT-BOX ERROR.
/*      ELSE MESSAGE 'Next Run Date: ' lv-run-date SKIP                    */
/*                   'Next Run Time  ' STRING(lv-run-time,'hh:mm am') SKIP */
/*                   'Ever Finish    ' lv-ending VIEW-AS ALERT-BOX INFO.   */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-show C-Win
ON CHOOSE OF b-show IN FRAME schedule-frame /* Show */
DO:
   RUN ASSIGN_frame.
   RUN build_schedule_line.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-syntax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-syntax C-Win
ON CHOOSE OF b-syntax IN FRAME schedule-frame /* Syntax */
DO:
   run ScheduleParser(lv-schedule:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

   IF return-value = 'problem' THEN
   MESSAGE 'There is a problem with the Syntax of the Schedule' VIEW-AS ALERT-BOX ERROR.
   ELSE
   IF NOT return-value = 'ended' THEN MESSAGE 'Syntax is correct' VIEW-AS ALERT-BOX INFO.

   IF return-value = 'ended' THEN MESSAGE 'This schedule has already ended!' VIEW-AS ALERT-BOX ERROR.

   IF NOT return-value = 'ending' AND NOT return-value = 'ended' AND NOT return-value = 'problem' THEN
   MESSAGE 'This schedule will run continueously' VIEW-AS ALERT-BOX INFO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Exit C-Win
ON CHOOSE OF btn-Exit IN FRAME schedule-frame /* Exit */
do:    
lv-schedule = 'quit'.
   RUN exit-trigger.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ending
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ending C-Win
ON VALUE-CHANGED OF lv-ending IN FRAME schedule-frame /* Ending Date */
DO:
  RUN SET_context.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-first
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-first C-Win
ON VALUE-CHANGED OF lv-first IN FRAME schedule-frame /* First */
DO:
  RUN SET_context.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-last C-Win
ON VALUE-CHANGED OF lv-last IN FRAME schedule-frame /* Last */
DO:
  RUN SET_context. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-userepeat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-userepeat C-Win
ON VALUE-CHANGED OF lv-userepeat IN FRAME schedule-frame /* Repeat */
DO:
  RUN SET_context.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-UseStartDates
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-UseStartDates C-Win
ON VALUE-CHANGED OF lv-UseStartDates IN FRAME schedule-frame /* Start Dates */
DO:
  RUN SET_context.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Lv-UseStartDays
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Lv-UseStartDays C-Win
ON VALUE-CHANGED OF Lv-UseStartDays IN FRAME schedule-frame /* Start Days */
DO:
  RUN SET_context.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-UseStartTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-UseStartTime C-Win
ON VALUE-CHANGED OF lv-UseStartTime IN FRAME schedule-frame /* Start Time */
DO:
   RUN SET_context.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

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
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    {{&core}sec-chk.i}
  RUN enable_UI.
    {{&core}focus.i}
    {{&core}wid-chk.i}
  RUN SET_context.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign_frame C-Win 
PROCEDURE assign_frame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN FRAME {&FRAME-NAME}
        {&list-1}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build_schedule_line C-Win 
PROCEDURE build_schedule_line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   lv-schedule = ''.
   /* everyday generation */
   IF lv-monday  THEN lv-schedule = lv-schedule + 'Weekly Monday'.
   IF lv-tuesday THEN lv-schedule = IF lv-monday 
                                    THEN lv-schedule + '^Tuesday'
                                    ELSE 'Weekly Tuesday'.
   IF lv-wednesday THEN lv-schedule = IF lv-monday OR 
                                         lv-tuesday 
                                      THEN lv-schedule + '^Wednesday'
                                      ELSE 'Weekly Wednesday'.
   IF lv-thursday THEN lv-schedule = IF lv-monday OR 
                                        lv-tuesday OR 
                                        lv-wednesday 
                                     THEN lv-schedule + '^Thursday'
                                     ELSE 'Weekly Thursday'.
   IF lv-friday THEN lv-schedule = IF lv-monday OR 
                                      lv-tuesday OR 
                                      lv-wednesday OR 
                                      lv-thursday 
                                   THEN lv-schedule + '^Friday'
                                   ELSE 'Weekly Friday'.
   IF lv-saturday THEN lv-schedule = IF lv-monday OR 
                                        lv-tuesday OR 
                                        lv-wednesday OR 
                                        lv-thursday OR 
                                        lv-friday 
                                     THEN lv-schedule + '^Saturday'
                                     ELSE 'Weekly Saturday'.
   IF lv-sunday THEN lv-schedule = IF lv-monday OR 
                                      lv-tuesday OR 
                                      lv-wednesday OR 
                                      lv-thursday OR 
                                      lv-friday OR 
                                      lv-saturday 
                                   THEN lv-schedule + '^Sunday'
                                   ELSE 'Weekly Sunday'.

   IF lv-weekdays THEN lv-schedule = IF lv-monday OR 
                                        lv-tuesday OR 
                                        lv-wednesday OR 
                                        lv-thursday OR 
                                        lv-friday OR 
                                        lv-saturday OR 
                                        lv-sunday 
                                     THEN lv-schedule + '^Weekdays'
                                     ELSE 'Weekly Weekdays'.

   IF lv-weekends THEN lv-schedule = IF lv-monday OR 
                                        lv-tuesday OR 
                                        lv-wednesday OR 
                                        lv-thursday OR 
                                        lv-friday OR 
                                        lv-saturday OR 
                                        lv-sunday OR 
                                        lv-weekdays 
                                     THEN lv-schedule + '^Weekends'
                                     ELSE 'Weekly Weekends'.

   IF lv-everyday THEN lv-schedule = IF lv-monday OR 
                                        lv-tuesday OR 
                                        lv-wednesday OR 
                                        lv-thursday OR 
                                        lv-friday OR 
                                        lv-saturday OR 
                                        lv-sunday OR 
                                        lv-weekdays OR 
                                        lv-weekends 
                                     THEN lv-schedule + '^Everyday'
                                     ELSE 'Weekly Everyday'.
   /* end everyday schedule build */
   /* monthly schedule build */
   /* first */
   IF lv-first 
   THEN DO:
      /*lv-schedule = lv-schedule + IF lv-schedule <> '' THEN '{&Delim2}' ELSE ''.*/
      IF lv-first-day THEN lv-schedule = lv-schedule + 'First Day'.

      IF lv-f-monday THEN lv-schedule = lv-schedule + IF   lv-first-day THEN '^' + 'Monday'
                                                     ELSE IF lv-schedule = '' THEN 'First Monday' ELSE '{&Delim2}First Monday'.
      IF lv-f-tuesday THEN lv-schedule = lv-schedule + IF lv-first-day OR lv-f-monday THEN '^' + 'Tuesday'
                                                     ELSE IF lv-schedule = '' THEN 'First Tuesday' ELSE '{&Delim2}First Tuesday'.
      IF lv-f-wednesday THEN lv-schedule = lv-schedule + IF lv-first-day OR lv-f-monday OR lv-f-tuesday THEN '^' + 'Wednesday'
                                                     ELSE IF lv-schedule = '' THEN 'First Wednesday' ELSE '{&Delim2}First Wednesday'.
      IF lv-f-Thursday THEN lv-schedule = lv-schedule + IF lv-first-day OR lv-f-monday OR lv-f-tuesday OR lv-f-wednesday THEN '^' + 'Thursday'
                                                     ELSE  IF lv-schedule = '' THEN 'First Thursday' ELSE '{&Delim2}First Thursday'.
      IF lv-f-Friday THEN lv-schedule = lv-schedule + IF lv-first-day OR lv-f-monday OR lv-f-tuesday OR lv-f-wednesday OR lv-f-thursday THEN '^' + 'Friday'
                                                     ELSE  IF lv-schedule = '' THEN 'First Friday' ELSE '{&Delim2}First Friday'.
      IF lv-f-Saturday THEN lv-schedule = lv-schedule + IF lv-first-day OR lv-f-monday OR lv-f-tuesday OR lv-f-wednesday OR lv-f-thursday OR lv-f-friday THEN '^' + 'Saturday'
                                                     ELSE  IF lv-schedule = '' THEN 'First Saturday' ELSE '{&Delim2}First Saturday'.
      IF lv-f-Sunday THEN lv-schedule = lv-schedule + IF lv-first-day OR lv-f-monday OR lv-f-tuesday OR lv-f-wednesday OR lv-f-thursday OR lv-f-friday OR lv-f-saturday THEN '^' + 'Sunday'
                                                     ELSE  IF lv-schedule = '' THEN 'First Sunday' ELSE '{&Delim2}First Sunday'.

   END.

   /* last */
             
   IF lv-last 
   THEN DO:
      /*lv-schedule = lv-schedule + IF lv-schedule <> '' THEN '{&Delim2}' ELSE ''.*/

      IF lv-last-day THEN lv-schedule = lv-schedule + 'Last Day'.

      IF lv-l-monday THEN lv-schedule = lv-schedule + IF   lv-last-day THEN '^' + 'Monday'
                                                     ELSE  IF lv-schedule = '' THEN 'Last Monday' ELSE '{&Delim2}Last Monday'.
      IF lv-l-tuesday THEN lv-schedule = lv-schedule + IF lv-last-day OR lv-l-monday THEN '^' + 'Tuesday'
                                                     ELSE  IF lv-schedule = '' THEN 'Last Tuesday' ELSE '{&Delim2}Last Tuesday'.
      IF lv-l-wednesday THEN lv-schedule = lv-schedule + IF lv-last-day OR lv-l-monday OR lv-l-tuesday THEN '^' + 'Wednesday'
                                                     ELSE  IF lv-schedule = '' THEN  'Last Wednesday' ELSE '{&Delim2}Last Wednesday'.
      IF lv-l-Thursday THEN lv-schedule = lv-schedule + IF lv-last-day OR lv-l-monday OR lv-l-tuesday OR lv-l-wednesday THEN '^' + 'Thursday'
                                                     ELSE IF lv-schedule = '' THEN 'Last Thursday' ELSE '{&Delim2}Last Thursday'.
      IF lv-l-friday THEN lv-schedule = lv-schedule + IF lv-last-day OR lv-l-monday OR lv-l-tuesday OR lv-l-wednesday OR lv-l-thursday THEN '^' + 'Friday'
                                                     ELSE  IF lv-schedule = '' THEN 'Last Friday' ELSE '{&Delim2}Last Friday'.
      IF lv-l-Saturday THEN lv-schedule = lv-schedule + IF lv-last-day OR lv-l-monday OR lv-l-tuesday OR lv-l-wednesday OR lv-l-thursday OR lv-l-friday THEN '^' + 'Saturday'
                                                     ELSE  IF lv-schedule = '' THEN 'Last Saturday' ELSE '{&Delim2}Last Saturday'.
      IF lv-l-Sunday THEN lv-schedule = lv-schedule + IF lv-last-day OR lv-l-monday OR lv-l-tuesday OR lv-l-wednesday OR lv-l-thursday OR lv-l-friday OR lv-l-saturday THEN '^' + 'Sunday'
                                                     ELSE  IF lv-schedule = '' THEN 'Last Sunday' ELSE '{&Delim2}Last Sunday'.

   END.

   /* end monthly schedule build */

   /* specific build */
   /* start dates*/
   IF lv-UseStartDates 
   THEN assign lv-schedule = lv-schedule + IF lv-schedule <> '' THEN '{&Delim2}' ELSE ''
               lv-schedule = lv-schedule + 'S-Dates ' + lv-startdates.

   /* start days */
   IF lv-UseStartDays 
   THEN assign lv-schedule = lv-schedule + IF lv-schedule <> '' THEN '{&Delim2}' ELSE ''
               lv-schedule = lv-schedule + 'Days ' + lv-startdays.
   /* end specific build */
   
   /* start and end build */
   IF lv-UseStartTime
   THEN assign lv-schedule = lv-schedule + IF lv-schedule <> '' THEN '{&Delim2}' ELSE ''
               lv-schedule = lv-schedule + 'S-Time ' + lv-starttime
               lv-schedule = IF lv-userepeat THEN lv-schedule + '^Minutes^' + STRING(lv-repeat) else lv-schedule.

   IF lv-ending 
   THEN DO:
      lv-schedule = lv-schedule + IF lv-schedule <> '' THEN '{&Delim2}Ending ' ELSE 'Ending '.
      IF lv-end-date <> '' THEN lv-schedule = lv-schedule + lv-end-date.
      IF lv-end-time <> '' AND 
         lv-end-date <> '' THEN lv-schedule = lv-schedule + '^' + lv-end-time.
      IF lv-end-time <> '' AND
         lv-end-date = ''  THEN lv-schedule = lv-schedule + lv-end-time.
   END. /* ending date/time */

   /* end start and end build */

   lv-schedule:SCREEN-VALUE IN FRAME {&FRAME-NAME} = lv-schedule.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  HIDE FRAME schedule-frame.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY lv-monday lv-weekdays lv-first lv-first-day lv-last lv-last-day 
          lv-tuesday lv-weekends lv-f-monday lv-l-monday lv-wednesday 
          lv-everyday lv-f-tuesday lv-l-tuesday lv-thursday lv-f-wednesday 
          lv-l-wednesday lv-friday lv-f-thursday lv-l-thursday lv-saturday 
          lv-f-friday lv-l-friday lv-sunday lv-f-saturday lv-l-saturday 
          lv-f-sunday lv-l-sunday Lv-UseStartDays lv-UseStartDates lv-startdays 
          lv-startdates lv-starttime lv-repeat lv-UseStartTime lv-userepeat 
          lv-end-date lv-end-time lv-ending lv-schedule lv-iterations 
      WITH FRAME schedule-frame.
  ENABLE btn-Exit RECT-11 RECT-12 RECT-13 RECT-14 RECT-15 lv-monday lv-weekdays 
         lv-first lv-first-day lv-last lv-last-day lv-tuesday lv-weekends 
         lv-f-monday lv-l-monday lv-wednesday lv-everyday lv-f-tuesday 
         lv-l-tuesday lv-thursday lv-f-wednesday lv-l-wednesday lv-friday 
         lv-f-thursday lv-l-thursday lv-saturday lv-f-friday lv-l-friday 
         lv-sunday lv-f-saturday lv-l-saturday lv-f-sunday lv-l-sunday 
         Lv-UseStartDays lv-UseStartDates lv-startdays lv-startdates 
         lv-starttime lv-repeat lv-UseStartTime lv-userepeat lv-end-date 
         lv-end-time lv-ending lv-schedule lv-iterations b-return b-show 
         b-syntax b-run-time b-forecast 
      WITH FRAME schedule-frame.
  {&OPEN-BROWSERS-IN-QUERY-schedule-frame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh C-Win 
PROCEDURE refresh :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE schedule-to-screen C-Win 
PROCEDURE schedule-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER v-schedule AS CHAR   NO-UNDO.

DEF VAR X AS INT NO-UNDO.
DEF VAR lv-part AS CHAR NO-UNDO.
   
   DO X = 1 TO NUM-ENTRIES(v-schedule,'{&Delim2}'):
      lv-part = ENTRY(X,v-schedule,'{&Delim2}').
      IF lv-part BEGINS 's-dates ' 
      THEN DO: 
         ASSIGN lv-Usestartdates = YES 
                lv-Usestartdates:checked IN FRAME schedule-frame = yes
                lv-startdates = SUBSTRING(lv-part,INDEX(lv-part,' ') + 1,LENGTH(lv-part) - 7)
                lv-startdates:SCREEN-VALUE = lv-startdates.
         APPLY 'value-changed' TO lv-UseStartDates.
      END. /* s-dates */
      IF lv-part BEGINS 'days ' THEN
      DO:
          ASSIGN lv-usestartdays = YES 
                 lv-usestartdays:checked = yes
                 lv-startdays = SUBSTRING(lv-part,INDEX(lv-part,' ') + 1,LENGTH(lv-part) - 4)
                 lv-startdays:SCREEN-VALUE = lv-startdays.
          APPLY 'value-changed' TO lv-usestartdays.
      END. /* days */

      IF lv-part BEGINS 'weekly' 
      THEN do:
         assign lv-monday    = INDEX(lv-part,'monday') > 0 
                lv-tuesday   = INDEX(lv-part,'tuesday') > 0 
                lv-wednesday = INDEX(lv-part,'wednesday') > 0
                lv-thursday  = INDEX(lv-part,'thursday') > 0 
                lv-friday    = INDEX(lv-part,'friday') > 0 
                lv-saturday  = INDEX(lv-part,'saturday') > 0 
                lv-sunday    = INDEX(lv-part,'sunday') > 0 
                lv-weekdays  = INDEX(lv-part,'weekdays') > 0 
                lv-weekends  = INDEX(lv-part,'weekends') > 0 
                lv-everyday  = INDEX(lv-part,'everyday') > 0 .
         disp lv-monday lv-tuesday lv-wednesday lv-thursday lv-friday 
              lv-saturday lv-sunday lv-weekdays lv-weekends lv-everyday.
      end.
      IF lv-part BEGINS 'first' 
      THEN DO:
         ASSIGN lv-first = YES
                lv-first:SCREEN-VALUE = 'yes'
                lv-first-day = INDEX(lv-part,' day') > 0 
                lv-f-monday = INDEX(lv-part,'monday') > 0 
                lv-f-tuesday = INDEX(lv-part,'tuesday') > 0 
                lv-f-wednesday = INDEX(lv-part,'wednesday') > 0
                lv-f-thursday = INDEX(lv-part,'thursday') > 0 
                lv-f-friday = iNDEX(lv-part,'friday') > 0 
                lv-f-saturday = INDEX(lv-part,'saturday') > 0 
                lv-f-sunday = INDEX(lv-part,'sunday') > 0.

         APPLY 'VALUE-CHANGED' TO lv-first.        
      END. /* first */
      IF lv-part BEGINS 'last' 
      THEN DO:
          ASSIGN lv-last = YES
                 lv-last:SCREEN-VALUE = 'yes'
                 lv-last-day = INDEX(lv-part,' day') > 0 
                 lv-l-monday = INDEX(lv-part,'monday') > 0
                 lv-l-tuesday = INDEX(lv-part,'tuesday') > 0
                 lv-l-wednesday = INDEX(lv-part,'wednesday') > 0 
                 lv-l-thursday = INDEX(lv-part,'thursday') > 0
                 lv-l-friday = INDEX(lv-part,'friday') > 0 
                 lv-l-saturday = INDEX(lv-part,'saturday') > 0 
                 lv-l-sunday = INDEX(lv-part,'sunday') > 0.
          APPLY 'VALUE-CHANGED' TO lv-last.
      END. /* last */
      IF lv-part BEGINS 's-time' 
      THEN DO:
          ASSIGN lv-usestarttime = YES
                 lv-usestarttime:checked = yes.

          IF INDEX(lv-part,'^') > 0 
          THEN DO:
             ASSIGN lv-userepeat = YES.
                    lv-userepeat:checked = yes.
                    lv-starttime = SUBSTRING(lv-part,8,5).
                    lv-starttime:SCREEN-VALUE = lv-starttime.
                    lv-repeat = SUBSTRING(lv-part,R-INDEX(lv-part,'^') + 1,
                                          LENGTH(lv-part) - R-INDEX(lv-part,'^')).
                    lv-repeat:SCREEN-VALUE = lv-repeat.

             APPLY 'VALUE-CHANGED' TO lv-repeat.
          END.
          ELSE ASSIGN lv-starttime = SUBSTRING(lv-part,8,5)
                      lv-starttime:SCREEN-VALUE = lv-starttime.

          APPLY 'value-changed' TO lv-ending.
      END. /* start time + repeat */
      IF lv-part BEGINS 'ending' 
      THEN DO:
          ASSIGN lv-ending = YES
                 lv-ending:SCREEN-VALUE = 'yes'.

          IF INDEX(lv-part,'^') > 0 
          THEN DO:
             ASSIGN lv-end-date = SUBSTRING(lv-part,8,INDEX(lv-part,'^') - 8)
                    lv-end-date:SCREEN-VALUE = lv-end-date
                    lv-end-time = SUBSTRING(lv-part,INDEX(lv-part,':') - 2,5)
                    lv-end-time:SCREEN-VALUE = lv-end-time.

          END. /* date and time to end */
          ELSE IF INDEX(lv-part,':') > 0 
               THEN DO:
                   ASSIGN lv-end-time = SUBSTRING(lv-part,INDEX(lv-part,':') - 2,5)
                          lv-end-time:SCREEN-VALUE = lv-end-time.
               END. /* just end time */
               ELSE DO:
                   ASSIGN lv-end-date = SUBSTRING(lv-part,8,INDEX(lv-part,'^') - 8)
                          lv-end-date:SCREEN-VALUE = lv-end-date.
               END. /* just  end date */
          APPLY 'value-changed' TO lv-ending.
      END. /* ending */
   END. /* loop through schedule setting vars on screen */

   ASSIGN lv-schedule = v-schedule
          lv-schedule:SCREEN-VALUE = v-schedule.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScheduleForecast C-Win 
PROCEDURE ScheduleForecast :
def input param pv-iterations as int  no-undo.
def input param pv-sched      as char no-undo.

{{&core}run.i &program   = "schedule-forecast.p"
             &path      = "{&tsk}"
             &noper     = true
             &Appsrv    = "System"
             &procedure = "refresh"
             &params    = "(pv-iterations,pv-sched)"} 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScheduleParser C-Win 
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

if lv-problem then return 'problem'.
if lv-ending  then return 'ending'.
if lv-ended   then return 'ended'.
return 'passed'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendResult C-Win 
PROCEDURE SendResult :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output param pv-sched as char no-undo.
  pv-sched = lv-schedule.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set_context C-Win 
PROCEDURE set_context :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN lv-end-date:SENSITIVE in FRAME {&FRAME-NAME} = lv-ending:checked 
       lv-end-time:SENSITIVE = lv-ending:checked
       lv-first-day:SENSITIVE     = lv-first:checked
       lv-f-monday:SENSITIVE      = lv-first:checked
       lv-f-tuesday:SENSITIVE     = lv-first:checked
       lv-f-wednesday:SENSITIVE   = lv-first:checked
       lv-f-thursday:SENSITIVE    = lv-first:checked
       lv-f-friday:SENSITIVE      = lv-first:checked
       lv-f-saturday:SENSITIVE    = lv-first:checked
       lv-f-sunday:SENSITIVE      = lv-first:checked
       lv-last-day:SENSITIVE     = lv-last:checked
       lv-l-monday:SENSITIVE      = lv-last:checked
       lv-l-tuesday:SENSITIVE     = lv-last:checked
       lv-l-wednesday:SENSITIVE   = lv-last:checked
       lv-l-thursday:SENSITIVE    = lv-last:checked
       lv-l-friday:SENSITIVE      = lv-last:checked
       lv-l-saturday:SENSITIVE    = lv-last:checked
       lv-l-sunday:SENSITIVE      = lv-last:checked
       lv-startdates:SENSITIVE     = lv-usestartdates:checked
       lv-startdays:SENSITIVE     = lv-usestartdays:checked
       lv-userepeat:SENSITIVE = lv-usestarttime:checked
       lv-starttime:SENSITIVE = lv-usestarttime:checked
       lv-repeat:SENSITIVE    = lv-usestarttime:checked
       lv-repeat:SENSITIVE = (lv-userepeat:checked AND lv-userepeat:SENSITIVE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

