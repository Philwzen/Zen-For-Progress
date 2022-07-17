&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def stream op.
def stream op2.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-auduser lv-file lv-SearchKey from-date ~
to-date Btn_OK rs-mode btn-detail Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS lv-auduser lv-file lv-SearchKey from-date ~
to-date rs-mode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 lv-auduser lv-file lv-SearchKey from-date to-date ~
rs-mode 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-detail 
     LABEL "Detail Report" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "Run" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

def var from-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1700 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 TOOLTIP "Clear ALL Tables Before This Date" NO-UNDO.

def var lv-auduser as char FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

def var lv-file as char FORMAT "X(256)":U 
     LABEL "Table" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

def var lv-SearchKey as char FORMAT "X(256)":U 
     LABEL "Search Key" 
     VIEW-AS FILL-IN 
     SIZE 41.8 BY .95 NO-UNDO.

def var to-date AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 TOOLTIP "Clear ALL Tables Before This Date" NO-UNDO.

def var rs-mode as char 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Count", "r",
"Purge", "p"
     SIZE 24 BY .71 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-auduser AT ROW 1.24 COL 16 COLON-ALIGNED WIDGET-ID 14
     lv-file AT ROW 2.43 COL 16 COLON-ALIGNED HELP
          "Matching Table Name" WIDGET-ID 8
     lv-SearchKey AT ROW 3.62 COL 16.2 COLON-ALIGNED WIDGET-ID 12
     from-date AT ROW 4.81 COL 16 COLON-ALIGNED WIDGET-ID 4
     to-date AT ROW 4.81 COL 42 COLON-ALIGNED WIDGET-ID 10
     Btn_OK AT ROW 6 COL 44
     rs-mode AT ROW 6.24 COL 19 NO-LABEL WIDGET-ID 16
     btn-detail AT ROW 7.43 COL 4 WIDGET-ID 20
     Btn_Cancel AT ROW 7.43 COL 44
     SPACE(6.19) SKIP(0.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Purge Audit Records"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN from-date IN FRAME Dialog-Frame
   6                                                                    */
/* SETTINGS FOR FILL-IN lv-auduser IN FRAME Dialog-Frame
   6                                                                    */
/* SETTINGS FOR FILL-IN lv-file IN FRAME Dialog-Frame
   6                                                                    */
/* SETTINGS FOR FILL-IN lv-SearchKey IN FRAME Dialog-Frame
   6                                                                    */
/* SETTINGS FOR RADIO-SET rs-mode IN FRAME Dialog-Frame
   6                                                                    */
/* SETTINGS FOR FILL-IN to-date IN FRAME Dialog-Frame
   6                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Purge Audit Records */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-detail Dialog-Frame
ON CHOOSE OF btn-detail IN FRAME Dialog-Frame /* Detail Report */
DO:
  run DetailReport.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Run */
DO:
    assign frame {&frame-name} {&list-6}.
def var lv-num as int no-undo.
def var lv-f as int no-undo.
run cleardown(lv-file,lv-searchkey,lv-auduser,from-date,to-date,rs-mode,
                     output lv-num,output lv-f).

message 'Processed ' lv-num ' Records and ' lv-f ' Fields.' 
view-as alert-box.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   to-date = today - 365.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearDown Dialog-Frame 
PROCEDURE ClearDown :
def input param pv-table as char no-undo.
def input param lv-searchkey as char no-undo.
def input param lv-auduser as char no-undo.
def input param from-date as date no-undo.
def input param to-date  as date no-undo.
def input param rs-mode as char no-undo.
def output param x as int no-undo.
def output param y as int no-undo.

def var lv-tstamp as char no-undo.
lv-tstamp = string(today,'99-99-9999') +  string(time) + '.d'.
output stream op close.
output stream op2 close.

if rs-mode = 'p' then do:
    output stream op to value('auditdetailpurge' + lv-tstamp).
    output stream op2 to value('auditlinepurge' + lv-tstamp).
end.

for each zen-auditdetail where zen-auditdetail.byname = lv-auduser
                           and zen-auditdetail.tablename begins pv-table
                           and zen-auditdetail.SearchFieldData begins lv-searchkey
                           and zen-auditdetail.auditdate > from-date 
                           and zen-auditdetail.auditdate < to-date 
                           exclusive-lock:
    if rs-mode = 'p' 
    then export stream op zen-auditdetail.                          
    
    for each zen-auditline of zen-auditdetail exclusive-lock.
        if rs-mode = 'p' 
        then do:
            export stream op2 zen-auditline.                          
            delete zen-auditline. 
        end.
        y = y + 1.
    end.
    if rs-mode = 'p' 
    then delete zen-auditdetail.
    x = x + 1.
end.
output stream op close.
output stream op2 close.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DetailReport Dialog-Frame 
PROCEDURE DetailReport :
def var x as int no-undo.
    def var y as int no-undo.
    def var lv-tstamp as char no-undo.
    lv-tstamp = string(today,'99-99-9999') +  string(time) + '.csv'.
    output stream op to value('auditrep' + lv-tstamp).
    put stream op unformatted 'Table,User,Count' skip.
    
    for each zen-auditdetail no-lock
                             break by tablename by byname:
        if last-of(byname) 
        then do:
            put stream op unformatted tablename ',' byname ',' y skip.
            y = 0.
        end.
        if last-of(tablename) 
        then do:
            put stream op unformatted tablename ',Total,' x skip.
            x = 0.
        end.
        x = x + 1.
        y = y + 1.
    end.
    output stream op close.
    message 'Report Complete in File ' skip 'auditrep' + lv-tstamp
    view-as alert-box.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY lv-auduser lv-file lv-SearchKey from-date to-date rs-mode 
      WITH FRAME Dialog-Frame.
  ENABLE lv-auduser lv-file lv-SearchKey from-date to-date Btn_OK rs-mode 
         btn-detail Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

