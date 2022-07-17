&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-auditconfig NO-UNDO LIKE zen-auditconfig.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
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


CREATE WIDGET-POOL.
&glob title-text    Audit Configuration
&glob table-name    zen-auditconfig
&glob Unique-key    {&table-name}tableid 
&glob KeepRefreshButton 
&Glob ImmediateQuery

/* ***************************  Definitions  ************************** */
{app-paths.i}

/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */

/* report return data table definitions */
&glob ReportReturnTable returntable
define temp-table t-{&ReportReturnTable} no-undo like {&table-name}.
/* temp-table can be any definition not just like table-name 
   must be the same in the report creation procedure .p */

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
&Scoped-define INTERNAL-TABLES t-zen-auditconfig

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-auditconfig.SearchFieldName ~
t-zen-auditconfig.recordcreates t-zen-auditconfig.recorddeletes ~
t-zen-auditconfig.tablename 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-auditconfig NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-auditconfig NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-auditconfig
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-auditconfig


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint lv-auduser lv-SearchKey rs-mode ~
lv-file btn-fields from-date btn-go to-date 
&Scoped-Define DISPLAYED-FIELDS t-zen-auditconfig.tablename ~
t-zen-auditconfig.SearchFieldName t-zen-auditconfig.keyfield ~
t-zen-auditconfig.recordcreates t-zen-auditconfig.recorddeletes ~
t-zen-auditconfig.Active 
&Scoped-define DISPLAYED-TABLES t-zen-auditconfig
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-auditconfig
&Scoped-Define DISPLAYED-OBJECTS lv-auduser lv-SearchKey rs-mode lv-file ~
from-date to-date 

/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-zen-auditconfig.tablename ~
t-zen-auditconfig.SearchFieldName t-zen-auditconfig.keyfield ~
t-zen-auditconfig.recordcreates t-zen-auditconfig.recorddeletes ~
t-zen-auditconfig.Active 
&Scoped-define Edit-List t-zen-auditconfig.SearchFieldName ~
t-zen-auditconfig.keyfield t-zen-auditconfig.recordcreates ~
t-zen-auditconfig.recorddeletes t-zen-auditconfig.Active 
&Scoped-define List-6 lv-auduser lv-SearchKey rs-mode lv-file from-date ~
to-date 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-fields 
     LABEL "Include/Exclude Fields" 
     SIZE 30 BY 1.14.

DEFINE BUTTON btn-go 
     LABEL "Run" 
     SIZE 14 BY 1.14.

DEFINE VARIABLE from-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 TOOLTIP "Clear ALL Tables Before This Date" NO-UNDO.

DEFINE VARIABLE lv-auduser AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "User" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE lv-file AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Table" 
     VIEW-AS FILL-IN 
     SIZE 26.8 BY 1 NO-UNDO.

DEFINE VARIABLE lv-SearchKey AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Search Key" 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE to-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/2525 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 TOOLTIP "Clear ALL Tables Before This Date" NO-UNDO.

DEFINE VARIABLE rs-mode AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Report", "r",
"Purge", "p"
     SIZE 24 BY .71 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-auditconfig SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-auditconfig.SearchFieldName FORMAT "X(40)":U WIDTH 51.6
      t-zen-auditconfig.recordcreates COLUMN-LABEL "Create" WIDTH 10.2
      t-zen-auditconfig.recorddeletes COLUMN-LABEL "Delete" WIDTH 9.4
      t-zen-auditconfig.tablename
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 119 BY 12.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     br-maint AT ROW 1.1 COL 11 HELP
          "Select the record to edit."
     lv-auduser AT ROW 18.62 COL 16 COLON-ALIGNED WIDGET-ID 14
     lv-SearchKey AT ROW 17.67 COL 61 COLON-ALIGNED WIDGET-ID 12
     rs-mode AT ROW 17.67 COL 106 NO-LABEL WIDGET-ID 16
     lv-file AT ROW 17.57 COL 16 COLON-ALIGNED HELP
          "Matching Table Name" WIDGET-ID 8
     t-zen-auditconfig.tablename AT ROW 13.62 COL 27.2 COLON-ALIGNED FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 48 BY 1
     t-zen-auditconfig.SearchFieldName AT ROW 14.62 COL 27.2 COLON-ALIGNED FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 48 BY 1
     t-zen-auditconfig.keyfield AT ROW 15.62 COL 27.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 48 BY 1
     t-zen-auditconfig.recordcreates AT ROW 13.71 COL 80
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY 1
     t-zen-auditconfig.recorddeletes AT ROW 14.71 COL 80
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY 1
     t-zen-auditconfig.Active AT ROW 15.71 COL 80 WIDGET-ID 2
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY 1
     btn-fields AT ROW 13.71 COL 102
     from-date AT ROW 18.62 COL 61.4 COLON-ALIGNED WIDGET-ID 4
     btn-go AT ROW 18.48 COL 108.2 WIDGET-ID 6
     to-date AT ROW 18.62 COL 86.8 COLON-ALIGNED WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1
         SIZE 134.4 BY 19.91
         TITLE "Audit Setup Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-auditconfig T "?" NO-UNDO schadm zen-auditconfig
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
         COLUMN             = 21.8
         ROW                = 7.38
         HEIGHT             = 26.62
         WIDTH              = 136.4
         MAX-HEIGHT         = 54.81
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 54.81
         VIRTUAL-WIDTH      = 320
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
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
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB br-maint 1 frame-maint */
/* SETTINGS FOR TOGGLE-BOX t-zen-auditconfig.Active IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN from-date IN FRAME frame-maint
   6                                                                    */
/* SETTINGS FOR FILL-IN t-zen-auditconfig.keyfield IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN lv-auduser IN FRAME frame-maint
   6                                                                    */
/* SETTINGS FOR FILL-IN lv-file IN FRAME frame-maint
   6                                                                    */
/* SETTINGS FOR FILL-IN lv-SearchKey IN FRAME frame-maint
   6                                                                    */
/* SETTINGS FOR TOGGLE-BOX t-zen-auditconfig.recordcreates IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-zen-auditconfig.recorddeletes IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR RADIO-SET rs-mode IN FRAME frame-maint
   6                                                                    */
/* SETTINGS FOR FILL-IN t-zen-auditconfig.SearchFieldName IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-zen-auditconfig.tablename IN FRAME frame-maint
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN to-date IN FRAME frame-maint
   6                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-auditconfig"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-zen-auditconfig.SearchFieldName
"t-zen-auditconfig.SearchFieldName" ? "X(40)" "character" ? ? ? ? ? ? no ? no no "51.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-auditconfig.recordcreates
"t-zen-auditconfig.recordcreates" "Create" ? "logical" ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.t-zen-auditconfig.recorddeletes
"t-zen-auditconfig.recorddeletes" "Delete" ? "logical" ? ? ? ? ? ? no ? no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.t-zen-auditconfig.tablename
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-fields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-fields window-maint
ON CHOOSE OF btn-fields IN FRAME frame-maint /* Include/Exclude Fields */
DO:
    runchild("{&aud}auditfield.w",this-procedure).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-go window-maint
ON CHOOSE OF btn-go IN FRAME frame-maint /* Run */
DO:
run cleardown.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/*******************
    DONT FORGET TO CHECK DIR LOCATION FOR BELOW INCLUDE !!!!!!!
    for path to appsrver .p 
********************/
{{&core}commonmaint.i &path = "{&aud}{&srv}"}  
                        /* &extraparams = "???"} */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
from-date = today - 365.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearDown window-maint 
PROCEDURE ClearDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-num as int no-undo.
  assign frame {&frame-name} {&list-6}.

message 'This Will Process Audit records' skip
        'For table ' lv-file  skip
        'between ' from-date ' and ' to-date '.' skip
        'Continue?'
view-as alert-box question buttons yes-no update x as log.
if not x then return.

    {{&core}run.i 
      &program   = "zen-auditdetail.p"
      &path      = "{&aud}{&srv}"
      &Appsrv    = "System"
      &procedure = "Cleardown"
      &params    = "(lv-file,lv-searchkey,lv-auduser,from-date,to-date,rs-mode,
                     output lv-num)"}

message 'Complete' skip
       lv-num ' Audit records processed'.

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
  DISPLAY lv-auduser lv-SearchKey rs-mode lv-file from-date to-date 
      WITH FRAME frame-maint.
  IF AVAILABLE t-zen-auditconfig THEN 
    DISPLAY t-zen-auditconfig.tablename t-zen-auditconfig.SearchFieldName 
          t-zen-auditconfig.keyfield t-zen-auditconfig.recordcreates 
          t-zen-auditconfig.recorddeletes t-zen-auditconfig.Active 
      WITH FRAME frame-maint.
  ENABLE br-maint lv-auduser lv-SearchKey rs-mode lv-file btn-fields from-date 
         btn-go to-date 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-after-print-trigger window-maint 
PROCEDURE local-after-print-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var lv-ok as log no-undo.
def var x     as int no-undo.

/*display a field from returned table to check its actually working */
for each t-{&ReportReturnTable}:
   x = x + 1.
   message t-{&ReportReturnTable}.tablename x skip
      'Next Record ?'
      view-as alert-box  
      buttons yes-no update lv-ok.
   if not lv-ok then leave.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn window-maint 
PROCEDURE Local-ChildReturn :
def input param pv-from as char no-undo.
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
def input param pv-to as handle no-undo.
case pv-to:private-data:
    when '{&core}audit/auditfield.w' then Run Refresh In pv-to 
            (t-zen-auditconfig.tablename).
end case.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh window-maint 
PROCEDURE Refresh :
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
  def var v-recid as recid no-undo. 
  def buffer b-{&table-name} for t-{&table-name}.
                                                                                   
/*    if index(',',t-zen-classCode.code:screen-value in frame {&frame-name}) ne 0      */
/*    then do:                                                                         */
/*        message msg(39,'save','Code','Contains ,','') view-as alert-box information. */
/*        return string(t-zen-classCode.code:handle).                                  */
/*    end.                                                                             */


   return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

