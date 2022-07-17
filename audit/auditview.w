&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME zwin-main


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-AuditDetail NO-UNDO LIKE Zen-AuditDetail.
DEFINE TEMP-TABLE t-Zen-AuditLine NO-UNDO LIKE Zen-AuditLine.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS zwin-main 
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
/*          This .W file was created with the Progress UIB.             */
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
&glob NoNew             /* no new button */
&glob nobrowse
&glob NoEdit            /* no edit button */
&glob NoSave            /* no save button */
&glob NoDelete          /* no delete button */
&glob NoUndo            /* no undo button */
&glob NoAudit           /* no audit button */
&glob NoExport          /* no export button */
&glob NoPrint           /* no print button */
&glob nohelp

/* CHANGEABLE BITS HERE change table-name to appropriate db table */
&glob Table-name zen-auditdetail
&glob unique-key {&table-name}tableid
&glob nochangedcheck
&undefine suppresswindow
def var v-list as char no-undo.

define temp-table w-field no-undo
    field w-key     like t-Zen-AuditDetail.{&table-name}tableid
    field w-field   like t-Zen-AuditLine.FieldName 
    field w-label   like t-Zen-AuditLine.FieldName
    field W-now     as char Column-label 'After Value'    
    field w-data    as char Column-label 'Before Value'
    INDEX order w-key w-field.
    
define temp-table w-hist no-undo
    field w-key     as date
    field w-time    as int
    field w-data    as char Column-label 'Value'
    INDEX order w-key descending w-time descending.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME main-frame
&Scoped-define BROWSE-NAME br-data

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES w-field t-Zen-AuditDetail t-Zen-Auditline ~
w-hist

/* Definitions for BROWSE br-data                                       */
&Scoped-define FIELDS-IN-QUERY-br-data if rs-namesdescs = "names" then w-field else w-label w-field.w-data w-now   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-data   
&Scoped-define SELF-NAME br-data
&Scoped-define QUERY-STRING-br-data FOR EACH w-field where w-field.w-key = t-Zen-AuditDetail.{&table-name}tableid
&Scoped-define OPEN-QUERY-br-data OPEN QUERY {&SELF-NAME} FOR EACH w-field where w-field.w-key = t-Zen-AuditDetail.{&table-name}tableid.
&Scoped-define TABLES-IN-QUERY-br-data w-field
&Scoped-define FIRST-TABLE-IN-QUERY-br-data w-field


/* Definitions for BROWSE br-head                                       */
&Scoped-define FIELDS-IN-QUERY-br-head t-Zen-AuditDetail.AUDitDate string(t-Zen-AuditDetail.AUDitTime,'HH:MM:SS') t-Zen-AuditDetail.auditaction t-Zen-AuditDetail.ByName t-Zen-AuditDetail.SearchFieldData   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-head   
&Scoped-define SELF-NAME br-head
&Scoped-define OPEN-QUERY-br-head  run GetAuditRecords.      OPEN QUERY {&SELF-NAME}     FOR EACH t-Zen-AuditDetail             NO-LOCK, ~
                first t-Zen-Auditline of t-Zen-AuditDetail             no-lock outer-join             by t-Zen-AuditDetail.AUDitDate descending             by t-Zen-AuditDetail.AUDitTime descending.
&Scoped-define TABLES-IN-QUERY-br-head t-Zen-AuditDetail t-Zen-Auditline
&Scoped-define FIRST-TABLE-IN-QUERY-br-head t-Zen-AuditDetail
&Scoped-define SECOND-TABLE-IN-QUERY-br-head t-Zen-Auditline


/* Definitions for BROWSE br-hist                                       */
&Scoped-define FIELDS-IN-QUERY-br-hist w-hist.w-key string(w-hist.w-time,'hh:mm') w-hist.w-data   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-hist   
&Scoped-define SELF-NAME br-hist
&Scoped-define QUERY-STRING-br-hist FOR EACH w-hist
&Scoped-define OPEN-QUERY-br-hist OPEN QUERY {&SELF-NAME} FOR EACH w-hist.
&Scoped-define TABLES-IN-QUERY-br-hist w-hist
&Scoped-define FIRST-TABLE-IN-QUERY-br-hist w-hist


/* Definitions for FRAME main-frame                                     */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb-Table lv-FromDate lv-ToDate lv-searchby ~
lv-searchKey lv-Adds lv-Updates lv-Deletes br-head rs-NamesDescs br-data ~
br-hist 
&Scoped-Define DISPLAYED-OBJECTS cb-Table lv-FromDate lv-ToDate lv-searchby ~
lv-searchKey lv-Adds lv-Updates lv-Deletes rs-NamesDescs lv-searchfieldname ~
vc-record-title vc-field-title vc-data-title 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cb-Table lv-FromDate lv-ToDate lv-searchby ~
lv-searchKey lv-Adds lv-Updates lv-Deletes rs-NamesDescs 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldDataType zwin-main 
FUNCTION GetFieldDataType RETURNS CHARACTER
  (pv-table as char,
   pv-field as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldDesc zwin-main 
FUNCTION GetFieldDesc RETURNS CHARACTER
  (pv-table as char,
   pv-field as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetRelatedFunction zwin-main 
FUNCTION GetRelatedFunction RETURNS CHARACTER
  (pv-table as char,
   pv-field as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetSearchkeyname zwin-main 
FUNCTION GetSearchkeyname RETURNS CHARACTER
  ( pv-table as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR zwin-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cb-Table AS CHARACTER FORMAT "X(256)":U 
     LABEL "Table" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 50 BY 1 TOOLTIP "Table that the action was carried out on" NO-UNDO.

DEFINE VARIABLE lv-FromDate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/90 
     LABEL "From" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16.8 BY .91 TOOLTIP "Date from which searching should begin" NO-UNDO.

DEFINE VARIABLE lv-searchfieldname AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 60 BY .62 NO-UNDO.

DEFINE VARIABLE lv-searchKey AS CHARACTER FORMAT "X(255)":U 
     LABEL "Search Key" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 45 BY .91 NO-UNDO.

DEFINE VARIABLE lv-ToDate AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16.8 BY .91 TOOLTIP "Date to which searching should end" NO-UNDO.

DEFINE VARIABLE vc-data-title AS CHARACTER FORMAT "X(256)":U INITIAL "Data History" 
      VIEW-AS TEXT 
     SIZE 20.4 BY .62 NO-UNDO.

DEFINE VARIABLE vc-field-title AS CHARACTER FORMAT "X(256)":U INITIAL "Field History" 
      VIEW-AS TEXT 
     SIZE 16.4 BY .62 NO-UNDO.

DEFINE VARIABLE vc-record-title AS CHARACTER FORMAT "X(256)":U INITIAL "Record History" 
      VIEW-AS TEXT 
     SIZE 19.8 BY .62 NO-UNDO.

DEFINE VARIABLE lv-searchby AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "All", "",
"TableId", "t",
"KeyField", "k"
     SIZE 30 BY .62 NO-UNDO.

DEFINE VARIABLE rs-NamesDescs AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Field Names", "names",
"Descriptions", "desc"
     SIZE 35.4 BY .71 TOOLTIP "Display database field names or full field descriptions" NO-UNDO.

DEFINE VARIABLE lv-Adds AS LOGICAL INITIAL yes 
     LABEL "Include Additions" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 TOOLTIP "Whether to include additions" NO-UNDO.

DEFINE VARIABLE lv-Deletes AS LOGICAL INITIAL yes 
     LABEL "Include Deletes" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 TOOLTIP "Whether to include deletes" NO-UNDO.

DEFINE VARIABLE lv-Updates AS LOGICAL INITIAL yes 
     LABEL "Include Updates" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 TOOLTIP "Whether to include updates" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-data FOR 
      w-field SCROLLING.

DEFINE QUERY br-head FOR 
      t-Zen-AuditDetail, 
      t-Zen-Auditline SCROLLING.

DEFINE QUERY br-hist FOR 
      w-hist SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-data
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-data zwin-main _FREEFORM
  QUERY br-data DISPLAY
      if rs-namesdescs = "names" 
        then w-field
        else w-label Format "X(30)" label "Field"
    w-field.w-data  format "x(50)"
    w-now           format "x(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-BOX SEPARATORS SIZE 134.8 BY 10.67.

DEFINE BROWSE br-head
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-head zwin-main _FREEFORM
  QUERY br-head DISPLAY
      t-Zen-AuditDetail.AUDitDate Column-label "Date            "
      string(t-Zen-AuditDetail.AUDitTime,'HH:MM:SS') column-label "Time       "
      t-Zen-AuditDetail.auditaction
      t-Zen-AuditDetail.ByName column-label 'By User' format 'x(20)'
    t-Zen-AuditDetail.SearchFieldData  column-label "Key" format 'x(255)'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-BOX SEPARATORS SIZE 191 BY 10.48 FIT-LAST-COLUMN.

DEFINE BROWSE br-hist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-hist zwin-main _FREEFORM
  QUERY br-hist DISPLAY
      w-hist.w-key  column-label 'Changed On' format '99/99/9999'
string(w-hist.w-time,'hh:mm') column-label 'At' format 'xxxxxx'
w-hist.w-data Column-label 'Changed From'    format 'x(50)'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-BOX SEPARATORS SIZE 56.6 BY 10.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME main-frame
     cb-Table AT ROW 1.1 COL 21 COLON-ALIGNED
     lv-FromDate AT ROW 1.24 COL 116 COLON-ALIGNED
     lv-ToDate AT ROW 1.24 COL 140 COLON-ALIGNED
     lv-searchby AT ROW 1.33 COL 75 NO-LABEL
     lv-searchKey AT ROW 2.91 COL 23 COLON-ALIGNED
     lv-Adds AT ROW 2.91 COL 118
     lv-Updates AT ROW 2.91 COL 139.4
     lv-Deletes AT ROW 2.91 COL 160.8
     br-head AT ROW 4.57 COL 4
     rs-NamesDescs AT ROW 15.24 COL 30 NO-LABEL
     br-data AT ROW 16.1 COL 4
     br-hist AT ROW 16.1 COL 139
     lv-searchfieldname AT ROW 2.19 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     vc-record-title AT ROW 3.86 COL 11 COLON-ALIGNED NO-LABEL
     vc-field-title AT ROW 15.14 COL 10 COLON-ALIGNED NO-LABEL
     vc-data-title AT ROW 15.38 COL 137 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 196 BY 25.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-Zen-AuditDetail T "?" NO-UNDO schadm Zen-AuditDetail
      TABLE: t-Zen-AuditLine T "?" NO-UNDO schadm Zen-AuditLine
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW zwin-main ASSIGN
         HIDDEN             = YES
         TITLE              = "Audit Detail Viewer"
         HEIGHT             = 26.14
         WIDTH              = 196
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW zwin-main
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME main-frame
   FRAME-NAME                                                           */
/* BROWSE-TAB br-head lv-Deletes main-frame */
/* BROWSE-TAB br-data rs-NamesDescs main-frame */
/* BROWSE-TAB br-hist br-data main-frame */
ASSIGN 
       br-head:ALLOW-COLUMN-SEARCHING IN FRAME main-frame = TRUE
       br-head:COLUMN-RESIZABLE IN FRAME main-frame       = TRUE.

/* SETTINGS FOR COMBO-BOX cb-Table IN FRAME main-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-Adds IN FRAME main-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-Deletes IN FRAME main-frame
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-FromDate IN FRAME main-frame
   1                                                                    */
/* SETTINGS FOR RADIO-SET lv-searchby IN FRAME main-frame
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-searchfieldname IN FRAME main-frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-searchKey IN FRAME main-frame
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-ToDate IN FRAME main-frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-Updates IN FRAME main-frame
   1                                                                    */
/* SETTINGS FOR RADIO-SET rs-NamesDescs IN FRAME main-frame
   1                                                                    */
/* SETTINGS FOR FILL-IN vc-data-title IN FRAME main-frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vc-field-title IN FRAME main-frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vc-record-title IN FRAME main-frame
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(zwin-main)
THEN zwin-main:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-data
/* Query rebuild information for BROWSE br-data
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH w-field
where w-field.w-key = t-Zen-AuditDetail.{&table-name}tableid
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE br-data */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-head
/* Query rebuild information for BROWSE br-head
     _START_FREEFORM

run GetAuditRecords.

    OPEN QUERY {&SELF-NAME}
    FOR EACH t-Zen-AuditDetail
            NO-LOCK,
         first t-Zen-Auditline of t-Zen-AuditDetail
            no-lock outer-join
            by t-Zen-AuditDetail.AUDitDate descending
            by t-Zen-AuditDetail.AUDitTime descending.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE br-head */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-hist
/* Query rebuild information for BROWSE br-hist
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH w-hist.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE br-hist */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME main-frame
/* Query rebuild information for FRAME main-frame
     _Query            is NOT OPENED
*/  /* FRAME main-frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME zwin-main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL zwin-main zwin-main
ON END-ERROR OF zwin-main /* Audit Detail Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL zwin-main zwin-main
ON WINDOW-CLOSE OF zwin-main /* Audit Detail Viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-data
&Scoped-define SELF-NAME br-data
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-data zwin-main
ON VALUE-CHANGED OF br-data IN FRAME main-frame
DO:
    run GetFldHist.       
    {&open-query-br-hist} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-head
&Scoped-define SELF-NAME br-head
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-head zwin-main
ON VALUE-CHANGED OF br-head IN FRAME main-frame
DO:
    assign rs-namesdescs.

if avail t-Zen-AuditDetail 
then  
    case lv-searchby:
        when 't' then lv-searchkey:screen-value = t-Zen-AuditDetail.SourceKey.
        when 'k' then lv-searchkey:screen-value = t-Zen-AuditDetail.Searchfielddata.
        otherwise lv-searchkey:screen-value = ''.
    end case.
   
    run getflds.
    {&open-query-br-data} 
    apply "VALUE-CHANGED":U to br-data.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-Table zwin-main
ON VALUE-CHANGED OF cb-Table IN FRAME main-frame /* Table */
DO:
    assign lv-searchkey:screen-value = ''.
    lv-searchfieldname:screen-value = GetSearchkeyname(self:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-searchby
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-searchby zwin-main
ON VALUE-CHANGED OF lv-searchby IN FRAME main-frame
DO:
  
  assign frame {&frame-name} lv-searchby.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-NamesDescs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-NamesDescs zwin-main
ON VALUE-CHANGED OF rs-NamesDescs IN FRAME main-frame
DO:
  assign rs-namesdescs.
  {&OPEN-QUERY-br-data}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-data
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK zwin-main 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{{&core}commonmaint.i &path = "{&sys}{&srv}"}
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {{&core}sec-chk.i}
    RUN enable_UI.
    {{&core}wid-chk.i}
  {{&core}focus.i}
 IF NOT THIS-PROCEDURE:PERSISTENT OR SESSION:DISPLAY-TYPE ne "GUI":U then
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignWidgets zwin-main 
PROCEDURE AssignWidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign frame {&frame-name}
    {&list-1}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildcombos zwin-main 
PROCEDURE buildcombos :
buildcombo(cb-table:handle in frame {&frame-name},
           "Zen-AuditConfig",
           "tablename",
           "tablename",
           "",
           "by tablename", 
           no,
           no).

cb-table:screen-value = entry(1,cb-table:list-items,'{&combodelim}').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI zwin-main  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(zwin-main)
  THEN DELETE WIDGET zwin-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dispwidgets zwin-main 
PROCEDURE dispwidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
display {&list-1} 
        {&display-list}
with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI zwin-main  _DEFAULT-ENABLE
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
  DISPLAY cb-Table lv-FromDate lv-ToDate lv-searchby lv-searchKey lv-Adds 
          lv-Updates lv-Deletes rs-NamesDescs lv-searchfieldname vc-record-title 
          vc-field-title vc-data-title 
      WITH FRAME main-frame IN WINDOW zwin-main.
  ENABLE cb-Table lv-FromDate lv-ToDate lv-searchby lv-searchKey lv-Adds 
         lv-Updates lv-Deletes br-head rs-NamesDescs br-data br-hist 
      WITH FRAME main-frame IN WINDOW zwin-main.
  {&OPEN-BROWSERS-IN-QUERY-main-frame}
  VIEW zwin-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAuditRecords zwin-main 
PROCEDURE GetAuditRecords :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign frame {&frame-name}
    {&list-1}.

empty temp-table t-{&table-name}.
empty temp-table t-zen-auditline.

{{&core}run.i &program   = "{&table-name}.p"
             &path      = "{&aud}{&srv}"
             &Appsrv    = "System"  
             &noper     = true
             &procedure = "open-querykey"
             &params    = "(cb-table,
                            lv-searchkey,
                            lv-fromdate,
                            lv-todate,  
                            lv-adds,    
                            lv-updates, 
                            lv-deletes, 
                            lv-searchby, 
                            OUTPUT TABLE t-{&table-name},
                            OUTPUT TABLE t-zen-auditline)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFldHist zwin-main 
PROCEDURE GetFldHist :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def buffer b-AuditDetail for t-Zen-AuditDetail.
def buffer b-Auditline   for t-Zen-Auditline.

def var lv-datatype as char no-undo.

for each w-hist:
    delete w-hist.
end.
if not avail w-field then return.
lv-datatype = GetFieldDatatype(t-Zen-AuditDetail.TableName,w-field.w-field).
if lv-datatype = ? then do:
    message 'Invalid Field' view-as alert-box.
    return error.
end.

FOR EACH b-AuditDetail 
    WHERE b-AuditDetail.SourceKey = t-Zen-AuditDetail.SourceKey
    no-lock:
    for each b-auditline Of b-AuditDetail
           where b-AuditLine.FieldName  = w-field.w-field
           no-lock:
        create w-hist.
        assign w-hist.w-key = b-AuditDetail.AUDitDate   
               w-hist.w-time = b-AuditDetail.AUDitTime.
        case lv-datatype:
            when 'character' then w-hist.w-data = b-AuditLine.DataChar. 
            otherwise do:
                 if GetRelatedFunction(t-Zen-AuditDetail.TableName,w-field.w-field) ne ''
                    then w-hist.w-data = b-AuditLine.DataChar.
                 else case lv-datatype:
                        when 'date'    then w-hist.w-data = string(b-AuditLine.DataDate).
                        when 'decimal' then w-hist.w-data = string(b-AuditLine.DataDec).
                        when 'integer' then w-hist.w-data = string(b-AuditLine.DataInt).
                        when 'logical' then w-hist.w-data = string(b-AuditLine.DataLog).
                      end case.
            end.            
        end case.
    end.
end.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFlds zwin-main 
PROCEDURE GetFlds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def buffer b-line for t-Zen-Auditline.
def buffer b-head for t-Zen-Auditdetail.

def var lv-usefunct as log  no-undo.
def var lv-datatype as char no-undo.

for each w-field:
    delete w-field.
end.

find b-head where b-head.{&unique-key} = t-Zen-Auditdetail.{&unique-key} 
        no-lock no-error.

find PREV b-head where b-head.TableName = t-Zen-AuditDetail.TableName
                   and b-head.SourceKey = t-zen-AuditDetail.SourceKey
                   AND can-find(first b-line Of b-head Where b-line.FieldName  = t-Zen-AuditLine.FieldName)
                 use-index audorder
        no-lock no-error.


open query q-audline
    for each t-Zen-Auditline of t-Zen-AuditDetail no-lock.
             
get first q-audline.

do while not query-off-end("q-audline"):
    if getrelatedFunction(t-zen-auditdetail.TableName,t-Zen-AuditLine.FieldName) ne ''
    THEN lv-usefunct = true.

    do while avail b-head:
         if can-find(last b-line Of b-head where b-line.FieldName  = t-Zen-AuditLine.FieldName)
        then do:
            find last b-line of b-head where b-line.FieldName  = t-Zen-AuditLine.FieldName
                                       no-lock no-error.
            leave.
        end.
        find prev b-head where b-head.TableName = t-Zen-AuditDetail.TableName
                           and b-head.SourceKey = t-zen-AuditDetail.SourceKey
                           AND can-find(first b-line Of b-head where b-line.FieldName  = t-Zen-AuditLine.FieldName)
                         use-index audorder
                         no-lock no-error.      
    end.   

    create w-field.
    assign
        w-key       = t-Zen-AuditLine.zen-auditdetailtableid
        w-field     = t-Zen-AuditLine.FieldName.
    lv-datatype = GetFieldDataType(t-Zen-AuditDetail.TableName,t-Zen-AuditLine.FieldName).
    w-label     = GetFieldDesc(t-Zen-AuditDetail.TableName,t-Zen-AuditLine.FieldName).
    
    case lv-datatype:
        when 'character' then assign
                                w-now  = if avail b-line then b-line.DataChar else 'Current'
                                w-data = t-Zen-AuditLine.DataChar. 
        otherwise do:
             if lv-usefunct then assign
                                    w-now  = if avail b-line then b-line.DataChar else 'Current'
                                    w-data = t-Zen-AuditLine.DataChar.
             else case lv-datatype:
                    when 'date'      then assign
                                            w-now  = if avail b-line then string(b-line.DataDate) else 'Current'
                                            w-data = string(t-Zen-AuditLine.DataDate).
                    when 'decimal'   then 
                                        assign
                                            w-now  = if avail b-line then string(b-line.DataDec) else 'Current'
                                            w-data = string(t-Zen-AuditLine.DataDec).
                    when 'integer'   then assign
                                            w-now  = if avail b-line then string(b-line.DataInt) else 'Current'
                                            w-data = string(t-Zen-AuditLine.DataInt).
                    when 'logical'   then assign
                                            w-now  = if avail b-line then string(b-line.DataLog) else 'Current' 
                                            w-data = string(t-Zen-AuditLine.DataLog).
                  end case.
        end.            
    end.
    get next q-audline.   
end.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn zwin-main 
PROCEDURE Local-ChildReturn :
def input param pv-from as char no-undo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise zwin-main 
PROCEDURE local-initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run buildcombos.

apply 'value-changed' to cb-table in frame {&frame-name}.
return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-openquery zwin-main 
PROCEDURE local-openquery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN AssignWidgets.
        for each w-field:
            delete w-field.
        end.

        for each w-hist:
            delete w-hist.
        end.
{&OPEN-QUERY-br-head}

if avail t-Zen-Auditdetail then 
    apply 'value-changed' to br-head in frame {&frame-name}.
else do:
        {&OPEN-QUERY-br-data}
        {&OPEN-QUERY-br-hist}
end.

return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-Query-trigger zwin-main 
PROCEDURE local-Query-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

if cb-Table:screen-value In Frame {&frame-name} = '<< all >>' then do:
    message 'please Select a table'
    view-as alert-box.
    return no-apply.
end.


   RUN GetAuditRecords. 

run openquery.

return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-undo-trigger zwin-main 
PROCEDURE local-undo-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN resetWidgets.
return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Update-Child-Procedures zwin-main 
PROCEDURE Local-Update-Child-Procedures :
def input param pv-to as handle no-undo.

case pv-to:private-data:
/*     when 'order-maint.w' then run refresh in  pv-to (t-customer.cust-num).  */
/*     when 'sr-maint.w'    then run refresh in  pv-to.                        */
end case.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh zwin-main 
PROCEDURE refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-sourcekey as char no-undo.
def input param pv-table     as char no-undo.

    assign       
        lv-searchkey:screen-value in frame {&frame-name} = pv-sourcekey
        cb-table:screen-value    = pv-table
        lv-FromDate:screen-value = string(today - 1,'99/99/9999')
        lv-ToDate:screen-value   = string(today,'99/99/9999').
        
   RUN Query-trigger.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetwidgets zwin-main 
PROCEDURE resetwidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN lv-FromDate     = today - 30
       lv-ToDate       = today
       cb-Table        = entry(1,cb-table:list-items in frame {&frame-name})
       lv-Adds         = true
       lv-Updates      = true
       lv-Deletes      = true.
run dispwidgets.                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldDataType zwin-main 
FUNCTION GetFieldDataType RETURNS CHARACTER
  (pv-table as char,
   pv-field as char) :
   def var lv-datatype as char no-undo.
   
   {{&core}run.i &program   = "{&table-name}.p"
                &path      = "{&aud}{&srv}"
                &Appsrv    = "System"  
                &noper     = true
                &procedure = "fielddatatype"
                &params    = "(pv-table,
                               pv-field,  
                              OUTPUT lv-datatype)"}

  RETURN lv-datatype.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldDesc zwin-main 
FUNCTION GetFieldDesc RETURNS CHARACTER
  (pv-table as char,
   pv-field as char) :
   def var lv-desc as char no-undo.
   
   {{&core}run.i &program   = "{&table-name}.p"
                &path      = "{&aud}{&srv}"
                &Appsrv    = "System"  
                &noper     = true
                &procedure = "fielddesc"
                &params    = "(pv-table,
                               pv-field,  
                              OUTPUT lv-desc)"}

  RETURN lv-desc.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetRelatedFunction zwin-main 
FUNCTION GetRelatedFunction RETURNS CHARACTER
  (pv-table as char,
   pv-field as char) :
   
   def var lv-function as char no-undo.
   
   {{&core}run.i &program   = "{&table-name}.p"
                &path      = "{&aud}{&srv}"
                &Appsrv    = "System"  
                &noper     = true
                &procedure = "relatedfunction"
                &params    = "(pv-table,
                               pv-field,  
                              OUTPUT lv-function)"}

  RETURN lv-function.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetSearchkeyname zwin-main 
FUNCTION GetSearchkeyname RETURNS CHARACTER
  ( pv-table as char ) :

def var lv-label as char no-undo.
{{&core}run.i &program   = "{&table-name}.p"
             &path      = "{&aud}{&srv}"
             &Appsrv    = "System"  
             &noper     = true
             &procedure = "Getsearchlabel"
             &params    = "(pv-table,
                            OUTPUT lv-label)"}
  RETURN lv-label.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

