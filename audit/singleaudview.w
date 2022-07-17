&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-auditdetail NO-UNDO LIKE zen-auditdetail.
DEFINE TEMP-TABLE t-zen-auditline NO-UNDO LIKE zen-auditline.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                                                         */
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
/****************************************************************************** 
 this is a semi generic routine you only need to paint the screen widgets
 then edit pc-save for any fields not on the screen ie time stamps etc
 and edit validate-screen for any validation logic you require to allow record 
 to be saved 
********************************************************************************/
CREATE WIDGET-POOL.
{app-paths.i}
&glob KeepRefreshButton
/* ***************************  Definitions  ************************** */
/* CHANGEABLE BITS HERE change table-name to appropriate db table */
&glob Table-name zen-auditdetail
&glob unique-key {&table-name}tableid
&glob nochangedcheck
&glob nobrowse
&glob nobrowsesorting
&undefine suppresswindow 

def var lv-sourcekey as char no-undo.
def var cb-table     as char no-undo. 

&glob NoNew             /* no new button */
&glob NoEdit            /* no edit button */
&glob NoSave            /* no save button */
&glob NoDelete          /* no delete button */
/*&glob NoExport          /* no export button */*/
&glob NoUndo            /* no undo button */
&glob NoQuery           /* no query button */
&glob NoAudit           /* no audit button */
&glob NoPrint           /* no print button */
&glob maxdatao
&glob btnstartrow 39
def var lv-wid as handle no-undo.
lv-wid = current-window.
lv-wid:sensitive = false.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME a-frame-maint
&Scoped-define BROWSE-NAME br-maint

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-zen-auditdetail t-zen-auditline

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-auditdetail.auditdate string(t-zen-auditdetail.audittime,"hh:mm am") t-zen-auditdetail.byname t-zen-auditdetail.auditaction if t-zen-auditdetail.auditaction ne "create" then t-zen-auditline.fieldname else "" if t-zen-auditline.datachar ne ? then t-zen-auditline.datachar else if t-zen-auditline.datadate ne ? then string(t-zen-auditline.datadate) else if t-zen-auditline.datadec ne ? then string(t-zen-auditline.datadec) else if t-zen-auditline.dataint ne ? then string(t-zen-auditline.dataint) else if t-zen-auditline.datalog ne ? then string(t-zen-auditline.datalog) else if t-zen-auditline.datadatetime ne ? then string(t-zen-auditline.datadatetime,"99/99/9999 HH:MM:SS.SSS ") else ""   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint   
&Scoped-define SELF-NAME br-maint
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-auditdetail NO-LOCK, ~
             EACH t-zen-auditline OF t-zen-auditdetail  outer-join NO-LOCK by t-zen-auditdetail.auditdate descending by t-zen-auditdetail.audittime descending INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY {&SELF-NAME} FOR EACH t-zen-auditdetail NO-LOCK, ~
             EACH t-zen-auditline OF t-zen-auditdetail  outer-join NO-LOCK by t-zen-auditdetail.auditdate descending by t-zen-auditdetail.audittime descending INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-auditdetail t-zen-auditline
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-auditdetail
&Scoped-define SECOND-TABLE-IN-QUERY-br-maint t-zen-auditline


/* Definitions for FRAME a-frame-maint                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-auditdetail, 
      t-zen-auditline SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _FREEFORM
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-auditdetail.auditdate 
      column-label "Date"           format "99/99/9999":U width 16
   string(t-zen-auditdetail.audittime,"hh:mm am") 
      column-label "Time"                                 width 12
   t-zen-auditdetail.byname   
      column-label "LogonID"      format "X(8)":U       width 14
   t-zen-auditdetail.auditaction 
                                                        width 16.2
   if t-zen-auditdetail.auditaction ne "create"
       then t-zen-auditline.fieldname else "" 
      column-label "Field Name"   format "X(20)":U 
   if t-zen-auditline.datachar ne ? then t-zen-auditline.datachar 
      else if t-zen-auditline.datadate ne ? then string(t-zen-auditline.datadate)
      else if t-zen-auditline.datadec ne ? then string(t-zen-auditline.datadec)
      else if t-zen-auditline.dataint ne ? then string(t-zen-auditline.dataint)
      else if t-zen-auditline.datalog ne ? then string(t-zen-auditline.datalog) 
      else if t-zen-auditline.datadatetime ne ? then string(t-zen-auditline.datadatetime,"99/99/9999 HH:MM:SS.SSS ")
      else "" 
      column-label "Changed From" format "x(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS SEPARATORS SIZE 177 BY 17.1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME a-frame-maint
     br-maint AT ROW 1.29 COL 19 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 196.8 BY 17.67 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-auditdetail T "?" NO-UNDO schadm zen-auditdetail
      TABLE: t-zen-auditline T "?" NO-UNDO schadm zen-auditline
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Audit Detail"
         HEIGHT             = 17.81
         WIDTH              = 196.8
         MAX-HEIGHT         = 45.33
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.33
         VIRTUAL-WIDTH      = 256
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = yes
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
/* SETTINGS FOR WINDOW window-maint
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME a-frame-maint
   FRAME-NAME                                                           */
/* BROWSE-TAB br-maint 1 a-frame-maint */
ASSIGN 
       FRAME a-frame-maint:PRIVATE-DATA     = 
                "1".

ASSIGN 
       br-maint:COLUMN-RESIZABLE IN FRAME a-frame-maint       = TRUE
       br-maint:COLUMN-MOVABLE IN FRAME a-frame-maint         = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(window-maint)
THEN window-maint:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME a-frame-maint
/* Query rebuild information for FRAME a-frame-maint
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME a-frame-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH t-zen-auditdetail NO-LOCK,
      EACH t-zen-auditline OF t-zen-auditdetail  outer-join NO-LOCK
by t-zen-auditdetail.auditdate descending
by t-zen-auditdetail.audittime descending
INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER"
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME br-maint

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{{&core}commonmaint.i &path = "{&aud}{&srv}"}
    /* &extraparams = "'class'," */
                
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

lv-wid:sensitive = true.

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
  ENABLE br-maint 
      WITH FRAME a-frame-maint IN WINDOW window-maint.
  {&OPEN-BROWSERS-IN-QUERY-a-frame-maint}
  VIEW window-maint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAuditRecords window-maint 
PROCEDURE GetAuditRecords :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

empty temp-table t-{&table-name}.
empty temp-table t-zen-auditline.

{{&core}run.i &program   = "{&table-name}.p"
             &path      = "{&aud}{&srv}"
             &Appsrv    = "System"  
             &noper     = true
             &procedure = "open-querykey"
             &params    = "(cb-table,
                            lv-sourcekey,
                            01/01/0001,
                            12/12/9999,  
                            true,    
                            true, 
                            true,  
                            't',
                            OUTPUT TABLE t-{&table-name},
                            OUTPUT TABLE t-zen-auditline)"}
/*
def input param pv-table     as char no-undo.
def input param pv-searchkey as char no-undo.
def input param pv-fromdate  as date no-undo.
def input param pv-todate    as date no-undo.
def input param pv-adds      as log  no-undo.
def input param pv-updates   as log  no-undo.
def input param pv-deletes   as log  no-undo.
def input param pv-by        as  char no-undo.
def output param table for t-{&table-name}.
def output param table for t-zen-auditline.
*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-browse-off-end window-maint 
PROCEDURE local-browse-off-end :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-browse-off-home window-maint 
PROCEDURE local-browse-off-home :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-Export-Trigger window-maint 
PROCEDURE local-Export-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
exportbrowse(br-maint:handle in frame {&FRAME-NAME}).
 
return 'override'.
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

return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-openquery window-maint 
PROCEDURE local-openquery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&window-name}:hidden = false.
RUN GetAuditRecords.
{&OPEN-QUERY-br-maint}
apply 'entry' to br-maint in frame {&frame-name}.
return 'override'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh window-maint 
PROCEDURE Refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-sourcekey as char no-undo.
def input param pv-table     as char no-undo.

assign       
  lv-sourcekey = pv-sourcekey
  cb-table     = pv-table.
{&window-name}:title = 'Audit Detail For Table ' + caps(pv-table) + ' Internal ID:' + lv-sourcekey.

run openquery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

