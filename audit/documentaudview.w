&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rexrept          PROGRESS
          schadm       PROGRESS
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
define temp-table t-actv NO-UNDO LIKE actv.
define temp-table t-zen-auditdetail NO-UNDO LIKE zen-auditdetail.
define temp-table t-zen-auditline NO-UNDO LIKE zen-auditline.



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
&glob tabs true
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
def var lv-actionsFourthColumn as handle     no-undo.

&glob NoNew             /* no new button */
&glob NoEdit            /* no edit button */
&glob NoSave            /* no save button */
&glob NoDelete          /* no delete button */
&glob NoExport          /* no export button */
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
&Scoped-define INTERNAL-TABLES t-zen-auditdetail t-zen-auditline t-actv

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-auditdetail.auditdate string(t-zen-auditdetail.audittime,"hh:mm am") t-zen-auditdetail.byname t-zen-auditdetail.auditaction if t-zen-auditdetail.auditaction ne "create" then t-zen-auditline.fieldname else " " if t-zen-auditline.datachar ne ? then t-zen-auditline.datachar else if t-zen-auditline.datadate ne ? then string(t-zen-auditline.datadate) else if t-zen-auditline.datadec ne ? then string(t-zen-auditline.datadec) else if t-zen-auditline.dataint ne ? then string(t-zen-auditline.dataint) else if t-zen-auditline.datalog ne ? then string(t-zen-auditline.datalog) else if t-zen-auditline.datadatetime ne ? then string(t-zen-auditline.datadatetime,"99/99/9999 HH:MM:SS.SSS ") else ""   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint   
&Scoped-define SELF-NAME br-maint
&scoped-define query-STRING-br-maint FOR EACH t-zen-auditdetail NO-LOCK, ~
             EACH t-zen-auditline OF t-zen-auditdetail  NO-LOCK by t-zen-auditdetail.auditdate descending by t-zen-auditdetail.audittime descending INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY {&SELF-NAME} FOR EACH t-zen-auditdetail NO-LOCK, ~
             EACH t-zen-auditline OF t-zen-auditdetail  NO-LOCK by t-zen-auditdetail.auditdate descending by t-zen-auditdetail.audittime descending INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-auditdetail t-zen-auditline
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-auditdetail
&Scoped-define SECOND-TABLE-IN-QUERY-br-maint t-zen-auditline


/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 if lookup(t-actv.actv-no,"WPFX2,WPFX3") ne 0 then ? else t-actv.dentered if lookup(t-actv.actv-no,"WPFX2,WPFX3") ne 0 then "" else string(integer(t-actv.tentered),"hh:mm am") if lookup(t-actv.actv-no,"WPFX2,WPFX3") ne 0 then "" else t-actv.logonid string( if t-actv.actv-no = "WPCR" then "created" else if t-actv.actv-no = "WPED" then "edited" else if t-actv.actv-no = "WPCM" then "completed" else if t-actv.actv-no = "WPFX" then "faxed" else if t-actv.actv-no = "WPFX2" then "job ID: " + t-actv.actv-clr else if t-actv.actv-no = "WPFX3" then "result:" else if t-actv.actv-no = "WPIM" then "imported" else if t-actv.actv-no = "WPEC" then "electronically signed/completed" else if t-actv.actv-no = "WPIC" then "imp/compl" else if t-actv.actv-no = "WPPI" then "printed" else "imp/edit") /* created or editing */ string( if t-actv.actv-no = "WPED" then trim(string(integer(t-actv.fperiod) / 60,">>>9.9")) + " minutes in Microsoft Word" else /* faxed */ if t-actv.actv-no = "WPFX" then "to " + trim(substring(t-actv.fyear,10,40)) else if t-actv.actv-no = "WPFX2" then "at " + trim(substring(t-actv.fyear,50)) else if t-actv.actv-no = "WPFX3" then t-actv.fperiod else "")   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&scoped-define query-STRING-BROWSE-2 FOR EACH t-actv NO-LOCK     BY t-actv.dentered descending     BY t-actv.tentered DESCENDING     by t-actv.actv-no  INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH t-actv NO-LOCK     BY t-actv.dentered descending     BY t-actv.tentered DESCENDING     by t-actv.actv-no  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 t-actv
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 t-actv


/* Definitions for FRAME one                                            */

/* Definitions for FRAME two                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
def var CtrlFrame AS WIDGET-HANDLE NO-UNDO.
def var chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query br-maint FOR 
      t-zen-auditdetail, 
      t-zen-auditline SCROLLING.

define query BROWSE-2 FOR 
      t-actv SCROLLING.
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
       then t-zen-auditline.fieldname else " " 
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
    WITH NO-ASSIGN NO-ROW-MARKERS SEPARATORS SIZE 177 BY 13.76
          ROW-HEIGHT-CHARS .86 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 window-maint _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      if lookup(t-actv.actv-no,"WPFX2,WPFX3") ne 0 then ?  else
      t-actv.dentered column-label "Date" format "99/99/99":U 
      width 14
   if lookup(t-actv.actv-no,"WPFX2,WPFX3") ne 0 then "" else 
      string(integer(t-actv.tentered),"hh:mm am") column-label "Time"
      width 14
   if lookup(t-actv.actv-no,"WPFX2,WPFX3") ne 0 then "" else
      t-actv.logonid column-label "User" format "x(15)":U 
      width 20
   string(
       if t-actv.actv-no = "WPCR"  then "created"   else
       if t-actv.actv-no = "WPED"  then "edited"    else
       if t-actv.actv-no = "WPCM"  then "completed" else
       if t-actv.actv-no = "WPFX"  then "faxed"     else
       if t-actv.actv-no = "WPFX2" then "job ID: " + t-actv.actv-clr         
                                                    else
       if t-actv.actv-no = "WPFX3" then "result:"   else
       if t-actv.actv-no = "WPIM"  then "imported"  else
       if t-actv.actv-no = "WPEC"  then "electronically signed/completed" else
       if t-actv.actv-no = "WPIC"  then "imp/compl" else
       if t-actv.actv-no = "WPPI"  then "printed"   else
       "imp/edit") column-label "Action" format "x(50)":U
       width 40
   /* created or editing */
   string(
      if t-actv.actv-no = "WPED"  then
         trim(string(integer(t-actv.fperiod) / 60,">>>9.9")) +
         " minutes in Microsoft Word"                else
         /* faxed */
      if t-actv.actv-no = "WPFX"  then 
         "to " + trim(substring(t-actv.fyear,10,40)) else 
      if t-actv.actv-no = "WPFX2" then
         "at " + trim(substring(t-actv.fyear,50))    else
      if t-actv.actv-no = "WPFX3" then
         t-actv.fperiod                              else
      "")
      column-label "Notes" format "x(120)":U 
      width 200
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 177 BY 13.48
          ROW-HEIGHT-CHARS .86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME a-frame-maint
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 193.2 BY 18
          WIDGET-ID 100.

DEFINE FRAME two
     BROWSE-2 AT ROW 1.05 COL 1.6 WIDGET-ID 300
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 11.6 ROW 3.52
         SIZE 178 BY 14 WIDGET-ID 100.

DEFINE FRAME one
     br-maint AT ROW 1 COL 2 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 11.2 ROW 3.57
         SIZE 178 BY 14 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-actv T "?" NO-UNDO rexrept actv
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
         HEIGHT             = 18.24
         WIDTH              = 193.4
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
/* REPARENT FRAME */
ASSIGN FRAME one:FRAME = FRAME a-frame-maint:HANDLE
       FRAME two:FRAME = FRAME a-frame-maint:HANDLE.

/* SETTINGS FOR FRAME a-frame-maint
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME one
   Custom                                                               */
/* BROWSE-TAB br-maint 1 one */
ASSIGN 
       FRAME one:PRIVATE-DATA     = 
                "1".

ASSIGN 
       br-maint:COLUMN-RESIZABLE IN FRAME one       = TRUE
       br-maint:COLUMN-MOVABLE IN FRAME one         = TRUE.

/* SETTINGS FOR FRAME two
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB BROWSE-2 1 two */
ASSIGN 
       FRAME two:HIDDEN           = TRUE
       FRAME two:PRIVATE-DATA     = 
                "2".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(window-maint)
THEN window-maint:HIDDEN = no.

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
      EACH t-zen-auditline OF t-zen-auditdetail  NO-LOCK
by t-zen-auditdetail.auditdate descending
by t-zen-auditdetail.audittime descending
INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER"
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH t-actv NO-LOCK
    BY t-actv.dentered descending
    BY t-actv.tentered DESCENDING
    by t-actv.actv-no  INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.t-actv.actv-dt|yes,Temp-Tables.t-actv.tentered|no"
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME one
/* Query rebuild information for FRAME one
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME one */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME two
/* Query rebuild information for FRAME two
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME two */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME a-frame-maint:HANDLE
       ROW             = 2.1
       COLUMN          = 9.8
       HEIGHT          = 15.81
       WIDTH           = 181
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      CtrlFrame:MOVE-BEFORE(FRAME two:HANDLE).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define FRAME-NAME two
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 window-maint
ON ROW-DISPLAY OF BROWSE-2 IN FRAME two
DO:
   /* var is set in local initialise to appropriate column 
      as we cannot directly reference a calculated column */
   lv-actionsFourthColumn:bgcolor =
      if t-actv.actv-no = "WPFX3" then 14 else ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME a-frame-maint
&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load window-maint  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
def var UIB_S    as log    NO-UNDO.
def var OCXFile  as char  NO-UNDO.

OCXFile = SEARCH( "documentaudview.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "documentaudview.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  VIEW FRAME a-frame-maint IN WINDOW window-maint.
  {&OPEN-BROWSERS-IN-QUERY-a-frame-maint}
  ENABLE BROWSE-2 
      WITH FRAME two IN WINDOW window-maint.
  {&OPEN-BROWSERS-IN-QUERY-two}
  ENABLE br-maint 
      WITH FRAME one IN WINDOW window-maint.
  {&OPEN-BROWSERS-IN-QUERY-one}
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getotherecords window-maint 
PROCEDURE getotherecords :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

empty temp-table t-actv.

{{&core}run.i &program   = "{&table-name}.p"
             &path      = "{&aud}{&srv}"
             &Appsrv    = "System"  
             &noper     = true
             &procedure = "getDocumentRecords"
             &params    = "(lv-sourcekey,
                            OUTPUT TABLE t-actv)"}
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
h-tab   = chctrlframe:tabstrip.

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
frame one:hidden = false.
{&OPEN-QUERY-br-maint}
br-maint:sensitive in frame one = true. 
apply 'entry' to br-maint.


   /* go get other records */
   run getotherecords.
   {&OPEN-QUERY-BROWSE-2}
frame one:move-to-top().


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
   cb-table     = pv-table
   lv-actionsFourthColumn = getcolumnhandle(
      browse-2:handle in frame two,
      "Notes").

run openquery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

