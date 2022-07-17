&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-log NO-UNDO LIKE zen-log.



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
&glob title-text Log Maintenance
&glob zenscreen true

&glob table-name zen-log
&glob unique-key {&table-name}tableid

/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
/*
&glob bug               /* turn on debug mesasges */
&glob tree              /* turn on procedure tree listing */
&glob NoButtons         /* no buttons displayed */
&glob NoDElete          /* no delete button */
&glob NoExport          /* no export button */
&glob NoQuery           /* no query button */
&glob NoExit            /* no exit button */
&glob NoPrint           /* no print button */
&glob NotFoundMessage   /* no record not found message */
*/
&glob NoImmediateQuery  /* do not openquery */

&glob NoChangedCheck    /* disable changed onleave check */
&glob NoNew             /* no new button */
&glob NoEdit            /* no edit button */
&glob NoSave            /* no save button */
&glob NoUndo            /* no undo button */
&glob NoHelp            /* no help button */
&glob NoExitCheck       /* disble exit question */
&glob NoAudit           /* no audit button */
&glob suppresswindow   /* no window creation */

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
&Scoped-define INTERNAL-TABLES t-zen-log

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-log.logtype t-zen-log.logdate ~
string(t-zen-log.LogTime,"hh:mm:ss") t-zen-log.LogUser t-zen-log.Program ~
t-zen-log.Action t-zen-log.LogMessage 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-log NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-log NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-log
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-log


/* Definitions for FRAME a-frame-maint                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb-type lv-from lv-to cb-by br-maint 
&Scoped-Define DISPLAYED-OBJECTS cb-type lv-from lv-to cb-by 

/* Custom List Definitions                                              */
/* add-list,edit-list,List-3,List-4,List-5,List-6                       */
&Scoped-define List-3 cb-type lv-from lv-to cb-by 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cb-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "By" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE cb-type AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","Activity","Appserver","ErrorMessage" 
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-from AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "From" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-to AS DATE FORMAT "99/99/9999":U INITIAL 12/31/3000 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-log SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-log.logtype FORMAT "x(12)":U
      t-zen-log.logdate
      string(t-zen-log.LogTime,"hh:mm:ss") COLUMN-LABEL "At" FORMAT "x(9)":U
      t-zen-log.LogUser FORMAT "x(10)":U
      t-zen-log.Program FORMAT "x(40)":U
      t-zen-log.Action
      t-zen-log.LogMessage WIDTH 72.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 181 BY 17.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME a-frame-maint
     cb-type AT ROW 1.1 COL 17 COLON-ALIGNED WIDGET-ID 2
     lv-from AT ROW 1.1 COL 57 COLON-ALIGNED WIDGET-ID 4
     lv-to AT ROW 1.1 COL 80 COLON-ALIGNED WIDGET-ID 8
     cb-by AT ROW 1.1 COL 103 COLON-ALIGNED WIDGET-ID 6
     br-maint AT ROW 2.24 COL 11 HELP
          "Select the record to edit."
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 192.6 BY 18.81 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-log T "?" NO-UNDO schadm zen-log
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Browse Maintenance"
         HEIGHT             = 27.19
         WIDTH              = 195.4
         MAX-HEIGHT         = 45.33
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.33
         VIRTUAL-WIDTH      = 256
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME a-frame-maint
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint cb-by a-frame-maint */
ASSIGN 
       br-maint:ALLOW-COLUMN-SEARCHING IN FRAME a-frame-maint = TRUE
       br-maint:COLUMN-RESIZABLE IN FRAME a-frame-maint       = TRUE
       br-maint:COLUMN-MOVABLE IN FRAME a-frame-maint         = TRUE.

ASSIGN 
       t-zen-log.logtype:AUTO-RESIZE IN BROWSE br-maint = TRUE
       t-zen-log.logdate:AUTO-RESIZE IN BROWSE br-maint = TRUE
       t-zen-log.LogUser:AUTO-RESIZE IN BROWSE br-maint = TRUE
       t-zen-log.Program:AUTO-RESIZE IN BROWSE br-maint = TRUE
       t-zen-log.Action:AUTO-RESIZE IN BROWSE br-maint = TRUE
       t-zen-log.LogMessage:AUTO-RESIZE IN BROWSE br-maint = TRUE.

/* SETTINGS FOR COMBO-BOX cb-by IN FRAME a-frame-maint
   3                                                                    */
/* SETTINGS FOR COMBO-BOX cb-type IN FRAME a-frame-maint
   3                                                                    */
/* SETTINGS FOR FILL-IN lv-from IN FRAME a-frame-maint
   3                                                                    */
/* SETTINGS FOR FILL-IN lv-to IN FRAME a-frame-maint
   3                                                                    */
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
     _TblList          = "Temp-Tables.t-zen-log"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-zen-log.logtype
"t-zen-log.logtype" ? "x(12)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-log.logdate
"t-zen-log.logdate" ? ? "date" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"string(t-zen-log.LogTime,""hh:mm:ss"")" "At" "x(9)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.t-zen-log.LogUser
"t-zen-log.LogUser" ? "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.t-zen-log.Program
"t-zen-log.Program" ? "x(40)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.t-zen-log.Action
"t-zen-log.Action" ? ? "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.t-zen-log.LogMessage
"t-zen-log.LogMessage" ? ? "character" ? ? ? ? ? ? no ? no no "72.2" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME br-maint

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.*/

ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/*main core logic*/
{{&core}commonmaint.i &path = "{&core}{&srv}"
                      &extraparams = "getcombokey(cb-type:handle in frame {&frame-name}),lv-from,lv-to,cb-by,"}
/*                  */
/*                &extratables = ",???"  */
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {{&core}sec-chk.i} /*program security check*/
  RUN enable_UI.     
  {{&core}wid-chk.i} /*widget level security check*/
  {{&core}focus.i}   /*find first enabled widget*/
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-validate window-maint 
PROCEDURE delete-validate :
/*------------------------------------------------------------------------------
  Purpose:   return 'passed' if ok to delete record  
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
  HIDE FRAME a-frame-maint.
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
  DISPLAY cb-type lv-from lv-to cb-by 
      WITH FRAME a-frame-maint.
  ENABLE cb-type lv-from lv-to cb-by br-maint 
      WITH FRAME a-frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-a-frame-maint}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-after-openQuery window-maint 
PROCEDURE local-after-openQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
setsensitive(true,'inc','*',frame {&frame-name}:handle).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn window-maint 
PROCEDURE Local-ChildReturn :
/*------------------------------------------------------------------------------
  Purpose:  allow processing after a child screen exits.   
  Parameters:  <none>
  Notes:   pv-from child which has exited    
------------------------------------------------------------------------------*/
def input param pv-from as char no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-trigger window-maint 
PROCEDURE local-delete-trigger :
def var lv-ok as log no-undo.

message 'This will Delete All matching records' skip
        'Continue?'
        view-as alert-box info 
        buttons yes-no 
        title 'Confirm Delete'
        update lv-ok .
        
if lv-ok then do:        
 {{&core}run.i &program   = "zen-log.p"
              &path      = "{&core}{&srv}"
              &Appsrv    = "System"
              &procedure = "dellog"
              &params    = "(getcombokey(cb-type:handle in frame {&frame-name}),lv-from,lv-to,cb-by)"}
  run openquery.
end.

return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise window-maint 
PROCEDURE local-initialise :
def var lv-list as char no-undo.

BuildCombo(cb-by:handle in frame {&frame-name},
           'usedb-zen-duser',
            'duser',
            'duser',
            '',
            '',
            no,
            yes).

ASSIGN 
    cb-type:PRIVATE-DATA IN frame {&FRAME-NAME} = '*{&combodelim}' + classcodes("logtype",OUTPUT lv-list)
    cb-type:LIST-ITEMS = 'All{&combodelim}' + lv-list
    cb-type:screen-value in frame {&frame-name} = entry(1,cb-type:LIST-ITEMS,'{&combodelim}')
    cb-by:screen-value in frame {&frame-name} = getsysvar('{&clv}user').
    
            
    
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
assign frame {&frame-name} {&list-3}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-query-trigger window-maint 
PROCEDURE local-query-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 run OpenQuery in this-procedure no-error.

return 'override'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-update-child-procedures window-maint 
PROCEDURE Local-update-child-procedures :
/*------------------------------------------------------------------------------
  Purpose:  refresh any child procedures
  Parameters:  pv-to handle of child requesting refresh
  Notes:       called via published event in child
------------------------------------------------------------------------------*/
def input param pv-to as handle no-undo.

case pv-to:private-data:
/*     when 'order-maint.w' then run refresh in  pv-to (t-{&table-name.cust-num).  */
/*     when 'sr-maint.w'    then run refresh in  pv-to.                            */
end case.        


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh window-maint 
PROCEDURE Refresh :
/*------------------------------------------------------------------------------
  Purpose:   refresh the screen  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  run openquery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen window-maint 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:   validate screen entries prior to save  
  Parameters:  <none>
  Notes:      retrnu 'passed' if ok or string of failed widget-handle 
-------------------------------------------------------------*/
  def var v-recid as recid no-undo. 
  def buffer b-{&table-name} for t-{&table-name}.
/*
  IF lv-new THEN
      IF CAN-FIND(FIRST b-{&Table-name} WHERE b-{&Table-name}.keyfield = t-{&Table-name}.keyfield) 
      THEN DO:
          MESSAGE msg (120,"","","","") VIEW-AS ALERT-BOX.
          RETURN STRING(t-{&Table-name}.keyfield:HANDLE in frame one).
      END.
  */

  return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

