&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-Printer NO-UNDO LIKE zen-Printer.



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
&glob title-text Printer Maintenance
&glob zenscreen true
&Glob ImmediateQuery

&glob table-name zen-printer
&glob unique-key {&table-name}tableid

/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
/*
&glob bug               /* turn on debug mesasges */
&glob tree              /* turn on procedure tree listing */
&glob NoChangedCheck    /* disable changed onleave check */
&glob NoExitCheck       /* disble exit question */
&glob NoButtons         /* no buttons displayed */
&glob NoNew             /* no new button */
&glob NoEdit            /* no edit button */
&glob NoSave            /* no save button */
&glob NoDElete          /* no delete button */
&glob NoExport          /* no export button */
&glob NoUndo            /* no undo button */
&glob NoQuery           /* no query button */
&glob NoAudit           /* no audit button */
&glob NoExit            /* no exit button */
&glob NoHelp            /* no help button */
&glob NoPrint           /* no print button */
&glob NoImmediateQuery  /* do not openquery */
&Glob ImmediateQuery    /* force open query */
&glob NotFoundMessage   /* no record not found message */
&glob suppresswinfdow   /* no window creation */
*/

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
&Scoped-define INTERNAL-TABLES t-zen-Printer

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-Printer.DeviceName ~
t-zen-Printer.PhysicalType 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-Printer NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-Printer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-Printer
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-Printer


/* Definitions for FRAME a-frame-maint                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint btn-Forms btn-Codes 
&Scoped-Define DISPLAYED-FIELDS t-zen-Printer.DeviceName ~
t-zen-Printer.Description t-zen-Printer.LocalPrinter t-zen-Printer.RESETBEG ~
t-zen-Printer.RESETEND 
&Scoped-define DISPLAYED-TABLES t-zen-Printer
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-Printer
&Scoped-Define DISPLAYED-OBJECTS cb-type cb-os cb-que cb-reset 

/* Custom List Definitions                                              */
/* add-list,edit-list,List-3,List-4,List-5,List-6                       */
&Scoped-define add-list t-zen-Printer.DeviceName t-zen-Printer.Description ~
cb-type t-zen-Printer.LocalPrinter cb-os cb-que cb-reset ~
t-zen-Printer.RESETBEG t-zen-Printer.RESETEND 
&Scoped-define edit-list t-zen-Printer.Description cb-type ~
t-zen-Printer.LocalPrinter cb-os cb-que cb-reset t-zen-Printer.RESETBEG ~
t-zen-Printer.RESETEND 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-Codes 
     LABEL "Codes" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-Forms 
     LABEL "Forms" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cb-os AS CHARACTER FORMAT "X(256)":U 
     LABEL "Os" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE cb-que AS CHARACTER FORMAT "X(256)":U 
     LABEL "Que" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE cb-reset AS CHARACTER FORMAT "X(256)":U 
     LABEL "Reset Function" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE cb-type AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 38 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-Printer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-Printer.DeviceName COLUMN-LABEL "Name" WIDTH 37.2
      t-zen-Printer.PhysicalType WIDTH 24.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.2 BY 10.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME a-frame-maint
     br-maint AT ROW 1.1 COL 13 HELP
          "Select the record to edit."
     btn-Forms AT ROW 1.48 COL 91
     btn-Codes AT ROW 3.86 COL 91
     t-zen-Printer.DeviceName AT ROW 11.76 COL 18 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     t-zen-Printer.Description AT ROW 11.76 COL 59 NO-LABEL WIDGET-ID 10
          VIEW-AS EDITOR
          SIZE 48 BY 3.29
     cb-type AT ROW 12.95 COL 18 COLON-ALIGNED
     t-zen-Printer.LocalPrinter AT ROW 14.14 COL 20 WIDGET-ID 8
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .81
     cb-os AT ROW 15.05 COL 18 COLON-ALIGNED
     cb-que AT ROW 16.24 COL 18 COLON-ALIGNED WIDGET-ID 2
     cb-reset AT ROW 17.43 COL 18 COLON-ALIGNED
     t-zen-Printer.RESETBEG AT ROW 17.57 COL 71
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .81
     t-zen-Printer.RESETEND AT ROW 17.57 COL 88.2
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 107.2 BY 18.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-Printer T "?" NO-UNDO dive zen-Printer
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
         HEIGHT             = 20.48
         WIDTH              = 107.2
         MAX-HEIGHT         = 45.33
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.33
         VIRTUAL-WIDTH      = 256
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
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
/* BROWSE-TAB br-maint 1 a-frame-maint */
/* SETTINGS FOR COMBO-BOX cb-os IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR COMBO-BOX cb-que IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR COMBO-BOX cb-reset IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR COMBO-BOX cb-type IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR EDITOR t-zen-Printer.Description IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-Printer.DeviceName IN FRAME a-frame-maint
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX t-zen-Printer.LocalPrinter IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-zen-Printer.RESETBEG IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-zen-Printer.RESETEND IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
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
     _TblList          = "Temp-Tables.t-zen-Printer"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-zen-Printer.DeviceName
"t-zen-Printer.DeviceName" "Name" ? "character" ? ? ? ? ? ? no ? no no "37.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-Printer.PhysicalType
"t-zen-Printer.PhysicalType" ? ? "character" ? ? ? ? ? ? no ? no no "24.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-Codes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Codes window-maint
ON CHOOSE OF btn-Codes IN FRAME a-frame-maint /* Codes */
DO:
  /* example of how to run a child program */
  RunChild('{&core}printercode-mnt.w',this-procedure). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Forms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Forms window-maint
ON CHOOSE OF btn-Forms IN FRAME a-frame-maint /* Forms */
DO:
  /* example of how to run a child program */
  RunChild('{&core}printerform-mnt.w',this-procedure). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-type window-maint
ON VALUE-CHANGED OF cb-type IN FRAME a-frame-maint /* Type */
DO:
  
def var lv-where as char no-undo.
lv-where = 'where PhysicalType = "' + cb-type:screen-value  + '"'.

buildcombo(cb-reset:handle in frame {&frame-name},
           'zen-printercode', 
           'zen-printercodetableid',
           'CodeName',
           lv-where,
           '',
           no,
           no).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */

ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/*main core logic*/
{{&core}commonmaint.i &path = "{&core}{&srv}"}
/*                &extraparams = "???,"  */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disp-wids window-maint 
PROCEDURE disp-wids :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign
    cb-type:screen-value in frame {&frame-name} = t-{&table-name}.PhysicalType
    cb-os:screen-value = t-{&table-name}.op-sys
    cb-que:screen-value = t-{&table-name}.os-queue.

setcombovalue(string(t-{&table-name}.ResetFunc),cb-reset:handle).

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
  DISPLAY cb-type cb-os cb-que cb-reset 
      WITH FRAME a-frame-maint.
  IF AVAILABLE t-zen-Printer THEN 
    DISPLAY t-zen-Printer.DeviceName t-zen-Printer.Description 
          t-zen-Printer.LocalPrinter t-zen-Printer.RESETBEG 
          t-zen-Printer.RESETEND 
      WITH FRAME a-frame-maint.
  ENABLE br-maint btn-Forms btn-Codes 
      WITH FRAME a-frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-a-frame-maint}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise window-maint 
PROCEDURE local-initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-keys as char no-undo.
def var lv-list as char no-undo.

lv-keys = classcodes('{&PrinterTypes}',output lv-list).
assign cb-type:list-items in frame {&frame-name} = lv-list
       cb-type:private-data = lv-keys.

lv-keys = classcodes('{&Opsystype}',output lv-list).
assign cb-os:list-items in frame {&frame-name} = lv-list
       cb-os:private-data = lv-keys.

  cb-que:list-items in frame {&frame-name} = 
      'Screen{&combodelim}Remote{&combodelim}Email{&combodelim}Fax{&combodelim}' +
      replace(session:get-printers(),',','{&combodelim}').

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
     when '{&core}printerform-mnt.w' 
        then run refresh in  pv-to (t-{&table-name}.{&unique-key}). 
     when '{&core}printercode-mnt.w'
        then run refresh in pv-to (t-zen-Printer.PhysicalType).
end case.        


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-save window-maint 
PROCEDURE pre-save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign
      t-{&table-name}.PhysicalType = cb-type
      t-{&table-name}.op-sys       = cb-os
      t-{&table-name}.os-queue     = cb-que.

t-{&table-name}.ResetFunc = dec(getcombokey(cb-reset:handle in frame {&frame-name})).
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

