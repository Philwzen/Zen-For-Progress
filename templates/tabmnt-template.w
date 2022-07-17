&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME window-maint
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
/* add temptable in uib add a field t-reid as recid to it.
set table-name to the tablename !!!
define query and browse on the temp table.
drop temptable field on to frames one - six.
put field in frame one in list-1 frame two in list-2 etc.
field should be displayed not enabled.
validation logic is in validatescreen .
see commonmaint.i for event points.
create a .p with same name as table.
set table-name in .p.
thats it.
*/
CREATE WIDGET-POOL.
{app-paths.i}
&glob tabs true
&glob title-text ???? Tabbed Maintenance
&glob table-name ??????
&glob unique-key {&table-name}tableid
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
&Glob NoImmediateQuery    /* force open query */
&glob NotFoundMessage   /* no record not found message */
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

/* Definitions for FRAME a-frame-maint                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint btn-orders 

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
DEFINE BUTTON btn-orders 
     LABEL "Orders" 
     SIZE 9 BY 1.14.


/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 91 BY 10.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME a-frame-maint
     br-maint AT ROW 1.43 COL 13 HELP
          "Select the record to edit."
     btn-orders AT ROW 17.43 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 107.8 BY 19.71.

DEFINE FRAME six
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13.8 ROW 13.95
         SIZE 87.2 BY 5.57.

DEFINE FRAME three
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13.8 ROW 13.95
         SIZE 87.2 BY 5.57.

DEFINE FRAME one
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13.6 ROW 14
         SIZE 88.6 BY 5.67.

DEFINE FRAME two
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13.8 ROW 13.95
         SIZE 87.2 BY 5.57.

DEFINE FRAME five
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13.8 ROW 13.95
         SIZE 87.2 BY 5.57.

DEFINE FRAME four
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13.8 ROW 13.95
         SIZE 87.2 BY 5.57.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Browse Maintenance"
         HEIGHT             = 19.81
         WIDTH              = 108
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW window-maint
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME five:FRAME = FRAME a-frame-maint:HANDLE
       FRAME four:FRAME = FRAME a-frame-maint:HANDLE
       FRAME one:FRAME = FRAME a-frame-maint:HANDLE
       FRAME six:FRAME = FRAME a-frame-maint:HANDLE
       FRAME three:FRAME = FRAME a-frame-maint:HANDLE
       FRAME two:FRAME = FRAME a-frame-maint:HANDLE.

/* SETTINGS FOR FRAME a-frame-maint
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint 1 a-frame-maint */
/* SETTINGS FOR FRAME five
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME five:HIDDEN           = TRUE
       FRAME five:PRIVATE-DATA     = 
                "5".

/* SETTINGS FOR FRAME four
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME four:HIDDEN           = TRUE
       FRAME four:PRIVATE-DATA     = 
                "4".

/* SETTINGS FOR FRAME one
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME one:HIDDEN           = TRUE
       FRAME one:PRIVATE-DATA     = 
                "1".

/* SETTINGS FOR FRAME six
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME six:HIDDEN           = TRUE
       FRAME six:PRIVATE-DATA     = 
                "6".

/* SETTINGS FOR FRAME three
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME three:HIDDEN           = TRUE
       FRAME three:PRIVATE-DATA     = 
                "3".

/* SETTINGS FOR FRAME two
   NOT-VISIBLE Custom                                                   */
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
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME five
/* Query rebuild information for FRAME five
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME five */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME four
/* Query rebuild information for FRAME four
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME four */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME one
/* Query rebuild information for FRAME one
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME one */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME six
/* Query rebuild information for FRAME six
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME six */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME three
/* Query rebuild information for FRAME three
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME three */
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
       ROW             = 12.57
       COLUMN          = 11
       HEIGHT          = 7.76
       WIDTH           = 96
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      CtrlFrame:MOVE-AFTER(br-maint:HANDLE IN FRAME a-frame-maint).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-orders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-orders window-maint
ON CHOOSE OF btn-orders IN FRAME a-frame-maint /* Orders */
DO:

  RunChild('{&sys}order-maint.w',this-procedure). 

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

{{&core}commonmaint.i &path = "{&sys}{&srv}"}
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

OCXFile = SEARCH( "tabmnt-template.wrx":U ).
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
ELSE MESSAGE "tabmnt-template.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  ENABLE btn-orders 
      WITH FRAME a-frame-maint IN WINDOW window-maint.
  {&OPEN-BROWSERS-IN-QUERY-a-frame-maint}
  {&OPEN-BROWSERS-IN-QUERY-five}
  {&OPEN-BROWSERS-IN-QUERY-four}
  {&OPEN-BROWSERS-IN-QUERY-six}
  {&OPEN-BROWSERS-IN-QUERY-three}
  {&OPEN-BROWSERS-IN-QUERY-two}
  {&OPEN-BROWSERS-IN-QUERY-one}
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
assign
    h-tab   = chctrlframe:tabstrip.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn window-maint 
PROCEDURE Local-ChildReturn :
/*------------------------------------------------------------------------------
  Purpose:  allow processing after a child screen exits.   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-from as char no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Update-Child-Procedures window-maint 
PROCEDURE Local-Update-Child-Procedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-to as handle no-undo.

case pv-to:private-data:
/*     when 'order-maint.w' then run refresh in  pv-to (t-customer.cust-num).  */
/*     when 'sr-maint.w'    then run refresh in  pv-to.                        */
end case.        
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
    
    run openquery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen window-maint 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
/*   def var v-recid as recid no-undo.                                                                 */
/*   def buffer b-{&table-name} for t-{&table-name}.                                                   */
/*                                                                                                     */
/*   IF lv-new THEN                                                                                    */
/*       IF CAN-FIND(FIRST b-{&Table-name} WHERE b-{&Table-name}.keyfield = t-{&Table-name}.keyfield)  */
/*       THEN DO:                                                                                      */
/*           MESSAGE msg (120,"","","","") VIEW-AS ALERT-BOX.                                          */
/*           RETURN STRING(t-{&Table-name}.keyfield:HANDLE in frame one).                              */
/*       END.                                                                                          */
  
  return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

