&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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

CREATE WIDGET-POOL.
{app-paths.i}
&glob title-text ????? Maintenance
&glob zenscreen true

&glob table-name ???? /* change this to be your tablename */
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
*/
&glob suppresswindow     /* no window creation */

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
/* add-list,edit-list,List-3,List-4,List-5,List-6                       */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-orders 
     LABEL "Orders" 
     SIZE 15 BY 1.14.


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
     btn-orders AT ROW 13.14 COL 76
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 106.4 BY 20.33 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Browse Maintenance"
         HEIGHT             = 20.67
         WIDTH              = 106.6
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
/* BROWSE-TAB br-maint 1 a-frame-maint */
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-orders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-orders window-maint
ON CHOOSE OF btn-orders IN FRAME a-frame-maint /* Orders */
DO:
  /* example of how to run a child program */
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
/*main core logic*/
{{&core}commonmaint.i &path = "{&sys}{&srv}"}
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
  ENABLE btn-orders 
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
/*  def var v-recid as recid no-undo. 
  def buffer b-{&table-name} for t-{&table-name}.

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

