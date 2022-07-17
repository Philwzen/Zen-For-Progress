&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-auditfield NO-UNDO LIKE zen-auditfield.



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
&glob title-text    Audit Fields
&glob table-name    zen-auditfield
&glob Unique-key {&table-name}tableid 
&glob KeepRefreshButton
&glob NoPrint       /* no print button */ 
Def Var lv-id As char No-undo.
/* ***************************  Definitions  ************************** */
{app-paths.i}

/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */

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
&Scoped-define INTERNAL-TABLES t-zen-auditfield

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-auditfield.Active ~
t-zen-auditfield.datatype t-zen-auditfield.fieldname 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-auditfield NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-auditfield NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-auditfield
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-auditfield


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint btn-pop 
&Scoped-Define DISPLAYED-FIELDS t-zen-auditfield.fieldname ~
t-zen-auditfield.datatype t-zen-auditfield.relatedfunction ~
t-zen-auditfield.Active 
&Scoped-define DISPLAYED-TABLES t-zen-auditfield
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-auditfield


/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-zen-auditfield.fieldname ~
t-zen-auditfield.datatype t-zen-auditfield.relatedfunction ~
t-zen-auditfield.Active 
&Scoped-define Edit-List t-zen-auditfield.fieldname ~
t-zen-auditfield.datatype t-zen-auditfield.relatedfunction ~
t-zen-auditfield.Active 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-pop 
     LABEL "Populate" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-auditfield SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-auditfield.Active WIDTH 8
      t-zen-auditfield.datatype FORMAT "x(10)":U WIDTH 17.4
      t-zen-auditfield.fieldname
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 74 BY 13.1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     br-maint AT ROW 1.48 COL 11 HELP
          "Select the record to edit."
     t-zen-auditfield.fieldname AT ROW 14.81 COL 33 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     t-zen-auditfield.datatype AT ROW 15.81 COL 33 COLON-ALIGNED WIDGET-ID 6 FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     t-zen-auditfield.relatedfunction AT ROW 16.81 COL 33 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     t-zen-auditfield.Active AT ROW 17.81 COL 35.2 WIDGET-ID 4
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY 1
     btn-pop AT ROW 18 COL 55.6 WIDGET-ID 8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.4 BY 19.62
         TITLE "Audit Fields Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-auditfield T "?" NO-UNDO schadm zen-auditfield
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
         COLUMN             = 78.8
         ROW                = 7.38
         HEIGHT             = 28.91
         WIDTH              = 88.8
         MAX-HEIGHT         = 46.52
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.52
         VIRTUAL-WIDTH      = 256
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
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint 1 frame-maint */
/* SETTINGS FOR TOGGLE-BOX t-zen-auditfield.Active IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-auditfield.datatype IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-zen-auditfield.fieldname IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-auditfield.relatedfunction IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-auditfield"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-zen-auditfield.Active
"t-zen-auditfield.Active" ? ? "logical" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-auditfield.datatype
"t-zen-auditfield.datatype" ? "x(10)" "character" ? ? ? ? ? ? no ? no no "17.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.t-zen-auditfield.fieldname
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

&Scoped-define SELF-NAME btn-pop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-pop window-maint
ON CHOOSE OF btn-pop IN FRAME frame-maint /* Populate */
DO:
    run autopopulate.
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
{{&core}commonmaint.i &path = "{&aud}{&srv}"  
                     &extraparams = "lv-id,"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoPopulate window-maint 
PROCEDURE AutoPopulate :
{{&core}run.i 
      &program   = "{&table-name}.p"
      &path      = "{&aud}{&srv}"
      &Appsrv    = "System"
      &procedure = "Autopopulate"
      &params    = "(lv-id)"}
      
  Run refresh (lv-id).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disp-wids window-maint 
PROCEDURE disp-wids :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
  IF AVAILABLE t-zen-auditfield THEN 
    DISPLAY t-zen-auditfield.fieldname t-zen-auditfield.datatype 
          t-zen-auditfield.relatedfunction t-zen-auditfield.Active 
      WITH FRAME frame-maint.
  ENABLE br-maint btn-pop 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn window-maint 
PROCEDURE Local-ChildReturn :
def input param pv-from as char no-undo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialise window-maint 
PROCEDURE Local-Initialise :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Update-Child-Procedures window-maint 
PROCEDURE Local-Update-Child-Procedures :
def input param pv-to as handle no-undo.

case pv-to:private-data:
/*     when 'order-maint.w' then run refresh in  pv-to (t-customer.cust-num).  */
/*     when 'sr-maint.w'    then run refresh in  pv-to.                        */
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
      t-zen-auditfield.tablename = lv-id.
   return "passed".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh window-maint 
PROCEDURE refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Def Input Param pv-id As char No-undo.
lv-id = pv-id.
Run openquery.
btn-pop:sensitive in frame {&frame-name} = num-results("{&browse-name}") = 0. 
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

   /* user must enter a field type */
   if t-zen-auditfield.datatype:screen-value in frame {&frame-name} = "" then do:
      message "Enter a field type."
         view-as alert-box info buttons OK.
      return string(t-zen-auditfield.datatype:handle in frame {&frame-name}).
   end.
 

   return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

