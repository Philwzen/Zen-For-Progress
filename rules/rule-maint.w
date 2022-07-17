&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-rule NO-UNDO LIKE zen-rule.



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
&glob title-text    Rules Maintenance
&glob table-name    zen-rule
&glob Unique-key {&table-name}tableid 
&glob suppresswindow
/* &glob KeepRefreshButton */
&Glob ImmediateQuery



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
&Scoped-define INTERNAL-TABLES t-zen-rule

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-rule.program ~
t-zen-rule.description t-zen-rule.RuleName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-rule NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-rule NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-rule
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-rule


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint 
&Scoped-Define DISPLAYED-FIELDS t-zen-rule.RuleName t-zen-rule.ExecuteOrder ~
t-zen-rule.description t-zen-rule.program t-zen-rule.Procedure ~
t-zen-rule.active t-zen-rule.ContinueOnError 
&Scoped-define DISPLAYED-TABLES t-zen-rule
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-rule
&Scoped-Define DISPLAYED-OBJECTS cb-type cb-group cb-mode 

/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List cb-type cb-group t-zen-rule.RuleName ~
t-zen-rule.ExecuteOrder t-zen-rule.description cb-mode t-zen-rule.program ~
t-zen-rule.Procedure t-zen-rule.active t-zen-rule.ContinueOnError 
&Scoped-define Edit-List cb-type cb-group t-zen-rule.RuleName ~
t-zen-rule.ExecuteOrder t-zen-rule.description cb-mode t-zen-rule.program ~
t-zen-rule.Procedure t-zen-rule.active t-zen-rule.ContinueOnError 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cb-group AS CHARACTER FORMAT "X(256)":U 
     LABEL "Group" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 43.4 BY 1 NO-UNDO.

DEFINE VARIABLE cb-mode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Execute Mode" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cb-type AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 30.4 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-rule SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-rule.program
      t-zen-rule.description WIDTH 27.2
      t-zen-rule.RuleName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 93.4 BY 9.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     br-maint AT ROW 1.14 COL 11.2 HELP
          "Select the record to edit."
     cb-type AT ROW 10.76 COL 19 COLON-ALIGNED
     cb-group AT ROW 10.76 COL 58.8 COLON-ALIGNED
     t-zen-rule.RuleName AT ROW 12.1 COL 26.4 COLON-ALIGNED FORMAT "X(25)"
          VIEW-AS FILL-IN 
          SIZE 49 BY 1
     t-zen-rule.ExecuteOrder AT ROW 12.05 COL 96.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     t-zen-rule.description AT ROW 13.24 COL 26.4 COLON-ALIGNED FORMAT "X(55)"
          VIEW-AS FILL-IN 
          SIZE 71.2 BY 1
     cb-mode AT ROW 14.43 COL 26.4 COLON-ALIGNED
     t-zen-rule.program AT ROW 15.76 COL 26.2 COLON-ALIGNED
          LABEL "Program"
          VIEW-AS FILL-IN 
          SIZE 61.4 BY 1
     t-zen-rule.Procedure AT ROW 16.95 COL 26.2 COLON-ALIGNED
          LABEL "Procedure"
          VIEW-AS FILL-IN 
          SIZE 61.6 BY 1
     t-zen-rule.active AT ROW 18.1 COL 28.2
          LABEL "Active"
          VIEW-AS TOGGLE-BOX
          SIZE 11.4 BY .81
     t-zen-rule.ContinueOnError AT ROW 18.1 COL 40.8
          LABEL "Continue on Error"
          VIEW-AS TOGGLE-BOX
          SIZE 27.4 BY .81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.4 BY 19.19
         TITLE "Rule Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-rule T "?" NO-UNDO schadm zen-rule
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
         HEIGHT             = 20.67
         WIDTH              = 107.4
         MAX-HEIGHT         = 46.52
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.52
         VIRTUAL-WIDTH      = 256
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
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
/* SETTINGS FOR FRAME frame-maint
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB br-maint 1 frame-maint */
ASSIGN 
       FRAME frame-maint:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX t-zen-rule.active IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR COMBO-BOX cb-group IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR COMBO-BOX cb-mode IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR COMBO-BOX cb-type IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-zen-rule.ContinueOnError IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-zen-rule.description IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-zen-rule.ExecuteOrder IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-rule.Procedure IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-zen-rule.program IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-zen-rule.RuleName IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-rule"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   = Temp-Tables.t-zen-rule.program
     _FldNameList[2]   > Temp-Tables.t-zen-rule.description
"t-zen-rule.description" ? ? "character" ? ? ? ? ? ? no ? no no "27.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.t-zen-rule.RuleName
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME br-maint

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*******************
    DONT FORGET TO CHECK DIR LOCATION FOR BELOW INCLUDE !!!!!!!
    for path to appsrver .p 
********************/
{{&core}commonmaint.i &path = "{&core}{&srv}"}  
                        /* &extraparams = "???"} */

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
ASSIGN cb-group:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = setcombovalue(t-zen-rule.RuleGroup,cb-group:HANDLE)
       cb-type:SCREEN-VALUE = setcombovalue(t-zen-rule.Ruletype,cb-type:HANDLE)
       cb-mode:SCREEN-VALUE = setcombovalue(t-zen-rule.ExecuteMode,cb-mode:HANDLE).
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
  DISPLAY cb-type cb-group cb-mode 
      WITH FRAME frame-maint.
  IF AVAILABLE t-zen-rule THEN 
    DISPLAY t-zen-rule.RuleName t-zen-rule.ExecuteOrder t-zen-rule.description 
          t-zen-rule.program t-zen-rule.Procedure t-zen-rule.active 
          t-zen-rule.ContinueOnError 
      WITH FRAME frame-maint.
  ENABLE br-maint 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn window-maint 
PROCEDURE Local-ChildReturn :
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
      run pop-combos in this-procedure no-error.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pop-combos window-maint 
PROCEDURE pop-combos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-list as char no-undo.

ASSIGN 
    cb-type:PRIVATE-DATA IN frame {&FRAME-NAME} = classcodes("{&ruletype}",OUTPUT lv-list)
    cb-type:LIST-ITEMS = lv-list
    cb-group:PRIVATE-DATA IN frame {&FRAME-NAME} = classcodes("{&rulegroup}",OUTPUT lv-list)
    cb-group:LIST-ITEMS = lv-list
    cb-mode:PRIVATE-DATA IN frame {&FRAME-NAME} = classcodes("{&rulemode}",OUTPUT lv-list)
    cb-mode:LIST-ITEMS = lv-list.
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
    t-zen-rule.RuleGroup = getcombokey(cb-group:HANDLE IN FRAME {&FRAME-NAME}).
    t-zen-rule.Ruletype  = getcombokey(cb-type:HANDLE).
    t-zen-rule.EXECUTEMODE  = getcombokey(cb-MODE:HANDLE).
   return "passed".
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

