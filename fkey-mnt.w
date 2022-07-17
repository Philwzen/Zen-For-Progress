&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-Dfkey NO-UNDO LIKE Zen-Dfkey.



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
&glob title-text Function Key Maintainance
&glob table-name zen-dfkey
&glob unique-key    {&table-name}TableId
&glob KeepRefreshButton
&Glob ImmediateQuery
&glob noprint
/* ***************************  Definitions  ************************** */
{app-paths.i}

/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */

/* is user system manager? Allow sys man to update system record field */
def var lv-systemManager   as log    no-undo.

def var lv-user            as char  no-undo.

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
&Scoped-define INTERNAL-TABLES t-zen-dfkey

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-dfkey.ForUsers ~
t-zen-dfkey.In-Prog t-zen-dfkey.action t-zen-dfkey.fkey 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-dfkey NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-dfkey NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-dfkey
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-dfkey


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint 
&Scoped-Define DISPLAYED-FIELDS t-Zen-Dfkey.fkey t-Zen-Dfkey.sysrecord ~
t-Zen-Dfkey.action t-Zen-Dfkey.in-prog t-Zen-Dfkey.ForUsers 
&Scoped-define DISPLAYED-TABLES t-Zen-Dfkey
&Scoped-define FIRST-DISPLAYED-TABLE t-Zen-Dfkey


/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-Zen-Dfkey.fkey t-Zen-Dfkey.sysrecord ~
t-Zen-Dfkey.action t-Zen-Dfkey.in-prog t-Zen-Dfkey.ForUsers 
&Scoped-define Edit-List t-Zen-Dfkey.fkey t-Zen-Dfkey.sysrecord ~
t-Zen-Dfkey.action t-Zen-Dfkey.in-prog t-Zen-Dfkey.ForUsers 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-dfkey SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-dfkey.ForUsers FORMAT "X(50)":U WIDTH 26
      t-zen-dfkey.In-Prog COLUMN-LABEL "In Program" FORMAT "X(75)":U
            WIDTH 62.2
      t-zen-dfkey.action FORMAT "X(60)":U WIDTH 50
      t-zen-dfkey.fkey COLUMN-LABEL "Hot Key" WIDTH 16.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 160.8 BY 13.14 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     br-maint AT ROW 1.43 COL 14.2 HELP
          "Select the record to edit."
     t-Zen-Dfkey.fkey AT ROW 15.29 COL 27 COLON-ALIGNED
          LABEL "Hot Key"
          VIEW-AS FILL-IN 
          SIZE 43 BY 1
     t-Zen-Dfkey.sysrecord AT ROW 15.29 COL 79.2 WIDGET-ID 64
          LABEL "System Record"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY 1
     t-Zen-Dfkey.action AT ROW 16.29 COL 27 COLON-ALIGNED FORMAT "X(70)"
          VIEW-AS FILL-IN 
          SIZE 72.2 BY 1
     t-Zen-Dfkey.in-prog AT ROW 17.29 COL 27 COLON-ALIGNED
          LABEL "In Program" FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 72.2 BY 1.05
     t-Zen-Dfkey.ForUsers AT ROW 18.33 COL 27 COLON-ALIGNED WIDGET-ID 2 FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 72.2 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 178.2 BY 19.71
         TITLE "Hot Keys".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-Zen-Dfkey T "?" NO-UNDO schadm Zen-Dfkey
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
         HEIGHT             = 22.57
         WIDTH              = 178.4
         MAX-HEIGHT         = 34.33
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 34.33
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
         FONT               = 9
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
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint 1 frame-maint */
/* SETTINGS FOR FILL-IN t-Zen-Dfkey.action IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
ASSIGN 
       br-maint:ALLOW-COLUMN-SEARCHING IN FRAME frame-maint = TRUE
       br-maint:COLUMN-RESIZABLE IN FRAME frame-maint       = TRUE.

/* SETTINGS FOR FILL-IN t-Zen-Dfkey.fkey IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-Zen-Dfkey.ForUsers IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-Zen-Dfkey.in-prog IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT                                   */
/* SETTINGS FOR TOGGLE-BOX t-Zen-Dfkey.sysrecord IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-dfkey"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-zen-dfkey.ForUsers
"t-zen-dfkey.ForUsers" ? "X(50)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-dfkey.In-Prog
"t-zen-dfkey.In-Prog" "In Program" "X(75)" "character" ? ? ? ? ? ? no ? no no "62.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.t-zen-dfkey.action
"t-zen-dfkey.action" ? "X(60)" "character" ? ? ? ? ? ? no ? no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.t-zen-dfkey.fkey
"t-zen-dfkey.fkey" "Hot Key" ? "character" ? ? ? ? ? ? no ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/*******************
    DONT FORGET TO CHECK DIR LOCATION FOR BELOW INCLUDE !!!!!!!
********************/
{{&core}commonmaint.i &path = "{&core}{&srv}"}

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
      t-{&table-name}.sysrecord:visible in frame {&frame-name} = lv-systemManager .
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
  IF AVAILABLE t-Zen-Dfkey THEN 
    DISPLAY t-Zen-Dfkey.fkey t-Zen-Dfkey.sysrecord t-Zen-Dfkey.action 
          t-Zen-Dfkey.in-prog t-Zen-Dfkey.ForUsers 
      WITH FRAME frame-maint.
  ENABLE br-maint 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn window-maint 
PROCEDURE Local-ChildReturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
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
   /* show sys record field if user is system managaer */
   assign
      lv-user                   = getUserID()
      lv-systemManager          = systemManager(lv-user) /* in zenlibrary.p */
      t-{&table-name}.sysrecord:visible in frame {&frame-name} = lv-systemManager .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-print-trigger window-maint 
PROCEDURE local-print-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    run PrintScreen in this-procedure 
                    ("{&table-name}",
                     "fkey,In-Prog,action",
                     "",
                     "")
                     no-error.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen window-maint 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   def var v-recid as recid no-undo. 
   def buffer b-{&table-name} for t-{&table-name}.

   if lookup(input frame {&frame-name} t-{&table-name}.fkey,"F6") ne 0
   then do:
      message "F6 can't be used as a Hot Key (invokes next-frame)."
         view-as alert-box info buttons OK.
      return string(t-{&table-name}.fkey:handle).
   end.

/*     if can-find(first b-product                                                 */
/*                 where b-product.product-Code =                                  */
/*                                             t-acccentre.product-Code:screen-value */
/*                   and recid(b-product) ne recid(t-acccentre))                     */
/*     then do:                                                                    */
/*         message "Error Product Already Exists!"                                 */
/*         view-as alert-box error.                                                */
/*         return no-apply.                                                        */
/*     end.                                                                        */
/*                                                                                 */

   return 'passed'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

