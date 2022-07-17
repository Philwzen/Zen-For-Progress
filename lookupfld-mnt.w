&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-lookupfld NO-UNDO LIKE zen-lookupfld.



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
&glob title-text Lookup Maintenance
&glob KeepRefreshButton

&glob table-name zen-lookupfld
&glob unique-key {&table-name}tableid

def var lv-lkid as char no-undo.
/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
&glob NoQuery           /* no query button */
&glob NoHelp            /* no help button */
&glob NoPrint           /* no print button */

&Glob ImmediateQuery    /* force open query */

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
&Scoped-define INTERNAL-TABLES t-zen-lookupfld

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-lookupfld.FieldLabel ~
t-zen-lookupfld.fieldname t-zen-lookupfld.order 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-lookupfld NO-LOCK ~
    BY t-zen-lookupfld.order
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-lookupfld NO-LOCK ~
    BY t-zen-lookupfld.order.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-lookupfld
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-lookupfld


/* Definitions for FRAME a-frame-maint                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS t-zen-lookupfld.order 
&Scoped-define ENABLED-TABLES t-zen-lookupfld
&Scoped-define FIRST-ENABLED-TABLE t-zen-lookupfld
&Scoped-Define ENABLED-OBJECTS br-maint 
&Scoped-Define DISPLAYED-FIELDS t-zen-lookupfld.fieldname ~
t-zen-lookupfld.FieldFormat t-zen-lookupfld.FieldLabel ~
t-zen-lookupfld.InfoFormat t-zen-lookupfld.order t-zen-lookupfld.DescField ~
t-zen-lookupfld.KeyField t-zen-lookupfld.SearchField ~
t-zen-lookupfld.extentnum t-zen-lookupfld.sysrecord t-zen-lookupfld.IField ~
t-zen-lookupfld.LField 
&Scoped-define DISPLAYED-TABLES t-zen-lookupfld
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-lookupfld


/* Custom List Definitions                                              */
/* add-list,edit-list,List-3,List-4,List-5,List-6                       */
&Scoped-define add-list t-zen-lookupfld.fieldname ~
t-zen-lookupfld.FieldFormat t-zen-lookupfld.FieldLabel ~
t-zen-lookupfld.InfoFormat t-zen-lookupfld.order t-zen-lookupfld.DescField ~
t-zen-lookupfld.KeyField t-zen-lookupfld.SearchField ~
t-zen-lookupfld.extentnum t-zen-lookupfld.sysrecord t-zen-lookupfld.IField ~
t-zen-lookupfld.LField 
&Scoped-define edit-list t-zen-lookupfld.fieldname ~
t-zen-lookupfld.FieldFormat t-zen-lookupfld.FieldLabel ~
t-zen-lookupfld.InfoFormat t-zen-lookupfld.order t-zen-lookupfld.DescField ~
t-zen-lookupfld.KeyField t-zen-lookupfld.SearchField ~
t-zen-lookupfld.extentnum t-zen-lookupfld.sysrecord t-zen-lookupfld.IField ~
t-zen-lookupfld.LField 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-lookupfld SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-lookupfld.FieldLabel WIDTH 21
      t-zen-lookupfld.fieldname
      t-zen-lookupfld.order WIDTH 8.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 95 BY 10.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME a-frame-maint
     br-maint AT ROW 1.43 COL 11 HELP
          "Select the record to edit."
     t-zen-lookupfld.fieldname AT ROW 12.86 COL 24.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-lookupfld.FieldFormat AT ROW 13.86 COL 24.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-lookupfld.FieldLabel AT ROW 14.86 COL 24.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-lookupfld.InfoFormat AT ROW 15.91 COL 14.8
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-lookupfld.order AT ROW 12.86 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     t-zen-lookupfld.DescField AT ROW 13.86 COL 90
          VIEW-AS TOGGLE-BOX
          SIZE 18.8 BY 1
     t-zen-lookupfld.KeyField AT ROW 14.86 COL 90
          VIEW-AS TOGGLE-BOX
          SIZE 18.8 BY 1
     t-zen-lookupfld.SearchField AT ROW 15.86 COL 90
          VIEW-AS TOGGLE-BOX
          SIZE 18.8 BY 1
     t-zen-lookupfld.extentnum AT ROW 12.86 COL 126.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     t-zen-lookupfld.sysrecord AT ROW 13.86 COL 128.2 WIDGET-ID 2
          LABEL "Distribute"
          VIEW-AS TOGGLE-BOX
          SIZE 15.6 BY 1
     t-zen-lookupfld.IField AT ROW 14.86 COL 128.2
          VIEW-AS TOGGLE-BOX
          SIZE 15.6 BY 1
     t-zen-lookupfld.LField AT ROW 15.86 COL 128.2
          VIEW-AS TOGGLE-BOX
          SIZE 15.6 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148.6 BY 17.86
         TITLE "Lookup Field Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-lookupfld T "?" NO-UNDO schadm zen-lookupfld
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
         HEIGHT             = 17.95
         WIDTH              = 149.2
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
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB br-maint 1 a-frame-maint */
/* SETTINGS FOR TOGGLE-BOX t-zen-lookupfld.DescField IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-lookupfld.extentnum IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-lookupfld.FieldFormat IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-lookupfld.FieldLabel IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-lookupfld.fieldname IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-zen-lookupfld.IField IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-lookupfld.InfoFormat IN FRAME a-frame-maint
   NO-ENABLE ALIGN-L 1 2                                                */
/* SETTINGS FOR TOGGLE-BOX t-zen-lookupfld.KeyField IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-zen-lookupfld.LField IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-lookupfld.order IN FRAME a-frame-maint
   1 2                                                                  */
/* SETTINGS FOR TOGGLE-BOX t-zen-lookupfld.SearchField IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-zen-lookupfld.sysrecord IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
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
     _TblList          = "Temp-Tables.t-zen-lookupfld"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _OrdList          = "Temp-Tables.t-zen-lookupfld.order|yes"
     _FldNameList[1]   > Temp-Tables.t-zen-lookupfld.FieldLabel
"t-zen-lookupfld.FieldLabel" ? ? "character" ? ? ? ? ? ? no ? no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.t-zen-lookupfld.fieldname
     _FldNameList[3]   > Temp-Tables.t-zen-lookupfld.order
"t-zen-lookupfld.order" ? ? "decimal" ? ? ? ? ? ? no ? no no "8.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME br-maint

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */

ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/*main core logic*/
{{&core}commonmaint.i &path = "{&core}{&srv}"
                &extraparams = "lv-lkid,"} 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateExtraFields window-maint 
PROCEDURE CreateExtraFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 t-zen-lookupfld.lookupname = lv-lkid.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  IF AVAILABLE t-zen-lookupfld THEN 
    DISPLAY t-zen-lookupfld.fieldname t-zen-lookupfld.FieldFormat 
          t-zen-lookupfld.FieldLabel t-zen-lookupfld.InfoFormat 
          t-zen-lookupfld.order t-zen-lookupfld.DescField 
          t-zen-lookupfld.KeyField t-zen-lookupfld.SearchField 
          t-zen-lookupfld.extentnum t-zen-lookupfld.sysrecord 
          t-zen-lookupfld.IField t-zen-lookupfld.LField 
      WITH FRAME a-frame-maint.
  ENABLE br-maint t-zen-lookupfld.order 
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
  def input param pv-lkid as char no-undo.
  lv-lkid = pv-lkid.

  run openquery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen window-maint 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:   validate screen entries prior to save  
  Parameters:  <none>
  Notes:      return 'passed' if ok or string of failed widget-handle 
-------------------------------------------------------------*/
def var v-recid as recid no-undo. 
def buffer b-{&table-name} for t-{&table-name}.

if t-{&table-name}.keyfield:checked in frame {&frame-name} then do:
    if can-find(first b-{&table-name} where b-{&table-name}.{&table-name}tableid
                                      ne t-{&table-name}.{&table-name}tableid
                                  and b-{&table-name}.keyfield)
    then do:
    message 'Only one Key Field allowed'.
    RETURN STRING(t-{&Table-name}.keyfield:HANDLE in frame {&frame-name}).
    end.
end.

if t-{&table-name}.descfield:checked then do:
    if can-find(first b-{&table-name} where b-{&table-name}.{&table-name}tableid
                                      ne t-{&table-name}.{&table-name}tableid
                                  and b-{&table-name}.descfield)
  then do:
    message 'Only one Description Field allowed'.
    RETURN STRING(t-{&Table-name}.descfield:HANDLE in frame {&frame-name}).
  end.
end.

/* if t-{&table-name}.searchfield:checked then do:                                    */
/*     if int(t-{&table-name}.extentnum:screen-value) > 0 then do:                    */
/*         message 'Cannot Search on An Array Field'.                                 */
/*         RETURN STRING(t-{&Table-name}.searchfield:HANDLE in frame {&frame-name}).  */
/*     end.                                                                           */
/*     if can-find(first b-{&table-name} where b-{&table-name}.{&table-name}tableid   */
/*                                       ne t-{&table-name}.{&table-name}tableid      */
/*                                   and b-{&table-name}.searchfield)                 */
/*   then do:                                                                         */
/*     message 'Only one Search Field allowed'.                                       */
/*     RETURN STRING(t-{&Table-name}.searchfield:HANDLE in frame {&frame-name}).      */
/*   end.                                                                             */
/* end.                                                                               */
/* !@# AKH - 02/04/05 - Irritating messages - commented out for now         */
/* if not can-find(first b-{&table-name} where b-{&table-name}.searchfield) */
/* then do:                                                                 */
/*     message 'Must Have one Search Field' view-as alert-box warning.      */
/* end.                                                                     */
/* if not can-find(first b-{&table-name} where b-{&table-name}.keyfield)    */
/* then do:                                                                 */
/*     message 'Must Have one Key Field' view-as alert-box warning.         */
/* end.                                                                     */
/* if not can-find(first b-{&table-name} where b-{&table-name}.descfield)   */
/* then do:                                                                 */
/*     message 'Must Have one Description Field' view-as alert-box warning. */
/* end.                                                                     */




return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

