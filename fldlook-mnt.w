&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          centrec          PROGRESS
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-fldlook NO-UNDO LIKE zen-fldlook.



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

&glob title-text Lookup Maint
&glob table-name zen-fldlook
&glob unique-key    {&table-name}TableId
/* &glob KeepRefreshButton */
&glob nohelp
&glob noprint
&glob nocontlog
&glob noaction
&glob nonotes
&Glob ImmediateQuery
/* ***************************  Definitions  ************************** */
{app-paths.i}

/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
def buffer  b-t-{&table-name} for t-{&table-name}.

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
&Scoped-define INTERNAL-TABLES t-Zen-fldlook

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-Zen-fldlook.LookupName ~
t-Zen-fldlook.tablename 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-Zen-fldlook NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-Zen-fldlook NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-Zen-fldlook
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-Zen-fldlook


/* Definitions for FRAME a-frame-maint                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint btn-fields 
&Scoped-Define DISPLAYED-FIELDS t-zen-fldlook.lookupname ~
t-zen-fldlook.tablename t-zen-fldlook.window-title ~
t-zen-fldlook.whereclause t-zen-fldlook.wherefield t-zen-fldlook.byclause ~
t-zen-fldlook.look-pgm t-zen-fldlook.prog-path t-zen-fldlook.srv-path ~
t-zen-fldlook.srv-prog t-zen-fldlook.srv-proc t-zen-fldlook.infopgm ~
t-zen-fldlook.infopath t-zen-fldlook.immediatequery t-zen-fldlook.help-ref ~
t-zen-fldlook.AltLookupName t-zen-fldlook.AltLookupLabel ~
t-zen-fldlook.help-file t-zen-fldlook.extra-details 
&Scoped-define DISPLAYED-TABLES t-zen-fldlook
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-fldlook
&Scoped-Define DISPLAYED-OBJECTS lv-fromlookup 

/* Custom List Definitions                                              */
/* add-list,edit-list,List-3,List-4,List-5,List-6                       */
&Scoped-define add-list t-zen-fldlook.lookupname t-zen-fldlook.tablename ~
t-zen-fldlook.window-title t-zen-fldlook.whereclause ~
t-zen-fldlook.wherefield t-zen-fldlook.byclause t-zen-fldlook.look-pgm ~
t-zen-fldlook.prog-path t-zen-fldlook.srv-path t-zen-fldlook.srv-prog ~
t-zen-fldlook.srv-proc t-zen-fldlook.infopgm t-zen-fldlook.infopath ~
t-zen-fldlook.immediatequery t-zen-fldlook.help-ref ~
t-zen-fldlook.AltLookupName t-zen-fldlook.AltLookupLabel ~
t-zen-fldlook.help-file t-zen-fldlook.extra-details 
&Scoped-define edit-list t-zen-fldlook.tablename t-zen-fldlook.window-title ~
t-zen-fldlook.whereclause t-zen-fldlook.wherefield t-zen-fldlook.byclause ~
t-zen-fldlook.look-pgm t-zen-fldlook.prog-path t-zen-fldlook.srv-path ~
t-zen-fldlook.srv-prog t-zen-fldlook.srv-proc t-zen-fldlook.infopgm ~
t-zen-fldlook.infopath t-zen-fldlook.immediatequery t-zen-fldlook.help-ref ~
t-zen-fldlook.AltLookupName t-zen-fldlook.AltLookupLabel ~
t-zen-fldlook.help-file t-zen-fldlook.extra-details 
&Scoped-define List-3 t-zen-fldlook.help-ref 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-copy 
     LABEL "Copy" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-fields 
     LABEL "Fields" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-fromlookup AS CHARACTER FORMAT "X(256)":U 
     LABEL "Copy From" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-Zen-fldlook SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-Zen-fldlook.LookupName COLUMN-LABEL "Lookup" FORMAT "X(30)":U
            WIDTH 37.2
      t-Zen-fldlook.tablename COLUMN-LABEL "Table" FORMAT "X(20)":U
            WIDTH 34.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 78 BY 10.62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME a-frame-maint
     br-maint AT ROW 1.43 COL 13 HELP
          "Select the record to edit."
     btn-copy AT ROW 1.48 COL 139 WIDGET-ID 2
     lv-fromlookup AT ROW 1.48 COL 104 COLON-ALIGNED WIDGET-ID 4
     t-zen-fldlook.lookupname AT ROW 12.19 COL 26 COLON-ALIGNED FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 69 BY 1
     btn-fields AT ROW 12.19 COL 98.8
     t-zen-fldlook.tablename AT ROW 13.19 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 69 BY 1
     t-zen-fldlook.window-title AT ROW 14.24 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 69 BY 1
     t-zen-fldlook.whereclause AT ROW 15.29 COL 28 NO-LABEL
          VIEW-AS EDITOR
          SIZE 69 BY 2.38
     t-zen-fldlook.wherefield AT ROW 17.67 COL 26 COLON-ALIGNED
          LABEL "Where Values" FORMAT "x(255)"
          VIEW-AS FILL-IN 
          SIZE 104 BY 1
     t-zen-fldlook.byclause AT ROW 18.71 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 68.6 BY 1
     t-zen-fldlook.look-pgm AT ROW 2.91 COL 117.2 COLON-ALIGNED
          LABEL "Lookup Program"
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     t-zen-fldlook.prog-path AT ROW 4.1 COL 117.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     t-zen-fldlook.srv-path AT ROW 5.29 COL 117.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     t-zen-fldlook.srv-prog AT ROW 6.48 COL 117.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     t-zen-fldlook.srv-proc AT ROW 7.62 COL 117.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     t-zen-fldlook.infopgm AT ROW 8.86 COL 117 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     t-zen-fldlook.infopath AT ROW 10.05 COL 117 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     t-zen-fldlook.immediatequery AT ROW 11.24 COL 131
          VIEW-AS TOGGLE-BOX
          SIZE 30 BY .81
     t-zen-fldlook.help-ref AT ROW 12.43 COL 129 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     t-zen-fldlook.AltLookupName AT ROW 14.81 COL 129 COLON-ALIGNED FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     t-zen-fldlook.AltLookupLabel AT ROW 16 COL 129 COLON-ALIGNED FORMAT "x(255)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     t-zen-fldlook.help-file AT ROW 17.19 COL 159 COLON-ALIGNED HELP
          "Help file name if using widget level help"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1 TOOLTIP "This field was originally called 'help file'."
     t-zen-fldlook.extra-details AT ROW 13.62 COL 129 COLON-ALIGNED WIDGET-ID 6 FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1 TOOLTIP "set to 'useinitval' to use initial value in search"
     "Where:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 15.86 COL 18.6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 193 BY 20.19
         TITLE "Field Lookup Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-fldlook T "?" NO-UNDO centrec zen-fldlook
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
         HEIGHT             = 29.19
         WIDTH              = 199.4
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
ASSIGN 
       FRAME a-frame-maint:PRIVATE-DATA     = 
                "3".

/* SETTINGS FOR FILL-IN t-zen-fldlook.AltLookupLabel IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-zen-fldlook.AltLookupName IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR BUTTON btn-copy IN FRAME a-frame-maint
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN t-zen-fldlook.byclause IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-fldlook.extra-details IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-zen-fldlook.help-file IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-HELP                                               */
/* SETTINGS FOR FILL-IN t-zen-fldlook.help-ref IN FRAME a-frame-maint
   NO-ENABLE 1 2 3                                                      */
/* SETTINGS FOR TOGGLE-BOX t-zen-fldlook.immediatequery IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-fldlook.infopath IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-fldlook.infopgm IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-fldlook.look-pgm IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-zen-fldlook.lookupname IN FRAME a-frame-maint
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN lv-fromlookup IN FRAME a-frame-maint
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN t-zen-fldlook.prog-path IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-fldlook.srv-path IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-fldlook.srv-proc IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-fldlook.srv-prog IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-fldlook.tablename IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR EDITOR t-zen-fldlook.whereclause IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-fldlook.wherefield IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT                                   */
/* SETTINGS FOR FILL-IN t-zen-fldlook.window-title IN FRAME a-frame-maint
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
     _TblList          = "Temp-Tables.t-Zen-fldlook"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-Zen-fldlook.LookupName
"LookupName" "Lookup" "X(30)" "character" ? ? ? ? ? ? no ? no no "37.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-Zen-fldlook.tablename
"tablename" "Table" "X(20)" "character" ? ? ? ? ? ? no ? no no "35.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy window-maint
ON CHOOSE OF btn-copy IN FRAME a-frame-maint /* Copy */
DO:
  def var lv-ok as log no-undo.
  assign frame {&frame-name}
         t-zen-fldlook.lookupname 
         lv-fromlookup.
         
  if t-zen-fldlook.lookupname = '' then do:
    message 'Invalid Lookupname' view-as alert-box error.
    return no-apply.
  end.
  
  find b-t-{&table-name} where b-t-{&table-name}.lookupname = lv-fromlookup
                           no-lock no-error.
                           
  if not avail b-t-{&table-name} then do:
    message 'Invalid From Lookup Name'
    view-as alert-box error.
    return no-apply.
  end.
  buffer-copy b-t-{&table-name} except lookupname {&table-name}tableid to t-{&table-name}.
  
  run display-fields in this-procedure no-error.
  message 'Copy Fields Also ?' view-as alert-box question update lv-ok.   
  
  if lv-ok then do:
  {{&core}run.i 
      &program   = "zen-lookupfld.p"
      &path      = "{&core}{&srv}"
      &Appsrv    = "System"
      &procedure = "duplicate"
      &noper     = 'true'
      &params    = "(lv-fromlookup,
                   t-zen-fldlook.lookupname)"}
  end. 
  t-zen-fldlook.lookupname:modified in frame {&frame-name} = true.
  run save-trigger. 
                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-fields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-fields window-maint
ON CHOOSE OF btn-fields IN FRAME a-frame-maint /* Fields */
DO:
  runchild ('{&core}lookupfld-mnt.w',this-procedure).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-zen-fldlook.tablename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-zen-fldlook.tablename window-maint
ON LEAVE OF t-zen-fldlook.tablename IN FRAME a-frame-maint /* Lookup Table */
DO:
  setsysvar("SelectValue",self:screen-value).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildcombobox window-maint 
PROCEDURE buildcombobox :
/* def var h             as Handle no-undo.                                   */
/* Def Var pv-list       As Char   No-undo.                                   */
/* Def Var lv-param-list As Char   No-undo.                                   */
/* Def Var Y             As Int    No-undo.                                   */
/*                                                                            */
/* Assign t-zen-fldlook.validationfunction:list-items In Frame four = ' '     */
/*        t-zen-fldlook.validationfunction:screen-value = ' '                 */
/*        h = session:first-procedure.                                        */
/*                                                                            */
/* do while valid-handle(h):                                                  */
/*                                                                            */
/*                                                                            */
/*     If Index(h:Private-data,'library') Ne 0                                */
/*     Then do:                                                               */
/*         message '1 ' h:Private-data view-as alert-box.                     */
/*         y = 1.                                                             */
/*         Do Y = 1 To Num-entries(h:internal-entries):                       */
/*             lv-param-list = h:get-signature(entry(Y,h:internal-entries)).  */
/*             message '2' lv-param-list view-as alert-box.                   */
/*             If (Num-entries(lv-param-list) Ne 4)                    or     */
/*                (Entry(2,lv-param-list) Ne 'logical')                or     */
/*                (not (entry(1,lv-param-list) begins 'func' Or               */
/*                      entry(1,lv-param-list) begins 'ext'))          or     */
/*                (can-do(Pv-list,entry(Y,h:internal-entries)))        or     */
/*                (Not entry(Y,h:internal-entries) Begins 'Valid')     or     */
/*                (Entry(1,Entry(3,lv-param-list),' ') Ne 'input')     or     */
/*                (Entry(3,Entry(3,lv-param-list),' ') Ne 'character') or     */
/*                (Entry(1,Entry(4,lv-param-list),' ') Ne 'output')    or     */
/*                (Entry(3,Entry(4,lv-param-list),' ') Ne 'character')        */
/*             then next.                                                     */
/*                                                                            */
/*             Pv-list = Pv-list + "," + entry(Y,h:internal-entries).         */
/*         end.                                                               */
/*     End.                                                                   */
/*     h = h:next-sibling.                                                    */
/* End.                                                                       */
/* t-zen-fldlook.validationfunction:list-items = pv-list.                     */

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
  setsysvar("SelectValue",'_').
  
  
if lv-newmode then
    assign
        t-Zen-fldlook.look-pgm:screen-value in frame {&frame-name} = 'lookup.w'
        t-Zen-fldlook.Prog-Path:screen-value = '{&core}'.
        
       
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
  DISPLAY lv-fromlookup 
      WITH FRAME a-frame-maint.
  IF AVAILABLE t-zen-fldlook THEN 
    DISPLAY t-zen-fldlook.lookupname t-zen-fldlook.tablename 
          t-zen-fldlook.window-title t-zen-fldlook.whereclause 
          t-zen-fldlook.wherefield t-zen-fldlook.byclause t-zen-fldlook.look-pgm 
          t-zen-fldlook.prog-path t-zen-fldlook.srv-path t-zen-fldlook.srv-prog 
          t-zen-fldlook.srv-proc t-zen-fldlook.infopgm t-zen-fldlook.infopath 
          t-zen-fldlook.immediatequery t-zen-fldlook.help-ref 
          t-zen-fldlook.AltLookupName t-zen-fldlook.AltLookupLabel 
          t-zen-fldlook.help-file t-zen-fldlook.extra-details 
      WITH FRAME a-frame-maint.
  ENABLE br-maint btn-fields 
      WITH FRAME a-frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-a-frame-maint}
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

RUN buildcombobox.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-new-trigger window-maint 
PROCEDURE local-new-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

assign 
    lv-fromlookup:sensitive in frame {&frame-name} = true
    btn-copy:sensitive = true
    lv-fromlookup:screen-value = t-Zen-fldlook.lookupname
    lv-fromlookup:modified = false.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-save-trigger window-maint 
PROCEDURE local-save-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign 
 lv-fromlookup:sensitive in frame {&frame-name} = false
 btn-copy:sensitive = false.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-undo-trigger window-maint 
PROCEDURE local-undo-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign 
 lv-fromlookup:sensitive in frame {&frame-name} = false
 btn-copy:sensitive = false.


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
     when '{&core}lookupfld-mnt.w' then run refresh in  pv-to 
         (t-zen-fldlook.lookupname).
/*     when 'sr-maint.w'    then run refresh in  pv-to.                        */
end case.        
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
def input param pv-name as char no-undo.
def var lv-row as rowid no-undo.
 
find t-zen-fldlook where t-zen-fldlook.lookupname = pv-name
                   no-lock no-error.

lv-row = if avail t-zen-fldlook then rowid(t-zen-fldlook)
                                else ?. 

if lv-row ne ? then do with frame {&frame-name}:
    Reposition {&browse-name} To rowid lv-row.
    {&browse-name}:refresh().                             
    apply 'value-changed' to {&browse-name}.
end.

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
def buffer check_alloc for t-{&Table-name}.
 
if can-find(first check_alloc where check_alloc.lookupname = t-{&Table-name}.lookupname:screen-value in frame {&frame-name}
                                and recid(check_alloc) ne recid(t-{&Table-name}))
then do:
    message msg(122,"Field",t-{&Table-name}.lookupname,'','') view-as alert-box.
    Return String(t-{&Table-name}.lookupname:Handle).
end.
     
If t-{&Table-name}.lookupname:screen-value = "" Then Do:
    Message msg(20,"Lookup",'','','') view-as alert-box.
    Return String(t-{&Table-name}.lookupname:Handle).
End. 
/*                                                                     */
/* If t-{&Table-name}.keyfield:screen-value in frame two = "" Then Do: */
/*     Message msg(20,"Key Field",'','','') view-as alert-box.         */
/*     Return String(t-{&Table-name}.keyfield:Handle).                 */
/* End.                                                                */
/*                                                                     */
/* If t-{&Table-name}.descfield:screen-value = "" Then Do:             */
/*     Message msg(20,"Description Field",'','','') view-as alert-box. */
/*     Return String(t-{&Table-name}.descfield:Handle).                */
/* End.                                                                */

return 'passed'.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

