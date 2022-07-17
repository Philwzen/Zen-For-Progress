&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-fldlook NO-UNDO LIKE Zen-fldlook.
DEFINE TEMP-TABLE t-Zen-widlook NO-UNDO LIKE Zen-widlook.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                                                         */
/*  PROGRAM TITLE   :                                                         */ 
/*                                                                            */
/*  CREATE DATE     :                                                         */
/*                                                                            */
/*  COMPANY NAME    :                                                         */
/*  VERSION NO.     :    1.0                                                  */
/******************************************************************************/
/******************************************************************************/
/*                          PATCH HISTORY                                     */
/*                                                                            */
/*           PATCH  USR                 Job                                   */
/* DATE      NO.    ID                  REF DESCRIPTION                       */
/* -------------------------------------------------------------------------- */
/* 22/09/00  v1                                                               */
/******************************************************************************/


create widget-pool.
&glob title-text    Assign a Lookup to a Field
&glob table-name    zen-widlook
&glob unique-key    {&table-name}TableId

&glob KeepRefreshButton 
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
&Scoped-define INTERNAL-TABLES t-Zen-widlook

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-Zen-widlook.LookupName ~
t-Zen-widlook.look-field 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-Zen-widlook NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-Zen-widlook NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-Zen-widlook
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-Zen-widlook


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-Search rs-type br-maint btn-newlookup 
&Scoped-Define DISPLAYED-FIELDS t-Zen-widlook.look-field ~
t-Zen-widlook.in-program t-Zen-widlook.in-frame t-Zen-widlook.lookupname ~
t-Zen-widlook.initialvaluefunction t-Zen-widlook.validationfunction ~
t-Zen-widlook.help-file t-Zen-widlook.help-ref t-Zen-widlook.sysrecord 
&Scoped-define DISPLAYED-TABLES t-Zen-widlook
&Scoped-define FIRST-DISPLAYED-TABLE t-Zen-widlook
&Scoped-Define DISPLAYED-OBJECTS lv-Search rs-type 

/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-Zen-widlook.look-field t-Zen-widlook.in-program ~
t-Zen-widlook.in-frame t-Zen-widlook.lookupname ~
t-Zen-widlook.initialvaluefunction t-Zen-widlook.validationfunction ~
t-Zen-widlook.help-file t-Zen-widlook.help-ref t-Zen-widlook.sysrecord 
&Scoped-define Edit-List t-Zen-widlook.look-field t-Zen-widlook.in-program ~
t-Zen-widlook.in-frame t-Zen-widlook.lookupname ~
t-Zen-widlook.initialvaluefunction t-Zen-widlook.validationfunction ~
t-Zen-widlook.help-file t-Zen-widlook.help-ref t-Zen-widlook.sysrecord 
&Scoped-define List-3 lv-Search rs-type 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-newlookup 
     LABEL "Lookups" 
     SIZE 12 BY 1.14.

DEFINE VARIABLE lv-Search AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Find" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE rs-type AS CHARACTER INITIAL "Field" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Field", "Field",
"Table", "Table",
"Program", "Program",
"Lookup", "Lookup"
     SIZE 43.6 BY .86 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-Zen-widlook SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-Zen-widlook.LookupName FORMAT "x(30)":U WIDTH 41.6
      t-Zen-widlook.look-field COLUMN-LABEL "Widget" FORMAT "x(40)":U
            WIDTH 35.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 82.4 BY 8.29 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     lv-Search AT ROW 1.24 COL 21 COLON-ALIGNED
     rs-type AT ROW 1.24 COL 56.6 NO-LABEL
     br-maint AT ROW 2.43 COL 16 HELP
          "Select the record to edit."
     t-Zen-widlook.look-field AT ROW 11 COL 19.6
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-Zen-widlook.in-program AT ROW 11.95 COL 20.6 FORMAT "X(80)"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-Zen-widlook.in-frame AT ROW 12.91 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     t-Zen-widlook.lookupname AT ROW 13.91 COL 32 COLON-ALIGNED FORMAT "X(80)"
          VIEW-AS FILL-IN 
          SIZE 61 BY 1
     btn-newlookup AT ROW 11.24 COL 88
     t-Zen-widlook.initialvaluefunction AT ROW 15.1 COL 32 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 61 BY 1 TOOLTIP "returns char (value as char,widget as handle,output extra as char)"
     t-Zen-widlook.validationfunction AT ROW 16.05 COL 32 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 61 BY 1 TOOLTIP "returns char (value as char,widget as handle,output extra as char)"
     t-Zen-widlook.help-file AT ROW 17 COL 32 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     t-Zen-widlook.help-ref AT ROW 17.95 COL 32 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
     t-Zen-widlook.sysrecord AT ROW 18.19 COL 53 WIDGET-ID 10
          LABEL "Distribute"
          VIEW-AS TOGGLE-BOX
          SIZE 17.6 BY 1.05
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 104.4 BY 19.29
         TITLE "Widget-Lookup Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-Zen-fldlook T "?" NO-UNDO schadm Zen-fldlook
      TABLE: t-Zen-widlook T "?" NO-UNDO schadm Zen-widlook
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 18.6
         ROW                = 7.48
         HEIGHT             = 22.43
         WIDTH              = 104.4
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
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB br-maint rs-type frame-maint */
/* SETTINGS FOR FILL-IN t-Zen-widlook.help-file IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-widlook.help-ref IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-widlook.in-frame IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-widlook.in-program IN FRAME frame-maint
   NO-ENABLE ALIGN-L 1 2 EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN t-Zen-widlook.initialvaluefunction IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-widlook.look-field IN FRAME frame-maint
   NO-ENABLE ALIGN-L 1 2                                                */
/* SETTINGS FOR FILL-IN t-Zen-widlook.lookupname IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN lv-Search IN FRAME frame-maint
   3                                                                    */
/* SETTINGS FOR RADIO-SET rs-type IN FRAME frame-maint
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX t-Zen-widlook.sysrecord IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-Zen-widlook.validationfunction IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-Zen-widlook"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-Zen-widlook.LookupName
"t-Zen-widlook.LookupName" ? "x(30)" "" ? ? ? ? ? ? no ? no no "41.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-Zen-widlook.look-field
"t-Zen-widlook.look-field" "Widget" "x(40)" "character" ? ? ? ? ? ? no ? no no "35.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define SELF-NAME btn-newlookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-newlookup window-maint
ON choose OF btn-newlookup IN FRAME frame-maint /* Lookups */
do:
  runchild("{&core}fldlook-mnt.w",this-procedure).

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-Search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-Search window-maint
ON leave OF lv-Search IN FRAME frame-maint /* Find */
do:
  if not t-{&table-name}.in-program:sensitive
  then run openquery.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-type window-maint
ON value-changed OF rs-type IN FRAME frame-maint
do:
 run openquery.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* Set current-window: this will parent dialog-boxes and frames. */
assign current-window                = {&window-name} 
       this-procedure:current-window = {&window-name}.

{{&core}commonmaint.i &path = "{&core}{&srv}"
                     &extraparams = "rs-type,lv-search,"}


/* Best default for GUI applications is... */
pause 0 before-hide.
 
/* Now enable the interface and wait for the exit condition. */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire. */
main-block:
do on error undo main-block, leave main-block:
  {{&core}sec-chk.i}
   run enable_UI.
  {{&core}wid-chk.i}
  {{&core}focus.i}
   if not this-procedure:persistent then
      wait-for close of this-procedure.
    
end.

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
    
    return "passed".

end procedure.

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
  DISPLAY lv-Search rs-type 
      WITH FRAME frame-maint.
  IF AVAILABLE t-Zen-widlook THEN 
    DISPLAY t-Zen-widlook.look-field t-Zen-widlook.in-program 
          t-Zen-widlook.in-frame t-Zen-widlook.lookupname 
          t-Zen-widlook.initialvaluefunction t-Zen-widlook.validationfunction 
          t-Zen-widlook.help-file t-Zen-widlook.help-ref t-Zen-widlook.sysrecord 
      WITH FRAME frame-maint.
  ENABLE lv-Search rs-type br-maint btn-newlookup 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
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
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-openquery window-maint 
PROCEDURE local-openquery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign frame {&frame-name}
    {&List-3}.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-update-child-procedures window-maint 
PROCEDURE Local-update-child-procedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-to as handle no-undo.
case pv-to:private-data:
    when "{&core}fldlook-mnt.w" then run refresh in pv-to (t-Zen-widlook.LookupName).  
end case.
end procedure.

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
/*                                                                                                               */
/*    IF NEW t-{&table-name} THEN                                                                                */
/*       IF CAN-FIND(FIRST b-{&table-name}                                                                       */
/*           WHERE b-{&table-name}.zone_ref = t-{&table-name}.zone_ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN */
/*       DO:                                                                                                     */
/*           MESSAGE msg(120,"Zone",STRING(t-{&table-name}.zone_ref:SCREEN-VALUE),"","") VIEW-AS ALERT-BOX.      */
/*           RETURN string(t-{&table-name}.zone_ref:HANDLE IN FRAME {&FRAME-NAME}).                              */
/*       end.                                                                                                    */
/*                                                                                                               */
/*    if t-{&table-name}.zone_ref:screen-value in frame {&frame-name}  = "" then                                 */
/*    do:                                                                                                        */
/*       message msg(20,"Zone Ref","","","") view-as alert-box information.                                      */
/*       return string(t-{&table-name}.zone_ref:handle).                                                         */
/*    end.                                                                                                       */
/*                                                                                                               */
/*    if t-{&table-name}.zone_desc:screen-value in frame {&frame-name} = "" then                                 */
/*    do:                                                                                                        */
/*       message msg(20,"Description","","","") view-as alert-box information.                                   */
/*       return string(t-{&table-name}.zone_desc:handle).                                                        */
/*    end.                                                                                                       */
/*                                                                                                               */
   return 'passed'.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

