&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-FieldDefault NO-UNDO LIKE Zen-FieldDefault.



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
&glob title-text Field Defaults Maintenance
&glob KeepRefreshButton

&glob table-name zen-fielddefault
&glob unique-key {&table-name}tableid

&glob suppresswinfdow   /* no window creation */
&glob NoChangedCheck    /* disable changed onleave check */


/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
/*
&glob NoNew             /* no new button */
&glob bug               /* turn on debug mesasges */
&glob tree              /* turn on procedure tree listing */
&glob NoExitCheck       /* disble exit question */
&glob NoButtons         /* no buttons displayed */
&glob NoEdit            /* no edit button */
&glob NoSave            /* no save button */
&glob NoDelete          /* no delete button */
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
def var lv-user as char no-undo.

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
&Scoped-define INTERNAL-TABLES t-Zen-FieldDefault

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-Zen-FieldDefault.DefaultValue ~
t-Zen-FieldDefault.WidExtent t-Zen-FieldDefault.WidgetName ~
t-Zen-FieldDefault.tablename t-Zen-FieldDefault.FrameName ~
t-Zen-FieldDefault.ProgramName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-Zen-FieldDefault NO-LOCK ~
    BY t-Zen-FieldDefault.duser ~
       BY t-Zen-FieldDefault.ProgramName ~
        BY t-Zen-FieldDefault.FrameName ~
         BY t-Zen-FieldDefault.tablename ~
          BY t-Zen-FieldDefault.WidgetName ~
           BY t-Zen-FieldDefault.WidExtent
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-Zen-FieldDefault NO-LOCK ~
    BY t-Zen-FieldDefault.duser ~
       BY t-Zen-FieldDefault.ProgramName ~
        BY t-Zen-FieldDefault.FrameName ~
         BY t-Zen-FieldDefault.tablename ~
          BY t-Zen-FieldDefault.WidgetName ~
           BY t-Zen-FieldDefault.WidExtent.
&Scoped-define TABLES-IN-QUERY-br-maint t-Zen-FieldDefault
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-Zen-FieldDefault


/* Definitions for FRAME a-frame-maint                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb-user lv-prog br-maint Btn-delall 
&Scoped-Define DISPLAYED-FIELDS t-Zen-FieldDefault.ProgramName ~
t-Zen-FieldDefault.FrameName t-Zen-FieldDefault.tablename ~
t-Zen-FieldDefault.WidgetName t-Zen-FieldDefault.WidExtent ~
t-Zen-FieldDefault.DefaultValue t-Zen-FieldDefault.PopulationRoutine 
&Scoped-define DISPLAYED-TABLES t-Zen-FieldDefault
&Scoped-define FIRST-DISPLAYED-TABLE t-Zen-FieldDefault
&Scoped-Define DISPLAYED-OBJECTS cb-user lv-prog 

/* Custom List Definitions                                              */
/* add-list,edit-list,List-3,List-4,List-5,List-6                       */
&Scoped-define add-list t-Zen-FieldDefault.ProgramName ~
t-Zen-FieldDefault.FrameName t-Zen-FieldDefault.tablename ~
t-Zen-FieldDefault.WidgetName t-Zen-FieldDefault.WidExtent ~
t-Zen-FieldDefault.DefaultValue t-Zen-FieldDefault.PopulationRoutine 
&Scoped-define edit-list t-Zen-FieldDefault.ProgramName ~
t-Zen-FieldDefault.FrameName t-Zen-FieldDefault.tablename ~
t-Zen-FieldDefault.WidgetName t-Zen-FieldDefault.WidExtent ~
t-Zen-FieldDefault.DefaultValue t-Zen-FieldDefault.PopulationRoutine 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-delall 
     LABEL "Delete All Items Shown" 
     SIZE 30 BY 1.14.

DEFINE VARIABLE cb-user AS CHARACTER FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 47 BY 1
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE lv-prog AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Program" 
     VIEW-AS FILL-IN 
     SIZE 129 BY 1
     BGCOLOR 21  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-Zen-FieldDefault SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-Zen-FieldDefault.DefaultValue WIDTH 22.2
      t-Zen-FieldDefault.WidExtent COLUMN-LABEL "Ext" WIDTH 4
      t-Zen-FieldDefault.WidgetName COLUMN-LABEL "Widget" FORMAT "X(25)":U
      t-Zen-FieldDefault.tablename FORMAT "X(15)":U
      t-Zen-FieldDefault.FrameName FORMAT "X(10)":U WIDTH 12.8
      t-Zen-FieldDefault.ProgramName FORMAT "X(75)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 179 BY 11.95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME a-frame-maint
     cb-user AT ROW 1.14 COL 20.6 COLON-ALIGNED WIDGET-ID 8
     lv-prog AT ROW 2.14 COL 20.6 COLON-ALIGNED WIDGET-ID 30
     br-maint AT ROW 3.14 COL 12 HELP
          "Select the record to edit."
     t-Zen-FieldDefault.ProgramName AT ROW 15.29 COL 23 COLON-ALIGNED HELP
          "" WIDGET-ID 36
          LABEL "Program Name" FORMAT "X(250)"
          VIEW-AS FILL-IN 
          SIZE 81 BY 1
     t-Zen-FieldDefault.FrameName AT ROW 16.29 COL 23 COLON-ALIGNED HELP
          "" WIDGET-ID 38
          LABEL "Frame" FORMAT "X(15)"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     t-Zen-FieldDefault.tablename AT ROW 17.29 COL 23 COLON-ALIGNED HELP
          "Name of table" WIDGET-ID 40
          LABEL "Table Name" FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     t-Zen-FieldDefault.WidgetName AT ROW 18.29 COL 23 COLON-ALIGNED HELP
          "Field Name" WIDGET-ID 44
          LABEL "Widget" FORMAT "X(35)"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     t-Zen-FieldDefault.WidExtent AT ROW 15.29 COL 119.6 COLON-ALIGNED HELP
          "" WIDGET-ID 46
          LABEL "Extent" FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     t-Zen-FieldDefault.DefaultValue AT ROW 16.29 COL 119.6 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          LABEL "Value" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 35.6 BY 1
     t-Zen-FieldDefault.PopulationRoutine AT ROW 17.29 COL 119.6 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     Btn-delall AT ROW 15.29 COL 161 WIDGET-ID 34
     "Defined as function(widget-handle,output extrastring) returns char" VIEW-AS TEXT
          SIZE 79 BY .86 AT ROW 18.29 COL 98 WIDGET-ID 32
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.1
         SIZE 199.4 BY 19.52
         TITLE "Widget Defaults" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-Zen-FieldDefault T "?" NO-UNDO schadm Zen-FieldDefault
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
         HEIGHT             = 19.76
         WIDTH              = 200.2
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
/* BROWSE-TAB br-maint lv-prog a-frame-maint */
ASSIGN 
       br-maint:ALLOW-COLUMN-SEARCHING IN FRAME a-frame-maint = TRUE
       br-maint:COLUMN-RESIZABLE IN FRAME a-frame-maint       = TRUE.

ASSIGN 
       t-Zen-FieldDefault.DefaultValue:AUTO-RESIZE IN BROWSE br-maint = TRUE
       t-Zen-FieldDefault.WidExtent:AUTO-RESIZE IN BROWSE br-maint = TRUE
       t-Zen-FieldDefault.WidgetName:AUTO-RESIZE IN BROWSE br-maint = TRUE
       t-Zen-FieldDefault.tablename:AUTO-RESIZE IN BROWSE br-maint = TRUE
       t-Zen-FieldDefault.FrameName:AUTO-RESIZE IN BROWSE br-maint = TRUE
       t-Zen-FieldDefault.ProgramName:AUTO-RESIZE IN BROWSE br-maint = TRUE.

/* SETTINGS FOR FILL-IN t-Zen-FieldDefault.DefaultValue IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR FILL-IN t-Zen-FieldDefault.FrameName IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR FILL-IN t-Zen-FieldDefault.PopulationRoutine IN FRAME a-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-FieldDefault.ProgramName IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR FILL-IN t-Zen-FieldDefault.tablename IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR FILL-IN t-Zen-FieldDefault.WidExtent IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR FILL-IN t-Zen-FieldDefault.WidgetName IN FRAME a-frame-maint
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT EXP-HELP                          */
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
     _TblList          = "Temp-Tables.t-Zen-FieldDefault"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _OrdList          = "Temp-Tables.t-Zen-FieldDefault.duser|yes,Temp-Tables.t-Zen-FieldDefault.ProgramName|yes,Temp-Tables.t-Zen-FieldDefault.FrameName|yes,Temp-Tables.t-Zen-FieldDefault.tablename|yes,Temp-Tables.t-Zen-FieldDefault.WidgetName|yes,Temp-Tables.t-Zen-FieldDefault.WidExtent|yes"
     _FldNameList[1]   > Temp-Tables.t-Zen-FieldDefault.DefaultValue
"t-Zen-FieldDefault.DefaultValue" ? ? "character" ? ? ? ? ? ? no ? no no "22.2" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-Zen-FieldDefault.WidExtent
"t-Zen-FieldDefault.WidExtent" "Ext" ? "integer" ? ? ? ? ? ? no ? no no "4" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.t-Zen-FieldDefault.WidgetName
"t-Zen-FieldDefault.WidgetName" "Widget" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.t-Zen-FieldDefault.tablename
"t-Zen-FieldDefault.tablename" ? "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.t-Zen-FieldDefault.FrameName
"t-Zen-FieldDefault.FrameName" ? "X(10)" "character" ? ? ? ? ? ? no ? no no "12.8" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.t-Zen-FieldDefault.ProgramName
"t-Zen-FieldDefault.ProgramName" ? "X(75)" "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-delall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-delall window-maint
ON CHOOSE OF Btn-delall IN FRAME a-frame-maint /* Delete All Items Shown */
DO:
def var lv-ok as log no-undo.

message 'This will Remove all defaults for : ' skip
        'User    : ' lv-user skip
        'Program : ' lv-prog skip
         'Continue ?.'
view-as alert-box question buttons yes-no update lv-ok.
if not lv-ok then return no-apply.

   {{&core}run.i &program   = "{&table-name}.p"
               &path      = "{&core}{&srv}"
               &Appsrv    = "System"
               &procedure = "DelAllForProgUser"
               &params    = "(lv-user,lv-prog)"}
   
   if anyerrors() then return no-apply.
   
   for each t-{&table-name} where t-{&table-name}.duser = lv-user 
                             and t-{&table-name}.ProgramName matches lv-prog:
      delete t-{&table-name}.
   end.
   
   {&browse-name}:refresh().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-user window-maint
ON VALUE-CHANGED OF cb-user IN FRAME a-frame-maint /* User */
DO:
  run refresh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-prog window-maint
ON LEAVE OF lv-prog IN FRAME a-frame-maint /* Program */
DO:
  run refresh.
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
{{&core}commonmaint.i &path = "{&core}{&srv}"
                      &extraparams = "lv-user,lv-prog,"}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createExtraFields window-maint 
PROCEDURE createExtraFields :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ----------------------------------------------------------------------------*/

   t-{&table-name}.duser  = cb-user:screen-value in frame {&frame-name}.
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
  DISPLAY cb-user lv-prog 
      WITH FRAME a-frame-maint.
  IF AVAILABLE t-Zen-FieldDefault THEN 
    DISPLAY t-Zen-FieldDefault.ProgramName t-Zen-FieldDefault.FrameName 
          t-Zen-FieldDefault.tablename t-Zen-FieldDefault.WidgetName 
          t-Zen-FieldDefault.WidExtent t-Zen-FieldDefault.DefaultValue 
          t-Zen-FieldDefault.PopulationRoutine 
      WITH FRAME a-frame-maint.
  ENABLE cb-user lv-prog br-maint Btn-delall 
      WITH FRAME a-frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-a-frame-maint}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-BEFORE-OPEN-query window-maint 
PROCEDURE local-BEFORE-OPEN-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
setsensitive(true,'inc','btn-query',frame {&frame-name}:handle).
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
run pop-combos.

run refresh.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pop-combos window-maint 
PROCEDURE pop-combos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

BuildCombo(cb-user:handle in frame {&frame-name},
           'zen-duser',
            'duser',
            'duser',
            '',
            '',
            no,
            no).
cb-user:screen-value in frame {&frame-name} = getsysvar('{&clv}user').


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
assign lv-user = getcombokey(cb-user:handle in frame {&frame-name})
       lv-prog.  

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

