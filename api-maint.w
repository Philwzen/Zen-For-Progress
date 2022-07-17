&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-apidetail NO-UNDO LIKE zen-apidetail.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                                                         */
/*  PROGRAM TITLE   :                                                                           */ 
/*                                                                            */
/*  CREATE DATE     :                                                                           */
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
/* 22/09/00  v1                                             release                   */
/******************************************************************************/


CREATE WIDGET-POOL.
&glob title-text    Api Maintenance
&glob table-name    zen-apidetail
&glob Unique-key    {&table-name}tableid 
&glob KeepRefreshButton
/* ***************************  Definitions  ************************** */
{app-paths.i}
/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
&glob suppresswindow
def var lv-countrycode as char no-undo.
def var lv-sysman      as  log no-undo.
lv-sysman = systemmanager(GetUserID()).
&glob NotFoundMessage
/*
&glob LoadBackGround test
*/

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
&Scoped-define INTERNAL-TABLES t-Zen-ApiDetail

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-Zen-ApiDetail.ProcedureName ~
t-Zen-ApiDetail.ApiName t-Zen-ApiDetail.TableName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-Zen-ApiDetail NO-LOCK ~
    BY t-Zen-ApiDetail.zen-apidetailtableid
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-Zen-ApiDetail NO-LOCK ~
    BY t-Zen-ApiDetail.zen-apidetailtableid.
&Scoped-define TABLES-IN-QUERY-br-maint t-Zen-ApiDetail
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-Zen-ApiDetail


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-country br-maint 
&Scoped-Define DISPLAYED-FIELDS t-zen-apidetail.ApiName ~
t-zen-apidetail.TableName t-zen-apidetail.ProgramPath ~
t-zen-apidetail.ProgramName t-zen-apidetail.sysrecord ~
t-zen-apidetail.ProcedureName t-zen-apidetail.async ~
t-zen-apidetail.defaultapserver t-zen-apidetail.noper 
&Scoped-define DISPLAYED-TABLES t-zen-apidetail
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-apidetail
&Scoped-Define DISPLAYED-OBJECTS lv-country 

/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-zen-apidetail.ApiName t-zen-apidetail.TableName ~
t-zen-apidetail.ProgramPath t-zen-apidetail.ProgramName ~
t-zen-apidetail.sysrecord t-zen-apidetail.ProcedureName ~
t-zen-apidetail.async t-zen-apidetail.defaultapserver t-zen-apidetail.noper 
&Scoped-define Edit-List t-zen-apidetail.ApiName t-zen-apidetail.TableName ~
t-zen-apidetail.ProgramPath t-zen-apidetail.ProgramName ~
t-zen-apidetail.sysrecord t-zen-apidetail.ProcedureName ~
t-zen-apidetail.async t-zen-apidetail.defaultapserver t-zen-apidetail.noper 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE lv-country AS CHARACTER FORMAT "X(256)":U 
     LABEL "Country" 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-Zen-ApiDetail SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-Zen-ApiDetail.ProcedureName COLUMN-LABEL "Procedure" FORMAT "x(20)":U
            WIDTH 30.2
      t-Zen-ApiDetail.ApiName COLUMN-LABEL "API" FORMAT "x(30)":U
            WIDTH 30.2
      t-Zen-ApiDetail.TableName WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 91 BY 10.71 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     lv-country AT ROW 1.24 COL 21 COLON-ALIGNED
     br-maint AT ROW 2.43 COL 12 HELP
          "Select the record to edit."
     t-zen-apidetail.ApiName AT ROW 13.38 COL 24.2
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-apidetail.TableName AT ROW 14.48 COL 23.2
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-apidetail.ProgramPath AT ROW 15.67 COL 29.4
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-apidetail.ProgramName AT ROW 16.86 COL 19.8
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     t-zen-apidetail.sysrecord AT ROW 17 COL 75.6
          VIEW-AS TOGGLE-BOX
          SIZE 17.4 BY .81
     t-zen-apidetail.ProcedureName AT ROW 18.05 COL 17.8
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     t-zen-apidetail.async AT ROW 18.19 COL 75.6
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     t-zen-apidetail.defaultapserver AT ROW 19.24 COL 18
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     t-zen-apidetail.noper AT ROW 19.38 COL 75.6
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112 BY 20.52
         TITLE "Api Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-apidetail T "?" NO-UNDO schadm zen-apidetail
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Zone Maintenance"
         COLUMN             = 60
         ROW                = 6.76
         HEIGHT             = 20.57
         WIDTH              = 112
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
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint lv-country frame-maint */
/* SETTINGS FOR FILL-IN t-zen-apidetail.ApiName IN FRAME frame-maint
   NO-ENABLE ALIGN-L 1 2                                                */
/* SETTINGS FOR TOGGLE-BOX t-zen-apidetail.async IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
ASSIGN 
       br-maint:COLUMN-RESIZABLE IN FRAME frame-maint       = TRUE.

ASSIGN 
       t-Zen-ApiDetail.ApiName:AUTO-RESIZE IN BROWSE br-maint = TRUE.

/* SETTINGS FOR FILL-IN t-zen-apidetail.defaultapserver IN FRAME frame-maint
   NO-ENABLE ALIGN-L 1 2                                                */
/* SETTINGS FOR TOGGLE-BOX t-zen-apidetail.noper IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-apidetail.ProcedureName IN FRAME frame-maint
   NO-ENABLE ALIGN-L 1 2                                                */
/* SETTINGS FOR FILL-IN t-zen-apidetail.ProgramName IN FRAME frame-maint
   NO-ENABLE ALIGN-L 1 2                                                */
/* SETTINGS FOR FILL-IN t-zen-apidetail.ProgramPath IN FRAME frame-maint
   NO-ENABLE ALIGN-L 1 2                                                */
/* SETTINGS FOR TOGGLE-BOX t-zen-apidetail.sysrecord IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-apidetail.TableName IN FRAME frame-maint
   NO-ENABLE ALIGN-L 1 2                                                */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-Zen-ApiDetail"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _OrdList          = "Temp-Tables.t-Zen-ApiDetail.zen-apidetailtableid|yes"
     _FldNameList[1]   > Temp-Tables.t-Zen-ApiDetail.ProcedureName
"t-Zen-ApiDetail.ProcedureName" "Procedure" "x(20)" "" ? ? ? ? ? ? no ? no no "30.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-Zen-ApiDetail.ApiName
"t-Zen-ApiDetail.ApiName" "API" "x(30)" "" ? ? ? ? ? ? no ? no no "30.2" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.t-Zen-ApiDetail.TableName
"t-Zen-ApiDetail.TableName" ? ? "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define SELF-NAME lv-country
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-country window-maint
ON LEAVE OF lv-country IN FRAME frame-maint /* Country */
DO:
    run openquery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{{&core}commonmaint.i &path = "{&core}{&srv}"
                      &extraparams = "lv-countrycode,"}
                
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
    run initialise.
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

  t-{&table-name}.SysRecord:hidden in frame {&frame-name} = not lv-sysman .
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
  DISPLAY lv-country 
      WITH FRAME frame-maint.
  IF AVAILABLE t-zen-apidetail THEN 
    DISPLAY t-zen-apidetail.ApiName t-zen-apidetail.TableName 
          t-zen-apidetail.ProgramPath t-zen-apidetail.ProgramName 
          t-zen-apidetail.sysrecord t-zen-apidetail.ProcedureName 
          t-zen-apidetail.async t-zen-apidetail.defaultapserver 
          t-zen-apidetail.noper 
      WITH FRAME frame-maint.
  ENABLE lv-country br-maint 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraAssign window-maint 
PROCEDURE ExtraAssign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  t-zen-apidetail.country = lv-countrycode.
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
def input param pv-from as handle no-undo.


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
lv-country:screen-value in frame {&frame-name} 
    = GetField("zen-country","country",getsysvar("{&clv}country"),'description').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-openquery window-maint 
PROCEDURE Local-openquery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if donotfire('btn-exit') then return 'override'.

assign frame {&frame-name} lv-country.

if lv-country = ? or 
   lv-country = ''
then do:
    message msg(21,'Country',lv-country,'','')
    view-as alert-box.
    return 'override'.
end.

lv-countrycode  = GetField("zen-country",'description',lv-country,"country").

if lv-newmode or lv-editmode then return 'override'.

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
/* ----------------------------------------------------------------
  Purpose:      Checks the zone ref and description are entered
  Parameters:   None
  Notes:        Puts up error messages if invalid and stops 
                processing.
-----------------------------------------------------------------*/
  def var v-recid as recid no-undo. 
  def buffer b-{&table-name} for t-{&table-name}.
  assign frame {&frame-name} lv-country.
   if lv-country:screen-value = '' or
      GetField("zen-country",'description',lv-country,"country") = ?
   then do:
      MESSAGE msg(20,"Country",lv-country,"","") VIEW-AS ALERT-BOX.
      RETURN string(lv-country:HANDLE).
   end.

   IF NEW t-{&table-name} THEN  
      IF CAN-FIND(FIRST b-{&table-name}
          WHERE b-{&table-name}.ApiName = t-{&table-name}.ApiName:SCREEN-VALUE
            and b-{&table-name}.tablename = t-{&table-name}.tableName:SCREEN-VALUE) THEN
      DO:   
          MESSAGE msg(122,"Api",STRING(t-{&table-name}.ApiName:SCREEN-VALUE),"","") VIEW-AS ALERT-BOX.
          RETURN string(t-{&table-name}.ApiName:HANDLE IN FRAME {&FRAME-NAME}).
      END.

   if t-{&table-name}.ApiName:screen-value in frame {&frame-name}  = "" then
   do:
      message msg(20,"Api Name","","","") view-as alert-box information.
      return string(t-{&table-name}.ApiName:handle).
   end.

   return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

