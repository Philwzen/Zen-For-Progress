&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-ocx NO-UNDO LIKE zen-ocx.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     : ????????                                                */
/*  PROGRAM TITLE   : ????????                                                */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    : East India Trading LTD                                    */
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
&glob title-text    ActiveX Controls
&glob table-name    zen-ocx
&glob unique-key    {&table-name}TableId
&glob KeepRefreshButton
&Glob ImmediateQuery

/* ***************************  Definitions  ************************** */
{app-paths.i}

/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */

def var lv-sysman     as  log no-undo.
lv-sysman = systemmanager(GetUserID()).

&glob noaudit
&glob noquery
&glob noprint

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
&Scoped-define INTERNAL-TABLES t-zen-ocx

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-ocx.ocx-filename ~
t-zen-ocx.description 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-ocx ~
      WHERE (if lv-sysman then true else t-zen-ocx.SysRecord = no) NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-ocx ~
      WHERE (if lv-sysman then true else t-zen-ocx.SysRecord = no) NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-ocx
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-ocx


/* Definitions for FRAME frame-maint                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-frame-maint ~
    ~{&OPEN-QUERY-br-maint}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint btn-reg 
&Scoped-Define DISPLAYED-FIELDS t-zen-ocx.description t-zen-ocx.ocx-name ~
t-zen-ocx.ocx-filename t-zen-ocx.surportingfiles t-zen-ocx.install ~
t-zen-ocx.sysrecord 
&Scoped-define DISPLAYED-TABLES t-zen-ocx
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-ocx


/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-zen-ocx.description t-zen-ocx.ocx-name ~
t-zen-ocx.ocx-filename t-zen-ocx.surportingfiles t-zen-ocx.install ~
t-zen-ocx.sysrecord 
&Scoped-define Edit-List t-zen-ocx.description t-zen-ocx.ocx-name ~
t-zen-ocx.ocx-filename t-zen-ocx.surportingfiles t-zen-ocx.install ~
t-zen-ocx.sysrecord 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD registeredocx window-maint 
FUNCTION registeredocx RETURNS LOGICAL
    ( pv-ocx as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-blob 
     LABEL "Blobs" 
     SIZE 11.8 BY 1.14.

DEFINE BUTTON btn-reg 
     LABEL "Register" 
     SIZE 12 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-ocx SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-ocx.ocx-filename FORMAT "X(55)":U WIDTH 66.6
      t-zen-ocx.description WIDTH 34.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 107 BY 13.24 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     br-maint AT ROW 1.1 COL 12
     t-zen-ocx.description AT ROW 14.43 COL 31 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 80 BY 1
     btn-reg AT ROW 14.67 COL 2 WIDGET-ID 4
     t-zen-ocx.ocx-name AT ROW 15.38 COL 31 COLON-ALIGNED FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 80 BY 1
     btn-blob AT ROW 15.86 COL 2 WIDGET-ID 2
     t-zen-ocx.ocx-filename AT ROW 16.33 COL 31 COLON-ALIGNED FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 80 BY 1
     t-zen-ocx.surportingfiles AT ROW 17.29 COL 12.8
          VIEW-AS FILL-IN 
          SIZE 80 BY 1
     t-zen-ocx.install AT ROW 18.38 COL 33
          LABEL "Attempt Install"
          VIEW-AS TOGGLE-BOX
          SIZE 25.6 BY .81
     t-zen-ocx.sysrecord AT ROW 18.38 COL 81
          VIEW-AS TOGGLE-BOX
          SIZE 20.2 BY .81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.4 BY 19.67
         TITLE "Binary File Registration".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-ocx T "?" NO-UNDO schadm zen-ocx
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "OCX Maintenance"
         COLUMN             = 68.4
         ROW                = 7.33
         HEIGHT             = 23.14
         WIDTH              = 120.4
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frame-maint
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint 1 frame-maint */
ASSIGN 
       br-maint:COLUMN-RESIZABLE IN FRAME frame-maint       = TRUE.

/* SETTINGS FOR BUTTON btn-blob IN FRAME frame-maint
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN t-zen-ocx.description IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-zen-ocx.install IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-zen-ocx.ocx-filename IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-zen-ocx.ocx-name IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-zen-ocx.surportingfiles IN FRAME frame-maint
   NO-ENABLE ALIGN-L 1 2                                                */
/* SETTINGS FOR TOGGLE-BOX t-zen-ocx.sysrecord IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-ocx"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "(if lv-sysman then true else t-zen-ocx.SysRecord = no)"
     _FldNameList[1]   > Temp-Tables.t-zen-ocx.ocx-filename
"t-zen-ocx.ocx-filename" ? "X(55)" "character" ? ? ? ? ? ? no ? no no "66.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-ocx.description
"t-zen-ocx.description" ? ? "character" ? ? ? ? ? ? no ? no no "34.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-blob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-blob window-maint
ON CHOOSE OF btn-blob IN FRAME frame-maint /* Blobs */
DO:
  runchild('{&core}blob-mnt.w',this-procedure).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-reg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-reg window-maint
ON CHOOSE OF btn-reg IN FRAME frame-maint /* Register */
DO:
    run RegisterOCX(t-zen-ocx.ocx-filename).
    if return-value ne 'passed' 
    then Message msg(159,t-zen-ocx.ocx-filename,'','','')
         view-as alert-box warning.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disp-Wids window-maint 
PROCEDURE Disp-Wids :
t-{&table-name}.SysRecord:hidden in frame {&frame-name} = not lv-sysman .

btn-blob:sensitive = not lv-newmode.

btn-reg:sensitive = NOT RegisteredOCX(t-zen-ocx.ocx-name) .
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
  IF AVAILABLE t-zen-ocx THEN 
    DISPLAY t-zen-ocx.description t-zen-ocx.ocx-name t-zen-ocx.ocx-filename 
          t-zen-ocx.surportingfiles t-zen-ocx.install t-zen-ocx.sysrecord 
      WITH FRAME frame-maint.
  ENABLE br-maint btn-reg 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialise window-maint 
PROCEDURE Local-Initialise :
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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param pv-to as handle no-undo.

case pv-to:private-data:
   when '{&core}blob-mnt.w' 
   then run refresh in pv-to ('{&table-name}',
                              t-{&table-name}.{&table-name}TableId,
                              t-zen-ocx.description,
                              t-zen-ocx.ocx-filename).
end case.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE registerocx window-maint 
PROCEDURE registerocx :
def input param pv-ocx as char no-undo.

def var lv-file   as char no-undo.
def var lv-from   as char no-undo.
def var lv-to     as char no-undo.
DEF VAR rg-path   AS CHAR NO-UNDO.
DEF VAR hInstance AS INT  NO-UNDO.
def var lv-ok     as log  no-undo.
def var x         as int  no-undo.
def var lv-windir as char no-undo.
 lv-windir = os-getenv('windir').
assign
   pv-ocx = dospath(pv-ocx)
   x = max(1,num-entries(pv-ocx,'~\'))
   lv-file = entry(x,pv-ocx,'~\')
   lv-from = getfullpath("{&core}ocx~\" + lv-file)
   lv-to   = substring(pv-ocx,1,r-index(pv-ocx,'~\') - 1).
if lv-to begins '$windir' 
then lv-to = replace(lv-to,'$windir',lv-windir).
/* ideally need to get file from blod in db so we need call
   to appserver to retrieve blod rather than the getfullpath above
then do an outputofile(blob,"{&core}ocx~\" + lv-file,'local') */

if lv-from ne ? and lv-from ne ''
then do:
   if t-zen-ocx.ocx-name ne 'unregisterable'
      then sysmsg(AltLanguage("Attempting to register OCX control " + pv-ocx + "...")).
   os-create-dir value(lv-to). /* it will fail if already exits so no problem */
   os-copy value(lv-from) value(lv-to).
   if t-zen-ocx.ocx-name ne 'unregisterable'
   then do:
      execute('regsvr32.exe ',
              lv-windir + '~\system32',
              lv-to + '~\' + lv-file,
              'normal').
   end.
   sysmsg('off').
   assign
      lv-from = substring(lv-from,1,r-index(lv-from,'~\'))
      lv-to   = lv-to + '~\'.

   do x = 1 to num-entries(t-zen-ocx.SurportingFiles):
     if entry(2,entry(x,t-zen-ocx.SurportingFiles),'.') ne 'exe' and
        entry(2,entry(x,t-zen-ocx.SurportingFiles),'.') ne 'msi' 
     then OS-COPY value(lv-from + entry(x,t-zen-ocx.SurportingFiles)) 
                  value(lv-to + entry(x,t-zen-ocx.SurportingFiles)).
     else do:
         message  'Run Associated Install ' entry(x,t-zen-ocx.SurportingFiles) '?'
         view-as alert-box question buttons yes-no update lv-ok.
         if lv-ok
         then execute(entry(x,t-zen-ocx.SurportingFiles),lv-from,"",'normal').
     end.
   end.
   return 'passed'.  
END.
ELSE do:
   sysmsg('off').
   return 'failed'.
end.

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
  /*def var v-recid as recid no-undo. 
  def buffer b-{&table-name} for t-{&table-name}.

   IF NEW t-{&table-name} THEN  
      IF CAN-FIND(FIRST b-{&table-name}
          WHERE b-{&table-name}.zone_ref = t-{&table-name}.zone_ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
      DO:   
          MESSAGE msg(120,"Zone",STRING(t-{&table-name}.zone_ref:SCREEN-VALUE),"","") VIEW-AS ALERT-BOX.
          RETURN string(t-{&table-name}.zone_ref:HANDLE IN FRAME {&FRAME-NAME}).
      END.

   if t-{&table-name}.zone_ref:screen-value in frame {&frame-name}  = "" then
   do:
      message msg(20,"Zone Ref","","","") view-as alert-box information.
      return string(t-{&table-name}.zone_ref:handle).
   end.

   if t-{&table-name}.zone_desc:screen-value in frame {&frame-name} = "" then
   do:
      message msg(20,"Description","","","") view-as alert-box information.
      return string(t-{&table-name}.zone_desc:handle).
   end.
   */
   return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION registeredocx window-maint 
FUNCTION registeredocx RETURNS LOGICAL
    ( pv-ocx as char ) :

      if t-zen-ocx.ocx-name = 'unregisterable' 
      then return false.

      DEF VAR ch-Check AS COM-HANDLE NO-UNDO.

      CREATE VALUE(pv-ocx) ch-Check NO-ERROR.
      
      IF error-status:error and not
         ERROR-STATUS:GET-MESSAGE(1) MATCHES "*0x80040112*" 
      THEN return False.

      else RETURN true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

