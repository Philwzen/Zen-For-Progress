&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-control NO-UNDO LIKE zen-control.



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
/* ??/??/??  P00    Philw   00  initial release                               
   01/19/10  P56    Annerik GG  Set memptrs to zero to avoid Progres bug      */
/******************************************************************************/
create widget-pool.
/* &glob bug screen * */
{app-paths.i}

&glob title-text System Control Maintenance
&glob table-name zen-control
&glob unique-key    {&table-name}TableId
/* &glob KeepRefreshButton  */
&Glob ImmediateQuery
&glob useDBbuttons
/* ***************************  Definitions  ************************** */
/* &undefine getallonquery  */
/* Parameters Definitions ---                                           */
&glob suppresswindow
/* Local var Definitions ---                                            */
def var lv-sysman     as  log no-undo.
lv-sysman = systemmanager(GetUserID()).

&glob nodefaults
&glob nohelp
&glob nosavenew

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME control-frame-maint
&Scoped-define BROWSE-NAME br-maint

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-zen-control

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-control.ctrl-idx ~
t-zen-control.ctrl-data 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-control NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-control NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-control
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-control


/* Definitions for FRAME control-frame-maint                            */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint btn-loadblob 
&Scoped-Define DISPLAYED-FIELDS t-zen-control.ctrl-idx ~
t-zen-control.sysrecord t-zen-control.ctrl-data t-zen-control.reread ~
t-zen-control.Description t-zen-control.Blob-Filename 
&Scoped-define DISPLAYED-TABLES t-zen-control
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-control


/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-zen-control.ctrl-idx t-zen-control.sysrecord ~
t-zen-control.ctrl-data t-zen-control.reread t-zen-control.Description ~
t-zen-control.Blob-Filename 
&Scoped-define Edit-List t-zen-control.sysrecord t-zen-control.ctrl-data ~
t-zen-control.reread t-zen-control.Description t-zen-control.Blob-Filename 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-loadblob 
     LABEL "Load Blob" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-control SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-control.ctrl-idx COLUMN-LABEL "Key" FORMAT "X(30)":U
            WIDTH 40
      t-zen-control.ctrl-data COLUMN-LABEL "Value" WIDTH 50
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 94 BY 9.95 NO-EMPTY-SPACE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME control-frame-maint
     br-maint AT ROW 1.52 COL 11 HELP
          "Select the record to edit."
     t-zen-control.ctrl-idx AT ROW 11.71 COL 15.2 COLON-ALIGNED FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     t-zen-control.sysrecord AT ROW 11.71 COL 78.4
          LABEL "Distributable Field"
          VIEW-AS TOGGLE-BOX
          SIZE 26 BY 1
     t-zen-control.ctrl-data AT ROW 12.71 COL 15.2 COLON-ALIGNED FORMAT "X(400)"
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     t-zen-control.reread AT ROW 12.71 COL 78.4
          LABEL "Reread"
          VIEW-AS TOGGLE-BOX
          SIZE 26 BY 1
     t-zen-control.Description AT ROW 14.24 COL 9 NO-LABEL
          VIEW-AS EDITOR
          SIZE 95.4 BY 4
     btn-loadblob AT ROW 18.67 COL 9
     t-zen-control.Blob-Filename AT ROW 18.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62.4 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 108.6 BY 19.95
         TITLE "System Control Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-control T "?" NO-UNDO schadm zen-control
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
         HEIGHT             = 20.48
         WIDTH              = 108.8
         MAX-HEIGHT         = 45.33
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.33
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
/* SETTINGS FOR FRAME control-frame-maint
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint 1 control-frame-maint */
/* SETTINGS FOR FILL-IN t-zen-control.Blob-Filename IN FRAME control-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-control.ctrl-data IN FRAME control-frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-zen-control.ctrl-idx IN FRAME control-frame-maint
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR EDITOR t-zen-control.Description IN FRAME control-frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-zen-control.reread IN FRAME control-frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX t-zen-control.sysrecord IN FRAME control-frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-control"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-zen-control.ctrl-idx
"t-zen-control.ctrl-idx" "Key" "X(30)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-control.ctrl-data
"t-zen-control.ctrl-data" "Value" ? "character" ? ? ? ? ? ? no ? no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME control-frame-maint
/* Query rebuild information for FRAME control-frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME control-frame-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-loadblob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-loadblob window-maint
ON choose OF btn-loadblob IN FRAME control-frame-maint /* Load Blob */
do:
  
   define var lv-filename as char   no-undo.
   define var lv-memptr   as memptr no-undo.


   /* make sure there is nothing left over in the memory pointer! */
   set-size(lv-memptr) = 0.

   lv-filename = getosfile(lv-filename).
   if lv-filename ne '?' then do:
      lv-filename = getfullpath(lv-filename).
      copy-lob from file lv-filename to lv-memptr.
      assign
         t-zen-control.Blob-Filename = lv-filename
         t-zen-control.ctrl-blob     = lv-memptr
         t-zen-control.ctrl-data     = 'blob'.
   end.
   disp t-zen-control.Blob-Filename
        t-zen-control.ctrl-data 
   with frame {&frame-name}.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */

/* Set current-window: this will parent dialog-boxes and frames. */
assign current-window                = {&window-name} 
       this-procedure:current-window = {&window-name}.
/*******************
    DONT FORGET TO CHECK DIR LOCATION FOR BELOW INCLUDE !!!!!!!
********************/
{{&core}commonmaint.i &path = "{&core}{&srv}"
                     &extraparams = "lv-sysman,"}

/* Best default for GUI applications is... */
pause 0 before-hide.
/* Now enable the interface and wait for the exit condition. */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire. */
main-block:
do on error undo main-block, leave main-block:
  {{&core}sec-chk.i}
  if not lv-sysman then 
  lv-progmode = 'read-only'.

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
  HIDE FRAME control-frame-maint.
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
  t-{&table-name}.SysRecord:hidden in frame {&frame-name} = not lv-sysman.

  btn-loadblob:sensitive = false.  
end procedure.

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
  IF AVAILABLE t-zen-control THEN 
    DISPLAY t-zen-control.ctrl-idx t-zen-control.sysrecord t-zen-control.ctrl-data 
          t-zen-control.reread t-zen-control.Description 
          t-zen-control.Blob-Filename 
      WITH FRAME control-frame-maint.
  ENABLE br-maint btn-loadblob 
      WITH FRAME control-frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-control-frame-maint}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-Enable window-maint 
PROCEDURE local-Enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  btn-loadblob:sensitive in frame {&frame-name} = true.  
end procedure.

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
end procedure.

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
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

