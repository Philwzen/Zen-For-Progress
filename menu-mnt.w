&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-Dmenu NO-UNDO LIKE Zen-Dmenu
       field t-node-id  as int
       field t-to-level as log
       field t-from-id as int.



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
&glob title-text Menus
&glob table-name zen-dmenu
&glob unique-key    {&table-name}TableId
&glob KeepRefreshButton
&Glob ImmediateQuery
&glob noPrint

/* ***************************  Definitions  ************************** */
{app-paths.i}

/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
{{&core}sl-maint.i}
def var lv-menuparent as char no-undo extent 99.
def var lv-depth as int no-undo init 1.
&glob Maxlistcount 2000
&glob NoBrowseSorting
DEF VAR lv-typekeys    AS CHAR NO-UNDO.
DEF VAR lv-typelist    AS CHAR NO-UNDO.
DEF VAR vc-parent-name AS CHAR NO-UNDO.
def var lv-sysman      as  log no-undo.
def var lv-group       as char no-undo.
def var lv-user as char no-undo.
def var lv-ugroup as char no-undo.
def var lv-hColor as handle no-undo.

lv-sysman = systemmanager(GetUserID()).

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
&Scoped-define INTERNAL-TABLES t-Zen-Dmenu

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-Zen-Dmenu.menu-action ~
t-Zen-Dmenu.menu-name t-Zen-Dmenu.menu-parent 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-Zen-Dmenu ~
      WHERE t-Zen-Dmenu.menu-parent = lv-menuparent[lv-depth] NO-LOCK ~
    BY t-Zen-Dmenu.menu-parent ~
       BY t-Zen-Dmenu.display-order
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-Zen-Dmenu ~
      WHERE t-Zen-Dmenu.menu-parent = lv-menuparent[lv-depth] NO-LOCK ~
    BY t-Zen-Dmenu.menu-parent ~
       BY t-Zen-Dmenu.display-order.
&Scoped-define TABLES-IN-QUERY-br-maint t-Zen-Dmenu
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-Zen-Dmenu


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-35 RECT-7 btn-in cb-group lv-all ~
br-maint btn-secrep 
&Scoped-Define DISPLAYED-FIELDS t-Zen-Dmenu.menu-id t-Zen-Dmenu.menu-parent ~
t-Zen-Dmenu.menu-name t-Zen-Dmenu.menu-action t-Zen-Dmenu.display-order ~
t-Zen-Dmenu.menu-grp t-Zen-Dmenu.sysrecord t-Zen-Dmenu.not-users ~
t-Zen-Dmenu.run-users t-Zen-Dmenu.not-groups t-Zen-Dmenu.run-groups ~
t-Zen-Dmenu.menu-inputs 
&Scoped-define DISPLAYED-TABLES t-Zen-Dmenu
&Scoped-define FIRST-DISPLAYED-TABLE t-Zen-Dmenu
&Scoped-Define DISPLAYED-OBJECTS cb-group lv-all lv-seccopy 

/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-Zen-Dmenu.menu-parent t-Zen-Dmenu.menu-name ~
t-Zen-Dmenu.menu-action t-Zen-Dmenu.display-order t-Zen-Dmenu.menu-grp ~
t-Zen-Dmenu.sysrecord t-Zen-Dmenu.not-users t-Zen-Dmenu.run-users ~
t-Zen-Dmenu.not-groups t-Zen-Dmenu.run-groups t-Zen-Dmenu.menu-inputs ~
lv-seccopy 
&Scoped-define Edit-List t-Zen-Dmenu.menu-parent t-Zen-Dmenu.menu-name ~
t-Zen-Dmenu.menu-action t-Zen-Dmenu.display-order t-Zen-Dmenu.menu-grp ~
t-Zen-Dmenu.sysrecord t-Zen-Dmenu.not-users t-Zen-Dmenu.run-users ~
t-Zen-Dmenu.not-groups t-Zen-Dmenu.run-groups t-Zen-Dmenu.menu-inputs ~
lv-seccopy 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-in 
     IMAGE-UP FILE "{&core}grafix/next-au.ico":U
     IMAGE-INSENSITIVE FILE "{&core}grafix/next-au-si.ico":U
     LABEL "in" 
     SIZE 8.2 BY 1.71 TOOLTIP "Step In".

DEFINE BUTTON btn-out 
     IMAGE-UP FILE "{&core}grafix/prev-au.ico":U
     LABEL "out" 
     SIZE 8.2 BY 1.67 TOOLTIP "Step Out".

DEFINE BUTTON btn-secrep 
     LABEL "Security Report" 
     SIZE 23 BY 1.14.

DEFINE VARIABLE cb-group AS CHARACTER FORMAT "X(256)":U 
     LABEL "Menu Group" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 24.8 BY 1
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE cb-ugroup AS CHARACTER FORMAT "X(256)":U 
     LABEL "UGroup" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE cb-user AS CHARACTER FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82.8 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82.8 BY 6.67.

DEFINE VARIABLE lv-all AS LOGICAL INITIAL yes 
     LABEL "all" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .81 NO-UNDO.

DEFINE VARIABLE lv-seccopy AS LOGICAL INITIAL yes 
     LABEL "Copy Security to Program Record" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-Zen-Dmenu SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-Zen-Dmenu.menu-action FORMAT "x(40)":U WIDTH 39
      t-Zen-Dmenu.menu-name FORMAT "X(40)":U WIDTH 29.4
      t-Zen-Dmenu.menu-parent COLUMN-LABEL "Parent" WIDTH 28.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 100 BY 16.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     btn-out AT ROW 1.24 COL 61.6 NO-TAB-STOP 
     btn-in AT ROW 1.24 COL 72.4 NO-TAB-STOP 
     cb-user AT ROW 1.48 COL 110 COLON-ALIGNED WIDGET-ID 8
     cb-ugroup AT ROW 1.48 COL 155 COLON-ALIGNED WIDGET-ID 10
     cb-group AT ROW 1.62 COL 25 COLON-ALIGNED NO-TAB-STOP 
     lv-all AT ROW 1.81 COL 52 NO-TAB-STOP 
     br-maint AT ROW 3.1 COL 11 HELP
          "Select the record to edit."
     t-Zen-Dmenu.menu-id AT ROW 3.81 COL 133.4 COLON-ALIGNED
          LABEL "ID"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 20 
     t-Zen-Dmenu.menu-parent AT ROW 4.81 COL 133.4 COLON-ALIGNED
          LABEL "Parent"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     t-Zen-Dmenu.menu-name AT ROW 5.81 COL 133.4 COLON-ALIGNED
          LABEL "Menu Text" FORMAT "X(31)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     t-Zen-Dmenu.menu-action AT ROW 6.81 COL 133.4 COLON-ALIGNED
          LABEL "Run File" FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     t-Zen-Dmenu.display-order AT ROW 7.81 COL 133.4 COLON-ALIGNED FORMAT ">>>>9"
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     t-Zen-Dmenu.menu-grp AT ROW 8.81 COL 133.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     t-Zen-Dmenu.sysrecord AT ROW 9.81 COL 135.4
          LABEL "System Record"
          VIEW-AS TOGGLE-BOX
          SIZE 32 BY 1
     btn-secrep AT ROW 11.24 COL 163 WIDGET-ID 2
     t-Zen-Dmenu.not-users AT ROW 12.81 COL 133.4 COLON-ALIGNED
          LABEL "Not Run Users"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1 TOOLTIP "Enter a comma delimited list of user IDs that CANNOT run this program."
     t-Zen-Dmenu.run-users AT ROW 13.81 COL 133.4 COLON-ALIGNED
          LABEL "Can Run Users"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1 TOOLTIP "Enter a comma delimited list of user IDs that CAN run this program."
     t-Zen-Dmenu.not-groups AT ROW 14.81 COL 133.4 COLON-ALIGNED
          LABEL "Not Run Groups"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1 TOOLTIP "Enter a comma delimited list of user groups that CANNOT run this program."
     t-Zen-Dmenu.run-groups AT ROW 15.81 COL 133.4 COLON-ALIGNED
          LABEL "Can Run Groups"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1 TOOLTIP "Enter a comma delimited list of user groups that CAN run this program."
     t-Zen-Dmenu.menu-inputs AT ROW 16.81 COL 133.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1 TOOLTIP "Enter any required input parameters."
     lv-seccopy AT ROW 17.91 COL 135 WIDGET-ID 6
     "Security - (priority is in descending order)" VIEW-AS TEXT
          SIZE 39.8 BY .95 AT ROW 11.43 COL 115.2
     "Menu Item" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.62 COL 115.2
     RECT-35 AT ROW 3.43 COL 114.2
     RECT-7 AT ROW 12.43 COL 114.2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 197.6 BY 19.57
         TITLE "Menu Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-Zen-Dmenu T "?" NO-UNDO schadm Zen-Dmenu
      ADDITIONAL-FIELDS:
          field t-node-id  as int
          field t-to-level as log
          field t-from-id as int
      END-FIELDS.
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
         HEIGHT             = 24.38
         WIDTH              = 197.6
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
/* BROWSE-TAB br-maint lv-all frame-maint */
ASSIGN 
       br-maint:COLUMN-RESIZABLE IN FRAME frame-maint       = TRUE.

/* SETTINGS FOR BUTTON btn-out IN FRAME frame-maint
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-ugroup IN FRAME frame-maint
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cb-ugroup:HIDDEN IN FRAME frame-maint           = TRUE.

/* SETTINGS FOR COMBO-BOX cb-user IN FRAME frame-maint
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cb-user:HIDDEN IN FRAME frame-maint           = TRUE.

/* SETTINGS FOR FILL-IN t-Zen-Dmenu.display-order IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR TOGGLE-BOX lv-seccopy IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Dmenu.menu-action IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT                                   */
/* SETTINGS FOR FILL-IN t-Zen-Dmenu.menu-grp IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Dmenu.menu-id IN FRAME frame-maint
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN t-Zen-Dmenu.menu-inputs IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Dmenu.menu-name IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT                                   */
/* SETTINGS FOR FILL-IN t-Zen-Dmenu.menu-parent IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-Zen-Dmenu.not-groups IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-Zen-Dmenu.not-users IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-Zen-Dmenu.run-groups IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-Zen-Dmenu.run-users IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX t-Zen-Dmenu.sysrecord IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-Zen-Dmenu"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _OrdList          = "Temp-Tables.t-Zen-Dmenu.menu-parent|yes,Temp-Tables.t-Zen-Dmenu.display-order|yes"
     _Where[1]         = "Temp-Tables.t-Zen-Dmenu.menu-parent = lv-menuparent[lv-depth]"
     _FldNameList[1]   > Temp-Tables.t-Zen-Dmenu.menu-action
"t-Zen-Dmenu.menu-action" ? "x(40)" "" ? ? ? ? ? ? no ? no no "39" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-Zen-Dmenu.menu-name
"t-Zen-Dmenu.menu-name" ? "X(40)" "character" ? ? ? ? ? ? no ? no no "29.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.t-Zen-Dmenu.menu-parent
"t-Zen-Dmenu.menu-parent" "Parent" ? "character" ? ? ? ? ? ? no ? no no "28.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define BROWSE-NAME br-maint
&Scoped-define SELF-NAME br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-maint window-maint
ON ROW-DISPLAY OF br-maint IN FRAME frame-maint
DO:
   if not(
      t-zen-dmenu.menu-action matches "*~.p" or
      t-zen-dmenu.menu-action matches "*~.w" or
      t-zen-dmenu.menu-action = ""           or
      t-zen-dmenu.menu-action = "xxx")       or
      t-zen-dmenu.menu-action = "help"
   then assign
      lv-hColor:bgcolor = 10.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-in window-maint
ON CHOOSE OF btn-in IN FRAME frame-maint /* in */
DO:
    assign lv-depth = lv-depth + 1
           btn-out:sensitive = lv-depth > 1
           lv-menuparent[lv-depth] = t-{&table-name}.menu-action
           vc-parent-name = t-{&TABLE-NAME}.MENU-action
           lv-group = If lv-all:checked Then '*'
                                        Else getcombokey(cb-group:handle)
           cb-group:sensitive = false.
    
      RUN openquery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-out window-maint
ON CHOOSE OF btn-out IN FRAME frame-maint /* out */
DO:
    assign lv-depth = lv-depth - 1
           btn-out:sensitive = lv-depth > 1
           cb-group:sensitive = not btn-out:sensitive.
    lv-group = if btn-out:sensitive And lv-all:checked then '*'
                                    else getcombokey(cb-group:handle).
    RUN openquery.
    IF AVAIL t-{&table-name} THEN
    ASSIGN vc-parent-name = t-{&table-name}.menu-parent.
    ELSE ASSIGN vc-parent-name = ''.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-secrep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-secrep window-maint
ON CHOOSE OF btn-secrep IN FRAME frame-maint /* Security Report */
DO:
  runchild('{&core}{&reports}securityrepparam.w',this-procedure).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-group window-maint
ON VALUE-CHANGED OF cb-group IN FRAME frame-maint /* Menu Group */
DO:
  rUN openquery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-ugroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-ugroup window-maint
ON VALUE-CHANGED OF cb-ugroup IN FRAME frame-maint /* UGroup */
DO:
    rUN openquery.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-user window-maint
ON VALUE-CHANGED OF cb-user IN FRAME frame-maint /* User */
DO:
    rUN openquery.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-all window-maint
ON VALUE-CHANGED OF lv-all IN FRAME frame-maint /* all */
DO:
 
    RUN openquery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{{&core}commonmaint.i &path = "{&core}{&srv}"
                     &extraparams = "lv-group,lv-user,lv-ugroup,"}

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
def buffer b-menu for t-zen-dmenu.


t-{&table-name}.SysRecord:hidden in frame {&frame-name} = not lv-sysman .

 if lv-newmode then
    assign 
        t-{&table-name}.run-groups:screen-value in frame {&frame-name} = '*'
        t-{&table-name}.run-users:screen-value = '*'
        t-{&table-name}.menu-parent:SCREEN-VALUE = vc-parent-name.
        
    DISP t-Zen-Dmenu.menu-id WITH FRAME {&FRAME-NAME}.
    
 /* assign */
/*     btn-out:sensitive = can-find(first b-menu where b-menu.menu-action = t-zen-dmenu.menu-parent) */
/*     btn-in:sensitive = can-find(first b-menu where b-menu.menu-parent = t-zen-dmenu.menu-action). */
/*    */
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
  DISPLAY cb-group lv-all lv-seccopy 
      WITH FRAME frame-maint.
  IF AVAILABLE t-Zen-Dmenu THEN 
    DISPLAY t-Zen-Dmenu.menu-id t-Zen-Dmenu.menu-parent t-Zen-Dmenu.menu-name 
          t-Zen-Dmenu.menu-action t-Zen-Dmenu.display-order t-Zen-Dmenu.menu-grp 
          t-Zen-Dmenu.sysrecord t-Zen-Dmenu.not-users t-Zen-Dmenu.run-users 
          t-Zen-Dmenu.not-groups t-Zen-Dmenu.run-groups t-Zen-Dmenu.menu-inputs 
      WITH FRAME frame-maint.
  ENABLE RECT-35 RECT-7 btn-in cb-group lv-all br-maint btn-secrep 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-edit-trigger window-maint 
PROCEDURE local-edit-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    apply "entry" to t-zen-dmenu.menu-parent in frame frame-maint.
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
    /* find handle of action column in browse so we can change color of it later */
    lv-hColor = getcolumnhandle(br-maint:handle in frame frame-maint,'action').
    apply 'value-changed' to lv-all in frame {&frame-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-openquery window-maint 
PROCEDURE local-openquery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign
    lv-user = getcombokey(cb-user:handle in frame {&frame-name})
    lv-ugroup = getcombokey(cb-ugroup:handle)
    lv-group = If lv-all:checked then '*'
                                 else getcombokey(cb-group:handle).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pop-combos window-maint 
PROCEDURE pop-combos :
def var lv-desc as char no-undo.
assign
    cb-group:private-data in frame {&frame-name} = classcodes('{&menugroup}',lv-desc)
    cb-group:list-items = lv-desc.
    
buildcombo(cb-user:handle in frame {&frame-name},
               'zen-duser',
               'duser',
               'duser',
               'where true',
               'by duser',
               no,yes).
assign
    cb-ugroup:private-data = classcodes('{&usergrp}',lv-desc)
    cb-ugroup:list-items = lv-desc.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-save window-maint 
PROCEDURE post-save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 if not lv-seccopy then return.
 
{{&core}run.i 
      &program   = "zen-dpgm.p"
      &path      = "{&core}{&srv}"
      &Appsrv    = "System"
      &procedure = "SetSecurityFields"
      &params    = "(t-Zen-Dmenu.menu-action,                   
                    t-Zen-Dmenu.not-users,
                    t-Zen-Dmenu.not-groups,
                    t-Zen-Dmenu.run-groups,
                    t-Zen-Dmenu.run-users)"}
                   


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

/*     if can-find(first b-{&table-name}                                                                    */
/*                 where b-{&table-name}.duser = t-{&table-name}.duser:screen-value in frame {&frame-name}  */
/*                   and recid(b-{&table-name}) ne recid(t-{&table-name}))                                  */
/*     then do:                                                                                             */
/*         MESSAGE msg (120,"","","","") VIEW-AS ALERT-BOX.                                                 */
/*         return string(t-{&table-name}.duser:handle).                                                     */
/*     end.                                                                                                 */

  
  return 'passed'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

