&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          centrec          PROGRESS
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-dpgm NO-UNDO LIKE zen-dpgm.



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

&glob tabs true
&glob title-text Program Details
&glob table-name zen-dpgm
&glob unique-key    {&table-name}TableId
&glob KeepRefreshButton
/* ***************************  Definitions  ************************** */
{app-paths.i}
&Glob ImmediateQuery
&glob getallonquery
&glob UseDBButtons
&glob nochangedcheck

define temp-table t-table no-undo like zen-dpgm.
&glob printoptable ,output t-table

/* Parameters Definitions ---                                           */
&glob NoPrint           /* no print button */
&glob nocrystal
/* Local var Definitions ---                                            */
   def var lv-sysman     as  log no-undo.
lv-sysman = systemmanager(GetUserID()).

def var lv-defbuts as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME dpgm-frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-zen-dpgm

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 t-zen-dpgm.pgm t-zen-dpgm.name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH t-zen-dpgm NO-LOCK ~
    BY t-zen-dpgm.pgm
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH t-zen-dpgm NO-LOCK ~
    BY t-zen-dpgm.pgm.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 t-zen-dpgm
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 t-zen-dpgm


/* Definitions for FRAME dpgm-frame                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-dpgm-frame ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Definitions for FRAME six                                            */
&Scoped-define FIELDS-IN-QUERY-six t-zen-dpgm.ButtonList 
&Scoped-define QUERY-STRING-six FOR EACH t-zen-dpgm SHARE-LOCK
&Scoped-define OPEN-QUERY-six OPEN QUERY six FOR EACH t-zen-dpgm SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-six t-zen-dpgm
&Scoped-define FIRST-TABLE-IN-QUERY-six t-zen-dpgm


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 lv-pgm btn-wid btn-prop 
&Scoped-Define DISPLAYED-OBJECTS lv-pgm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 t-zen-dpgm.pgm t-zen-dpgm.name t-zen-dpgm.menu-grp ~
t-zen-dpgm.win-x t-zen-dpgm.win-y t-zen-dpgm.sysrecord ~
t-zen-dpgm.UseDefaults t-zen-dpgm.multiinstance t-zen-dpgm.created ~
t-zen-dpgm.author t-zen-dpgm.shortcut t-zen-dpgm.dataguess t-zen-dpgm.type 
&Scoped-define List-2 t-zen-dpgm.not-users t-zen-dpgm.run-users ~
t-zen-dpgm.not-groups t-zen-dpgm.run-groups t-zen-dpgm.noteditusers ~
t-zen-dpgm.editusers t-zen-dpgm.noteditgroups t-zen-dpgm.editgroups 
&Scoped-define List-3 t-zen-dpgm.comments t-zen-dpgm.icon ~
t-zen-dpgm.help-file t-zen-dpgm.help-ref 
&Scoped-define List-4 t-zen-dpgm.AsyncReport t-zen-dpgm.RepTitle cb-reptype ~
t-zen-dpgm.RepPath t-zen-dpgm.RepProg t-zen-dpgm.RepProc ~
t-zen-dpgm.CrystalPath t-zen-dpgm.cryfilename t-zen-dpgm.ExtractFilePath ~
t-zen-dpgm.extractfilename t-zen-dpgm.Params 
&Scoped-define List-5 lv-description 
&Scoped-define List-6 t-zen-dpgm.ButtonList 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-prop 
     LABEL "Properties":L 
     SIZE 15.8 BY 1.1.

DEFINE BUTTON btn-wid 
     LABEL "Widgets":L 
     SIZE 15.8 BY 1.1.

DEFINE VARIABLE lv-pgm AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Program" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-description AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 92 BY 8.33 NO-UNDO.

DEFINE VARIABLE cb-reptype AS CHARACTER FORMAT "X(256)":U 
     LABEL "Report Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 64 BY 1 NO-UNDO.

DEFINE BUTTON btn-in 
     LABEL ">>>>" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "Ok" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-out 
     LABEL "<<<<" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE sl-from AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 30 BY 5.95 NO-UNDO.

DEFINE VARIABLE sl-to AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 45 BY 5.71 NO-UNDO.

DEFINE VARIABLE lv-overlay AS LOGICAL INITIAL no 
     LABEL "Overlay" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .76 NO-UNDO.

DEFINE VARIABLE lv-sensitive AS LOGICAL INITIAL no 
     LABEL "Sensitive" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .86 NO-UNDO.

DEFINE VARIABLE lv-visible AS LOGICAL INITIAL no 
     LABEL "Visible" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      t-zen-dpgm SCROLLING.

DEFINE QUERY six FOR 
      t-zen-dpgm SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 window-maint _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      t-zen-dpgm.pgm COLUMN-LABEL "Id" FORMAT "x(50)":U
      t-zen-dpgm.name FORMAT "x(50)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 115 BY 4.52 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME dpgm-frame
     BROWSE-2 AT ROW 2.43 COL 11 WIDGET-ID 200
     lv-pgm AT ROW 1.29 COL 20.6 COLON-ALIGNED
     btn-wid AT ROW 1.29 COL 94.2 HELP
          "Widgets"
     btn-prop AT ROW 1.24 COL 111 HELP
          "Widgets" WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 129.2 BY 19.86
         TITLE "Program Details".

DEFINE FRAME three
     t-zen-dpgm.comments AT ROW 1.48 COL 16 NO-LABEL
          VIEW-AS EDITOR
          SIZE 92 BY 6.91
     t-zen-dpgm.icon AT ROW 8.91 COL 14.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     t-zen-dpgm.help-file AT ROW 9.91 COL 14.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     t-zen-dpgm.help-ref AT ROW 10.91 COL 14.2 COLON-ALIGNED
          LABEL "Help Ref #"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1 TOOLTIP "Enter the help reference number that matches the help manual context number."
     "Comments:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.48 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14 ROW 8.33 SCROLLABLE .

DEFINE FRAME four
     t-zen-dpgm.AsyncReport AT ROW 1.24 COL 68.6 WIDGET-ID 14
          VIEW-AS TOGGLE-BOX
          SIZE 20.6 BY 1
     t-zen-dpgm.RepTitle AT ROW 1.24 COL 23.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     cb-reptype AT ROW 2.19 COL 23.4 COLON-ALIGNED WIDGET-ID 12
     t-zen-dpgm.RepPath AT ROW 3.14 COL 23.4 COLON-ALIGNED FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 81.8 BY 1
     t-zen-dpgm.RepProg AT ROW 4.1 COL 23.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52 BY 1.05
     t-zen-dpgm.RepProc AT ROW 5.1 COL 23.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-dpgm.CrystalPath AT ROW 6.1 COL 23.4 COLON-ALIGNED WIDGET-ID 6 FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 83 BY 1
     t-zen-dpgm.cryfilename AT ROW 7.1 COL 23.4 COLON-ALIGNED FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 83 BY 1
     t-zen-dpgm.ExtractFilePath AT ROW 8.1 COL 23.4 COLON-ALIGNED WIDGET-ID 8 FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 83 BY 1
     t-zen-dpgm.extractfilename AT ROW 9.1 COL 25.4 NO-LABEL
          VIEW-AS EDITOR MAX-CHARS 255
          SIZE 83.6 BY 1.81
     t-zen-dpgm.Params AT ROW 11 COL 6.8 WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 83 BY 1
     "Extract File Names:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.1 COL 6.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 12 ROW 8.33 SCROLLABLE .

DEFINE FRAME six
     t-zen-dpgm.ButtonList AT ROW 1.1 COL 13 HELP
          "comma delim string of ButtonName^SensitiveVisibleOverlay" NO-LABEL WIDGET-ID 4
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 96 BY 2.14 TOOLTIP "ButtonName^SensitiveVisibleOverlay"
     sl-from AT ROW 5.62 COL 7 NO-LABEL WIDGET-ID 36
     btn-in AT ROW 5.62 COL 40 WIDGET-ID 44
     btn-out AT ROW 6.81 COL 40 WIDGET-ID 46
     sl-to AT ROW 5.62 COL 60 NO-LABEL WIDGET-ID 38
     lv-sensitive AT ROW 8 COL 40 WIDGET-ID 50
     lv-visible AT ROW 8.71 COL 40 WIDGET-ID 52
     lv-overlay AT ROW 9.38 COL 40 WIDGET-ID 54
     btn-ok AT ROW 10.24 COL 42 WIDGET-ID 48
     "The bit below allows quick changing of buttons select required buttons" VIEW-AS TEXT
          SIZE 73 BY .62 AT ROW 3.48 COL 4 WIDGET-ID 56
     "Available Common Buttons" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 4.91 COL 7 WIDGET-ID 58
     "Buttons Used By Screen" VIEW-AS TEXT
          SIZE 45 BY .62 AT ROW 4.81 COL 60 WIDGET-ID 60
     "Hit ok to put changes in button field and then save changes" VIEW-AS TEXT
          SIZE 62 BY .62 AT ROW 4.1 COL 4 WIDGET-ID 62
     "Buttons:" VIEW-AS TEXT
          SIZE 9 BY .81 AT ROW 1.14 COL 4 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13.6 ROW 8.52 SCROLLABLE  WIDGET-ID 100.

DEFINE FRAME two
     t-zen-dpgm.not-users AT ROW 1.48 COL 30 COLON-ALIGNED
          LABEL "Users Who Cannot Run"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-dpgm.run-users AT ROW 2.48 COL 30 COLON-ALIGNED
          LABEL "Users Who Can Run"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-dpgm.not-groups AT ROW 3.48 COL 30 COLON-ALIGNED
          LABEL "Groups That Cannot Run"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-dpgm.run-groups AT ROW 4.48 COL 30 COLON-ALIGNED
          LABEL "Groups That Can Run"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-dpgm.noteditusers AT ROW 6.24 COL 30 COLON-ALIGNED
          LABEL "Users Who Cannot Edit"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-dpgm.editusers AT ROW 7.24 COL 30 COLON-ALIGNED
          LABEL "Users Who Can Edit"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-dpgm.noteditgroups AT ROW 8.24 COL 30 COLON-ALIGNED
          LABEL "Groups That Cannot Edit"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     t-zen-dpgm.editgroups AT ROW 9.24 COL 30 COLON-ALIGNED
          LABEL "Groups That Can Edit"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13 ROW 8.91 SCROLLABLE .

DEFINE FRAME five
     lv-description AT ROW 2.19 COL 3 NO-LABEL
     "Description" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 1.1 COL 3.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13.8 ROW 8.91 SCROLLABLE .

DEFINE FRAME one
     t-zen-dpgm.pgm AT ROW 1 COL 17 COLON-ALIGNED
          LABEL "File Name" FORMAT "x(50)"
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     t-zen-dpgm.name AT ROW 2 COL 17 COLON-ALIGNED
          LABEL "Screen Name"
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     t-zen-dpgm.menu-grp AT ROW 3 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     t-zen-dpgm.win-x AT ROW 8 COL 86.6 COLON-ALIGNED
          LABEL "X Position"
          VIEW-AS FILL-IN 
          SIZE 8.4 BY 1
     t-zen-dpgm.win-y AT ROW 9 COL 86.6 COLON-ALIGNED
          LABEL "Y Position"
          VIEW-AS FILL-IN 
          SIZE 8.4 BY 1
     t-zen-dpgm.sysrecord AT ROW 8.14 COL 19
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .67
     t-zen-dpgm.UseDefaults AT ROW 8.76 COL 19
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .71
     t-zen-dpgm.multiinstance AT ROW 9.38 COL 19
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .71
     t-zen-dpgm.created AT ROW 5 COL 17.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     t-zen-dpgm.author AT ROW 6 COL 17.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     t-zen-dpgm.shortcut AT ROW 7 COL 17.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     t-zen-dpgm.dataguess AT ROW 5 COL 86.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     t-zen-dpgm.type AT ROW 6 COL 86.4 COLON-ALIGNED WIDGET-ID 4 FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1 TOOLTIP "Program Type"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13.6 ROW 8.52 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-dpgm T "?" NO-UNDO centrec zen-dpgm
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
         HEIGHT             = 20.19
         WIDTH              = 131.8
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
/* REPARENT FRAME */
ASSIGN FRAME five:FRAME = FRAME dpgm-frame:HANDLE
       FRAME four:FRAME = FRAME dpgm-frame:HANDLE
       FRAME one:FRAME = FRAME dpgm-frame:HANDLE
       FRAME six:FRAME = FRAME dpgm-frame:HANDLE
       FRAME three:FRAME = FRAME dpgm-frame:HANDLE
       FRAME two:FRAME = FRAME dpgm-frame:HANDLE.

/* SETTINGS FOR FRAME dpgm-frame
   NOT-VISIBLE FRAME-NAME Custom                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME three:MOVE-AFTER-TAB-ITEM (btn-prop:HANDLE IN FRAME dpgm-frame)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BROWSE-2 1 dpgm-frame */
ASSIGN 
       FRAME dpgm-frame:HIDDEN           = TRUE
       FRAME dpgm-frame:PRIVATE-DATA     = 
                "6".

/* SETTINGS FOR FRAME five
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME five:SCROLLABLE       = FALSE
       FRAME five:HIDDEN           = TRUE
       FRAME five:PRIVATE-DATA     = 
                "5".

/* SETTINGS FOR EDITOR lv-description IN FRAME five
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FRAME four
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME four:SCROLLABLE       = FALSE
       FRAME four:HIDDEN           = TRUE
       FRAME four:PRIVATE-DATA     = 
                "4".

/* SETTINGS FOR TOGGLE-BOX t-zen-dpgm.AsyncReport IN FRAME four
   NO-ENABLE 4                                                          */
/* SETTINGS FOR COMBO-BOX cb-reptype IN FRAME four
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.cryfilename IN FRAME four
   NO-ENABLE 4 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN t-zen-dpgm.CrystalPath IN FRAME four
   NO-ENABLE 4 EXP-FORMAT                                               */
/* SETTINGS FOR EDITOR t-zen-dpgm.extractfilename IN FRAME four
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.ExtractFilePath IN FRAME four
   NO-ENABLE 4 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN t-zen-dpgm.Params IN FRAME four
   NO-ENABLE ALIGN-L 4                                                  */
/* SETTINGS FOR FILL-IN t-zen-dpgm.RepPath IN FRAME four
   NO-ENABLE 4 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN t-zen-dpgm.RepProc IN FRAME four
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.RepProg IN FRAME four
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.RepTitle IN FRAME four
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FRAME one
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME one:SCROLLABLE       = FALSE
       FRAME one:HIDDEN           = TRUE
       FRAME one:PRIVATE-DATA     = 
                "1".

/* SETTINGS FOR FILL-IN t-zen-dpgm.author IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.created IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.dataguess IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.menu-grp IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX t-zen-dpgm.multiinstance IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.name IN FRAME one
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN t-zen-dpgm.pgm IN FRAME one
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN t-zen-dpgm.shortcut IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX t-zen-dpgm.sysrecord IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.type IN FRAME one
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR TOGGLE-BOX t-zen-dpgm.UseDefaults IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.win-x IN FRAME one
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.win-y IN FRAME one
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FRAME six
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME six:SCROLLABLE       = FALSE
       FRAME six:HIDDEN           = TRUE
       FRAME six:PRIVATE-DATA     = 
                "6".

/* SETTINGS FOR EDITOR t-zen-dpgm.ButtonList IN FRAME six
   NO-ENABLE 6 EXP-HELP                                                 */
/* SETTINGS FOR FRAME three
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME three:SCROLLABLE       = FALSE
       FRAME three:HIDDEN           = TRUE
       FRAME three:PRIVATE-DATA     = 
                "3".

/* SETTINGS FOR EDITOR t-zen-dpgm.comments IN FRAME three
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.help-file IN FRAME three
   3                                                                    */
/* SETTINGS FOR FILL-IN t-zen-dpgm.help-ref IN FRAME three
   3 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.icon IN FRAME three
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FRAME two
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME two:SCROLLABLE       = FALSE
       FRAME two:HIDDEN           = TRUE
       FRAME two:PRIVATE-DATA     = 
                "2".

/* SETTINGS FOR FILL-IN t-zen-dpgm.editgroups IN FRAME two
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN t-zen-dpgm.editusers IN FRAME two
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN t-zen-dpgm.not-groups IN FRAME two
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN t-zen-dpgm.not-users IN FRAME two
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN t-zen-dpgm.noteditgroups IN FRAME two
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN t-zen-dpgm.noteditusers IN FRAME two
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN t-zen-dpgm.run-groups IN FRAME two
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN t-zen-dpgm.run-users IN FRAME two
   NO-ENABLE 2 EXP-LABEL                                                */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "Temp-Tables.t-zen-dpgm"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.t-zen-dpgm.pgm|yes"
     _FldNameList[1]   > Temp-Tables.t-zen-dpgm.pgm
"t-zen-dpgm.pgm" "Id" "x(50)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-dpgm.name
"t-zen-dpgm.name" ? "x(50)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME dpgm-frame
/* Query rebuild information for FRAME dpgm-frame
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME dpgm-frame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME four
/* Query rebuild information for FRAME four
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME four */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME one
/* Query rebuild information for FRAME one
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME one */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME six
/* Query rebuild information for FRAME six
     _TblList          = "Temp-Tables.t-zen-dpgm"
     _Query            is NOT OPENED
*/  /* FRAME six */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME three
/* Query rebuild information for FRAME three
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME three */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME two
/* Query rebuild information for FRAME two
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME two */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME dpgm-frame:HANDLE
       ROW             = 7.19
       COLUMN          = 11
       HEIGHT          = 12.62
       WIDTH           = 114
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      CtrlFrame:MOVE-AFTER(FRAME three:HANDLE).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define FRAME-NAME six
&Scoped-define SELF-NAME btn-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-in window-maint
ON CHOOSE OF btn-in IN FRAME six /* >>>> */
DO:
  run moveentries(sl-from:handle,sl-to:handle).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok window-maint
ON CHOOSE OF btn-ok IN FRAME six /* Ok */
DO:
  run process-selected-items.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-out window-maint
ON CHOOSE OF btn-out IN FRAME six /* <<<< */
DO:
    run moveentries(sl-to:handle,sl-from:handle).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME dpgm-frame
&Scoped-define SELF-NAME btn-prop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-prop window-maint
ON CHOOSE OF btn-prop IN FRAME dpgm-frame /* Properties */
DO:
    if avail {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} 
        then
        runchild("{&core}property-mnt.w",this-procedure).
    else
       message 'Please Select a Program First' {&mess-disp-type}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-wid window-maint
ON CHOOSE OF btn-wid IN FRAME dpgm-frame /* Widgets */
DO:
    if avail {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} 
        then
        runchild("{&core}wid-mnt.w",this-procedure).
    else
       message 'Please Select a Program First' {&mess-disp-type}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME four
&Scoped-define SELF-NAME cb-reptype
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-reptype window-maint
ON VALUE-CHANGED OF cb-reptype IN FRAME four /* Report Type */
DO:
   run setreporttype (self).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME dpgm-frame
&Scoped-define SELF-NAME lv-pgm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-pgm window-maint
ON ANY-KEY OF lv-pgm IN FRAME dpgm-frame /* Program */
DO:
  apply lastkey.                    
  lv-pgm = lv-pgm:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  run openquery.
  return no-apply.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */
{{&core}sl-maint.i &sldelim = ,}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
PAUSE 0 BEFORE-HIDE.
{{&core}commonmaint.i &path = "{&core}{&srv}"
                     &extraparams = "lv-pgm,"} 
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {{&core}sec-chk.i}
  RUN enable_UI.
  {{&core}wid-chk.i}
  {{&core}focus.i}

assign sl-to:delimiter in frame six = ','
       sl-from:delimiter = ','.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load window-maint  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "dpgm-mnt.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "dpgm-mnt.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  HIDE FRAME dpgm-frame.
  HIDE FRAME five.
  HIDE FRAME four.
  HIDE FRAME one.
  HIDE FRAME six.
  HIDE FRAME three.
  HIDE FRAME two.
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
def var x as int no-undo.

assign
   sl-to:list-items in frame six = ' '.
   sl-from:list-items            = ' '.

t-{&table-name}.SysRecord:hidden in frame one = not lv-sysman.

if lv-newmode then
    assign 
        t-zen-dpgm.author:screen-value in frame one = GetUserID()
        t-zen-dpgm.created:screen-value             = string(today).

if avail t-zen-dpgm then do:
   lv-description:screen-value in frame five = t-zen-dpgm.description.
   cb-reptype  = setcombovalue(t-zen-dpgm.ReportRunType,cb-reptype:handle in frame four).
   apply 'value-changed' to cb-reptype.
end.

do x = 1 to num-entries(t-zen-dpgm.ButtonList,","):
    sl-to:add-last(entry(x,t-zen-dpgm.ButtonList,",")).
end.

do x = 1 to num-entries(lv-defbuts,","):
   if not can-do(sl-to:list-items,entry(x,lv-defbuts,","))
   then sl-from:add-last(entry(x,lv-defbuts,",")).
end.

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
  RUN control_load.
  DISPLAY lv-pgm 
      WITH FRAME dpgm-frame.
  ENABLE BROWSE-2 lv-pgm btn-wid btn-prop 
      WITH FRAME dpgm-frame.
  {&OPEN-BROWSERS-IN-QUERY-dpgm-frame}
  DISPLAY cb-reptype 
      WITH FRAME four.
  IF AVAILABLE t-zen-dpgm THEN 
    DISPLAY t-zen-dpgm.AsyncReport t-zen-dpgm.RepTitle t-zen-dpgm.RepPath 
          t-zen-dpgm.RepProg t-zen-dpgm.RepProc t-zen-dpgm.CrystalPath 
          t-zen-dpgm.cryfilename t-zen-dpgm.ExtractFilePath 
          t-zen-dpgm.extractfilename t-zen-dpgm.Params 
      WITH FRAME four.
  {&OPEN-BROWSERS-IN-QUERY-four}
  IF AVAILABLE t-zen-dpgm THEN 
    DISPLAY t-zen-dpgm.comments t-zen-dpgm.icon t-zen-dpgm.help-file 
          t-zen-dpgm.help-ref 
      WITH FRAME three.
  ENABLE t-zen-dpgm.help-file t-zen-dpgm.help-ref 
      WITH FRAME three.
  {&OPEN-BROWSERS-IN-QUERY-three}
  IF AVAILABLE t-zen-dpgm THEN 
    DISPLAY t-zen-dpgm.pgm t-zen-dpgm.name t-zen-dpgm.menu-grp t-zen-dpgm.win-x 
          t-zen-dpgm.win-y t-zen-dpgm.sysrecord t-zen-dpgm.UseDefaults 
          t-zen-dpgm.multiinstance t-zen-dpgm.created t-zen-dpgm.author 
          t-zen-dpgm.shortcut t-zen-dpgm.dataguess t-zen-dpgm.type 
      WITH FRAME one.
  ENABLE t-zen-dpgm.win-x t-zen-dpgm.win-y 
      WITH FRAME one.
  {&OPEN-BROWSERS-IN-QUERY-one}
  DISPLAY sl-from sl-to lv-sensitive lv-visible lv-overlay 
      WITH FRAME six.
  IF AVAILABLE t-zen-dpgm THEN 
    DISPLAY t-zen-dpgm.ButtonList 
      WITH FRAME six.
  ENABLE sl-from btn-in btn-out sl-to lv-sensitive lv-visible lv-overlay btn-ok 
      WITH FRAME six.
  {&OPEN-BROWSERS-IN-QUERY-six}
  IF AVAILABLE t-zen-dpgm THEN 
    DISPLAY t-zen-dpgm.not-users t-zen-dpgm.run-users t-zen-dpgm.not-groups 
          t-zen-dpgm.run-groups t-zen-dpgm.noteditusers t-zen-dpgm.editusers 
          t-zen-dpgm.noteditgroups t-zen-dpgm.editgroups 
      WITH FRAME two.
  {&OPEN-BROWSERS-IN-QUERY-two}
  DISPLAY lv-description 
      WITH FRAME five.
  {&OPEN-BROWSERS-IN-QUERY-five}
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
    t-zen-dpgm.description = lv-description:screen-value in frame five.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls window-maint 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign
    h-tab   = chctrlframe:tabstrip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-afterinitialise window-maint 
PROCEDURE local-afterinitialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

assign sl-to:delimiter in frame six = ','
       sl-from:delimiter = ','.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-crystal-trigger window-maint 
PROCEDURE local-crystal-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* return '*{&delim2}Author'.  */
return ''.
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
def var lv-scratch as char no-undo.
lv-defbuts = GetCtrl('DefButList').
cb-reptype:private-data in frame four = classcodes('ReportType',output lv-scratch).
cb-reptype:list-items = lv-scratch.

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
    t-zen-Dpgm.win-x:screen-value in frame one = '1'
    t-zen-Dpgm.win-y:screen-value = '93'
    sl-from:list-items in frame six = lv-defbuts
    btn-wid:sensitive in frame {&frame-name} = false. 
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
btn-wid:sensitive in frame {&frame-name} = true.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-child-procedures window-maint 
PROCEDURE local-update-child-procedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-to as handle no-undo.

if avail T-zen-dpgm then
case pv-to:name:
    when "{&core}wid-mnt.w" 
      then run refresh in pv-to (T-zen-dpgm.pgm:screen-value in frame one).
    when "{&core}property-mnt.w"
      then run refresh in pv-to ('Program',T-zen-dpgm.pgm:screen-value in frame one).

end case.


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
btn-wid:sensitive in frame {&frame-name} = true.

t-zen-dpgm.ReportRunType = GetCombokey(cb-reptype:handle in frame four).
t-zen-dpgm.UseCystal     = cb-reptype:screen-value matches "*crystal*".
   return "passed".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Process-Selected-Items window-maint 
PROCEDURE Process-Selected-Items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if t-zen-dpgm.ButtonList:sensitive in frame six then
   t-zen-dpgm.ButtonList:screen-value in frame six = sl-to:list-items.
else message 'not in edit mode'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetReportType window-maint 
PROCEDURE SetReportType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-combo as handle no-undo.
   def var lv-enablelist as char no-undo.
   case getcombokey(pv-combo):
      when 'browse' then do: 
         lv-enablelist = 't-zen-dpgm.RepTitle,cb-reptype'.
         if lv-Newmode then
            assign t-zen-dpgm.RepPath:screen-value in frame four = ''
                   t-zen-dpgm.RepProg:screen-value = ''
                   t-zen-dpgm.RepProc:screen-value = ''
                   t-zen-dpgm.CrystalPath:screen-value = ''
                   t-zen-dpgm.cryfilename:screen-value = ''
                   t-zen-dpgm.ExtractFilePath:screen-value = ""
                   t-zen-dpgm.extractfilename:screen-value = ''.
         t-zen-dpgm.UseCystal = false.
      end.
      when 'report' then do:
         lv-enablelist = 't-zen-dpgm.RepTitle,cb-reptype,t-zen-dpgm.RepPath,t-zen-dpgm.RepProg,t-zen-dpgm.RepProc'.
         if lv-Newmode then
            assign t-zen-dpgm.RepPath:screen-value in frame four = '{&server}{&srv}'
                   t-zen-dpgm.RepProg:screen-value = '????.p'
                   t-zen-dpgm.RepProc:screen-value = 'print-table'
                   t-zen-dpgm.CrystalPath:screen-value = ''
                   t-zen-dpgm.cryfilename:screen-value = ''
                   t-zen-dpgm.ExtractFilePath:screen-value = ""
                   t-zen-dpgm.extractfilename:screen-value = ''.
         t-zen-dpgm.UseCystal = false.
      end.
      when 'table' then do:
         lv-enablelist = "t-zen-dpgm.RepTitle,cb-reptype,t-zen-dpgm.RepPath,t-zen-dpgm.RepProg,t-zen-dpgm.RepProc,
                    t-zen-dpgm.CrystalPath,t-zen-dpgm.cryfilename,t-zen-dpgm.ExtractFilePath, 
                    t-zen-dpgm.extractfilename,t-zen-dpgm.Params".
         if lv-Newmode then
            assign t-zen-dpgm.RepPath:screen-value = '{&reports}{&srv}'
                   t-zen-dpgm.RepProg:screen-value = '????-extract.p'
                   t-zen-dpgm.RepProc:screen-value = 'ÉxtractData'
                   t-zen-dpgm.CrystalPath:screen-value = '{&crystal}'
                   t-zen-dpgm.cryfilename:screen-value = '????.rpt'
                   t-zen-dpgm.ExtractFilePath:screen-value = GetCtrl("{&Data-Extract-Dir}")
                   t-zen-dpgm.extractfilename:screen-value = '????.txt'.
         t-zen-dpgm.UseCystal = true.
      end.
      when 'xml' then do:
         lv-enablelist = "t-zen-dpgm.RepTitle,cb-reptype,t-zen-dpgm.RepPath,t-zen-dpgm.RepProg,t-zen-dpgm.RepProc,
                    t-zen-dpgm.CrystalPath,t-zen-dpgm.cryfilename,t-zen-dpgm.ExtractFilePath, 
                    t-zen-dpgm.extractfilename,t-zen-dpgm.Params".
         if lv-Newmode then
            assign t-zen-dpgm.RepPath:screen-value = '{&reports}{&srv}'
                   t-zen-dpgm.RepProg:screen-value = '????.p'
                   t-zen-dpgm.RepProc:screen-value = ''
                   t-zen-dpgm.CrystalPath:screen-value = '{&crystal}'
                   t-zen-dpgm.cryfilename:screen-value = '????.rpt'
                   t-zen-dpgm.ExtractFilePath:screen-value = GetCtrl("{&Data-Extract-Dir}")
                   t-zen-dpgm.extractfilename:screen-value = '????.???'.
         t-zen-dpgm.UseCystal = true.
      end.
   end case.

setsensitive(false,'exc',lv-enablelist,frame four:handle).
setsensitive(true,'ínc',lv-enablelist,frame four:handle).
pv-combo:modified = true.

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

 IF NEW t-{&Table-name} THEN
      IF CAN-FIND(FIRST b-{&Table-name} 
                        WHERE b-{&Table-name}.pgm = t-{&Table-name}.pgm) THEN
      DO:
          MESSAGE msg (120,"","","","") VIEW-AS ALERT-BOX.
          RETURN STRING(t-{&Table-name}.pgm:HANDLE in frame one).
      END.

  return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

