&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME win-main


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-dpgm NO-UNDO LIKE zen-dpgm.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS win-main 
/******************************************************************************/
/*  PROGRAM ID.     : ????????                                                */
/*  PROGRAM TITLE   : ????????                                                */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    : Zennor Computing LTD                                    */
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{app-paths.i}
&glob title-text About       /* message box title */
&glob Table-name zen-dpgm
&glob Unique-key {&table-name}tableid
&glob NoChangedCheck    /* disable changed onleave check */
&glob NoExitCheck       /* disble exit question */
&glob NoButtons         /* no buttons displayed */
&glob about
/* ***************************  Definitions  ************************** */
&glob suppresswindow
def var v-w    as handle no-undo.
def var v-prog as char   no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-zen-dpgm

/* Definitions for FRAME f-main                                         */
&Scoped-define FIELDS-IN-QUERY-f-main t-zen-dpgm.name t-zen-dpgm.created ~
t-zen-dpgm.author t-zen-dpgm.pgm t-zen-dpgm.comments 
&Scoped-define ENABLED-FIELDS-IN-QUERY-f-main t-zen-dpgm.comments 
&Scoped-define ENABLED-TABLES-IN-QUERY-f-main t-zen-dpgm
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-f-main t-zen-dpgm
&Scoped-define QUERY-STRING-f-main FOR EACH t-zen-dpgm NO-LOCK
&Scoped-define OPEN-QUERY-f-main OPEN QUERY f-main FOR EACH t-zen-dpgm NO-LOCK.
&Scoped-define TABLES-IN-QUERY-f-main t-zen-dpgm
&Scoped-define FIRST-TABLE-IN-QUERY-f-main t-zen-dpgm


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS t-zen-dpgm.comments 
&Scoped-define ENABLED-TABLES t-zen-dpgm
&Scoped-define FIRST-ENABLED-TABLE t-zen-dpgm
&Scoped-Define ENABLED-OBJECTS IMAGE-1 RECT-14 Btn_OK 
&Scoped-Define DISPLAYED-FIELDS t-zen-dpgm.name t-zen-dpgm.created ~
t-zen-dpgm.author t-zen-dpgm.pgm t-zen-dpgm.comments 
&Scoped-define DISPLAYED-TABLES t-zen-dpgm
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-dpgm
&Scoped-Define DISPLAYED-OBJECTS lv-company vc-version lv-system ~
lv-proversion 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 t-zen-dpgm.name t-zen-dpgm.created t-zen-dpgm.author ~
t-zen-dpgm.pgm t-zen-dpgm.comments 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 12.8 BY 1.19 TOOLTIP "OK"
     BGCOLOR 8 .

DEFINE VARIABLE lv-company AS CHARACTER FORMAT "X(256)":U 
     LABEL "Company" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE lv-proversion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Prover" 
      VIEW-AS TEXT 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE lv-system AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 44.8 BY 1 NO-UNDO.

DEFINE VARIABLE vc-version AS CHARACTER FORMAT "X(256)":U 
     LABEL "Version" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 45 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "{&core}grafix/zen.ico":U
     SIZE 5.6 BY 1.33.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100.6 BY 7.1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY f-main FOR 
      t-zen-dpgm SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     lv-company AT ROW 1.48 COL 22.6 COLON-ALIGNED
     vc-version AT ROW 2.62 COL 22.6 COLON-ALIGNED
     t-zen-dpgm.name AT ROW 3.76 COL 22.6 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 45 BY 1
     t-zen-dpgm.created AT ROW 4.86 COL 22.6 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 16 BY 1
     t-zen-dpgm.author AT ROW 5.95 COL 22.6 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 45 BY 1
     t-zen-dpgm.pgm AT ROW 6.95 COL 22.6 COLON-ALIGNED HELP
          "Physical Program name"
          LABEL "Called By" FORMAT "x(255)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 79.4 BY 1
     t-zen-dpgm.comments AT ROW 8.62 COL 8 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
          SIZE 94 BY 5.48
     Btn_OK AT ROW 14.57 COL 89
     lv-system AT ROW 14.57 COL 10 NO-LABEL
     lv-proversion AT ROW 14.57 COL 74 COLON-ALIGNED WIDGET-ID 2
     IMAGE-1 AT ROW 14.33 COL 3
     RECT-14 AT ROW 1.24 COL 4.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.6 BY 15.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-dpgm T "?" NO-UNDO paris zen-dpgm
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW win-main ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 83.8
         ROW                = 5.52
         HEIGHT             = 15.71
         WIDTH              = 107.2
         MAX-HEIGHT         = 15.71
         MAX-WIDTH          = 112
         VIRTUAL-HEIGHT     = 15.71
         VIRTUAL-WIDTH      = 112
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN win-main = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN t-zen-dpgm.author IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR EDITOR t-zen-dpgm.comments IN FRAME f-main
   1                                                                    */
ASSIGN 
       t-zen-dpgm.comments:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR FILL-IN t-zen-dpgm.created IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-company IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-proversion IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-system IN FRAME f-main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN t-zen-dpgm.name IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.pgm IN FRAME f-main
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT EXP-HELP                            */
/* SETTINGS FOR FILL-IN vc-version IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _TblList          = "Temp-Tables.t-zen-dpgm"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK win-main
ON CHOOSE OF Btn_OK IN FRAME f-main /* OK */
DO:
/*   btnhelp(this-procedure,no). */
  run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 win-main
ON MOUSE-MENU-CLICK OF IMAGE-1 IN FRAME f-main
DO:
/*    run displayme in h-tpro. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 win-main
ON MOUSE-SELECT-DBLCLICK OF IMAGE-1 IN FRAME f-main
DO:
    sysmsg('Dumping Procedure Calls').
    run {&core}proccalls.p.
    sysmsg('off').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK win-main 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */

ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/* main core logic */
{{&core}commonmaint.i &path = "{&sys}{&srv}"}
/* &extraparams  = "whatever you want,"}*/

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {{&core}sec-chk.i}   /* screen security check */
    RUN enable_UI.
   {{&core}wid-chk.i}   /* widgetlevel security check */
   {{&core}focus.i}     /* set focus to first enabled widget */
IF NOT THIS-PROCEDURE:PERSISTENT OR SESSION:DISPLAY-TYPE ne "GUI":U then
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI win-main  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI win-main  _DEFAULT-ENABLE
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
  DISPLAY lv-company vc-version lv-system lv-proversion 
      WITH FRAME f-main.
  IF AVAILABLE t-zen-dpgm THEN 
    DISPLAY t-zen-dpgm.name t-zen-dpgm.created t-zen-dpgm.author t-zen-dpgm.pgm 
          t-zen-dpgm.comments 
      WITH FRAME f-main.
  ENABLE IMAGE-1 RECT-14 t-zen-dpgm.comments Btn_OK 
      WITH FRAME f-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialise win-main 
PROCEDURE Local-Initialise :
/* allow customising of initialisation 
return 'overide' to stop default processing
*/
def var lv-user as char no-undo.

   lv-user = GetUserId().
   image-1:sensitive  in frame {&frame-name} = systemmanager(lv-user).      

  assign
    lv-system:screen-value = GetCtrl("{&systemname}")
    vc-version:SCREEN-VALUE = getctrl("{&SystemVersion}")
    lv-company:screen-value = getctrl("{&CompanyTitle}")
    lv-proversion:screen-value = proversion .

  run openquery.
  
  if not avail t-zen-dpgm then 
      ASSIGN author:screen-value   = lv-user
             comments:screen-value = AltLanguage('No Program Record Found So Using Run Time Values')
             created:screen-value  = string(today,'99/99/9999')
             NAME:screen-value     = v-prog.
  else 
      disp {&list-1} with frame {&frame-name}.

return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-openquery win-main 
PROCEDURE Local-openquery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{{&core}run.i &program    = "zen-dpgm.p"
              &path      = "{&core}{&srv}"
              &noper     = True
              &Appsrv    = "System"
              &procedure = "find-record"
              &params    = "(v-prog,
                             input-Output table t-zen-dpgm)"}
find t-zen-dpgm where t-zen-dpgm.pgm = v-prog no-error.                             
return 'override'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh win-main 
PROCEDURE Refresh :
/*------------------------------------------------------------------------------
  Purpose: called from parent to refesh itself    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-prog as handle no-undo.  

pv-prog = if pv-prog:name = GetCtrl('MainMenuProgram')
         then widget-handle(current-window:private-data)
         else pv-prog.
         
v-prog = pv-prog:name.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

