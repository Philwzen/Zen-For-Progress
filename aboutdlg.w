&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-dpgm NO-UNDO LIKE zen-dpgm.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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

/* ***************************  Definitions  ************************** */

def var v-w    as handle no-undo.
def var v-prog as char   no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-zen-dpgm

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame t-zen-dpgm.comments ~
t-zen-dpgm.name t-zen-dpgm.created t-zen-dpgm.author t-zen-dpgm.pgm 
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH t-zen-dpgm SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH t-zen-dpgm SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame t-zen-dpgm
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame t-zen-dpgm


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 RECT-14 Btn_OK 
&Scoped-Define DISPLAYED-FIELDS t-zen-dpgm.comments t-zen-dpgm.name ~
t-zen-dpgm.created t-zen-dpgm.author t-zen-dpgm.pgm 
&Scoped-define DISPLAYED-TABLES t-zen-dpgm
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-dpgm
&Scoped-Define DISPLAYED-OBJECTS lv-company vc-version lv-system 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 t-zen-dpgm.comments t-zen-dpgm.name ~
t-zen-dpgm.created t-zen-dpgm.author t-zen-dpgm.pgm 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 8 BY 1.19 TOOLTIP "OK"
     BGCOLOR 8 .

DEFINE VARIABLE lv-company AS CHARACTER FORMAT "X(256)":U 
     LABEL "Company" 
      VIEW-AS TEXT 
     SIZE 43 BY .62 NO-UNDO.

DEFINE VARIABLE lv-system AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 17 BY .62 NO-UNDO.

DEFINE VARIABLE vc-version AS CHARACTER FORMAT "X(256)":U 
     LABEL "Version" 
      VIEW-AS TEXT 
     SIZE 42.8 BY .62 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "{&core}grafix/zen.ico":U
     SIZE 5.6 BY 1.33.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 7.1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      t-zen-dpgm SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     t-zen-dpgm.comments AT ROW 8.43 COL 3.8 NO-LABEL
          VIEW-AS EDITOR
          SIZE 56 BY 3.48
     Btn_OK AT ROW 12.43 COL 51
     lv-company AT ROW 1.38 COL 13.6 COLON-ALIGNED
     vc-version AT ROW 2.52 COL 13.6 COLON-ALIGNED
     t-zen-dpgm.name AT ROW 3.67 COL 13.6 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 42.8 BY .62
     t-zen-dpgm.created AT ROW 4.76 COL 13.6 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 16 BY .62
     t-zen-dpgm.author AT ROW 5.86 COL 13.6 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 42.4 BY .62
     t-zen-dpgm.pgm AT ROW 6.86 COL 13.6 COLON-ALIGNED HELP
          "Physical Program name"
          LABEL "Called By" FORMAT "x(255)"
           VIEW-AS TEXT 
          SIZE 42.6 BY .62
     lv-system AT ROW 12.67 COL 12 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 12.19 COL 4
     RECT-14 AT ROW 1.05 COL 4
     SPACE(2.19) SKIP(5.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-dpgm T "?" NO-UNDO schadm zen-dpgm
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN t-zen-dpgm.author IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR EDITOR t-zen-dpgm.comments IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.created IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-company IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-system IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN t-zen-dpgm.name IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-dpgm.pgm IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT EXP-HELP                            */
/* SETTINGS FOR FILL-IN vc-version IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "Temp-Tables.t-zen-dpgm"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 Dialog-Frame
ON MOUSE-MENU-CLICK OF IMAGE-1 IN FRAME Dialog-Frame
DO:
/*    run displayme in h-tpro. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF IMAGE-1 IN FRAME Dialog-Frame
DO:
/*     sysmsg('Dumping Procedure Calls').  */
/*     run {&core}proccalls.p.             */
/*     sysmsg('off').                      */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

   v-prog = current-window:private-data.
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


{{&core}commonmaint.i &path = "{&sys}{&srv}"}
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {{&core}sec-chk.i}   /* screen security check */
    RUN enable_UI.
   {{&core}wid-chk.i}   /* widgetlevel security check */
   {{&core}focus.i}     /* set focus to first enabled widget */
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY lv-company vc-version lv-system 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE t-zen-dpgm THEN 
    DISPLAY t-zen-dpgm.comments t-zen-dpgm.name t-zen-dpgm.created 
          t-zen-dpgm.author t-zen-dpgm.pgm 
      WITH FRAME Dialog-Frame.
  ENABLE IMAGE-1 RECT-14 Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialise Dialog-Frame 
PROCEDURE Local-Initialise :
image-1:sensitive  in frame {&frame-name} = systemmanager(GetUserID()).      

  assign
    lv-system:screen-value = GetCtrl("{&systemname}")
    vc-version:SCREEN-VALUE = getctrl("{&SystemVersion}")
    lv-company:screen-value = getctrl("{&CompanyTitle}") .
run openquery.
  if not avail t-zen-dpgm then 
      ASSIGN author:screen-value   = GetUserID()
             comments:screen-value = AltLanguage('No Program Record Found So Using Run Time Values')
             created:screen-value  = string(today,'99/99/9999')
             NAME:screen-value     = v-prog.
  else 
      disp {&list-1} with frame {&frame-name}.
return 'override'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-OpenQuery Dialog-Frame 
PROCEDURE Local-OpenQuery :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh Dialog-Frame 
PROCEDURE refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-hand as handle no-undo.  
  lv-hand = widget-handle(active-window:private-data).  
  v-prog = lv-hand:name.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

