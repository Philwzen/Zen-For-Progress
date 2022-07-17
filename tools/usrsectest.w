&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME win-main
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
&glob title-text ?????       /* message box title */
/* &glob table-name ?????    /* main table we are working on */ */
/* &glob unique-key {&table-name}tableid  /* Table uniquekey name */ */
/*
&glob bug               /* turn on debug mesasges */
&glob tree              /* turn on procedure tree listing */
&glob NoChangedCheck    /* disable changed onleave check */
&glob NoExitCheck       /* disble exit question */
&glob NoButtons         /* no buttons displayed */
&glob NoNew             /* no new button */
&glob NoEdit            /* no edit button */
&glob NoSave            /* no save button */
&glob NoDElete          /* no delete button */
&glob NoExport          /* no export button */
&glob NoUndo            /* no undo button */
&glob NoQuery           /* no query button */
&glob NoAudit           /* no audit button */
&glob NoExit            /* no exit button */
&glob NoHelp            /* no help button */
&glob NoPrint           /* no print button */
&glob NoImmediateQuery  /* do not openquery */
&Glob NoImmediateQuery    /* force open query */
&glob NotFoundMessage   /* no record not found message */
*/
/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Lv-user lv-group lv-pgm lv-securityalgorithm ~
lv-menuparent lv-menu btn-Test btn-exit 
&Scoped-Define DISPLAYED-OBJECTS Lv-user lv-group lv-pgm lv-canprog ~
lv-securityalgorithm lv-menuparent lv-menu lv-canmenu 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Lv-user lv-group lv-pgm lv-securityalgorithm ~
lv-menuparent lv-menu 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-exit 
     LABEL "Exit" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-Test 
     LABEL "Test" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-canmenu AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     LABEL "pass" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE lv-canprog AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     LABEL "pass" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE lv-group AS CHARACTER FORMAT "X(256)":U 
     LABEL "Group" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE lv-menu AS CHARACTER FORMAT "X(256)":U 
     LABEL "Menu" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1 NO-UNDO.

DEFINE VARIABLE lv-menuparent AS CHARACTER FORMAT "X(256)":U 
     LABEL "Parent Menu" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1 NO-UNDO.

DEFINE VARIABLE lv-pgm AS CHARACTER FORMAT "X(256)":U 
     LABEL "Program" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1 NO-UNDO.

DEFINE VARIABLE Lv-user AS CHARACTER FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE lv-securityalgorithm AS INTEGER INITIAL 2 
     VIEW-AS SLIDER MIN-VALUE 1 MAX-VALUE 5 HORIZONTAL 
     TIC-MARKS TOP FREQUENCY 1
     SIZE 32 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     Lv-user AT ROW 1.71 COL 15.6 COLON-ALIGNED WIDGET-ID 2
     lv-group AT ROW 1.71 COL 74 COLON-ALIGNED WIDGET-ID 18
     lv-pgm AT ROW 3.14 COL 15.6 COLON-ALIGNED WIDGET-ID 4
     lv-canprog AT ROW 3.14 COL 104 COLON-ALIGNED WIDGET-ID 14
     lv-securityalgorithm AT ROW 4.57 COL 19 NO-LABEL WIDGET-ID 28
     lv-menuparent AT ROW 6.48 COL 15.6 COLON-ALIGNED WIDGET-ID 6
     lv-menu AT ROW 7.43 COL 15.6 COLON-ALIGNED WIDGET-ID 8
     lv-canmenu AT ROW 7.43 COL 105 COLON-ALIGNED WIDGET-ID 16
     btn-Test AT ROW 10.29 COL 11
     btn-exit AT ROW 10.29 COL 103 WIDGET-ID 20
     "Remember to include any '&&' in the menu name but not the parent" VIEW-AS TEXT
          SIZE 79 BY .86 AT ROW 8.86 COL 2 WIDGET-ID 22
     "logic number" VIEW-AS TEXT
          SIZE 25 BY .86 AT ROW 4.81 COL 52 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 146.2 BY 11.43
          WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW win-main ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 18
         ROW                = 8.43
         HEIGHT             = 11.43
         WIDTH              = 146.2
         MAX-HEIGHT         = 15.91
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 15.91
         VIRTUAL-WIDTH      = 146.2
         RESIZE             = no
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
/* SETTINGS FOR FILL-IN lv-canmenu IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-canprog IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-group IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-menu IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-menuparent IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-pgm IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR SLIDER lv-securityalgorithm IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN Lv-user IN FRAME f-main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-exit win-main
ON CHOOSE OF btn-exit IN FRAME f-main /* Exit */
DO:
  run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Test win-main
ON CHOOSE OF btn-Test IN FRAME f-main /* Test */
DO:
assign frame {&frame-name}
{&list-1}.

find zen-duser where zen-duser.duser = lv-user
                 no-lock no-error.
if not avail zen-duser then do:
    message 'Invalid User'.
    return no-apply.
End.

/* pgm sec */
FIND FIRST zen-dpgm where zen-dpgm.pgm = lv-pgm no-lock NO-ERROR.  
if not avail zen-dpgm then
FIND FIRST zen-dpgm where zen-dpgm.pgm = dospath(lv-pgm) no-lock NO-ERROR.  
if not avail zen-dpgm then
FIND FIRST zen-dpgm where zen-dpgm.pgm = unixpath(lv-pgm) no-lock NO-ERROR.             
                             
if avail zen-dpgm 
then do:
    lv-canprog = SecurityCheck(lv-user,
                               lv-group,
                               zen-dpgm.not-users,
                               zen-dpgm.not-group,
                               zen-dpgm.run-users,
                               zen-dpgm.run-groups).
                                   
end.

disp lv-canprog  with frame {&frame-name}.

/* menu user security */
find zen-dmenu where zen-dmenu.menu-parent = lv-menuparent 
                 and zen-dmenu.menu-name = lv-menu 
                 no-lock no-error.
if avail zen-dmenu then do:
   lv-canmenu = SecurityCheck(lv-user,
                              lv-group,
                              zen-dmenu.not-users,
                              zen-dmenu.not-group,
                              zen-dmenu.run-users,
                              zen-dmenu.run-groups).
end.
disp lv-canmenu  with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Lv-user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Lv-user win-main
ON LEAVE OF Lv-user IN FRAME f-main /* User */
DO:
if not donotfire('btn-exit') then do:
assign lv-user.
  find zen-duser where zen-duser.duser = lv-user
                 no-lock no-error.
if not avail zen-duser then do:
    message 'Invalid User'.
    return no-apply.
End.

lv-group:screen-value  = zen-duser.U-GROUP.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK win-main 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{{&core}pgm-hdr.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
oN CLOSE OF THIS-PROCEDURE do:
    if not lv-exited then
        run exit-trigger in this-procedure no-error.
    run disable_ui.             
end.

ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
    run exit-trigger in this-procedure no-error.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
    run exit-trigger in this-procedure no-error.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    {{&core}sec-chk.i}
    RUN enable_UI.
    {{&core}focus.i}
    {{&core}wid-chk.i}
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
  DISPLAY Lv-user lv-group lv-pgm lv-canprog lv-securityalgorithm lv-menuparent 
          lv-menu lv-canmenu 
      WITH FRAME f-main.
  ENABLE Lv-user lv-group lv-pgm lv-securityalgorithm lv-menuparent lv-menu 
         btn-Test btn-exit 
      WITH FRAME f-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exit-trigger win-main 
PROCEDURE exit-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
apply "close":u to this-procedure.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

