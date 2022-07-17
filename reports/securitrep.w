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

/* ***************************  Preprocessors  ************************** */
{app-paths.i}
&glob title-text Menu Security Report     /* message box title */

&glob NoNew             /* no new button */
&glob NoEdit            /* no edit button */
&glob NoSave            /* no save button */
&glob NoDelete          /* no delete button */
&glob NoExport          /* no export button */
&glob NoUndo            /* no undo button */
&glob NoQuery           /* no query button */
&glob NoAudit           /* no audit button */
&glob NoPrint           /* no print button */
&glob NoChangedCheck    /* disable changed onleave check */
&glob NoExitCheck       /* disble exit question */
&undefine suppresswindow
/*
&glob bug               /* turn on debug mesasges */
&glob tree              /* turn on procedure tree listing */
&glob NoButtons         /* no buttons displayed */
&glob NoExit            /* no exit button */
&glob NoHelp            /* no help button */
&glob NoImmediateQuery  /* do not openquery */
&Glob NoImmediateQuery  /* force open query */
&glob NotFoundMessage   /* no record not found message */
*/

/* ***************************  Definitions  ************************** */
define temp-table t-result no-undo
   field t-item   as char
   field t-user   as char
   field t-access as char
   field t-prog   as char.
define stream op.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-user BUTTON-1 
&Scoped-Define DISPLAYED-OBJECTS lv-user 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Button 1" 
     SIZE 15 BY 1.14.

def var lv-user as char FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 40 BY 1.24 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     lv-user AT ROW 1.24 COL 24 COLON-ALIGNED WIDGET-ID 14
     BUTTON-1 AT ROW 6.95 COL 28 WIDGET-ID 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.8 BY 10.76
         
         TITLE "Menu Security Report".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW win-main ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 21.4
         ROW                = 9.29
         HEIGHT             = 10.76
         WIDTH              = 112.8
         MAX-HEIGHT         = 12.1
         MAX-WIDTH          = 112.8
         VIRTUAL-HEIGHT     = 12.1
         VIRTUAL-WIDTH      = 112.8
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
ASSIGN 
       FRAME f-main:HIDDEN           = TRUE
       FRAME f-main:PRIVATE-DATA     = 
                "1".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(win-main)
THEN win-main:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 win-main
ON CHOOSE OF BUTTON-1 IN FRAME f-main /* Button 1 */
DO:
assign frame {&frame-name}
       lv-user.
       
setsysvar('user',lv-user).

    attachmenu (current-window,
                frame {&frame-name}:handle,
                this-procedure).
refreshtemptables().
    RUN bldmenubar (current-window,
                    frame {&frame-name}:handle,
                    PgmMenuGroup('{&client}mainmenu.w'),
                    this-procedure).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK win-main 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
/* this dont work in v8 for my style of windows 
if int(substring(proversion,1,1)) >= 7 then
{&WINDOW-NAME}:parent         = current-window.      
*/
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{{&core}commonmaint.i &path = "{&sys}{&srv}"}

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
    
{&window-name}:hidden = false.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   run enable_UI.
    run initialise.
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
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(win-main)
  THEN DELETE WIDGET win-main.
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
  DISPLAY lv-user 
      WITH FRAME f-main IN WINDOW win-main.
  ENABLE lv-user BUTTON-1 
      WITH FRAME f-main IN WINDOW win-main.
  VIEW FRAME f-main IN WINDOW win-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise win-main 
PROCEDURE local-initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
buildcombo(lv-user:handle in frame {&frame-name},
           'zen-duser',
           'duser',
           'duser',
           'where true',
           'by duser',
           no,no).
           
return 'override'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh win-main 
PROCEDURE refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen win-main 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    
    return 'passed'. 
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

