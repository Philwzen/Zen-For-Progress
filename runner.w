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
/*  COMPANY NAME    : East India Trading ltd (COPYRIGHT 94,95,96)               */
/*  VERSION NO.     : 1.0                                                     */
/******************************************************************************/
/******************************************************************************/
/*                          PATCH HISTORY                                     */
/*                                                                            */
/*           PATCH  USR     Job                                               */
/* DATE      NO.    ID      REF DESCRIPTION                                   */
/* -------------------------------------------------------------------------- */
/* ??/??/??  P00    ?????   00  initial release                               */
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{app-paths.i}
&glob nobuttons
&glob KeepRefreshButton
&glob nochangedcheck
&glob noexitcheck
/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-file lv-proc Btn_OK Btn_exit btn-sel ~
lv-per 
&Scoped-Define DISPLAYED-OBJECTS lv-file lv-proc lv-per 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-file lv-proc lv-per 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-sel 
     LABEL "&Select" 
     SIZE 14 BY 1.24.

DEFINE BUTTON Btn_exit AUTO-END-KEY DEFAULT 
     LABEL "e&Xit" 
     SIZE 16.8 BY 1.24
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "&Run" 
     SIZE 16.8 BY 1.24
     BGCOLOR 8 .

def var lv-file as char FORMAT "X(256)":U 
     LABEL "File" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

def var lv-proc as char FORMAT "X(256)":U 
     LABEL "Proc" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

def var lv-per as log INITIAL no 
     LABEL "Per" 
     VIEW-AS TOGGLE-BOX
     SIZE 8.6 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     lv-file AT ROW 1.1 COL 7 COLON-ALIGNED HELP
          "File name to Run"
     lv-proc AT ROW 2.19 COL 7 COLON-ALIGNED HELP
          "File name to Run" WIDGET-ID 2
     Btn_OK AT ROW 3.57 COL 2
     Btn_exit AT ROW 3.57 COL 63
     btn-sel AT ROW 1.19 COL 67.4
     lv-per AT ROW 3.86 COL 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.2 BY 4.24
         .


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
         COLUMN             = 20.8
         ROW                = 8.57
         HEIGHT             = 4.43
         WIDTH              = 82.2
         MAX-HEIGHT         = 36.57
         MAX-WIDTH          = 204.6
         VIRTUAL-HEIGHT     = 36.57
         VIRTUAL-WIDTH      = 204.6
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN lv-file IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-per IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-proc IN FRAME f-main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-sel win-main
ON CHOOSE OF btn-sel IN FRAME f-main /* Select */
DO:
 lv-file = GETosFILE(lv-file).
 display lv-file with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK win-main
ON CHOOSE OF Btn_OK IN FRAME f-main /* Run */
DO:
  assign {&list-1}.

  if lv-file = '' or lv-file = ? or search(lv-file) = ? then do:
      message msg(20,'File Name','','','') view-as alert-box error.
      return no-apply.
  end.


  if lv-proc = '' then run prognoproc.
                  else run progproc.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-file win-main
ON MOUSE-SELECT-DBLCLICK OF lv-file IN FRAME f-main /* File */
DO:
    apply 'choose' to btn-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-file win-main
ON RETURN OF lv-file IN FRAME f-main /* File */
DO:
  apply 'choose' to btn-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-proc win-main
ON MOUSE-SELECT-DBLCLICK OF lv-proc IN FRAME f-main /* Proc */
DO:
    apply 'choose' to btn-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-proc win-main
ON RETURN OF lv-proc IN FRAME f-main /* Proc */
DO:
  apply 'choose' to btn-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK win-main 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{{&core}commonmaint.i &path = "{&core}{&srv}"}

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
  DISPLAY lv-file lv-proc lv-per 
      WITH FRAME f-main.
  ENABLE lv-file lv-proc Btn_OK Btn_exit btn-sel lv-per 
      WITH FRAME f-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProgNoProc win-main 
PROCEDURE ProgNoProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if can-do('.p,.w,.r',substring(lv-file,length(lv-file) - 1)) then 
      if lv-per then 
         runchild(lv-file,this-procedure). 
      else run value(lv-file).
  else do:
      assign
          file-info:file-name = lv-file
          lv-file             = file-info:full-pathname.
      win-exec(lv-file,1).
  end. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProgProc win-main 
PROCEDURE ProgProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {{&core}run.i 
      &programc   = lv-file
      &Appsrv     = "System"
      &noper      = 'yes'
      &procedurec = lv-proc}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

