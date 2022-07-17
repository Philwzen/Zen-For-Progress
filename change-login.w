&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME win-main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS win-main 
/******************************************************************************/
/*  PROGRAM ID.     : login.w                                                 */
/*  PROGRAM TITLE   : Login user id password screen                           */
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
&glob KeepRefreshButton
/* ***************************  Definitions  ************************** */
{app-paths.i}
def var lv-tries as int no-undo.
DEF VAR main-bar-handle AS HANDLE.

def var lv-mainmenuprog as char no-undo.
&glob Nobuttons         /* no new button */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-user lv-pass Btn_ok Btn-exit 
&Scoped-Define DISPLAYED-OBJECTS lv-user lv-pass 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-exit DEFAULT 
     LABEL "E&xit" 
     SIZE 12 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_ok DEFAULT 
     LABEL "&Ok" 
     SIZE 11.8 BY 1.24
     BGCOLOR 8 .

DEFINE VARIABLE lv-pass AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pword" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-user AS CHARACTER FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     lv-user AT ROW 1.14 COL 12.8 COLON-ALIGNED
     lv-pass AT ROW 2.43 COL 7.6 BLANK 
     Btn_ok AT ROW 3.86 COL 3.6
     Btn-exit AT ROW 3.95 COL 20.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 35.41 BY 4.77.


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
         COLUMN             = 20
         ROW                = 7.86
         HEIGHT             = 4.86
         WIDTH              = 36.2
         MAX-HEIGHT         = 26
         MAX-WIDTH          = 112
         VIRTUAL-HEIGHT     = 26
         VIRTUAL-WIDTH      = 112
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 2
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
ASSIGN 
       FRAME f-main:RESIZABLE        = TRUE.

/* SETTINGS FOR FILL-IN lv-pass IN FRAME f-main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-exit win-main
ON CHOOSE OF Btn-exit IN FRAME f-main /* Exit */
DO:
  run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ok win-main
ON CHOOSE OF Btn_ok IN FRAME f-main /* Ok */
DO:
    assign
        lv-user = lc(trim(lv-user:screen-value)) 
        lv-pass = lc(trim(lv-pass:screen-value)).
    
    if not ValidUser(lv-user,lv-pass) then do:
        Lv-tries = lv-tries + 1.
/*         sound("nocando"). */
        message 'Invalid Entry' view-as alert-box error .
        if lv-tries > 2 then quit.
        apply 'entry' to lv-user.
        return no-apply.
    end.
    else do:
        {&window-name}:private-data = entry(1,session:parameter,'^').
        setsysvar('user',lv-user).
        setsysvar('system',Getctrl('{&systemname}')).
        setsysvar('language',string(UserLanguage(lv-user))).
        setsysvar('UserGroup',usergroup(lv-user)).
        setsysvar('country',string(userCountry(lv-user))).

        initlibraries(lv-user).
        {&window-name}:hidden = true.
        lv-mainmenuprog = GetCtrl('{&MainMenuProgram}').
        if filenotfound(lv-MainMenuProg) 
            then do:
/*message msg(50,'MainMenu',lv-mainmenuprog,'','')*/
                message 'MainMenu not Found ' lv-mainmenuprog
                    view-as alert-box.
                {&window-name}:hidden = false.
                return no-apply.
            end.
        main-bar-handle = runchild(lv-MainMenuProg,this-procedure).
        SetSysVar("top-window",string(main-bar-handle)).
            apply 'choose' to btn-exit.
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
{{&core}commonmaint.i &path = "{&sys}{&srv}"}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {{&core}sec-chk.i}
    RUN enable_UI.
  {{&core}wid-chk.i}
  {{&core}focus.i}
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
  DISPLAY lv-user lv-pass 
      WITH FRAME f-main.
  ENABLE lv-user lv-pass Btn_ok Btn-exit 
      WITH FRAME f-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-update-child-procedures win-main 
PROCEDURE Local-update-child-procedures :
def input param pv-to as handle no-undo.
case pv-to:private-data:
     when lv-mainmenuprog then run refresh in pv-to (lv-user).
end case.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

