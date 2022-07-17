&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME window-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                              */
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

CREATE WIDGET-POOL.
&glob zenscreen     true
/* ***************************  Definitions  ************************** */
{app-paths.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frame-maint

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-cancel-message btn-go-message cb-type ~
Cb-recipient lv-textout 
&Scoped-Define DISPLAYED-OBJECTS lv-from cb-type Cb-recipient lv-textin ~
lv-textout 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cb-type Cb-recipient lv-textin lv-textout 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GroupsList window-maint 
FUNCTION GroupsList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UserList window-maint 
FUNCTION UserList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel-message 
     IMAGE-UP FILE "{&core}grafix/undo-su.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 4" 
     SIZE 15 BY 1.67.

DEFINE BUTTON btn-go-message 
     IMAGE-UP FILE "{&core}grafix/email-su.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 3" 
     SIZE 15 BY 1.67.

def var Cb-recipient as char FORMAT "X(256)":U 
     LABEL "Recipient" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 29.8 BY 1 NO-UNDO.

def var cb-type as char FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "User","Group","System" 
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

def var lv-textin as char 
     VIEW-AS EDITOR LARGE
     SIZE 68 BY 3.62 NO-UNDO.

def var lv-textout as char 
     VIEW-AS EDITOR LARGE
     SIZE 68 BY 3.38 NO-UNDO.

def var lv-from as char FORMAT "X(256)":U 
     LABEL "Received From" 
      VIEW-AS TEXT 
     SIZE 51 BY .62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     btn-cancel-message AT ROW 11.62 COL 50.4
     btn-go-message AT ROW 11.71 COL 5.2
     lv-from AT ROW 2.76 COL 17 COLON-ALIGNED
     cb-type AT ROW 1.57 COL 3.6 COLON-ALIGNED NO-LABEL
     Cb-recipient AT ROW 1.57 COL 39.6 COLON-ALIGNED
     lv-textin AT ROW 3.48 COL 3.8 NO-LABEL
     lv-textout AT ROW 7.86 COL 3.8 NO-LABEL
     "Send Text" VIEW-AS TEXT
          SIZE 10.8 BY .62 AT ROW 7.24 COL 3.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 73.6 BY 12.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Maintenance"
         COLUMN             = 89.4
         ROW                = 8.19
         HEIGHT             = 12.81
         WIDTH              = 73.6
         MAX-HEIGHT         = 30.95
         MAX-WIDTH          = 159.8
         VIRTUAL-HEIGHT     = 30.95
         VIRTUAL-WIDTH      = 159.8
         SHOW-IN-TASKBAR    = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW window-maint
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frame-maint
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR COMBO-BOX Cb-recipient IN FRAME frame-maint
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cb-type IN FRAME frame-maint
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-from IN FRAME frame-maint
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR lv-textin IN FRAME frame-maint
   NO-ENABLE 1                                                          */
ASSIGN 
       lv-textin:READ-ONLY IN FRAME frame-maint        = TRUE.

/* SETTINGS FOR EDITOR lv-textout IN FRAME frame-maint
   1                                                                    */
ASSIGN 
       lv-textout:RETURN-INSERTED IN FRAME frame-maint  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(window-maint)
THEN window-maint:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-cancel-message
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel-message window-maint
ON CHOOSE OF btn-cancel-message IN FRAME frame-maint /* Button 4 */
DO:
    apply 'close' to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-go-message
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-go-message window-maint
ON CHOOSE OF btn-go-message IN FRAME frame-maint /* Button 3 */
DO:
    SendMessage(cb-recipient:screen-value,lv-textout:screen-value).
    lv-textout:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-type window-maint
ON VALUE-CHANGED OF cb-type IN FRAME frame-maint
DO:
    CASE self:screen-value:
        WHEN "user" THEN cb-recipient:list-items = userlist().
        WHEN "group" THEN cb-recipient:list-items = groupslist().
        WHEN "system" THEN cb-recipient:list-items = Getsysvar("system").
    END CASE.

    cb-recipient:screen-value = entry(1,cb-recipient:list-items,'{&combodelim}').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.
/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  RUN exitCheck.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  RUN ExitCheck.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {{&core}sec-chk.i}
  
  RUN enable_UI.
  RUN initialise.
  {{&core}wid-chk.i}
  {{&core}focus.i}
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(window-maint)
  THEN DELETE WIDGET window-maint.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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
  DISPLAY lv-from cb-type Cb-recipient lv-textin lv-textout 
      WITH FRAME frame-maint IN WINDOW window-maint.
  ENABLE btn-cancel-message btn-go-message cb-type Cb-recipient lv-textout 
      WITH FRAME frame-maint IN WINDOW window-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitcheck window-maint 
PROCEDURE exitcheck :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise window-maint 
PROCEDURE initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN cb-type:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "system"
           cb-type:delimiter = '{&combodelim}'
           cb-recipient:delimiter = '{&combodelim}'.
    APPLY "value-changed" TO cb-type.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh window-maint 
PROCEDURE refresh :
def input param pv-extra as char no-undo.
def input param pv-txt   as char no-undo.

frame {&frame-name}:move-to-top().
if pv-extra = '' then return.

assign
    lv-from:screen-value  = entry(2,pv-extra,'{&Delim2}')
    lv-textin:screen-value = lv-textin:screen-value + chr(10) + pv-txt.
    
/*                                                               */
/* case entry(1,pv-extra,'{&Delim2}'):                                   */
/*     when GetUserID()      then lv-textin:fgcolor = 10.  */
/*     when Getsysvar("usergroup") then lv-textin:fgcolor = 14.  */
/*     when Getsysvar("system")    then lv-textin:fgcolor = 12.  */
/*     otherwise lv-textin:fgcolor = 0.                          */
/* end case.                                                     */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GroupsList window-maint 
FUNCTION GroupsList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
def var lv-value as char no-undo.
      {{&core}run.i &program   = "zen-duser.p"
                   &path      = "{&core}{&srv}"
                   &Appsrv    = "System"  
                   &procedure = "ListGroups"
                   &params    = "(OUTPUT lv-value)"}

        if lv-value = ? then lv-value = ''.               

  RETURN lv-value.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UserList window-maint 
FUNCTION UserList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
def var lv-value as char no-undo.

      {{&core}run.i &program   = "zen-duser.p"
                   &path      = "{&core}{&srv}"
                   &Appsrv    = "System"  
                   &procedure = "ListUsers"
                   &params    = "(OUTPUT lv-value)"}

        if lv-value = ? then lv-value = ''.               
  return lv-value.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

