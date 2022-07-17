&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{app-paths.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-method lv-to lv-cc lv-from lv-subject ~
lv-messagetext lv-attachments btnSend btnStop 
&Scoped-Define DISPLAYED-OBJECTS rs-method lv-to lv-cc lv-from lv-subject ~
lv-messagetext lv-attachments 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 rs-method lv-to lv-cc lv-from lv-subject ~
lv-messagetext lv-attachments 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSend 
     LABEL "Send" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnStop AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-attachments AS CHARACTER INITIAL "c:~\rex~\app-paths.i" 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 72 BY 4.05 NO-UNDO.

DEFINE VARIABLE lv-messagetext AS CHARACTER INITIAL "this would be your message text. please see attachments" 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 68 BY 4.52 NO-UNDO.

DEFINE VARIABLE lv-cc AS CHARACTER FORMAT "X(256)":U 
     LABEL "CC" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE lv-from AS CHARACTER FORMAT "X(256)":U INITIAL "guidev@practice-alt.com" 
     LABEL "From" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE lv-subject AS CHARACTER FORMAT "X(256)":U 
     LABEL "Subject:" 
     VIEW-AS FILL-IN 
     SIZE 72 BY .95 NO-UNDO.

DEFINE VARIABLE lv-to AS CHARACTER FORMAT "X(256)":U INITIAL "philw@metronet.co.uk" 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE rs-method AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "CDO", "CDO",
"SMTP", "SMPT",
"MAPI", "MAPI",
"Outlook", "Outlook",
"Unix", "Unix"
     SIZE 86 BY .71 TOOLTIP "Method To Send Email Via" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rs-method AT ROW 2.19 COL 13 NO-LABEL WIDGET-ID 2
     lv-to AT ROW 3.14 COL 18 COLON-ALIGNED
     lv-cc AT ROW 4.14 COL 18 COLON-ALIGNED
     lv-from AT ROW 5.19 COL 18 COLON-ALIGNED
     lv-subject AT ROW 6.19 COL 18 COLON-ALIGNED
     lv-messagetext AT ROW 8.86 COL 20 HELP
          "what ever message you want to type in" NO-LABEL
     lv-attachments AT ROW 14.57 COL 20 NO-LABEL
     btnSend AT ROW 18.86 COL 30
     btnStop AT ROW 18.86 COL 50
     "Message Text" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 9.1 COL 4 WIDGET-ID 10
     "Comma Seperated list" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 13.62 COL 19 WIDGET-ID 8
     "Attachements:" VIEW-AS TEXT
          SIZE 15 BY .62 TOOLTIP "File names of attachements above - comma separated" AT ROW 13.62 COL 4
     "Method To use" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.24 COL 8 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.2 BY 19.52.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "EMail Tester"
         HEIGHT             = 24.19
         WIDTH              = 102.8
         MAX-HEIGHT         = 38.57
         MAX-WIDTH          = 102.8
         VIRTUAL-HEIGHT     = 38.57
         VIRTUAL-WIDTH      = 102.8
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR EDITOR lv-attachments IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-cc IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-from IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR EDITOR lv-messagetext IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-subject IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-to IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR RADIO-SET rs-method IN FRAME DEFAULT-FRAME
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* EMail Tester */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* EMail Tester */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSend C-Win
ON CHOOSE OF btnSend IN FRAME DEFAULT-FRAME /* Send */
DO:
assign frame {&frame-name} {&list-1}.


 WapiSendMail(rs-method,
              lv-from,
              lv-to,
              lv-cc,
              lv-subject,
              lv-messagetext,
              lv-attachments).
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStop C-Win
ON CHOOSE OF btnStop IN FRAME DEFAULT-FRAME /* Exit */
DO:
  apply 'close' to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-method
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-method C-Win
ON VALUE-CHANGED OF rs-method IN FRAME DEFAULT-FRAME
DO:
  lv-subject:screen-value = trim(entry(1,lv-subject:screen-value,':')) + 
                            " :Sent Using " + self:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN enable_UI.
  run initialise.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY rs-method lv-to lv-cc lv-from lv-subject lv-messagetext lv-attachments 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rs-method lv-to lv-cc lv-from lv-subject lv-messagetext lv-attachments 
         btnSend btnStop 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise C-Win 
PROCEDURE initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
apply 'value-changed' to rs-method in frame {&frame-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

