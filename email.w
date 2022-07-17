&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{app-paths.i}
&undefine suppresswindow
&glob NoButtons         /* no buttons displayed */

def input param pv-from  as char no-undo.
def input param pv-to  as char no-undo.
def input param pv-cc  as char no-undo.
def input param pv-subject  as char no-undo.
def input param pv-text  as char no-undo.
def input param pv-attachments  as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-to lv-cc lv-from lv-subject ~
lv-messagetext lv-attachments btnSend btnStop 
&Scoped-Define DISPLAYED-OBJECTS lv-to lv-cc lv-from lv-subject ~
lv-messagetext lv-attachments 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-to lv-cc lv-from lv-subject lv-messagetext ~
lv-attachments 

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

DEFINE VARIABLE lv-attachments AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 71 BY 2.24 NO-UNDO.

DEFINE VARIABLE lv-messagetext AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 68 BY 9.05 NO-UNDO.

DEFINE VARIABLE lv-cc AS CHARACTER FORMAT "X(256)":U 
     LABEL "CC" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE lv-from AS CHARACTER FORMAT "X(256)":U 
     LABEL "From" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE lv-subject AS CHARACTER FORMAT "X(256)":U 
     LABEL "Subject:" 
     VIEW-AS FILL-IN 
     SIZE 72 BY .95 NO-UNDO.

DEFINE VARIABLE lv-to AS CHARACTER FORMAT "X(256)":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lv-to AT ROW 1.24 COL 18 COLON-ALIGNED
     lv-cc AT ROW 2.29 COL 18 COLON-ALIGNED
     lv-from AT ROW 3.33 COL 18 COLON-ALIGNED
     lv-subject AT ROW 4.38 COL 18 COLON-ALIGNED
     lv-messagetext AT ROW 5.43 COL 20 HELP
          "what ever message you want to type in" NO-LABEL
     lv-attachments AT ROW 15.91 COL 20 NO-LABEL
     btnSend AT ROW 18.38 COL 29
     btnStop AT ROW 18.38 COL 49
     "Message" VIEW-AS TEXT
          SIZE 10 BY .67 AT ROW 5.43 COL 8 WIDGET-ID 10
     "Comma Seperated list" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 14.95 COL 19 WIDGET-ID 8
     "Attachments:" VIEW-AS TEXT
          SIZE 13 BY .62 TOOLTIP "File names of attachements above - comma separated" AT ROW 15.86 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93.8 BY 22.71.


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
         HEIGHT             = 18.91
         WIDTH              = 94.2
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
 run save-trigger.
 run exit-trigger.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStop C-Win
ON CHOOSE OF btnStop IN FRAME DEFAULT-FRAME /* Exit */
DO:
  run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{{&core}commonmaint.i}
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
  DISPLAY lv-to lv-cc lv-from lv-subject lv-messagetext lv-attachments 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE lv-to lv-cc lv-from lv-subject lv-messagetext lv-attachments btnSend 
         btnStop 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise C-Win 
PROCEDURE local-initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if pv-from = '' 
then pv-from = GetFieldWhere('zen-duser','duser = "' + GetUserId() + '"','email').

assign
    lv-to:screen-value in frame {&frame-name} = pv-to
    lv-from:screen-value = pv-from
    lv-subject:screen-value = pv-subject
    lv-messagetext:screen-value = pv-text
    lv-cc:screen-value = pv-cc
    lv-attachments:screen-value = pv-attachments.
    
return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-save-record C-Win 
PROCEDURE local-save-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-return as char no-undo.

   
   lv-return =  wapisendmail(getsysvar('emailmethod'),
                lv-from, 
                lv-to,
                lv-cc,
                lv-subject,
                lv-messagetext,
                lv-attachments).
    if lv-return ne '' 
    then message 'Problem with Email ' lv-return
         view-as alert-box error.
    else message 'Email Sent' view-as alert-box information.     
return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validate-screen C-Win 
PROCEDURE Validate-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    if lv-to:screen-value in frame {&frame-name} = '' 
    then do:
        message msg(67,"A To Address","","","") view-as alert-box.
        return string(lv-to:handle).
    end.
    if lv-from:screen-value = '' 
    then do:
        message msg(67,"A From Address","","","") view-as alert-box.
        return string(lv-From:handle).
    end.
    if lv-subject:screen-value = '' 
    then do:
        message msg(67,"A Subject","","","") view-as alert-box.
        return string(lv-subject:handle).
    end.

    if lv-messagetext:screen-value = '' 
    then do:
        message msg(67,"A Message","","","") view-as alert-box.
        return string(lv-messagetext:handle).
    end.
    return 'passed'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

