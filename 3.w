&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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
    def var ch-app as com-handle no-undo.
    def var ch-doc as com-handle no-undo.

def var ch-viewer as com-handle no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-doc Btn-shut Btn-exit Btn-Load btn-close ~
BUTTON-1 Btn-start BUTTON-2 rs-type 
&Scoped-Define DISPLAYED-OBJECTS lv-doc rs-type 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-doc rs-type 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-close 
     LABEL "Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn-exit 
     LABEL "Exit" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn-Load 
     LABEL "Load" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn-shut 
     LABEL "stop" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn-start 
     LABEL "Start" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-1 
     LABEL "viewer" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "Button 2" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-doc AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\temp~\phil.doc" 
     LABEL "Doc" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE rs-type AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Word", "wordprocessor",
"word97", "word97"
     SIZE 25 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lv-doc AT ROW 1.24 COL 7 COLON-ALIGNED WIDGET-ID 20 NO-TAB-STOP 
     Btn-shut AT ROW 1.24 COL 75 WIDGET-ID 24
     Btn-exit AT ROW 1.24 COL 94 WIDGET-ID 6
     Btn-Load AT ROW 2.43 COL 5 WIDGET-ID 2
     btn-close AT ROW 2.43 COL 21 WIDGET-ID 22
     BUTTON-1 AT ROW 2.43 COL 39 WIDGET-ID 30
     Btn-start AT ROW 2.43 COL 57 WIDGET-ID 26
     BUTTON-2 AT ROW 2.43 COL 106 WIDGET-ID 38
     rs-type AT ROW 2.67 COL 76 NO-LABEL WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 169 BY 23.19 WIDGET-ID 100.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 23.38
         WIDTH              = 169.6
         MAX-HEIGHT         = 33.86
         MAX-WIDTH          = 223.6
         VIRTUAL-HEIGHT     = 33.86
         VIRTUAL-WIDTH      = 223.6
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
/* SETTINGS FOR FILL-IN lv-doc IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR RADIO-SET rs-type IN FRAME DEFAULT-FRAME
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 3.86
       COLUMN          = 4
       HEIGHT          = 19.52
       WIDTH           = 111
       WIDGET-ID       = 28
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {C4D244A6-CD18-11D3-9852-00600889E535} type: Ole4GL */
      CtrlFrame:MOVE-AFTER(rs-type:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&window-name} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-close C-Win
ON CHOOSE OF btn-close IN FRAME DEFAULT-FRAME /* Close */
DO:
 officeclosedocument(ch-doc).
  release object ch-doc no-error.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-exit C-Win
ON CHOOSE OF Btn-exit IN FRAME DEFAULT-FRAME /* Exit */
DO:
    apply 'close' to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Load
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Load C-Win
ON CHOOSE OF Btn-Load IN FRAME DEFAULT-FRAME /* Load */
DO:

    assign frame {&frame-name} {&list-1}.
    if filenotfound(lv-doc) then do:
        message "Document not Found!" skip
                "Continue ? "
        view-as alert-box question buttons yes-no update lv-ok as log.
        if not lv-ok then return no-apply.
    end.
    
    ch-doc = officeopendocument(ch-app,lv-doc,'edit').
   officeSetlistener(ch-app,ch-doc,'wordprocessor').
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-shut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-shut C-Win
ON CHOOSE OF Btn-shut IN FRAME DEFAULT-FRAME /* stop */
DO:
  officecloseapplication(ch-app).
  release object ch-app.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-start C-Win
ON CHOOSE OF Btn-start IN FRAME DEFAULT-FRAME /* Start */
DO:
assign frame {&frame-name} rs-type.
ch-app = officestartapplication(rs-type,'visible').  

/* create 'KWPS.Application' ch-App connect no-error.  */
/*   if error-status:error                                  */
/*   then do:                                               */
/*        create 'KWPS.Application' ch-App no-error.        */
/*        if error-status:error                             */
/*        then do:                                          */
/*         message 'Cannot Start ' 'KWPS.Application' skip  */
/* Error-Status:Get-Message(Error-Status:Num-Messages)      */
/*         view-as alert-box error.                         */
/*        end.                                              */
/*   end.                                                   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* viewer */
DO:
       ASSIGN FRAME {&FRAME-NAME} {&list-1}.
def var lv-srv as char no-undo.
def var ch-app as com-handle no-undo.

 lv-srv = 'word97.Application'.

message  'Start with ' ch-viewer:ObjectClass skip lv-srv.

if not ch-viewer:ObjectClass begins lv-srv 
then do:
    ch-viewer:closefile(). 
    ch-viewer:unload().
    release object ch-app no-error.
end.

/* ch-viewer:openfile(lv-doc,false).  */

if not valid-handle(ch-app) then do:
    ch-viewer:CreateObject(lv-srv,false) no-error.
    if error-status:error then do:
       message 'Error Was        :' Error-Status:Get-Message(Error-Status:Num-Messages)
        {&dbt}.
        return no-apply.
    end.
    message 'created ' lv-srv.
    
    ch-viewer:server() no-error.
    
    if error-status:error then do:
       message 'Error Was        :' Error-Status:Get-Message(Error-Status:Num-Messages)
        {&dbt}.
        return no-apply.
    end.
    message 'Server now ' ch-viewer:ObjectClass.
    
    ch-app = ch-viewer:OleHandle:application no-error.
end.
if error-status:error then do:
   message 'Error Was        :' Error-Status:Get-Message(Error-Status:Num-Messages)
    {&dbt}.
    return no-apply.
end.
message 'setapp'.

/* ch-doc = officeopendocument(ch-app,lv-doc,'readonly').  */

ch-app:Documents:Open(lv-doc,no,1,,) no-error.  

if error-status:error then do:
   message 'Error Was        :' Error-Status:Get-Message(Error-Status:Num-Messages)
    {&dbt}.
    return no-apply.
end.
ch-doc = ch-app:activedocument .


message 'good document ' valid-handle(ch-doc) .

officeprintpreview(ch-app,ch-doc).
message  'Done ' ch-viewer:ObjectClass skip ch-doc:fullName.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
  os-command '"c:\program files (x86)\microsoft office\office\winword.exe" /regserver'.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-doc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-doc C-Win
ON MOUSE-SELECT-DBLCLICK OF lv-doc IN FRAME DEFAULT-FRAME /* Doc */
DO:
  self:screen-value = GetOsFile(lv-doc).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
Procedure OOCommand external 'c:\rex\project1.dll':
    def input param action as char.
    def input param params as char.
end procedure.
  

/* Set current-window: this will parent dialog-boxes and frames. */
assign current-window                = {&window-name} 
       this-procedure:current-window = {&window-name}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is... */
pause 0 before-hide.

/* Now enable the interface and wait for the exit condition. */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire. */
main-block:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE main-block:
   run enable_UI.
    officesetsuite('microsoft').
   if not this-procedure:persistent then
      wait-for close of this-procedure.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
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

OCXFile = SEARCH( "3.wrx":U ).
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
ELSE MESSAGE "3.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DocEvents.Quit C-Win 
PROCEDURE DocEvents.Quit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
message program-name(1) {&dbt}.
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
  RUN control_load.
  DISPLAY lv-doc rs-type 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE lv-doc Btn-shut Btn-exit Btn-Load btn-close BUTTON-1 Btn-start 
         BUTTON-2 rs-type 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ch-viewer = chCtrlFrame:Ole4GL.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

