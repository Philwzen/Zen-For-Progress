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

{app-paths.i}

&glob nobuttons

&glob NoChangedCheck
&glob NoExitCheck
&glob IsMainMenu
&undefine suppresswindow
def var lv-index     as int              no-undo.
def var lv-system    as char             no-undo.
def var lv-user      as char             no-undo.
def var ch-tab       as com-handle       no-undo.
def var lv-syslist   as char   extent 20 no-undo.
def var lv-divlist  as char   extent 20 no-undo.
def var lv-childlist as handle extent 20 no-undo.
def var lv-ranfromlist as handle extent 20 no-undo.
def var lv-division  as char             no-undo.
lv-user = getsysvar('{&clv}user'). /* in zenlibrary.p */
lv-system = Getctrl('{&systemname}').
def var lv-MainButtons as char no-undo .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-37 cb-currentdivision 
&Scoped-Define DISPLAYED-OBJECTS cb-currentdivision lv-execmesswid 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSysDiv C-Win 
FUNCTION SetSysDiv RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetWinTitle C-Win 
FUNCTION SetWinTitle RETURNS CHARACTER
  ( pv-user as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cb-currentdivision AS CHARACTER FORMAT "X(256)":U 
     LABEL "Division" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 57.2 BY 1 NO-UNDO.

DEFINE VARIABLE lv-execmesswid AS CHARACTER FORMAT "X(256)":U INITIAL ? 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70.6 BY 1.71.

DEFINE BUTTON btn-cancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-progaction AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1
     BGCOLOR 20  NO-UNDO.

DEFINE VARIABLE rs-butnum AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "1", "1",
"2", "2",
"3", "3",
"4", "4",
"5", "5"
     SIZE 55 BY 6.43 TOOLTIP "Select Button to Assign To"
     BGCOLOR 21  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fr-assignUserButton
     lv-progaction AT ROW 2.05 COL 17.2 COLON-ALIGNED NO-LABEL
     rs-butnum AT ROW 3.76 COL 19.2 NO-LABEL
     btn-ok AT ROW 11.76 COL 27.4
     btn-cancel AT ROW 11.76 COL 50.4
     "Action:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 2 COL 9.2
     "Assign To:" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 3.81 COL 5.2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 63.8 ROW 10.38
         SIZE 78 BY 13.

DEFINE FRAME win-tab-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.43
         SIZE 253.2 BY 1.62.

DEFINE FRAME DEFAULT-FRAME
     cb-currentdivision AT ROW 1.67 COL 12.8 COLON-ALIGNED
     lv-execmesswid AT ROW 1.71 COL 173 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     RECT-37 AT ROW 1.43 COL 4.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 253.2 BY 2.52.


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
         HEIGHT             = 33.76
         WIDTH              = 253.2
         MAX-HEIGHT         = 46.52
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.52
         VIRTUAL-WIDTH      = 256
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,                                                          */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:PRIVATE-DATA     = 
                "*".

/* SETTINGS FOR FILL-IN lv-execmesswid IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fr-assignUserButton
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME fr-assignUserButton:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lv-progaction IN FRAME fr-assignUserButton
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME win-tab-frame
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fr-assignUserButton
/* Query rebuild information for FRAME fr-assignUserButton
     _Query            is NOT OPENED
*/  /* FRAME fr-assignUserButton */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME win-tab-frame:HANDLE
       ROW             = 1
       COLUMN          = 2
       HEIGHT          = 1.43
       WIDTH           = 252
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-MINIMIZED OF C-Win /* <insert window title> */
DO:
  publish 'childminimize'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESTORED OF C-Win /* <insert window title> */
DO:
  publish 'childrestored'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fr-assignUserButton
&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME fr-assignUserButton /* Cancel */
DO:
  frame fr-assignUserButton:hidden = true.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME fr-assignUserButton /* OK */
DO:
    def var lv-but  as handle no-undo.    
    def var x       as int no-undo.
    def var lv-menu as char no-undo.
    def var lv-proglist as char no-undo.
    if lv-index = 0 then return.

    assign frame fr-assignUserButton
        rs-butnum
        lv-but = GetWidHandle(frame default-frame:handle,'btn-usr' + rs-butnum).

    if valid-handle(lv-but) 
    then do:
        lv-but:private-data = frame fr-assignUserButton:private-data.
        lv-but:tooltip = lv-progaction:screen-value.
        do x = 1 to 5: /* buildlist of all user buttons functions */
            lv-but = GetWidHandle(frame default-frame:handle,'btn-usr' + string(x)).
               /* in generallibrary.p */
            lv-proglist = lv-proglist + '|' + lv-but:private-data.
        end.
        lv-proglist = substring(lv-proglist,2).
        errorclear(). /* save user button defs */  /* in zenlibrary.p */
            {{&core}run.i &program   = "zen-f-hotkey.p"
                         &path      = "{&core}{&srv}"
                         &Appsrv    = "System"
                         &procedure = "Save-User-Buttons"
                         &params    = "(lv-user,lv-proglist)"}
        anyerrors(). /* in zenlibrary.p */ 
/*         message lv-progaction:screen-value 'has been assigned to user button'  */
/*             string(rs-butnum,"x.")                                             */
/*         view-as alert-box information.                                         */
    end.
    else message 'Invalid button' view-as alert-box error.

    apply 'choose' to btn-cancel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME cb-currentdivision
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-currentdivision C-Win
ON VALUE-CHANGED OF cb-currentdivision IN FRAME DEFAULT-FRAME /* Division */
DO:
/*   lv-division = getcombokey(cb-currentdivision:handle). */
                   /* in generallibrary.p */
  SetSysDiv().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME win-tab-frame
&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Click
PROCEDURE CtrlFrame.TabStrip.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
run tabchoose in this-procedure (ch-tab:SelectedItem:Index).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
C-Win:hidden = true.
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
frame {&frame-name}:private-data = unixpath(this-procedure:file-name).
   /* in generallibrary.p */

/* initlibraries(). */ /* in zenlibrary.p */
/* SYSMSG('off').   */
{{&core}commonmaint.i &path = "{&core}{&srv}"}

ON window-restored OF {&WINDOW-NAME} ANYWHERE DO:
  run ParentResize (this-procedure,'Restored').
END.
ON window-minimized  OF {&WINDOW-NAME} ANYWHERE DO:
   run ParentResize (this-procedure,'Minimized').
END.
ON  window-maximized OF {&WINDOW-NAME} ANYWHERE DO:
   run ParentResize (this-procedure,'Maximized').
END.
on ctrl-f1,ctrl-f2,ctrl-f3,ctrl-f4,ctrl-f5 of {&window-name} anywhere do:
    def var x as int no-undo.
    x = int(substring(last-event:label,length(last-event:label),1)).
    run UsrBut-Action(?,x).
end.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
/*    C-Win:hidden = true. */
/*    {{&core}sec-chk.i}   */
   C-Win:hidden = true.
   frame default-frame:row = 1.
   {{&core}wid-chk.i}
   C-Win:hidden = true.
   RUN enable_UI.
   apply 'entry' to cb-currentdivision.
   

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    wait-for close of this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignBut-trigger C-Win 
PROCEDURE AssignBut-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if lv-index = 0 then return.

def var lv-prog as handle no-undo.
def var x as int no-undo.
def var y as int no-undo.
def var lv-ok as log no-undo.
def var lv-list as char no-undo.
def var lv-but as handle no-undo.
def var lv-butprog as char no-undo.

def var lv-init as char no-undo.
def var lv-from as char no-undo.
def var lv-fromtype as char no-undo.
def var lv-progtitle as char no-undo.
assign
   y = int(ch-tab:SelectedItem:Index)
   lv-fromtype = lv-ranfromlist[y]:type
   lv-prog = lv-childlist[y]
   lv-from = if lv-fromtype = 'button' 
               then lv-ranfromlist[y]:private-data
               else lv-prog:name + ':' + entry(1,lv-ranfromlist[y]:private-data,'|') + ':' + lv-fromtype.
/* choose 1 to 5 for button num */
do x = 1 to 5: /* buildlist of all user buttons functions */
   lv-but = GetWidHandle(frame default-frame:handle,'btn-usr' + string(x)).
   lv-butprog = ''.

   lv-butprog = entry(2,lv-but:private-data,':') no-error.
   if lv-from = lv-but:private-data
   then do:
      message 
         replace(programtitle(lv-butprog,'menu'),"&","")/* in zenlibrary.p */
         'is already defined on button ' + string(x) + "." skip
         'Do you want to assign it to another button?'
      view-as alert-box question buttons yes-no update lv-ok.                   
      if not lv-ok then return. 
      else do:
          lv-but:private-data = 'Not Defined'.
      end.
   end.
   if lv-init = '' and lv-butprog = '' then lv-init = string(x).
   lv-progtitle = programtitle(lv-butprog,'menu').
   if lv-progtitle begins('Menu id') then lv-progtitle = ''. 
   lv-list = lv-list + ',' + string(x) + '  ' + lv-progtitle + ',' + string(x).
end.
lv-list = substring(lv-list,2).

assign
   rs-butnum:radio-buttons in frame fr-assignUserButton = lv-list.
   rs-butnum:screen-value = if lv-init = '' then '1' else lv-init.
   frame fr-assignUserButton:hidden = false.
   frame fr-assignUserButton:private-data = /* lv-prog:name + ':' + */ lv-from /* + ':' + lv-fromtype */.

lv-progaction:screen-value = 
if num-entries(lv-ranfromlist[y]:private-data,':') > 1 
then programtitle(entry(2,lv-ranfromlist[y]:private-data,':'),'menu') /* in zenlibrary.p */
else programtitle(entry(1,lv-ranfromlist[y]:private-data,'|'),'menu'). /* in zenlibrary.p */

frame fr-assignUserButton:move-to-top().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkReleaseNotes C-Win 
PROCEDURE checkReleaseNotes :
/*------------------------------------------------------------------------------
  Purpose:     To make sure user has the chance to review new release notes
               after updates.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def var lastVersionViewed as char  no-undo.
   def var currentVersion    as char  no-undo.
   def var viewNotes         as log    no-undo.


   assign
      lastVersionViewed = 
         getfield('s-user','signonid',lv-user,'last-releaseNotes')
      currentVersion    = getctrl("{&SystemVersion}").

   if lastversionViewed ge currentVersion then return.

   /* ask user if he/she wants to see help */
   message 
      "A new release has been installed since you last logged in." skip(1)
      "Do you want to review the release notes?" skip
      "(They include the rules for a contest with a $100 prize!)"
      view-as alert-box question buttons yes-no update viewNotes.

   if viewNotes then btnHelp(?,yes). /* zenlibrary.p */
   
   /* in any case, update user record so user is not asked again */
   {{&core}run.i &program   = "s-user.p"
      &path      = "{&server}{&tables}"
      &Appsrv    = "System"
      &procedure = "updateField"
      &params    = "(lv-user,
                     'last-releaseNotes',
                     currentVersion)"}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

OCXFile = SEARCH( "mainmenu.wrx":U ).
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
ELSE MESSAGE "mainmenu.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateTab C-Win 
PROCEDURE CreateTab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-child as handle no-undo.
                 
if lv-Division ne getcombokey(cb-currentdivision:handle in frame {&frame-name})
then SetSysDiv().

lv-index = lv-index + 1.

ch-tab:Tabs:Add(lv-index,,caps(lv-division) + ' ' + 
                programtitle(string(pv-child),"program")).

assign
    lv-syslist[lv-index]   = lv-system
    lv-divlist[lv-index]  = lv-division
    lv-childlist[lv-index] = pv-child
    lv-ranfromlist[lv-index] = self
    ch-tab:tabs(lv-index):Selected = True.
/* no need to run as selected fires trigger */
/* run tabchoose in this-procedure (lv-index).  */

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
  DISPLAY cb-currentdivision lv-execmesswid 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-37 cb-currentdivision 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME win-tab-frame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-win-tab-frame}
  DISPLAY lv-progaction rs-butnum 
      WITH FRAME fr-assignUserButton IN WINDOW C-Win.
  ENABLE rs-butnum btn-ok btn-cancel 
      WITH FRAME fr-assignUserButton IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fr-assignUserButton}
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
ch-tab = chCtrlFrame:TabStrip.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn C-Win 
PROCEDURE Local-ChildReturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-from as char no-undo.
def var y as int no-undo.
def var x as int no-undo.
def var lv-h as handle no-undo.
def var gotone as log no-undo.

lv-h = widget-handle(pv-from).
do x = 1 to ch-tab:tabs:count:
    if lv-childlist[x] = lv-h then do:
        gotone = true.
        leave.
    end.
end.
if gotone then do:
    ch-tab:tabs:remove(x).
    y = x.
    do y = x to ch-tab:tabs:count + 1:
        assign lv-syslist[y]   = lv-syslist[y + 1]
               lv-divlist[y]  = lv-divlist[y + 1]
               lv-childlist[y] = lv-childlist[y + 1]
               lv-ranfromlist[y]  = lv-ranfromlist[y + 1].
    end.
    assign x = ch-tab:tabs:count
           lv-index = x.
    if x > 0 then do:
        ch-tab:tabs(x):Selected = True.
        run tabchoose in this-procedure (x).
    end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Exit-Trigger C-Win 
PROCEDURE Local-Exit-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-ok as log no-undo.

def var lv-totjobs as int no-undo.
def var lv-usrjobs as int no-undo.

if lv-asyncreports then do:
    lv-usrjobs = max(int(GetSysVar('{&AsyncCurrentJobs}')),0).  
    if lv-usrjobs > 0 then do:
        message 'You still have ' lv-usrjobs ' report running!' skip
                'Do you want to cancel them and exit?'
        view-as alert-box Question buttons yes-no update lv-ok.
        if not lv-ok then return 'override'.
    
        lv-totjobs = max(int(GetCtrl('{&AsyncCurrentJobs}')),0).
        
        lv-totjobs = lv-totjobs - lv-usrjobs.
        SetCtrl('{&AsyncCurrentJobs}',string(lv-totjobs)).
        
    end.
end.

/*     CleanSession(). /* in zenlibrary.p */  */
/*     quit.                                  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialise C-Win 
PROCEDURE Local-Initialise :
def var lv-session as char  no-undo.

c-Win:hidden = true.
lv-session = entry(1,session:parameter,'^').
/*   setsysvar("top-window",STRING(THIS-PROCEDURE:HANDLE)).  */
setwintitle(lv-user).

  def var lv-desc as char no-undo.

assign
    cb-currentdivision:private-data in frame {&frame-name} = classcodes('divisions',lv-desc)
    cb-currentdivision:list-items = lv-desc
    cb-currentdivision:screen-value = entry(1,cb-currentdivision:list-items)
    lv-division = getcombokey(cb-currentdivision:handle).       

SetSysDiv().

setsysvar("{&clv}top-win=" + STRING(current-window) + 
          ",division=" + lv-division +
          ",{&clv}execmesswid=" + string(lv-execmesswid:handle in frame {&frame-name}) +
          "{&clv}top-window=" + string(this-procedure)
          ,'').

SetExecMessHandle (lv-execmesswid:handle).
C-Win:visible = true.
return 'override'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Update-Child-Procedures C-Win 
PROCEDURE Local-Update-Child-Procedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def input param pv-to as handle no-undo.
   
def var x as int no-undo.
   case pv-to:private-data:
      when '{&core}about.w'
            then run refresh in pv-to (this-procedure).
   end case.        


do x = 1 to ch-tab:tabs:count:
    if lv-childlist[x] = pv-to then do:
        run tabchoose in this-procedure (x).
        leave.
    end.
end.

   return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ParentResize C-Win 
PROCEDURE ParentResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-from as handle no-undo.
def input param pv-mode as char no-undo.
def var x as int no-undo.

do x = 1 to 20:
   if valid-handle(lv-childlist[x]) then do:
   if lv-childlist[x]:filename = 'codeclient/general/rexchui.w' then do:
      run SetDosWinState in lv-childlist[x] (pv-mode).
      if pv-mode = 'restored' then 
            run ChildTabChoose in lv-childlist[x] (lv-childlist[x],yes).
      leave.
   end.
   end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ParentSetTabInfo C-Win 
PROCEDURE ParentSetTabInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-child as handle no-undo.
def var lv-extrainfo as char no-undo.
def var x as int no-undo.
def var lv-replace as log no-undo.
def var lv-caption as char no-undo.

run SendTabInfo in pv-child (output lv-extrainfo) no-error.
if lv-extrainfo = '' then return.

lv-replace = lv-extrainfo begins ':'.
if not lv-replace 
then lv-extrainfo = ': ' + lv-extrainfo.
else lv-extrainfo = substring(lv-extrainfo,2).

do x = 1 to 20:
    if lv-childlist[x] = pv-child 
    then do:
        if not lv-replace 
        then lv-caption = entry(1,ch-tab:Tabs(x):Caption,':').
        else lv-caption = ''.
        ch-tab:Tabs(x):Caption = lv-caption + lv-extrainfo.       
        leave.
    end.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh C-Win 
PROCEDURE Refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-user as char no-undo.  
def var lv-startprog as char no-undo.

/*format of buttonlist is:
  CommaSepListOfButtonsAndProperties|CommonProperies
  
  name^SensitiveVisibleOverlay^tooltip^icon|
  procedure,window,frame,horizontal,flat,startcol,startrow,height,width,center */
  
     lv-mainbuttons = 
            "skip^fff,
            s-sys1^ttf,
            s-sys2^ttf,
            s-sys3^ttf,
            s-sys4^ttf,
            s-sys5^fff,
            skip^fff,skip^fff,skip^fff,skip^fff,
            s-usr1^ttf,
            s-usr2^ttf,
            s-usr3^ttf,
            s-usr4^ttf,
            s-usr5^ttf,
            assignbut^ttf^Assign most recently opened window to one of the special buttons on the left,
            Skip^fff,skip^fff,
            Refresh^ttf,
            Exit^ttf^^bigexit|" + 
            string(this-procedure) + "," + string({&window-name}:handle) + "," + string(frame default-frame:handle) + "," +
            "true,true," + string(rect-37:width-pixels + 10) + ",10,32,32,false".

lv-user = pv-user.
/*     run reloadlibs.  */
FreezeWindow(current-window,1). /* in zenlibrary.p */


AttachMenu({&WINDOW-NAME}:HANDLE,frame {&frame-name}:handle,this-procedure).
/* in zenlibrary.p */

cb-currentdivision:screen-value = setcombovalue(getsysvar('{&clv}division'),cb-currentdivision:handle).

setwintitle(lv-user).

createbuts(lv-MainButtons). /* in zenlibrary.p */

lv-execmesswid:hidden = not stringtolog(getctrl('DisplaySrvExecTime')) no-error.

FreezeWindow(current-window,0). /* in zenlibrary.p */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh-Trigger C-Win 
PROCEDURE Refresh-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-wid  as handle no-undo.  
def var lv-wid1 as handle no-undo.

lv-wid = frame {&frame-name}:first-child.
lv-wid = lv-wid:first-child.

do while valid-handle(lv-wid):
   lv-wid1 = lv-wid.
   lv-wid = lv-wid1:next-sibling.

   if lv-wid1:type = 'button' 
      then delete widget lv-wid1.
end.
RefreshTempTables(). 
run refresh in this-procedure (getuserid()). /* in zenlibrary.p */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReLoadLibs C-Win 
PROCEDURE ReLoadLibs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var zh         as handle no-undo.
def var lv-liblist as char   no-undo.
def var x          as int    no-undo.
def var zh1        as handle no-undo.

zh = widget-handle(entry(1,session:super-procedures)).
lv-liblist = entry(2,zh:private-data,'-').

do x = 1 to num-entries(session:super-procedures):
    zh = widget-handle(entry(x,session:super-procedures)).
    SESSION:REMOVE-SUPER-PROCEDURE(zh).
    delete procedure zh.
end.  

run loadsuper(lv-liblist).

initLibraries(lv-user).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TabChoose C-Win 
PROCEDURE TabChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-tab as int no-undo.

assign
   lv-system = lv-syslist[pv-tab]
   cb-currentdivision:screen-value in frame {&frame-name} =
      setcombovalue(lv-divlist[pv-tab],cb-currentdivision:handle).

SetSysDiv().

publish 'childtabchoose' (lv-childlist[pv-tab],?). 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UsrBut-Action C-Win 
PROCEDURE UsrBut-Action :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-buthandle as handle no-undo.
def input param pv-butnumber as int no-undo.

if not valid-handle(pv-buthandle) then do:
    pv-buthandle = GetWidHandle(frame {&frame-name}:handle,
                                'btn-usr' + string(pv-butnumber)).
end.

if not valid-handle(pv-buthandle) then do:
    message 'Button Not Found ' 'btn-usr' pv-butnumber.
    return 'error'.
end.

if pv-buthandle:private-data = 'not defined' then 
   message "No action has been assigned yet to this button.".
   /* Self:name ' Action Not Defined'. */
   
else do:
   /* run whatever program has been assigned to this button */
   runchild (entry(1,pv-buthandle:private-data,':'),this-procedure). /* in generallibrary.p */ 
end.   


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UsrBut-Trigger C-Win 
PROCEDURE UsrBut-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* message self:private-data skip  */
/*         program-name(1) skip    */
/*         program-name(2).        */


run UsrBut-Action(self,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Welcome C-Win 
PROCEDURE Welcome :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSysDiv C-Win 
FUNCTION SetSysDiv RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
def var lv-olddiv as char no-undo.
def var lv-oldsys as char no-undo.
def var x as int no-undo.
def var lv-firsttime as log no-undo init true.

assign
   lv-olddiv = lv-division
   lv-oldsys  = lv-system.

lv-division = getcombokey(cb-currentdivision:handle in frame {&frame-name}).


if lv-oldsys ne lv-system then
   setsysvar('system',lv-system). /* in zenlibrary.p */

if lv-olddiv ne lv-division 
then do:
   setsysvar('division',lv-division). /* in zenlibrary.p */
   do x = 1 to lv-index:
      if lv-divlist[x] = lv-division
      then do:
         lv-firsttime = false.
         leave.
      end.
   end.
   if lv-firsttime then 
      run Welcome in this-procedure. /* displays announcements */
end.

RETURN "".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetWinTitle C-Win 
FUNCTION SetWinTitle RETURNS CHARACTER
  ( pv-user as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

{&window-name}:title = Getctrl('{&CompanyTitle}')  + ": " +
                       Getctrl('{&systemname}')    + ": " +
                       Getctrl('{&SystemVersion}') + ": " +
                       "User: " + pv-user + " " + 
                       if pdbname(1) ne ? 
                         then pdbname(1)
                         else entry(1,session:parameter,'^').
     
  RETURN "".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

