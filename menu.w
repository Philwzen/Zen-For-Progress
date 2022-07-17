&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
{zen/link.i "new" "charisma"}

def var ch-listview as com-handle no-undo.
def var lv-tries as int no-undo.
/* bodge */
def var lv-parent as char extent 50 no-undo.
def var lv-level  as int no-undo init 1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-user lv-pass BTN-ok Btn-exit Rs-style 
&Scoped-Define DISPLAYED-OBJECTS lv-user lv-pass Rs-style lv-text 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE buttonicons AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chbuttonicons AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE ImageLoader AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImageLoader AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE LargeIcons AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chLargeIcons AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE ListView AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chListView AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE SmallIcons AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chSmallIcons AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Toolbar AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chToolbar AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-back 
     LABEL "Back" 
     SIZE 7.4 BY 1.

DEFINE BUTTON Btn-exit AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 7 BY 1.

DEFINE BUTTON BTN-ok 
     LABEL "Ok" 
     SIZE 7.8 BY 1.

DEFINE VARIABLE lv-text AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 24.8 BY 7.48 NO-BOX  NO-UNDO.

DEFINE VARIABLE lv-pass AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16.6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-user AS CHARACTER FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE IMAGE lv-image
     FILENAME "adeicon/blank":U
     SIZE 8.8 BY 2.05.

DEFINE VARIABLE Rs-style AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Large", 0,
"Small", 1,
"List", 2,
"Report", 3
     SIZE 38.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lv-user AT ROW 3.57 COL 9.4 COLON-ALIGNED
     lv-pass AT ROW 3.57 COL 35.2 COLON-ALIGNED BLANK 
     BTN-ok AT ROW 3.57 COL 55.8
     Btn-back AT ROW 3.57 COL 74.6
     Btn-exit AT ROW 3.57 COL 92
     Rs-style AT ROW 4.86 COL 10.6 NO-LABEL
     lv-text AT ROW 8.71 COL 1.2 NO-LABEL
     lv-image AT ROW 6.48 COL 2
    WITH 1 DOWN NO-BOX OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101 BY 15.43
         CANCEL-BUTTON Btn-exit.


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
         HEIGHT             = 15.52
         WIDTH              = 101.2
         MAX-HEIGHT         = 20.76
         MAX-WIDTH          = 191.6
         VIRTUAL-HEIGHT     = 20.76
         VIRTUAL-WIDTH      = 191.6
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR BUTTON Btn-back IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE lv-image IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lv-image:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR EDITOR lv-text IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lv-text:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Toolbar ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1.29
       COLUMN       = 2.8
       HEIGHT       = 2
       WIDTH        = 98.2
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME ListView ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 6
       COLUMN       = 27
       HEIGHT       = 10.38
       WIDTH        = 74.4
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME buttonicons ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 11.48
       COLUMN       = 16.2
       HEIGHT       = 1.81
       WIDTH        = 7.6
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME ImageLoader ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 11.67
       COLUMN       = 3.4
       HEIGHT       = 1.86
       WIDTH        = 8.4
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME SmallIcons ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 13.71
       COLUMN       = 15.6
       HEIGHT       = 1.81
       WIDTH        = 7.6
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME LargeIcons ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 13.76
       COLUMN       = 3.2
       HEIGHT       = 1.81
       WIDTH        = 7.6
       HIDDEN       = yes
       SENSITIVE    = yes.
      Toolbar:NAME = "Toolbar":U .
/* Toolbar OCXINFO:CREATE-CONTROL from: {66833FE6-8583-11D1-B16A-00C0F0283628} type: Toolbar */
      ListView:NAME = "ListView":U .
/* ListView OCXINFO:CREATE-CONTROL from: {BDD1F04B-858B-11D1-B16A-00C0F0283628} type: ListView */
      buttonicons:NAME = "buttonicons":U .
/* buttonicons OCXINFO:CREATE-CONTROL from: {2C247F23-8591-11D1-B16A-00C0F0283628} type: ImageList */
      ImageLoader:NAME = "ImageLoader":U .
/* ImageLoader OCXINFO:CREATE-CONTROL from: {0414CF80-543F-11D1-ABF8-0000C0E7C6DF} type: ImageLoader */
      SmallIcons:NAME = "SmallIcons":U .
/* SmallIcons OCXINFO:CREATE-CONTROL from: {2C247F23-8591-11D1-B16A-00C0F0283628} type: ImageList */
      LargeIcons:NAME = "LargeIcons":U .
/* LargeIcons OCXINFO:CREATE-CONTROL from: {2C247F23-8591-11D1-B16A-00C0F0283628} type: ImageList */
      ListView:MOVE-AFTER(Rs-style:HANDLE IN FRAME DEFAULT-FRAME).
      buttonicons:MOVE-AFTER(lv-text:HANDLE IN FRAME DEFAULT-FRAME).
      ImageLoader:MOVE-AFTER(buttonicons).
      SmallIcons:MOVE-AFTER(ImageLoader).
      LargeIcons:MOVE-AFTER(SmallIcons).

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


&Scoped-define SELF-NAME Btn-back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-back C-Win
ON CHOOSE OF Btn-back IN FRAME DEFAULT-FRAME /* Back */
DO:
  lv-parent[lv-level] = ''.
  lv-level = lv-level - 1.
  if lv-level = 1 then self:sensitive = false.
  RUN pop-list in this-procedure (lv-parent[lv-level]).
  run disp-info (ch-listview:selecteditem:tag).

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


&Scoped-define SELF-NAME BTN-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-ok C-Win
ON CHOOSE OF BTN-ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
   assign
        lv-user = lc(trim(lv-user:screen-value)) 
        lv-pass = lc(trim(lv-pass:screen-value)).

    if not setuserid(lv-user,lv-pass,ldbname(1)) then do:
        Lv-tries = lv-tries + 1.
        message 'Invalid Entry' view-as alert-box error .
        if lv-tries > 2 then quit.
        apply 'entry' to lv-user.
        return no-apply.
    end.
    else do:
        assign 
            z-user = userid('charisma')
            lv-svar = 'user'
            lv-mode = 'write'.
        {zen/sysvars.i lv-mode lv-svar z-user}
        if session:display-type = 'gui' then do: 
                {&window-name}:private-data = session:parameter.
            run get-hdr in h-library (input {&window-name}:handle).       
        end.
        RUN pop-list in this-procedure (" ") .
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ListView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ListView C-Win
PROCEDURE ListView.ListView.MouseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    x
    y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-x      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-y      AS INTEGER NO-UNDO.

run disp-info (ch-listview:selecteditem:tag).

case p-button:
    when 1 then do: /* left button */
        run list-action (this-procedure,ch-listview:selecteditem:tag).
    end.
    when 2 then do: /* right button */
    end.
end case.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs-style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs-style C-Win
ON VALUE-CHANGED OF Rs-style IN FRAME DEFAULT-FRAME
DO:
  assign rs-style.
  ch-listview:view = rs-style.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Toolbar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Toolbar C-Win
PROCEDURE Toolbar.Toolbar.ButtonClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS COM-HANDLE NO-UNDO.

message p-button:index view-as alert-box.

case p-button:index:
    
end case.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

lv-svar = 'user'.
{zen/sysvars.i lv-mode lv-svar z-user}
lv-svar = 'system'.
{zen/sysvars.i lv-mode lv-svar z-system}
lv-svar = 'language'.
{zen/sysvars.i lv-mode lv-svar z-lancod}
lv-svar = 'extra'.
{zen/sysvars.i lv-mode lv-svar vretextra}
lv-svar = 'retcode'.
{zen/sysvars.i lv-mode lv-svar vretcode}

{zen/pgm-hdr.i}

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

    {zen/sec-chk.i}

    RUN enable_UI.

    {zen/wid-chk.i}  

    {zen/focus.i}

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.
if SESSION:DISPLAY-TYPE ne "GUI":U then run tidy.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE back C-Win 
PROCEDURE back :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  lv-parent[lv-level] = ''.
  lv-level = lv-level - 1.
  if lv-level = 1 then self:sensitive = false.
  RUN pop-list in this-procedure (lv-parent[lv-level]).
  run disp-info (ch-listview:selecteditem:tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-butttons C-Win 
PROCEDURE build-butttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var x as int no-undo init 1.
def var but-list as char no-undo initial
        "ok-su.bmp,undo-su.bmp,canc-su.bmp".
def var ch-but as com-handle no-undo.

ch-but = chToolbar:Toolbar:buttons.
chToolbar:Toolbar:ImageList = chsmallicons:ImageList.   

do x = 1 to num-entries(but-list):
    chimageloader:ImageLoader:loadimage(chbuttonicons:ImageList,
                                  'zen\grafix\' + entry(x,but-list)).
    ch-but:add(,,,,x).
end.

chToolbar:Toolbar:ImageList = chbuttonicons:ImageList. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-list C-Win 
PROCEDURE build-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  move this stuff to a persistent library
------------------------------------------------------------------------------*/
def input param pv-handle as com-handle no-undo.
def input param pv-index  as int        no-undo.
def input param pv-text   as char       no-undo.
def input param pv-action as char       no-undo.
def input param pv-icon   as int        no-undo.

    pv-handle:ListItems:Add(pv-index,,pv-text,,).
    pv-handle:listitems(pv-index):tag = pv-action.
    pv-handle:listitems(pv-index):icon = pv-icon.
    pv-handle:listitems(pv-index):smallicon = pv-icon.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-procedure C-Win 
PROCEDURE close-procedure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  apply 'close' to this-procedure.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win _CONTROL-LOAD
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

OCXFile = SEARCH( "menu.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chbuttonicons = buttonicons:COM-HANDLE
    UIB_S = chbuttonicons:LoadControls( OCXFile, "buttonicons":U)
    chImageLoader = ImageLoader:COM-HANDLE
    UIB_S = chImageLoader:LoadControls( OCXFile, "ImageLoader":U)
    chLargeIcons = LargeIcons:COM-HANDLE
    UIB_S = chLargeIcons:LoadControls( OCXFile, "LargeIcons":U)
    chListView = ListView:COM-HANDLE
    UIB_S = chListView:LoadControls( OCXFile, "ListView":U)
    chSmallIcons = SmallIcons:COM-HANDLE
    UIB_S = chSmallIcons:LoadControls( OCXFile, "SmallIcons":U)
    chToolbar = Toolbar:COM-HANDLE
    UIB_S = chToolbar:LoadControls( OCXFile, "Toolbar":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "menu.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disp-info C-Win 
PROCEDURE disp-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-action as char no-undo.

def var lv-mode   as char no-undo init 'read'.
def var lv-sysvar as char no-undo init 'system'.
def var z-system  as char no-undo.
def var v-id                   as int no-undo.
def var lv-path as char no-undo.
lv-path = if Rs-style = 2 then 'zen\grafix\small\' 
                          else 'zen\grafix\large\'.
                          
   {zen/sysvars.i lv-mode lv-sysvar z-system}

    v-id = int(pv-action).

    {zen/find.i &table = "zen-dmenu"
                &where = "where zen-dmenu.menu-system contains z-system 
                                    and zen-dmenu.menu-id     = v-id"
                &type  = "first"            
                &lock  = "no"}
do with frame {&frame-name}:
    lv-image:load-image(lv-path + zen-dmenu.icon).
    lv-image:hidden = false.
    lv-text:screen-value = 'Stored Information About ' + zen-dmenu.menu-name.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  DISPLAY lv-user lv-pass Rs-style lv-text 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE lv-user lv-pass BTN-ok Btn-exit Rs-style 
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
    ch-listview = chlistview:ListView.
   
    run pop-imagelist.
    
    RUN build-butttons.
    
    ch-listview:icons      = chlargeicons:ImageList.
    ch-listview:smallicons = chsmallicons:ImageList.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE list-action C-Win 
PROCEDURE list-action :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       move to persistent library
------------------------------------------------------------------------------*/
def input param pv-called-from as handle no-undo.
def input param pv-action as char no-undo.

def var lv-mode   as char no-undo init 'read'.
def var lv-sysvar as char no-undo init 'system'.
def var z-system  as char no-undo.

    def var lv-menu-out            as char no-undo.     
    def var lv-menu-ios            as char no-undo.
    def var v-id                   as int no-undo.
    def var v-sys                  as char no-undo.
    def buffer bmenu for zen-dmenu.
    
    {zen/sysvars.i lv-mode lv-sysvar z-system}

    v-id = int(pv-action).

    def var lv-menu-bar as handle no-undo.
    {zen/find.i &table = "zen-dmenu"
                &where = "where zen-dmenu.menu-system contains z-system 
                            and zen-dmenu.menu-id     = v-id"
                &type  = "first"            
                &lock  = "no"}
                
      {zen/find.i &table = "bmenu"
                &where = "where bmenu.menu-system contains z-system
                            and bmenu.menu-item = zen-dmenu.menu-action"
                 &type  = "first"            
                &lock  = "no"}
    if avail bmenu then do:
        lv-parent[lv-level] = zen-dmenu.menu-item.    
        lv-level = lv-level + 1.
        btn-back:sensitive in frame {&frame-name} = true.
        RUN pop-list in this-procedure (zen-dmenu.menu-action).
        run disp-info (ch-listview:selecteditem:tag).
        return.
    end.          
    assign
        lv-menu-out = zen-dmenu.menu-output
        lv-menu-ios = zen-dmenu.menu-ios.

     if search(zen-dmenu.menu-action) ne ? or
        search(substring(zen-dmenu.menu-action,1,index(zen-dmenu.menu-action,'.')) + 'r') ne ? then do: 

           {zen/find.i &table = "zen-dpgm"
                      &where = "where zen-dpgm.system contains z-system
                                  and zen-dpgm.pgm = zen-dmenu.menu-action"
                      &type  = "first"            
                      &lock  = "no"}

            if avail zen-dpgm and zen-dpgm.min-on-run then do:
                   active-window:window-state = 2.
            end.
        if zen-dmenu.menu-pers then do:
           if zen-dmenu.menu-input = '' and zen-dmenu.menu-output = '' and zen-dmenu.menu-ios = '' then 
                run value(zen-dmenu.menu-action) persistent.
            else if zen-dmenu.menu-input ne '' and zen-dmenu.menu-output ne '' then   
                run value(zen-dmenu.menu-action) persistent (input zen-dmenu.menu-input,
                                             output lv-menu-out).
            else if zen-dmenu.menu-input ne '' then
                run value(zen-dmenu.menu-action) persistent (input zen-dmenu.menu-input).
            else if zen-dmenu.menu-output ne '' then 
                run value(zen-dmenu.menu-action) persistent (output lv-menu-out).            
            else if zen-dmenu.menu-ios ne '' then 
                run value(zen-dmenu.menu-action) persistent (input-output lv-menu-ios).            
        end.
        else do: 
            if zen-dmenu.menu-input = '' and zen-dmenu.menu-output = '' and zen-dmenu.menu-ios = '' then 
                run value(zen-dmenu.menu-action).
            else if zen-dmenu.menu-input ne '' and zen-dmenu.menu-output ne '' then   
                run value(zen-dmenu.menu-action) ( input zen-dmenu.menu-input,
                                               output lv-menu-out).
            else if zen-dmenu.menu-input ne '' then 
                run value(zen-dmenu.menu-action) (input zen-dmenu.menu-input).
            else if zen-dmenu.menu-output ne '' then 
                run value(zen-dmenu.menu-action) (output lv-menu-out).
            else if zen-dmenu.menu-ios ne '' then 
                run value(zen-dmenu.menu-action) (input-output lv-menu-ios).            
        end.
     end. 
     else 
        if can-do(pv-called-from:internal-entries,zen-dmenu.menu-action) then do:
          if zen-dmenu.menu-input ne '' and zen-dmenu.menu-output ne '' then  
               run value(zen-dmenu.menu-action) in pv-called-from (input zen-dmenu.menu-input, 
                                                               output lv-menu-out).
          else if zen-dmenu.menu-input = '' and zen-dmenu.menu-output = '' then      
               run value(zen-dmenu.menu-action) in pv-called-from. 
          else if zen-dmenu.menu-input ne '' then
               run value(zen-dmenu.menu-action) in pv-called-from (input zen-dmenu.menu-input) .
          else if zen-dmenu.menu-output ne '' then 
               run value(zen-dmenu.menu-action) in pv-called-from (output lv-menu-out).   
        end.  
     else
        message zen-dmenu.menu-action 'Cannot Be Found'
            view-as alert-box error.

     if avail zen-dpgm and not zen-dmenu.menu-pers and zen-dpgm.min-on-run then do:
                 active-window:window-state = 3.
     end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok C-Win 
PROCEDURE ok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign
        lv-user = lc(trim(lv-user:screen-value in frame {&frame-name})) 
        lv-pass = lc(trim(lv-pass:screen-value)).

    if not setuserid(lv-user,lv-pass,ldbname(1)) then do:
        Lv-tries = lv-tries + 1.
        message 'Invalid Entry' view-as alert-box error .
        if lv-tries > 2 then quit.
        apply 'entry' to lv-user.
        return no-apply.
    end.
    else do:
        assign 
            z-user = userid('charisma')
            lv-svar = 'user'
            lv-mode = 'write'.
        {zen/sysvars.i lv-mode lv-svar z-user}
        if session:display-type = 'gui' then do: 
                {&window-name}:private-data = session:parameter.
            run get-hdr in h-library (input {&window-name}:handle).       
        end.
        RUN pop-list in this-procedure (" ") .
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pop-imagelist C-Win 
PROCEDURE pop-imagelist :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       move to library
------------------------------------------------------------------------------*/

for each zen-icons no-lock:
    chimageloader:ImageLoader:loadimage(chlargeicons:ImageList,
                                       'zen\grafix\large\' + zen-icons.Icon).
    chimageloader:ImageLoader:loadimage(chsmallicons:ImageList,
                                       'zen\grafix\small\' + zen-icons.Icon).
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pop-list C-Win 
PROCEDURE pop-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       

------------------------------------------------------------------------------*/
def input param pv-start as char no-undo.

def var x           as int           no-undo.
def var lv-can-menu as log init true no-undo.

def buffer bmenu for zen-dmenu.
def buffer cmenu for zen-dmenu.
    
ch-ListView:listitems:clear.
{zen/find.i &table = "zen-duser"
            &where = "where  zen-duser.system contains z-system
                       and  zen-duser.duser = z-user"
            &type  = "first"              
            &lock  = "no"}
            
for each bmenu where bmenu.menu-system contains z-system
                 and bmenu.menu-item = pv-start no-lock
                by menu-id:
    /* menu user security */
   lv-can-menu = true.
   if avail zen-duser then do:
       if can-do(bmenu.not-users,z-user) or
           can-do(bmenu.not-group,zen-duser.U-GROUP) 
          Then lv-can-menu = false.
          else    
             if not can-do(bmenu.run-users,z-user) or
               not can-do(bmenu.run-groups,zen-duser.U-GROUP)
             then lv-can-menu = false.
    end.
    if not lv-can-menu or
       bmenu.menu-name begins '<'
    then next.
    
    /* check its not a sub menu itself */
    {zen/find.i &table = "cmenu"
                &where = "where cmenu.menu-system contains z-system
                          and  cmenu.menu-item = bmenu.menu-action"
                 &type  = "first"            
                &lock  = "no"}
    x = x + 1.
    if not avail cmenu 
    then run build-list(ch-ListView,x,bmenu.menu-name,string(bmenu.menu-id),2).
    else run build-list(ch-ListView,x,bmenu.menu-name,string(bmenu.menu-id),1).
end.    
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


