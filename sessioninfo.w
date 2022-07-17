&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/******************************************************************************/
/*  PROGRAM ID.     : OPSYSProcsWinView.w                                     */
/*  PROGRAM TITLE   : Lists all persistent procedures currently running       */
/*                    within OPSYS, both locally and on the AppServer.        */
/*                    Also displays local procedure info where possible.      */
/*                                                                            */
/*                    Based on pprocs.w written by Phil White                 */
/*                                                                            */
/*  CREATE DATE     : 12/01/96                                                */
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
/* 12/01/96  p00    philw   00  initial release                               */
/* 12/07/99         andrewb     Modified for use with OPSYS                   */
/******************************************************************************/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
&glob KeepRefreshButton
/* ***************************  Definitions  ************************** */
{app-paths.i}
&undefine suppresswindow
define temp-table t-data no-undo
    field t-proc as char
    field t-intproc as char
    field t-params as char extent 100
    index order t-proc t-intproc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-type sel-procs sel-int-procs sel-params ~
sl-mess Btn-sesion Btn-progs Btn-servers btn-info Btn-Clear Btn-unload ~
Btn-cache 
&Scoped-Define DISPLAYED-OBJECTS rs-type sel-procs sel-int-procs sel-params ~
sl-mess 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD selectentries WINDOW-1 
FUNCTION selectentries RETURNS CHARACTER
  ( input pv-list as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-cache 
     LABEL "Refresh Cache" 
     SIZE 21 BY 1.14.

DEFINE BUTTON Btn-Clear 
     LABEL "Clear Programs" 
     SIZE 21 BY 1.14.

DEFINE BUTTON btn-info 
     LABEL "Appsrv Info" 
     SIZE 21 BY 1.14.

DEFINE BUTTON Btn-progs 
     LABEL "Running Progs" 
     SIZE 21 BY 1.14.

DEFINE BUTTON Btn-servers 
     LABEL "Last Server" 
     SIZE 21 BY 1.14.

DEFINE BUTTON Btn-sesion 
     LABEL "Session Params" 
     SIZE 21 BY 1.14.

DEFINE BUTTON Btn-unload 
     LABEL "Unload Selected" 
     SIZE 21 BY 1.14.

DEFINE VARIABLE rs-type AS CHARACTER INITIAL "Local" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "All", "all",
"Ext", "ext",
"Func", "Func",
"Proc", "Proc",
"Local", "Local"
     SIZE 43 BY .71 NO-UNDO.

DEFINE VARIABLE sel-int-procs AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SORT SCROLLBAR-VERTICAL 
     SIZE 40.4 BY 16.91 NO-UNDO.

DEFINE VARIABLE sel-params AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 39 BY 5.48
     FONT 0 NO-UNDO.

DEFINE VARIABLE sel-procs AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 50.4 BY 16.91 NO-UNDO.

DEFINE VARIABLE sl-mess AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 50 BY 16.91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     rs-type AT ROW 1.24 COL 61 NO-LABEL
     sel-procs AT ROW 2.19 COL 10.2 NO-LABEL
     sel-int-procs AT ROW 2.19 COL 62.4 NO-LABEL
     sel-params AT ROW 2.19 COL 104.8 NO-LABEL
     sl-mess AT ROW 2.19 COL 145 NO-LABEL WIDGET-ID 12
     Btn-sesion AT ROW 10.76 COL 123 WIDGET-ID 2
     Btn-progs AT ROW 11.95 COL 123 WIDGET-ID 4
     Btn-servers AT ROW 13.14 COL 123 WIDGET-ID 6
     btn-info AT ROW 14.33 COL 123 WIDGET-ID 14
     Btn-Clear AT ROW 15.52 COL 123 WIDGET-ID 8
     Btn-unload AT ROW 16.71 COL 123 WIDGET-ID 16
     Btn-cache AT ROW 17.91 COL 123 WIDGET-ID 18
     "Containing" VIEW-AS TEXT
          SIZE 12.8 BY .62 AT ROW 1.24 COL 47 WIDGET-ID 22
     "Appserver Session Details" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 1.24 COL 146 WIDGET-ID 20
     "Session Persistent Programs" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 1.24 COL 11
     "Procedure Details And Parameters" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 1.24 COL 107.2
     "Appserver Session" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 9.57 COL 124 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 195.4 BY 18.38.


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
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Session Information"
         COLUMN             = 32.2
         ROW                = 8.38
         HEIGHT             = 18.38
         WIDTH              = 195.4
         MAX-HEIGHT         = 19.81
         MAX-WIDTH          = 195.4
         VIRTUAL-HEIGHT     = 19.81
         VIRTUAL-WIDTH      = 195.4
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT WINDOW-1:LOAD-ICON("adeicon\prospy":U) THEN
    MESSAGE "Unable to load icon: adeicon\prospy"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-cache
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-cache WINDOW-1
ON CHOOSE OF Btn-cache IN FRAME FRAME-A /* Refresh Cache */
do:
        {{&core}run.i &program   = "zenlibrary.p"
                        &path      = "{&core}{&lib}"
                        &Appsrv    = "System"
                        &procedure = "PopulateTempTables"}

  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Clear WINDOW-1
ON choose OF Btn-Clear IN FRAME FRAME-A /* Clear Programs */
do:
  def var lv-params as char no-undo.
   {{&core}run.i &program   = "clearall.p"
                        &path      = "{&core}{&srv}"
                        &Appsrv    = "System"
                        &procedure = "clearallprocs"
                        &params    = "~{~}"}
message 'done see log'.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-info WINDOW-1
ON choose OF btn-info IN FRAME FRAME-A /* Appsrv Info */
do:
   def var lv-params as char no-undo.
   {{&core}run.i &program   = "clearall.p"
                        &path      = "{&core}{&srv}"
                        &Appsrv    = "System"
                        &procedure = "AppserverInfo"
                        &params    = "(output lv-params)"}
   sl-mess:list-items = lv-params.

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-progs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-progs WINDOW-1
ON choose OF Btn-progs IN FRAME FRAME-A /* Running Progs */
do:
  def var lv-params as char no-undo.
   {{&core}run.i &program   = "clearall.p"
                        &path      = "{&core}{&srv}"
                        &Appsrv    = "System"
                        &procedure = "whatsrunning"
                        &params    = "(output lv-params)"}
sl-mess:list-items = lv-params.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-servers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-servers WINDOW-1
ON choose OF Btn-servers IN FRAME FRAME-A /* Last Server */
do:
  def var lv-last as log no-undo.
  def var lv-params as char no-undo.
   {{&core}run.i &program   = "clearall.p"
                        &path      = "{&core}{&srv}"
                        &Appsrv    = "System"
                        &procedure = "lastserverrunning"
                        &params    = "(output lv-last,output lv-params)"}
lv-params = 'Last Server ' + string(lv-last) + ',' + lv-params.
sl-mess:list-items = lv-params.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-sesion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-sesion WINDOW-1
ON choose OF Btn-sesion IN FRAME FRAME-A /* Session Params */
do:
  def var lv-params as char no-undo.
   {{&core}run.i &program   = "clearall.p"
                        &path      = "{&core}{&srv}"
                        &Appsrv    = "System"
                        &procedure = "sessionparams"
                        &params    = "(output lv-params)"}
   sl-mess:list-items = lv-params.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-unload
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-unload WINDOW-1
ON choose OF Btn-unload IN FRAME FRAME-A /* Unload Selected */
do:
/* need to run it once for each running appserver !! */
/* not a prob in dev as we only have 2 */

  run unloadprogs.
  run unloadprogs.
                      
  apply 'choose' to btn-progs.                     
  message 'done see log'.
  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-type WINDOW-1
ON VALUE-CHANGED OF rs-type IN FRAME FRAME-A
DO:
  apply 'value-changed' to sel-procs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sel-int-procs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sel-int-procs WINDOW-1
ON VALUE-CHANGED OF sel-int-procs IN FRAME FRAME-A
DO:
   def var lv-param-list as char no-undo.
   
   run getinfo ('params',sel-procs:handle,output lv-param-list).

   sel-params:list-items = lv-param-list.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sel-procs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sel-procs WINDOW-1
ON VALUE-CHANGED OF sel-procs IN FRAME FRAME-A
DO:
    def var lv-proclist as char no-undo.

    run getinfo ('procs',self:handle,output lv-proclist).
    setsensitive(true,'inc','btn-delete,btn-print',frame {&frame-name}:handle).
    sel-int-procs:list-items = lv-proclist.
    apply 'value-changed' to sel-int-procs.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


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
    run initialise.
    
  IF NOT THIS-PROCEDURE:PERSISTENT or SESSION:DISPLAY-TYPE ne "GUI"
  tHEN WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete-trigger WINDOW-1 
PROCEDURE Delete-trigger :
def var h    as handle no-undo.
    def var v-ok as log no-undo.
    
    h = widget-handle(entry(sel-procs:lookup(sel-procs:screen-value) in frame {&frame-name}, sel-procs:private-data,'{&combodelim}')).
        
    message 'Delete Instance of Procedure ' h:file-name 
    view-as alert-box question buttons yes-no update v-ok.
    
    if v-ok then do:
        delete procedure h.    
        run load-procs.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY rs-type sel-procs sel-int-procs sel-params sl-mess 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE rs-type sel-procs sel-int-procs sel-params sl-mess Btn-sesion 
         Btn-progs Btn-servers btn-info Btn-Clear Btn-unload Btn-cache 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW WINDOW-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exit-trigger WINDOW-1 
PROCEDURE exit-trigger :
apply "close":u to this-procedure.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getinfo WINDOW-1 
PROCEDURE getinfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-type as char   no-undo.
def input  param pv-Sel  as handle no-undo.    
def output param pv-list as char   no-undo.

def var h as handle no-undo.
def var x as int no-undo.
def var y as int no-undo.
def var z as int no-undo.
def var lv-proclist  as char no-undo.
def var lv-paramlist as char no-undo.

h = widget-handle(entry(pv-sel:lookup(pv-sel:screen-value),pv-sel:private-data,'{&combodelim}')).
case pv-type:
    when 'procs' then do:
        pv-list = replace(selectentries(h:internal-entries),',','{&combodelim}').
    end.                              
    WHEN 'params' then pv-list = if h:internal-entries = ? 
                                  then "** None{&combodelim}** Details Not Available"
                                  else replace(h:get-signature(self:screen-value),',','{&combodelim}').
    when 'print' then do:
        do x = 1 to pv-sel:num-items:
            h = widget-handle(entry(x,pv-sel:private-data,'{&combodelim}')).
            if h:internal-entries ne ? then do:
                lv-proclist = selectentries(h:internal-entries).
                do y = 1 to num-entries(lv-proclist,'{&combodelim}'):
                    lv-paramlist = h:get-signature(entry(y,lv-proclist,'{&combodelim}')).
                    
                    if entry(1,lv-paramlist,'{&combodelim}') = 'EXTERN' then
                       if can-find(first t-data where t-data.t-intproc = entry(y,lv-proclist,'{&combodelim}'))
                       then next.
                    
                    if can-find(first t-data where t-data.t-proc    = entry(x,pv-sel:list-items,'{&combodelim}')
                                               and t-data.t-intproc = entry(y,lv-proclist,'{&combodelim}'))
                    then next.
                    
                    create t-data.
                    assign t-data.t-proc    = entry(x,pv-sel:list-items,'{&combodelim}')
                           t-data.t-intproc = entry(y,lv-proclist,'{&combodelim}').
                    do z = 1 to num-entries(lv-paramlist,'{&combodelim}'):
                        t-data.t-params[z]  = entry(z,lv-paramlist,'{&combodelim}').
                    end.
                end.
            end.
        end.
    end.
end case.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise WINDOW-1 
PROCEDURE initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

          /*sensitive,visible,overlay */        
              /*horizontal,flat,startcol,startrow,height,width */  

    
    createbuts ("Delete^ttf,Query^ttf,Print^ttf,Exit^ttf{&Delim2}" + 
string(this-procedure) + "," +
                      string({&window-name}:handle) + "," +
                      string(frame {&frame-name}:handle) + "," + 
                      "{&btnhorizontal},{&btnflat},{&btnstartcol},{&btnstartrow},{&btnheight},{&btnwidth},{&btncenter}").
run load-procs.
sl-mess:delimiter = ','.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-procs WINDOW-1 
PROCEDURE load-procs :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

    def var h       as handle no-undo.
    def var lv-context as char no-undo. 
def var h-appserver as handle no-undo.
    assign h = session:first-procedure
           sel-procs:list-items in frame {&FRAME-NAME} = ?
           sel-procs:private-data = "".

    run walktree (h).       

    h-appserver = getappserverhandle("system") no-error.
    IF VALID-HANDLE(h-appserver) 
    THEN DO:
        h = h-appserver:FIRST-PROCEDURE.
        run walktree (h).
    END.

    if num-entries(sel-procs:list-items,'{&combodelim}') > 0 
        then sel-procs:screen-value = entry(1, sel-procs:list-items,'{&combodelim}').

    apply "value-changed":u to sel-procs.         

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Print-trigger WINDOW-1 
PROCEDURE Print-trigger :
def var lv-list as char no-undo.
def var x as int no-undo.

run getinfo ('print',sel-procs:handle in frame {&frame-name},output lv-list).
     
output stream op to 'InternalCalls.dat'.

for each t-data:
    sysmsg('Processing ' + t-data.t-proc).
    put stream op unformatted t-data.t-proc '{&Delim2}' t-data.t-intproc '{&Delim2}' .
    do x = 1 to 100:
        if t-data.t-params[x] ne '' then
            put stream op unformatted t-data.t-params[x] '{&Delim2}'.
    end.
    put stream op unformatted skip.
end.
output stream op close.
sysmsg('off').
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE query-trigger WINDOW-1 
PROCEDURE query-trigger :
run load-procs. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetLastFocus WINDOW-1 
PROCEDURE SetLastFocus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnloadProgs WINDOW-1 
PROCEDURE UnloadProgs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var x as int no-undo.
def var lv-params as char no-undo.
 
  do X = 1 to sl-mess:num-items in frame {&frame-name}:
   if sl-mess:is-selected(x) then
       LV-PARaMS = lv-params + entry(x,sl-mess:list-items) + ','.
  end.
  lv-params = substring(lv-params,1,length(lv-params) - 1).

    {{&core}run.i &program   = "clearall.p"
                        &path      = "{&core}{&srv}"
                        &Appsrv    = "System"
                        &procedure = "unloadprocs"
                        &params    = "(lv-params)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE walktree WINDOW-1 
PROCEDURE walktree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param h as handle no-undo.

def var v-fname as char   no-undo.
def var v-pos   as int    no-undo.

      do while valid-handle(h):   
            assign v-pos   = r-index(h:file-name, "\")
                   v-pos   = v-pos + 1
                   v-fname = substring(h:file-name, v-pos).
        
            if sel-procs:list-items in frame {&frame-name} = ? then
                assign sel-procs:list-items   = v-fname
                       sel-procs:private-data = string(h).
            else
                assign sel-procs:list-items   = sel-procs:list-items + '{&combodelim}' + v-fname
                       sel-procs:private-data = sel-procs:private-data + '{&combodelim}' + string(h).          
            h = h:next-sibling.
        end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION selectentries WINDOW-1 
FUNCTION selectentries RETURNS CHARACTER
  ( input pv-list as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
def var lv-list       as char no-undo.
def var lv-param-list as char no-undo.
def var x             as int  no-undo.
def var h as handle no-undo.
assign frame {&frame-name} rs-type.
if pv-list = ? 
    then lv-list =  "** AppServer Persistent Procedure" + '{&combodelim}' + "** Details Not Available".
else do:
    assign frame {&frame-name} rs-type
           h = widget-handle(entry(sel-procs:lookup(sel-procs:screen-value),sel-procs:private-data,'{&combodelim}')).          
    case rs-type:
        when 'all' then lv-list = pv-list.
        when 'local' then 
            do x = 1 to num-entries(pv-list):
                lv-param-list = h:get-signature(entry(x,pv-list)).
                if entry(1,lv-param-list) begins 'ext' then next.
                if lv-list = "" then lv-list = entry(x,pv-list).
                                else if index(lv-list,entry(x,pv-list)) = 0
                                    then lv-list = lv-list + '{&combodelim}' + entry(x,pv-list).   
            end.
        when 'proc' or
        when 'func' or
        when 'ext' then
             do x = 1 to num-entries(pv-list):
                lv-param-list = h:get-signature(entry(x,pv-list)).
                if not entry(1,lv-param-list) begins rs-type then next.
                if lv-list = "" then lv-list = entry(x,pv-list).
                                else if index(lv-list,entry(x,pv-list)) = 0
                                    then lv-list = lv-list + '{&combodelim}' + entry(x,pv-list).   
            end.
    end case.
end.

RETURN lv-list.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

