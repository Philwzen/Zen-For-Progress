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

/* &glob top */

def var lv-page as int no-undo init 1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-msgbx lv-tables btn-run lv-topwhere ~
Btn-all lv-keys btn-exit lv-Returning 
&Scoped-Define DISPLAYED-OBJECTS lv-msgbx lv-tables lv-topwhere lv-keys ~
lv-Returning 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-tables lv-topwhere lv-keys lv-Returning 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsgBx C-Win 
FUNCTION MsgBx RETURNS CHARACTER
  ( pv-msg as char  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-all 
     LABEL "All" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-exit AUTO-END-KEY 
     LABEL "exit" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-run 
     LABEL "run" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-msgbx AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 158 BY 2.14 NO-UNDO.

DEFINE VARIABLE lv-keys AS CHARACTER FORMAT "X(256)":U INITIAL "locationtableid,locationtableid" 
     LABEL "Key fields" 
     VIEW-AS FILL-IN 
     SIZE 79 BY 1 NO-UNDO.

DEFINE VARIABLE lv-Returning AS CHARACTER FORMAT "X(256)":U 
     LABEL "Return fields" 
     VIEW-AS FILL-IN 
     SIZE 79 BY 1 NO-UNDO.

DEFINE VARIABLE lv-tables AS CHARACTER FORMAT "X(256)":U INITIAL "perloc,location" 
     LABEL "Tables" 
     VIEW-AS FILL-IN 
     SIZE 79 BY 1 NO-UNDO.

DEFINE VARIABLE lv-topwhere AS CHARACTER FORMAT "X(256)":U INITIAL "where true, of perloc" 
     LABEL "Top level Where" 
     VIEW-AS FILL-IN 
     SIZE 79 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lv-msgbx AT ROW 13.86 COL 2 NO-LABEL WIDGET-ID 26
     lv-tables AT ROW 16.24 COL 19 COLON-ALIGNED WIDGET-ID 8
     btn-run AT ROW 16.48 COL 134.8 WIDGET-ID 6
     lv-topwhere AT ROW 17.43 COL 19 COLON-ALIGNED WIDGET-ID 10
     Btn-all AT ROW 18.14 COL 134.8 WIDGET-ID 24
     lv-keys AT ROW 18.62 COL 19 COLON-ALIGNED WIDGET-ID 12
     btn-exit AT ROW 19.76 COL 134.8 WIDGET-ID 4
     lv-Returning AT ROW 19.81 COL 19 COLON-ALIGNED WIDGET-ID 14
     "zen-auditconfig,zen-auditfield" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 16.48 COL 101 WIDGET-ID 16
     "where tablename = 'zen-dpgm'" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 17.67 COL 101 WIDGET-ID 18
     "tablename,tablename" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 18.86 COL 101 WIDGET-ID 20
     "*~;* empty means all" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 20.05 COL 101 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.2 BY 20.33
         CANCEL-BUTTON btn-exit WIDGET-ID 100.


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
         TITLE              = "Data Viewer"
         HEIGHT             = 27.05
         WIDTH              = 160.2
         MAX-HEIGHT         = 27.05
         MAX-WIDTH          = 160.2
         VIRTUAL-HEIGHT     = 27.05
         VIRTUAL-WIDTH      = 160.2
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
/* SETTINGS FOR FILL-IN lv-keys IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-Returning IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-tables IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-topwhere IN FRAME DEFAULT-FRAME
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Data Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Data Viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-all C-Win
ON CHOOSE OF Btn-all IN FRAME DEFAULT-FRAME /* All */
DO:
  run GetData (?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-exit C-Win
ON CHOOSE OF btn-exit IN FRAME DEFAULT-FRAME /* exit */
DO:
  apply 'close' to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-run
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-run C-Win
ON CHOOSE OF btn-run IN FRAME DEFAULT-FRAME /* run */
DO:
    assign frame {&frame-name}
         {&list-1}
         lv-page  = 1.
  run GetData(1).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefineBrowse C-Win 
PROCEDURE DefineBrowse :
def input-output param h-browse as handle no-undo.

    CREATE BROWSE h-browse ASSIGN
        FRAME = FRAME {&frame-name}:HANDLE
        HIDDEN = NO
        NO-VALIDATE = YES
        WIDTH = 120
        HEIGHT = 18
        x = 88
        y = 18
        column-resizable = true 
        allow-column-searching = true
        SEPARATORS = YES
        SENSITIVE = YES
        triggers:
                ON 'value-changed' persistent run value-changedTrigger in this-procedure.
                on 'off-end' persistent run off-end-trigger in this-procedure.
                ON 'START-SEARCH' persistent run sortbrowse in this-procedure.
                on 'off-home' persistent run off-home-trigger in this-procedure.
        end triggers.
   
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
  DISPLAY lv-msgbx lv-tables lv-topwhere lv-keys lv-Returning 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE lv-msgbx lv-tables btn-run lv-topwhere Btn-all lv-keys btn-exit 
         lv-Returning 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillDataset C-Win 
PROCEDURE FillDataset :
def input param pv-page as int no-undo.
def input param pv-tables as char no-undo.
def input param pv-where as char no-undo.
def input param pv-relation as char no-undo.
def input param pv-fieldlist as char no-undo.
def input param pv-lookupname as char no-undo.
def output param DATASET-HANDLE pv-dataset.

/* example  
    ("zen-auditconfig,zen-auditfield",
     "where tablename = 'zen-dpgm'",
     "tablename,tablename",
     "!tstamp,!*tableid,*;!tstamp,!*tableid,*",
     OUTPUT DATASET-HANDLE h-dataset)
*/
def var lv-ok as log no-undo.
def var x as int no-undo.
def var y as int no-undo.
def var h-db            as handle no-undo.
def var h-field         as handle no-undo.
def var lv-fieldlist as char no-undo.
def var h-datasource as handle no-undo.
def var h-buffer as handle no-undo.
def var lv-idxname as char no-undo.
def var h-qry as handle no-undo.
def var h-tt     as handle extent 18 no-undo.
def var lv-batchsize as int64 no-undo init 25.
def var lv-rowid as rowid no-undo.
def var lv-restartrowid as rowid no-undo.
def var lv-idx as char no-undo.
def var lv-rep-pos as char no-undo.
def var lv-rep-value as char no-undo.
def var lv-where as char no-undo.    

if pv-page = ? 
    then assign lv-batchsize = 99999999999.
    else lv-batchsize = 20 /* int(getctrl('maxlistcount')) */ .

create dataset pv-dataset.

do x = 1 to NUM-ENTRIES(pv-tables):
    if pv-fieldlist = '*' or pv-fieldlist = '' 
    then lv-fieldlist = '!tstamp,*'.
    else lv-fieldlist = entry(x,pv-fieldlist,';').

    create buffer h-db for table ENTRY(x, pv-tables).
    create temp-table h-tt[x].
    
    h-tt[x]:create-like(ENTRY(x, pv-tables)). 
    
    h-tt[x]:TEMP-TABLE-PREPARE("t-" + ENTRY(x, pv-tables)).
    h-tt[x]:schema-marshal = 'FULL'.
    pv-dataset:ADD-BUFFER(h-tt[x]).
    if x > 1 and pv-relation ne ''
    then pv-dataset:ADD-RELATION(pv-dataset:GET-BUFFER-HANDLE(x - 1),
                                 pv-dataset:GET-BUFFER-HANDLE(x),
                                 pv-relation).
    lv-where = lv-where + ENTRY(x,pv-tables) + " " + ENTRY(x,pv-where) + ', each '.
end.             
/* if x > 1 then pv-dataset:GET-BUFFER-HANDLE(1):fill-mode = 'no-fill'. */

lv-where = "FOR EACH " + substring(lv-where,1,length(lv-where) - 7).
        
create query h-qry.
create data-source h-datasource.        
do x = 1 to NUM-ENTRIES(pv-tables):
    create buffer h-buffer for table ENTRY(x, pv-tables).
    h-datasource:ADD-SOURCE-BUFFER(h-buffer, ENTRY(x,pv-tables) + 'tableid').
    pv-dataset:GET-BUFFER-HANDLE(x):ATTACH-DATA-SOURCE(h-datasource).
    if x = 1 then 
        pv-dataset:GET-BUFFER-HANDLE(x):batch-size = lv-batchsize.
    h-qry:ADD-BUFFER(h-buffer). 
end.  

h-qry:QUERY-PREPARE(lv-where).

h-datasource:QUERY = h-qry. 

if pv-page = ? or pv-page = 0 
then do:
    h-qry:query-open().
    h-qry:get-first().
  /*  setsysvar("UpRestartRowid",string(h-buffer:rowid)). */
    h-datasource:restart-rowid = ?.
end.
else tranche: do:
    if pv-page > 0 then do:
/*         lv-restartrowid = to-rowid(getsysvar("DownRestartRowid")). */
        h-qry:query-open().   
        h-datasource:restart-rowid = lv-restartrowid.
    end.
    else do: /* yuck nasty nasty is there a better way ??? !!*/
/*         lv-restartrowid = to-rowid(getsysvar("UpRestartRowid")). */
        lv-idx = h-qry:index-information.
        h-qry:query-close().
        x = 1.
        find zen-fldlook where zen-fldlook.lookupname = pv-lookupname no-lock.
        lv-where = zen-fldlook.whereclause.
        do x = 1 to num-entries(zen-fldlook.wherefield):
             assign lv-rep-pos = "#" + string(x)
                    lv-rep-value = entry(x,zen-fldlook.wherefield).
                    lv-rep-value = "'" + dynamic-function(entry(1,lv-rep-value,'{&Delim2}'),
                                                          entry(2,lv-rep-value,'{&Delim2}')) + "'".
                    lv-where     = replace(lv-where,lv-rep-pos,LV-REP-VALUE).
        end.
        h-qry:QUERY-PREPARE("FOR EACH " + ENTRY(1, pv-tables) + " " + lv-where + " use-index " + lv-idx).
        h-datasource:QUERY = h-qry.
        h-qry:query-open().
        y = 0.
        lv-rowid = lv-restartrowid.
        h-qry:reposition-to-rowid(lv-restartrowid).  
        h-qry:get-prev(no-lock) no-error.        
        do while not h-qry:query-off-end:
            y = y + 1.            
            lv-rowid = h-buffer:rowid .
            if y = lv-batchsize then leave.
            h-qry:get-prev(no-lock) no-error.        
        end. 
        if lv-rowid = lv-restartrowid or y = 0
        then pv-dataset:GET-BUFFER-HANDLE(1):batch-size = 0.
        else do:
            h-datasource:restart-rowid = lv-rowid no-error.
            pv-dataset:GET-BUFFER-HANDLE(1):batch-size = y no-error.
        end.
    end.
end.

message  'Table  : ' pv-tables skip
         'Passed in Where: ' pv-where skip
         'up where : ' lv-where skip
         'Server prepare : ' h-qry:prepare-string skip
         'Server Index   : ' h-qry:index-information skip
         'page    : ' pv-page skip
         'results : ' h-qry:num-results skip
         'y : ' y skip
         'batchsize : ' pv-dataset:GET-BUFFER-HANDLE(1):batch-size skip
         {&dbt}.

if pv-dataset:GET-BUFFER-HANDLE(1):batch-size > 0 
then do:
    h-qry:get-first().
    do y = 1 to lv-batchsize:
        do x = 1 to h-qry:num-buffers:
            pv-dataset:GET-BUFFER-HANDLE(x):buffer-create().
            pv-dataset:GET-BUFFER-HANDLE(x):buffer-copy(h-qry:GET-BUFFER-HANDLE(x)).
        end.
        h-qry:get-next().
        if h-qry:query-off-end then leave.
        end.
  /*  pv-dataset:FILL(). */
 /*   if pv-page < 0 
    then setsysvar("UpRestartRowid",string(lv-rowid)).
    else setsysvar("DownRestartRowid",string(h-datasource:next-rowid)). */
end.

delete object h-qry no-error.
  
do x = 1 to pv-dataset:NUM-BUFFERS:
    h-buffer = pv-dataset:GET-BUFFER-HANDLE(x).
    delete object h-buffer:DATA-SOURCE no-error.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillDataSetByPage C-Win 
PROCEDURE FillDataSetByPage :
def input param pv-page as int no-undo.
def input param pv-tables as char no-undo.
def input param pv-where as char no-undo.
def input param pv-relation as char no-undo.
def input param pv-fieldlist as char no-undo.
def output param DATASET-HANDLE pv-dataset.

def var x as int no-undo.
def var y as int no-undo.
def var h-db            as handle no-undo.
def var h-field         as handle no-undo.
def var lv-fieldlist as char no-undo.
def var h-datasource as handle no-undo.
def var h-buffer as handle no-undo.
def var h-qry as handle no-undo.
def var h-tt     as handle extent 18 no-undo.
def var lv-batchsize as int64 no-undo init 25.

msgbx('ServerSide Begin ***************').
etime(true).

message pv-page skip 
        pv-tables skip 
        pv-where skip 
        pv-fieldlist 
view-as alert-box.

lv-batchsize = 20.

create dataset pv-dataset.

do x = 1 to NUM-ENTRIES(pv-tables):
    if pv-fieldlist = '*' or pv-fieldlist = ''
    then lv-fieldlist = '!tstamp,*'.
    else lv-fieldlist = entry(x,pv-fieldlist,';').

    create buffer h-db for table ENTRY(x, pv-tables).
    create temp-table h-tt[x].
    /* h-tt[x]:create-like(ENTRY(x, pv-tables)). */
    
    do y = 1 to h-db:num-fields:
        h-field = h-db:Buffer-field(y).        
        if can-do(lv-fieldlist,h-field:name)
        then h-tt[x]:ADD-LIKE-FIELD(h-field:name,h-field).
    end.
    
    h-tt[x]:TEMP-TABLE-PREPARE("t-" + ENTRY(x, pv-tables)).
    h-tt[x]:schema-marshal = 'full'.
    pv-dataset:ADD-BUFFER(h-tt[x]:DEFAULT-BUFFER-HANDLE).

    if x > 1
    then
    pv-dataset:ADD-RELATION(pv-dataset:GET-BUFFER-HANDLE(x - 1),
                            pv-dataset:GET-BUFFER-HANDLE(x),
                            ENTRY(x - 1,pv-relation,';')).
end.             
        
do x = 1 to NUM-ENTRIES(pv-tables):
    create data-source h-datasource.    
    create buffer h-buffer for table ENTRY(x, pv-tables).
    h-datasource:ADD-SOURCE-BUFFER(h-buffer, ENTRY(x,pv-relation)).
    pv-dataset:GET-BUFFER-HANDLE(x):ATTACH-DATA-SOURCE(h-datasource).
    if x = 1 
    then do:
        pv-dataset:GET-BUFFER-HANDLE(x):batch-size = lv-batchsize.
        create query h-qry.
        h-qry:ADD-BUFFER(h-buffer).
        h-qry:QUERY-PREPARE("FOR EACH " + ENTRY(1, pv-tables) +
            " " + ENTRY(1,pv-where)).
        h-datasource:QUERY = h-qry.
        if pv-page < 1
        then h-datasource:restart-row = ((pv-page - 1 ) * lv-batchsize) - 1.
        else h-datasource:restart-row = 1 + (pv-page - 1 ) * lv-batchsize .
    end. 
end. 

msgbx('Page           : ' + string(pv-page)).
msgbx('Tables         : ' + pv-tables).
msgbx('FieldList      : ' + pv-fieldlist).
msgbx('Passed in Where: ' + pv-where).
msgbx('Server prepare : ' + h-qry:prepare-string). 
msgbx('Server Index   : ' + h-qry:index-information).
msgbx('Batchsize      : ' + string(lv-batchsize)).
pv-dataset:FILL().
delete object h-qry.
  
do x = 1 to pv-dataset:NUM-BUFFERS:
    h-buffer = pv-dataset:GET-BUFFER-HANDLE(x).
    delete object h-buffer:DATA-SOURCE.
end.

msgbx('ServerSideComplete ***************').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetData C-Win 
PROCEDURE GetData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-page as int no-undo.

def var h-browse  as handle no-undo.
def var h-dataset AS HANDLE NO-UNDO.

lv-page = lv-page + pv-page.

if self:type = 'browse' /* GONE OFF-END */ 
then h-browse = self.
else run DefineBrowse (input-output h-browse).
 
 
run FillDataset (lv-page,
                lv-tables,
                lv-topwhere,
                lv-keys,
                lv-returning,
                'dummy',
                OUTPUT DATASET-HANDLE h-dataset append).

run PopulateBrowse(input dataset-handle h-dataset,h-browse,'').

if pv-page > 0 then h-browse:query:Get-Buffer-handle(1):find-first(). 
               else h-browse:query:Get-Buffer-handle(1):find-last(). 

h-browse:query:REPOSITION-TO-ROWID(h-browse:query:Get-Buffer-handle(1):rowid).
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialise C-Win 
PROCEDURE Initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

apply 'choose' to btn-run in frame {&frame-name}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE off-end-trigger C-Win 
PROCEDURE off-end-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

run GetData (1).
                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE off-home-trigger C-Win 
PROCEDURE off-home-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

run GetData (-1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateBrowse C-Win 
PROCEDURE PopulateBrowse :
Def input param DATASET-HANDLE pv-dataset.
def input param h-browse as handle no-undo.
def input param pv-by as char no-undo.

def var h-qry as handle no-undo.
def var lv-h as handle no-undo.                
def var h-buffer as handle extent 18 no-undo.
def var x as int no-undo.
def var lv-where as char no-undo.
lv-where  = 'for each '.

CREATE QUERY h-qry.

do x = 1 to pv-dataset:num-buffers:
    h-buffer[x] = pv-dataset:GET-BUFFER-HANDLE(x).
    h-qry:ADD-BUFFER(h-buffer[x]).
    lv-where = lv-where + h-buffer[x]:NAME.
    if x > 1 then do:
        lv-h = pv-dataset:get-relation(x - 1).
        lv-where = lv-where + ' where ' + 
                    h-buffer[x]:NAME + '.' + entry(x,lv-h:relation-fields) +
                  ' = ' +
                    h-buffer[x - 1]:NAME + '.' + entry(x - 1,lv-h:relation-fields).
    end.
    lv-where = lv-where + ' ,each '.
end.
lv-where = substring(lv-where,1,r-index(lv-where,',') - 1).
lv-where = lv-where + pv-by.
h-qry:QUERY-PREPARE(lv-where).
h-qry:QUERY-OPEN().

x = 1.
h-browse:query = h-qry.   
do x = 1 to pv-dataset:num-buffers:
    h-browse:ADD-COLUMNS-FROM(pv-dataset:GET-BUFFER-HANDLE(x)) no-error.
end.
h-browse:set-sort-arrow(1,true).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortBrowse C-Win 
PROCEDURE SortBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* h-col  = h-br:CURRENT-COLUMN */
/* lv-by = 'by h-col:name'. */
/* run PopulateBrowse(input dataset-handle h-dataset,h-browse,lv-by). */

/*  h-br:CLEAR-SORT-ARROWS(). */
/*  h-br:set-sort-arrow(lv-colnum,(v-desc ne '')). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changedTrigger C-Win 
PROCEDURE value-changedTrigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsgBx C-Win 
FUNCTION MsgBx RETURNS CHARACTER
  ( pv-msg as char  ) :
&glob crlf CHR(13) + CHR(10)

  lv-msgbx:insert-string(pv-msg + {&crlf}) in frame {&frame-name}.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

