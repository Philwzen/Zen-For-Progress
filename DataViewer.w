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
{app-paths.i}
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
&Scoped-Define ENABLED-OBJECTS lv-tables btn-run lv-topwhere Btn-all ~
lv-keys btn-exit lv-Returning 
&Scoped-Define DISPLAYED-OBJECTS lv-tables lv-topwhere lv-keys lv-Returning 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-tables lv-topwhere lv-keys lv-Returning 

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE lv-keys AS CHARACTER FORMAT "X(256)":U 
     LABEL "Key fields" 
     VIEW-AS FILL-IN 
     SIZE 79 BY 1 NO-UNDO.

DEFINE VARIABLE lv-Returning AS CHARACTER FORMAT "X(256)":U 
     LABEL "Return fields" 
     VIEW-AS FILL-IN 
     SIZE 79 BY 1 NO-UNDO.

DEFINE VARIABLE lv-tables AS CHARACTER FORMAT "X(256)":U INITIAL "zen-dpgm" 
     LABEL "Tables" 
     VIEW-AS FILL-IN 
     SIZE 79 BY 1 NO-UNDO.

DEFINE VARIABLE lv-topwhere AS CHARACTER FORMAT "X(256)":U INITIAL "where true by pgm" 
     LABEL "Top level Where" 
     VIEW-AS FILL-IN 
     SIZE 79 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lv-tables AT ROW 16.48 COL 16 COLON-ALIGNED WIDGET-ID 8
     btn-run AT ROW 16.71 COL 131.8 WIDGET-ID 6
     lv-topwhere AT ROW 17.67 COL 16 COLON-ALIGNED WIDGET-ID 10
     Btn-all AT ROW 18.38 COL 131.8 WIDGET-ID 24
     lv-keys AT ROW 18.86 COL 16 COLON-ALIGNED WIDGET-ID 12
     btn-exit AT ROW 20 COL 131.8 WIDGET-ID 4
     lv-Returning AT ROW 20.05 COL 16 COLON-ALIGNED WIDGET-ID 14
     "zen-auditconfig,zen-auditfield" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 16.71 COL 98 WIDGET-ID 16
     "where tablename = 'zen-dpgm'" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 17.91 COL 98 WIDGET-ID 18
     "tablename,tablename" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 19.1 COL 98 WIDGET-ID 20
     "*~;* empty means all" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 20.29 COL 98 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148 BY 20.38
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
         HEIGHT             = 20.38
         WIDTH              = 149
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
        HEIGHT = 14
        x = 88
        y = 10
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
  DISPLAY lv-tables lv-topwhere lv-keys lv-Returning 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE lv-tables btn-run lv-topwhere Btn-all lv-keys btn-exit lv-Returning 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
 
 
{{&core}run.i &program   = "dynamic.p"
              &path      = "{&core}{&srv}"
              &Appsrv    = "System"
              &procedure = "FillDataset"
              &params    = "(lv-page,
                            lv-tables,
                            lv-topwhere,
                            lv-keys,
                            lv-returning,
                            'dummy',
                            OUTPUT DATASET-HANDLE h-dataset append)"}
                            
  run PopulateBrowse(input dataset-handle h-dataset,h-browse,"").
                   
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

/* apply 'choose' to btn-run in frame {&frame-name}. */


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
if pv-by ne "" then lv-where = lv-where + pv-by.
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

