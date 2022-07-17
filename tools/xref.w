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
def var lv-tf as char no-undo init 'temp/'.
def var lv-opsys as char no-undo.

lv-opsys = opsys.

def stream ip.
def stream ip2.
Def Stream op.

def temp-table w-wrk no-undo
    field w-parent as char format 'x(30)'
    field w-child  as char format 'x(30)'
    field w-type   as char  format 'x(30)'
    index order is unique w-parent w-child.

def temp-table t-sort no-undo
    field t-line as int
    field t-irec as char
index order t-line.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS sl-path btn-add Btn-Ok btn-del btn-cancel ~
lv-mess 
&Scoped-Define DISPLAYED-OBJECTS sl-path lv-mess 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Mess C-Win 
FUNCTION Mess RETURNS CHARACTER
  ( pv-msg as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-add 
     LABEL "Add" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-cancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-del 
     LABEL "Delete" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn-Ok 
     LABEL "Ok" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-mess AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 44 BY 9.52 NO-UNDO.

DEFINE VARIABLE sl-path AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 12.14 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     sl-path AT ROW 1.71 COL 4 NO-LABEL WIDGET-ID 2
     btn-add AT ROW 1.95 COL 36 WIDGET-ID 6
     Btn-Ok AT ROW 1.95 COL 58 WIDGET-ID 4
     btn-del AT ROW 3.14 COL 36 WIDGET-ID 8
     btn-cancel AT ROW 3.14 COL 58 WIDGET-ID 10
     lv-mess AT ROW 4.33 COL 36 NO-LABEL WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13.48 WIDGET-ID 100.


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
         HEIGHT             = 13.52
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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


&Scoped-define SELF-NAME btn-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-add C-Win
ON CHOOSE OF btn-add IN FRAME DEFAULT-FRAME /* Add */
DO:
def var lv-dir as char no-undo.
    system-dialog get-dir lv-dir INITIAL-DIR '.'.
  sl-path:add-last(lv-dir).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  apply 'close' to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-del C-Win
ON CHOOSE OF btn-del IN FRAME DEFAULT-FRAME /* Delete */
DO:
  sl-path:delete(sl-path:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Ok C-Win
ON CHOOSE OF Btn-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
    run doit.
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
   run initialise.
  RUN enable_UI.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoIt C-Win 
PROCEDURE DoIt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    def var x as int no-undo.
    do x = 1 to sl-path:num-items in frame {&frame-name}:  
        mess('Dir ' + string(x) + ' of ' + string(sl-path:num-items) + ' ' + sl-path:entry(x)).
        run processdir (sl-path:entry(x)).
    end.

    run dumpout.
message 'done' {&dbt}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpOut C-Win 
PROCEDURE DumpOut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
output stream op close.
output stream op to 'xrefout.txt'.
for each w-wrk:
    put stream op unformatted w-parent ',' w-child ',' w-type skip.
end.
output stream op close.
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
  DISPLAY sl-path lv-mess 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE sl-path btn-add Btn-Ok btn-del btn-cancel lv-mess 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialise C-Win 
PROCEDURE Initialise :
sl-path:list-items in frame {&frame-name} =  
'codezen\,codezen\srv\,codezen\audit\,codezen\audit\srv\,codezen\reports\,
codezen\reports\cry\,codezen\reports\srv\,codezen\libraries\,codezen\triggers,
sys\,sys\srv\,triggers\'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessDir C-Win 
PROCEDURE ProcessDir :
def input param pv-dir as char no-undo.
def var v-file as char format 'x(12)' no-undo. 

input stream ip2 from os-dir(pv-dir) no-echo. 

repeat:
    import stream ip2 unformatted V-FILE .
    v-file = replace(entry(1,v-file,' '),'"','').
    
    if length(substring(v-file,r-index(v-file,'.') + 1)) > 1 
    then next.
    if not can-do('w,p',substring(v-file,length(v-file),1)) 
    then next.

    v-file = pv-dir + v-file. 
    mess('Compiling ' + v-file).
    process events.
    compile value(v-file) xref value(lv-tf + 'temp.ref') no-error.

    if compiler:error and not compiler:warning and 
        ERROR-STATUS:GET-NUMBER(1) ne 2884 AND
        ERROR-STATUS:GET-NUMBER(1) ne 0 and
        ERROR-STATUS:GET-NUMBER(1) ne 6430
    then do:
        mess('Fatal Error in compilation of ' + v-file + ' at line ' + 
              string(compiler:error-row) + ' ' + ERROR-STATUS:GET-MESSAGE(1)).
        next.
    end.
    mess('Building ' + v-file).
    process events.

    run ReadInXref in this-procedure.
    Run ProcessXref in this-procedure.
end.
    input stream ip2 close. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessXref C-Win 
PROCEDURE ProcessXref :
def var v-child  as char no-undo.
def var v-field  as char no-undo.
def var v-fname  as char no-undo.
def var v-typ    as char no-undo.
def var v-inproc as char no-undo.
def var v-cproc  as char no-undo.
def var v-temp   as char no-undo.
def var lv-validtypes as char no-undo init 'run'.
/*    'reference,SORT-ACCESS,access,search,update,create,delete,run,procedure,include,function'. */
def var lv-uniques as char no-undo init 'run,include,procedure,function'.  
def var lv-temptables as char no-undo init 'worktable,workfile,temptable'.
  
for each t-sort by t-line descending:
        assign 
           v-child = replace(entry(2,t-irec,' '),'"','')
           v-typ   = replace(entry(4,t-irec,' '),'"','') 
           v-fname = replace(entry(5,t-irec,' '),'"','') no-error.
        
    v-field = entry(6,t-irec,' ') no-error.
    if error-status:error then v-field = ''.
    v-temp = entry(7,t-irec,' ') no-error.
    if error-status:error then v-temp = ''.

    if not can-do(lv-validtypes,v-typ)
    then next.

    assign v-child = trim(v-child)
           v-fname = trim(v-fname)
           v-field = trim(v-field)
           v-child = if index(v-child,' ') ne 0 
                          then entry(1,v-child,' ')
                          else v-child
           v-fname = if index(v-fname,' ') ne 0 
                          then entry(1,v-fname,' ')
                          else v-fname
           v-child = entry(1,v-child)
           v-fname = entry(1,v-fname)
           v-fname = replace(v-fname,'~/','~\')
           v-child = replace(v-child,'~/','~\').
    if v-fname begins 'value' then next.
    if can-do('.w,.p',substring(v-fname,length(v-fname) - 1,2)) then next. 
           
    /* sort out unique occurances */
    if can-do(lv-uniques,v-typ) then do:
        v-inproc = v-fname. 
        if not can-find(first w-wrk where w-parent = v-child
                                      and w-child  = v-fname) 
        then do: /* child records */
            create w-wrk.
            assign w-parent = v-child  
                   w-child  = v-fname
                   w-type   = v-typ. 
        end. 
    end.
end.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadInXref C-Win 
PROCEDURE ReadInXref :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var v-irec   as char no-undo.
    empty temp-table t-sort.
    
    input stream ip from value(lv-tf + 'temp.ref') no-echo.
    repeat:
        import stream ip unformatted v-irec.
        create t-sort.
        assign t-line = int(entry(3,v-irec,' '))
               t-irec = v-irec no-error.
    End.
    input stream ip close.
/*     os-delete value(lv-tf + 'temp.ref'). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Mess C-Win 
FUNCTION Mess RETURNS CHARACTER
  ( pv-msg as char ) :
  
  lv-mess:screen-value in frame {&frame-name} = pv-msg.
  
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

