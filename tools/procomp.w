&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/******************************************************************************/
/*  PROGRAM ID.     : ????????                                                */
/*  PROGRAM TITLE   : ????????                                                */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    : Zennor Computing LTD                                    */
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure"s triggers and internal procedures 
     will execute in this procedure"s storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var v-error-fil as char format "x(45)" no-undo.
def var v-comp-fil  as char format "x(45)" no-undo.
def var v-err       as log                 no-undo.

def stream op.
def stream ip.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS sl-dirs v_filespec v_dest btn-compile ~
btn-cancel sl-doing 
&Scoped-Define DISPLAYED-OBJECTS lv-db sl-dirs v_filespec v_dest sl-doing 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 sl-dirs v_filespec v_dest 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Cancel" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF.

DEFINE BUTTON btn-compile 
     LABEL "Compile" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF.

DEFINE BUTTON btn-print 
     LABEL "Print Errors" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
     &ELSE SIZE 15 BY 1 &ENDIF.

DEFINE BUTTON btn-view 
     LABEL "View Errors" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
     &ELSE SIZE 14 BY 1 &ENDIF.

DEFINE VARIABLE lv-db AS CHARACTER FORMAT "X(256)":U 
     LABEL "Database" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 30 BY 1
     &ELSE SIZE 30 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE v_dest AS CHARACTER FORMAT "X(256)":U 
     LABEL "Output Destination" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 31 BY 1
     &ELSE SIZE 31 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE v_filespec AS CHARACTER FORMAT "X(256)":U INITIAL "*.p" 
     LABEL "Compile Filespec" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sl-dirs AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 24 BY 16
     &ELSE SIZE 24 BY 16 &ENDIF NO-UNDO.

DEFINE VARIABLE sl-doing AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 44 BY 13
     &ELSE SIZE 44 BY 13 &ENDIF NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lv-db
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 20 COLON-ALIGNED
          &ELSE AT ROW 2 COL 20 COLON-ALIGNED &ENDIF
     sl-dirs
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 55
          &ELSE AT ROW 2 COL 55 &ENDIF NO-LABEL
     v_filespec
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 20 COLON-ALIGNED
          &ELSE AT ROW 3 COL 20 COLON-ALIGNED &ENDIF
     v_dest
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 20 COLON-ALIGNED
          &ELSE AT ROW 4 COL 20 COLON-ALIGNED &ENDIF
     btn-compile
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 8
          &ELSE AT ROW 6 COL 8 &ENDIF
     btn-cancel
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 31
          &ELSE AT ROW 6 COL 31 &ENDIF
     sl-doing
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 2
          &ELSE AT ROW 8 COL 2 &ENDIF NO-LABEL
     btn-print
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 48
          &ELSE AT ROW 19 COL 48 &ENDIF
     btn-view
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 65
          &ELSE AT ROW 19 COL 65 &ENDIF
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW TERMINAL-SIMULATION ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 21
         WIDTH              = 80.43
         MAX-HEIGHT         = 21
         MAX-WIDTH          = 80.43
         VIRTUAL-HEIGHT     = 21
         VIRTUAL-WIDTH      = 80.43
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = yes
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btn-print IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btn-print:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btn-view IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btn-view:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN lv-db IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lv-db:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR SELECTION-LIST sl-dirs IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN v_dest IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN v_filespec IN FRAME DEFAULT-FRAME
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel TERMINAL-SIMULATION
ON CHOOSE OF btn-cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-compile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-compile TERMINAL-SIMULATION
ON CHOOSE OF btn-compile IN FRAME DEFAULT-FRAME /* Compile */
DO:
  run compileit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-print TERMINAL-SIMULATION
ON CHOOSE OF btn-print IN FRAME DEFAULT-FRAME /* Print Errors */
DO:
    &if "{&opsys}" = "win32" 
        &then DOS silent copy value(v-error-fil) prn:.
        &else UNIX silent cp value(v-error-fil) |lp. 
    &endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-view
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-view TERMINAL-SIMULATION
ON CHOOSE OF btn-view IN FRAME DEFAULT-FRAME /* View Errors */
DO:
    &if "{&opsys}" = "win32" 
        &then DOS type value(v-error-fil) |more.
        &else UNIX ls value(v-error-fil) |pg.
     &endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 


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
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckErrors TERMINAL-SIMULATION 
PROCEDURE CheckErrors :
if v-err then do:
    message "Compiler Errors have been sent to " (v-error-fil) 
    view-as alert-box.
    assign btn-view:sensitive in frame {&frame-name} = true
           btn-print:sensitive = true
           btn-view:hidden     = false
           btn-print:hidden    = false.
end.
else message "Compile OK No Errors" view-as alert-box.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE compileit TERMINAL-SIMULATION 
PROCEDURE compileit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
def var x           as int  no-undo.
def var in_progname as char no-undo.
def var lv-prog     as char no-undo.

assign frame {&frame-name}
       {&list-1}
       sl-doing:list-items   = ""
       sl-doing:screen-value = ""
       btn-view:sensitive  = false
       btn-print:sensitive = false
       btn-view:hidden     = true
       btn-print:hidden    = true.
        
&if "{&opsys}" = "win32" 
    &then DOS silent del value(v-comp-fil).
    &else UNIX silent rm value(v-comp-fil).
&endif
        
do x = 1 to sl-dirs:num-items:        
    &if "{&opsys}" = "win32" 
        &then DOS silent dir/b VALUE(entry(x,sl-dirs:list-items) + v_filespec) >> value(v-comp-fil).
        &else UNIX silent ls VALUE(entry(x,sl-dirs:list-items) + v_filespec) >> value(v-comp-fil) 2>&1.
    &endif
end.

INPUT stream ip FROM value(v-comp-fil) NO-ECHO.
output stream op to value(v-error-fil).

REPEAT:
    import stream ip unformatted in_progname.
    if in_progname = "" then next.
    
    &if "{&opsys}" = "win32" &then 
    do x = 1 to sl-dirs:num-items:        
        lv-prog = search(entry(x,sl-dirs:list-items) + in_progname).
        if lv-prog ne ? then leave.
    end.
    lv-prog = substring(lv-prog,3).
    &else
    lv-prog = in_progname.
    &endif
    
    
    sl-doing:add-last(lv-prog).
    sl-doing:scroll-to-item(lv-prog).
    
    COMPILE VALUE(lv-prog) save into value(v_dest) no-error. 
    if compiler:error then do:
        v-err = true.
        put stream op unformatted "## " lv-prog " start" skip.
        put stream op unformatted 
            "Compilation error in " COMPILER:FILENAME " at line "
             COMPILER:ERROR-ROW " column " COMPILER:ERROR-COL 
             " Error " Error-Status:Get-Message(Error-Status:Num-Messages)
             skip. 
        put stream op unformatted  "## " lv-prog " end" skip.             
    end.    
END.

input stream  ip close.
output stream op close.

run checkerrors.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI TERMINAL-SIMULATION  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI TERMINAL-SIMULATION  _DEFAULT-ENABLE
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
  DISPLAY lv-db sl-dirs v_filespec v_dest sl-doing 
      WITH FRAME DEFAULT-FRAME IN WINDOW TERMINAL-SIMULATION.
  ENABLE sl-dirs v_filespec v_dest btn-compile btn-cancel sl-doing 
      WITH FRAME DEFAULT-FRAME IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise TERMINAL-SIMULATION 
PROCEDURE initialise :
assign v-error-fil = "error.fil"
       v-comp-fil  = "comp.fil".

&if "{&OPSYS}" = "win32" &then
/*                                                                     */
/*     sl-dirs:list-items in frame {&frame-name} =                     */
/*         "audit\,audit\srv\,docman\,docman\srv,reports\,             */
/*         reports\srv\,srv\,static\,system\,triggers\,{&core},{&core}srv\". */
/*                                                                     */
/*     case pdbname(1):                                                */
/*         when "sigtest" then assign lv-db  = "Test"                  */
/*                                    v_dest = "..\..\runtime\test".   */
/*         when "sgmstr" then assign lv-db  = "Prod"                   */
/*                                   v_dest = "..\..\runtime\prod".    */
/*         when "sigdev" then assign lv-db  = "Dev"                    */
/*                                   v_dest = "..\..\runtime\dev".     */
/*         otherwise assign lv-db = pdbname(1)                         */
/*                          v_dest = "..\..\runtime\".                 */
/*     end case.                                                       */
    
&else

    sl-dirs:list-items in frame {&frame-name} = 
        "audit/,audit/srv/,docman/,docman/srv,reports/,reports/srv/,srv/,static/,system/,triggers/,{&core},{&core}srv/".

    case pdbname(1):
        when "sigtest" then assign lv-db  = "Test"
                                   v_dest = "../../runtime/test".
        when "sgmstr" then assign lv-db  = "Prod"
                                   v_dest = "../../runtime/prod".
        when "sigdev" then assign lv-db  = "Dev"
                                  v_dest = "../../runtime/dev".
        otherwise assign lv-db = pdbname(1)
                         v_dest = "../../runtime/".
    end case.

&endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

