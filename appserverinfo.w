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

create widget-pool.
{app-paths.i}
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS sl-mess Btn-sesion Btn-progs Btn-servers ~
btn-info Btn-Clear Btn-unload Btn-cache 
&Scoped-Define DISPLAYED-OBJECTS sl-mess 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE VARIABLE sl-mess AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 68 BY 19.76 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     sl-mess AT ROW 1 COL 27 NO-LABEL WIDGET-ID 12
     Btn-sesion AT ROW 1.95 COL 3 WIDGET-ID 2
     Btn-progs AT ROW 3.86 COL 3 WIDGET-ID 4
     Btn-servers AT ROW 6 COL 3 WIDGET-ID 6
     btn-info AT ROW 8.14 COL 3 WIDGET-ID 14
     Btn-Clear AT ROW 10.05 COL 3.2 WIDGET-ID 8
     Btn-unload AT ROW 11.95 COL 3 WIDGET-ID 16
     Btn-cache AT ROW 13.86 COL 3 WIDGET-ID 18
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.8 BY 20.1 WIDGET-ID 100.


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
         TITLE              = "Appserver Help"
         HEIGHT             = 20.19
         WIDTH              = 94.8
         MAX-HEIGHT         = 20.76
         MAX-WIDTH          = 94.8
         VIRTUAL-HEIGHT     = 20.76
         VIRTUAL-WIDTH      = 94.8
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Appserver Help */
or endkey of {&window-name} anywhere do:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  if this-procedure:PERSISTENT then return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Appserver Help */
do:
  /* This event will close the window and terminate the procedure.  */
  apply "CLOSE":U to this-procedure.
  return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-cache
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-cache C-Win
ON CHOOSE OF Btn-cache IN FRAME DEFAULT-FRAME /* Refresh Cache */
do:
        {{&core}run.i &program   = "zenlibrary.p"
                        &path      = "{&core}{&lib}"
                        &Appsrv    = "System"
                        &procedure = "PopulateTempTables"}

  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Clear C-Win
ON choose OF Btn-Clear IN FRAME DEFAULT-FRAME /* Clear Programs */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-info C-Win
ON choose OF btn-info IN FRAME DEFAULT-FRAME /* Appsrv Info */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-progs C-Win
ON choose OF Btn-progs IN FRAME DEFAULT-FRAME /* Running Progs */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-servers C-Win
ON choose OF Btn-servers IN FRAME DEFAULT-FRAME /* Last Server */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-sesion C-Win
ON choose OF Btn-sesion IN FRAME DEFAULT-FRAME /* Session Params */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-unload C-Win
ON choose OF Btn-unload IN FRAME DEFAULT-FRAME /* Unload Selected */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set current-window: this will parent dialog-boxes and frames. */
assign current-window                = {&window-name} 
       this-procedure:current-window = {&window-name}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
on close of this-procedure 
   run disable_UI.

/* Best default for GUI applications is... */
pause 0 before-hide.

/* Now enable the interface and wait for the exit condition. */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire. */
main-block:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
   on end-key undo MAIN-BLOCK, leave main-block:
   run enable_UI.
   if not this-procedure:persistent then
      wait-for close of this-procedure.
end.

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
  DISPLAY sl-mess 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE sl-mess Btn-sesion Btn-progs Btn-servers btn-info Btn-Clear Btn-unload 
         Btn-cache 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnloadProgs C-Win 
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

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

