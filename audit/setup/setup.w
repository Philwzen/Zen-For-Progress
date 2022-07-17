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
/* only needed if more than 1 db connected ie dive. */
&glob db 
&glob op stream op
&glob ip stream ip
def var lv-mes       as char no-undo.
def {&ip}.
def {&op}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-dozen lv-srv lv-crtrig lv-detrig ~
lv-wrtrig lv-dbtrig lv-seq lv-aud Btn-doit Btn-Exit lv-table 
&Scoped-Define DISPLAYED-OBJECTS lv-db lv-dozen lv-srv lv-crtrig lv-detrig ~
lv-wrtrig lv-dbtrig lv-seq lv-aud lv-table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-dozen lv-srv lv-crtrig lv-detrig lv-wrtrig ~
lv-dbtrig lv-seq lv-aud lv-table 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-doit 
     LABEL "Do It" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn-Exit AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-db AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE lv-table AS CHARACTER FORMAT "X(256)":U 
     LABEL "table" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE lv-aud AS LOGICAL INITIAL no 
     LABEL "Create Audit Configs" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE lv-crtrig AS LOGICAL INITIAL no 
     LABEL "Generate 'Create' Trigger" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE lv-dbtrig AS LOGICAL INITIAL no 
     LABEL "DefineTriggers In Schema" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE lv-detrig AS LOGICAL INITIAL no 
     LABEL "Generate 'Delete' Trigger" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE lv-dozen AS LOGICAL INITIAL no 
     LABEL "Process Zen Tables" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE lv-retrig AS LOGICAL INITIAL no 
     LABEL "Generate 'Read' Trigger" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE lv-seq AS LOGICAL INITIAL no 
     LABEL "Define Sequences In Schema" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE lv-srv AS LOGICAL INITIAL no 
     LABEL "Generate Server Progs" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE lv-wrtrig AS LOGICAL INITIAL no 
     LABEL "Generate 'Write' Trigger" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lv-db AT ROW 1.24 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     lv-dozen AT ROW 2.43 COL 8 WIDGET-ID 6
     lv-srv AT ROW 3.38 COL 8 WIDGET-ID 8
     lv-crtrig AT ROW 4.33 COL 8 WIDGET-ID 10
     lv-detrig AT ROW 5.29 COL 8 WIDGET-ID 12
     lv-wrtrig AT ROW 6.24 COL 8 WIDGET-ID 14
     lv-retrig AT ROW 7.19 COL 8 WIDGET-ID 16
     lv-dbtrig AT ROW 8.14 COL 8 WIDGET-ID 18
     lv-seq AT ROW 9.1 COL 8 WIDGET-ID 20
     lv-aud AT ROW 10 COL 8 WIDGET-ID 22
     Btn-doit AT ROW 11.48 COL 4 WIDGET-ID 2
     Btn-Exit AT ROW 11.48 COL 24 WIDGET-ID 4
     lv-table AT ROW 12.91 COL 9 COLON-ALIGNED WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 45.2 BY 13.95
         CANCEL-BUTTON Btn-Exit WIDGET-ID 100.


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
         TITLE              = "System Setup"
         HEIGHT             = 13.95
         WIDTH              = 45.2
         MAX-HEIGHT         = 34.1
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 34.1
         VIRTUAL-WIDTH      = 256
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
/* SETTINGS FOR TOGGLE-BOX lv-aud IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-crtrig IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-db IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lv-dbtrig IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-detrig IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-dozen IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-retrig IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-retrig:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX lv-seq IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-srv IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-table IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-wrtrig IN FRAME DEFAULT-FRAME
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* System Setup */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* System Setup */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-doit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-doit C-Win
ON CHOOSE OF Btn-doit IN FRAME DEFAULT-FRAME /* Do It */
DO:
 assign frame {&frame-name} {&list-1}.
 run doit.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Exit C-Win
ON CHOOSE OF Btn-Exit IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  apply 'close' to this-procedure.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create-Seq C-Win 
PROCEDURE Create-Seq :
def input param pv-table as char no-undo.
   
   sysmsg(lv-mes + 'Sequences').

   run deletesequence(pv-table).
   
   Def Var h-qry   As Handle No-undo.
   Def Var b-data  As Handle No-undo.
   def var h-field as handle no-undo.
   def var y       as int    no-undo.

   If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().
   If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
   Create Query h-QrY.
   Create Buffer b-data For Table pv-table.
   If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data). 
   b-data:disable-load-triggers(false).
   
   h-QRY:QUERY-PREPARE('FOR EACH {&db}' + pv-table + ' no-lock by ' + pv-table + 'tableid ').
   h-QRY:QUERY-OPEN.
   h-QRY:GET-LAST().
  
   if not h-qry:query-off-end then do:
      h-field = b-data:buffer-field(pv-table + 'tableid').
      y = h-field:buffer-value.
      if y = ? then y = 0.
   end.

   find first {&db}_sequence where {&db}_sequence._db-recid = {&db}_file._db-recid 
                               and {&db}_sequence._seq-name = "next-" + pv-table + "tableid"
                             exclusive-lock no-error.
   
   if not avail {&db}_sequence 
   then create {&db}_sequence no-error.
   assign
       {&db}_sequence._db-recid = {&db}_file._db-recid
       {&db}_sequence._seq-name = string( "next-" + substring(pv-table,1,20) + "tableid" )
       {&db}_sequence._seq-init = y + 1
       {&db}_sequence._seq-incr = 1
       {&db}_sequence._seq-min  = ?
       {&db}_sequence._seq-max  = ?
       {&db}_sequence._cycle-ok = FALSE.
   release {&db}_sequence.    
  
   If Valid-handle(h-QrY) 
   Then DELETE OBJECT h-QrY.  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create-Trig C-Win 
PROCEDURE Create-Trig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input param pv-mode as char no-undo.
    def input param pv-table as char no-undo.
    def input param pv-directory as char no-undo.
    
    create {&db}_file-trig.
    assign
        _file-trig._file-recid = recid(_file)
        _file-trig._event      = pv-mode
        _file-trig._override   = yes
        _file-trig._proc-name  = pv-directory + lc(pv-table) + substring(pv-mode,1,2) + '.p'.
        _file-trig._trig-crc   = ? no-error.
    release {&db}_file-trig.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteSequence C-Win 
PROCEDURE DeleteSequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-table as char no-undo.

for each {&db}_sequence where {&db}_sequence._db-recid = {&db}_file._db-recid 
                          and {&db}_sequence._seq-name = "next-" + pv-table + "tableid"
                        exclusive-lock:
     delete {&db}_sequence.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteTriggers C-Win 
PROCEDURE DeleteTriggers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    for each {&db}_file-trig of _file exclusive-lock:
        delete _file-trig.
    end.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Doit C-Win 
PROCEDURE Doit :
DEF VAR lv-Directory AS CHAR NO-UNDO.
def var lv-uniquekey as char no-undo.

FOR EACH {&db}_File WHERE _FIle-num > 0 
                     and _file-num < 32000
                    and if lv-table ne '' then _file-name = lv-table else true
                    NO-LOCK:
    lv-mes = "Processing : " + {&db}_File._file-name + ' '.
    lv-db:screen-value in frame {&frame-name} = lv-db + {&db}_File._file-name.
    
    if {&db}_File._FILE-NAME BEGINS 'ZEN'
    then if lv-dozen then lv-Directory = "{&core}{&triggers}". 
                     else next.
    else lv-Directory = "{&triggers}".
    sysmsg(lv-mes).
    if lv-aud then do:
        if not can-find(first zen-auditconfig where zen-auditconfig.tablename = {&db}_File._file-name)
            and _file-name ne 'zen-context'                                                 
            and not _file-name begins 'zen-aud'                                             
        then do transaction:      
            sysmsg(lv-mes + 'Auditing').
                                                                     
            create zen-auditconfig. 
            assign zen-auditconfig.recordcreates = true                                    
                   zen-auditconfig.recorddeletes = true  
                   zen-auditconfig.Active        = true                                  
                   zen-auditconfig.keyfield      = _file-name + 'tableid'                  
                   zen-auditconfig.tablename     = _file-name.  
            
            for each {&db}_field of {&db}_file no-lock:
                create zen-auditfield.
                assign zen-auditfield.Active        = true
                       zen-auditfield.datatype      = {&db}_field._data-type
                       zen-auditfield.fieldname     = {&db}_field._field-name
                       zen-auditfield.tablename     = {&db}_File._FILE-NAME
                       zen-auditfield.zen-auditconfigtableid = zen-auditconfig.zen-auditconfigtableid.
            end.
            find first zen-auditfield where zen-auditfield.zen-auditconfigtableid = zen-auditconfig.zen-auditconfigtableid
                                      no-lock no-error.                  
        end.               
        find first zen-auditconfig where zen-auditConfig.TableName = {&db}_file._file-name 
                                  no-lock no-error.                                                                                                                                                                 
    end.
                          
    lv-uniquekey = {&db}_File._File-name + 'tableid'.
    
    /* create server side prog */
    if lv-srv then RUN gensrvprog({&db}_file._file-name). 
    
    /* create trigger programs */
    if lv-crtrig then RUN GenCreate({&db}_file._file-name,lv-Directory,lv-uniquekey).
    if lv-detrig then RUN GenDelete({&db}_file._file-name,lv-Directory).
    if lv-wrtrig then RUN GenWrite ({&db}_file._file-name,lv-Directory).
/*     if lv-retrig then RUN GenRead  ({&db}_file._file-name,lv-Directory).  */ 
   
     /* create db triggers in schema */
    if lv-dbtrig then do:
        sysmsg(lv-mes + 'Schema Triggers').

       run DeleteTriggers.
       run create-trig('create',{&db}_file._file-name,lv-directory).
       if not ({&db}_file._file-name begins 'zen-aud' 
               or can-do("zen-context",{&db}_file._file-name ))
       then do:
           run create-trig('delete',{&db}_file._file-name,lv-directory).
           run create-trig('write',{&db}_file._file-name,lv-directory).
       end.
     end.
    /* create sequences in schema */
    if lv-seq then run create-seq({&db}_file._file-name).
END.
lv-db:screen-value = lv-db.

sysmsg('off').
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
  DISPLAY lv-db lv-dozen lv-srv lv-crtrig lv-detrig lv-wrtrig lv-dbtrig lv-seq 
          lv-aud lv-table 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE lv-dozen lv-srv lv-crtrig lv-detrig lv-wrtrig lv-dbtrig lv-seq lv-aud 
         Btn-doit Btn-Exit lv-table 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenCreate C-Win 
PROCEDURE GenCreate :
def input param pv-table     as char no-undo.
    def input param pv-directory as char no-undo.
    def input param pv-uniquekey as char no-undo.
    
    run triggerhead(pv-directory,pv-table,'CREATE').        
     
    if avail zen-auditconfig then do:
        PUT {&op} UNFORMATTED "~{~{&aud~}trcreate.i ~&Table    = ~"" + pv-table + "~"" SKIP
                        "                  ~&Uniquekey = ~"" + pv-uniquekey + "~"}" SKIP.
    end.
    else do:
        PUT {&op} UNFORMATTED "&if defined(KeyInTriggers) ne 0 &then" skip.
        PUT {&op} UNFORMATTED "~{~{&aud~}generatethekey.i   &TableName      = ~"" + pv-table + "~"" SKIP.
        PUT {&op} UNFORMATTED "                          &Unique-key     = ~"" + pv-uniquekey + "~"}" skip.
        put {&op} unformatted "&endif".
    end.
    
    OUTPUT {&op} CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenDelete C-Win 
PROCEDURE GenDelete :
def input param pv-table as char no-undo.
    def input param pv-directory as char no-undo.

    run triggerhead(pv-directory,pv-table,'DELETE').
   
    if avail zen-auditconfig then
        PUT {&op} UNFORMATTED "~{~{&aud~}trdelete.i ~&Table = ~"" pv-table "~"" SKIP
                        "                  ~&key   = ~"" pv-table "." zen-auditConfig.KeyField  "~"}" SKIP.
    OUTPUT {&op} CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenRead C-Win 
PROCEDURE GenRead :
def input param pv-table as char no-undo.
    def input param pv-directory as char no-undo.
    run triggerhead(pv-directory,pv-table,'READ').

    OUTPUT {&op} CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenSrvProg C-Win 
PROCEDURE GenSrvProg :
def input param pv-table as char no-undo.
    sysmsg(lv-mes + 'Server Prog').
def var lv-dir as char no-undo.
if pv-table begins 'zen' 
then lv-dir = '{&core}{&srv}'.
else lv-dir = '{&sys}{&srv}'.

    DEF VAR irec AS CHAR NO-UNDO.
    DEF VAR lv-ok AS LOG NO-UNDO.
    IF SEARCH(lc(lv-dir + pv-table + ".p")) NE ? 
    THEN DO:
        MESSAGE pv-table + ".p" ' already exists overwrite?'
        VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lv-ok.
        IF NOT lv-ok THEN RETURN.
    END.
    OUTPUT {&op} TO VALUE(lc(lv-dir + pv-table + ".p")) NO-ECHO.
    INPUT {&ip} FROM VALUE("{&template}appsrv-template.p") NO-ECHO.
    REPEAT:
        IMPORT {&ip} UNFORMATTED irec.
        if trim(irec) begins "File"    then irec = irec + pv-table + '.p'.
        if trim(irec) begins "Author"  then irec = irec + userid.
        if trim(irec) begins "Created" then irec = irec + string(today).

        IF irec BEGINS "&glob table-name" 
        then PUT {&op} UNFORMATTED "&glob table-name " pv-table SKIP.
        ELSE PUT {&op} UNFORMATTED irec SKIP.
        IF irec = "" then put {&op} unformatted skip(1).
    END.
    output {&op} close.
    input {&ip} close.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenWrite C-Win 
PROCEDURE GenWrite :
def input param pv-table as char no-undo.
    def input param pv-directory as char no-undo.
    run triggerhead(pv-directory,pv-table,'WRITE').

    if avail zen-auditconfig then    
        PUT {&op} UNFORMATTED "~{~{&aud~}trwrite.i ~&Table = ~"" pv-table "~"" skip
                        "                 ~&key   = ~"" pv-table "." zen-auditConfig.KeyField "~"}" SKIP.
                        
    OUTPUT {&op} CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise C-Win 
PROCEDURE initialise :
lv-db = "{&db}".

if lv-db = '' 
then lv-db = ldbname(1).

disp lv-db with frame {&frame-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TriggerHead C-Win 
PROCEDURE TriggerHead :
def input param pv-directory as char no-undo.
def input param pv-table     as char no-undo.
def input param pv-mode as char no-undo.

    sysmsg(lv-mes + pv-mode + ' Trigger').

    def var lv-t as char no-undo.
    lv-t = lc(substring(pv-mode,1,2)) + ".p".
    
    OUTPUT {&op} TO VALUE(lc(pv-Directory + pv-table + lv-t)) NO-ECHO.
    PUT {&op} UNFORMATTED "TRIGGER PROCEDURE FOR " pv-mode " OF " pv-table. 
    if pv-mode = 'write' then PUT {&op} UNFORMATTED  " OLD OldBuffer".
    PUT {&op} UNFORMATTED "." SKIP(1).
    PUT {&op} UNFORMATTED "&glob serverprogram" SKIP.
    PUT {&op} UNFORMATTED "~{app-paths.i justvars = 'true'~}" SKIP.
    PUT {&op} UNFORMATTED "~{~{&core~}control.i~}" SKIP.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

