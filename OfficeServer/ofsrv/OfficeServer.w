&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME win-main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS win-main 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
CREATE WIDGET-POOL.
{app-paths.i}
&undefine suppresswindow

&glob nobrowse
&glob nobuttons

Def Stream ip.
Def Stream op.
Def Stream err.


Def Temp-table t-toprocess No-undo
    Field t-filename As Char format 'x(20)' column-label 'File'
    field t-path     as char format 'x(40)' column-label 'Path'
    Field t-status   As Char format 'x(40)' column-label 'Status' initial 'Waiting'.

Def Temp-table t-error No-undo
    Field t-filename As Char
    field t-path     as char
    Field t-status   As Char.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main
&Scoped-define BROWSE-NAME br-processing

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-toprocess

/* Definitions for BROWSE br-processing                                 */
&Scoped-define FIELDS-IN-QUERY-br-processing GetFileName(t-filename) t-status   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-processing   
&Scoped-define SELF-NAME br-processing
&Scoped-define QUERY-STRING-br-processing FOR EACH t-toprocess
&Scoped-define OPEN-QUERY-br-processing OPEN QUERY {&SELF-NAME} FOR EACH t-toprocess.
&Scoped-define TABLES-IN-QUERY-br-processing t-toprocess
&Scoped-define FIRST-TABLE-IN-QUERY-br-processing t-toprocess


/* Definitions for FRAME f-main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-processing 
&Scoped-Define DISPLAYED-OBJECTS lv-srvdir lv-tagdir lv-tagfile lv-datdir ~
lv-bakdir lv-msg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-srvdir lv-tagdir lv-tagfile lv-datdir lv-bakdir 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CleanUpFiles win-main 
FUNCTION CleanUpFiles RETURNS LOGICAL
  ( pv-set as char,
    pv-data as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ErrLog win-main 
FUNCTION ErrLog RETURNS LOGICAL
  ( pv-msg as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetParam win-main 
FUNCTION GetParam RETURNS CHARACTER
  ( pv-set as char,
    pv-key as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsgParent win-main 
FUNCTION MsgParent RETURNS LOGICAL
  ( pv-msg as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSen win-main 
FUNCTION SetSen RETURNS LOGICAL
  ( pv-onoff as log )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Stopme win-main 
FUNCTION Stopme RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
def var lv-bakdir as char FORMAT "X(256)":U 
     LABEL "Backup Dir" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1 NO-UNDO.

def var lv-datdir as char FORMAT "X(256)":U 
     LABEL "Dat Dir" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1 NO-UNDO.

def var lv-msg as char FORMAT "X(256)":U INITIAL "Stopped" 
     LABEL "Status" 
      VIEW-AS TEXT 
     SIZE 67 BY .62 NO-UNDO.

def var lv-srvdir as char FORMAT "X(256)":U 
     LABEL "Log Dir" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1 NO-UNDO.

def var lv-tagdir as char FORMAT "X(256)":U 
     LABEL "Tag Dir" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1 NO-UNDO.

def var lv-tagfile as char FORMAT "X(256)":U 
     LABEL "Stop Name" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-processing FOR 
      t-toprocess SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-processing
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-processing win-main _FREEFORM
  QUERY br-processing DISPLAY
      GetFileName(t-filename) Format 'x(30)' column-label 'File'
t-status
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 97 BY 5.71 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     lv-srvdir AT ROW 3.62 COL 17 COLON-ALIGNED
     lv-tagdir AT ROW 4.57 COL 17 COLON-ALIGNED
     lv-tagfile AT ROW 5.52 COL 17 COLON-ALIGNED
     lv-datdir AT ROW 6.48 COL 17 COLON-ALIGNED
     lv-bakdir AT ROW 7.43 COL 17 COLON-ALIGNED
     br-processing AT ROW 9.81 COL 1
     lv-msg AT ROW 8.86 COL 17 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.4 BY 15.33.


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
  CREATE WINDOW win-main ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 17.6
         ROW                = 7.24
         HEIGHT             = 15.38
         WIDTH              = 98.4
         MAX-HEIGHT         = 27
         MAX-WIDTH          = 112
         VIRTUAL-HEIGHT     = 27
         VIRTUAL-WIDTH      = 112
         MAX-BUTTON         = no
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
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
/* BROWSE-TAB br-processing lv-bakdir f-main */
/* SETTINGS FOR FILL-IN lv-bakdir IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-datdir IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-msg IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-srvdir IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-tagdir IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-tagfile IN FRAME f-main
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(win-main)
THEN win-main:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-processing
/* Query rebuild information for BROWSE br-processing
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH t-toprocess.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE br-processing */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME br-processing

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK win-main 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
/* this dont work in v8 for my style of windows 
if int(substring(proversion,1,1)) >= 7 then
{&WINDOW-NAME}:parent         = current-window.      
*/
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{{&core}commonmaint.i}
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
oN CLOSE OF THIS-PROCEDURE do:
    if not lv-exited then
        run exit-trigger in this-procedure no-error.
    run disable_ui.
quit.             
end.

ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
    run exit-trigger in this-procedure no-error.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
    run exit-trigger in this-procedure no-error.
END.
    
{&window-name}:hidden = false.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    run initialise.
IF NOT THIS-PROCEDURE:PERSISTENT OR SESSION:DISPLAY-TYPE ne "GUI":U then
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Clear-trigger win-main 
PROCEDURE Clear-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    Empty Temp-table t-ERROR.
    setsensitive(false,'inc','btn-clear',frame {&frame-name}:handle).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI win-main  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(win-main)
  THEN DELETE WIDGET win-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI win-main  _DEFAULT-ENABLE
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
  DISPLAY lv-srvdir lv-tagdir lv-tagfile lv-datdir lv-bakdir lv-msg 
      WITH FRAME f-main IN WINDOW win-main.
  ENABLE br-processing 
      WITH FRAME f-main IN WINDOW win-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDataFileList win-main 
PROCEDURE GetDataFileList :
Empty Temp-table t-toprocess.

def var lv-fname as char no-undo format 'x(50)'.
def var lv-path  as char no-undo.

input stream ip from os-dir(entry(1,lv-datdir,'/')).

Repeat:
    import stream ip unformatted lv-fname.
    if entry(3,lv-fname,' ') eq 'f' 
    then do:
        assign
            lv-path  = substring(entry(2,lv-fname,' '),2)
            lv-path  = substring(lv-path,1,length(lv-path) - 1)
            lv-fname = substring(entry(1,lv-fname,' '),2)
            lv-fname = substring(lv-fname,1,length(lv-fname) - 1).
        if entry(2,lv-fname,'.') = 'dat'
        then do:
            find t-error where t-error.t-filename = lv-fname NO-ERROR.
            create t-toprocess.
            assign t-toprocess.t-filename = lv-fname
                   t-toprocess.t-path     = lv-path
                   t-toprocess.t-status   = if not avail t-error then 'Waiting'
                                                                 else t-error.t-status.
        End.
    End.
End.
input stream ip close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-Delete-Trigger win-main 
PROCEDURE local-Delete-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-ok       as log  no-undo.
def var lv-proclist as char no-undo.
def var lv-proc     as char no-undo.
def var lv-pid      as int  no-undo.
def var x as int no-undo.

message ' Kill Dead Office Programs?'
view-as alert-box question buttons yes-no update lv-ok.

if not lv-ok then return.

lv-proclist = WapiListProcesses().

do x = 1 to num-entries(lv-proclist):
    lv-proc = entry(x,lv-proclist).
    lv-pid  = int(entry(1,lv-proc,':')) no-error.
    if error-status:error then next.
    lv-proc = entry(2,lv-proc,':').
    if can-do('winword.exe,excel.exe,outlook.exe',lv-proc) 
    then WapiKillProcess(lv-pid).
End.
return 'override'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-Display-Fields win-main 
PROCEDURE local-Display-Fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
disp {&list-1} with frame {&frame-name}.
return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-Edit-trigger win-main 
PROCEDURE local-Edit-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
setsen(true).
return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit-trigger win-main 
PROCEDURE local-exit-trigger :
run stop-trigger.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise win-main 
PROCEDURE local-initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var lv-buttonparams as char no-undo. 

/*
"horizontal,{&btnflat},{&btnstartcol},{&btnstartrow},width,height,centered".                       
*/
    lv-buttonparams = "Start^ttf,Stop^ftf,Clear^ftf,Edit^ttf,undo^ftf,Save^ftf,Delete^ttf,Exit^ttf|" + 
                      string(this-procedure) + "," +
                      string({&window-name}:handle) + "," +
                      string(frame {&frame-name}:handle) + "," + 
                      "true,{&btnflat},{&btnstartcol},{&btnstartrow},32,32,true".                       
                
    CreateButs(lv-buttonparams).   
    
 assign 
    lv-datdir  = GetIniValue('OfficeServer','OfsData')
    lv-bakdir  = lv-datdir +  GetIniValue('OfficeServer','OfsBack')
    lv-srvdir  = GetIniValue('OfficeServer','OfsHome')
    lv-tagdir  = GetIniValue('OfficeServer','OfsTag')
    lv-tagfile = GetIniValue('OfficeServer','StopmeName').
    
    RUN Display-Fields.  
  return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-Save-Trigger win-main 
PROCEDURE local-Save-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-hand as handle no-undo.
def var lv-f as handle no-undo.

  
    run validate-screen in this-procedure no-error.    

    if return-value ne 'passed' then do:
        lv-hand = widget-handle(return-value).
        if valid-handle(lv-hand) then Do:
            lv-f = lv-hand:Frame.
            run Display-fields.
            apply 'entry' to lv-hand.
        End.
        Return 'failed'.
    end.

setsen(false).

assign frame {&frame-name}
        {&list-1}.
return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-undo-trigger win-main 
PROCEDURE local-undo-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    setsen(false).
    RUN Display-Fields.
    return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MergeControlProg win-main 
PROCEDURE MergeControlProg :
def input param pv-id as char no-undo.

Def Var lv-mergeprogram  As Char No-undo.
def var lv-MergeProcedure as char no-undo.
Def Var lv-Sourcedata    As Char No-undo.
Def Var lv-officeprogram As Char No-undo.
Def Var lv-templatefile  As Char No-undo.
Def Var lv-destfile      As Char No-undo.
Def Var lv-dstring       As Char No-undo.
def var lv-native        as char no-undo.
def var lv-printername   as char no-undo.
def var lv-extra         as char no-undo.
def var lv-notify        as char no-undo.
def var h-prog       as handle     no-undo.
def var h-applhandle as com-handle no-undo.
def var lv-mprogpath as char       no-undo.

assign
    lv-mergeprogram   = replace(GetParam(pv-id,'MergeProgram'),'\','/')
    lv-mprogpath      = substring(lv-mergeprogram,1,r-index(lv-mergeprogram,'/'))
    lv-MergeProgram   = substring(lv-mergeprogram,r-index(lv-mergeprogram,'/') + 1)
    lv-MergeProcedure = GetParam(pv-id,'MergeProcedure')
    lv-SourceData     = GetParam(pv-id,'SourceData')
    lv-OfficeProgram  = GetParam(pv-id,'OfficeProgram')
    lv-TemplateFile   = GetParam(pv-id,'TemplateFile')
    lv-DestFile       = GetParam(pv-id,'DestFile')
    lv-Native         = GetParam(pv-id,'Native')
    lv-printername    = GetParam(pv-id,'Printername')
    lv-extra          = GetParam(pv-id,'extra')
    lv-notify         = GetParam(pv-id,'Notify').

if lv-mergeprogram ne ? 
then do:                        /* more processing necessary by progress */
    msgparent('Running Merge Program ' + lv-mergeprogram).
    if lv-mergeprocedure ne ?  /* internal proc to do it */
    then do:
        msgparent('Calling Merge Routine ' + lv-mergeprocedure).

        {{&core}run.i &programc   = lv-mergeprogram
                     &pathc      = lv-mprogpath
                     &Appsrv     = "NTEnsignia"  
                     &noper      = true
                     &procedurec = lv-mergeprocedure
                     &params     = "(lv-sourcedata,
                                     lv-officeprogram,
                                     lv-templatefile,
                                     lv-destfile,
                                     lv-extra)"}
        if error-status:error then do:
            errlog('Run Merge Routine ').
            return error.
        End.

    End.
    else do:                    /* whole procedure to do it */
        msgparent('Running Merge Program ' + lv-mergeprogram).
        run lv-mergeprogram (lv-sourcedata,lv-officeprogram,lv-templatefile,lv-destfile,lv-extra).
    End.

/* what about autoprinting it out 
    if lv-printername ne '' and
       lv-printername ne ?  and
       lv-printername ne 'default' and
       lv-printername ne 'none' 
    then do:
could open word etc and do it that way.
        h-applhandle = msOpenApplication('word.application','hidden').
        if not MsSetPrinter(h-applhandle,lv-printername) 
            then errlog('MsSetPrinter').
        if lv-printername begins 'pdf' 
            then if not MsPrint(h-applhandle,lv-DestFile) 
                  then errlog('MsPrint').
        else if not MsPrint(h-applhandle,'') 
            then errlog('MsPrint').
        mscloseapplication(h-applhandle) 
or use proprint not s tidy
 RUN ProPrintFile(INPUT  SESSION:PRINTER-CONTROL-HANDLE,
                         INPUT  0,  /* Use Dialog (Disabled) */
                         INPUT  p_Window:HWND,
                         INPUT  p_FontNumber,
                         INPUT  p_PrintFile,
                         INPUT  0,  /* Page Range (Disabled) */
                         OUTPUT PrintResult).



    End.
*/
End.
else do:    /* it a straight dump into office program */
    if not StringTolog(lv-native) 
    then do:        
        case lv-officeprogram:
            when 'word' then do:
                msgparent('Loading Word').
                h-applhandle = msOpenApplication('word.application','hidden').
                if not MsMergeToWord(h-applhandle,lv-sourcedata,lv-templatefile) then errlog('MsMergeToWord').
            End.
            when 'excel'   then do:
                msgparent('Loading Excel').
                h-applhandle = msOpenApplication('excel.application','hidden').
                if not MsMergeToExcel(h-applhandle,lv-sourcedata,lv-templatefile) then errlog('MsMergeToExcel').
            End.
            when 'outlook' then do:
                msgparent('Loading Outlook').
                h-applhandle = msOpenApplication('outlook.application','hidden').
                if not MsMergeToOutlook(h-applhandle,'from',lv-notify,'Officeserver','OfficeServer',lv-sourcedata,'') then errlog('MsMergeToOutlook').
            End.
            When 'Crystal' Then do:
                /* get crystal routines from zen */
            End.
         end case.
    End.
    else do:
        msgparent('Loading Application').
        Execute(lv-sourcedata,'','','hidden'). /* prog,dir,params,mode(hidden,normal,min,max) */ 
    End.
    msgparent('Saving Document ' + lv-destfile).

    if not MSSaveAs(h-applhandle,lv-destfile,lv-templatefile) 
        then errlog('MSSaveAs').

    if lv-printername ne '' and
       lv-printername ne ?  and
       lv-printername ne 'default' and
       lv-printername ne 'none' 
    then do:
        if not MsSetPrinter(h-applhandle,lv-printername) 
            then errlog('MsSetPrinter').

        if lv-printername begins 'pdf' 
            then if not MsPrint(h-applhandle,lv-DestFile) 
                  then errlog('MsPrint').
        else if not MsPrint(h-applhandle,'') 
            then errlog('MsPrint').
    End.

    if not mscloseapplication(h-applhandle) 
        then errlog('mscloseapplication').
End.

msgparent('Complete Resting a bit').
if lv-notify ne '' then do:
    Wait(200).
/* file:\\ENSIGNNT29\DEPLOY\bob\7512.doc */
    h-applhandle = MSOpenApplication("outlook.application",'visible').
    MsMergeToOutlook(h-applhandle,'',lv-notify,
                    'OfficeServer',
                    'Youre Job Has Completed File:' + 
                    WapiGetShareName(GetIniValue('OfficeServer','ODrive')) + '\' + lv-destfile +
                    ' Will Expire in Three Days.',
                     '',
                     string(today + 3,'99/99/9999') + string(time,'hh:mm:ss am')).
    MSCloseApplication(h-applhandle).
End.
wait(500).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessFiles win-main 
PROCEDURE ProcessFiles :
os-delete value(lv-tagdir + lv-tagfile) no-error.

Def Var lv-Sourcedata    As Char No-undo.
Def Var lv-templatefile  As Char No-undo.
Def Var lv-native        As Char No-undo.
Def var X                As Int  no-undo.

Do While Not stopme():
    msgparent('Refreshing List').
    process events.

    Run GetDataFileList In This-procedure.

    {&OPEN-QUERY-br-processing}

    For Each t-toprocess where t-toprocess.t-status = 'waiting':
        msgparent('Processing ' + t-toprocess.t-filename).
        assign
            t-toprocess.t-status = 'Processing'
            lv-SourceData   = GetParam(t-toprocess.t-filename,'SourceData')
            lv-TemplateFile = GetParam(t-toprocess.t-filename,'TemplateFile')
            lv-native       = GetParam(t-toprocess.t-filename,'native').
        browse br-processing:refresh().

        if FileNotFound(lv-sourcedata)  then do:    
            errlog('Source Data ' + lv-sourcedata + ' Not Found').
            msgparent('Missing Source Data').
            next.
        End.

        if lv-templatefile ne '' and 
           lv-native       = '' then 
            if FileNotFound(lv-templatefile)  then do:    
                errlog('Template File ' + lv-templatefile + ' Not Found').
                msgparent('Missing Template File').
                next.
            End.

        run MergeControlProg (t-toprocess.t-filename) no-error.

        if not error-status:error 
        then do:
            MsgParent('Processed ' + t-toprocess.t-filename).
            CleanUpFiles(t-toprocess.t-path,lv-SourceData).               
            t-toprocess.t-status = 'Processed'.
        End.
        else message Error-Status:Get-Message(Error-Status:Num-Messages) view-as alert-box.
        br-processing:refresh() in frame {&frame-name}.    
    End.
    msgparent('Nothing To Do').
    wait(500).
End.
msgparent('Stopped').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh win-main 
PROCEDURE refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Def Input Param pv-msg As Char No-undo.

 lv-msg:screen-value In Frame {&frame-name} = pv-msg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Start-Trigger win-main 
PROCEDURE Start-Trigger :
run undo-trigger.    
    
    setsensitive(false,'inc','btn-start,btn-edit,btn-delete',frame {&frame-name}:handle).
    setsensitive(true,'inc','btn-stop',frame {&frame-name}:handle).
    
    run processfiles.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Stop-Trigger win-main 
PROCEDURE Stop-Trigger :
setsensitive(true,'inc','btn-start,btn-edit,btn-delete',frame {&frame-name}:handle).
setsensitive(false,'inc','btn-stop',frame {&frame-name}:handle).

    Output Stream op To value(lv-tagdir + lv-tagfile).
    Put Stream op 'stop now'.
    Output Stream op Close.

    msgparent('Stopping').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen win-main 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
/* validate the screen widgets etc if any fail 
    display message put focus on that widget and return 'failed' 
 e.g.

  IF lv-new THEN
      IF CAN-FIND(FIRST b-{&Table-name} WHERE b-{&Table-name}.keyfield = t-{&Table-name}.keyfield) 
      THEN DO:
          MESSAGE msg (120,"","","","") VIEW-AS ALERT-BOX.
          RETURN STRING(t-{&Table-name}.keyfield:HANDLE in frame one).
      END.
   else return 'passed'.
*/

    if not can-do('/,\',substring(lv-srvdir:screen-value in frame {&frame-name},length(lv-srvdir:screen-value),1))
    then do:
        message "Must Finish with a '\'" view-as alert-box.
        RETURN STRING(lv-srvdir:HANDLE).
    End.
    if not can-do('/,\',substring(lv-Tagdir:screen-value,length(lv-Tagdir:screen-value),1))
    then do:
        message "Must Finish with a '\'" view-as alert-box.
        RETURN STRING(lv-Tagdir:HANDLE).
    End.
    if not can-do('/,\',substring(lv-datdir:screen-value,length(lv-datdir:screen-value),1))
    then do:
        message "Must Finish with a '\'" view-as alert-box.
        RETURN STRING(lv-datdir:HANDLE).
    End.
    if not can-do('/,\',substring(lv-bakdir:screen-value,length(lv-bakdir:screen-value),1))
    then do:
        message "Must Finish with a '\'" view-as alert-box.
        RETURN STRING(lv-bakdir:HANDLE).
    End.
    if lv-tagfile:screen-value = '' 
    then do:
        message "Must Finish Not Be Blank !" view-as alert-box.
        RETURN STRING(lv-tagfile:HANDLE).
    End.

    return 'passed'.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CleanUpFiles win-main 
FUNCTION CleanUpFiles RETURNS LOGICAL
  ( pv-set as char,
    pv-data as char ) :

  msgparent('Moving Files').

  backup(pv-set,GetFullPath(lv-bakdir) + '\' + getfilename(pv-set)).
  backup(pv-data,GetFullPath(lv-bakdir) + '\' + getfilename(pv-data)).

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ErrLog win-main 
FUNCTION ErrLog RETURNS LOGICAL
  ( pv-msg as char ) :
&glob errmsg Error-Status:Get-Message(Error-Status:Num-Messages)
    create t-error.
    buffer-copy t-toprocess to t-error.

    t-error.t-status = 'Failed ' + pv-msg + '**' + {&errmsg}.
    
    setsensitive(true,'inc','btn-clear',frame {&frame-name}:handle).

    output stream err to value(lv-srvdir + 'OfficeServerErrors.log') append.

    put stream err unformatted 
            string(today,'99/99/9999') ': ' 
            string(time,'HH:MM') '* ' 
            t-error.t-status skip.

    output stream err close.

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetParam win-main 
FUNCTION GetParam RETURNS CHARACTER
  ( pv-set as char,
    pv-key as char ) :

    def var lv-str as char no-undo.

    LOAD pv-set DIR lv-datdir BASE-KEY "INI" no-error.

    USE pv-set.

    GET-KEY-VALUE SECTION "OfficeServer" KEY pv-key VALUE lv-str.

    Unload pv-set NO-ERROR.

    RETURN lv-str.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsgParent win-main 
FUNCTION MsgParent RETURNS LOGICAL
  ( pv-msg as char ) :
/*
    message pv-msg view-as alert-box.
 */
    Run Refresh (pv-msg) .
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSen win-main 
FUNCTION SetSen RETURNS LOGICAL
  ( pv-onoff as log ) :


setsensitive(pv-onoff,'inc','btn-undo,btn-save,lv-bakdir,lv-datdir,lv-srvdir,lv-tagdir,lv-tagfile',frame {&frame-name}:handle).
setsensitive(not pv-onoff,'inc','btn-edit',frame {&frame-name}:handle).

RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Stopme win-main 
FUNCTION Stopme RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
  
  RETURN not FileNotFound(lv-tagdir + lv-tagfile).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

