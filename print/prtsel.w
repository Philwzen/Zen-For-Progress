&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rex              PROGRESS
*/
&Scoped-define WINDOW-NAME win-main


/* Temp-Table and Buffer definitions                                    */
define temp-table t-printer NO-UNDO LIKE printer.



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

/* ***************************  Definitions  ************************** */
{app-paths.i}

&glob title-text Select Printer
&glob KeepRefreshButton
&glob table-name printer
&glob unique-key {&table-name}tableid
&glob defbutlist        Help^ttf,Exit^ttf{&Delim2}
&glob noexitcheck 
&Glob ImmediateQuery true
&glob nochangedcheck true
&glob suppresswindow
&glob useBrDblClick
&glob justLoadDefaults
/* load defaults without actually creating the defaults button.  This was put
   in place, so multiple users could have different tray defaults. */

def input param  lv-parenthandle as handle no-undo.
def var lv-printerparams as char no-undo init 'NONE'.
def var lv-repname as char no-undo.
def var lv-params  as char no-undo.
def var lv-userid  as char no-undo.
def var lv-syscode as char no-undo.
def var lv-usecrystal as log no-undo.

if valid-handle(lv-parenthandle) then do:
    run SubscribeToAll in lv-parenthandle (this-procedure,'parent') no-error.
    lv-usecrystal = pgmproperty(lv-parenthandle:name,'UseCrystal') = 'yes'.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main
&Scoped-define BROWSE-NAME br-maint

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-printer

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-printer.printer-name ~
t-printer.descr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&scoped-define query-STRING-br-maint FOR EACH t-printer NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-printer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-printer
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-printer


/* Definitions for FRAME f-main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint lv-copies lv-tray btnOK 
&Scoped-Define DISPLAYED-OBJECTS lv-tray lv-batch cb-que 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-copies lv-tray lv-batch lv-filename 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnOK 
     LABEL "OK" 
     SIZE 15 BY 1.14.

def var cb-que as char FORMAT "X(256)":U 
     LABEL "Queue" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

def var lv-copies as int FORMAT ">9":U INITIAL 0 
     LABEL "Copies" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
      NO-UNDO.

def var lv-filename as char FORMAT "X(256)":U 
     LABEL "File Name" 
     VIEW-AS FILL-IN 
     SIZE 44.4 BY 1
      NO-UNDO.

def var lv-tray as char INITIAL "l" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Upper Tray", "u",
"Lower Tray", "l"
     SIZE 19.2 BY 2
      NO-UNDO.

def var lv-batch as log INITIAL no 
     LABEL "Process Later in a Batch Session" 
     VIEW-AS TOGGLE-BOX
     SIZE 42.4 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query br-maint FOR 
      t-printer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint win-main _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-printer.printer-name COLUMN-LABEL "Printer" FORMAT "x(60)":U
            WIDTH 75.2
      t-printer.descr COLUMN-LABEL "Description" FORMAT "x(55)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 134 BY 9.57
          FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     br-maint AT ROW 1.48 COL 11 HELP
          "Select the record to edit." WIDGET-ID 200
     lv-copies AT ROW 11.24 COL 18 COLON-ALIGNED WIDGET-ID 8
     lv-tray AT ROW 11.24 COL 32.8 NO-LABEL WIDGET-ID 12
     lv-batch AT ROW 11.24 COL 101 WIDGET-ID 6
     cb-que AT ROW 12.38 COL 99 COLON-ALIGNED WIDGET-ID 4
     lv-filename AT ROW 12.81 COL 22.4 COLON-ALIGNED WIDGET-ID 10
     btnOK AT ROW 14.48 COL 67 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 147.2 BY 15.05
          WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: t-printer T "?" NO-UNDO rex printer
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW win-main ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 7.4
         ROW                = 31
         HEIGHT             = 15.05
         WIDTH              = 147.2
         MAX-HEIGHT         = 15.05
         MAX-WIDTH          = 147.2
         VIRTUAL-HEIGHT     = 15.05
         VIRTUAL-WIDTH      = 147.2
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN win-main = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
/* BROWSE-TAB br-maint 1 f-main */
/* SETTINGS FOR COMBO-BOX cb-que IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lv-batch IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-copies IN FRAME f-main
   NO-DISPLAY 1                                                         */
/* SETTINGS FOR FILL-IN lv-filename IN FRAME f-main
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       lv-filename:HIDDEN IN FRAME f-main           = TRUE.

/* SETTINGS FOR RADIO-SET lv-tray IN FRAME f-main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-printer"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-printer.printer-name
"t-printer.printer-name" "Printer" "x(60)" "character" ? ? ? ? ? ? no ? no no "75.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-printer.descr
"t-printer.descr" "Description" "x(55)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK win-main
ON CHOOSE OF btnOK IN FRAME f-main /* OK */
DO:
  run prt-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-batch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-batch win-main
ON VALUE-CHANGED OF lv-batch IN FRAME f-main /* Process Later in a Batch Session */
DO:
  cb-que:sensitive = self:checked.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK win-main 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/* main core logic */
{{&core}commonmaint.i &path = "{&server}{&tables}"
                      &extraparams = "lv-userid,"}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {{&core}sec-chk.i}   /* screen security check */
    RUN enable_UI.
    RUN initialise.
   {{&core}wid-chk.i}   /* widgetlevel security check */
   {{&core}focus.i}     /* set focus to first enabled widget */
IF NOT THIS-PROCEDURE:PERSISTENT OR SESSION:DISPLAY-TYPE ne "GUI":U then
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  /* Hide all frames. */
  HIDE FRAME f-main.
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
  DISPLAY lv-tray lv-batch cb-que 
      WITH FRAME f-main.
  ENABLE br-maint lv-copies lv-tray btnOK 
      WITH FRAME f-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-afterinitialise win-main 
PROCEDURE Local-afterinitialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   if PgmUseDefaults(this-procedure) then
      LoadFieldDefaults(this-procedure,frame {&frame-name}:handle).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Br-Changed-Trigger win-main 
PROCEDURE Local-Br-Changed-Trigger :
/* yuck there has to be a better way */

if avail t-printer then do:
   assign
      lv-filename:visible in frame {&frame-name}  = (t-printer.printer-name begins 'file' or 
                                                     t-printer.printer-name begins 'html')
      lv-copies:hidden =  not t-printer.local-printer 
      lv-copies:hidden = (t-printer.printer-name begins 'file' or
                          t-printer.printer-name begins 'screen' or
                          t-printer.printer-name begins 'fax')  
      lv-batch:hidden = (t-printer.printer-name begins 'screen' or
                         t-printer.printer-name begins 'fax')
      lv-tray:sensitive = (t-printer.c-low-tray ne '' or
                           t-printer.c-up-tray  ne '' or
                           t-printer.low-tray  ne '' or
                           t-printer.up-tray ne '')
      lv-tray:screen-value = (if t-printer.default-tray ne "" and
         t-printer.default-tray ne ? then t-printer.default-tray else
         input frame {&frame-name} lv-tray)
      cb-que:hidden = lv-batch:hidden.
end.

assign
  lv-tray:hidden     = not lv-tray:sensitive
  lv-filename:sensitive = lv-filename:visible.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Br-MsDblClick-Trigger win-main 
PROCEDURE Local-Br-MsDblClick-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   run prt-trigger in this-procedure.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialise win-main 
PROCEDURE Local-Initialise :
lv-syscode  = getSysCode().   /* in rexlibrary.p */
lv-userid   = getUserid().    /* in zenlibrary.p */
lv-copies:screen-value in frame {&frame-name} = '1'.


if not lv-usecrystal 
then do:
   lv-batch:sensitive = true.
   buildcombo(cb-que:handle,
              'zen-tserver',
               'taskserver',
               'taskserver',
               '','',no,no).
end.
else assign lv-batch:sensitive = no
            cb-que:sensitive = no.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-OpenQuery win-main 
PROCEDURE Local-OpenQuery :
def var lv-prtlist as char no-undo.
   def var lv-defprt as char no-undo.
   def var x as int no-undo.

   RUN Clear-tables in this-procedure no-error.
   
   
   /* get the printers from the database, either all printers or the ones to 
      which the user is restricted.  Always get the screen */
   {{&core}run.i 
      &program   = "{&table-name}.p"
      &path      = "{&server}{&tables}"
      &Appsrv    = "System"  
      &procedure = "get-printers"
      &params    = "(lv-userid,lv-syscode,output table t-{&table-name})"}
   
   /* make a list of the local printers */
   lv-prtlist = session:get-printers().
   
   /* fix up names to match the local printer name - add session #, etc. */
   do x = 1 to num-entries(lv-prtlist):
      if not can-find(t-printer where 
         t-printer.printer-name = entry(x,lv-prtlist))
      then do:
         find t-printer where 
             entry(x,lv-prtlist) matches t-printer.printer-name no-error.
         if avail t-printer then t-printer.printer-name = entry(x,lv-prtlist).
      end.
   end.
   
   /* only display remote or good local printers */
   for each t-{&table-name} where 
   t-printer.printer-name ne 'file'   and 
   t-printer.printer-name ne 'screen' and
   t-printer.printer-name ne 'fax'    and
   t-printer.printer-name ne 'email':
      if t-printer.local-printer and
         not can-do(session:get-printers(),t-printer.printer-name)
         then delete t-printer.
   end.
   lv-defprt = GetField('s-user','signonid',lv-userid,'def-printer').
   lv-defprt = replace(lv-defprt,'*','').

   {&OPEN-QUERy-{&BROWSE-NAME}}
   find first t-{&table-name} where printer-name begins lv-defprt no-lock no-error.
   if avail t-{&table-name} then
   reposition {&BROWSE-NAME} to rowid rowid(t-{&table-name}).

   RUN br-changed-trigger.
   
   return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prt-Trigger win-main 
PROCEDURE Prt-Trigger :
assign frame {&frame-name}
      {&list-1}
      cb-que
      lv-printerparams = string(t-printer.printertableid) + '{&Delim2}' +
                         t-printer.printer-name           + '{&Delim2}' +
                         string(lv-copies)                + '{&Delim2}' +
                         string(lv-tray)                  + '{&Delim2}' +
                         lv-filename                      + '{&Delim2}' + 
                         string(lv-batch)                 + '{&Delim2}' + 
                         cb-que                           +  '{&Delim2}'.

   if (t-printer.printer-name = 'file' or t-printer.printer-name = 'HTML')
      and lv-filename = ''
   then do:
      message 'Enter a file name.' view-as alert-box.
      return.
   end.
   
   run exit-trigger.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-info win-main 
PROCEDURE send-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param pv-param as char no-undo.
pv-param = lv-printerparams.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

