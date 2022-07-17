&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          centrec          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-Printer NO-UNDO LIKE zen-Printer.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/
CREATE WIDGET-POOL.
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{app-paths.i}

&glob title-text Select zen-printer
&glob KeepRefreshButton
&glob table-name zen-printer
&glob unique-key {&table-name}tableid
&glob defbutlist        Help^ttf,Exit^ttf{&Delim2}
&glob noexitcheck 
&Glob ImmediateQuery true
&glob nochangedcheck true
&glob useBrDblClick
&glob justLoadDefaults
/* load defaults without actually creating the defaults button.  This was put
   in place, so multiple users could have different tray defaults. */

def input param  lv-parenthandle as handle no-undo.
def output param lv-printerparams as char no-undo init 'NONE'.
def var lv-repname as char no-undo.
def var lv-params  as char no-undo.

def var lv-userid  as char no-undo.
def var lv-syscode as char no-undo.

   define temp-table validprinter no-undo
     field DeviceName as char
     index order is primary DeviceName.

if not valid-handle(lv-parenthandle) then do: /* bodge !! */
lv-parenthandle = getprochandle('local',entry(2,program-name(3),' ')).
/* message program-name(3) skip */
/*     lv-parenthandle:name {&dbt}. */
end.    

run SubscribeToAll in lv-parenthandle (this-procedure,'parent').

def var lv-usecrystal as log no-undo.
lv-usecrystal = pgmproperty(lv-parenthandle:name,'UseCrystal') = 'yes'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME br-maint

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-zen-printer

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-printer.DeviceName ~
t-zen-printer.Description t-zen-printer.PhysicalType 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-printer NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-printer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-printer
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-printer


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint lv-copies btnOK 
&Scoped-Define DISPLAYED-OBJECTS lv-batch cb-que 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-copies lv-batch lv-filename 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnOK 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cb-que AS CHARACTER FORMAT "X(256)":U 
     LABEL "Queue" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE lv-copies AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Copies" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-filename AS CHARACTER FORMAT "X(256)":U 
     LABEL "File Name" 
     VIEW-AS FILL-IN 
     SIZE 44.4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-batch AS LOGICAL INITIAL no 
     LABEL "Process Later in a Batch Session" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-printer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint Dialog-Frame _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-printer.DeviceName COLUMN-LABEL "Printer" FORMAT "x(20)":U
      t-zen-printer.Description FORMAT "x(40)":U
      t-zen-printer.PhysicalType FORMAT "x(15)":U WIDTH 13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 79 BY 9.57 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     br-maint AT ROW 1.48 COL 11 HELP
          "Select the record to edit."
     lv-copies AT ROW 11.24 COL 18 COLON-ALIGNED
     lv-batch AT ROW 11.24 COL 27
     cb-que AT ROW 12.43 COL 17 COLON-ALIGNED WIDGET-ID 4
     lv-filename AT ROW 13.62 COL 17 COLON-ALIGNED
     btnOK AT ROW 14.81 COL 33 WIDGET-ID 2
     SPACE(42.99) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select Printer".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-Printer T "?" NO-UNDO centrec zen-Printer
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB br-maint 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR COMBO-BOX cb-que IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lv-batch IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-copies IN FRAME Dialog-Frame
   NO-DISPLAY 1                                                         */
/* SETTINGS FOR FILL-IN lv-filename IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       lv-filename:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-printer"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-zen-printer.DeviceName
"DeviceName" "Printer" "x(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.t-zen-printer.Description
     _FldNameList[3]   > Temp-Tables.t-zen-printer.PhysicalType
"PhysicalType" ? ? "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select Printer */
DO:
  run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
  run prt-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-batch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-batch Dialog-Frame
ON VALUE-CHANGED OF lv-batch IN FRAME Dialog-Frame /* Process Later in a Batch Session */
DO:
  cb-que:sensitive = self:checked.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{{&core}commonmaint.i &path = "{&server}{&tables}"
                      &extraparams = "lv-userid,"}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {{&core}sec-chk.i}
  RUN enable_UI.
  run initialise.
  {{&core}wid-chk.i}
  {{&core}focus.i}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY lv-batch cb-que 
      WITH FRAME Dialog-Frame.
  ENABLE br-maint lv-copies btnOK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-br-changed-trigger Dialog-Frame 
PROCEDURE local-br-changed-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* yuck there has to be a better way */

if avail t-zen-printer then do:
   assign
      lv-filename:visible in frame {&frame-name}  = (t-zen-printer.DeviceName begins 'file' or 
                                                     t-zen-printer.DeviceName begins 'html')
      lv-copies:hidden =  not t-zen-printer.LocalPrinter 
      lv-copies:hidden = (t-zen-printer.DeviceName begins 'file' or
                          t-zen-printer.DeviceName begins 'screen' or
                          t-zen-printer.DeviceName begins 'fax')  
      lv-batch:hidden = (t-zen-printer.DeviceName begins 'screen' or
                         t-zen-printer.DeviceName begins 'fax')
      cb-que:hidden = lv-batch:hidden.
end.

  lv-filename:sensitive = lv-filename:visible.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-br-msdblclick-trigger Dialog-Frame 
PROCEDURE local-br-msdblclick-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   run prt-trigger in this-procedure.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise Dialog-Frame 
PROCEDURE local-initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* lv-syscode  = getSysCode().   /* in rexlibrary.p */ */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-openquery Dialog-Frame 
PROCEDURE local-openquery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def var lv-prtlist as char no-undo.
   def var lv-defprt as char no-undo.
   def var x as int no-undo.
   
   RUN Clear-tables in this-procedure no-error.
   
   {{&core}run.i &program   = "{&table-name}.p"
                 &path      = "{&core}{&srv}"
                 &Appsrv    = "System"  
                 &procedure = "get-printers"
                 &params    = "(output table t-{&table-name})"}
   
   /* make a list of the local printers */
   lv-prtlist = session:get-printers().
   
   /* fix up names to match the local printer name - add session #, etc. */
   do x = 1 to num-entries(lv-prtlist):
      find t-zen-printer where t-zen-printer.DeviceName = entry(x,lv-prtlist) no-error.
      if not avail t-zen-printer 
      then do:
         find t-zen-printer where 
             entry(x,lv-prtlist) matches t-zen-printer.DeviceName no-error.
         if avail t-zen-printer then t-zen-printer.DeviceName = entry(x,lv-prtlist).
      end.
      if avail t-zen-printer 
      then do:
        create validprinter.
        validprinter.DeviceName = t-zen-printer.DeviceName.
      end.
   end.
   
   /* only display remote or good local printers */
   for each t-{&table-name} where t-zen-printer.DeviceName ne 'file'
                              and t-zen-printer.DeviceName ne 'screen' 
                              and t-zen-printer.DeviceName ne 'fax'   
                              and t-zen-printer.DeviceName ne 'email'
                              and t-zen-printer.LocalPrinter
                              and not can-find(validprinter where validprinter.DeviceName = t-zen-printer.DeviceName):

      if not can-do(lv-prtlist,t-zen-printer.DeviceName)
         then delete t-zen-printer.

   end.

   lv-defprt = GetField('zen-duser','duser',lv-userid,'def-printer').
   lv-defprt = replace(lv-defprt,'*','').

   {&OPEN-QUERy-{&BROWSE-NAME}}
   find first t-{&table-name} where t-zen-printer.DeviceName begins lv-defprt no-lock no-error.
   if avail t-{&table-name} then
   reposition {&BROWSE-NAME} to rowid rowid(t-{&table-name}).

   RUN br-changed-trigger.
   
   return 'override'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prt-trigger Dialog-Frame 
PROCEDURE prt-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   assign frame {&frame-name}
      {&list-1}
      cb-que
      lv-printerparams = string(t-zen-printer.zen-printertableid) + '{&Delim2}' +
                         t-zen-printer.DeviceName           + '{&Delim2}' +
                         string(lv-copies)                + '{&Delim2}' +
                         ''                  + '{&Delim2}' +
                         lv-filename                      + '{&Delim2}' + 
                         string(lv-batch)                 + '{&Delim2}' + 
                         cb-que                           +  '{&Delim2}'.

   if (t-zen-printer.DeviceName = 'file' or t-zen-printer.DeviceName = 'HTML')
      and lv-filename = ''
   then do:
      message 'Enter a file name.' view-as alert-box.
      return.
   end.
   run exit-trigger.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

