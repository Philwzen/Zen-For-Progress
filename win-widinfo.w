&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          schadm       PROGRESS
*/
&Scoped-define WINDOW-NAME win-main


/* Temp-Table and Buffer definitions                                    */
define temp-table t-zen-fldlook NO-UNDO LIKE zen-fldlook.
define temp-table t-zen-lookupfld NO-UNDO LIKE zen-lookupfld.



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
&glob KeepRefreshButton
/* ***************************  Definitions  ************************** */
{app-paths.i}
&glob Table-name zen-fldlook
&glob unique-key    {&table-name}TableId
&glob suppresswindow
Def Input Param Table For t-{&table-name}.
def input param pv-id as dec no-undo.
Def Input Param pv-startvalue As Char No-undo.

def var h-data as handle no-undo.
def var h-qry  as handle no-undo.
def var h-br   as handle no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-ok 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-ok 
     LABEL "OK" 
     SIZE 15 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     Btn-ok AT ROW 2.19 COL 44 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.8 BY 3.71
         .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW Template
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-fldlook T "?" NO-UNDO schadm zen-fldlook
      TABLE: t-zen-lookupfld T "?" NO-UNDO schadm zen-lookupfld
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
         COLUMN             = 18
         ROW                = 8.24
         HEIGHT             = 3.81
         WIDTH              = 97.8
         MAX-HEIGHT         = 13.48
         MAX-WIDTH          = 112
         VIRTUAL-HEIGHT     = 13.48
         VIRTUAL-WIDTH      = 112
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
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-ok win-main
ON CHOOSE OF Btn-ok IN FRAME f-main /* OK */
DO:
  run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK win-main 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/* main core logic */
 {{&core}commonmaint.i &path = "{&sys}{&srv}"}
/* &extraparams  = "whatever you want,"}*/

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

find t-{&table-name} where t-{&table-name}.{&table-name}tableid = pv-id no-lock no-error.
if not avail t-{&table-name} then run exit-trigger.

frame {&frame-name}:title = t-zen-fldlook.Window-title.
run GetFieldRecords(t-{&table-name}.{&table-name}tableid).

    RUN enable_UI.
run getdata (pv-startvalue).
if return-value = 'none' then run exit-trigger.

run dispdata.
if return-value = 'none' then run exit-trigger.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DispData win-main 
PROCEDURE DispData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var h-buf     as handle no-undo.
def var h-fld     as handle no-undo.
def var h-fillin  as handle no-undo.
def var h-label   as handle no-undo.
def var x         as int    no-undo.
def var offset    as int    no-undo.
def var labelX    as int    no-undo.
def var lv-filler as char   no-undo.
def var lv-row as int no-undo init 2.
def var lv-bgcolor as int no-undo.
lv-bgcolor = GetColour('{&displayField}').
h-buf = h-data:default-buffer-handle.
If Valid-handle(h-QrY) Then do:
   h-QrY:QUERY-CLOSE().
end.
If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.

create query h-qry.
h-QrY:Add-buffer(h-buf).
h-qry:query-prepare('for each t-' + t-zen-fldlook.tablename + ' no-lock ').
h-qry:query-open.
h-qry:get-first().
if h-qry:query-off-end then 
do:
    message 'There is no lookup information available' skip
            'for this value: ' pv-startvalue + '.'
    view-as alert-box information.
    return 'none'.
end.

do x = 1 to h-buf:num-fields:
    h-fld = h-buf:Buffer-field(x).

    find t-zen-lookupfld where t-zen-lookupfld.fieldname = h-fld:name
                         no-lock no-error.

    frame {&frame-name}:height = lv-row + 4.
    btn-ok:row = lv-row + 2.
    create text h-fillin
    assign 
      frame     = Frame {&frame-name}:handle
      data-type = h-fld:Data-Type
      format    = if avail t-zen-lookupfld and t-zen-lookupfld.InfoFormat ne '' 
                    then t-zen-lookupfld.InfoFormat
                    else h-fld:Format
      width     = Frame {&frame-name}:width - 26
      name      = h-fld:name
      screen-value = h-fld:buffer-value
      row       = lv-row
      column    = 23   
      visible   = yes    
      read-only = yes
      bgcolor = lv-bgcolor
      sensitive = false.
    lv-row = lv-row + 1.

    if not valid-handle(h-fillin:side-label-handle)
    then do:
        assign
            offset = font-table:get-text-width-pixels (h-fld:Label + ": ",Frame {&frame-name}:font) + 1
            labelX = h-fillin:x - offset. 
        if LabelX > 0 then do:
            create text h-label 
            assign      
                frame        = frame {&frame-name}:handle
                row          = h-fillin:row + .2
                format       = "x(" + string(length(h-fld:Label + ":" ) + 1) + ")"         
                screen-value = h-fld:Label + ":"       
                x = LabelX
                width-pixels = offset - font-table:get-text-width-pixels(" ",Frame {&frame-name}:font) 
                visible      = yes.
            h-fillin:side-label-handle = h-label.
        end.
    end.
end. 
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
  ENABLE Btn-ok 
      WITH FRAME f-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetData win-main 
PROCEDURE GetData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param pv-select as char no-undo.

ErrorClear().
{{&core}run.i &program   = "dyn-lookup.p"
            &path      = "{&core}{&srv}"
            &Appsrv    = "System"  
            &procedure = "getrecord"
            &params    = "(pv-id,
                           pv-select,
                           output table-handle h-data)"}


if AnyErrors() then do:
    message 'error' view-as alert-box.
    return error.    
end.

if not valid-handle(h-data) then
do:
    message 'There is no lookup information available' skip
            'for this value: ' pv-startvalue + '.'
    view-as alert-box information.
    return 'none'.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFieldRecords win-main 
PROCEDURE GetFieldRecords :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param pv-id as dec no-undo.

def var lv-where as char no-undo.
empty temp-table t-zen-lookupfld.

ErrorClear().
lv-where = 'where zen-lookupfld.zen-fldlooktableid = ' + string(pv-id).

{{&core}run.i &program   = "dynamic.p"
            &path      = "{&core}{&srv}"
            &Appsrv    = "System"  
            &procedure = "GetAllRecords"
            &params    = "('zen-lookupfld',
                           lv-where,
                           input-output table t-zen-lookupfld)"}

if AnyErrors() then do:
    message 'error' view-as alert-box.
    return error.    
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

