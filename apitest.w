&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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

/* ***************************  Definitions  ************************** */
{app-paths.i}
&glob title-text ?????       /* message box title */
&glob NoButtons         /* no buttons displayed */
{{&core}def-table.i &table-name = zen-apidetail}

/* ***************************  Definitions  ************************** */
def var lv-data      as char   no-undo.
    {{&core}load-library.i &LibraryName = "api" &AsSuper = 'no'}  

def temp-table apicall no-undo
    field params as char extent 20
    field apiname as char.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-ip lv-op btn-libfunc btn-runi ~
btn-localfunc 
&Scoped-Define DISPLAYED-OBJECTS lv-ip lv-op 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Callapi2 win-main 
FUNCTION Callapi2 RETURNS CHARACTER
  ( pv-api as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Getapirecord win-main 
FUNCTION Getapirecord RETURNS CHARACTER
  ( pv-api as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-libfunc 
     LABEL "Lib func" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-localfunc 
     LABEL "Local Func" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-runi 
     LABEL "run.i" 
     SIZE 15 BY 1.14.

def var lv-ip as char FORMAT "X(256)":U 
     LABEL "Ip" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.19 NO-UNDO.

def var lv-op as char FORMAT "X(256)":U 
     LABEL "op" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     lv-ip AT ROW 2.43 COL 10 COLON-ALIGNED
     lv-op AT ROW 4.57 COL 10 COLON-ALIGNED
     btn-libfunc AT ROW 6.48 COL 23
     btn-runi AT ROW 6.48 COL 42
     btn-localfunc AT ROW 6.52 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 59.6 BY 7.48.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
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
         ROW                = 7.1
         HEIGHT             = 7.62
         WIDTH              = 59.6
         MAX-HEIGHT         = 46.52
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.52
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

&Scoped-define SELF-NAME btn-libfunc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-libfunc win-main
ON CHOOSE OF btn-libfunc IN FRAME f-main /* Lib func */
DO:
def var lv-x as char no-undo.

assign frame {&frame-name}
    lv-ip
    lv-op:screen-value = ''.

def var h-b as handle no-undo.
Create Buffer h-b For Table 'apicall'.
h-b:empty-temp-table().
create apicall.
assign apiname  = 'test'
       params[1] = lv-ip
       params[2] = lv-op.

lv-x = CallApi(h-b).
h-b:find-first.
lv-op = h-b:BUFFER-FIELD('params'):BUFFER-VALUE(2).

message 'error-status ' error-status:error skip
        'return-vlaue ' lv-x skip
        'ip ' lv-ip skip
        'op ' lv-op skip
        'data ' lv-data  skip
view-as alert-box.
disp 
lv-op
with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-localfunc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-localfunc win-main
ON CHOOSE OF btn-localfunc IN FRAME f-main /* Local Func */
DO:
def var lv-x as char no-undo.

assign frame {&frame-name}
    lv-ip
    lv-op:screen-value = ''.

def var h-b as handle no-undo.
Create Buffer h-b For Table 'apicall'.
h-b:empty-temp-table().
create apicall.
assign apiname  = 'test'
       params[1] = lv-ip
       params[2] = lv-op.

lv-x = CallApi2(h-b).
h-b:find-first.
lv-op = h-b:BUFFER-FIELD('params'):BUFFER-VALUE(2).

message 'error-status ' error-status:error skip
        'return-vlaue ' lv-x skip
        'ip ' lv-ip skip
        'op ' lv-op skip
        'data ' lv-data  skip
view-as alert-box.
disp 
lv-op
with frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-runi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-runi win-main
ON CHOOSE OF btn-runi IN FRAME f-main /* run.i */
DO:
    assign frame {&frame-name}
        lv-ip
        lv-op:screen-value = ''.

{{&core}run.i &api = "test"
              &params = "(lv-ip,output lv-op)"}

    disp lv-op with frame {&frame-name}.

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
   {{&core}sec-chk.i}   /* screen security check */
    RUN enable_UI.
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
  DISPLAY lv-ip lv-op 
      WITH FRAME f-main.
  ENABLE lv-ip lv-op btn-libfunc btn-runi btn-localfunc 
      WITH FRAME f-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn win-main 
PROCEDURE Local-ChildReturn :
/*------------------------------------------------------------------------------
  Purpose: run after a child exits     
  Parameters:  pv-from is name of child procedure
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-from as handle no-undo.

case pv-from:

end case.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialise win-main 
PROCEDURE Local-Initialise :
/* allow customising of initialisation 
return 'overide' to stop default processing
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Update-Child-Procedures win-main 
PROCEDURE Local-Update-Child-Procedures :
/*------------------------------------------------------------------------------
  Purpose: refesh any child procedures    
  Parameters: pv-to handle of child requesting refresh
  Notes:       
------------------------------------------------------------------------------*/

def input param pv-to as handle no-undo.

case pv-to:private-data:
/*     when 'order-maint.w' then run refresh in  pv-to (t-customer.cust-num).  */
/*     when 'sr-maint.w'    then run refresh in  pv-to.                        */
end case.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh win-main 
PROCEDURE Refresh :
/*------------------------------------------------------------------------------
  Purpose: called from parent to refesh itself    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Callapi2 win-main 
FUNCTION Callapi2 RETURNS CHARACTER
  ( pv-api as handle) :

def var hcall        as handle no-undo.
def var lv-api       as char no-undo.
def var h            as handle no-undo.
def var x            as int    no-undo.
def var lv-paramlist as char   no-undo.
def var lv-datatype  as char   no-undo.
def var lv-value     as char   no-undo.
def var lv-mode      as char   no-undo.
def var lv-rvalue    as char   no-undo.

pv-api:find-first.
lv-api = pv-api:BUFFER-FIELD("apiname"):BUFFER-VALUE.

empty temp-table t-zen-apidetail.
GetApiRecord(lv-api).
find t-zen-apidetail where t-zen-apidetail.apiname = lv-api no-lock no-error.
if not avail t-zen-apidetail 
then return error 'No Api Found' + lv-api.

CREATE CALL hCall. 
assign
    HCALL:CALL-TYPE  = procedure-CALL-TYPE 
    hCall:CALL-NAME  = t-zen-apidetail.programname
    hCall:PERSISTENT = true.

h = ?. /* getappserverhandle(t-zen-apidetail.defaultapserver).*/
if not valid-handle(h) 
    then h = session:first-procedure.
    else hcall:server = h.

do while valid-handle(h):
    if h:private-data = t-zen-apidetail.programname
    then leave.
    h = h:next-sibling.
end.

if not valid-handle(h) 
    then hcall:INVOKE. 
    else hCall:IN-HANDLE = h.

assign
    h                    = hCall:IN-HANDLE
    hcall:asynchronous   = t-zen-apidetail.async 
    lv-paramlist         = h:get-signature(t-zen-apidetail.procedurename)
    lv-paramlist         = substring(lv-paramlist,index(lv-paramlist,',') + 2) /* mode name type */
    hCall:CALL-NAME      = t-zen-apidetail.procedurename
    hCall:NUM-PARAMETERS = num-entries(lv-paramlist).

/* if num-entries(lv-paramlist) ne num-entries(pv-paramdata) */
/* then do:                                                  */
/*     DELETE PROCEDURE h.                                   */
/*     DELETE OBJECT hCall.                                  */
/*     return error 'Mismatched Params ' + pv-api.           */
/* end.                                                      */

do x = 1 to num-entries(lv-paramlist):
    assign
        lv-datatype = entry(3,entry(x,lv-paramlist),' ')
        lv-mode     = entry(1,entry(x,lv-paramlist),' ').
    hCall:SET-PARAMETER(x,lv-datatype,lv-mode,pv-api:BUFFER-FIELD('params'):BUFFER-VALUE(x)).
end.

hCall:INVOKE.
lv-rvalue = string(hcall:return-value).
DELETE PROCEDURE h.
DELETE OBJECT hCall.

RETURN  string(hcall) + ',' + 
        string(h).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Getapirecord win-main 
FUNCTION Getapirecord RETURNS CHARACTER
  ( pv-api as char ) :
    {{&core}run.i &program   = "zen-apidetail.p"
                  &path      = "{&core}{&srv}"
                  &Appsrv    = "System"  
                  &procedure = "find-record"
                  &params    = "(pv-api,input-output table t-zen-apidetail)"}
  RETURN "".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

