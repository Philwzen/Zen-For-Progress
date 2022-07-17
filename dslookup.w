&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME ialog-Frame


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-fldlook NO-UNDO LIKE zen-fldlook.
DEFINE TEMP-TABLE t-zen-lookupfld NO-UNDO LIKE zen-lookupfld.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS ialog-Frame 
CREATE WIDGET-POOL.
{app-paths.i}

&glob table-name zen-fldlook

Def Input Param Table For t-{&table-name}.
def input param pv-id as dec  no-undo.
Def input Param pv-startvalue As Char No-undo.
def var lv-page as int no-undo init 0.
def var h-browse  as handle no-undo.
def var h-dataset AS HANDLE NO-UNDO.
def var lv-where as char no-undo.

def var lv-browseby as char no-undo.
def var lv-keyfield     as char     no-undo.
def var lv-searchfield1  as char     no-undo.
def var lv-searchfield2  as char     no-undo.
def var lv-descfield    as char     no-undo.
def var lv-key  as char no-undo.
def var lv-desc as char no-undo.

def var lv-returning as char no-undo.
def var lv-properorder as char no-undo.

def var lv-field as char no-undo.
def var lv-tables as char no-undo.
def var lv-topwhere as char no-undo.
def var lv-topby as char no-undo.
def var lv-getwhere as char no-undo.
def var lv-getby        as char  no-undo.
def var lv-acctdefid as char no-undo. /* '248' on guidev */
def var lv-delim as char no-undo init ' '.

def var lv-gotlast as log no-undo.
/* maybe a new field on lookup record */
lv-delim = GetCtrl('LookupDelim') . 
lv-where = ' '.

if asc(lv-delim) = -1 then lv-delim = {&LookupDelim}.
pv-startvalue = replace(pv-startvalue,"'","").
pv-startvalue = replace(pv-startvalue,"~~","").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME ialog-Frame

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BuildTopWhere ialog-Frame 
FUNCTION BuildTopWhere RETURNS CHARACTER
  ( pv-where as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetBrowseRecordId ialog-Frame 
FUNCTION GetBrowseRecordId RETURNS DECIMAL
  ( pv-browse as handle,
    pv-mode as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldHandle ialog-Frame 
FUNCTION GetFieldHandle RETURNS HANDLE
  ( pv-buffer as handle,
    pv-fname as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetStartValue ialog-Frame 
FUNCTION GetStartValue RETURNS CHARACTER
  ( pv-table as char ,
    pv-field as char,
    pv-value as char,
    pv-s1 as char,
    pv-s2 as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReorderBrowseColumns ialog-Frame 
FUNCTION ReorderBrowseColumns RETURNS CHARACTER
  ( pv-browse as handle,
    pv-fieldorder as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidSearchValue ialog-Frame 
FUNCTION ValidSearchValue returns logical
  (input-output pv-search as char ,
   pv-type as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-all 
     LABEL "Get All" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-exit AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-search 
     LABEL "Search" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME ialog-Frame
     SPACE(82.81) SKIP(20.39)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>" WIDGET-ID 100.

DEFINE FRAME FRAME-ok
     Btn-all AT ROW 1.48 COL 5 WIDGET-ID 24 NO-TAB-STOP 
     btn-ok AT ROW 1.48 COL 47 WIDGET-ID 30 NO-TAB-STOP 
     btn-exit AT ROW 1.48 COL 64 WIDGET-ID 4 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 19.67 SCROLLABLE  WIDGET-ID 200.

DEFINE FRAME FRAME-select
     lv-search AT ROW 1.48 COL 23 COLON-ALIGNED WIDGET-ID 28
     btn-search AT ROW 1.48 COL 65 WIDGET-ID 6 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82 BY 3.1 WIDGET-ID 300.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-fldlook T "?" NO-UNDO schadm zen-fldlook
      TABLE: t-zen-lookupfld T "?" NO-UNDO schadm zen-lookupfld
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ok:FRAME = FRAME ialog-Frame:HANDLE
       FRAME FRAME-select:FRAME = FRAME ialog-Frame:HANDLE.

/* SETTINGS FOR FRAME FRAME-ok
   Size-to-Fit                                                          */
ASSIGN 
       FRAME FRAME-ok:SCROLLABLE       = FALSE.

/* SETTINGS FOR FRAME FRAME-select
                                                                        */
/* SETTINGS FOR DIALOG-BOX ialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-select:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-ok:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME ialog-Frame:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-ok
/* Query rebuild information for FRAME FRAME-ok
     _Query            is NOT OPENED
*/  /* FRAME FRAME-ok */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-select
/* Query rebuild information for FRAME FRAME-select
     _Query            is NOT OPENED
*/  /* FRAME FRAME-select */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX ialog-Frame
/* Query rebuild information for DIALOG-BOX ialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX ialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME ialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ialog-Frame ialog-Frame
ON WINDOW-CLOSE OF FRAME ialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ok
&Scoped-define SELF-NAME Btn-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-all ialog-Frame
ON CHOOSE OF Btn-all IN FRAME FRAME-ok /* Get All */
DO:
  lv-search = GetStartValue(t-zen-fldlook.tablename,
                            lv-keyfield,
                            pv-startvalue,
                            lv-searchfield1,
                            lv-searchfield2).
                          
    disp lv-search with frame frame-select.       
   delete object h-browse no-error.
  lv-page = ?.
 run startsearch('all').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-exit ialog-Frame
ON CHOOSE OF btn-exit IN FRAME FRAME-ok /* Cancel */
DO:
return pv-startvalue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok ialog-Frame
ON CHOOSE OF btn-ok IN FRAME FRAME-ok /* OK */
DO:
run ok-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-select
&Scoped-define SELF-NAME btn-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-search ialog-Frame
ON CHOOSE OF btn-search IN FRAME FRAME-select /* Search */
DO:
   apply 'tab' to lv-search. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search ialog-Frame
ON CTRL-D OF lv-search IN FRAME FRAME-select /* Search */
DO:
  run extrainfo in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search ialog-Frame
ON RETURN OF lv-search IN FRAME FRAME-select /* Search */
DO:
   apply 'tab' to lv-search.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search ialog-Frame
ON TAB OF lv-search IN FRAME FRAME-select /* Search */
DO:
 if lv-page ne ? 
    then run startsearch(''). 
    else run proc-findrow (lv-search:screen-value in frame frame-select).
    if return-value = 'failed' then return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME ialog-Frame
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK ialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

on 'f4' anywhere APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  frame frame-select:private-data = 'firsttime'.  
  RUN enable_UI.
  run initialise.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
 return lv-key + '{&Delim2}' + lv-desc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AltValueChanged ialog-Frame 
PROCEDURE AltValueChanged :
assign pv-id = dec(self:screen-value) 
        lv-search  = ''
        lv-searchfield1 = ''
        lv-searchfield2 = ''
        lv-search:screen-value in frame frame-select = ''.
 run initialise.
 apply 'entry' to lv-search in frame frame-select.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DblClick-Trigger ialog-Frame 
PROCEDURE DblClick-Trigger :
if last-event:row < 2.1
then return no-apply.

apply 'choose' to btn-ok in frame frame-ok.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefineBrowse ialog-Frame 
PROCEDURE DefineBrowse :
def output param h-browse as handle no-undo.

    CREATE BROWSE h-browse ASSIGN
        FRAME = FRAME {&frame-name}:HANDLE
        column = 1
        row = 4
        row-markers = false
        row-height-chars = 1
        HIDDEN = true
        NO-VALIDATE = YES
        WIDTH = 120
        DOWN = 12
        fit-last-column = true
        column-resizable = true 
        allow-column-searching = true
        /* SEPARATORS = yes */
        SENSITIVE = YES
        name = 'lv-search'
        READ-ONLY = true
/*      
 
fit-last-column = true 
*/
        triggers:
                on 'ctrl-d' persistent run extrainfo in this-procedure.
                on "RETURN" persistent run ok-trigger in this-procedure.
                on 'off-end' persistent run OffEndTRigger in this-procedure.
                ON 'START-SEARCH' persistent run sortbrowse in this-procedure.
                on 'off-home' persistent run OffHomeTrigger in this-procedure. 
                on 'end' persistent run gotoend.
                on "MOUSE-SELECT-DBLCLICK" persistent run dblclick-trigger in this-procedure.
                on "MOUSE-MENU-CLICK" persistent run proc-findrow (?).
        end triggers.
       h-browse:move-after-tab-item(frame FRAME-select:handle).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI ialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME FRAME-ok.
  HIDE FRAME FRAME-select.
  HIDE FRAME ialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI ialog-Frame  _DEFAULT-ENABLE
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
  {&OPEN-BROWSERS-IN-QUERY-ialog-Frame}
  DISPLAY lv-search 
      WITH FRAME FRAME-select.
  ENABLE lv-search btn-search 
      WITH FRAME FRAME-select.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-select}
  ENABLE Btn-all btn-ok btn-exit 
      WITH FRAME FRAME-ok.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ok}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraInfo ialog-Frame 
PROCEDURE ExtraInfo :
def var x as int no-undo.
def var v-field as handle no-undo.
def var h-buf   as handle no-undo.
def var h-qry as handle no-undo.
def var lv-key as char no-undo.

 h-browse:select-focused-row().
 h-qry = h-browse:query.
 h-buf = h-qry:Get-Buffer-handle(1).
  
 do x = 1 to h-buf:num-fields:
     v-field = h-buf:Buffer-field(x).
     if v-field:name = t-zen-fldlook.tablename + 'tableid'
        then do: 
            lv-key  = v-field:buffer-value.
            leave.
        end.
 end.
 
 run value(t-zen-fldlook.infoPath + t-zen-fldlook.infopgm)
         (input Table t-zen-fldlook,t-zen-fldlook.zen-fldlooktableid,lv-key,'tableid').

 /* does the same as global trigger 
 on ctrl-d     of frame {&frame-name} anywhere 
 run widget-help in widget-handle(current-window:private-data) 
 ('info','') /*WidgetInfo() */ .

 */
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetData ialog-Frame 
PROCEDURE GetData :
/*-----------------------------------------------------------------------------
  Purpose: Retrieve data for the lookup.
  Notes:   
-----------------------------------------------------------------------------*/
   /* pv-page can be:
       0 = leave the lookup as is
       1 = add a page of data down (next)
      -1 = add a page of data up   (prev)
       ? = get all data */
def input param pv-page as int no-undo.
def input param pv-brid as dec no-undo.
def var lv-by as char no-undo.
   def var lv-proc as char no-undo.
if lv-gotlast and pv-page > 0 then return.

freezewindow(current-window,1).

   lv-page = pv-page.

if valid-handle(h-browse) and 
   self:type = 'browse'  
then h-browse = self.
else run DefineBrowse (output h-browse).

lv-getwhere = if pv-page < 0 or pv-page = ? 
                    then buildtopwhere(lv-topwhere) + lv-getby
                    else lv-where.
if valid-handle(h-dataset) and pv-page = ? 
then h-dataset:empty-dataset().

if pv-page < 0 then lv-getwhere = lv-getwhere + " " + lv-topby.

lv-proc = "FillDataset".
/* message 'page : ' pv-page skip */
/*        'srvwhere : ' lv-getwhere skip */
/*        'topby : ' lv-topby skip */
/*        'getby : ' lv-getby skip */
/*        'where : ' lv-where */
/* {&dbt}. */
/* etime(yes). */
{{&core}run.i &program   = "dynamic.p"
              &path      = "{&core}{&srv}"
              &Appsrv    = "System"
              &procedurec = lv-proc
              &params    = "(lv-page,
                             lv-tables,
                             lv-getwhere,
                             '',  /* dataset linking keyfields not used */
                             lv-returning,
                             t-zen-fldlook.lookupname,
                             OUTPUT DATASET-HANDLE h-dataset append)"}
/* message etime {&dbt}.   */                           
/* numrecords('prodataset',h-dataset:GET-BUFFER-HANDLE(1)). */

/* lv-by = replace(lv-topby,'by sys-cd',''). */
/* lv-by = replace(lv-by,'by practice',''). */
/* h-dataset:GET-BUFFER-HANDLE(1):SYNCHRONIZE(). */
lv-gotlast = h-dataset:GET-BUFFER-HANDLE(1):last-batch.
                                            
run PopulateBrowse(input dataset-handle h-dataset,h-browse,lv-getby,pv-brid).

freezewindow(current-window,0). 

if return-value = 'none' then lv-page = 1.
                                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFields ialog-Frame 
PROCEDURE GetFields :
def input param pv-id as char no-undo.
def var x as int no-undo.
empty temp-table t-zen-lookupfld.
assign lv-searchfield2 = ''
       lv-searchfield1 = ''
       lv-returning = ''
       lv-keyfield = ''.
       
ErrorClear(). /* in zenlibrary.p */
{{&core}run.i &program   = "zen-lookupfld.p"
            &path      = "{&core}{&srv}"
            &Appsrv    = "System"  
            &procedure = "open-query"
            &params    = "(pv-id,
                           output table T-ZEN-LOOKUPFLD)"}
 
if AnyErrors() then do: /* in zenlibrary.p */
    message 'error' view-as alert-box.
    return error.    
end.
for each t-zen-lookupfld by order:
    if not t-zen-lookupfld.LField 
    then delete t-zen-lookupfld.
    else do:
        x = x + 1.
        order = x.
    end.
end.
for each t-zen-lookupfld no-lock by order :
    lv-returning = lv-returning + ',' + t-zen-lookupfld.fieldname .
    if t-zen-lookupfld.keyfield and lv-keyfield = '' 
    then lv-keyfield    = t-zen-lookupfld.fieldname.
    if t-zen-lookupfld.searchfield then do:
      if lv-searchfield1 = '' then do:
        lv-searchfield1 = t-zen-lookupfld.fieldname.
        lv-search:label in frame frame-select = t-zen-lookupfld.FieldLabel.
      end.  
      else if lv-searchfield2 = '' then lv-searchfield2 = t-zen-lookupfld.fieldname.
    end.
    if t-zen-lookupfld.descfield   then lv-descfield   = t-zen-lookupfld.fieldname.
end.
lv-properorder = substring(lv-returning,2).
lv-returning = t-zen-fldlook.tablename + 'tableid' + lv-returning.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetLookupRecord ialog-Frame 
PROCEDURE GetLookupRecord :
def input param  lv-id as dec no-undo.

find t-{&table-name} where {&table-name}tableid = lv-id 
                     no-lock no-error.
if not avail t-{&table-name} then return.
frame {&frame-name}:title = t-zen-fldlook.Window-title.

run setupalt(lv-id).

lv-search:tooltip in frame frame-select = 'The delimiter character is "' + string(lv-delim,"x") + '".'.

End Procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GotoEnd ialog-Frame 
PROCEDURE GotoEnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-last as dec no-undo.
    run Getdata (?,getbrowserecordid(h-browse,'first')).
    lv-last = h-browse:query:num-results - h-browse:num-iterations.
    
    h-browse:query:reposition-to-row(int(lv-last)).
    h-browse:select-row(1).
    h-browse:fetch-selected-row(1).        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialise ialog-Frame 
PROCEDURE Initialise :
assign lv-searchfield1 = ''
       lv-searchfield2 = ''.
       
run getlookuprecord(pv-id).
run getfields(t-zen-fldlook.lookupname).
assign
    lv-tables   = t-zen-fldlook.tablename
    lv-field    = lv-searchfield1                              
    lv-topwhere = t-zen-fldlook.whereclause 
    lv-topby    = t-zen-fldlook.byclause.
    
    setbgcolour(frame {&frame-name}:handle,'lv-search,lv-usealt','{&InputField}').
if num-entries(t-zen-fldlook.tablename) = 1 then 
lv-search = GetStartValue(t-zen-fldlook.tablename,
                          lv-keyfield,
                          pv-startvalue,
                          lv-searchfield1,
                          lv-searchfield2).
                          
disp lv-search with frame frame-select.   
run startsearch('').
apply 'entry' to lv-search in frame frame-select.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OffEndTrigger ialog-Frame 
PROCEDURE OffEndTrigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if not lv-gotlast then

run GetData(1,getbrowserecordid(h-browse,'last')).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OffHomeTrigger ialog-Frame 
PROCEDURE OffHomeTrigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 if not lv-gotlast then
 
    run GetData(-1,getbrowserecordid(h-browse,'first')).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ok-Trigger ialog-Frame 
PROCEDURE Ok-Trigger :
def var x as int no-undo.
 def var v-field as handle no-undo.
 def var h-buf   as handle no-undo.
 def var h-qry as handle no-undo.

 h-browse:select-focused-row().
 h-qry = h-browse:query.
 /* this really needs modding to deal with multiple tables 
    like if the field returned is not on first table in query 
    it will fail. unlikely but possible ! */

 h-buf = h-qry:Get-Buffer-handle(1).
  
 do x = 1 to h-buf:num-fields:
     v-field = h-buf:Buffer-field(x).
     if v-field:name = lv-keyfield  then lv-key  = v-field:buffer-value.
     if v-field:name = lv-descfield then lv-desc = v-field:buffer-value.
 end.
  apply 'GO' to FRAME {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopFullSearchValues ialog-Frame 
PROCEDURE PopFullSearchValues :
def input        param tableName    as char  no-undo.
def input        param searchField1 as char  no-undo.
def input        param searchField2 as char  no-undo.
def input        param initialWhere as char  no-undo.
def input        param indexToUse   as char  no-undo.
def input-output param searchValue1 as char  no-undo.
def input-output param searchValue2 as char  no-undo.

{{&core}run.i &program   = "dynamic.p"
              &path      = "{&core}{&srv}"
              &Appsrv    = "System"
              &procedure = "FindFullSearchValues"
              &params    = "(Tablename,
                            searchfield1,
                            searchfield2,
                            initialwhere,
                            indextouse,
                            input-OUTPUT searchvalue1,
                            input-OUTPUT searchvalue2)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateBrowse ialog-Frame 
PROCEDURE PopulateBrowse :
Def input param DATASET-HANDLE pv-dataset.
def input param h-browse as handle no-undo.
def input param pv-by as char no-undo.
def input param pv-id as dec no-undo.

def var h-qry as handle no-undo.
def var lv-h as handle no-undo. 
def var lv-exclude as char extent 18 no-undo.               
def var h-buffer as handle extent 18 no-undo.
def var x as int no-undo.
def var lv-where as char no-undo.
def var lv-repowhere as char no-undo.
def var lv-label        as char   no-undo.
def var lv-format       as char   no-undo.
def var lv-fname as char no-undo.

lv-where  = 'for each '.
CREATE QUERY h-qry.

do x = 1 to pv-dataset:num-buffers:
    h-buffer[x] = pv-dataset:GET-BUFFER-HANDLE(x).
    lv-exclude[x] = substring(h-buffer[x]:table,3) + 'tableid'.
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
lv-where = lv-where /* + lv-getwhere */ + ' ' + pv-by.
       
h-qry:QUERY-PREPARE(lv-where).
h-qry:QUERY-OPEN().
x = 1.
def var y as int no-undo.
def var h-buf as handle no-undo.
def var h-field as handle no-undo.
def var h-col as handle no-undo.
def var lv-colwidth as int no-undo.
def var lv-width as int no-undo.
y = 1.

h-browse:query = h-qry.   
do y = 1 to pv-dataset:num-buffers:
    /* h-browse:ADD-COLUMNS-FROM(pv-dataset:GET-BUFFER-HANDLE(x),lv-exclude[x]) no-error. */
    h-buf = pv-dataset:GET-BUFFER-HANDLE(y).
    do x = 1 to h-buf:num-fields:
        /* if h-buf:buffer-field(x):name = lv-exclude[y] */
        /*   then next. */
           
        assign h-field = h-buf:Buffer-field(x)
               lv-format = h-field:format
               lv-label = properform(h-field:label).
                  
        Find t-zen-lookupfld where t-zen-lookupfld.fieldname = h-field:name 
                               and t-zen-lookupfld.lfield no-lock no-error.
        if avail t-zen-lookupfld 
        then do:  
            assign lv-format = if t-zen-lookupfld.FieldFormat = '' 
                                then h-field:format
                                else t-zen-lookupfld.FieldFormat
                   lv-label = if t-zen-lookupfld.FieldLabel = ''
                                then  h-field:label
                                else t-zen-lookupfld.FieldLabel
                   lv-fname = if extent > 0 
                                then h-buf:name + '.' + h-field:name  + '[' + string(extent) + ']' 
                                else h-buf:name + '.' + h-field:name
                   h-field:format = lv-format
                   h-field:label = lv-label
                   h-col = h-browse:ADD-LIKE-column(lv-fname)
                   lv-colwidth = h-field:width-chars
                   h-col:width-chars = max(font-table:get-text-width-chars(h-col:label,h-browse:font),
                                           font-table:get-text-width-chars(string(h-field),h-browse:font))
                   h-col:width-chars = max(h-col:width-chars,lv-colwidth)
                   lv-width = lv-width + h-col:width + 1
                   h-col:read-only = true
                   h-col:resizable = true
                   h-col:width-chars = h-col:width-chars + 1
                   lv-width = lv-width + 1 no-error.
            if h-col:name = lv-searchfield1
                then assign
                        h-browse:current-column = h-col
                        lv-search:private-data in frame frame-select = string(h-col) + '|' + h-col:data-type
                        lv-search = trim(string(h-field:buffer-value,h-col:format))
                        h-col:LABEL-BGCOLOR = 15 no-error.
 /*                else if GetLookupInfo(h-col,'browse' + t-{&table-name}.lookupname) ne ? */
/*                      then h-col:LABEL-BGCOLOR = 8. */
            if lv-searchfield2 ne '' 
            then if h-col:name = lv-searchfield2
                    then lv-search = lv-search + lv-delim + trim(string(h-field:buffer-value,h-col:format)) no-error.   
            if h-col:data-type = 'date' 
            then assign
                   h-col:width-chars = h-col:width-chars + 1
                   lv-width = lv-width + 1.
        end.
    end. 
end.

disp lv-search with frame frame-select.
lv-width = lv-width + 2.
if lv-width > 200 then lv-width = 200.
h-browse:width = lv-width.
if lv-width >= frame {&frame-name}:width - 3
    then frame {&frame-name}:width = max(90,lv-width + 3).
    
assign
    lv-width = max(83,h-browse:width + 3)
    frame frame-ok:col = max(1,(lv-width - frame frame-ok:width) / 2)
    frame frame-select:col = max(1,(lv-width - frame frame-select:width) / 2) 
    frame {&frame-name}:width = max(90,lv-width)  
    h-browse:col = max(1,(frame frame-ok:width-chars - h-browse:width-chars) / 2)
no-error.

 
if pv-id ne ?
then do:
    lv-repowhere = 'where ' + lv-exclude[1] + ' = ' + string(pv-id).
    h-buffer[1]:find-first(lv-repowhere) no-error.
    h-qry:reposition-to-ROWID(h-buffer[1]:rowid) no-error.
    if lv-page < 0 then do:
        h-qry:get-prev() no-error.
        h-qry:reposition-to-ROWID(h-buffer[1]:rowid) no-error.      
    end.
end.
else h-browse:select-row(1) no-error.
/* else if h-browse:private-data = '' */
/*         then h-buffer[1]:find-last(). */
/*         else h-buffer[1]:find-first(). */
/* h-qry:reposition-to-ROWID(h-buffer[1]:rowid). */

ReorderBrowseColumns(h-browse,lv-properorder).

h-browse:hidden = false.
frame {&frame-name}:hidden = false.
apply 'entry' to h-browse.
h-browse:select-focused-row() no-error.

if h-qry:num-results = 0 
then do:
    message 'No Records Found' view-as alert-box information.
    if frame frame-select:private-data = 'firsttime'
    then apply 'choose' to btn-exit in frame frame-ok.
    else return 'none'.
end.
frame frame-select:private-data = ''. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-FindRow ialog-Frame 
PROCEDURE Proc-FindRow :
def input param lv-value  as char   no-undo format 'x(15)'.

def var v-hqry    as handle no-undo.
def var v-hcolumn as handle no-undo.
def var lv-qry    as char   no-undo.
def var h-db      as handle no-undo.
Def Var h-qry     As Handle No-undo.
def var h-qry2    as handle no-undo.
def var x as int no-undo.
def var lv-searchvalue1 as char no-undo.
def var lv-searchvalue2 as char no-undo.
def var lv-eqexpr as char init ' >= '.

v-hcolumn = h-browse:CURRENT-COLUMN.
if v-hcolumn = ? 
then do:
    v-hcolumn = h-browse:first-COLUMN.
    DO WHILE VALID-HANDLE(v-hcolumn):
        if v-hcolumn:LABEL-BGCOLOR = 15 then leave.
        v-hcolumn = v-hcolumn:NEXT-COLUMN no-error.
    END.
    if v-hcolumn = ? 
    then do:
      v-hcolumn = widget-handle(entry(1,lv-search:private-data in frame frame-select,'|')).
      if not valid-handle(v-hcolumn) then return 'failed'.
    end.
end.

/* i know i know only one active wait-for 
but update is quiclkest and easiest method */
if lv-value = ? then
update lv-value label "Search For" at 6 skip
  " Enter the search value or press Esc to cancel." at 5
   go-on (tab)
   with frame upd side-labels three-d
   view-as dialog-box  title "Quick Search".
if not ValidSearchValue(input-output lv-value,v-hcolumn:data-type)
then return 'failed'.
  
if Valid-handle(h-QrY2) then h-QrY2:QUERY-CLOSE().
if Valid-handle(h-QrY2) then delete object h-QrY2 no-error.
h-qry = h-browse:query.
if h-qry:num-buffers > 1 then return.
create buffer h-db for table h-qry:get-buffer-handle(v-hcolumn:table).
Create Query h-QrY2.
h-QrY2:Add-buffer(h-db).
if num-entries(lv-value,lv-delim) > 1 and 
   lv-searchfield1 = v-hcolumn:name then do:
    lv-where = ' where '.
    lv-searchvalue1 = entry(1,lv-value,lv-delim).
    do x = 2 to num-entries(lv-value,lv-delim):
        lv-searchvalue2 = lv-searchvalue2 + entry(x,lv-value,lv-delim) + ' '.
    end.
    lv-searchvalue2 = trim(lv-searchvalue2).
    lv-getby = ''.
    if lv-searchField1 ne ''
    then do:
        lv-where = lv-where + lv-searchField1 + lv-eqexpr + '"' + lv-searchvalue1 + '"'.
        lv-getby = ' by ' + lv-searchField1. 
    end.
    if lv-searchField2 ne '' 
    then do:
       lv-where = lv-where +
                   ' and if ' + lv-searchField1 + ' = "' + lv-searchvalue1 + '"' +
                   ' then ' + lv-searchField2 + lv-eqexpr + '"' + lv-searchvalue2 + '"' +
                   ' else ' + lv-searchField2 + ' >= ""'.
        lv-getby = lv-getby + ' by ' + lv-searchField2.
    end.
lv-qry = "For EACH " + h-db:name +
             lv-where + " no-lock" + lv-getby.
end.
else lv-qry = "For EACH " + h-db:name +
         " where string(" + v-hcolumn:name + ") begins '" + lv-value + "'" +
         " no-lock" + " by " + v-hcolumn:name.

h-QRY2:QUERY-PREPARE(lv-qry).

h-QRY2:QUERY-OPEN.

h-qry2:get-first.
if h-db:available
 then h-qry:reposition-to-rowid(h-db:rowid). 
else do:
/*     message 'Nothing was found beginning with "' + lv-value + '".'. */
    lv-qry = "For EACH " + h-db:name +
             " where string(" + v-hcolumn:name + ") >= '" + lv-value + "'" +
             " no-lock" + " by " + v-hcolumn:name.
    h-QRY2:QUERY-PREPARE(lv-qry).
    h-QRY2:QUERY-OPEN.
    h-qry2:get-first.
    if h-db:available
     then h-qry:reposition-to-rowid(h-db:rowid). 
    else do:
        message 'Nothing was found greater than or equal to "' + 
                lv-value + '".' skip 
               'Try again after pressing "Get All" (unless you already pressed it).'
        view-as alert-box information.
        return 'failed'.
    end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetUpAlt ialog-Frame 
PROCEDURE SetUpAlt :
def input param  lv-id as dec no-undo.
def buffer b-tab for t-{&table-name}.
def var lv-lab as char no-undo.
def var lv-alt as char no-undo.
def var lv-buts as char no-undo.
def var lv-usealt as handle no-undo.
def var lv-f as handle no-undo.
def var lv-orig as dec no-undo.
def var x as int no-undo.

if lv-acctdefid = '' 
then lv-acctdefid = string(lv-id).

lv-f = frame frame-select:handle.

if valid-handle(getwidhandle(lv-f,'rs-alt')) then return.

if lv-id ne t-{&table-name}.{&table-name}tableid 
then do:
    lv-orig = t-{&table-name}.{&table-name}tableid.
    find t-{&table-name} where t-{&table-name}.{&table-name}tableid = lv-id 
                         no-lock no-error.
    if not avail t-{&table-name} 
    then do:
        find t-{&table-name} where t-{&table-name}.{&table-name}tableid = lv-orig
                         no-lock no-error.
        return.
    end.
end.

if t-{&table-name}.altlookupname ne '' 
then do:
    do x = 1 to num-entries(t-{&table-name}.altlookupname,'|'):
        lv-alt = entry(x,t-{&table-name}.altlookupname,'|').
        lv-lab = entry(x,t-{&table-name}.altlookuplabel,'|').
        find b-tab where b-tab.lookupname = lv-alt
                   no-lock NO-ERROR.
        if not avail b-tab then do:
            message 'Invalid Alt Lookup ' lv-alt skip
                    'Please inform Tech Surport'
            view-as alert-box error.
            next.
        end.                   
        lv-buts = lv-buts  + ',' + 
                  lv-lab + ',' + string(b-tab.{&table-name}tableid).
    end.               
    lv-buts = 'Accounts,' + string(lv-id) + lv-buts.

    create radio-set lv-usealt           
    assign frame        = lv-f
           horizontal = true
           name = 'rs-alt'
           column = 1
           row = 2.91
           sensitive    = true
           radio-buttons = lv-buts 
           tab-stop = false
           visible      = true
    triggers:
        on value-changed persistent run AltValueChanged in this-procedure.
    end triggers.
    lv-usealt:column = (lv-f:width-chars - lv-usealt:width-chars) / 2.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortBrowse ialog-Frame 
PROCEDURE SortBrowse PRIVATE :
def var h-col as handle no-undo.
def var lv-by as char no-undo.
def var v-hcolumn as handle no-undo.
def var lv-id as int no-undo.
def var x as int no-undo.
def var lv-colnum as int no-undo.
def var h as handle no-undo.

assign  h-col     = h-browse:CURRENT-COLUMN 
        lv-by = 'by ' + h-col:name 
        lv-field = h-col:name.

if h-col:buffer-field:extent > 0 then do:
    message 'Cannot sort on array elements.' view-as alert-box information.
    return.
end.     

if h-browse:private-data = '' 
then assign lv-by          = lv-by + ' descending'
            h-browse:private-data = 'descending'.
else h-browse:private-data = ''.


lv-id = int(GetLookupInfo(h-col,'browse' + t-{&table-name}.lookupname)).
if lv-id = ? and
   lv-page ne ? then do:
    message 
       'No special lookup has been defined for' h-col:label  
       '(' + h-col:table + '.' + h-col:name + ').' skip 
       'Press the "Get All" button first to enable sorting on this column.'.
    run getlookuprecord(pv-id).
    lv-id = pv-id.
end.
if pv-id ne lv-id then run SetUpAlt(lv-id).

if lv-page ne ? then do:
    if pv-id ne lv-id then do:
        run getlookuprecord(lv-id).
        if not avail t-{&table-name} then do:
            message 
               'No special lookup has been defined for' h-col:label  
               '(' + h-col:table + '.' + h-col:name + ').' skip 
               'Press the "Get All" button first to enable sorting on this column.'.
            run getlookuprecord(pv-id).
            lv-id = pv-id.
        end.
        
        if t-zen-fldlook.tablename = 'acct' /**************************************/
        then getwidhandle(frame frame-select:handle,'rs-alt'):screen-value = lv-acctdefid.

        pv-id = lv-id.
        run getfields(t-zen-fldlook.lookupname).
                                   
      assign
          lv-tables   = t-zen-fldlook.tablename
          lv-field    = lv-searchfield1                              
          lv-topwhere = t-zen-fldlook.whereclause 
          lv-topby    = t-zen-fldlook.byclause.
          
      
      if h-col:data-type = 'date' then lv-search:screen-value in frame frame-select = '01/01/0001'.
      if h-col:data-type = 'integer' or 
         h-col:data-type = 'decimal' then lv-search:screen-value = '0'.
      if h-col:data-type = 'character' then lv-search:screen-value = ''.
      lv-search:private-data = h-col:data-type.
      run startsearch('').
/*
      if t-zen-fldlook.tablename = 'acct'                      
        then run startsearch('').
        else run startsearch('all').
*/        
    end.
    else do:
        run PopulateBrowse(input dataset-handle h-dataset,h-browse,lv-by,?). 
        h-col = h-browse:CURRENT-COLUMN.
    end.
end.
else do:
    run PopulateBrowse(input dataset-handle h-dataset,h-browse,lv-by,?).
    h-col     = h-browse:CURRENT-COLUMN.
    frame {&frame-name}:title = entry(1,frame {&frame-name}:title,' ') + ' By ' + h-col:label.
    lv-search:label = substring(h-col:label,1,19).
end.

    x = 1.
    DO x = 1 to h-browse:num-columns:
      ASSIGN v-hcolumn = h-browse:get-browse-column(x)  
             v-hcolumn:LABEL-BGCOLOR = 8.
      if not valid-handle(h-col)
        then if v-hcolumn:name = lv-searchfield1 then h-col = v-hcolumn.
      if v-hcolumn = h-col then lv-colnum = x.
    END.
    h-col:LABEL-BGCOLOR = 15.
    
 /*    if h-col:data-type = 'date' then lv-search:screen-value = '01/01/0001'. */
/*     if h-col:data-type = 'integer' or */
/*        h-col:data-type = 'decimal' then lv-search:screen-value = '0'. */
/*     if h-col:data-type = 'character' then lv-search:screen-value = ''. */
/*     lv-search:private-data = h-col:data-type. */

    h-browse:CLEAR-SORT-ARROWS().
    h-browse:set-sort-arrow(lv-colnum,(h-browse:private-data ne '')).


apply 'entry' to lv-search.
return no-apply.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSearch ialog-Frame 
PROCEDURE StartSearch :
def input param pv-mode as char no-undo.
def var x as int no-undo.
def var lv-rep-pos as char no-undo.
def var lv-rep-value as char no-undo.
def var lv-eqexpr as char init ' >= '.
def var lv-searchvalue1 as char no-undo.
def var lv-searchvalue2 as char no-undo.
def var lv-stype as char no-undo.
def var lv-idx as char no-undo.

/* remove rubish values from field like commas */
if num-entries(lv-search:screen-value in frame frame-select) > 1 
then lv-search:screen-value = entry(1,lv-search:screen-value).

    assign frame frame-select
         lv-search 
         lv-page  = 0
         lv-browseby = lv-topby.
if lv-search = ? then lv-search = ''.

   lv-stype = if num-entries(lv-search:private-data,'|') > 1 
                then entry(2,lv-search:private-data,'|')
                else lv-search:private-data.
                
   if not ValidSearchValue(input-output lv-search,lv-stype)
   then return 'failed'. 
                
lv-where = BuildTopWhere(lv-topwhere).

if lv-searchfield2 ne '' and
   num-entries(lv-search,lv-delim) > 1 
then do:
    lv-searchvalue1 = entry(1,lv-search,lv-delim).
    do x = 2 to num-entries(lv-search,lv-delim):
        lv-searchvalue2 = lv-searchvalue2 + entry(x,lv-search,lv-delim) + ' '.
    end.
    lv-searchvalue2 = trim(lv-searchvalue2).
    if error-status:error then do:
        ErrorCreate(50,'Searchvalue2','Invalid Substring',' ',' ').
    end.
    /****** eriks new bit *******/
    lv-idx = GetNamedValue("InitIdx",t-zen-fldlook.extra-details).
    if lv-idx = '' then lv-idx = "default".
    run popFullSearchValues in this-procedure(
       input        t-zen-fldlook.tablename,
       input        lv-searchfield1,
       input        lv-searchfield2,
       input        lv-where,
       input        lv-idx,
                            input-output lv-searchvalue1,
                            input-output lv-searchvalue2).
    /*********************/
end.
else do:
    lv-searchvalue1 = lv-search.
end.
lv-getby = ''.
if lv-searchField1 ne ''
then do:
    lv-where = lv-where + ' and ' + lv-searchField1 + lv-eqexpr + '"' + lv-searchvalue1 + '"'.
    lv-getby = ' by ' + lv-searchField1. 
end.
if lv-searchField2 ne '' 
then do:
   lv-where = lv-where +
               ' and if ' + lv-searchField1 + ' = "' + lv-searchvalue1 + '"' +
               ' then ' + lv-searchField2 + lv-eqexpr + '"' + lv-searchvalue2 + '"' +
               ' else ' + lv-searchField2 + ' >= ""'.
    /* " and (" + lv-searchField1 + " > " + quoter(lv-searchvalue1) + " or " +
             lv-searchField2 + lv-eqexpr + quoter(lv-searchvalue2) + ")". */

    lv-getby = lv-getby + ' by ' + lv-searchField2.
end.

lv-where = lv-where + ' ' + lv-topby + lv-getby. 

if valid-handle(h-dataset) 
then do:
    h-dataset:empty-dataset().
    h-dataset = ?.
end.
delete object h-browse no-error.
lv-getwhere = lv-where.
if pv-mode ne 'all'
    then run GetData(0,?).
    else run GetData(?,?).
/* btn-all:label in frame frame-ok = if lv-page = ? then 'Refresh' else 'Get All'. */

btn-all:hidden in frame frame-ok = pv-mode = 'all'.


apply 'entry' to h-browse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BuildTopWhere ialog-Frame 
FUNCTION BuildTopWhere RETURNS CHARACTER
  ( pv-where as char ) :
 def var x as int no-undo.
 def var lv-rep-pos as char no-undo.
 def var lv-rep-value as char no-undo.
    
 if pv-where <> "" then
 do x = 1 to num-entries(t-zen-fldlook.wherefield):
     assign lv-rep-pos = "#" + string(x)
            lv-rep-value = entry(x,t-zen-fldlook.wherefield).
            lv-rep-value = "'" + dynamic-function(entry(1,lv-rep-value,'{&Delim2}'),
                                                  entry(2,lv-rep-value,'{&Delim2}')) + "'".
            pv-where     = replace(pv-where,lv-rep-pos,LV-REP-VALUE).
 end.

  RETURN pv-where.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetBrowseRecordId ialog-Frame 
FUNCTION GetBrowseRecordId RETURNS DECIMAL
  ( pv-browse as handle,
    pv-mode as char ) :

    if not valid-handle(pv-browse) then return 1.

    def var lv-id as dec no-undo.
    def var h-buf as handle no-undo.
    def var lv-currow as int no-undo init 1.

    h-buf  = pv-browse:query:get-buffer-handle(1).
    case pv-mode:
        when 'last'  then lv-currow = pv-browse:query:num-results.
        when 'first' then lv-currow = 1.
        otherwise lv-currow = 1.
    end case.

    pv-browse:query:reposition-to-row(lv-currow).
    pv-browse:select-row(1) no-error.
    
    pv-browse:fetch-selected-row(1) no-error.   
    if h-buf:available 
    then lv-id = int(getfieldvalue(h-buf,substring(h-buf:table,3) + 'tableid',0)).
    else lv-id = 1.
    
  RETURN lv-id.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldHandle ialog-Frame 
FUNCTION GetFieldHandle RETURNS HANDLE
  ( pv-buffer as handle,
    pv-fname as char ) :
    Def Var X       As Int    no-undo.
    def var h-field as handle no-undo.

    do x = 1 to pv-buffer:num-fields:
        h-field = pv-buffer:Buffer-field(X).
        If h-field:name = pv-fname Then RETURN h-field.
    end.
    RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetStartValue ialog-Frame 
FUNCTION GetStartValue RETURNS CHARACTER
  ( pv-table as char ,
    pv-field as char,
    pv-value as char,
    pv-s1 as char,
    pv-s2 as char ) :
  
 def var lv-value as char no-undo.
 def var lv-where as char no-undo.        
 def var lv-dtype as char no-undo.
 
    lv-where = BuildTopWhere(replace(lv-topwhere,'where','')) +
               ' and ' + pv-field + ' = "' + pv-value + '"'.

    {{&core}run.i &program   = "dynamic.p"
                  &path      = "{&core}{&srv}"
                  &Appsrv    = "System"
                  &procedure = "FindStartRecord"
                  &params    = "(pv-table,lv-where,pv-s1,pv-s2,output lv-value,output lv-dtype)"}
    lv-search:private-data in frame frame-select = lv-dtype.                     
  RETURN lv-value.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReorderBrowseColumns ialog-Frame 
FUNCTION ReorderBrowseColumns RETURNS CHARACTER
  ( pv-browse as handle,
    pv-fieldorder as char ) :
     
def var h-col as handle no-undo.    
def var lv-currentorder as char no-undo.
def var lv-from as int no-undo.
def var lv-to as int no-undo.
def var x as int no-undo.
def var y as int no-undo.

freezewindow(current-window,1).
do x = num-entries(pv-fieldorder) to 1 by -1:
    lv-currentorder = ''.
    h-col = pv-browse:first-column.
    do while valid-handle(h-col):
        lv-currentorder = lv-currentorder + ',' + h-col:name.
        h-col = h-col:next-column.
    end.
    lv-currentorder = substring(lv-currentorder,2).
       
    assign
        lv-from = lookup(entry(x,pv-fieldorder),lv-currentorder)
        lv-to = 1.
    h-browse:move-column(lv-from,lv-to).
end.

/* do x = 1 to pv-browse:num-columns: */
/*     lv-currentorder = ''. */
/*     h-col = pv-browse:first-column. */
/*     do while valid-handle(h-col): */
/*         lv-currentorder = lv-currentorder + ',' + h-col:name. */
/*         h-col = h-col:next-column. */
/*     end. */
/*     lv-currentorder = substring(lv-currentorder,2). */
/*    */
/*     h-col = pv-browse:first-column. */
/*     inner-loop: */
/*     do while valid-handle(h-col): */
/*         assign */
/*             lv-from = lookup(h-col:name,lv-currentorder) */
/*             lv-to = lookup(h-col:name,lv-properorder). */
/*         if lv-from ne lv-to */
/*         then do: */
/*             message h-col:name skip */
/*             pv-fieldorder skip */
/*             lv-currentorder skip lv-from lv-to */
/*        {&dbt}. */
/*    */
/*             h-browse:move-column(lv-from,lv-to). */
/*             leave inner-loop. */
/*         end. */
/*         h-col = h-col:next-column. */
/*     end. */
/* end. */

pv-browse:CLEAR-SORT-ARROWS(). 
do x = 1 to pv-browse:num-columns:
    if pv-browse:GET-BROWSE-COLUMN(x):name = lv-searchfield1
    then do:
        pv-browse:set-sort-arrow(x,true).
        leave.
    end.
end.
freezewindow(current-window,0).
  RETURN "".   /* Function return value. */
  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidSearchValue ialog-Frame 
FUNCTION ValidSearchValue returns logical
  (input-output pv-search as char ,
   pv-type as char ) :
  
  def var lv-ok as log no-undo init true.
  def var lv-date as date no-undo.

   case pv-type:
      when 'date' then do:
         if pv-search = '' then pv-search = '01/01/0001'.
         if not validdate(pv-search) then do:
            message 'Invalid Date Value must be one of' skip
                    'dd/mm/yy dd/mm/yyyy ddmmyy ddmmyyyy'
             view-as alert-box error.
            lv-ok = false.
         end.
         lv-date = stringtodate(pv-search).
         pv-search = string(lv-date).
      end.
      when 'integer' or 
      when 'decimal' then do:
         if not ISnumeric(pv-search) then do:
            message 'Invalid Numeric Value' view-as alert-box error.
            lv-ok = false.
         end.
      end.
      otherwise do:
          pv-search = replace(pv-search,'"','').
      end.
   end case.
   
  return lv-ok.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

