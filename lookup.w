&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME frame-lookup


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-fldlook NO-UNDO LIKE zen-fldlook.
DEFINE TEMP-TABLE t-zen-lookupfld NO-UNDO LIKE zen-lookupfld.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS frame-lookup 
/******************************************************************************/
/*  PROGRAM ID.     : ????????                                                */
/*  PROGRAM TITLE   : ????????                                                */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    : East India Trading LTD                                    */
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
CREATE WIDGET-POOL.
/* &glob bug * */
/* ***************************  Definitions  ************************** */
{app-paths.i}


&glob Table-name zen-fldlook
&glob unique-key    {&table-name}TableId
&glob about
Def Input Param Table For t-{&table-name}.
def input param pv-id         as dec  no-undo.
Def Input Param pv-startvalue As Char No-undo.

def var lv-multi as log no-undo init true.
def var v-numrows       as Int      no-undo.
def var lv-select       as char     no-undo.
def var lv-moretocome   as log      no-undo init true.
def var lv-keyfield     as char     no-undo.
def var lv-searchfield1  as char     no-undo.
def var lv-searchfield2  as char     no-undo.
def var lv-descfield    as char     no-undo.
def var h-data          as handle   no-undo.
def var h-qry           as handle   no-undo.
def var h-col as handle no-undo.
def var h-br            as handle   no-undo.
def var h-buf           as handle   no-undo.
def var lv-lastid       as char     no-undo init ?.
def var h-scol          as handle   no-undo.
def var lv-where        as char     no-undo.
def var lv-by           as char     no-undo.
def var lv-insort as log no-undo.
def var lv-dblclicked as log no-undo.
def var lv-delim as char no-undo.
def var lv-key  as char no-undo.
 def var lv-desc as char no-undo.

lv-where =  " sys-cd = '" + getsysvar('{&clv}gs-sys-cd') + /* in zenlibrary.p */
                  "' and practice = '" + getsysvar('{&clv}practice') + "'".
                           /* in zenlibrary.p */
lv-delim = ' '.
lv-delim = getfieldwhere('sysopt',lv-where,'aoptn[10]').
lv-where = ' '.

if asc(lv-delim) = -1 then lv-delim = ' '.
pv-startvalue = replace(pv-startvalue,"'","").
pv-startvalue = replace(pv-startvalue,"~~","").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frame-lookup

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 n lv-search 
&Scoped-define List-2 Btn-OK RECT-3 Btn_Cancel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GotAll frame-lookup 
FUNCTION GotAll RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MultiField frame-lookup 
FUNCTION MultiField RETURNS LOGICAL
  ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-getall  NO-FOCUS
     LABEL "Get All" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn-OK AUTO-GO  NO-FOCUS
     LABEL "OK" 
     SIZE 8.4 BY 1 TOOLTIP "Ok"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancel AUTO-END-KEY  NO-FOCUS
     LABEL "Cancel" 
     SIZE 10 BY 1.05 TOOLTIP "Exit"
     BGCOLOR 8 .

DEFINE VARIABLE lv-more AS CHARACTER FORMAT "X(256)":U INITIAL "More Data To Come" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     FGCOLOR 12  NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 1.76.

DEFINE BUTTON btn-search  NO-FOCUS
     LABEL "Find" 
     SIZE 11 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 38.2 BY 1 NO-UNDO.

DEFINE RECTANGLE n
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 3.33.

DEFINE VARIABLE lv-usealt AS LOGICAL INITIAL no 
     LABEL "Use Alternate" 
     VIEW-AS TOGGLE-BOX
     SIZE 73 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-lookup
     SPACE(82.00) SKIP(19.57)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE " Lookup ".

DEFINE FRAME FRAME-select
     btn-search AT ROW 2.05 COL 63
     lv-search AT ROW 2.14 COL 21.8 COLON-ALIGNED
     lv-usealt AT ROW 3.48 COL 4
     "Select Records" VIEW-AS TEXT
          SIZE 17.8 BY .67 AT ROW 1.48 COL 3.8
     n AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79 BY 3.57.

DEFINE FRAME FRAME-ok
     Btn-OK AT ROW 2.1 COL 49.8
     lv-more AT ROW 2.1 COL 19 COLON-ALIGNED NO-LABEL
     btn-getall AT ROW 2 COL 5
     Btn_Cancel AT ROW 2.05 COL 60.8
     RECT-3 AT ROW 1.71 COL 47.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 17.57
         SIZE 79 BY 2.62
         DEFAULT-BUTTON Btn-OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-fldlook T "?" NO-UNDO sigmstr zen-fldlook
      TABLE: t-zen-lookupfld T "?" NO-UNDO schadm zen-lookupfld
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ok:FRAME = FRAME frame-lookup:HANDLE
       FRAME FRAME-select:FRAME = FRAME frame-lookup:HANDLE.

/* SETTINGS FOR DIALOG-BOX frame-lookup
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME frame-lookup:SCROLLABLE       = FALSE
       FRAME frame-lookup:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-ok
                                                                        */
/* SETTINGS FOR BUTTON Btn-OK IN FRAME FRAME-ok
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Cancel IN FRAME FRAME-ok
   2                                                                    */
/* SETTINGS FOR FILL-IN lv-more IN FRAME FRAME-ok
   NO-ENABLE                                                            */
ASSIGN 
       lv-more:READ-ONLY IN FRAME FRAME-ok        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME FRAME-ok
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FRAME FRAME-select
                                                                        */
/* SETTINGS FOR FILL-IN lv-search IN FRAME FRAME-select
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-usealt IN FRAME FRAME-select
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-usealt:HIDDEN IN FRAME FRAME-select           = TRUE.

/* SETTINGS FOR RECTANGLE n IN FRAME FRAME-select
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX frame-lookup
/* Query rebuild information for DIALOG-BOX frame-lookup
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX frame-lookup */
&ANALYZE-RESUME

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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define FRAME-NAME FRAME-ok
&Scoped-define SELF-NAME btn-getall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-getall frame-lookup
ON CHOOSE OF btn-getall IN FRAME FRAME-ok /* Get All */
DO:
{{&core}bug.i}
    assign 
        lv-search:screen-value in frame frame-select = ''
        lv-search  
        lv-lastid = '-1'.
    
    if lv-search = '' and valid-handle(h-col)
    then do:
       if h-col:data-type = 'date' then lv-search = '01/01/0001'.
       if h-col:data-type = 'integer' or 
          h-col:data-type = 'decimal' then lv-search = '0'.
    end.
    
        run createbrowse.
        h-buf:empty-temp-table no-error.
        Run get-data (lv-search,'down').
        
    if lv-searchfield1 ne '' 
        then lv-by = ' by ' + lv-searchfield1.
        else lv-by = ''.

   run dobrowse(t-zen-fldlook.tablename,'',lv-by,'down',yes).
 lv-moretocome = no.
 lv-more:hidden in frame frame-ok = not lv-moretocome.
btn-getall:hidden in frame frame-ok = not lv-moretocome.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-OK frame-lookup
ON CHOOSE OF Btn-OK IN FRAME FRAME-ok /* OK */
DO:
{{&core}bug.i}
 def var x as int no-undo.
 def var v-field as handle no-undo.
 def var h-buf   as handle no-undo.
 
 h-br:select-focused-row().
 h-buf = h-data:default-buffer-handle.

 do x = 1 to h-buf:num-fields:  
     v-field = h-buf:Buffer-field(x).
     if v-field:name = lv-keyfield  then lv-key  = v-field:buffer-value.
     if v-field:name = lv-descfield then lv-desc = v-field:buffer-value.
 end.
apply 'GO' to FRAME {&frame-name}.
/*  return lv-key + '{&Delim2}' + lv-desc. */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-select
&Scoped-define SELF-NAME btn-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-search frame-lookup
ON CHOOSE OF btn-search IN FRAME FRAME-select /* Find */
DO:
  {{&core}bug.i}
   run search-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ok
&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel frame-lookup
ON CHOOSE OF Btn_Cancel IN FRAME FRAME-ok /* Cancel */
DO:
{{&core}bug.i}
return pv-startvalue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-select
&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search frame-lookup
ON LEAVE OF lv-search IN FRAME FRAME-select /* Search */
DO:
  {{&core}bug.i}
    if keyfunction(lastkey) ne 'return' then do:
        lv-select = self:screen-value.
        if valid-handle(Last-event:widget-enter)
        then do:
            if not can-do('h-br,btn_cancel,btn-getall,btn-ok',Last-event:Widget-enter:name) 
            then do:
                run search-trigger.
            end.
/*             else return no-apply. */
        end.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search frame-lookup
ON RETURN OF lv-search IN FRAME FRAME-select /* Search */
DO:
  {{&core}bug.i}
  lv-select = self:screen-value.
  run search-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search frame-lookup
ON TAB OF lv-search IN FRAME FRAME-select /* Search */
DO:
 /*    {{&core}bug.i} */
/*     lv-select = self:screen-value. */
/*     if not donotfire('btn_cancel,btn-getall,btn-ok') */
/*     then run search-trigger. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-usealt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-usealt frame-lookup
ON VALUE-CHANGED OF lv-usealt IN FRAME FRAME-select /* Use Alternate */
DO:
{{&core}bug.i}
 if self:checked then pv-id = dec(entry(2,self:private-data)).
                 else pv-id = dec(entry(1,self:private-data)).

 run startlookup.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frame-lookup
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK frame-lookup 


/* ***************************  Main Block  *************************** */
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.
/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

on 'f4' anywhere APPLY "END-ERROR":U TO SELF.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    {{&core}sec-chk.i}
    
    RUN enable_UI.

    freezewindow(FRAME {&FRAME-NAME}:handle,1).
    Run initialise.
    freezewindow(FRAME {&FRAME-NAME}:handle,0).
    
    {{&core}wid-chk.i}
    
    apply 'entry' to lv-search.
      
    WAIT-FOR GO OF FRAME {&frame-name}. 
END.
RUN disable_UI.
 return lv-key + '{&Delim2}' + lv-desc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateBrowse frame-lookup 
PROCEDURE CreateBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
if valid-handle(h-br)  then delete widget h-br.
if valid-handle(h-qry) then delete widget h-qry.

if not valid-handle(h-br) then
create browse h-br
    ASSIGN FRAME = frame {&frame-name}:HANDLE
           /* QUERY = h-qry  */
           row-markers = false
           row = 5
           row-height-chars = 1
           name = 'h-br'
           column = 1
           WIDTH = 3
           DOWN = 12
           visible = true
           SENSITIVE = TRUE
           READ-ONLY = true
           fit-last-column = true
        triggers:
            ON end,off-end persistent run off-endTrigger ('down').
            ON home,off-home persistent run off-endTrigger ('up').
            ON 'START-SEARCH' persistent run sortbrowse.
            on "MOUSE-MENU-CLICK":U persistent run proc-findrow (?).
            ON 'mouse-select-dblclick' persistent run dblclick-trigger.
        end triggers. 
        h-br:move-after-tab-item(frame FRAME-select:handle).
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dblclick-trigger frame-lookup 
PROCEDURE dblclick-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}

if last-event:row < 2.62
then return no-apply.

lv-dblclicked = yes.

apply 'choose' to btn-ok in frame frame-ok.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI frame-lookup  _DEFAULT-DISABLE
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
  HIDE FRAME frame-lookup.
  HIDE FRAME FRAME-ok.
  HIDE FRAME FRAME-select.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoBrowse frame-lookup 
PROCEDURE DoBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
def input param pv-table as char no-undo.
def input param pv-where as char no-undo.
def input param pv-by    as char no-undo.
def input param pv-direction as char no-undo.
def input param pv-new as log no-undo.
pv-table = entry(1,pv-table,'{&delim2}').

def var h-col       as handle no-undo.
def var x           as int    no-undo.
def var lv-cname    as char   no-undo.
def var lv-colwidth as dec    no-undo.
def var lv-width    as dec    no-undo.
def var lv-lastpos  as rowid  no-undo.
if valid-handle(h-scol)
    then lv-cname = h-scol:name.
    else lv-cname = lv-searchfield1.

h-buf = h-data:default-buffer-handle no-error.
if lv-lastid ne ? and valid-handle(h-buf)
then lv-lastpos = h-buf:rowid. 

if pv-new then do:
    If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
    
    create query h-qry.
    h-QrY:Add-buffer(h-buf).
    h-qry:query-prepare('preselect each t-' + pv-table + 
                        pv-where + ' no-lock ' + pv-by).                
    h-qry:query-open.
    h-qry:get-first() no-error.
    if h-qry:num-results = 0 then do:
         h-qry:query-prepare('preselect each t-' + pv-table + ' no-lock ' + pv-by).
         h-qry:query-open.
         h-qry:get-first() no-error.
    end.

    btn-ok:sensitive in frame frame-ok = not h-qry:query-off-end.

    h-br:query = h-qry.
    if h-qry:query-off-end and valid-handle(h-scol) 
    then do:
          if h-scol:data-type = 'date' then lv-search = '01/01/0001'.
          if h-scol:data-type = 'integer' or 
             h-scol:data-type = 'decimal' then lv-search = '0'.
          if h-scol:data-type = 'character' then lv-search = ''.
          lv-search:private-data in frame frame-select = h-scol:data-type.
    end.
    if lv-lastid ne ? then do:  
        h-buf:find-first('where ' + pv-table + 'tableid = ' + string(lv-lastid)).            
        h-qry:REPOSITION-TO-ROWID(h-buf:rowid) no-error.
    end.
    if pv-direction = 'up ' then do:
        h-qry:get-prev no-error.           
        h-qry:REPOSITION-TO-ROWID(h-buf:rowid) no-error.
    end.
    /* not right but at aleast it works for now */
    assign
        h-br:col = 1
        h-br:hidden = true
        lv-width = 3
        h-br:width = max(lv-width,h-br:width)
        frame {&frame-name}:width = max(80,frame {&frame-name}:width). 
    /* add fields to browse */
    do x = 1 to h-buf:num-fields:
        if h-buf:buffer-field(x):name = pv-table + 'tableid'
            then next.
         h-col = h-br:ADD-LIKE-column(h-buf:Buffer-field(x)).
         assign
             lv-colwidth = h-buf:Buffer-field(x):width-chars
             h-col:width-chars = max(font-table:get-text-width-chars(h-col:label,h-br:font),
                                     font-table:get-text-width-chars(string(h-buf:Buffer-field(x)),h-br:font))
             h-col:width-chars = max(h-col:width-chars,lv-colwidth)
             lv-width = lv-width + h-col:width + 1
             h-col:read-only = true
             h-col:resizable = true.
        if h-col:name = lv-cname 
            then h-col:LABEL-BGCOLOR = 15. 
        else if GetLookupInfo(h-col,'browse' + t-{&table-name}.lookupname) ne ? 
            then h-col:LABEL-BGCOLOR = 8.
        if h-col:name = lv-searchfield1 
            then assign
                    h-scol = h-col
                    h-br:current-column = h-col
                    lv-search:label in frame frame-select = h-col:label
                    lv-search:private-data = string(h-col) + '|' + h-col:data-type
                    lv-search = trim(string(h-buf:buffer-field(x):buffer-value,h-col:format)) no-error.  
        if multifield(lv-search:screen-value in frame frame-select) then do:
            if h-col:name = lv-searchfield2 
                then lv-search = lv-search + lv-delim + trim(string(h-buf:buffer-field(x):buffer-value,h-col:format)) no-error.
        end.
    end. 
    assign
       h-col:width-chars = h-col:width-chars + 1
       lv-width = lv-width + 1.
    
    if lv-width > 200 then lv-width = 200.
    
    if lv-width >= frame {&frame-name}:width - 3
        then frame {&frame-name}:width = max(80,lv-width + 3).
    
    assign
    /*     frame {&frame-name}:col = (session:width - frame {&frame-name}:width) / 2  */
        h-br:width = lv-width
        h-br:col = (frame {&frame-name}:width - h-br:width) / 2
        frame frame-ok:col = (frame {&frame-name}:width - frame frame-ok:width) / 2
        frame frame-select:col = (frame {&frame-name}:width - frame frame-select:width) / 2
        h-br:hidden = false.
   
    if h-qry:query-off-end then do:
       message 'No Records Found ' skip
                pv-table ' ' pv-where ' ' pv-by 
       view-as alert-box information.
/*        return. */
    end.

    apply 'entry' to h-br.
    centerwindow(frame {&frame-name}:handle).
end.
else do:
    h-qry:query-prepare('preselect each t-' + pv-table + 
                        pv-where + ' no-lock ' + pv-by).
    h-qry:query-open.
    h-qry:get-first() no-error.
    if h-qry:num-results = 0 then do:
         h-qry:query-prepare('preselect each t-' + pv-table + ' no-lock ' + pv-by).
         h-qry:query-open.
         h-qry:get-first() no-error.
    end.
    if lv-lastid ne ? then do:  
        h-buf:find-first('where ' + pv-table + 'tableid = ' + string(lv-lastid)).            
        h-qry:REPOSITION-TO-ROWID(h-buf:rowid) no-error.
    end.
    if pv-direction = 'up ' then do:
        h-qry:get-prev.           
        h-qry:REPOSITION-TO-ROWID(h-buf:rowid).
    end.
    h-buf = h-qry:get-buffer-handle(1).
    do x = 1 to h-buf:num-fields:
        h-col = h-buf:buffer-field(x).
        if h-col:name = lv-searchfield1 
            then assign
                   lv-search:label in frame frame-select = h-col:label
                   lv-search:private-data = string(h-col) + '|' + h-col:data-type
                   lv-search = trim(string(h-col:buffer-value,h-col:format)) no-error.  
        if multifield(lv-search:screen-value in frame frame-select) then do:
            if h-col:name = lv-searchfield2 
                then lv-search = lv-search + lv-delim + trim(string(h-col:buffer-value,h-col:format)) no-error.
        end.
    end. 
end.
 /*    h-br:CLEAR-SORT-ARROWS(). */
/*     h-br:set-sort-arrow(lv-colnum,(v-desc ne '')). */
/* message h-qry:prepare-string skip */
/* h-qry:index-information {&dbt}. */
disp lv-search with frame frame-select.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI frame-lookup  _DEFAULT-ENABLE
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
  {&OPEN-BROWSERS-IN-QUERY-frame-lookup}
  DISPLAY lv-search 
      WITH FRAME FRAME-select.
  ENABLE btn-search lv-search 
      WITH FRAME FRAME-select.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-select}
  DISPLAY lv-more 
      WITH FRAME FRAME-ok.
  ENABLE Btn-OK btn-getall Btn_Cancel 
      WITH FRAME FRAME-ok.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ok}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-data frame-lookup 
PROCEDURE get-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
def input param pv-select    as char no-undo.
def input param pv-direction as char no-undo.
def var x as int no-undo.
def var lv-numrecs as int no-undo.
def var lv-gotsome as log no-undo.

if valid-handle(h-qry) then do:
    if pv-direction = 'down' then h-qry:get-last.
                             else h-qry:get-first.
    h-buf = h-data:default-buffer-handle.
    if lv-lastid ne '-1' and lv-lastid ne ?
    then do:
        do x = 1 to h-buf:num-fields:
            if h-buf:buffer-field(x):name = substring(h-buf:table,3) + 'tableid'
            then do:
                lv-lastid = string(h-buf:buffer-field(x):buffer-value).
                leave.
            end.
        end.
    end.
    else do:
        DELETE OBJECT h-QrY no-error.
        h-buf:empty-temp-table no-error.
    end.
end.

/** debug messages for beginning delim chgaracter **/
pv-select = trim(pv-select).
if pv-select begins lv-delim
then do:
   x = 1.
   lv-search = pv-select.
   do while substring(pv-select,x,1) = lv-delim:
      substring(pv-select,x,1) = 'A'.
      x = x + 1.
   end.
   message 'Changing search for "' + lv-search + '" to "' + 
      substring(pv-select,x) + '".'.
end.
lv-numrecs = numrecords('',h-data).

ErrorClear(). /* in zenlibrary.p */ 
{{&core}run.i &programc   = t-zen-fldlook.srv-prog
              &pathc      = t-zen-fldlook.srv-path
              &Appsrv     = "System"  
              &procedurec = t-zen-fldlook.srv-proc
              &params     = "(pv-select,
                              pv-id,
                              lv-lastid,
                              pv-direction,
                              output table-handle h-data append)"}
                              
if lv-lastid = '-1' then lv-lastid = ?.

lv-gotsome = numrecords('',h-data) ne lv-numrecs.
lv-moretocome = if lv-gotsome then return-value = 'more' else true.
lv-more:hidden in frame frame-ok = not lv-moretocome.
btn-getall:hidden in frame frame-ok = not lv-moretocome.

if ANYErrors() then do: /* in zenlibrary.p */
/*    return error.  */
end.

if not lv-gotsome 
then return 'none'.
else return return-value.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFields frame-lookup 
PROCEDURE GetFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
def input param pv-id as char no-undo.
empty temp-table t-zen-lookupfld.
lv-searchfield2 = ''.
lv-searchfield1 = ''.
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

for each t-zen-lookupfld no-lock by order :
    if t-zen-lookupfld.keyfield    and lv-keyfield = '' then lv-keyfield    = t-zen-lookupfld.fieldname.
    if t-zen-lookupfld.searchfield then do:
      if lv-searchfield1 = '' then lv-searchfield1 = t-zen-lookupfld.fieldname.
      else if lv-searchfield2 = '' then lv-searchfield2 = t-zen-lookupfld.fieldname.
    end.
    if t-zen-lookupfld.descfield   then lv-descfield   = t-zen-lookupfld.fieldname.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFirstDesc frame-lookup 
PROCEDURE GetFirstDesc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
def input  param pv-select as char no-undo.
def output param pv-desc   as char no-undo.


ErrorClear(). /* in zenlibrary.p */ 
{{&core}run.i &program   = "dyn-lookup.p"
              &path      = "{&core}{&srv}"
              &Appsrv    = "System"  
              &procedure = GetFirstDescription
              &params    = "(pv-select,
                             pv-id,
                             output pv-desc)"}

if AnyErrors() then do: /* in zenlibrary.p */
    message 'error' view-as alert-box.
    return error.    
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetLookupRecord frame-lookup 
PROCEDURE GetLookupRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
def input param  lv-id as dec no-undo.
def buffer b-tab for t-{&table-name}.
find t-{&table-name} where {&table-name}tableid = lv-id 
                     no-lock no-error.
if not avail t-{&table-name} then return.

if t-{&table-name}.altlookupname ne '' 
then do:
    find b-tab where b-tab.lookupname = t-{&table-name}.altlookupname 
               no-lock NO-ERROR.

    assign lv-usealt:visible in frame frame-select = avail b-tab
           lv-usealt:sensitive    = avail b-tab
           lv-usealt:label        = t-{&table-name}.altlookuplabel
           lv-usealt:private-data = string(pv-id) + ',' +
                                    string(b-tab.{&table-name}tableid).
end.
lv-search:tooltip = 'The delimiter character is "' + string(lv-delim,"x") + '".'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise frame-lookup 
PROCEDURE initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
run getlookuprecord(pv-id).

run startlookup.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Off-EndTrigger frame-lookup 
PROCEDURE Off-EndTrigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
def input param pv-direction as char no-undo.

def var x as int no-undo.
def var lv-desc as char no-undo.

if not lv-moretocome
then return.

/* if lv-insort         then return.  */

    lv-by = ''.

if pv-direction = 'down' 
    then do: h-qry:get-last.
             lv-desc = ''.
         end.
    else do: h-qry:get-first.
             lv-desc = ''.
         end.
assign     
    h-buf = h-data:default-buffer-handle
    lv-select = ' ' + lv-delim + ' '.

do x = 1 to h-buf:num-fields:
   if h-buf:buffer-field(x):name = substring(h-buf:table,3) + 'tableid'
   then lv-lastid = string(h-buf:buffer-field(x):buffer-value).

    if h-buf:buffer-field(x):name = lv-searchfield1 
         then entry(1,lv-select,lv-delim) = string(h-buf:buffer-field(x):buffer-value).  
    if h-buf:buffer-field(x):name = lv-searchfield2 
         then entry(2,lv-select,lv-delim) = string(h-buf:buffer-field(x):buffer-value).  
end.

lv-search:screen-value in frame frame-select = lv-select.

Run get-data (lv-select ,pv-direction).

if return-value = 'none' then return.
lv-where = ''.

/* debug dump records to file */
/* numrecords('dumpclient',h-data). */
 
/* if pv-direction eq 'down' then do: */
/*     lv-by = ''. */
/*     run dobrowse(t-zen-fldlook.tablename,lv-where,lv-by,pv-direction,no). */
/* end. */
/* else */
do:

    lv-by = ' by ' + lv-searchfield1 + lv-desc + 
            if lv-searchfield2 ne '' then ' by ' + lv-searchfield2 + lv-desc
                                     else ''.
    run dobrowse(t-zen-fldlook.tablename,lv-where,lv-by,pv-direction,no).
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ok-Trigger frame-lookup 
PROCEDURE Ok-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
apply 'choose' to btn-ok in frame frame-ok.
return return-value.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-FindRow frame-lookup 
PROCEDURE Proc-FindRow :
{{&core}bug.i}
def input param pv-searchvalue  as char   no-undo format 'x(10)'.

def var v-hqry    as handle no-undo.
def var v-hcolumn as handle no-undo.
def var lv-qry    as char   no-undo.
def var h-db      as handle no-undo.
Def Var h-qry     As Handle No-undo.
def var h-qry2    as handle no-undo.
def var x as int no-undo.
if not gotall() then return.

v-hcolumn = h-br:CURRENT-COLUMN.

if v-hcolumn = ? 
then do:
    v-hcolumn = h-br:first-COLUMN.
    DO WHILE VALID-HANDLE(v-hcolumn):
        if v-hcolumn:LABEL-BGCOLOR = 15 then leave.
        v-hcolumn = v-hcolumn:NEXT-COLUMN no-error.
    END.
    if v-hcolumn = ? 
    then do:
      v-hcolumn = widget-handle(entry(1,lv-search:private-data in frame frame-select,'|')).
      if not valid-handle(v-hcolumn) then return.
    end.
end.

If Valid-handle(h-QrY2) Then h-QrY2:QUERY-CLOSE().
If Valid-handle(h-QrY2) Then DELETE OBJECT h-QrY2 no-error.
h-qry = h-br:query.
if h-qry:num-buffers > 1 then return.
create buffer h-db for table h-qry:get-buffer-handle(v-hcolumn:table).

/* i know i know only one active wait-for 
but update is quiclkest and easiest method */
if pv-searchvalue = ? then
update pv-searchvalue label "Search For" at 6 skip
  " Enter the search value or press Esc to cancel." at 5
   go-on (tab)
   with frame upd side-labels three-d  
   view-as dialog-box  title "Quick Search".

Create Query h-QrY2.
h-QrY2:Add-buffer(h-db).
if not multifield(pv-searchvalue) 
then lv-qry = "For EACH " + h-db:name +
         " where string(" + v-hcolumn:name + ") begins '" + pv-searchvalue + "'" +
         " no-lock" + " by " + v-hcolumn:name.
else do:
    def var lv-searchvalue1 as char no-undo.
    def var lv-searchvalue2 as char no-undo.
    lv-searchvalue1 = entry(1,pv-searchvalue,lv-delim).
    do x = 2 to num-entries(pv-searchvalue,lv-delim):
        lv-searchvalue2 = lv-searchvalue2 + entry(x,pv-searchvalue,lv-delim) + ' '.
    end.
    lv-searchvalue2 = trim(lv-searchvalue2).
    
    lv-qry = "For EACH " + h-db:name +
         " where string(" + lv-searchfield1 + ") begins '" + lv-searchvalue1 + "'" +
         ' and if ' + lv-searchField1 + ' = "' + lv-searchvalue1 + '"' +
               ' then string(' + lv-searchField2 + ') >= ' + '"' + lv-searchvalue2 + '"' +
               ' else string(' + lv-searchField2 + ') >= ""'.
         " no-lock" + " by " + v-hcolumn:name.
end.
        
h-QRY2:QUERY-PREPARE(lv-qry).
h-QRY2:QUERY-OPEN.
h-qry2:get-first.
if h-db:available
 then h-qry:reposition-to-rowid(h-db:rowid). 
else do:
    message 'Nothing found beginning with "' + pv-searchvalue + '".'.
    lv-qry = "For EACH " + h-db:name +
             " where " + v-hcolumn:name + " >= '" + pv-searchvalue + "'" +
             " no-lock" + " by " + v-hcolumn:name.
    h-QRY2:QUERY-PREPARE(lv-qry).
    h-QRY2:QUERY-OPEN.
    h-qry2:get-first.
    if h-db:available
     then h-qry:reposition-to-rowid(h-db:rowid). 
    else message 'Nothing Found'.
end.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resethandles frame-lookup 
PROCEDURE resethandles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
/* assign h-data  = ?  */
/*        h-qry  = ?   */
/*        h-br   = ?   */
/*        h-buf = ?.   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Search-Trigger frame-lookup 
PROCEDURE Search-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
if not lv-search:modified in frame frame-select then return.

  assign frame frame-select
         lv-search  
         lv-search = if lv-search = ? then '' else lv-search
         lv-lastid = ?
         lv-select = lv-search
         lv-by = ''  /* if valid-handle(h-scol) then h-scol:name
                                         else lv-searchfield1 */
         lv-insort = no.
 /* lv-moretocome = if lv-search = '' then true else lv-moretocome. */
 
if lv-by ne '' then lv-by = ' by ' + lv-by.
 
   case lv-search:private-data:
      when 'date' then do:
         if not validdate(lv-select) then do:
            message 'Invalid Date Value must be 99/99/9999' view-as alert-box error.
            return.
         end.
      end.
      when 'integer' or 
      when 'decimal' then do:
         if not ISnumeric(lv-select) then do:
            message 'Invalid Numeric Value' view-as alert-box error.
            return.
         end.
      end.
      otherwise do:
/*          lv-select = replace(lv-select,"'",''). */
          lv-select = replace(lv-select,'"','').
      end.
   end case.
   if not lv-moretocome then do:
    run proc-findrow(lv-select).
    return.
   end.
/*   if lv-select ne ''
   then */
    do:
      h-buf:empty-temp-table no-error.
      if valid-handle(h-data) then delete widget h-data.
      run createbrowse.
      lv-lastid = ?.
      Run get-data (lv-select,'down').
   end.
/*    else do: */
/*       run createbrowse. */
/*    end. */
   run dobrowse(t-zen-fldlook.tablename,'',lv-by,'down',yes).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sortbrowse frame-lookup 
PROCEDURE sortbrowse PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
def var v-hcolumn as handle no-undo.
def var lv-id as int no-undo.

 assign h-col  = h-br:CURRENT-COLUMN
        h-scol = h-col
        v-hcolumn = h-br:FIRST-COLUMN
        frame {&frame-name}:private-data = this-procedure:name.
        
if lv-moretocome and 
   h-col:LABEL-BGCOLOR ne 15 
then do: 
   lv-id = int(GetLookupInfo(h-col,'browse' + t-{&table-name}.lookupname)).

   run getlookuprecord(lv-id).
   
   if not avail t-{&table-name} then do:
      message 'No Lookup Defined for ' h-col:label  '(' h-col:table h-col:name ')'.
      h-col = h-scol.
      run getlookuprecord(pv-id).
   end.
   else do:
      pv-id = lv-id.
      if h-col:data-type = 'date' then lv-search:screen-value in frame frame-select= '01/01/0001'.
      if h-col:data-type = 'integer' or 
         h-col:data-type = 'decimal' then lv-search:screen-value = '0'.
      if h-col:data-type = 'character' then lv-search:screen-value = ''.
      lv-search:private-data = h-col:data-type.
      lv-usealt:checked = false.
      run startlookup.
   end.
end.
else do:
   lv-by    = ' by ' + h-col:name.
   lv-where = '' .
   /* " where " + h-col:name + " >= '" + lv-search:screen-value + "'" */
   
   if h-br:private-data = '' 
       then assign
                lv-by             = lv-by + ' descending'
                h-br:private-data = 'descending'.
       else h-br:private-data = ''.

   if not h-qry:query-prepare('for each t-' + entry(1,t-zen-fldlook.tablename,'{&delim2}') + 
                               lv-where + ' no-lock ' + lv-by)
   then message entry(1,t-zen-fldlook.tablename,'{&delim2}') skip
                lv-where skip lv-by skip
                'prepare failed'.

   h-qry:query-open.
   h-br:refresh() no-error. 
   DO WHILE VALID-HANDLE(v-hcolumn):
        ASSIGN v-hcolumn:LABEL-BGCOLOR = 8
               v-hcolumn               = v-hcolumn:NEXT-COLUMN.
   END.
   h-col:LABEL-BGCOLOR = 15.
   lv-search:label in frame frame-select = h-col:label.
   frame {&frame-name}:title = entry(1,frame {&frame-name}:title,' by') + ' By ' + h-col:label.
end.
/* message program-name(1) skip lv-dblclicked. */
/* apply 'entry' to lv-search. */

if not lv-dblclicked
then apply 'entry' to lv-search.
else lv-dblclicked = no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartLookup frame-lookup 
PROCEDURE StartLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
find t-{&table-name} where {&table-name}tableid = pv-id no-lock.

h-data = ?.
assign
    frame {&frame-name}:title = t-zen-fldlook.Window-title
    lv-by = t-zen-fldlook.byclause.

setbgcolour(frame {&frame-name}:handle,'lv-search','{&InputField}').
run createbrowse.
run getfields(t-zen-fldlook.lookupname).
if pv-startvalue ne '' then do:
    run GetFirstDesc(pv-startvalue,output pv-startvalue).
    LV-search:screen-value in frame frame-select = pv-startvalue.
    pv-startvalue = ''.
end.
    lv-search:modified = true.
    lv-moretocome = true.
    run search-trigger.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GotAll frame-lookup 
FUNCTION GotAll RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
  {{&core}bug.i}
    if not lv-more:hidden in frame frame-ok 
    then do:
        message 'Please get all records before sorting.' view-as alert-box.
        return false.
    end.

  RETURN true.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MultiField frame-lookup 
FUNCTION MultiField RETURNS LOGICAL
  ( pv-value as char ) :
{{&core}bug.i}

    if lv-multi ne (num-entries(pv-value,lv-delim) > 1)
    then run resethandles.
    lv-multi = lv-searchField2 ne ''.
    if lv-multi 
    then lv-multi = num-entries(pv-value,lv-delim) > 1.

  RETURN lv-multi.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

