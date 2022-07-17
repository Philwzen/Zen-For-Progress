&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME frame-main


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-fldlook NO-UNDO LIKE zen-fldlook.
DEFINE TEMP-TABLE t-zen-lookupfld NO-UNDO LIKE zen-lookupfld.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS frame-main 
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
&glob KeepRefreshButton
/* ***************************  Definitions  ************************** */
{app-paths.i}

&glob Table-name zen-fldlook
&glob unique-key    {&table-name}TableId
&glob about
&glob IgnoreSecurity true
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
def var h-br            as handle   no-undo.
def var h-buf           as handle   no-undo.
def var lv-lastid       as char     no-undo init ?.
def var h-scol          as handle   no-undo.
def var h-col as handle no-undo.
def var lv-where        as char     no-undo.
def var lv-by           as char     no-undo.
def var lv-delim as char no-undo.
def var lv-direction as char no-undo.
lv-where =  " sys-cd = '" + getsysvar('{&clv}gs-sys-cd') + /* in zenlibrary.p */
                  "' and practice = '" + getsysvar('{&clv}practice') + "'".
                           /* in zenlibrary.p */
lv-delim = ' '.
lv-delim = getfieldwhere('sysopt',lv-where,'aoptn[10]').
lv-where = ' '.
if pv-startvalue = '' then pv-startvalue = '9999999999'.
if asc(lv-delim) = -1 then lv-delim = ' '.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frame-main

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 n lv-search 
&Scoped-define List-2 Btn-OK RECT-3 Btn_Cancel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MultiField frame-main 
FUNCTION MultiField RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

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

DEFINE BUTTON Btn_Cancel AUTO-GO  NO-FOCUS
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

DEFINE FRAME frame-main
     SPACE(81.20) SKIP(19.38)
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
         AT COL 2 ROW 17.48
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
ASSIGN FRAME FRAME-ok:FRAME = FRAME frame-main:HANDLE
       FRAME FRAME-select:FRAME = FRAME frame-main:HANDLE.

/* SETTINGS FOR DIALOG-BOX frame-main
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME frame-main:SCROLLABLE       = FALSE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX frame-main
/* Query rebuild information for DIALOG-BOX frame-main
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX frame-main */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-getall frame-main
ON CHOOSE OF btn-getall IN FRAME FRAME-ok /* Get All */
DO:

assign 
    lv-search:screen-value in frame frame-select = ''
    lv-search  
    lv-lastid = '-1'.

    run createbrowse.
    h-buf:empty-temp-table no-error.

    Run get-data (lv-search,lv-direction).

    run doit (lv-search).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-OK frame-main
ON CHOOSE OF Btn-OK IN FRAME FRAME-ok /* OK */
DO:
 def var x as int no-undo.
 def var v-field as handle no-undo.
 def var h-buf   as handle no-undo.

 def var lv-key  as char no-undo.
 def var lv-desc as char no-undo.
 
 h-buf = h-data:default-buffer-handle.

 do x = 1 to h-buf:num-fields:  
     v-field = h-buf:Buffer-field(x).

     if v-field:name = lv-keyfield  then lv-key  = v-field:buffer-value.
     if v-field:name = lv-descfield then lv-desc = v-field:buffer-value.
 end.
 return lv-key + '{&Delim2}' + lv-desc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-select
&Scoped-define SELF-NAME btn-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-search frame-main
ON CHOOSE OF btn-search IN FRAME FRAME-select /* Find */
DO:
   assign frame frame-select
         lv-search  
         lv-search = if lv-search = ? then '' else lv-search
         lv-lastid = ?
         lv-select = lv-search
         lv-by = ''
lv-moretocome = if lv-search = '' then true else lv-moretocome.


   if valid-handle(h-col) then do:
      case h-col:data-type:
         when 'date' then do:
            if not validdate(lv-select) then do:
               message 'Invalid Value' view-as alert-box error.
               return no-apply.
            end.
         end.
         when 'integer' or 
         when 'decimal' then do:
            if not ISnumeric(lv-select) then do:
               message 'Invalid Value' view-as alert-box error.
               return no-apply.
            end.
         end.
      end case.
   end.


   if lv-moretocome  or 
      lv-select ne '' 
   then do:
      if valid-handle(h-data) then delete widget h-data.
      run createbrowse.
      h-buf:empty-temp-table no-error.
      lv-lastid = ?.
      Run get-data (lv-select,lv-direction).
   end.
   else run createbrowse.
   run dobrowse(entry(1,t-zen-fldlook.tablename,'{&delim2}'),'',lv-by).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ok
&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel frame-main
ON CHOOSE OF Btn_Cancel IN FRAME FRAME-ok /* Cancel */
DO:
return ''.
/*  
apply 'close' to this-procedure.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-select
&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search frame-main
ON RETURN OF lv-search IN FRAME FRAME-select /* Search */
DO:
  apply 'tab' to lv-search.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search frame-main
ON TAB OF lv-search IN FRAME FRAME-select /* Search */
DO:
  
  lv-select = self:screen-value.  

  if donotfire('btn_cancel,btn-getall,btn-ok,h-br') then return.


apply 'choose' to btn-search.                             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-usealt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-usealt frame-main
ON VALUE-CHANGED OF lv-usealt IN FRAME FRAME-select /* Use Alternate */
DO:
 if self:checked then pv-id = dec(entry(2,self:private-data)).
                 else pv-id = dec(entry(1,self:private-data)).
 run startlookup.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frame-main
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK frame-main 


/* ***************************  Main Block  *************************** */
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.
/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
{{&core}pgm-hdr.i}


MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {{&core}sec-chk.i}
  RUN enable_UI.
  Run initialise.
  {{&core}wid-chk.i}
apply 'entry' to lv-search.

  WAIT-FOR GO OF FRAME frame-main. 
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateBrowse frame-main 
PROCEDURE CreateBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if valid-handle(h-br)  then delete widget h-br.
if valid-handle(h-qry) then delete widget h-qry.

if not valid-handle(h-br) then
create browse h-br
    ASSIGN FRAME = frame frame-main:HANDLE
           /* QUERY = h-qry  */
           row-markers = false
           row = 5
           row-height-chars = 1
           column = 1
           WIDTH = 3
           DOWN = 12
           visible = true
           SENSITIVE = TRUE
           READ-ONLY = NO
           fit-last-column = true
        triggers:
            ON end,off-end persistent run off-endTrigger (lv-direction).
/*             ON home,off-home persistent run off-endTrigger ('up').  */
            ON 'START-SEARCH' persistent run sortbrowse.
            ON 'mouse-select-dblclick' persistent run ok-trigger.
        end triggers. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI frame-main  _DEFAULT-DISABLE
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
  HIDE FRAME frame-main.
  HIDE FRAME FRAME-ok.
  HIDE FRAME FRAME-select.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoBrowse frame-main 
PROCEDURE DoBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-table as char no-undo.
def input param pv-where as char no-undo.
def input param pv-by    as char no-undo.

def var h-col as handle no-undo.
def var x     as int    no-undo.
def var lv-cname as char no-undo.
def var lv-colwidth as dec    no-undo.
def var lv-width as dec no-undo.
def var lv-lastpos      as rowid    no-undo.

/* message 'use by ' t-{&table-name}.byclause                */
/*    view-as alert-box buttons yes-no update lv-ok as log.  */
/*    if lv-ok then pv-by = t-{&table-name}.byclause.        */

pv-by = t-{&table-name}.byclause.

if valid-handle(h-scol)
    then lv-cname = h-scol:name.
    else lv-cname = lv-searchfield1.
   
    h-buf = h-data:default-buffer-handle.
   
   if lv-lastid ne ? then lv-lastpos = h-buf:rowid. 

   If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.

   create query h-qry.

   h-QrY:Add-buffer(h-buf).
   h-qry:query-prepare('for each t-' + pv-table + 
                       pv-where + ' no-lock ' + pv-by).
   h-qry:query-open.
   if h-qry:num-results = 0 then do:
        h-qry:query-prepare('for each t-' + pv-table + ' no-lock ' + pv-by).
        h-qry:query-open.
   end.
   h-qry:get-first().
   if h-qry:query-off-end then do:
    btn-ok:sensitive in frame frame-ok = false.
    return.
   end.

/* not right but at aleast it works for now */
   assign
       h-br:query = h-qry
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
        if h-col:name = lv-searchfield1 
            then assign
                    lv-search:label in frame frame-select = h-col:label
                    lv-search = h-buf:buffer-field(x):buffer-value.  
        if multifield() then do:
            if h-col:name = lv-searchfield2 
            then lv-search = lv-search + lv-delim + h-buf:buffer-field(x):buffer-value.
        end.
    end. 
disp lv-search with frame frame-select.
assign
   h-col:width-chars = h-col:width-chars + 1
   lv-width = lv-width + 1.

if lv-width > 200 then lv-width = 200.

if lv-width >= frame {&frame-name}:width - 3
    then frame {&frame-name}:width = max(80,lv-width + 3).

    if lv-lastid ne ? then do:  
        h-buf:find-first('where ' + pv-table + 'tableid = ' + string(lv-lastid)).            
        h-qry:REPOSITION-TO-ROWID(h-buf:rowid) no-error.
    end.
assign
/*     frame {&frame-name}:col = (session:width - frame {&frame-name}:width) / 2  */
    h-br:width = lv-width
    h-br:col = (frame {&frame-name}:width - h-br:width) / 2
    frame frame-ok:col = (frame {&frame-name}:width - frame frame-ok:width) / 2
    frame frame-select:col = (frame {&frame-name}:width - frame frame-select:width) / 2
    h-br:hidden = false.
centerwindow(frame {&frame-name}:handle).
apply 'entry' to h-br.
/* apply 'entry' to lv-search.  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doit frame-main 
PROCEDURE doit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def input param pv-select as char no-undo.

if lv-searchfield1 ne '' 
    then lv-by = ' by ' + lv-searchfield1.
    else lv-by = ''.

   run dobrowse(entry(1,t-zen-fldlook.tablename,'{&delim2}'),'',lv-by).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI frame-main  _DEFAULT-ENABLE
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
  {&OPEN-BROWSERS-IN-QUERY-frame-main}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-data frame-main 
PROCEDURE get-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-select    as char no-undo.
def input param pv-direction as char no-undo.
def var x as int no-undo.

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

if opsys = 'unix' then 
pv-select = replace(pv-select,'\','~\').

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
lv-moretocome = return-value = 'more'.
lv-more:hidden in frame frame-ok = not lv-moretocome.
btn-getall:hidden in frame frame-ok = not lv-moretocome.

if AnyErrors() then do: /* in zenlibrary.p */
    return error.    
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFields frame-main 
PROCEDURE GetFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFirstDesc frame-main 
PROCEDURE GetFirstDesc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise frame-main 
PROCEDURE initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def buffer b-tab for t-{&table-name}.

find t-{&table-name} where {&table-name}tableid = pv-id 
                     no-lock no-error.

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

lv-direction = 'down'.
   
run startlookup.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Off-EndTrigger frame-main 
PROCEDURE Off-EndTrigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-direction as char no-undo.

def var x as int no-undo.
if not lv-moretocome then return.
if pv-direction = 'down' then h-qry:get-last.
                         else h-qry:get-first.

h-buf = h-data:default-buffer-handle.
lv-select = ' ' + lv-delim + ' '.
do x = 1 to h-buf:num-fields:
   if h-buf:buffer-field(x):name = substring(h-buf:table,3) + 'tableid'
   then lv-lastid = string(h-buf:buffer-field(x):buffer-value).

   if h-buf:buffer-field(x):name = lv-searchfield1 then entry(1,lv-select,lv-delim) = string(h-buf:buffer-field(x):buffer-value).
   if h-buf:buffer-field(x):name = lv-searchfield2 then entry(2,lv-select,lv-delim) = string(h-buf:buffer-field(x):buffer-value).
end.
lv-search:screen-value in frame frame-select = lv-select.

run createbrowse.

Run get-data (lv-select ,pv-direction).
lv-where = ''. lv-by = ''.
run dobrowse(entry(1,t-zen-fldlook.tablename,'{&delim2}'),lv-where,lv-by).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ok-Trigger frame-main 
PROCEDURE Ok-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
apply 'choose' to btn-ok in frame frame-ok.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resethandles frame-main 
PROCEDURE resethandles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* assign h-data  = ?  */
/*        h-qry  = ?   */
/*        h-br   = ?   */
/*        h-buf = ?.   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sortbrowse frame-main 
PROCEDURE sortbrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def var h-col as handle no-undo.
   def var v-hcolumn as handle no-undo.

if lv-moretocome then do:
    message 'Please get all records before sorting.'
        view-as alert-box.
    return.
end.
    assign h-col = h-br:CURRENT-COLUMN
           h-scol = h-col
/*            lv-search:label in frame frame-select = h-col:label  */
           v-hcolumn = h-br:FIRST-COLUMN.

    DO WHILE VALID-HANDLE(v-hcolumn):
        ASSIGN v-hcolumn:LABEL-BGCOLOR = 8
               v-hcolumn               = v-hcolumn:NEXT-COLUMN.
    END.

/*    lv-searchfield1 = h-col:name.  */

/* find t-zen-lookupfld where t-zen-lookupfld.fieldname = lv-searchfield1 no-lock no-error.  */
/* if avail t-zen-lookupfld                                                                  */
/* then assign btn-getall:sensitive in frame frame-ok = t-zen-lookupfld.extentnum = 0        */
/*             btn-search:sensitive in frame frame-select = t-zen-lookupfld.extentnum = 0    */
/*             lv-search:sensitive in frame frame-select = t-zen-lookupfld.extentnum = 0.    */
/* else if ambiguous t-zen-lookupfld                                                         */
/*     then assign btn-getall:sensitive in frame frame-ok = false                            */
/*                 btn-search:sensitive in frame frame-select = false                        */
/*                 lv-search:sensitive in frame frame-select = false.                        */

/*    lv-search:label = h-col:label.                                          */
/*    lv-where = ' where ' + lv-searchfield + ' >= "' +                       */
/*                       lv-search:screen-value in frame frame-select + '"'.  */

   lv-by = ' by ' + h-col:name.
lv-where = ''.
   if h-br:private-data = '' 
       then assign
                lv-by = lv-by + ' descending'
                h-br:private-data = 'descending'.
       else h-br:private-data = ''.

   if not h-qry:query-prepare('for each t-' + entry(1,t-zen-fldlook.tablename,'{&delim2}') + 
                       lv-where + ' no-lock ' + lv-by)
then message 'prepare failed'.
   h-qry:query-open.
   h-br:refresh().
   h-col:LABEL-BGCOLOR = 15.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartLookup frame-main 
PROCEDURE StartLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

find t-{&table-name} where {&table-name}tableid = pv-id no-lock.

h-data = ?.
assign
    frame {&frame-name}:title = t-zen-fldlook.Window-title
    lv-by = t-zen-fldlook.byclause.

    setbgcolour(frame {&frame-name}:handle,'lv-search','{&InputField}').
    run createbrowse.
    run getfields(t-zen-fldlook.lookupname) .
 
    LV-search:screen-value in frame frame-select = pv-startvalue.
if pv-startvalue ne '9999999999' then do:
    run GetFirstDesc(pv-startvalue,output pv-startvalue).
    pv-startvalue = '9999999999'.
end.
    apply 'choose' to btn-search.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MultiField frame-main 
FUNCTION MultiField RETURNS LOGICAL
  ( /* parameter-definitions */ ) :

if lv-multi ne (num-entries(lv-search:screen-value in frame frame-select,lv-delim) > 1)
then run resethandles.

  lv-multi = num-entries(lv-search:screen-value,lv-delim) > 1.

  RETURN lv-multi.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

