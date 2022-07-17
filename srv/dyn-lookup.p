&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
 create widget-pool.
 &glob bug
/* ***************************  Definitions  ************************** */
{app-paths.i}


def var lv-maxlistcount as int no-undo.
lv-maxlistcount = int(getctrl('maxlistcount')).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-AddTableFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddTableFields Procedure 
FUNCTION AddTableFields returns character
  ( pv-lookupname as char,
    pv-db as handle,
    pv-table as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFieldHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldHandle Procedure 
FUNCTION GetFieldHandle returns handle
  ( pv-buffer as handle,
    pv-fname as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetString Procedure 
FUNCTION SetString returns character
  ( pv-str as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 22.71
         WIDTH              = 50.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}mainp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetFirstDescription) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFirstDescription Procedure 
PROCEDURE GetFirstDescription :
def input  param pv-searchvalue as char no-undo.
def input  param pv-lookupid    as dec  no-undo.
def output param pv-returnvalue as char no-undo.

def var lv-searchfield1 as char no-undo.
def var lv-searchfield2 as char no-undo.
def var lv-where  as char   no-undo.
def var lv-keyfield as char no-undo.
def var h-field as handle no-undo.
def var x as  int no-undo.
def var lv-rep-value as char no-undo.
def var lv-rep-pos as char no-undo.
def var h-db     as handle no-undo.
def var h-qry    as handle no-undo.
def var lv-delim as char no-undo.
def var lv-ok as log no-undo.
lv-delim = GetCtrl('LookupDelim').
if lv-delim = '' then lv-delim = ' '.

find first zen-fldlook where zen-fldlook.zen-fldlooktableid = pv-lookupid 
                       no-lock no-error.
create buffer h-db for table zen-fldlook.tablename.

for each zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname 
                         and zen-lookupfld.lfield 
                         and (zen-lookupfld.KeyField or zen-lookupfld.SearchField)
        no-lock:
    do x = 1 to h-db:num-fields:
        h-field = h-db:Buffer-field(x).
        if h-field:name = zen-lookupfld.fieldname 
        then do: 
           if zen-lookupfld.KeyField then lv-keyfield = h-field:name.
           if zen-lookupfld.SearchField then do:
               if lv-searchfield1 = '' then lv-searchField1 = h-field:name.
               else if lv-searchfield2 = '' then lv-searchfield2 = h-field:name.
           end.
        end.
    end.
end.

X = 1.

if Valid-handle(h-QrY) then h-QrY:QUERY-CLOSE().
if Valid-handle(h-QrY) then delete object h-QrY no-error.
create query h-QrY.
h-QrY:Add-buffer(h-db).

lv-where = zen-fldlook.whereclause.                 

if lv-where <> "" then
do x = 1 to num-entries(zen-fldlook.wherefield):
    assign lv-rep-pos = "#" + string(x)
           lv-rep-value = entry(x,zen-fldlook.wherefield).
           lv-rep-value = "'" + dynamic-function(entry(1,lv-rep-value,'{&Delim2}'),
                                                 entry(2,lv-rep-value,'{&Delim2}')) + "'".
           lv-where     = replace(lv-where,lv-rep-pos,LV-REP-VALUE).
end.

if lv-where = '' then lv-where = ' where '.
                 else lv-where = lv-where + ' and ' .
lv-where = lv-where + lv-keyField + " = '" + pv-searchvalue + "'".
lv-where = "for each " + zen-fldlook.tablename + " " +
                     lv-where + " no-lock ".

lv-ok = h-QRY:QUERY-PREPARE(lv-where) no-error.
if not lv-ok
then do:
    ErrorCreate(50,'Lookup Query',lv-where,'','').
    return 'failed'.
end.

h-QRY:QUERY-OPEN.
h-QRY:GET-NEXT().

if not h-qry:query-off-end 
then do:
    pv-returnvalue = GetFieldValue(h-db,lv-searchfield1,0).
    if lv-searchfield2 ne '' 
    then pv-returnvalue = pv-returnvalue + lv-delim + GetFieldValue(h-db,lv-searchfield2,0).
end.

if Valid-handle(h-QrY) then    
    delete object h-QrY no-error.  

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetRecord Procedure 
PROCEDURE GetRecord :
def input  param pv-lookupid  as dec  no-undo.
def input  param pv-type as char no-undo.
def input  param pv-select    as char no-undo.
def output param table-handle pv-hand.

def var lv-altlookupname as char no-undo.
find first zen-fldlook where zen-fldlook.zen-fldlooktableid = pv-lookupid 
                       no-lock no-error.
if not can-find(first zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname)
then do:
   pv-hand = ?.
   return.
end.

lv-altlookupname = zen-fldlook.AltLookupName.
if num-entries(zen-fldlook.tablename,'{&Delim2}') > 1  
then run GetTheJoinDataRecord (pv-lookupid,pv-select,pv-type,output table-handle pv-hand).
else run GetTheDataRecord (pv-lookupid,pv-select,pv-type,output table-handle pv-hand).

if pv-hand = ? and lv-altlookupname ne ''
then do:
   find first zen-fldlook where zen-fldlook.lookupname = lv-altlookupname 
                          no-lock no-error.
   if not can-find(first zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname)
   then do:
      pv-hand = ?.
      return.
   end.
   if num-entries(zen-fldlook.tablename,'{&Delim2}') > 1 
   then run GetTheJoinDataRecord (zen-fldlook.zen-fldlooktableid,pv-select,pv-type,output table-handle pv-hand).
   else run GetTheDataRecord (zen-fldlook.zen-fldlooktableid,pv-select,pv-type,output table-handle pv-hand).
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetTheDataRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTheDataRecord Procedure 
PROCEDURE GetTheDataRecord :
def input  param pv-lookupid  as dec  no-undo.
def input  param pv-select    as char no-undo.
def input  param pv-type      as char no-undo.
def output param table-handle pv-hand.

find first zen-fldlook where zen-fldlook.zen-fldlooktableid = pv-lookupid 
                       no-lock no-error.
if not can-find(first zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname)
then do:
   pv-hand = ?.
   return.
end.

def var b-data    as handle no-undo.
def var lv-where  as char   no-undo.
def var lv-search as char no-undo.
def var lv-field  as char   no-undo.
def var lv-fname as char no-undo.
def var h-buff  as handle no-undo.
def var h-field as handle no-undo.
def var X       as int no-undo.
def var y as  int no-undo.
def var z as int no-undo.
def var cnt     as int no-undo init 1.
def var lv-goto as char no-undo.
def var lv-rep-value as char no-undo.
def var lv-rep-pos as char no-undo.
def var lv-qry   as char   no-undo.
def var lv-label as char no-undo.
def var lv-format as char no-undo.
def var h-tt     as handle no-undo.
def var h-db     as handle no-undo.
def var h-qry    as handle no-undo.
def var h-fld as handle no-undo.
def var lv-table as char no-undo.
lv-table = entry(1,zen-fldlook.tablename,'{&delim2}').

create buffer h-db for table lv-table.
create temp-table pv-hand.
for each zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname no-lock by order:
    do y = 1 to h-db:num-fields:
        h-field = h-db:Buffer-field(y).
        if zen-lookupfld.keyField then lv-search = h-field:name.
        if h-field:name = zen-lookupfld.fieldname and
            zen-lookupfld.ifield then do:
            assign
                lv-format = if zen-lookupfld.FieldFormat = '' 
                            then h-field:format
                            else zen-lookupfld.FieldFormat
                lv-label = if zen-lookupfld.FieldLabel = ''
                            then  h-field:label
                            else zen-lookupfld.FieldLabel.
            if zen-lookupfld.extentnum = 0 
            then lv-fname = zen-lookupfld.fieldname.
            else lv-fname = zen-lookupfld.fieldname + 'x' + string(zen-lookupfld.extentnum).
            pv-hand:ADD-NEW-FIELD(lv-fname,
                          h-field:data-type,0,
                          lv-format,'',
                          lv-label,lv-label). 
            leave.
        end.
    end.
end.

pv-hand:temp-table-prepare("t-" + lv-table).
X = 1.
h-tt = pv-hand:default-buffer-handle.

if Valid-handle(h-QrY) then h-QrY:QUERY-CLOSE().
if Valid-handle(h-QrY) then delete object h-QrY no-error.
create query h-QrY.
h-QrY:Add-buffer(h-db).

case pv-type:
    when 'data' then do:
        lv-where =  zen-fldlook.whereclause.                 
        if lv-where <> "" then
        do x = 1 to num-entries(zen-fldlook.wherefield):
            assign lv-rep-pos = "#" + string(x)
                   lv-rep-value = entry(x,zen-fldlook.wherefield).
                   lv-rep-value = "'" + dynamic-function(entry(1,lv-rep-value,'{&Delim2}'),entry(2,lv-rep-value,'{&Delim2}')) + "'".
                   lv-where     = replace(lv-where,lv-rep-pos,LV-REP-VALUE).
        end.

        if lv-where = '' then lv-where = ' where '.
                         else lv-where = lv-where + ' and ' .
        lv-where = lv-where + lv-search + ' = "' + pv-select + '"'.       

        lv-where = "for each " + lv-table + " " +
                                 lv-where + " no-lock " +
                                 zen-fldlook.byclause.
    end.
    when 'tableid' then lv-where = "for each " + lv-table + " " +
                                    "where " + lv-table + "tableid = " + pv-select.
end case.


h-QRY:QUERY-PREPARE(lv-where).

if error-status:error then do:
    ErrorCreate(50,'Lookup Query','','','').
    return 'failed'.
end.

h-QRY:QUERY-OPEN.
if error-status:error then do:
    ErrorCreate(50,'Lookup Query','','','').
    return 'failed'.
end.

h-buff = h-qry:get-buffer-handle(1).

h-QRY:GET-NEXT().
/* message h-qry:num-results skip lv-where {&dbt}. */
if not H-QRY:QUERY-OFF-END then
do:
    h-tt:buffer-create().
    h-tt:buffer-copy(h-db).
    for each zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname 
                             and zen-lookupfld.ifield
                           no-lock:
        if zen-lookupfld.extentnum = 0 
        then lv-fname = zen-lookupfld.fieldname.
        else lv-fname = zen-lookupfld.fieldname + 'x' + string(zen-lookupfld.extentnum).
        y = 1.
        do y = 1 to h-tt:num-fields:
            h-field = h-tt:Buffer-field(y).
            if lv-fname ne h-field:name then next.
            z = 1.
            lv-fname = zen-lookupfld.fieldname.
            do z = 1 to h-db:num-fields:
                h-fld = h-db:buffer-field(z).
                if h-fld:name = lv-fname then do:
                    if zen-lookupfld.extentnum > 0 
                    then h-field:Buffer-value = h-fld:buffer-value(zen-lookupfld.extentnum).
                    else h-field:Buffer-value = h-fld:buffer-value.
                    leave.
                end.
            end.
        end.
     end.                                                
    if H-QRY:QUERY-OFF-END then leave.
end.
if h-qry:num-results = 0 then pv-hand = ?.
if Valid-handle(h-QrY) then    
    delete object h-QrY no-error.  

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetTheJoinDataRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTheJoinDataRecord Procedure 
PROCEDURE GetTheJoinDataRecord :
def input  param pv-lookupid  as dec  no-undo.
def input  param pv-select    as char no-undo.
def input  param pv-type as char no-undo.
def output param table-handle pv-hand.

find first zen-fldlook where zen-fldlook.zen-fldlooktableid = pv-lookupid 
                       no-lock no-error.
if not can-find(first zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname)
then do:
   pv-hand = ?.
   return.
end.

def var b-data    as handle no-undo.
def var lv-where  as char   no-undo.
def var lv-search as char no-undo.
def var lv-field  as char   no-undo.
def var lv-fname as char no-undo.
def var h-buff  as handle no-undo.
def var h-field as handle no-undo.
def var X       as int no-undo.
def var y as  int no-undo.
def var z as int no-undo.
def var cnt     as int no-undo init 1.
def var lv-goto as char no-undo.
def var lv-rep-value as char no-undo.
def var lv-rep-pos as char no-undo.
def var lv-qry   as char   no-undo.
def var lv-label as char no-undo.
def var lv-format as char no-undo.
def var h-tt     as handle no-undo.
def var h-db     as handle no-undo.
def var h-qry    as handle no-undo.
def var h-fld as handle no-undo.
def var h-db2 as handle no-undo.
def var lv-mastertable as char no-undo.

/* define dynamic temp table for results */
lv-mastertable = entry(1,zen-fldlook.tablename,'{&delim2}').
create buffer h-db for table lv-mastertable.
create temp-table pv-hand.

lv-search = AddTableFields(zen-fldlook.lookupname,h-db,pv-hand).

pv-hand:temp-table-prepare("t-" + entry(1,zen-fldlook.tablename,'{&delim2}')).
X = 1.
h-tt = pv-hand:default-buffer-handle.

if Valid-handle(h-QrY) then h-QrY:QUERY-CLOSE().
if Valid-handle(h-QrY) then delete object h-QrY no-error.
create query h-QrY.
h-QrY:Add-buffer(h-db).

/* deal with multiple tables in query */
if num-entries(zen-fldlook.tablename,'{&Delim2}') > 1  then do:
   do x = 2 to num-entries(zen-fldlook.tablename,'{&Delim2}'):
       create buffer h-db2 for table entry(x,zen-fldlook.tablename,'{&Delim2}').
       if Valid-handle(h-QrY) then h-QrY:Add-buffer(h-db2). 
   /*     lv-prepstring = lv-prepstring + "EACH " + entry(x,pv-table,'{&Delim2}')  */
   /*                     + ' ' + entry(x,pv-Where,'{&Delim2}') + " no-lock, ".    */
   end.
   x = 1.
end.

lv-where =  zen-fldlook.whereclause.                 
if lv-where <> "" then
do x = 1 to num-entries(zen-fldlook.wherefield):
    assign lv-rep-pos = "#" + string(x)
           lv-rep-value = entry(x,zen-fldlook.wherefield).
           lv-rep-value = "'" + dynamic-function(entry(1,lv-rep-value,'{&Delim2}'),entry(2,lv-rep-value,'{&Delim2}')) + "'".
           lv-where     = replace(lv-where,lv-rep-pos,LV-REP-VALUE).
end.

if lv-where = '' then lv-where = ' where '.
                 else lv-where = lv-where + ' and ' .
lv-where = lv-where + lv-search + ' = "' + pv-select + '"'.
lv-where = "for each " + lv-mastertable + " " +
                     lv-where + " no-lock " +
                     zen-fldlook.byclause.

h-QRY:QUERY-PREPARE(lv-where) no-error.

if error-status:error then do:
    ErrorCreate(50,'Lookup Query','','','').
    return 'failed'.
end.

h-QRY:QUERY-OPEN.

h-buff = h-qry:get-buffer-handle(1).

h-QRY:GET-NEXT().

if not H-QRY:QUERY-OFF-END then
do:
    h-tt:buffer-create().
    h-tt:buffer-copy(h-db).
    for each zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname 
                             and zen-lookupfld.ifield
                           no-lock:
        if zen-lookupfld.extentnum = 0 
        then lv-fname = zen-lookupfld.fieldname.
        else lv-fname = zen-lookupfld.fieldname + 'x' + string(zen-lookupfld.extentnum).
        y = 1.
        do y = 1 to h-tt:num-fields:
            h-field = h-tt:Buffer-field(y).
            if lv-fname ne h-field:name then next.
            z = 1.
            lv-fname = zen-lookupfld.fieldname.
            do z = 1 to h-db:num-fields:
                h-fld = h-db:buffer-field(z).
                if h-fld:name = lv-fname then do:
                    if zen-lookupfld.extentnum > 0 
                    then h-field:Buffer-value = h-fld:buffer-value(zen-lookupfld.extentnum).
                    else h-field:Buffer-value = h-fld:buffer-value.
                    leave.
                end.
            end.
        end.
     end.                                                
    if H-QRY:QUERY-OFF-END then leave.
end.
if h-qry:num-results = 0 then pv-hand = ?.
if Valid-handle(h-QrY) then    
    delete object h-QrY no-error.  

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-JoinLookup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JoinLookup Procedure 
PROCEDURE JoinLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input  param pv-searchvalue as char no-undo.
def input  param pv-lookupid    as dec  no-undo.
def input  param pv-lastid      as char no-undo.
def input  param pv-direction   as char no-undo.
def output param table-handle pv-hand.

def var lv-searchfield1 as char no-undo init ''.
def var lv-searchfield2 as char no-undo init ''.
def var lv-searchvalue1 as char no-undo init ''.
def var lv-searchvalue2 as char no-undo init ''.
def var lv-delim        as char no-undo init ' '.
def var b-data          as handle no-undo.
def var lv-where        as char   no-undo.
def var lv-select       as char  no-undo.
def var lv-maxcount     as int   no-undo.
def var lv-field        as char   no-undo.
def var lv-by           as char   no-undo.
def var lv-keyfield     as char no-undo.
def var h-buff          as handle no-undo.
def var h-field         as handle no-undo.
def var h-fld           as handle no-undo.
def var lv-off-end      as log no-undo.
def var X               as int no-undo.
def var y               as int no-undo.
def var z               as int no-undo.
def var cnt             as int no-undo init 1.
def var lv-goto         as char no-undo.
def var lv-rep-value    as char no-undo.
def var lv-where2       as char no-undo.
def var lv-query        as char no-undo.
def var lv-by2          as char no-undo.
def var lv-gotone       as log  no-undo.
def var lv-rep-pos      as char no-undo.
def var lv-qry          as char   no-undo.
def var lv-label        as char   no-undo.
def var lv-format       as char   no-undo.
def var h-tt            as handle no-undo.
def var h-db            as handle no-undo.
def var h-db2 as        handle no-undo.
def var h-qry           as handle no-undo.
def var lv-mastertable as char no-undo.
if opsys = 'unix' then do:
   pv-searchvalue = replace(pv-searchvalue,'\\','').
   pv-searchvalue = replace(pv-searchvalue,'"','~"').
   pv-searchvalue = replace(pv-searchvalue,"'","~'").
end.

lv-delim = GetCtrl('LookupDelim').
/* needs to use sysopt.aoptn[10] as delimiter field */
lv-searchvalue1 = pv-searchvalue.
/*
lv-searchvalue1 = entry(1,pv-searchvalue,lv-delim).
if num-entries(pv-searchvalue,lv-delim) > 1 then
    lv-searchvalue2 = Substring(pv-searchvalue,Index(pv-searchvalue,entry(2,pv-searchvalue,lv-delim))).
*/
find first zen-fldlook where zen-fldlook.zen-fldlooktableid = pv-lookupid 
                       no-lock no-error.
if not avail zen-fldlook then do: 
   ErrorCreate(50,'zen-fldlook',string(pv-lookupid),pv-searchvalue,string(pv-lastid)).
   return 'failed'.
end.

if not can-find(first zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname)
then do:
   pv-hand = ?.
   return.
end.

/* reinitialise maxlistcount as it is reset at bottom if we are at end of
   dataset
 lv-maxlistcount = zen-fldlook.dataguess.*/
cnt = 1.
if pv-lastid = '-1' then 
    assign pv-lastid = ?
           lv-maxcount = 999999.
else lv-maxcount = lv-maxlistcount.

/* define dynamic temp table for results */
lv-mastertable = entry(1,zen-fldlook.tablename,'{&delim2}').
create buffer h-db for table lv-mastertable.
create temp-table pv-hand.
pv-hand:add-like-field(lv-mastertable + 'tableid',lv-mastertable + '.' + lv-mastertable + 'tableid').

for each zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname no-lock by order:
    do y = 1 to h-db:num-fields:
        h-field = h-db:Buffer-field(y).

        if h-field:name = zen-lookupfld.fieldname 
        then do:  
    
           if zen-lookupfld.SearchField then do:
               if lv-searchfield1 = '' then lv-searchField1 = h-field:name.
               else if lv-searchfield2 = '' then lv-searchfield2 = h-field:name.
           end.
   
           if zen-lookupfld.Keyfield and 
              lv-keyfield = '' 
           then lv-keyField = h-field:name.
            if zen-lookupfld.lfield then do:
                assign
                    lv-format = if zen-lookupfld.FieldFormat = '' 
                                then h-field:format
                                else zen-lookupfld.FieldFormat
                    lv-label = if zen-lookupfld.FieldLabel = ''
                                then  h-field:label
                                else zen-lookupfld.FieldLabel.
                pv-hand:ADD-NEW-FIELD(zen-lookupfld.fieldname,
                              h-field:data-type,0,
                              lv-format,'',
                              lv-label,lv-label). 
            end.
        end.
    end.
end.

pv-hand:temp-table-prepare("t-" + lv-mastertable).
X = 1.
h-tt = pv-hand:default-buffer-handle.
h-tt:empty-temp-table().

/* create query */
if Valid-handle(h-QrY) then h-QrY:QUERY-CLOSE().
if Valid-handle(h-QrY) then delete object h-QrY no-error.
create query h-QrY.
if Valid-handle(h-QrY) then h-QrY:Add-buffer(h-db).

/* deal with multiple tables in query */
if num-entries(zen-fldlook.tablename,'{&Delim2}') > 1  then do:
   do x = 2 to num-entries(zen-fldlook.tablename,'{&Delim2}'):
       create buffer h-db2 for table entry(x,zen-fldlook.tablename,'{&Delim2}').
       if Valid-handle(h-QrY) then h-QrY:Add-buffer(h-db2). 
   /*     lv-prepstring = lv-prepstring + "EACH " + entry(x,pv-table,'{&Delim2}')  */
   /*                     + ' ' + entry(x,pv-Where,'{&Delim2}') + " no-lock, ".    */
   end.
   x = 1.
end.

/* build were clause */
lv-where =  zen-fldlook.whereclause.                 
if lv-where <> "" then
do x = 1 to num-entries(zen-fldlook.wherefield):
    assign lv-rep-pos = "#" + string(x)
           lv-rep-value = entry(x,zen-fldlook.wherefield).
           lv-rep-value = "'" + dynamic-function(entry(1,lv-rep-value,'{&Delim2}'),
                                                 entry(2,lv-rep-value,'{&Delim2}')) + "'".
           lv-where     = replace(lv-where,lv-rep-pos,LV-REP-VALUE).
end.
lv-select = lv-where + ' and ' .

if lv-searchField1 ne ''
then assign lv-where = lv-where + ' and ' + lv-searchField1 + ' >= "' + lv-searchvalue1 + '"'.
/*             lv-by    = ' by ' + lv-searchField1.  */
if lv-searchField2 ne '' 
then do:
   lv-where2 = ' and if ' + lv-searchField1 + ' begins "' + lv-searchvalue1 + '"' +
               ' then ' + lv-searchField2 + ' >= "' + lv-searchvalue2 + '"' +
               ' else ' + lv-searchField2 + ' >= ""'.  /*********   BEGINS OR >= ????????? ***********/
/*    lv-by2    =  ' by ' + lv-searchField2. */
end.

lv-by    = zen-fldlook.byclause /* + ' ' + lv-by + lv-by2 */ .
lv-query = "for each " + lv-mastertable + " " +
                     lv-where + lv-where2 + " no-lock " + lv-by /*+ lv-by2*/
                      + " max-rows " + string(3 * lv-maxcount) .

if not h-QRY:QUERY-PREPARE(lv-query)
then do:
    ErrorCreate(50,'Lookup Query',lv-query,'','').
    return 'failed'.
end.

h-QRY:QUERY-OPEN.
h-buff = h-qry:get-buffer-handle(1).
h-qry:get-first().

/* if not session:remote                                    */
/*  then message 'in ' program-name(1) skip                 */
/*       'id ' pv-lastid skip                               */
/*       'max ' lv-maxcount skip                            */
/*       'using ' lv-query skip                             */
/*       'found ' h-qry:num-results skip                    */
/*       'for ' lv-searchfield1 ' : ' lv-searchvalue1 skip  */
/*       'and ' lv-searchfield2 ' : ' lv-searchvalue2 skip  */
/*       'index ' h-qry:index-information                   */
/*       'offend ' H-QRY:QUERY-OFF-END.                     */

if PV-LASTID ne ? then do: /* reposition to last record found  */
    lv-where = 'where ' + h-buff:name + 'tableid = ' + pv-lastid.
    h-buff:find-first(lv-where,no-lock).
    h-qry:reposition-to-rowid(h-buff:rowid) no-error.
   if error-status:error then do:
       ErrorCreate(999,'Reposition Failed',lv-mastertable,lv-where,lv-query).
       return 'failed'.
   end.
   if pv-direction = 'down' 
       then h-QRY:GET-NEXT().
       else H-QRY:GET-PREV().
end.
else if h-qry:num-results > 0 then do: /* get first good record */
 /* change "smith" (for example) to "smitgzzzzzz" */
   def var working-name as char no-undo.
   if lv-searchvalue2 ne "" 
      then working-name = lv-searchvalue1.
/* substring(lv-searchvalue1,1,length(lv-searchvalue1) - 1) +
                          chr(asc(substring(lv-searchvalue1,length(lv-searchvalue1),1)) - 1) + 
                          "zzzzzzzzzz".
   lv-where = lv-searchField1 + ' > "' + working-name + '"'.
*/
   lv-where = lv-searchField1 + ' >= "' + working-name + '"'.
   if lv-searchvalue2 ne "" /* searching on last and first names */
   then do:
      lv-gotone = h-buff:find-first(lv-select + lv-where,no-lock) no-error.
      assign
         working-name = getfieldhandle(h-buff,lv-searchfield1):buffer-value
         lv-where  = lv-searchField1 + ' begins "' + working-name + '"'
         lv-where2 = ' and ' + lv-searchField2 + ' begins "' + lv-searchvalue2 + '"' .
      h-buff:find-first(lv-select + lv-where + lv-where2,no-lock) no-error.
   end. 

   /* user searching on (all or part of) last name only, or searches using first name have failed */
   if (lv-searchvalue2 = "" and lv-searchvalue1 ne "") or not lv-gotone 
   then do:
      lv-where  = lv-searchField1 + ' ge "' + lv-searchvalue1 + '"'.
      lv-gotone = h-buff:find-first(lv-select + lv-where,no-lock) no-error.
   end.
   h-qry:reposition-to-rowid(h-buff:rowid) no-error.  
end.

if h-qry:num-results = 0 then do:
       ErrorCreate(999,'No Records Found',' ',' ','').
       return 'failed'.
end.

/* and add new records to return table */
if not H-QRY:QUERY-OFF-END then
do X = 1 to LV-MAXCOUNT:
    if pv-direction = 'down' 
        then h-QRY:GET-NEXT() no-error.
        else H-QRY:GET-PREV() no-error.
    if H-QRY:QUERY-OFF-END 
        then leave.

    h-tt:buffer-create().
    h-tt:buffer-copy(h-db).
    y = 1.
    do y = 1 to h-tt:num-fields:
        h-field = h-tt:Buffer-field(y).
        if h-field:name = lv-mastertable + 'tableid'
        then do:
            h-field:buffer-value = GetFieldValue(h-db,h-field:name,0).
            next.
        end.
        for each zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname 
                                 and zen-lookupfld.fieldname = h-field:name 
                                 and zen-lookupfld.lfield
                               no-lock:
            h-field:buffer-value = GetFieldValue(h-db,h-field:name,zen-lookupfld.extentnum).
        end.
     end.                                                
end.

lv-off-end = h-qry:query-off-end.
if Valid-handle(h-QrY) then    
    delete object h-QrY no-error.
if lv-off-end 
      then return .
      else return 'more'.


end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Open-Query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Open-Query Procedure 
PROCEDURE Open-Query :
def input  param pv-searchvalue as char no-undo.
def input  param pv-lookupid    as dec  no-undo.
def input  param pv-lastid      as char no-undo.
def input  param pv-direction   as char no-undo.
def output param table-handle pv-hand.

def var lv-eqexpr as char no-undo init ' >= '.
def var lv-searchfield1 as char no-undo init ''.
def var lv-searchfield2 as char no-undo init ''.
def var lv-searchvalue1 as char no-undo init ''.
def var lv-searchvalue2 as char no-undo init ''.
def var lv-delim        as char no-undo init ' '.
def var b-data          as handle no-undo.
def var lv-where        as char   no-undo.
def var lv-select       as char  no-undo.
def var lv-maxcount     as int   no-undo.
def var lv-field        as char   no-undo.
def var lv-by           as char   no-undo.
def var lv-keyfield     as char no-undo.
def var h-buff          as handle no-undo.
def var h-field         as handle no-undo.
def var h-fld           as handle no-undo.
def var lv-off-end      as log no-undo.
def var X               as int no-undo.
def var y               as int no-undo.
def var z               as int no-undo.
def var cnt             as int no-undo init 1.
def var lv-goto         as char no-undo.
def var lv-rep-value    as char no-undo.
def var lv-where2       as char no-undo.
def var lv-query        as char no-undo.
def var lv-by2          as char no-undo.
def var lv-gotone       as log  no-undo.
def var lv-rep-pos      as char no-undo.
def var lv-qry          as char   no-undo.
def var lv-label        as char   no-undo.
def var lv-format       as char   no-undo.
def var h-tt            as handle no-undo.
def var h-db            as handle no-undo.
def var h-qry           as handle no-undo.
def var lv-z as char no-undo.
def var lv-ok as log no-undo.

if opsys = 'unix' then do:
   pv-searchvalue = replace(pv-searchvalue,'\\','').
   pv-searchvalue = replace(pv-searchvalue,'"','~"').
   pv-searchvalue = replace(pv-searchvalue,"'","~'").
end.

if pv-direction = 'up' 
   then assign lv-eqexpr = ' <= ' lv-z = 'z'.
   else assign lv-eqexpr = ' >= ' lv-z = ''.


lv-delim = GetCtrl('LookupDelim').
/* needs to use sysopt.aoptn[10] as delimiter field */

find first zen-fldlook where zen-fldlook.zen-fldlooktableid = pv-lookupid 
                       no-lock no-error.
if not avail zen-fldlook then do: 
   ErrorCreate(50,'zen-fldlook',string(pv-lookupid),pv-searchvalue,string(pv-lastid)).
   return 'failed'.
end.

if not can-find(first zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname)
then do:
   pv-hand = ?.
   return.
end.

/* reinitialise maxlistcount as it is reset at bottom if we are at end of
   dataset
 lv-maxlistcount = zen-fldlook.dataguess.*/
cnt = 1.
if pv-lastid = '-1' then 
    assign pv-lastid = ?
           lv-maxcount = 999999.
else lv-maxcount = lv-maxlistcount.

/* define dynamic temp table for results */
create buffer h-db for table zen-fldlook.tablename.
create temp-table pv-hand.
pv-hand:add-like-field(zen-fldlook.tablename + 'tableid',zen-fldlook.tablename + '.' + zen-fldlook.tablename + 'tableid').
for each zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname no-lock by order:
    do y = 1 to h-db:num-fields:
        h-field = h-db:Buffer-field(y).
        if h-field:name = zen-lookupfld.fieldname 
        then do:  
           if zen-lookupfld.SearchField then do:
               if lv-searchfield1 = '' then lv-searchField1 = h-field:name.
               else if lv-searchfield2 = '' then lv-searchfield2 = h-field:name.
           end.
           if zen-lookupfld.Keyfield and 
              lv-keyfield = '' 
           then lv-keyField = h-field:name.
            if zen-lookupfld.lfield then do:
                assign
                    lv-format = if zen-lookupfld.FieldFormat = '' 
                                then h-field:format
                                else zen-lookupfld.FieldFormat
                    lv-label = if zen-lookupfld.FieldLabel = ''
                                then  h-field:label
                                else zen-lookupfld.FieldLabel.
                pv-hand:ADD-NEW-FIELD(zen-lookupfld.fieldname,
                              h-field:data-type,0,
                              lv-format,'',
                              lv-label,lv-label). 
            end.
        end.
    end.
end.

if lv-searchfield2 ne '' and
   num-entries(pv-searchvalue,lv-delim) > 1 
then do:
    lv-searchvalue1 = entry(1,pv-searchvalue,lv-delim).
    do x = 2 to num-entries(pv-searchvalue,lv-delim):
        lv-searchvalue2 = lv-searchvalue2 + entry(x,pv-searchvalue,lv-delim) + ' '.
    end.
    lv-searchvalue2 = trim(lv-searchvalue2).
    if error-status:error then do:
        ErrorCreate(50,'Searchvalue2','Invalid Substring',' ',' ').
    end.
end.
else do:
    lv-searchvalue1 = pv-searchvalue.
end.

pv-hand:temp-table-prepare("t-" + zen-fldlook.tablename).
X = 1.
h-tt = pv-hand:default-buffer-handle.
h-tt:empty-temp-table().

/* create query */
if Valid-handle(h-QrY) then h-QrY:QUERY-CLOSE().
if Valid-handle(h-QrY) then delete object h-QrY no-error.
create query h-QrY.
h-QrY:Add-buffer(h-db).

/* build were clause */
lv-where =  zen-fldlook.whereclause.                 
if lv-where <> "" then
do x = 1 to num-entries(zen-fldlook.wherefield):
    assign lv-rep-pos = "#" + string(x)
           lv-rep-value = entry(x,zen-fldlook.wherefield).
           lv-rep-value = "'" + dynamic-function(entry(1,lv-rep-value,'{&Delim2}'),
                                                 entry(2,lv-rep-value,'{&Delim2}')) + "'".
           lv-where     = replace(lv-where,lv-rep-pos,LV-REP-VALUE).
end.
lv-select = lv-where + ' and ' .

if lv-searchField1 ne ''
then assign lv-where = lv-where + ' and ' + lv-searchField1 + lv-eqexpr + '"' + lv-searchvalue1 + '"'.

if lv-searchField2 ne '' 
then do:
   lv-where2 = ' and if ' + lv-searchField1 + ' = "' + lv-searchvalue1 + '"' +
               ' then ' + lv-searchField2 + lv-eqexpr + '"' + lv-searchvalue2 + '"' +
               ' else ' + lv-searchField2 + ' >= ""'. 
end.

lv-by    = zen-fldlook.byclause /* + ' ' + lv-by + lv-by2 */ .
lv-query = "for each " + zen-fldlook.tablename + " " +
                     lv-where + lv-where2 + " no-lock " + lv-by + " INDEXED-REPOSITION " /*+ lv-by2
                      + " INDEXED-REPOSITION " + " max-rows " + string(2 * lv-maxcount) */ .

if lv-query = '' or lv-query = ? then do:
    ErrorCreate(50,'Lookup Query','Unknown value or blank',' ',' ').
    return 'failed'.
end.

lv-ok = h-QRY:QUERY-PREPARE(lv-query) no-error. 
if not lv-ok
then do:
    ErrorCreate(50,'Lookup Query',lv-query,'','').
    return 'failed'.
end.

h-QRY:QUERY-OPEN.
h-buff = h-qry:get-buffer-handle(1).

if PV-LASTID ne ? then do: /* reposition to last record found  */
    lv-where = 'where ' + h-buff:name + 'tableid = ' + pv-lastid.
    h-buff:find-first(lv-where,no-lock) no-error.
   if error-status:error then do:
       ErrorCreate(999,'find by tableid Failed',zen-fldlook.tablename,lv-where,'').
       return 'failed'.
   end.

   h-qry:reposition-to-rowid(h-buff:rowid) no-error.
   if error-status:error then do:
       ErrorCreate(999,'Reposition Failed',zen-fldlook.tablename,lv-where,lv-query).
       return 'failed'.
   end.
   if pv-direction = 'down'
       then do:
        h-QRY:GET-NEXT().
        h-QRY:GET-NEXT().
       end.
     else H-QRY:GET-PREV().
end.
else do: /* get first good record */
 /* change "smith" (for example) to "smitgzzzzzz" */

   def var working-name as char no-undo.
   if lv-searchvalue2 ne "" 
      then working-name = lv-searchvalue1.
/*
substring(lv-searchvalue1,1,length(lv-searchvalue1) - 1) +
                          chr(asc(substring(lv-searchvalue1,length(lv-searchvalue1),1)) - 1) + 
                          "zzzzzzzzzz".

   lv-where = lv-searchField1 + ' > "' + working-name + '"'.
*/

   lv-where = lv-searchField1 + ' >= "' + working-name + '"'.
   if lv-searchvalue2 ne "" /* searching on last and first names */
   then do:     
      lv-gotone = h-buff:find-first(lv-select + lv-where,no-lock) no-error. 

      assign
         working-name = getfieldhandle(h-buff,lv-searchfield1):buffer-value no-error.
         /*         lv-where  = lv-searchField1 + ' begins "' + working-name + '"'
         lv-where2 = ' and ' + lv-searchField2 + ' begins "' + lv-searchvalue2 + '"' .

         */
         lv-where  = lv-searchField1 + ' >= "' + working-name + '"'.
         lv-where2 = ' and ' + lv-searchField2 + ' >= "' + lv-searchvalue2 + '"' .
      lv-gotone = h-buff:find-first(lv-select + lv-where + lv-where2,no-lock) no-error.
      if not lv-gotone 
      then do: lv-gotone = h-buff:find-first(lv-select + lv-where,no-lock) no-error.
      end.
   end. 

   /* user searching on (all or part of) last name only, 
      or searches using first name have failed */
   if (lv-searchvalue2 = "" and lv-searchvalue1 ne "") or not lv-gotone 
   then do:
      lv-where  = lv-searchField1 + ' ge "' + lv-searchvalue1 + '"'.
      lv-gotone = h-buff:find-first(lv-select + lv-where,no-lock) no-error. 
   end.
   
   h-qry:reposition-to-rowid(h-buff:rowid) no-error.

   if pv-direction = 'down'
        then h-QRY:GET-first().
        else H-QRY:GET-PREV().

   if H-QRY:QUERY-OFF-END then do:
    h-qry:get-first().
   end.
end.

/* erik uncomment this for message */
/* if not session:remote */
/*  then message 'in ' program-name(1) skip */
/*       'id ' pv-lastid skip */
/*       'max ' lv-maxcount skip */
/*       'direction ' pv-direction skip */
/*       'eqexpr ' lv-eqexpr     skip */
/*       'sysprac ' lv-select skip */
/*       'Query ' lv-query skip */
/*       'Found ' h-qry:num-results skip */
/*       'for ' lv-searchfield1 ' : ' lv-searchvalue1 skip */
/*       'and ' lv-searchfield2 ' : ' lv-searchvalue2 skip */
/*       'Prepare ' h-qry:PREPARE-STRING skip */
/*       'index ' h-qry:index-information skip */
/*       'offend ' H-QRY:QUERY-OFF-END. */
/* server side logging */
/* output stream bug to 'debug.log' append. */
/* put stream bug unformatted */
/* 'in ' program-name(1) skip */
/*       'id ' pv-lastid skip */
/*       'max ' lv-maxcount skip */
/*       'direction ' pv-direction skip */
/*        'zen where'  zen-fldlook.whereclause skip */
/*        'real sys ' getsysvar('gs-sys-cd') skip */
/*        'realprac ' getsysvar('practice') skip */
/*        'gs-sys-cd ' getsysvar('gs-sys-cd') skip */
/*        'fnzip' gsopt('fn-zip') skip */ 
/*       'eqexpr ' lv-eqexpr     skip */
/*       'sysprac ' lv-select skip */
/*       'Query ' lv-query skip */
/*       'Found ' h-qry:num-results skip */
/*       'for ' lv-searchfield1 ' : ' lv-searchvalue1 skip */
/*       'and ' lv-searchfield2 ' : ' lv-searchvalue2 skip */
/*       'Prepare ' h-qry:PREPARE-STRING skip */
/*       'index ' h-qry:index-information skip */
/*       'offend ' H-QRY:QUERY-OFF-END skip */
/* . */
/* output stream bug close. */


if h-qry:num-results = 0 then do:
/*        ErrorCreate(999,'No Records Found',' ',' ','').  */
       return 'failed'.
end.
        
/* and add new records to return table */
/* IF not H-QRY:QUERY-OFF-END THEN */
do X = 1 to LV-MAXCOUNT:

/*     if pv-direction = 'down' */
/*         then h-QRY:GET-NEXT(). */
/*         ELSE H-QRY:GET-PREV(). */
    if H-QRY:QUERY-OFF-END 
        then leave.
    h-tt:buffer-create().
    h-tt:buffer-copy(h-db).
    if pv-direction = 'down'
        then h-QRY:GET-NEXT().
        else H-QRY:GET-PREV().
end.
/* debug dump records to file */
/*  numrecords('dumpserver',pv-hand). */


lv-off-end = h-qry:query-off-end.
if Valid-handle(h-QrY) then    
    delete object h-QrY no-error.
if lv-off-end 
/*    and pv-direction ne 'up'  */
   and pV-LASTID ne ? 
      then return .
      else return 'more'.


end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query-desc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query-desc Procedure 
PROCEDURE open-query-desc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-searchvalue as char no-undo.
def input  param pv-lookupid    as dec  no-undo.
def input  param pv-lastid      as char no-undo.
def input  param pv-direction   as char no-undo.
def output param table-handle pv-ReturnTable.

find first zen-fldlook where zen-fldlook.zen-fldlooktableid = pv-lookupid 
                       no-lock no-error.

if not can-find(first zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname)
then do:
   pv-ReturnTable = ?.
   return.
end.
/* message pv-direction zen-fldlook.byclause.  */
/* if index(zen-fldlook.byclause,'desc') ne 0               */
/* then do:                                                 */
/*    if pv-direction = 'down' then pv-direction = 'up'.    */
/*                             else pv-direction = 'down'.  */
/* end.                                                     */
/* message pv-direction.  */
def var lv-searchfield1 as char  no-undo init ''.
def var lv-searchfield2 as char  no-undo init ''.
def var lv-searchvalue1 as char  no-undo init ''.
def var lv-searchvalue2 as char  no-undo init ''.
def var lv-searchtype1  as char  no-undo.
def var lv-searchtype2  as char  no-undo.
def var lv-delim        as char  no-undo init ' '.
def var lv-where        as char  no-undo.
def var lv-select       as char  no-undo.
def var lv-maxcount     as int   no-undo.
def var lv-field        as char  no-undo.
def var lv-by           as char  no-undo.
/* def var lv-keyfield as char no-undo.  */
def var h-DBbuffer      as handle no-undo.
def var h-field         as handle no-undo.
def var lv-off-end      as log   no-undo.
def var X               as int   no-undo.
def var y               as int   no-undo.
def var lv-rep-value    as char  no-undo.
def var lv-rep-pos      as char  no-undo.
def var lv-label        as char  no-undo.
def var lv-format       as char  no-undo.
def var H-ReturnTable   as handle no-undo.
def var H-DbTable       as handle no-undo.
def var H-DbQuery       as handle no-undo.
def var lv-where2       as char  no-undo.
def var lv-query        as char  no-undo.
def var lv-by2          as char  no-undo.
def var lv-gotone       as log   no-undo.
def var lv-index        as char  no-undo.
def var lv-gele as char no-undo.

lv-delim = GetCtrl('LookupDelim').

if opsys = 'unix' then do:
   pv-searchvalue = replace(pv-searchvalue,'\\','').
   pv-searchvalue = replace(pv-searchvalue,'"','~"').
   pv-searchvalue = replace(pv-searchvalue,"'","~'").
end.
lv-searchvalue1 = pv-searchvalue.

/* quick fix to allow single field lookups to work as they used to
really need to pput field on table and then change everything to use
 open-query and set below if multifield is allowed

lv-searchvalue1 = entry(1,pv-searchvalue,lv-delim).
if num-entries(pv-searchvalue,lv-delim) > 1 then
    lv-searchvalue2 = Substring(pv-searchvalue,Index(pv-searchvalue,entry(2,pv-searchvalue,lv-delim))).
*/

if pv-lastid = '-1' then 
    assign pv-lastid = ?
           lv-maxcount = 999999.
else lv-maxcount = lv-maxlistcount.

create buffer H-DbTable for table zen-fldlook.tablename.
create temp-table pv-ReturnTable.
/* add the fields we need to the return table */
pv-ReturnTable:add-like-field(zen-fldlook.tablename + 'tableid',zen-fldlook.tablename + '.' + zen-fldlook.tablename + 'tableid').
for each zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname no-lock by order:
    do y = 1 to H-DbTable:num-fields:
        h-field = H-DbTable:Buffer-field(y).
        if h-field:name = zen-lookupfld.fieldname 
        then do:  
           if zen-lookupfld.SearchField then do:
               if lv-searchfield1 = '' then assign lv-searchField1 = h-field:name lv-searchtype1 = h-field:data-type.
               else if lv-searchfield2 = '' then assign lv-searchfield2 = h-field:name lv-searchtype2 = h-field:data-type.
           end.
/*            if zen-lookupfld.Keyfield and     */
/*               lv-keyfield = ''               */
/*            then lv-keyField = h-field:name.  */
            if zen-lookupfld.lfield then do:
                 lv-format = if zen-lookupfld.FieldFormat = '' 
                             then h-field:format
                             else zen-lookupfld.FieldFormat no-error.
               if error-status:error then do:
                   ErrorCreate(999,'Invalid Format',zen-fldlook.lookupname,zen-lookupfld.fieldname,zen-lookupfld.FieldFormat).
                   return 'failed'.
               end.
                 lv-label = if zen-lookupfld.FieldLabel = ''
                             then  h-field:label
                             else zen-lookupfld.FieldLabel no-error.
               if error-status:error then do:
                   ErrorCreate(999,'Invalid Field label',zen-fldlook.lookupname,zen-lookupfld.fieldname,zen-lookupfld.FieldLabel).
                   return 'failed'.
               end.
                pv-ReturnTable:ADD-NEW-FIELD(zen-lookupfld.fieldname,
                              h-field:data-type,0,
                              lv-format,'',
                              lv-label,lv-label) no-error. 
               if error-status:error then do:
                   ErrorCreate(999,'Add Field Failed',zen-fldlook.lookupname,zen-lookupfld.fieldname,h-field:data-type).
                   return 'failed'.
               end.
            end.
        end.
    end.
end.
pv-ReturnTable:temp-table-prepare("t-" + zen-fldlook.tablename).
X = 1.
H-ReturnTable = pv-ReturnTable:default-buffer-handle.
H-ReturnTable:empty-temp-table().

/* set up the query */
if Valid-handle(H-DbQuery) then H-DbQuery:QUERY-CLOSE().
if Valid-handle(H-DbQuery) then delete object H-DbQuery no-error.
create query H-DbQuery.
H-DbQuery:Add-buffer(H-DbTable).
lv-where = zen-fldlook.whereclause.   
              
if lv-where <> "" then do:
   /* replace "#" arguments with quoted strings */
   do x = 1 to num-entries(zen-fldlook.wherefield):
       assign lv-rep-pos = "#" + string(x)
              lv-rep-value = entry(x,zen-fldlook.wherefield).
              lv-rep-value = "'" + dynamic-function(entry(1,lv-rep-value,'{&Delim2}'),
                                                    entry(2,lv-rep-value,'{&Delim2}')) + "'".
              lv-where     = replace(lv-where,lv-rep-pos,LV-REP-VALUE).
   end.

/* message lv-where. */
   /* replace "@" arguments with non-quoted values (for integers, etc.) */
   do x = 1 to num-entries(zen-fldlook.wherefield):
       assign lv-rep-pos = "@" + string(x)
              lv-rep-value = entry(x,zen-fldlook.wherefield).
              lv-rep-value = dynamic-function(entry(1,lv-rep-value,'{&Delim2}'),
                                              entry(2,lv-rep-value,'{&Delim2}')).
              lv-where     = replace(lv-where,lv-rep-pos,LV-REP-VALUE).
   end.
end. /* a "where" clause was entered */
lv-select = lv-where.
if PV-LASTID ne ? then lv-gele = ' <= '.
                  else lv-gele = ' >= '.
if lv-searchField1 ne ''
then do:
   if lv-searchtype1 = 'character' then lv-searchtype1 = 'string'.
   if index(lv-where,'#s') ne 0 
      then lv-where = replace(lv-where,'#s'," and " + lv-searchField1 + lv-gele + lv-searchtype1 + "('" + lv-searchvalue1 + "')").
      else lv-where = lv-where + ' and ' + lv-searchField1 + lv-gele + lv-searchtype1 + "('" + lv-searchvalue1 + "')".
   if index(zen-fldlook.byclause,lv-searchfield1) = 0 
      then lv-by    = 'by ' + lv-searchField1.
      else lv-by = ''.
end.
if lv-searchField2 ne '' 
then do:
   if lv-searchtype2 = 'character' then lv-searchtype2 = 'begins string'.
   else lv-searchtype2 = ' >= ' + lv-searchtype2.

   assign lv-where2 = ' and ' + lv-searchField2 + lv-searchtype2 + "('" + lv-searchvalue2 + "')"
            lv-by2  =  'by ' + lv-searchField2.
end.


if index(zen-fldlook.byclause,lv-by2) = 0 then 
lv-by    = zen-fldlook.byclause + ' ' + lv-by + ' ' + lv-by2.
/* need to reposition to first record where 1 and 2  
   then get all the following records  */
lv-query = "for each " + zen-fldlook.tablename + " " +
                     lv-where + " no-lock " + lv-by + 
                     " max-rows " + string(lv-maxcount + 50).

/* message program-name(1) skip lv-query view-as alert-box.  */
if not H-DbQuery:QUERY-PREPARE(lv-query)
then do:
    ErrorCreate(50,'Lookup Query',lv-query,' ',' ').
    return 'failed'.
end.

H-DbQuery:QUERY-OPEN.
/* if not session:remote */
/*  then message 'in ' program-name(1) skip */
/*       'using ' lv-query skip */
/*       'found ' H-DbQuery:num-results skip */
/*       'for ' lv-searchfield1 ' : ' lv-searchvalue1 ' : ' lv-searchtype1 skip */
/*       'and ' lv-searchfield2 ' : ' lv-searchvalue2 ' : ' lv-searchtype2 skip */
/*       'by  ' lv-by skip */
/*       'index ' H-DbQuery:index-information skip */
/*       'offend ' H-DbQuery:QUERY-OFF-END skip */
/*       'lastid ' pv-lastid. */                     

h-DBbuffer = H-DbQuery:get-buffer-handle(1).
/* reposition query so we get the next set of records */
if PV-LASTID ne ? then do:
    lv-where = 'where ' + h-DBbuffer:name + 'tableid = ' + pv-lastid.
    h-DBbuffer:find-first(lv-where,no-lock).
    H-DbQuery:reposition-to-rowid(h-DBbuffer:rowid) no-error.

   if error-status:error then do:
       ErrorCreate(999,'Reposition Failed',zen-fldlook.tablename,lv-where,lv-query).
       return 'failed'.
   end.
/*    if pv-direction = 'down'        */
/*        then H-DbQuery:GET-NEXT().  */
/*        ELSE H-DbQuery:GET-PREV().  */
end.
else do:
/*    if H-DbQuery:index-information ne ''                                                          */
/*    then lv-index = ' use-index ' + H-DbQuery:index-information.                                  */
/*    if lv-searchField1 ne ''                                                                      */
/*    then lv-where = lv-select + ' and ' + lv-searchField1 + ' begins "' + lv-searchvalue1 + '"'.  */
/*    lv-gotone = h-DBbuffer:find-first(lv-where + lv-where2 + lv-index,no-lock) no-error.          */
/*    if not lv-gotone then do:                                                                     */
/*       lv-where2 = ' and ' + lv-searchField2 + ' < "' + lv-searchvalue2 + '"'.                    */
/*       lv-gotone = h-DBbuffer:find-first(lv-where + lv-where2 + lv-index,no-lock) no-error.       */
/*       if not lv-gotone then do:                                                                  */
/*          lv-where = lv-where + ' and ' + lv-searchField1 + ' < "' + lv-searchvalue1 + '"'.       */
/*          h-DBbuffer:find-last(lv-where + lv-index,no-lock) no-error.                             */
/*       end.                                                                                       */
/*    end.                                                                                          */
/*    H-DbQuery:reposition-to-rowid(h-DBbuffer:rowid) no-error.                                     */
/*    H-DbQuery:GET-next().  */
end.
x = 1.
if not H-DbQuery:QUERY-OFF-END then
do X = 1 to LV-MAXCOUNT:
    if pv-direction = 'down'   
        then H-DbQuery:GET-NEXT().
        else H-DbQuery:GET-PREV().
    if H-DbQuery:QUERY-OFF-END 
        then leave.
    H-ReturnTable:buffer-create().
    H-ReturnTable:buffer-copy(H-DbTable).
/*     y = 1.                                                                                                             */
/*     do y = 1 to H-ReturnTable:num-fields:                                                                              */
/*         h-field = H-ReturnTable:Buffer-field(y).                                                                       */
/*         if h-field:name = zen-fldlook.tablename + 'tableid'                                                            */
/*         then do:                                                                                                       */
/*             h-field:buffer-value = GetFieldValue(H-DbTable,h-field:name,0) no-error.                                   */
/*             if error-status:error then do:                                                                             */
/*                 ErrorCreate(999,'Data Copy Failed',zen-fldlook.lookupname,h-field:name,string(h-field:buffer-value)).  */
/*             end.                                                                                                       */
/*             next.                                                                                                      */
/*         end.                                                                                                           */
/*         for each zen-lookupfld where zen-lookupfld.lookupname = zen-fldlook.lookupname                                 */
/*                                  and zen-lookupfld.fieldname = h-field:name                                            */
/*                                  and zen-lookupfld.lfield                                                              */
/*                                no-lock:                                                                                */
/*             h-field:buffer-value = GetFieldValue(H-DbTable,h-field:name,zen-lookupfld.extentnum) no-error.             */
/*             if error-status:error then do:                                                                             */
/*                 ErrorCreate(999,'Data Copy Failed',zen-fldlook.lookupname,h-field:name,string(h-field:buffer-value)).  */
/*             end.                                                                                                       */
/*         end.                                                                                                           */
/*      end.                                                                                                              */
end.
lv-off-end = H-DbQuery:query-off-end.
if Valid-handle(H-DbQuery) then    
    delete object H-DbQuery no-error.

if lv-off-end 
      then return .
      else return 'more'.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-AddTableFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddTableFields Procedure 
FUNCTION AddTableFields returns character
  ( pv-lookupname as char,
    pv-db as handle,
    pv-table as handle) :

def var y         as int    no-undo.
def var h-field   as handle no-undo.
def var lv-search as char no-undo.
def var lv-fname as char no-undo.
def var lv-format as char no-undo.
def var lv-label  as char no-undo.


/* add the fields */
for each zen-lookupfld where zen-lookupfld.lookupname = pv-lookupname no-lock by order:
    do y = 1 to pv-db:num-fields:
        h-field = pv-db:Buffer-field(y).
        if zen-lookupfld.keyField then lv-search = h-field:name.
        if h-field:name = zen-lookupfld.fieldname and
            zen-lookupfld.ifield then do:
            assign
                lv-format = if zen-lookupfld.FieldFormat = '' 
                            then h-field:format
                            else zen-lookupfld.FieldFormat
                lv-label = if zen-lookupfld.FieldLabel = ''
                            then  h-field:label
                            else zen-lookupfld.FieldLabel.
            if zen-lookupfld.extentnum = 0 
            then lv-fname = zen-lookupfld.fieldname.
            else lv-fname = zen-lookupfld.fieldname + 'x' + string(zen-lookupfld.extentnum).
            pv-table:ADD-NEW-FIELD(lv-fname,
                          h-field:data-type,0,
                          lv-format,'',
                          lv-label,lv-label). 
            leave.
        end.
    end.
end.

  return lv-search.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFieldHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldHandle Procedure 
FUNCTION GetFieldHandle returns handle
  ( pv-buffer as handle,
    pv-fname as char ) :
    def var X       as int    no-undo.
    def var h-field as handle no-undo.

    do x = 1 to pv-buffer:num-fields:
        h-field = pv-buffer:Buffer-field(X).
        if h-field:name = pv-fname then return h-field.
    end.
    return ?.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetString Procedure 
FUNCTION SetString returns character
  ( pv-str as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  return pv-str.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

