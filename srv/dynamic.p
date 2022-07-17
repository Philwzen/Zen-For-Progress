&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&glob serverprogram true
{app-paths.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}mainp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Dyn-BuildCombo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dyn-BuildCombo Procedure 
PROCEDURE Dyn-BuildCombo :
def input param pv-table as char no-undo.
def input param pv-key   as char no-undo.
def input param pv-field as char no-undo.
def input param pv-where as char no-undo.
def input param pv-by    as char no-undo.
def input param pv-none  as log  no-undo.
def input param pv-wild  as log  no-undo.
def output param pv-codes  as char no-undo.
def output param pv-values as char no-undo.

{{&core}bldcombo.i} /* so we have same functionality here
                         as in cachelibrary cachecombo */
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Dyn-GetFieldWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dyn-GetFieldWhere Procedure 
PROCEDURE Dyn-GetFieldWhere :
def input  param pv-table     as char no-undo.
def input  param pv-where     as char no-undo. 
def input  param pv-datafield as char no-undo.
def output param pv-data      as char no-undo.

{{&core}getfieldwhere.i}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillDataset Procedure 
PROCEDURE FillDataset :
def input param pv-page as int no-undo.
def input param pv-tables as char no-undo.
def input param pv-where as char no-undo.
def input param pv-relation as char no-undo.
def input param pv-fieldlist as char no-undo.
def input param pv-lookupname as char no-undo.
def output param DATASET-HANDLE pv-dataset.

/* example  
    ("zen-auditconfig,zen-auditfield",
     "where tablename = 'zen-dpgm'",
     "tablename,tablename",
     "!tstamp,!*tableid,*;!tstamp,!*tableid,*",
     OUTPUT DATASET-HANDLE h-dataset)
*/
def var lv-ok as log no-undo.
def var x as int no-undo.
def var y as int no-undo.
def var h-db            as handle no-undo.
def var h-field         as handle no-undo.
def var lv-fieldlist as char no-undo.
def var h-datasource as handle no-undo.
def var h-buffer as handle no-undo.
def var lv-idxname as char no-undo.
def var h-qry as handle no-undo.
def var h-tt     as handle extent 18 no-undo.
def var lv-batchsize as int64 no-undo init 25.
def var lv-rowid as rowid no-undo.
def var lv-restartrowid as rowid no-undo.
def var lv-idx as char no-undo.
def var lv-rep-pos as char no-undo.
def var lv-rep-value as char no-undo.
def var lv-where as char no-undo.    

if pv-page = ? 
    then assign lv-batchsize = 99999999999.
    else lv-batchsize = int(getctrl('maxlistcount')).

create dataset pv-dataset.

do x = 1 to NUM-ENTRIES(pv-tables):
    if pv-fieldlist = '*' or pv-fieldlist = '' 
    then lv-fieldlist = '!tstamp,*'.
    else lv-fieldlist = entry(x,pv-fieldlist,';').

    create buffer h-db for table ENTRY(x, pv-tables).
    create temp-table h-tt[x].
    
    h-tt[x]:create-like(ENTRY(x, pv-tables)). 
    
    h-tt[x]:TEMP-TABLE-PREPARE("t-" + ENTRY(x, pv-tables)).
    h-tt[x]:schema-marshal = 'FULL'.
    pv-dataset:ADD-BUFFER(h-tt[x]).
    if x > 1 and pv-relation ne ''
    then pv-dataset:ADD-RELATION(pv-dataset:GET-BUFFER-HANDLE(x - 1),
                                 pv-dataset:GET-BUFFER-HANDLE(x),
                                 pv-relation).
    lv-where = lv-where + ENTRY(x,pv-tables) + " " + ENTRY(x,pv-where) + ', each '.
end.             
/* if x > 1 then pv-dataset:GET-BUFFER-HANDLE(1):fill-mode = 'no-fill'. */

lv-where = "FOR EACH " + substring(lv-where,1,length(lv-where) - 7).
        
create query h-qry.
create data-source h-datasource.        
do x = 1 to NUM-ENTRIES(pv-tables):
    create buffer h-buffer for table ENTRY(x, pv-tables).
    h-datasource:ADD-SOURCE-BUFFER(h-buffer, ENTRY(x,pv-tables) + 'tableid').
    pv-dataset:GET-BUFFER-HANDLE(x):ATTACH-DATA-SOURCE(h-datasource).
    if x = 1 then 
        pv-dataset:GET-BUFFER-HANDLE(x):batch-size = lv-batchsize.
    h-qry:ADD-BUFFER(h-buffer). 
end.  

h-qry:QUERY-PREPARE(lv-where).
h-datasource:QUERY = h-qry.

if pv-page = ? or pv-page = 0 
then do:
    h-qry:query-open().
    h-qry:get-first().
    setsysvar("UpRestartRowid",string(h-buffer:rowid)).
    h-datasource:restart-rowid = ?.
end.
else tranche: do:
    if pv-page > 0 then do:
        lv-restartrowid = to-rowid(getsysvar("DownRestartRowid")).
        h-qry:query-open().   
        h-datasource:restart-rowid = lv-restartrowid.
    end.
    else do: /* yuck nasty nasty is there a better way ??? !!*/
        lv-restartrowid = to-rowid(getsysvar("UpRestartRowid")).
        lv-idx = h-qry:index-information.
        h-qry:query-close().
        x = 1.
        find zen-fldlook where zen-fldlook.lookupname = pv-lookupname no-lock.
        lv-where = zen-fldlook.whereclause.
        do x = 1 to num-entries(zen-fldlook.wherefield):
             assign lv-rep-pos = "#" + string(x)
                    lv-rep-value = entry(x,zen-fldlook.wherefield).
                    lv-rep-value = "'" + dynamic-function(entry(1,lv-rep-value,'{&Delim2}'),
                                                          entry(2,lv-rep-value,'{&Delim2}')) + "'".
                    lv-where     = replace(lv-where,lv-rep-pos,LV-REP-VALUE).
        end.
        h-qry:QUERY-PREPARE("FOR EACH " + ENTRY(1, pv-tables) + " " + lv-where + " use-index " + lv-idx).
        h-datasource:QUERY = h-qry.
        h-qry:query-open().
        y = 0.
        lv-rowid = lv-restartrowid.
        h-qry:reposition-to-rowid(lv-restartrowid).  
        h-qry:get-prev(no-lock) no-error.        
        do while not h-qry:query-off-end:
            y = y + 1.            
            lv-rowid = h-buffer:rowid .
            if y = lv-batchsize then leave.
            h-qry:get-prev(no-lock) no-error.        
        end. 
        if lv-rowid = lv-restartrowid or y = 0
        then pv-dataset:GET-BUFFER-HANDLE(1):batch-size = 0.
        else do:
/*             if y < lv-batchsize then do: */
/*                 h-qry:query-open(). */
/*                 h-qry:reposition-to-rowid(lv-rowid). */
/*             end. */
            h-datasource:restart-rowid = lv-rowid no-error.
            pv-dataset:GET-BUFFER-HANDLE(1):batch-size = y no-error.
        end.
    end.
end.

/* message  'Table  : ' pv-tables skip */
/*          'Passed in Where: ' pv-where skip */
/*          'up where : ' lv-where skip */
/*          'Relations: ' pv-relation skip */
/*          'Server prepare : ' h-qry:prepare-string skip */
/*          'Server Index   : ' h-qry:index-information skip */
/*          'page    : ' pv-page skip */
/*          'results : ' h-qry:num-results skip */
/*          'y : ' y skip */
/* /*          'acct : ' getfieldvalue(h-buffer,'acct-no',0) skip */ */
/*          'batchsize : ' pv-dataset:GET-BUFFER-HANDLE(1):batch-size skip */
/*          {&dbt}. */

if pv-dataset:GET-BUFFER-HANDLE(1):batch-size > 0 
then do:
    h-qry:get-first().
    do y = 1 to lv-batchsize:
        do x = 1 to h-qry:num-buffers:
            pv-dataset:GET-BUFFER-HANDLE(x):buffer-create().
            pv-dataset:GET-BUFFER-HANDLE(x):buffer-copy(h-qry:GET-BUFFER-HANDLE(x)).
        end.
        h-qry:get-next().
        if h-qry:query-off-end then leave.
        end.
  /*  pv-dataset:FILL(). */

    if pv-page < 0 
    then setsysvar("UpRestartRowid",string(lv-rowid)).
    else setsysvar("DownRestartRowid",string(h-datasource:next-rowid)).
end.

delete object h-qry no-error.
  
do x = 1 to pv-dataset:NUM-BUFFERS:
    h-buffer = pv-dataset:GET-BUFFER-HANDLE(x).
    delete object h-buffer:DATA-SOURCE no-error.
end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillDataSetByPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillDataSetByPage Procedure 
PROCEDURE FillDataSetByPage :
def input param pv-page as int no-undo.
def input param pv-tables as char no-undo.
def input param pv-where as char no-undo.
def input param pv-relation as char no-undo.
def input param pv-fieldlist as char no-undo.
def output param DATASET-HANDLE pv-dataset.

/* example  
    ("zen-auditconfig,zen-auditfield",
     "where tablename = 'zen-dpgm'",
     "tablename,tablename",
     "!tstamp,!*tableid,*;!tstamp,!*tableid,*",
     OUTPUT DATASET-HANDLE h-dataset)
*/
def var x as int no-undo.
def var y as int no-undo.
def var h-db            as handle no-undo.
def var h-field         as handle no-undo.
def var lv-fieldlist as char no-undo.
def var h-datasource as handle no-undo.
def var h-buffer as handle no-undo.
def var h-qry as handle no-undo.
def var h-tt     as handle extent 18 no-undo.
def var lv-batchsize as int64 no-undo init 25.

if pv-page = ? 
    then assign lv-batchsize = 99999999999
                pv-page = 1.
    else lv-batchsize = int(getctrl('maxlistcount')).

create dataset pv-dataset.

do x = 1 to NUM-ENTRIES(pv-tables):
    if pv-fieldlist = '*' or pv-fieldlist = '' 
    then lv-fieldlist = '!tstamp,*'.
    else lv-fieldlist = entry(x,pv-fieldlist,';').

    create buffer h-db for table ENTRY(x, pv-tables).
    create temp-table h-tt[x].
    /* h-tt[x]:create-like(ENTRY(x, pv-tables)). */
    do y = 1 to h-db:num-fields:
        h-field = h-db:Buffer-field(y).        
        if can-do(lv-fieldlist,h-field:name)
        then  h-tt[x]:ADD-LIKE-FIELD(h-field:name,h-field).
    end.
    h-tt[x]:TEMP-TABLE-PREPARE("t-" + ENTRY(x, pv-tables)).
    h-tt[x]:schema-marshal = 'full'.
    pv-dataset:ADD-BUFFER(h-tt[x]:DEFAULT-BUFFER-HANDLE).

    if x > 1
    then
    pv-dataset:ADD-RELATION(pv-dataset:GET-BUFFER-HANDLE(x - 1),
                           pv-dataset:GET-BUFFER-HANDLE(x),
                           ENTRY(x - 1,pv-relation,';')).
end.             
        
do x = 1 to NUM-ENTRIES(pv-tables):
    create data-source h-datasource.    
    create buffer h-buffer for table ENTRY(x, pv-tables).
    h-datasource:ADD-SOURCE-BUFFER(h-buffer, ENTRY(x,pv-relation)).
    pv-dataset:GET-BUFFER-HANDLE(x):ATTACH-DATA-SOURCE(h-datasource).
    if x = 1 
    then do:
        pv-dataset:GET-BUFFER-HANDLE(x):batch-size = lv-batchsize.
        create query h-qry.
        h-qry:ADD-BUFFER(h-buffer).
        h-qry:QUERY-PREPARE("FOR EACH " + ENTRY(1, pv-tables) +
            " " + ENTRY(1,pv-where)).
        h-datasource:QUERY = h-qry.
        if pv-page < 1
        then h-datasource:restart-row = ((pv-page - 1 ) * lv-batchsize) - 1.
        else h-datasource:restart-row = 1 + (pv-page - 1 ) * lv-batchsize .
    end. 
end. 

pv-dataset:FILL().
delete object h-qry.
  
do x = 1 to pv-dataset:NUM-BUFFERS:
    h-buffer = pv-dataset:GET-BUFFER-HANDLE(x).
    delete object h-buffer:DATA-SOURCE.
end.



end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FindFullSearchValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindFullSearchValues Procedure 
PROCEDURE FindFullSearchValues :
def input        param tableName    as char  no-undo.
def input        param searchField1 as char  no-undo.
def input        param searchField2 as char  no-undo.
def input        param initialWhere as char  no-undo.
def input        param indexToUse   as char  no-undo.
def input-output param searchValue1 as char  no-undo.
def input-output param searchValue2 as char  no-undo.

   def var hBuffer      as handle     no-undo.
   /* indicates if we found a valid record to use in the lookups, necessary
      for no-error option when using find-last method */
   def var recordFound  as log    no-undo.


        /* create the buffer for the query */
   create buffer hBuffer for table tableName.

/*    /* hardcoded for now, cannot use "by" phrases */ */
/*    indexToUse = if tableName = "acct"    then "lname"   else */
/*                 if tableName = "acctrel" then "arelnam" else */
/*                                               indexToUse. */

   /* try finding a record that begins with the two search values (optimal) */
   recordFound = hBuffer:find-first(
      initialWhere +
      (if trim(initialWhere) ne "where" then " and " else " ") +
      searchField1 + " begins " + quoter(searchValue1) + " and " +
      searchField2 + " begins " + quoter(searchValue2) +
      (if indexToUse ne "" then " use-index " + indexToUse else ""),
      no-lock) no-error.

   /* try finding what would be the next record if the users record actually
      existed within the primary search criteria (searchValue1) */
   if not recordFound then recordFound = hBuffer:find-first(
      initialWhere +
      (if trim(initialWhere) ne "where" then " and " else " ") +
      searchField1 + " begins " + quoter(searchValue1) + " and " +
      searchField2 + " > "      + quoter(searchValue2) +
      (if indexToUse ne "" then " use-index " + indexToUse else ""),
      no-lock) no-error.

   /* try finding the very first record after the primary search criteria */
   if not recordFound then recordFound = hBuffer:find-first(
      initialWhere +
      (if trim(initialWhere) ne "where" then " and " else " ") +
      searchField1 + " > " + quoter(searchValue1) +
      (if indexToUse ne "" then " use-index " + indexToUse else ""),
      no-lock) no-error.

   /* Should only get here if we were trying to find a record beyond the
      very last one in the table.  So, just find the very last one
      (note: still need to use search criteria to make sure the correct
       index is used, hopefully) */
   if not recordFound then recordFound = hBuffer:find-last(
      initialWhere +
      (if trim(initialWhere) ne "where" then " and " else " ") +
      searchField1 + " < " + quoter(searchValue1) +
      (if indexToUse ne "" then " use-index " + indexToUse else ""),
      no-lock) no-error.

   /* if we have a result, then we need to return its value */
   if recordFound then assign
      searchValue1 = string(hBuffer:buffer-field(searchField1):buffer-value)
      searchValue2 = string(hBuffer:buffer-field(searchField2):buffer-value).

   /* make sure clean up happens */
   finally:
      delete object hBuffer.
   end finally.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FindStartRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindStartRecord Procedure 
PROCEDURE FindStartRecord :
def input param pv-table as char no-undo.
def input param pv-where as char no-undo.
def input param pv-s1 as char no-undo.
def input param pv-s2 as char no-undo.
def output param pv-value as char no-undo.
def output param pv-dtype as char no-undo.

def var lv-row as char no-undo.
def var lv-delim as char no-undo init ' '.
def var lv-ok as log no-undo.
lv-delim = ' '.

def var x as int no-undo.
def var h-buff  as handle no-undo.
def var h-field as handle no-undo.
pv-where = ' Where ' + pv-where.
create buffer h-buff for table pv-table.
lv-ok = h-buff:find-first(pv-where,no-lock) no-error.
if lv-ok then do:
    do x = 1 to h-buff:num-fields:
        h-field = h-buff:Buffer-field(X).
        if h-field:name = pv-s1 then do:
            pv-value = string(h-field:Buffer-value).
            pv-dtype = h-field:data-type.
            leave.
        end.
    end.
    if pv-s2 ne '' 
    then do x = 1 to h-buff:num-fields:
        h-field = h-buff:Buffer-field(X).
        if h-field:name = pv-s2 then do:
            pv-value = pv-value + lv-delim + string(h-field:Buffer-value).
            leave.
        end.
    end.
    setsysvar('LookupStartRowid',string(h-buff:rowid)).
end.
else setsysvar('LookupStartRowid','').

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAllRecords) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAllRecords Procedure 
PROCEDURE GetAllRecords :
def input  param pv-name      as char no-undo.
def input  param pv-where     as char no-undo.
def input-output param table-handle pv-hand.

def var lv-qry   as char   no-undo.
def var h-tt     as handle no-undo.
def var h-db     as handle no-undo.
def var h-qry    as handle no-undo.

create buffer h-db for table pv-name.
create buffer h-tt for table pv-hand:default-buffer-handle.
h-tt:empty-temp-table().

if Valid-handle(h-QrY) then h-QrY:QUERY-CLOSE().
if Valid-handle(h-QrY) then delete object h-QrY no-error.
create query h-QrY.
h-QrY:Add-buffer(h-db).

lv-qry = "For EACH " + pv-name + ' ' +
                 pv-where + ' ' +
                "no-lock".
h-QRY:QUERY-PREPARE(lv-qry).
h-QRY:QUERY-OPEN.
h-qry:get-first.

do while not h-qry:query-off-end:
    h-tt:buffer-create().
    h-tt:buffer-copy(h-db).
    h-qry:GET-NEXT().
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MassUpdate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MassUpdate Procedure 
PROCEDURE MassUpdate :
/* pv-name - name of db table
   pv-where - where clause to use
   pv-hand  - single record of changes
   pv-orig  - single blank record
*/
def input param pv-name  as char no-undo.
def input param pv-where as char no-undo.
def input param table-handle pv-hand.
def input param table-handle pv-orig.

def var h        as handle no-undo.
def var th       as handle no-undo.
def var oh       as handle no-undo.
def var lv-qry   as char   no-undo.
def var h-qry    as handle no-undo.
def var lv-updlist as char no-undo.
def var x as int no-undo.
def var lv-fname as char no-undo.
def var h-fieldt as handle no-undo.
def var h-fieldo as handle no-undo.
def var h-field as  handle no-undo.

create buffer h for table pv-name.
th = pv-hand:default-buffer-handle.
oh = pv-orig:default-buffer-handle no-error.

if getctrl("{&read-only}") = 'yes' then do:
    if not Systemmanager(getsysvar("user")) then do:
        ErrorCreate(1000,'','','','').
        return .    
    end.
end.

if not th:find-first() then do:
    ErrorCreate(50,pv-name,'changes','','').
    return .    
end.
if not oh:find-first()
then do:
    ErrorCreate(50,pv-name,'template','','').
    return.
end.

do x = 1 to th:num-fields:
    h-fieldt = th:Buffer-field(X).
    h-fieldo = oh:Buffer-field(h-fieldt:name).
    if h-fieldt:buffer-value ne h-fieldo:buffer-value
    then lv-updlist = lv-updlist + ',' + h-fieldt:name.
end.
lv-updlist = substring(lv-updlist,2).

if Valid-handle(h-QrY) then h-QrY:QUERY-CLOSE().
if Valid-handle(h-QrY) then delete object h-QrY no-error.
create query h-QrY.
h-QrY:Add-buffer(h).

lv-qry = "For EACH " + pv-name + ' ' +
                 pv-where + ' ' +
                "exclusive-lock".
do transaction:
    h-QRY:QUERY-PREPARE(lv-qry).
    h-QRY:QUERY-OPEN.
    h-qry:get-first.
    
    do while not h-qry:query-off-end:
        do x = 1 to num-entries(lv-updlist):
            lv-fname = entry(x,lv-updlist).
            h-fieldt = th:Buffer-field(lv-fname).
            h-field  = h:Buffer-field(lv-fname).
            h-field:buffer-value = h-fieldt:buffer-value.
        end.
        h:buffer-release.
        h-qry:GET-NEXT().
    end.
end. /* transaction */

if return-value begins 'write' then do:
    ErrorCreate(165,return-value,'','','').
    return.
end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-DeleteRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-DeleteRecord Procedure 
PROCEDURE Proc-DeleteRecord :
/*
 replace individual delete routines with one generic delete
*/
def input param pv-name as char no-undo.
def input param pv-id   as dec  no-undo.

def var h as handle no-undo.
def var lv-where as char no-undo.

if getctrl("{&read-only}") = 'yes' then do:
    if not Systemmanager(GetSysVar("user")) then do:
        ErrorCreate(10,'','','','').
        return .    
    end.
end.

create buffer h for table pv-name.

lv-where = 'where ' + pv-name + 'tableid = ' + string(pv-id).
if not h:find-first(lv-where,no-lock)
then do:
   ErrorCreate(50,'Table','{&table-name}','','').
   return .    
end.

/* check ok to delete record */
/* callapi() */
/* Run delete-validation In This-procedure (pv-name,pv-id) no-error.  */

if not error-status:error then
    if return-value ne 'passed' then do:
        ErrorCreate(8,'','','',''). 
        return .
    end.

do transaction:
 if h:find-first(lv-where,exclusive-lock)
    then do:
        /* delete any child tables */
/*         run delete-related-tables in this-procedure (pv-name,pv-id) no-error.  */
       
        &IF DEFINED(softdelete) NE 0 &THEN
            run soft-delete in THIS-PROCEDURE (pv-name,pv-id) no-error.
            h:buffer-RELEASE.
            if haderrors() then return.
        &ELSE
            h:buffer-delete().
        &endif
    end.
    else do:
        ErrorCreate(7,'Delete','Failed','','').
    end.
end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-InputFromFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-InputFromFile Procedure 
PROCEDURE Proc-InputFromFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-filename as char no-undo.
def output param pv-mfile as memptr no-undo.

   /* make sure there is nothing left over in the memory pointer! */
   set-size(pv-mfile) = 0.
pv-mfile = InputFromFile(pv-filename,'local').

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-OutputToFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-OutputToFile Procedure 
PROCEDURE Proc-OutputToFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input-output param pv-file as char no-undo.
def input param pv-mfile as memptr no-undo.

pv-file = OutPutToFile(pv-file,pv-mfile,'local').

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

