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
&glob serverprogram true
/* ***************************  Definitions  ************************** */
{app-paths.i}



/* changeable bits here change table-name to appropriate DB table */
&glob table-name zen-auditfield
&glob unique-key {&table-name}tableid 
{{&core}def-table.i &table-name = {&table-name}}
define query q-{&table-name} for {&table-name} scrolling.

&glob OverrideMaxListCount 100

define temp-table w-field no-undo
field fname like zen-auditfield.fieldname 
field description like  zen-auditfield.description 
field dtype like zen-auditfield.datatype.

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
         HEIGHT             = 20.14
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

&IF DEFINED(EXCLUDE-AutoPopulate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoPopulate Procedure 
procedure AutoPopulate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-table as char no-undo.

def var x as int no-undo.
find zen-auditconfig where zen-auditconfig.tablename = pv-table
                     no-lock no-error.
for each w-field:
    delete w-field.
end.                     
do x = 1 to num-dbs:
    create alias dictdb for database value(ldbname(x)).
    run GetFields(zen-auditconfig.tablename).
end.

for each w-field:
    create zen-auditfield.
    assign 
        zen-auditfield.zen-auditconfigtableid = zen-auditconfig.zen-auditconfigtableid
        zen-auditfield.tablename = zen-auditconfig.tablename
        zen-auditfield.fieldname = w-field.fname
        zen-auditfield.description = w-field.description
        zen-auditfield.datatype = w-field.dtype
        zen-auditfield.Active = index(w-field.fname,'tableid') = 0 
        zen-auditfield.sysrecord = true.   
end.                     
 

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clear-table) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-table Procedure 
procedure clear-table :
for each t-{&table-name}:
    delete t-{&table-name}.
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-record Procedure 
procedure delete-record :
   /* USES MANDATORY PARAMETERS
def input param pv-tableid as dec no-undo.
*/
   {{&core}apsrv-deleterecord.i}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Delete-Related-Tables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete-Related-Tables Procedure 
procedure Delete-Related-Tables :
/*------------------------------------------------------------------------------
  Purpose:     delete any child tables of this parent
  Parameters:  <none>
  Notes:       leave blank if not required    
------------------------------------------------------------------------------*/
/* EXAMPLE
   find prod-matrix of product exclusive-lock no-error.
   if avail prod-matrix 
    then do:
       {{&core}runlocal.i &program   = "PROD-MATRIX.p"       
                         &path      = "{&core}{&srv}"
                         &procedure = "DELETE-RECORD"             
                         &params    = "(PROD-MATRIC.PROD-MATRIXTABLEID)"} 
    end.
   for each prod-curr of product exclusive-lock:
       {{&core}runlocal.i &program   = "PROD-CURR.p"       
                         &path      = "{&core}{&srv}"
                         &procedure = "DELETE-RECORD"             
                         &params    = "(PROD-CURR.PROD-CURRTABLEID)"} 
    end.
   */
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-validation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-validation Procedure 
procedure delete-validation :
/*------------------------------------------------------------------------------
  Purpose:     check ok to delete record
  Parameters:  <none>
  Notes:       return 'passed' if it's OK to delete.
------------------------------------------------------------------------------*/
/* example 
   if can-find(first ledger-item of product) then return 'failed'.
*/
    return 'passed'.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-find-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-record Procedure 
procedure find-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-code as char no-undo.
   {{&core}apsrv-findrecord.i 
        &where = "where true"}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-records) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-records Procedure 
procedure get-records :
   /* USES MANDATORY PARAMETERS YOU NEED NOT WORRY ONLY 
def input param pv-direction as char no-undo.
def input  param pv-rowid     as recid no-undo.
def output param table        for  t-{&table-name}.
   can use &whereclause as where clause and &by for sorting
   e.g. &whereclause = "where {&table-name}.class = pv-class"       
        &by          = "by {&table-name}.name"                                                   
********************************************************/

def input param pv-tid as char no-undo.
   {{&core}apsrv-getrecords.i 
&whereclause = " where zen-auditfield.tablename = pv-tid" }

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFields Procedure 
procedure GetFields :
def input param pv-file as char no-undo.
   def var h-qry   as handle no-undo.
   def var b-data  as handle no-undo.
   def var b-field as handle no-undo.   
   def var h-buff  as handle no-undo.
   def var h-field as handle no-undo.
   def var lv-where as char no-undo.
   def var h-dbfield as handle no-undo.
   def var lv-sysrec as log no-undo.
   if Valid-handle(h-QrY) then h-QrY:QUERY-CLOSE().
   if Valid-handle(h-QrY) then delete object h-QrY no-error.
   create query h-QrY.

   create buffer b-data for table 'dictdb._file'.
   create buffer b-field for table 'dictdb._field'.

   if Valid-handle(h-QrY) 
   then do: 
    h-QrY:Add-buffer(b-data).   
    h-QrY:Add-buffer(b-field).   
   end.
   lv-where = "FOR EACH " + "_file where " + 
         "_file._file-name = '" + 
         pv-file + "' no-lock, each " +
         "_field of " + "_file no-lock".
         
   h-QRY:QUERY-PREPARE(lv-where).
   h-QRY:QUERY-OPEN.
   h-QRY:GET-NEXT().
   do while not h-qry:query-off-end:   
      create w-field.
      assign
         w-field.fname = b-field:Buffer-field('_field-name'):buffer-value
         w-field.description = b-field:Buffer-field('_desc'):buffer-value
         w-field.dtype = b-field:Buffer-field('_data-type'):buffer-value.
      h-QRY:GET-NEXT().
   end.
   if Valid-handle(h-QrY) then  
     delete object h-QrY no-error.   
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-key-assign) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE key-assign Procedure 
procedure key-assign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find _file where _file-name = Zen-Auditfield.TableName no-lock no-error.
if not avail _file then return.
find _field of _file where _field-name = zen-Auditfield.FieldName no-lock no-error.
if not avail _field then return.
Zen-Auditfield.datatype = _field._data-type.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Procedure 
procedure open-query :
   /* USES MANDATORY PARAMETERS
      output table for t-{&table-name}.
      returns ALL records matching the &where clause
   can use &whereclause as where clause and &by for sorting
      e.g.  &where = "where {&table-name}.class = pv-class"       
        &by          = "by {&table-name}.name"   
********************************************************/

   {{&core}apsrv-openquery.i}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pop-combo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pop-combo Procedure 
procedure pop-combo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param pv-codes as char no-undo.
def output param pv-values as char no-undo.
/*
def var lv-list as char no-undo.
lv-list =  BuildCombo(?,
                      "{&table-name}",
                      "keyfield",
                      "descriptionfield",
                      "whereclause",
                      "byclause",
                      Noneflag,
                      AllFlag).
assign
    pv-codes  = entry(1,lv-list,'{&Delim3}')
    pv-values = entry(2,lv-list,'{&Delim3}').
*/    
/* or use old style
{{&core}apsrvbldcombo.i &table = "{&Table-name}"
                       &field = "broker.Name"
                       &key   = "{&Table-name}.code"}
*/
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create Procedure 
procedure post-create :
/*------------------------------------------------------------------------------
  Purpose:     just a hook incase we need it
  Parameters:  <none>
  Notes:       called after record create before buffer copy.
------------------------------------------------------------------------------*/
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-update) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update Procedure 
procedure post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       called after buffer copy for create or update      
------------------------------------------------------------------------------*/
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pre-Create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pre-Create Procedure 
procedure Pre-Create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       run before record create
------------------------------------------------------------------------------*/
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pre-Save) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pre-Save Procedure 
procedure Pre-Save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       run as first thing in appsrv-saverecord.i      
------------------------------------------------------------------------------*/
   return "passed".
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-save-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE save-record Procedure 
procedure save-record :
   /* USES MANDATORY PARAMETERS
def input param pv-recid as recid no-undo. 
def input-output param table for t-{&table-name}.
*/
   {{&core}apsrv-saverecord.i}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

