def stream op.

define temp-table t-data no-undo
 field t-db as char 
 field t-table as char
index order t-table.

def var x       as int  no-undo.

output stream op to 'auditcheck.log'.

do x = 1 to num-dbs:
/*     create alias dictdb for database value(ldbname(x)). */
    run gettablelocations(ldbname(x)).
end.

for each zen-auditconfig no-lock:

    find t-data where t-data.t-table = zen-auditconfig.tablename no-error.
    if not avail t-data then do:
        put stream op unformatted 'Table Not in Schema: ' zen-auditconfig.tablename skip.
        next.
    end.
    run checkconfig(t-data.t-db,
                   zen-auditconfig.tablename,
                   zen-auditconfig.keyfield,
                   zen-auditconfig.SearchFieldName).
end.
output stream op close.

procedure checkconfig:
   def input param pv-db as char no-undo.
   def input param pv-table as char no-undo.
   def input param pv-key as char no-undo.
   def input param pv-search as char no-undo.
   
   Def Var b-data  As Handle No-undo.
   def var b-field as handle no-undo.   
   def var lv-where as char no-undo.
   def var lv-ok as log no-undo.

   Create Buffer b-data For Table pv-db + '._file'.
   Create Buffer b-field For Table pv-db + '._field'.
   
   lv-where = "where " + pv-db + "._file._file-name = '" + pv-table + "'".
   lv-ok = b-data:find-first(lv-where,no-lock) no-error.
   if not lv-ok then do:
    put stream op unformatted "Invalid File: " pv-db '.' pv-table skip.
    return.
   end.
   lv-where = "where " + pv-db + "._field._file-recid = '" + string(b-data:recid) + 
             "' and " + pv-db + "._field._field-name = '" + pv-key + "'".
   lv-ok = b-field:find-first(lv-where,no-lock) no-error.
   
   if not lv-ok then put stream op unformatted "Invalid Key: " pv-key skip.
   
    do x = 1 to num-entries(pv-search,'|'):
        lv-where = "where " + pv-db + "._field._file-recid = '" + string(b-data:recid) + 
                  "' and " + pv-db + "._field._field-name = '" + entry(x,pv-search,'|') + "'".
        lv-ok = b-field:find-first(lv-where,no-lock) no-error.
        if not lv-ok
        then put stream op unformatted "Invalid Searchfield: " pv-table '.' entry(x,pv-search,'|') skip.
    end.
   
end procedure.


procedure gettablelocations:
   def input param pv-db as char no-undo.
   Def Var h-qry   As Handle No-undo.
   Def Var b-data  As Handle No-undo.
   def var b-field as handle no-undo.   
   Def Var h-buff  As Handle No-undo.
   Def Var h-field As Handle No-undo.
   def var lv-where as char no-undo.
   def var h-dbfield as handle no-undo.
   def var lv-sysrec as log no-undo.
   If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().
   If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
   Create Query h-QrY.
   lv-where = pv-db + '._file'.
   Create Buffer b-data For Table lv-where.
   Create Buffer b-field For Table pv-db + '._field'.

   If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data).   
   
   lv-where = "FOR EACH " + pv-db + "._file where " + 
         pv-db + "._file._file-number > 0 and " + 
         pv-db + "._file._file-number < 32000 no-lock".
   
   h-QRY:QUERY-PREPARE(lv-where).
   h-QRY:QUERY-OPEN.
   h-QRY:GET-NEXT().
   do while not h-qry:query-off-end:     
      lv-where = "where " + pv-db + "._field._file-recid = '" + string(b-data:recid) + 
                 "' and " + pv-db + "._field._field-name = 'sysrecord'".
      assign
         h-dbfield = b-data:Buffer-field('_file-name').
      create t-data.
      assign t-data.t-db = pv-db
             t-data.t-table = h-dbfield:buffer-value.
      h-QRY:GET-NEXT().
   end.
   If Valid-handle(h-QrY) Then  
     DELETE OBJECT h-QrY no-error.   
end procedure.
