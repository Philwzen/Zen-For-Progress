def var x       as int  no-undo.

do x = 1 to num-dbs:
    create alias dictdb for database value(ldbname(x)).
    run db-qry(ldbname(x)).
end.

procedure db-qry:
   def input param pv-db as char no-undo.
   Def Var h-tqry   As Handle No-undo.
   def var h-fqry  as handle no-undo.
   Def Var b-data  As Handle No-undo.
   def var b-field as handle no-undo.   
   Def Var h-buff  As Handle No-undo.
   Def Var h-field As Handle No-undo.
   
   def var lv-tablewhere as char no-undo.
   def var lv-fieldwhere as char no-undo.
   def var h-dbtable as handle no-undo.
   def var h-dbfield as handle no-undo.
   def var h-dbdatatype as handle no-undo.
   def var lv-ok as log no-undo.
def var y as int no-undo.
   If Valid-handle(h-tQrY) Then h-tQrY:QUERY-CLOSE().
   If Valid-handle(h-tQrY) Then DELETE OBJECT h-tQrY no-error.
   Create Query h-tQrY.

   Create Buffer b-data For Table pv-db + '._file'.
   Create Buffer b-field For Table pv-db + '._field'.

   If Valid-handle(h-tQrY) Then h-tQrY:Add-buffer(b-data).   
   
   lv-tablewhere = "FOR EACH " + pv-db + "._file where " + 
         pv-db + "._file._file-number > 0 and " + 
         pv-db + "._file._file-number < 32000 no-lock".
   
   lv-ok = h-tQRY:QUERY-PREPARE(lv-tablewhere) no-error.
   if not lv-ok then do:
    message '1 ' lv-tablewhere view-as alert-box.
    return.
   end.
   lv-ok = h-tQRY:QUERY-OPEN no-error.
   if not lv-ok then do:
    message '2 ' lv-tablewhere view-as alert-box.
    return.
   end.

   h-tQRY:GET-NEXT().
   do while not h-tqry:query-off-end:     
      h-dbtable = b-data:Buffer-field('_file-name').

      If Valid-handle(h-fQrY) Then h-fQrY:QUERY-CLOSE().
      If Valid-handle(h-fQrY) Then DELETE OBJECT h-fQrY no-error.
      Create Query h-fQrY.
      If Valid-handle(h-fQrY) Then h-fQrY:Add-buffer(b-field).
      lv-fieldwhere = "for each " + pv-db + "._field where " + pv-db + "._field._file-recid = " + string(b-data:recid)
                       + " and not (can-do('sys-cd,practice,logonid,dchanged,dentered,lchgdt,edate'," + pv-db + "._field._field-name))". 
      lv-ok = h-fQRY:QUERY-PREPARE(lv-fieldwhere) no-error.
      if not lv-ok then do:
        message '3 ' Error-Status:Get-Message(Error-Status:Num-Messages) skip lv-fieldwhere view-as alert-box.
        return.
      end.
      lv-ok = h-fQRY:QUERY-OPEN no-error.
      if not lv-ok then do:
        message '4 'lv-fieldwhere view-as alert-box.
        return.
      end.

      h-fQRY:GET-NEXT().
      do while not h-fqry:query-off-end:
         h-dbfield    = b-field:Buffer-field('_field-name').
         h-dbdatatype = b-field:Buffer-field('_data-type').
         find zen-auditfield where zen-auditfield.tablename = h-dbtable:buffer-value
                               and zen-auditfield.fieldname = h-dbfield:buffer-value
                             exclusive-lock
                             no-error.  
         if avail zen-auditfield
         then do:           
              zen-auditfield.datatype  = h-dbdatatype:buffer-value.
         end.
/*          else do: */
/*             message 'Not Found ' h-dbtable:buffer-value h-dbfield:buffer-value. */
/*             y = y + 1. */
/*          end. */
         
         h-fQRY:GET-NEXT().
      end.
      If Valid-handle(h-fQrY) Then  
        DELETE OBJECT h-fQrY no-error. 
      h-tQRY:GET-NEXT().
   end.
   If Valid-handle(h-tQrY) Then  
     DELETE OBJECT h-tQrY no-error.   
end procedure.
