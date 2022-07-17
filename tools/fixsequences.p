{app-paths.i}
def var x as int no-undo.
DEF VAR LV-OK AS LOG NO-UNDO.
Message 'Fix Bad Values?' 
view-as alert-box buttons yes-no update lv-ok.

def stream op.
output stream op to 'checksequences.log'.
pause 0 before-hide.

do x = 1 to num-dbs:
    create alias dictdb for database value(ldbname(x)).
    run db-qry(ldbname(x)).
end.

output stream op close.

procedure db-qry:
    def input param pv-db as char no-undo.
    put stream op unformatted '##Processing DB ' pv-db skip.
    Def Var h-qry   As Handle No-undo.
    Def Var b-data  As Handle No-undo.
    Def Var h-buff  As Handle No-undo.
    Def Var h-field As Handle No-undo.
    def var lv-where as char no-undo.
    If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().
    If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
    Create Query h-QrY.
    lv-where = pv-db + '._file'.
    Create Buffer b-data For Table lv-where.
    If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data).   
    
    lv-where = "FOR EACH " + pv-db + "._file where " + 
            pv-db + "._file._file-number > 0 and " + 
            pv-db + "._file._file-number < 32000 no-lock".

    h-QRY:QUERY-PREPARE(lv-where).
    h-QRY:QUERY-OPEN.
    h-QRY:GET-NEXT().
    do while not h-qry:query-off-end:
        run table-qry(pv-db,
                      b-data:buffer-field('_file-name'):buffer-value).
        h-QRY:GET-NEXT().
    end.
    If Valid-handle(h-QrY) Then  
        DELETE OBJECT h-QrY no-error.   
end procedure.


procedure table-qry:
   def input param pv-db as char no-undo.
   def input param pv-table as char no-undo.
   def var x as int no-undo.
   def var y as int no-undo.
  
   run getlasttABLEID(pv-table ,output x).

      y = dynamic-current-VALUE('next-' + pv-table  + 'tableid',pv-db).
      if y = ? then y = 0.
      if y < x 
      then DO:
        put stream op unformatted 
           pv-db ' ' pv-table  ' ' x ' next-' + pv-table  + 'tableid ' y ' *** Bad Value '.
           if lv-ok then do:
                dynamic-current-value('next-' + pv-table  + 'tableid',pv-db) = x no-error.
                put stream op unformatted 'Has Been Reset Value to ' dynamic-current-value('next-' + pv-table  + 'tableid',pv-db).
           end.
      end.
   put stream op unformatted skip.

END.


procedure getlasttableid:
   def input param pv-table as char no-undo.
   def output param y       as int    no-undo.

   Def Var h-qry   As Handle No-undo.
   Def Var b-data  As Handle No-undo.
   def var h-field as handle no-undo.
   If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().
   If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
   Create Query h-QrY.
   Create Buffer b-data For Table pv-table.
   If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data). 
   
   h-QRY:QUERY-PREPARE('FOR EACH ' + pv-table + 
                       ' where ' + pv-table + 'tableid ne ? no-lock use-index tableid ').
   h-QRY:QUERY-OPEN.
   h-QRY:GET-LAST().
  
   if not h-qry:query-off-end then do:
      h-field = b-data:buffer-field(pv-table + 'tableid').
      y = h-field:buffer-value.
      if y = ? then y = 0.
   end.

   If Valid-handle(h-QrY) Then    
           DELETE OBJECT h-QrY.  
end procedure.
