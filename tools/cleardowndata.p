&glob db schadm
/* add to this list to NOT clear tabel down 
   should be any table that contain stuffthe users have 
   configured themselves klike default values functionkeys etc 
   unless of course its a brand new db then leave list empty and
   run fixuserids.p afterwards to populate zen-duser */
&glob IgnoreList zen-auditdetail,zen-auditline,zen-duser,zen-dfkey,zen-f-hotkey,zen-fielddefault

def stream op.
output stream op to 'Cleardown.log'.

def var lv-ok as log no-undo.
pause 0 before-hide.

def var x as int no-undo.
message 'Clear Down All the Data in {&db}?'
view-as alert-box question buttons yes-no update lv-ok.

if lv-ok then do:
   message 'Report Only (dont delete anything) ?'
   view-as alert-box question buttons yes-no update lv-ok.
   run db-qry('{&db}',lv-ok).
end.

output stream op close.

PROCEDURE db-qry :
    def input param pv-db as char no-undo.
    def input param pv-reponly as log no-undo.
    def var lv-ok as log no-undo.
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
         h-field = b-data:buffer-field('_file-name').

         if not can-do('{&IgnoreList}',h-field:buffer-value)
         then do:
            lv-ok = no.
            message 'Cleardown ' h-field:buffer-value ' ?'
            view-as alert-box question buttons yes-no update lv-ok.
            if not lv-ok then do: 
               put stream op unformatted  h-field:buffer-value ' Skipped' skip.
            end.
            else run table-qry(h-field:buffer-value,pv-reponly).
         end.
         else put stream op unformatted  h-field:buffer-value ' Ignored' skip.
         h-QRY:GET-NEXT().
    end.
    If Valid-handle(h-QrY) Then  
        DELETE OBJECT h-QrY no-error.   

END PROCEDURE.

PROCEDURE table-qry :
   DEF INPUT PARAM pv-table AS CHAR NO-UNDo.
   def input param pv-reponly as log no-undo.
   Def Var h-qry   As Handle No-undo.
   Def Var b-data  As Handle No-undo.
   Def Var h-buff  As Handle No-undo.
   
   put stream op unformatted 'Table ' pv-table.
   
   DEF VAR X AS int NO-UNDO.
   def var y as int no-undo.
   If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().
   If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
   Create Query h-QrY.
   Create Buffer b-data For Table pv-table.
   
   b-data:DISABLE-LOAD-TRIGGERS(NO).
   If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data). 
   DO TRANSACTION:
       h-QRY:QUERY-PREPARE("FOR EACH " + pv-table + ' exclusive-lock.').
       h-QRY:QUERY-OPEN.
       h-buff = h-qry:get-buffer-handle(1).
       h-QRY:GET-NEXT().
       X = 0.
       DO while not h-QRY:Query-off-end:
         X = X  + 1.
   /* comment this out to do nothing just report what would happen */
         if not pv-reponly 
         then do: 
            y = y + 1.
            h-buff:BUFFER-DELETE().
         end.
         h-QRY:GET-NEXT().
       end.
      put stream op unformatted  ' Found ' x ' Records. Deleted ' y skip.
   END.
   
   If Valid-handle(h-QrY) Then    
       DELETE OBJECT h-QrY no-error.  
       
END PROCEDURE.
