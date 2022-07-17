/*  fix any bad tableid values and update appropriate sequences */

def stream op.
output stream op to 'fixtableids.log'.

def var lv-ok as log no-undo.
pause 0 before-hide.

def var x as int no-undo.

do x = 1 to num-dbs:
   create alias dictdb for database value(ldbname(x)).
   message 'Run for DB ' ldbname(x) '?'
   view-as alert-box question buttons yes-no update lv-ok.
   if lv-ok then do:
      lv-ok = no.
      message 'Update Values?'
      view-as alert-box buttons yes-no update lv-ok.
      run db-qry(ldbname(x),lv-ok).
   end.
end.

output stream op close.

PROCEDURE db-qry :
    def input param pv-db as char no-undo.
    def input param pv-forreal as log no-undo.
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
        run table-qry(h-field:buffer-value,pv-forreal).
        h-QRY:GET-NEXT().
    end.
    If Valid-handle(h-QrY) Then  
        DELETE OBJECT h-QrY no-error.   

END PROCEDURE.

PROCEDURE table-qry :
    def input param pv-table as char no-undo.
    def input param pv-forreal as log no-undo.
    put stream op unformatted 'Table ' pv-table.
    Def Var h-qry   As Handle No-undo.
    Def Var b-data  As Handle No-undo.
    Def Var h-buff  As Handle No-undo.
    Def Var h-field As Handle No-undo.
    def var x       as int    no-undo.
    def var y       as int    no-undo.
    If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().
    If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
    Create Query h-QrY.
    Create Buffer b-data For Table pv-table.
    If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data). 
    b-data:disable-load-triggers(false).

    h-QRY:QUERY-PREPARE("FOR EACH dictdb." + pv-table + 
                           ' where ' + pv-table + 'tableid ne ? no-lock by ' + pv-table + 'tableid ').
    h-QRY:QUERY-OPEN.
    h-qry:get-last().
    if not h-qry:query-off-end then do:
/*         h-field = b-data:buffer-field(pv-table + 'tableid').  */
/*         y = h-field:buffer-value.                             */
        put stream op unformatted ' Last id was ' 
            dynamic-current-value ( "next-" + pv-table + "tableid","dictdb").
        if y = ? then y = 1.
        /*** do transaction: ***/
        DO:
            h-QRY:QUERY-PREPARE("FOR EACH dictdb." + pv-table + ' where ' + pv-table + 'tableid = ? share-lock').
            h-QRY:QUERY-OPEN.
            h-QRY:GET-first().
            do while not ( h-qry:query-off-end )
            transaction :
                y = y + 1.
                h-field = b-data:buffer-field(pv-table + 'tableid').
                /* comment out lines below to NOT change anything */
               if pv-forreal then 
                h-field:buffer-value =
                  dynamic-next-value("next-" + pv-table + "tableid","dictdb").
                b-data:buffer-release().
                h-QRY:GET-NEXT().
            end.
            put stream op unformatted  ' Last id Now ' dynamic-current-value ( "next-" + pv-table + "tableid","dictdb")
                                       ' Fixed ' y ' Records' skip.
        end. /* transblk: DO: */
    end.
    else put stream op unformatted  ' Empty'.
    If Valid-handle(h-QrY) Then    
        DELETE OBJECT h-QrY no-error.  
    put stream op skip.

END PROCEDURE.
