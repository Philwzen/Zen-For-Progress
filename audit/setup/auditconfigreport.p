/*          /* we never audit the following so dont bother checking */         */
/*         if index(_field-name,'tableid') ne 0 then next.                     */
/*         if can-do('dentered,dchanged,lchgdt,sys-cd,practice,acct-no,pt-num' */
/*                     ,_field-name) then next.                                */

def stream op.
def var lv-ok as log no-undo.
message 'include count of existing audit records? '
view-as alert-box question buttons yes-no update lv-ok.

output stream op to 'auditconfigreport.csv'.
put stream op unformatted 'Database,Table Name,Field Name,Status,Count' skip.

def var x as int no-undo.

do x = 1 to num-dbs:
    create alias dictdb for database value(ldbname(x)).
    run db-qry(ldbname(x)).
end.

output stream op close.

procedure db-qry:
    def input param pv-db as char no-undo.
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
        run checkaudit(pv-db,
                       b-data:buffer-field('_file-name'):buffer-value,
                       b-data:recid).
        h-QRY:GET-NEXT().
    end.
    If Valid-handle(h-QrY) Then  
        DELETE OBJECT h-QrY no-error.   
end procedure.

procedure checkaudit:
    def input param pv-db as char no-undo.
    def input param pv-table as char no-undo.
    def input param pv-recid as recid no-undo.
    def var lv-fieldname as char no-undo.
    Def Var h-qry   As Handle No-undo.
    Def Var b-data  As Handle No-undo.
    Def Var h-buff  As Handle No-undo.
    Def Var h-field As Handle No-undo.
    def var lv-where as char no-undo.
    def var lv-mess as char no-undo.
    def var cnt as int no-undo.
    If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().
    If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
    Create Query h-QrY.
    lv-where = pv-db + '._field'.
    Create Buffer b-data For Table lv-where.
    If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data).   
    
    lv-where = "FOR EACH " + pv-db + "._field where " + 
            pv-db + "._field._file-recid = " + string(pv-recid) +
            " no-lock".

    find Zen-AuditConfig where Zen-AuditConfig.TableName = pv-table
                         no-lock no-error.
    if not avail Zen-AuditConfig 
    then do: /* not setup */
      put stream op unformatted pv-db ',' pv-table ',All,Not Setup' skip.
      next.
    end.
    if not zen-auditconfig.active 
    then do: /* not Active */
      put stream op unformatted pv-db ',' pv-table ',All,Not Active' skip.
      next.
    end.

    h-QRY:QUERY-PREPARE(lv-where).
    h-QRY:QUERY-OPEN.
    h-QRY:GET-NEXT().
    do while not h-qry:query-off-end:
        lv-fieldname = string(b-data:buffer-field('_field-name'):buffer-value).
        If not can-find(FIRST Zen-Auditfield 
                          Where zen-auditfield.tablename = zen-auditconfig.tablename
                            and zen-auditfield.fieldname = lv-fieldname)
        then lv-mess = 'Not Audited'.
        else lv-mess = 'Audited'.
        if lv-ok then do:
           cnt = 0.
           for each zen-auditdetail where zen-auditdetail.tablename = pv-table
                                    no-lock,
               each zen-auditline of zen-auditdetail
                                  where zen-auditline.fieldname = lv-fieldname
                                  no-lock:
              cnt = cnt + 1.
           end.
        end.
        put stream op unformatted pv-db ',' pv-table ',' lv-fieldname ',' lv-mess ',' cnt skip.
        h-qry:get-next().
    end.
    If Valid-handle(h-QrY) Then  
        DELETE OBJECT h-QrY no-error.   
end procedure.


