def var x as int  no-undo.
def var y as int no-undo.
def stream op.
def var lv-forreal as log no-undo init true.
def buffer b-detail for zen-auditdetail.
def buffer b-line for zen-auditline.


function deldetail returns log (pv-id as rowid):
    if lv-forreal then do:
        find b-detail where rowid(b-detail) = pv-id exclusive-lock.
        delete b-detail.    
    end.
    x = x + 1.
end.

function delline returns log (pv-id as rowid):
    if lv-forreal then do:
        find b-line where rowid(b-line) = pv-id exclusive-lock.
        delete b-line.
    end.
    y = y + 1.
end.
function AuditedTable returns log (pv-tablename as char):
    return can-find(first zen-auditconfig where zen-auditconfig.tablename = pv-tablename 
                                            and zen-auditconfig.Active).
end.
Function AuditedField Returns log (pv-tablename as char , pv-fieldname as char):
    return can-find(first zen-auditfield where zen-auditfield.tablename = pv-tablename 
                                           and zen-auditfield.fieldname = pv-fieldname   
                                           and zen-auditfield.Active).
end.

output stream op to 'cleannonauditedtables.log'.
/* setup files:
   zen-auditconfig (for files)  is parent of zen-auditfield (for fields)  */
/* actual audit data:
   zen-auditdetail (file level) is parent of zen-auditline  (field level) */

etime(true).
put stream op unformatted 'Processing Unuadited Tables' skip.

for each zen-auditdetail no-lock:
    if not AuditedTable(zen-auditdetail.tablename)
    then do: /* not supposed to be audited at all */
        for each zen-auditline of zen-auditdetail no-lock:  
            delline(rowid(zen-auditline)).
        end.
        if lv-forreal then do:
            deldetail(rowid(zen-auditdetail)). 
        end.
    end. 
    else do: /* check its fields that are audited */
        for each zen-auditline of zen-auditdetail no-lock:
            /* make sure this field should be audited */
            if not AuditedField(zen-auditdetail.tablename,zen-auditline.fieldname)
            then delline(rowid(zen-auditline)).
        end. 
        if zen-auditdetail.AuditAction = 'Update' /* remove childless update events */
           and not can-find (first zen-auditline of zen-auditdetail)
        then deldetail(rowid(zen-auditdetail)). 
    end.
end. /* for each zen-auditdetail */
put stream op unformatted 'Removed a total of ' x ' Detail records with ' y ' lines' skip.
put stream op unformatted 'Done in ' string(etime / 1000) ' Seconds' skip.
etime(true).

y = 0.
x = 0.
put stream op unformatted 'Processing Orphaned Lines' skip.
/* delete orphanned line records */
for each zen-auditline no-lock:
    if not can-find(first zen-auditdetail of zen-auditline)
    then delline(rowid(zen-auditline)).
end.
put stream op unformatted 'Removed a total of ' y ' Orphaned lines' skip.
put stream op unformatted 'Done in ' string(etime / 1000) ' Seconds' skip.
output stream op close.
