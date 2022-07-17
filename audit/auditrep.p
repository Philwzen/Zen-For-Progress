def stream op.
def var x as int no-undo.
def var y as int no-undo.
output stream op to 'auditrep.csv'.
put stream op unformatted 'Table,User,Count' skip.

for each zen-auditdetail no-lock
                         break by tablename by byname:
    if last-of(byname) 
    then do:
        put stream op unformatted tablename ',' byname ',' y skip.
        y = 0.
    end.
    if last-of(tablename) 
    then do:
        put stream op unformatted tablename ',Total,' x skip.
        x = 0.
    end.
    x = x + 1.
    y = y + 1.
end.
/*    
zen-auditdetail.tablename zen-auditdetail.byname zen-auditdetail.AuditAction
*/



