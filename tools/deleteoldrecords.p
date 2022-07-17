

def var lv-table  as char no-undo format 'x(30)' init '*'.
def var lv-field  as char no-undo format 'x(30)' init '*'.
def var lv-ok as log no-undo.
def var lv-date   as date no-undo format '99/99/9999' init today.
def var lv-action  as char no-undo  init '*'.
def var lv-user  as char no-undo format 'x(30)' init '*'.

update lv-table lv-field lv-date lv-action lv-user
with 1 column.


message 'Delete old audit data for' skip
         'Table  ' lv-table skip
         'Field  ' lv-field skip
         'Before ' lv-date 
         'Action ' lv-action 
         'By     ' lv-user
view-as alert-box question buttons yes-no update lv-ok.
if not lv-ok then leave.

/* now remove any old audit data if we want to 
we can add any of these fields to selection of zen-auditdetail to narrow
record selection .
zen-auditdetail.auditdate   = date of action
zen-auditdetail.AuditAction = 'create' or 'update' or 'delete'
zen-auditdetail.audittime   = time of action probably unnecesary
zen-auditdetail.byname      = userid
*/
for each zen-auditdetail where zen-auditdetail.tablename matches lv-table
                           and zen-auditdetail.auditdate <= lv-date
                           and zen-auditdetail.AuditAction matches lv-action
                           and zen-auditdetail.byname matches lv-user
                         exclusive-lock:
    for each zen-auditline where zen-auditline.zen-auditdetailtableid = zen-auditdetail.zen-auditdetailtableid
                             and zen-auditline.fieldname matches lv-field
                           exclusive-lock:
      delete zen-auditline.
    end.
    /* if no field info left then detail header record */
if not can-find(first zen-auditline where zen-auditline.zen-auditdetailtableid = zen-auditdetail.zen-auditdetailtableid)
      then delete zen-auditdetail.
end.
