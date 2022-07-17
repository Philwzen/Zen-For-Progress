def var lv-table  as char no-undo format 'x(30)' init '*'.
def var lv-field  as char no-undo format 'x(30)' init '*'.
def var lv-enable as log  no-undo init true.
def var lv-ok as log no-undo.


update lv-table lv-field lv-enable
with 1 column.

/* this is the structure of the config tables */
/* use matches so we can do wild card setings for table and filename
 eg lv-table = '*' and lv-field = '*tableid'  */
/* if Active flag is false then no auditing will be done for that field or 
if table active flag is false then whole table will not be auditted */

for each Zen-AuditConfig where Zen-AuditConfig.TableName matches lv-table
                         exclusive-lock:
   message 'Set whole table ' Zen-AuditConfig.TableName ' Active to ' lv-enable ' ?'
   view-as alert-box question buttons yes-no update lv-ok.
   if lv-ok then zen-auditconfig.Active = lv-enable.

   for each Zen-Auditfield  Where zen-auditfield.tablename = zen-auditconfig.tablename
                              and zen-auditfield.fieldname matches lv-field
                            exclusive-lock:
      zen-auditfield.Active = lv-enable.   
   end.
end.

