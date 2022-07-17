/* files and fields in the 4 non-schadm databases that we are *NOT* auditing?              */
/* If we are not auditing an entire file, I just need to see that, not all of its field names. */
/* Otherwise, if we are auditing the file, please show its non-audited fields.                 */

/* change this preprocessor to reflect the db you want to report on */
&glob db dictdb

def stream op.
output stream op to '{&db}notauditedlist.csv'.

for each {&db}._file where _file-num > 0 
                       and _file-num < 32000
                     no-lock:

    find Zen-AuditConfig where Zen-AuditConfig.TableName = _file-name
                           and zen-auditconfig.active 
                         no-lock no-error.
    if not avail Zen-AuditConfig 
    then do: /* either not setup or not Active */
      put stream op '{&db},' _file-name ',All' skip.
      next.
    end.
    for each {&db}._field of {&db}._file no-lock:
         /* we never audit the following so dont bother checking */
        if index(_field-name,'tableid') ne 0 then next.
        if can-do('dentered,dchanged,lchgdt,sys-cd,practice,acct-no,pt-num'
                    ,_field-name) then next.
        /* if cant find it its not audited */
        If not can-find(FIRST Zen-Auditfield Where zen-auditfield.tablename = zen-auditconfig.tablename
                                               and zen-auditfield.fieldname = _field-name)
        then do:
            put stream op '{&db},' _file-name ',' _field-name skip.
        end.
    end.
end.
output stream op close.
