def var nodetail as int no-undo.
def var notbldef as int no-undo.
def var noflddef as int no-undo.
def var lv-msg   as char no-undo format 'x(60)'.

def stream op.

form lv-msg with frame a .

output stream op to 'checkaudtables.csv'.
put stream op unformatted 'type,table,field,nodetail,notable,nofield' skip.
disp lv-msg with frame a.


/* setup files:
   zen-auditconfig (for files)  is parent of zen-auditfield (for fields)  */

/* actual audit data:
   zen-auditdetail (file level) is parent of zen-auditline  (field level) */

/* for each child audit record, see if the parent exists */
for each zen-auditline no-lock by zen-auditline.zen-auditdetailtableid:
    find zen-auditdetail where 
       zen-auditdetail.zen-auditdetailtableid = zen-auditline.zen-auditdetailtableid
       no-lock no-error.
    /* if the parent doesn't exist ... */
    if not avail zen-auditdetail then do:
       lv-msg:screen-value = 
          'Detail ' + string(zen-auditline.zen-auditdetailtableid) + 
          zen-auditline.fieldname.
       nodetail = nodetail + 1.
       put stream op unformatted 
          'Detail,' zen-auditline.zen-auditdetailtableid ',' 
          zen-auditline.fieldname ',' nodetail skip.
       next.
    end.
    
    /* if we got this far, the child has a parent.  Check to see if the
       file should be audited */
    find zen-auditconfig where 
       zen-auditconfig.tablename = zen-auditdetail.tablename and 
       zen-auditconfig.Active    = yes no-lock no-error.
    /* audit records exist for a file which shouldn't be audited */
    if not avail zen-auditconfig then do:
        lv-msg:screen-value = 'Table ' + 
           zen-auditdetail.tablename + zen-auditline.fieldname. 
        notbldef = notbldef + 1.
        put stream op unformatted 
           'Table,' zen-auditdetail.tablename ',' 
           zen-auditline.fieldname ',,' notbldef skip.        
        next.
    end.                                            
       
    /* if we got this far, parent record exists and file should be audited.
       Now make sure that the field within the file should be audited */
    find zen-auditfield where 
       zen-auditfield.tablename = zen-auditdetail.tablename and 
       zen-auditfield.fieldname = zen-auditline.fieldname   and 
       zen-auditfield.Active    = yes no-lock no-error.
    /* audit record exists for a field which shouldn't be audited */
    if not avail zen-auditfield then do:
        lv-msg:screen-value = 'Field ' + zen-auditdetail.tablename + zen-auditline.fieldname. 
        noflddef = noflddef + 1.
        put stream op unformatted 
           'Field,' zen-auditdetail.tablename ',' 
           zen-auditline.fieldname ',,,' noflddef skip.
    end.                        
end. /* loop through all child audit records */
    
put stream op unformatted 'Total,,' nodetail ',' notbldef ',' noflddef skip.
output stream op close.
