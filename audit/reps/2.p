def temp-table t-auddel no-undo
    field deltable as log
    field delline  as log
    field tablename as char 
    field fieldname as char.
       

def var x as int  no-undo.
def var y as int no-undo.
def stream op.
def var lv-forreal as log no-undo init true.
def buffer b-detail for zen-auditdetail.

output stream op to 'cleannonauditedtables.log'.
x = 0.

/* setup files:
   zen-auditconfig (for files)  is parent of zen-auditfield (for fields)  */

/* actual audit data:
   zen-auditdetail (file level) is parent of zen-auditline  (field level) */
etime(true).
put stream op unformatted 'Processing Unuadited Tables' skip.
for each zen-auditdetail no-lock break by zen-auditdetail.tablename:
   /* make sure this file is supposed to be audited at all */
   if first-of(zen-auditdetail.tablename) then do:     
      if not can-find(first zen-auditconfig where 
         zen-auditconfig.tablename = zen-auditdetail.tablename and 
         zen-auditconfig.Active    = yes)
      then do:
         y = 0.
         x = 0.
         put stream op unformatted  'Completely Removed ' zen-auditdetail.tablename ' With '.
         for each b-detail where b-detail.tablename = zen-auditdetail.tablename
                           exclusive-lock:
            x = x + 1.
            for each zen-auditline of zen-auditdetail exclusive-lock:  
               y = y + 1.                           
                if lv-forreal then delete zen-auditline.
            end.
            if lv-forreal then delete b-detail. 
         end.
         put stream op unformatted  x ' Occurances and total of ' y ' Field records' skip.
      end. /* this table should not be audited */
      
      else for each zen-auditline of zen-auditdetail no-lock:
         /* make sure this field should be audited */
         if not can-find(first zen-auditfield where 
            zen-auditfield.tablename = zen-auditdetail.tablename and 
            zen-auditfield.fieldname = zen-auditline.fieldname   and 
            zen-auditfield.Active)
         then do:
            create t-auddel.
            t-auddel.tablename = zen-auditdetail.tablename.
            t-auddel.fieldname = zen-auditline.fieldname.
            put stream op unformatted 'delete,' zen-auditdetail.tablename ',' zen-auditline.fieldname skip.
         end. /* build temp-table of fields which shouldn't be audited */  
      end. /* check through fields audited for first record */
   end. /* first audit record for a given table */ 
end. /* for each zen-auditdetail */

y = 0 .
put stream op unformatted 'Done in ' string(etime / 1000) skip.
etime(true).
put stream op unformatted 'Processing Unuadited Fields' skip.
/* go through the fields which shouldn't be audited and delete any audit
   records */
for each t-auddel no-lock:
    x = 0.
    y = 0.
    for each zen-auditdetail where zen-auditdetail.tablename = t-auddel.tablename exclusive-lock:
        y = y + 1.
        find zen-auditline of zen-auditdetail where zen-auditline.fieldname = t-auddel.fieldname exclusive-lock no-error.
        if avail zen-auditline then do:
            x = x + 1.
            if lv-forreal then delete zen-auditline.   
        end.
    end.
    put stream op unformatted 'Removed a total of ' x ' Field records From ' y ' Occurances of table '  t-auddel.tablename skip.
end.
put stream op unformatted 'Done in ' string(etime / 1000) skip.
output stream op close.
