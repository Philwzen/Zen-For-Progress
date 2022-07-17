/* find and remove any duplicate zen-auditfield records */

&glob zac zen-auditconfig
&glob zaf zen-auditfield
&glob key tableid

def buffer b-field for {&zaf}.
def var x as int no-undo.

def stream op.
output stream op to 'dedupe.log'.

for each {&zac} no-lock:
   put stream op 'Processing '{&zac}.tablename skip.
   x = 0.
   for each {&zaf} where {&zaf}.{&zac}{&key} = {&zac}.{&zac}{&key}
                   no-lock 
                   break by {&zaf}.{&zac}{&key} by {&zaf}.fieldname:
      if first-of({&zaf}.fieldname)
       then do:
         put stream op 'Keeping ' {&zaf}.fieldname ' tableid ' {&zaf}.{&zaf}{&key} skip.
         for each b-field where b-field.{&zac}{&key} = {&zaf}.{&zac}{&key}
                            and b-field.fieldname = {&zaf}.fieldname
                            and b-field.{&zaf}{&key} ne {&zaf}.{&zaf}{&key}
                          exclusive-lock:
            put stream op 'Deleting ' {&zaf}.fieldname ' tableid ' b-field.{&zaf}{&key} skip.
            x = x + 1.
            delete b-field.
         end.
      end.
   end.
   put stream op ' Cleaned ' x ' records' skip.
end.
