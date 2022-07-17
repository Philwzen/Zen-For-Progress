&global-define tablename zen-widlook
def buffer b-{&tablename} for {&tablename}.
current-window:width = 300.
def var x as int no-undo.
for each {&tablename} no-lock:
   find first b-{&tablename} 
         where b-{&tablename}.in-program = {&tablename}.in-program
           and b-{&tablename}.in-frame = {&tablename}.in-frame
           and b-{&tablename}.look-field = {&tablename}.look-field
           and b-{&tablename}.{&tablename} ne {&tablename}.{&tablename}tableid
   no-lock no-error.
   if avail b-{&tablename} then do:
         find current b-{&tablename} exclusive-lock.
/*          delete b-{&tablename}.  */
         x = x + 1.
         disp b-{&tablename}.in-program format 'x(5)'
              b-{&tablename}.in-frame format 'x(5)'
              b-{&tablename}.look-field format 'x(25)'
              b-{&tablename}.{&tablename}tableid
              {&tablename}.{&tablename}tableid
         with width 255.
   end.
end.
message x.
