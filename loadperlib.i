def var {&handle} as handle no-undo.
{&handle} = session:first-procedure.
do while {&handle} <> ?:
   if {&handle}:private-data  = '{&library}' 
   then do:
      if {&handle}:private-data  = '{&library}'  
      then leave.
   end.
   {&handle} = {&handle}:next-sibling.
end.  

if not valid-handle({&handle}) then do:
   run value('{&path}{&library}.p') persistent set {&handle} {&params}.  
   {&handle}:private-data = '{&library}'.
end.

