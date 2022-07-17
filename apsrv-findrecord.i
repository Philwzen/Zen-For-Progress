/*------------------------------------------------------------------------------
  Purpose: standard appserver find a record    
  Parameters:  temp-table  and &where = where clause
  Notes:       
------------------------------------------------------------------------------*/

def input-output param table for t-{&table-name}.

run clear-table in this-procedure no-error.
{{&core}find.i &table = "{&table-name}"
              &where = "{&where}"
              &type  = "first"
              &lock  = "no"}
if avail {&table-name} then do:
    create t-{&table-name}.
    buffer-copy {&table-name} to t-{&table-name}. 
    &if '{&unique-key}' = 't-recid' &then
    t-{&table-name}.{&unique-key} = recid({&table-name}).
    &endif
END.
else do:
   ErrorCreate(50,'{&table-name}','{&where}','','').
   return .    
end.

