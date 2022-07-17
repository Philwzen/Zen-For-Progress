/*------------------------------------------------------------------------------
  Purpose: standard appserver openquery routine
  Parameters:  
  Notes:   
------------------------------------------------------------------------------*/
{&extraparams}
def input param scratch as char no-undo.
def input param scratch2 like  t-{&table-name}.{&unique-key} no-undo.
def output param table  for t-{&table-name}.
{&extratables}

RUN clear-table IN THIS-PROCEDURE NO-ERROR.

close query q-{&table-name}.

if can-find(first {&table-name} {&where}) 
then do:
    OPEN QUERY q-{&table-name}
        FOR EACH {&table-name} {&where}
            no-lock {&by} /* indexed-reposition */.

    GET first q-{&table-name}.
    do while not query-off-end("q-{&table-name}"):
        create t-{&table-name}.
        buffer-copy {&table-name} to t-{&table-name} {&exceptfields}. 
        &if '{&unique-key}' = 't-recid' &then
        t-{&table-name}.{&unique-key} = recid({&table-name}).
        &endif
        RUN extra-fields IN THIS-PROCEDURE NO-ERROR.
        GET next q-{&table-name}.
    end.

    return return-value.                 
end.


