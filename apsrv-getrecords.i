/*------------------------------------------------------------------------------
  Purpose:    get records in standard table query from appserver 
  Parameters:  <none>
  Notes:         
   /* USES MANDATORY PARAMETERS YOU NEED NOT WORRY ONLY 
def input param pv-direction as char no-undo.
def input  param pv-id     as recid no-undo.
def output param table        for  t-{&table-name}.

   can use &whereclause as where clause and &by for sorting
      e.g.  &where = "where {&table-name}.class = pv-class"       
        &by          = "by {&table-name}.name"                                                   
********************************************************/
------------------------------------------------------------------------------*/
def input param pv-direction as char no-undo.
def input  param pv-id        like  t-{&table-name}.{&unique-key} no-undo.
def output param table        for  t-{&table-name}.
{&extratables}
def buffer b-{&table-name} for {&table-name}.

def var lv-maxlistcount as int   no-undo.
def var lv-rowid        as rowid no-undo.

&IF defined(OverrideMaxListCount) = 0  
    &THEN lv-maxlistcount = MaxDataGuess(unixpath(this-procedure:file-name)).   
    &else lv-maxlistcount = {&OverrideMaxListCount}.
&ENDIF
if pv-direction = 'all' 
then assign pv-direction = 'down'
            pv-id        = ?
            lv-maxlistcount = 99999999.
def var x as int no-undo init 1.
/* clear out the temp-table */
run {&tbl}clear-table in this-procedure no-error.
&if defined(messages) ne 0
&then message {&messages} 
              'Direction ' pv-direction skip
              'Id ' pv-id skip
              'MaxListCount ' lv-maxlistcount. 
&endif

   close query q-{&table-name}.
   open query q-{&table-name}
        for each {&table-name} {&whereclause} 
            no-lock {&by}.

   if pv-id = ?  then . /* top of file */
   else if pv-id = -1 then do: /* got to  end of file */
        get last q-{&table-name}.
        get next q-{&table-name}.
   end.
   else do: /* reposition to current posisiton */
        &if '{&unique-key}' = 't-recid' &then
        find b-{&table-name} where recid(b-{&table-name}) = pv-id 
                             no-lock no-error.
        &else
        find b-{&table-name} where b-{&table-name}.{&unique-key} = pv-id 
                             no-lock no-error.
        &endif
        reposition q-{&table-name} to rowid rowid(b-{&table-name}). 
        if pv-direction = 'down'
            then get next q-{&table-name}.
            else get prev q-{&table-name}.
    end.

    do x = 1 to lv-maxlistcount:
        if pv-direction = 'up' then 
            get prev q-{&table-name}.
        else 
            get next q-{&table-name}.

        if query-off-end("q-{&table-name}") then leave.

        if available {&table-name}
        then do:
            create t-{&table-name}.
            buffer-copy {&table-name} {&except-fields} to t-{&table-name}.
            &if '{&unique-key}' = 't-recid' &then
            t-{&table-name}.{&unique-key} = recid({&table-name}).
            &endif
            run {&tbl}extra-fields in this-procedure no-error.
            if return-value = "rejected record" then x = x - 1.
        end.
        else leave.
    end.

    run {&tbl}extra-tables in this-procedure {&extra-params} no-error.

   if x <= lv-maxlistcount then return ''.
                       else return 'More'.
