/*------------------------------------------------------------------------------
  Purpose: standard appserver delete record routine    
  Parameters:  temp-table.uniqueid
  Notes: Temp table MUST have a unique field ({&table-name}.{&Unique-key}}
  so we can pass it to find the 
         right record in temptable on appsrv. then find approrpiate db record 
	(or create a record if new ( = ?))  
------------------------------------------------------------------------------*/
def input param pv-id like t-{&table-name}.{&unique-key} no-undo.

if getctrl("{&read-only}") = 'yes' then do:
    if not Systemmanager(GetUserID()) then do:
        ErrorCreate(10,'','','','').
        return .    
    end.
end.

&if '{&unique-key}' = 't-recid' &then
find {&table-name} where recid({&table-name}) = pv-id 
                   no-lock no-error.
&else
find {&table-name} where {&table-name}.{&Unique-key} = pv-id 
                   no-lock no-error.
&endif

if not avail {&table-name} then do:
   ErrorCreate(50,'Table','{&table-name}','','').
   return .    
end.
/* check ok to delete record */
Run delete-validation In This-procedure No-error.

If Not Error-status:Error Then
    If Return-value Ne 'passed' Then Do:
        ErrorCreate(8,return-value,'','',''). 
        return .
    End.

find current {&table-name} exclusive-lock no-error.
if avail {&table-name} then do:
    /* delete any child tables */
    run delete-related-tables in this-procedure no-error.
   
    &IF DEFINED(softdelete) NE 0 &THEN
        RUN soft-delete IN THIS-PROCEDURE NO-ERROR.
        RELEASE {&table-name}.
        if haderrors() then return.
    &ELSE
        delete {&table-name}.
    &endif
end.
else do: /* error processing */
    ErrorCreate(7,'Delete','Failed','','').
end.  

