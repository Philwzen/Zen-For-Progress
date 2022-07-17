/*------------------------------------------------------------------------------
  Purpose: standard appserver save record routine    
  Parameters:  t-{&table-name}.{&Unique-key} , temp-table
  Notes: Temp table MUST have  a unique key so we can pass it to find the 
         right record in temptable on appsrv. then find appropriate db record 
  	 (or create a record if ({&Unique-key} = ?))                 
------------------------------------------------------------------------------*/
/* maybe we should go to prodatasets to avoid problems with &extratables */
{&extrasave}

def input param pv-id like {&table-name}.{&Unique-key} no-undo.
def input-output param table for t-{&table-name}.  /* new version of record */
def input param table for tb-{&table-name}. /* old version, if avail */

{&extraTables}

def var lv-changedFields as char no-undo.


/* if system is read-only and user is not a system manager, don't try to save
   the changes */
if getctrl("{&read-only}") = 'yes' then do:
   if not Systemmanager(getsysvar("user")) then do:
      ErrorCreate(1000,'','','','').
      return .    
   end.
end.


/* find record we are supposed to be saving */
find t-{&table-name} where t-{&table-name}.{&Unique-key} = pv-id no-error.
if not avail t-{&table-name} 
then do:
   ErrorCreate(50,'t-{&table-name}',string(pv-id),'','').
   return.    
end.


run pre-save in this-procedure no-error.

if haderrors() then return.

/*----------------------
    New record stuff
----------------------*/
do transaction: /* need this else cant trap trigger error */
   /* create a new record */
   if pv-id = ? then do: 
       run pre-create in this-procedure no-error.
       if haderrors() or return-value = "problem" then return.
       create {&table-name}.
       run post-create in this-procedure no-error.
       if haderrors() then do:
           delete {&table-name}.
           return.
       end.                                               
   end. /* creating new record */
   else do: 
      /* find current version of record from database */
      find {&table-name} exclusive-lock where 
         {&table-name}.{&Unique-key} = pv-id no-wait no-error.
      if not avail {&table-name} then do:
         if locked {&table-name} 
         then ErrorCreate(7,'By',GetLockingCulprit(RECID(tb-{&table-name})),'','').
         else ErrorCreate(50,'{&table-name}'+ ' original',string(pv-id),' ',' ').
         return.
      end.

     /* find version of record as it was originally sent to the client side */
      find tb-{&table-name} no-lock where tb-{&table-name}.{&Unique-key} = pv-id 
                                    no-error.
      if not avail tb-{&table-name} then do:
         ErrorCreate(50,'{&table-name}'+ ' original',string(pv-id),' ',' ').
         return.
      end.
      
      /* compare current version in database with the version originally sent
         over to the client side to be sure that no other user has made changes
         before we copy our changed version in */
      buffer-compare {&table-name} to tb-{&table-name} save lv-changedFields {&no-lobs}.
      if lv-changedFields ne '' /* somebody else has changed db record while we had it!! */
      then do:
         buffer-copy {&table-name} to t-{&table-name}.
         release {&table-name}.
         ErrorCreate(111,' ',' ',' ',' ').
         ErrorCreate(110,lv-changedFields,' ',' ',' ').
         return.
      end.
   end. /* updating existing record */
   if pv-id ne ? then
      run Update-Children in this-procedure no-error.
      
      if not avail t-{&table-name} /* in case of a for each in update-children!! */
      then find t-{&table-name} where t-{&table-name}.{&Unique-key} = pv-id no-error.   
      
   /* updated to add tableid to list so that if new doesn't overwrite actual tables tableid with ?*/
   &if "{&Unique-key}" = "{&table-name}tableid" &then
      buffer-copy t-{&table-name} except {&Unique-key} 
   	  to {&table-name} {&no-lobs} no-error.
      if error-status:error then do:
         ErrorCreate(999,"DB write", 
                         string(avail t-{&table-name}),
                         string(avail {&table-name}),'').
         return.
      end.
   &else
   	buffer-copy t-{&table-name} to {&table-name}  {&no-lobs} no-error.
   &endif 
   if pv-id = ? then
       run Key-Assign in this-procedure no-error.
      
   /* now copy it back incase any fields auto filled ie keys etc */
   buffer-copy {&table-name} to t-{&table-name}  {&no-lobs}.
   run post-update in this-procedure no-error.
   {&extrasavelogic} 
end. /* transaction */
find current {&table-name} no-lock no-error.
release t-{&table-name} no-error.
if return-value begins 'write' 
then do:
    ErrorCreate(165,return-value,'','','').
    return.
end.
/* would nice to have a way of running something after transaction is finished */
run after-save-transaction in this-procedure no-error.
