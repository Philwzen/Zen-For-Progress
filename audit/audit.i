/*
*   audit.i
*   from audit
*   Standard audit include for database triggers
*	dfsdsdfsdfdf
*   Parameters:
*       &TableName  Table name
        &OldBuffer  Old buffer name
        &Update     Yes for write trigger, No for delete trigger
*/

procedure loadaudlib:
def output param lh-lib as handle no-undo.
def var x as int no-undo.

    do x = 1 to num-entries(session:super-procedures):
        lh-lib = widget-handle(entry(x,session:super-procedures)).
        if lh-lib:private-data  = "library-audlib" 
        then return. /* library already a super */
        /* should always return from here after first trigger fires */
    end.  

    lh-lib = session:first-procedure.
    do while lh-lib <> ?:
        if lh-lib:private-data  = "library-auditlib" 
        then do: /* running but not a super */
            if not can-do(session:super-procedures,string(lh-lib)) then do:
                session:ADD-SUPER-PROCEDURE(lh-lib).
                return.
            end.
            else return.
        end.
        lh-lib = lh-lib:next-sibling.
    end.  
    /* not running so run it */
    run {&aud}auditlib.p persistent set lh-lib.  
    lh-lib:private-data = 'library-audlib'.
    session:ADD-SUPER-PROCEDURE(lh-lib).
end procedure.


   def var b-before   As Handle No-undo.
   Def var b-after    As Handle No-undo.
   DEF VAR h-auditlib AS HANDLE NO-UNDO.

   run loadaudlib (output h-auditlib).    
    assign                                       
        b-before  = buffer {&Oldbuffer}:handle   
        b-after   = buffer {&TableName}:handle.  
 
  run audit in h-auditlib (b-before,b-after,{&update},{&TableNameKey}).
