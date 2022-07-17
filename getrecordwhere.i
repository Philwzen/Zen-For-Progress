def input  param pv-tablename as char no-undo.
def input  param pv-where as char no-undo. 
def input  param pv-retname as char no-undo.
def output param pv-tablehandle as handle no-undo.
    
    if pv-retname = ''
    then pv-retname = "t-" + pv-tablename.
    
    &if defined(usecache) ne 0 &then
        pv-tablename = 't-' + pv-tablename.
    &endif

    DEFINE VARIABLE lv-buff           AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-db AS HANDLE NO-UNDO.
    
    create buffer lv-db for table pv-tablename.
    CREATE TEMP-TABLE pv-tablehandle.           /* Create an empty, undefined TEMP-TABLE */
    pv-tablehandle:CREATE-LIKE(lv-db).         /* Give it table?s fields & indexes */
    pv-tablehandle:TEMP-TABLE-PREPARE(pv-retname). 
    lv-buff = pv-tablehandle:DEFAULT-BUFFER-HANDLE.  /* Get the buffer handle for the temp-table */

    lv-db:find-first(pv-where) no-error.
    if error-status:error then do:
        return '** Record Not Found ' + pv-tablename + pv-where.
    end.

    lv-buff:BUFFER-CREATE.  
    lv-buff:BUFFER-COPY(lv-db).
