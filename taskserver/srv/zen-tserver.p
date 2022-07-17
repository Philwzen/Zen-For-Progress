&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
&glob serverprogram true
/* ***************************  Definitions  ************************** */
{app-paths.i}



{{&tsk}paramdelim.i}
/* CHANGEABLE BITS HERE change table-name to appropriate db table */
&glob Table-name zen-tserver
&glob Unique-key {&table-name}tableid 

{{&core}def-table.i &table-name = {&table-name}}

Def QUERY q-{&table-name} FOR {&table-name} scrolling.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-GetDlcBinaaaaaaa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetDlcBinaaaaaaa Procedure 
FUNCTION GetDlcBinaaaaaaa RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 16
         WIDTH              = 44.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}mainp.i}

&if "{&opsys}" = "win32" &then
{{&tsk}{&srv}winsubmit.i}
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-clear-table) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-table Procedure 
PROCEDURE clear-table :
for each t-{&table-name}:
    delete t-{&table-name}.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-record Procedure 
PROCEDURE delete-record :
/* USES MANDATORY PARAMETERS
def input param pv-recid as recid no-undo.
def input-output param table for t-{&table-name}.
*/
{{&core}apsrv-deleterecord.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Delete-Related-Tables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete-Related-Tables Procedure 
PROCEDURE Delete-Related-Tables :
/*------------------------------------------------------------------------------
  Purpose: delete any child tables of this parent
  Parameters:  <none>
  Notes:   leave blank if not required    
------------------------------------------------------------------------------*/
/* EXAMPLE
   find prod-matrix of product exclusive-lock no-error.
   if avail prod-matrix 
    then delete prod-matrix.
   for each prod-curr of product exclusive-lock:
       delete prod-cur.
   end.
   */

for each zen-task of zen-tserver exclusive-lock:
   delete zen-task.
end.
for each zen-taskhist of zen-tserver exclusive-lock:
   delete zen-taskhist.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-validation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-validation Procedure 
PROCEDURE delete-validation :
/*------------------------------------------------------------------------------
  Purpose:     check ok to delete record
  Parameters:  <none>
  Notes: return 'passed' if its ok to delete.
------------------------------------------------------------------------------*/
/* example 
    if can-find(first ledger-item of product) then return 'Failed'.
*/
    Return 'passed'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-find-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-record Procedure 
PROCEDURE find-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Def Input Param pv-code As char No-undo.

{{&core}apsrv-findrecord.i 
        &where = "where true"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-records) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-records Procedure 
PROCEDURE get-records :
/* USES MANDATORY PARAMETERS YOU NEED NOT WORRY ONLY 
   CALLED INTERNALLY TO THIS PROCEDURE FROM OPEN-QUERY
def output param table for t-{&table-name}.
def input-output param pv-listcount as int no-undo
*/
{{&core}apsrv-getrecords.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Procedure 
PROCEDURE open-query :
/* USES MANDATORY PARAMETERS
        input-output table  for t-{&table-name}.
        input-output pv-listcount as int.
********************************************************/

    {{&core}apsrv-openquery.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create Procedure 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:    just a hook incase we need it
  Parameters:  <none>
  Notes:   called after record create before buffer copy.
------------------------------------------------------------------------------*/
    assign
        Zen-Tserver.Started = false
        Zen-Tserver.TStatus = '{&tstopped}'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-update) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update Procedure 
PROCEDURE post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: called after buffer copy for create or update      
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pre-Create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pre-Create Procedure 
PROCEDURE Pre-Create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       run before record create
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pre-Save) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pre-Save Procedure 
PROCEDURE Pre-Save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: run as first thing in appsrv-saverecord.i      
------------------------------------------------------------------------------*/
   return "passed".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-save-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE save-record Procedure 
PROCEDURE save-record :
/* uses MANDATORY PARAMETERS
def input param pv-recid as recid no-undo. 
def input-output param table for t-{&table-name}.
*/
{{&core}apsrv-saverecord.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-start-stop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE start-stop Procedure 
PROCEDURE start-stop :
DEFINE INPUT PARAMETER lv-taskserver AS CHAR no-undo.
DEF INPUT PARAM pv-force AS LOG NO-UNDO.
DEF BUFFER b-taskserver FOR {&table-name}.
DEF VAR lv-pid     AS INT NO-UNDO.
DEF VAR vh-process AS INT NO-UNDO.
DEF VAR lv-com-line AS CHAR NO-UNDO.
def var lv-session as char no-undo.
def var lv-logfile as char no-undo.
lv-session = "'" + trim(sessionid()) + "'".
   DO TRANSACTION:
      FIND FIRST {&table-name} WHERE {&table-name}.taskserver = lv-taskserver
                               EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL {&table-name} 
        THEN do:
            ErrorCreate(50,'Table','{&table-name}','','').
            RETURN ERROR.
        end.
      ASSIGN {&table-name}.started   = NOT {&table-name}.started
             {&TABLE-name}.startdate = IF {&table-name}.started THEN TODAY ELSE {&table-name}.startdate
             {&TABLE-name}.starttime = IF {&table-name}.started THEN TIME  ELSE {&table-name}.starttime
             {&table-name}.startuser = IF {&table-name}.started THEN GetSysVar("user") ELSE {&table-name}.startuser
             {&table-name}.pid       = 0.
      
      IF pv-force THEN {&table-name}.tstatus = IF {&table-name}.started THEN '{&tstarted}'  ELSE '{&tstopped}'.
                  ELSE {&table-name}.tstatus = IF {&table-name}.started THEN '{&tstarting}' ELSE '{&tstopping}'.

      FIND FIRST {&table-name} WHERE {&table-name}.taskserver = lv-taskserver
                               NO-LOCK NO-ERROR.
      lv-logfile = if {&table-name}.logfile = '' then '{&logs}' + lv-taskserver + '.log'
                                                 else {&table-name}.logfile.
   END. /*transaction */

   IF {&table-name}.started 
   THEN do: /* start task server */
        lv-com-line = getdlcbin() + 
                      '_progres -b -p ' + {&table-name}.ServerProg + 
                      ' -param local' + lv-delim +
                      lv-taskserver + lv-delim + 
                      lv-logfile + lv-delim +
                      {&table-name}.DBParameters + lv-delim /* + 
                       lv-session */.

    &if "{&OPSYS}" = 'unix' &then 
        lv-com-line = lv-com-line + ' &'.
        OS-COMMAND SILENT VALUE(lv-com-line).
    &else /* 'win32' */
        vh-process = createpro(lv-com-line,'',OUTPUT lv-pid). 
        DO TRANSACTION:
            FIND b-taskserver WHERE RECID(b-taskserver) = RECID({&table-name}) EXCLUSIVE-LOCK.
            b-taskserver.pid = lv-pid.
            FIND CURRENT b-taskserver NO-LOCK.
        END.
    &endif   
   
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetDlcBinaaaaaaa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetDlcBinaaaaaaa Procedure 
FUNCTION GetDlcBinaaaaaaa RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
def var lv-dlc as char format 'x(20)' no-undo.

 &if "{&OPSYS}" = 'unix' &then 
    lv-dlc =  OS-GETENV("DLC") + '/bin/_progres.exe'.
&else

DEF VAR longname AS CHAR NO-UNDO.
DEF VAR shortname AS CHAR NO-UNDO.
DEF VAR returnvalue as int NO-UNDO.

     GET-KEY-VALUE SECTION "startup" KEY "dlc" VALUE lv-dlc.

&GLOB shortsize 68
 
longname = lv-dlc + "\bin\prowin32.exe".
shortname = FILL("-", {&shortsize}).
 
RUN GetShortPathNameA (longname,
                       OUTPUT shortname,
                       LENGTH(shortname),
                       OUTPUT ReturnValue).
    lv-dlc =  substring(shortname,1,r-index(shortname,'prowin32') - 1).        
&endif


  RETURN lv-dlc.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

