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





/* CHANGEABLE BITS HERE change table-name to appropriate db table */
&glob Table-name zen-task
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
         HEIGHT             = 20.14
         WIDTH              = 45.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}mainp.i}

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

DEFINE INPUT PARAMETER lv-taskserver AS CHAR NO-UNDO.

    {{&core}apsrv-openquery.i
           &WHERE = "WHERE zen-task.taskserver = lv-taskserver"}

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

&IF DEFINED(EXCLUDE-submit-task) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE submit-task Procedure 
PROCEDURE submit-task :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT  PARAM lv-runpgm     AS CHAR NO-UNDO.
def input param lv-proc as char no-undo.
DEF INPUT  PARAM lv-taskserver AS CHAR NO-UNDO.
DEF INPUT  PARAM vd-run-date   AS DATE NO-UNDO.
DEF INPUT  PARAM lv-run-time   AS INT  NO-UNDO.
DEF INPUT  PARAM lv-schedule   AS CHAR NO-UNDO.
DEF INPUT  PARAM lv-type       AS CHAR NO-UNDO.
DEF INPUT  PARAM lv-parameters AS CHAR NO-UNDO.
DEF OUTPUT PARAM lv-task-no    AS INT  NO-UNDO.

DEF VAR vd-id      AS DEC NO-UNDO. /* new tableid */

/* get new taskid - lowest number we can - must be unique */
FIND LAST zen-task USE-INDEX taskid NO-LOCK NO-ERROR.
ASSIGN lv-task-no = IF AVAIL zen-task THEN zen-task.taskid + 1 ELSE 1.

FIND LAST zen-taskhist WHERE zen-taskhist.taskid = lv-task-no 
USE-INDEX taskid NO-LOCK NO-ERROR.

IF AVAIL zen-taskhist THEN 
DO:
   FIND LAST zen-taskhist USE-INDEX taskid NO-LOCK NO-ERROR.
   ASSIGN lv-task-no = zen-taskhist.taskid + 1.
END.                                                      
    /* otherwise leave as is */     
/* otherwise leave as is */

CREATE zen-task.
ASSIGN zen-task.taskid     = lv-task-no
       zen-task.taskpgm    = lv-runpgm
       zen-task.taskproc = lv-proc
       zen-task.taskserver = lv-taskserver
       zen-task.rundate    = vd-run-date
       zen-task.runtime    = lv-run-time
       zen-task.schedule   = lv-schedule
       zen-task.parameters = lv-parameters
       zen-task.cycle      = 1
       zen-task.submitdate = TODAY
       zen-task.submittime = TIME       
       zen-task.tstatus    = '{&twaiting}'
       zen-task.tmessage   = 'Waiting to Run'
       zen-task.tasktype   = lv-type.
zen-task.submituser = GetSysVar("user").

RELEASE zen-task.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update-schedule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-schedule Procedure 
PROCEDURE update-schedule :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER vr-recid    AS dec NO-UNDO.
DEF INPUT PARAMETER lv-schedule AS CHAR  NO-UNDO.
DEF INPUT PARAMETER vd-run-date AS DATE  NO-UNDO.
DEF INPUT PARAMETER lv-run-time AS INT   NO-UNDO.


if lv-schedule = 'asap'
then assign
        vd-run-date = StringToDate(GetServerValueFor('today'))
        lv-run-time = StringToInt(GetServerValueFor('time'),session:numeric-separator).


FIND FIRST zen-task WHERE zen-task.zen-tasktableid = vr-recid EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL zen-task THEN
DO:

    ASSIGN zen-task.schedule = lv-schedule
           zen-task.rundate  = vd-run-date
           zen-task.runtime  = lv-run-time.

    RELEASE zen-task.
END.
else ErrorCreate(50,'{&table-name}',string(vr-recid),'','').


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

