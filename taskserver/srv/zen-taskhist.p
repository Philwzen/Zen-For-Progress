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
&glob Table-name zen-taskhist
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
   Other Settings: CODE-ONLY
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

&IF DEFINED(EXCLUDE-clear-history) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-history Procedure 
PROCEDURE clear-history :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER lv-server AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER lv-from   AS CHAR NO-UNDO. /* screen-value - clear before date or not? */
DEFINE INPUT PARAMETER vd-date   AS DATE no-undo.

IF lv-from = 'yes' THEN
DO:
   FOR EACH zen-taskhist WHERE zen-taskhist.taskserver = lv-server AND 
                               zen-taskhist.rundate < vd-date EXCLUSIVE-LOCK:
       DELETE zen-taskhist.
   END.
END.
ELSE
DO:
   FOR EACH zen-taskhist WHERE zen-taskhist.taskserver = lv-server EXCLUSIVE-LOCK:
       
       DELETE zen-taskhist.
   END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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
Def Input  Param pv-direction As Char  No-undo.
def Input  param pv-rowid     as recid no-undo.
def output param table for t-{&table-name}.

can use &where as where clause and &by for sorting
 eg  &where = "where {&table-name}.class = pv-class"       
     &by    = "by {&table-name}.name"                                                   
********************************************************/

{{&core}apsrv-getrecords.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Procedure 
PROCEDURE open-query :
/* USES MANDATORY PARAMETERS
     output table  for t-{&table-name}.
     return ALL records
can use &where as where clause and &by for sorting
 eg  &where = "where {&table-name}.class = pv-class"       
     &by    = "by {&table-name}.name"   
********************************************************/

DEFINE INPUT PARAMETER lv-taskserver AS CHAR NO-UNDO.

FOR EACH t-zen-taskhist:
    DELETE t-zen-taskhist.
END.

{{&core}apsrv-openquery.i
           &WHERE = "WHERE zen-taskhist.taskserver = lv-taskserver"}

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

&IF DEFINED(EXCLUDE-restore-task) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restore-task Procedure 
PROCEDURE restore-task :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pv-id like {&table-name}.{&Unique-key} NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR t-zen-taskhist.
DEFINE OUTPUT PARAMETER lv-task-no AS INT NO-UNDO.

DEF VAR vd-id      AS DEC NO-UNDO. /* new tableid */

ASSIGN lv-task-no = 0.

FIND FIRST t-zen-taskhist WHERE t-zen-taskhist.{&Unique-key} = pv-id NO-LOCK NO-ERROR.
IF AVAIL t-zen-taskhist THEN
DO:
    
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
    
    CREATE zen-task.
    ASSIGN zen-task.taskid     = lv-task-no
           zen-task.taskpgm    = t-zen-taskhist.taskpgm
           zen-task.taskserver = t-zen-taskhist.taskserver
           zen-task.rundate    = t-zen-taskhist.rundate
           zen-task.runtime    = t-zen-taskhist.runtime
           zen-task.schedule   = t-zen-taskhist.schedule
           zen-task.parameters = t-zen-taskhist.parameters
           zen-task.cycle      = 1
           zen-task.submitdate = TODAY
           zen-task.submittime = TIME
           zen-task.tstatus    = '{&twaiting}'.
    zen-task.submituser = GetSysVar("user").
    
    RELEASE zen-task.


END. /* found history - restore task from history */


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

