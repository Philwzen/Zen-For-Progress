&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&glob serverprogram true
/* &glob table-name zen-tserver  */
DEFINE INPUT PARAMETER lv-tserver AS CHAR NO-UNDO.
def input param pv-logfile as char no-undo.
def input param lv-context as char no-undo.
{app-paths.i}

{{&core}mainp.i}
{{&tsk}paramdelim.i}
lv-logfile = pv-logfile.
if lv-logfile = '' then lv-logfile = '{&logs}' + lv-tserver + '.log'.
def stream s-batch.
OUTPUT TO value("{&logs}" + lv-tserver + 'screen.log'). /* SCREEN */
put unformatted program-name(1) ' Loading Taskserver ' lv-tserver skip.
{{&tsk}{&srv}taskserver.i}
{{&tsk}{&srv}winsubmit.i}

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

IF NOT can-find(FIRST zen-tserver WHERE zen-tserver.taskserver = lv-tserver) 
THEN DO:
   put-log(lv-logfile, "** TaskServer: " + lv-tserver + " was unable to be located in DB - cannot start").
   RETURN.
END. /* couldn't find taskserver in DB */
ELSE DO: /* initialise task server */
   put-log(lv-logfile, "TaskServer: " + lv-tserver + " - Found and Initialised in DB").
   update-server-status(lv-tserver, '{&tstarted}').
END.

run ProcessJobs no-error.

if error-status:error then return.
run WaitForFinishingTasks.

put-log(lv-logfile, '** Taskserver: Now Stopped').
update-server-status(lv-tserver, '{&tstopped}').


put unformatted program-name(1) ' Closing Taskserver ' lv-tserver skip.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ProcessJobs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessJobs Procedure 
PROCEDURE ProcessJobs :
DEF VAR vd-lookdate AS DATE   NO-UNDO. /* search for task on this date */
DEF VAR lv-looktime AS INT    NO-UNDO. /* search for task on this time */
DEF VAR lv-cmd-line AS CHAR   NO-UNDO. /* unix command line for taskserver process' */
DEF VAR lv-pid     AS INT NO-UNDO.
DEF VAR vh-process AS INT NO-UNDO.
DEF BUFFER b-task FOR zen-task.
def var lv-session as char no-undo.
lv-session = trim(sessionid()).
repeat on error undo, retry:
     if retry then DO:
        put-log(lv-logfile,program-name(1) +  '** TaskServer: Retry Occurred Firing up Process for Task').
        RETURN error.
     END.

     FIND FIRST zen-tserver WHERE zen-tserver.taskserver = lv-tserver
                            NO-LOCK NO-ERROR.
     
     IF NOT zen-tserver.started 
     THEN DO:
         put-log(lv-logfile,program-name(1) +  '** TaskServer: Stop Signal Recieved').
         return.
     END.

     ASSIGN vd-lookdate = TODAY
            lv-looktime = TIME.
        /* all tasks due to run today and with a time less than now eg. need to run now */
     FOR EACH zen-task WHERE zen-task.taskserver =  lv-tserver  AND
                             (IF zen-tserver.catchup then zen-task.rundate    <=  vd-lookdate 
                                                     ELSE zen-task.rundate    =  vd-lookdate) AND
                                zen-task.runtime    <= lv-looktime AND
                                zen-task.tstatus    = '{&twaiting}'
                       NO-LOCK:
        update-server-status(lv-tserver, '{&tsearching}').
        put-log(lv-logfile,program-name(1) +  ' TaskServer: Found Task ' + string(zen-task.taskid)).
        lv-cmd-line = getdlcbin() + '_progres -b -p ' + '{&tsk}{&srv}tserver-process-db-connect.p' + 
                                    ' -param local' + lv-delim +
                                    lv-tserver + lv-delim + 
                                    string(zen-task.taskid) + lv-delim + 
                                    zen-tserver.logfile + lv-delim + 
                                    zen-tserver.DBParameters + lv-delim /* + 
                                    lv-session */ no-error.
        put-log(lv-logfile, program-name(1) + ' Trying to Execute ' + lv-cmd-line).
        &if "{&OPSYS}" = 'unix' &then 
            lv-cmd-line = lv-cmd-line + ' &'. /****AK****/
            OS-COMMAND SILENT VALUE(lv-cmd-line).
        &else
            vh-process = createpro(lv-cmd-line,'',OUTPUT lv-pid).    
            DO TRANSACTION:                        
                FIND b-task WHERE recid(b-task) = recid(zen-task) EXCLUSIVE-LOCK.
                b-task.pid = lv-pid.
                FIND CURRENT b-task NO-LOCK.
            END.
/*             lv-cmd-line = 'start /B ' + lv-cmd-line.    */
/*             output stream s-batch to tskc.bat.          */
/*             put stream s-batch unformatted lv-cmd-line. */
/*             output stream s-batch close.                */
/*             OS-COMMAND no-wait "cmd /C tskc.bat".       */
        &endif
        put-log(lv-logfile,program-name(1) + ' excuted').           
     END. 
     update-server-status(lv-tserver, '{&twaiting}').
     PAUSE zen-tserver.twait NO-MESSAGE. /* wait set time before looking for new tasks */
END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WaitForFinishingTasks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WaitForFinishingTasks Procedure 
PROCEDURE WaitForFinishingTasks :
/* wait for any tasks that are running before stopping the server fully */
DO WHILE CAN-FIND(FIRST zen-task WHERE zen-task.taskserver = lv-tserver and
                                       zen-task.tstatus    = '{&trunning}'):
    PAUSE zen-tserver.twait NO-MESSAGE. /* wait set time before looking for new tasks */
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

