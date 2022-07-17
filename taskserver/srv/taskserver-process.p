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

DEFINE INPUT PARAMETER pv-tserver AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pv-task    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pv-logfile AS CHAR NO-UNDO.
&glob serverprogram true
{app-paths.i}

def var lv-screenlog as char no-undo.

if pv-logfile = '' then pv-logfile = '{&logs}' + pv-tserver + '.log'.
lv-screenlog = replace(pv-logfile,'.log','process.log').

/* just incase of big fall over output terminal to file */

OUTPUT TO VALUE(lv-screenlog) append.
/* put unformatted 'Loading TaskserverProcess ' + pv-task + ' ' + pv-tserver + ' ' + pv-logfile skip.  */

DEF VAR h-parser    AS HANDLE NO-UNDO. /* handle to parser library for schedule interpritation */
DEF VAR lv-converted  AS CHAR NO-UNDO. /* converted schedule line eg. screen/record - different dmy formats */
DEF VAR vd-sched-date AS DATE NO-UNDO. /* next run date */
DEF VAR lv-sched-time AS INT  NO-UNDO. /* next run time */
DEF VAR lv-ended      AS LOG  NO-UNDO. /* schedule ended */
DEF VAR lv-problem    AS LOG  NO-UNDO. /* problem with schedule syntax (should never happen here */
DEF VAR lv-ending     AS LOG  NO-UNDO. /* schedule ever ending? again - don't care here */
DEF VAR lv-iterate    AS LOG  NO-UNDO. /* schedule iterating times throughout date */
DEF VAR lv-success   AS LOG  NO-UNDO. /* TASK RAN OK ?? */
DEF VAR n             AS INT  NO-UNDO init 1. /* entry of task number in task list */
DEF VAR lv-start-time AS INT  NO-UNDO. /* start time of task */
DEF VAR vd-start-date AS DATE NO-UNDO. /* start date of task */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-RunJob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RunJob Procedure 
FUNCTION RunJob RETURNS LOGICAL
  ( pv-prog as char,
    pv-proc as char,
    pv-params as char)  FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}mainp.i}

{{&tsk}{&srv}taskserver.i}


run initialise.

if return-value = 'passed' then 
DO n = 1 TO NUM-ENTRIES(pv-task):
    update-task-status('{&trunning}',INT(ENTRY(n,pv-task))).
    put-log(pv-logfile,program-name(1) + 'Starting ' + ENTRY(n,pv-task) + ' on ' + pv-tserver ).
    RUN run-task(INT(ENTRY(n,pv-task))).
END. 
if valid-handle(h-parser) then delete procedure h-parser.
/* put unformatted 'Closing TaskserverProcess ' + pv-task + pv-tserver + ' ' + pv-logfile skip.  */
OUTPUT CLOSE.
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BatchReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BatchReport Procedure 
PROCEDURE BatchReport :
def input  param pv-prog as char no-undo.
def input  param pv-proc as char no-undo.
def input  param pv-params as char   no-undo.
def output param pv-ok   as log    no-undo.

def var lv-mreport   as memptr no-undo.
def var lv-mschemaholder as handle no-undo.
def var lv-printtable as handle no-undo.
def var lv-endfilename as char no-undo.

def var lv-prtparams as char no-undo.
def var lv-dpgmparams as char no-undo.
def var lv-params as char no-undo.


assign 
   lv-prtparams = entry(1,pv-params,'{&ComboDelim}')
   lv-dpgmparams = entry(2,pv-params,'{&ComboDelim}')
   lv-params = entry(3,pv-params,'{&ComboDelim}')
   lv-endfilename = UnixPath("{&SrvRepout}/") + string(zen-task.rundate,'99-99-9999')  + 
                     zen-task.taskserver + zen-task.submituser + 
                     string(zen-task.taskid) + '.spl'.
set-size(lv-mreport) = 0.
errorclear().

  {{&base}run.i &programc   = pv-prog
                &procedurec = pv-proc
                &Appsrv    = "System"
                &params    = "(no,
                     lv-prtparams,
                     output lv-mreport,
                     output lv-mschemaholder,
                     lv-dpgmparams,
                     lv-params,
                     output table-handle lv-printtable)"}

pv-ok = NOT haderrors().
if pv-ok
then do:
    OutputToFile(lv-endfilename,lv-mreport,'local').
    /* ?? tell usr job is complete or let them check output somehow ?? 
          maybe create a zen-spool record ? 
    spool-id    from trigger
    task-id     zen-task.taskid
    reportname  entry(1,PgmProperty(pv-prog,'repinfo'),"{&Delim2}")  Zen-Dpgm.RepTitle
    printername entry(2,lv-prtparams,'{&Delim2}')
    filename    lv-endfilename
    create-by   zen-task.submituser
    create-date today
    create-time time
    can-view    zen-task.submituser
    not-view    '*'
    keep-until  today + 90     
          */
end.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initialise) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise Procedure 
PROCEDURE initialise :
IF NOT CAN-find(FIRST zen-tserver WHERE zen-tserver.taskserver = pv-tserver)
THEN do:
   put-log(pv-logfile,program-name(1) +  " No TaskServer found in DB for " + pv-tserver + ' ' + pv-task).
   RETURN 'failed'.
END.
ELSE do:
    RUN {&tsk}schedule-parser.p PERSISTENT SET h-parser .
    if not valid-handle(h-parser) then do:
       put-log(pv-logfile,program-name(1) + ' failed to load schedule-parser.p').
       return 'failed'.
    end.
    return 'passed'.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NoProcNoParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NoProcNoParams Procedure 
PROCEDURE NoProcNoParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input  param pv-prog as char no-undo.
def output param pv-ok as log no-undo.

{{&core}run.i &programc  = pv-prog
              &noper     = true
              &Appsrv    = "System"}

pv-ok = NOT haderrors().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NoProcParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NoProcParams Procedure 
PROCEDURE NoProcParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input  param pv-prog as char no-undo.
def input  param pv-params as char no-undo.
def output param pv-ok as log no-undo.

{{&core}run.i &programc  = pv-prog
              &noper     = true
              &Appsrv    = "System"
              &params    = "(pv-params)"}
pv-ok = NOT haderrors().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcNoParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcNoParams Procedure 
PROCEDURE ProcNoParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input  param pv-prog as char no-undo.
def input  param pv-proc as char no-undo.
def output param pv-ok as log no-undo.

{{&core}run.i &programc  = pv-prog
              &noper     = true
              &Appsrv    = "System"
              &procedurec = pv-proc}
pv-ok = NOT haderrors().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcParams Procedure 
PROCEDURE ProcParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-prog as char no-undo.
def input  param pv-proc as char no-undo.
def input  param pv-params as char no-undo.
def output param pv-ok as log no-undo.

{{&core}run.i &programc  = pv-prog
              &noper     = true
              &Appsrv    = "System"
              &procedurec = pv-proc
              &params    = "(pv-params)"}
pv-ok = NOT haderrors().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Run-Task) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Run-Task Procedure 
PROCEDURE Run-Task :
DEF INPUT PARAM pv-taskid AS INT NO-UNDO.
    DEF VAR lv-success      AS LOG NO-UNDO. 

    FIND FIRST zen-task WHERE zen-task.taskid = pv-taskid NO-LOCK NO-ERROR.
    MAIN-BLOCK:
    DO on error undo, retry:
        if retry THEN DO:
           put-log(pv-logfile,program-name(1) + '  Run Statement Failed for Task' + string(pv-taskid)).
           update-task-status('{&tfailed}',pv-taskid).
           update-task-message('Run Statement Failed',INT(ENTRY(n,pv-task))).
           create-history(pv-taskid, vd-start-date, lv-start-time).
           delete-task(pv-taskid).
           return.
        END.

        IF AVAIL zen-task THEN DO:
            ASSIGN vd-start-date = TODAY
                   lv-start-time = TIME.

            put-log(pv-logfile,program-name(1) + ' Running Task ' + string(zen-task.taskid)).
            update-task-message('About to Run', zen-task.taskid).

if zen-task.tasktype = '{&treport}' 
then run BatchReport(lc(zen-task.taskpgm),zen-task.taskproc,zen-task.parameters,output lv-success).
else lv-success = RunJob(lc(zen-task.taskpgm),zen-task.taskproc,zen-task.parameters).

put unformatted lv-success '*' zen-task.taskpgm '*' zen-task.taskproc '*' zen-task.parameters skip .

            RUN REFRESH IN h-parser(zen-task.schedule,
                                    TODAY,
                                    TIME,
                                    NO,
                                    OUTPUT lv-converted,
                                    OUTPUT vd-sched-date, 
                                    OUTPUT lv-sched-time, 
                                    OUTPUT lv-ended, 
                                    OUTPUT lv-problem, 
                                    OUTPUT lv-ending,
                                    OUTPUT lv-iterate).
            
            IF NOT lv-iterate AND vd-sched-date = zen-task.rundate 
            THEN DO:
               /* not iterating inside one day - so increment day to look forward from by one
                  and get next schedule date/time */
               RUN REFRESH IN h-parser(zen-task.schedule,
                                       TODAY + 1,
                                       0,
                                       NO,
                                       OUTPUT lv-converted,
                                       OUTPUT vd-sched-date, 
                                       OUTPUT lv-sched-time, 
                                       OUTPUT lv-ended, 
                                       OUTPUT lv-problem, 
                                       OUTPUT lv-ending,
                                       OUTPUT lv-iterate).
            END. /* not iterating around inside one day - get next date/time */

            /* if blank schedule is only a one off run - lv-ended will never be set as will never end in effect - trust me ! :D */
            ASSIGN lv-ended = zen-task.schedule = ''.
            IF NOT lv-ended THEN ASSIGN lv-ended = vd-sched-date = ?.

            IF lv-success THEN DO:
                update-task-status('{&tcomplete}',pv-taskid).
                update-task-message('Run Successfully',pv-taskid).

                /* otherwise is an iterating task and if completed ok - get next run-date/time */
                IF NOT lv-ended             
                THEN update-run-info(pv-taskid, vd-sched-date, lv-sched-time).
                                
                IF NOT lv-iterate 
                THEN put-log(pv-logfile,program-name(1) + ' Task ' + string(zen-task.taskid) + ' Completed Successfully').
                ELSE DO:
                   UPDATE-task-cycle(pv-taskid).
                   put-log(pv-logfile,program-name(1) + ' Iteration ' + STRING(zen-task.cycle) + ' of Task ' + 
                                      string(zen-task.taskid) + ' Completed Successfully').
                END. /* task iterating */

                create-history(pv-taskid, vd-start-date, lv-start-time).
                
                IF lv-ended 
                    THEN DELETE-task(pv-taskid).
                    ELSE update-task-status('{&twaiting}',pv-taskid). 
            END. /* success */
            ELSE DO:
                update-task-status('{&tfailed}',pv-taskid).
                update-task-message('Problems Encountered During Run',pv-taskid).

                IF NOT lv-iterate 
                THEN put-log(pv-logfile,program-name(1) + ' Error Occured During Run of Task ' + string(zen-task.taskid)).
                ELSE DO:
                    UPDATE-task-cycle(pv-taskid).
                    put-log(pv-logfile,program-name(1) + ' Error Occured During Run of Task:' + string(zen-task.taskid) +
                                       ' Iteration: ' + STRING(zen-task.cycle)).
                END. /* iterating task */

                create-history(pv-taskid, vd-start-date, lv-start-time).

                IF lv-ended 
                    THEN DELETE-task(pv-taskid).
                    ELSE update-task-status('{&twaiting}',pv-taskid).
            END. /* problem */
        END. /* avail b-zen-task */
        ELSE lv-success = NO.
    END. /* main-block */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-RunJob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RunJob Procedure 
FUNCTION RunJob RETURNS LOGICAL
  ( pv-prog as char,
    pv-proc as char,
    pv-params as char) :

def var lv-ok as log no-undo.

case pv-proc:
   when '' then do:
      case pv-params:
         when '' then run noprocnoparams (pv-prog,output lv-ok).
         otherwise    run noprocparams (pv-prog,pv-params,output lv-ok).
      end case.
   end.
   otherwise do:
      case pv-params:
         when '' then run procnoparams (pv-prog,pv-proc,output lv-ok).
         otherwise    run procparams (pv-prog,pv-proc,pv-params, output lv-ok).
      end case.
   end.
end case.

return lv-ok.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

