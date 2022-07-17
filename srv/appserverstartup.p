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
&glob serverprogram true
def input param pv-startupparam as char no-undo. 
/* ***************************  Definitions  ************************** */
{app-paths.i justvars = true}  
{{&tsk}taskservercodes.i}                        
{{&tsk}paramdelim.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-GetDlcBin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetDlcBin Procedure 
FUNCTION GetDlcBin RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MonitorRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MonitorRunning Procedure 
FUNCTION MonitorRunning RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NumberOfAppservers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NumberOfAppservers Procedure 
FUNCTION NumberOfAppservers RETURNS INTEGER
  ( pv-by as int )  FORWARD.

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
this-procedure:private-data = "LIBRARY-appserverstartup".

&if "{&opsys}" = "win32" &then
{{&tsk}{&srv}winsubmit.i}
&endif


if can-find (first zen-control where zen-control.ctrl-idx = "AppServerPreLoadTables"
                                 and zen-control.ctrl-data = 'true')
/* if pv-startupparam = ''  */
    then RUN loadtableprocedures in this-procedure.

/* program to monitor directories for various incoming files */

if not MonitorRunning() then do:
  /* run {&server}{&general}startmonitor.p. */
end.

if can-find (first zen-control where zen-control.ctrl-idx = "AutoStartTaskServers"
                                 and zen-control.ctrl-data = 'true')
then run StartTaskservers.

NumberOfAppservers(1).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-loadtableprocedures) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadtableprocedures Procedure 
PROCEDURE loadtableprocedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR lv-file AS CHAR NO-UNDO.
    
for each _file where _file-num > 0 
                 and _file-num < 32000
               no-lock:
    lv-file = search("{&core}{&srv}" + _file-name + '.p').
    if lv-file = ? then do:
        lv-file = search("{&sys}{&srv}" + _file-name + '.p').
        if lv-file = ? then next.
    End.
    RUN VALUE(lv-file) PERSIST no-error.

   
End.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartTaskServers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartTaskServers Procedure 
PROCEDURE StartTaskServers :
DEF BUFFER b-taskserver FOR zen-tserver.
DEF VAR lv-pid     AS INT NO-UNDO.
DEF VAR lv-com-line AS CHAR NO-UNDO.
def var vh-process as int no-undo.
def var lv-logfile as char no-undo.

   for each zen-tserver where autostart  
                        and not zen-tserver.started 
                        no-lock:
      lv-logfile = if zen-tserver.logfile = '' then '{&logs}' + zen-tserver.taskserver + '.log'
                                                 else zen-tserver.logfile.

      lv-com-line = getdlcbin() + '_progres' + 
                      ' -b -p ' + zen-tserver.ServerProg + 
                      ' -param local' + lv-delim +
                      zen-tserver.taskserver + lv-delim + 
                      lv-logfile + lv-delim +
                      zen-tserver.DBParameters + lv-delim /* + 
                       lv-session */.

    &if "{&OPSYS}" = 'unix' &then 
        lv-com-line = lv-com-line + ' &'.
        OS-COMMAND SILENT VALUE(lv-com-line).
    &else /* 'win32' */
        vh-process = createpro(lv-com-line,'',OUTPUT lv-pid). 
        DO TRANSACTION:
            FIND b-taskserver WHERE RECID(b-taskserver) = RECID(zen-tserver) EXCLUSIVE-LOCK.
            assign b-taskserver.tstatus = '{&tstarted}'
                   b-taskserver.pid = lv-pid
                   b-taskserver.started = true.
            FIND CURRENT b-taskserver NO-LOCK.
        END.
    &endif   
   
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetDlcBin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetDlcBin Procedure 
FUNCTION GetDlcBin RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
def var lv-dlc as char format 'x(20)' no-undo.

&if "{&opsys}" = "win32" &then
   GET-KEY-VALUE SECTION "startup" KEY "dlc" VALUE lv-dlc.
   lv-dlc = lv-dlc + "~\bin~\".
&else
   lv-dlc = OS-GETENV("DLC") + '/bin/'.
&endif

  RETURN lv-dlc.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MonitorRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MonitorRunning Procedure 
FUNCTION MonitorRunning RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
   return can-find(zen-control where zen-control.ctrl-idx = 'VtxMonitor'
                                 and zen-control.ctrl-data = 'up').
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NumberOfAppservers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NumberOfAppservers Procedure 
FUNCTION NumberOfAppservers RETURNS INTEGER
  ( pv-by as int ) :

def var lv-num as int no-undo.

if can-find (first zen-control where zen-control.ctrl-idx = "RunningAppservers")
then do:
   find first zen-control where zen-control.ctrl-idx = "RunningAppservers"
                          exclusive-lock.
   lv-num = int(zen-control.ctrl-data).
end.
else do:
   create zen-control.
   zen-control.ctrl-idx = "RunningAppservers".
end.
  zen-control.ctrl-data = string(lv-num + pv-by).
  release zen-control.

  RETURN lv-num + pv-by.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

