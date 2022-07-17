&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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
DEFINE STREAM out-s.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Create-History Include 
FUNCTION Create-History RETURNS LOGICAL
    (ip-task AS INT, ip-start AS DATE, ip-start-time AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Delete-Task Include 
FUNCTION Delete-Task RETURNS LOGICAL
    (ip-id AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Put-Log Include 
FUNCTION Put-Log RETURNS LOGICAL
    (ip-logfile AS CHAR, ip-message AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Update-Run-Info Include 
FUNCTION Update-Run-Info RETURNS LOGICAL
    (ip-id AS INT, vd-date AS DATE, lv-time AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Update-Server-Status Include 
FUNCTION Update-Server-Status RETURNS LOGICAL
    (ip-server AS CHAR, ip-status AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Update-Task-Cycle Include 
FUNCTION Update-Task-Cycle RETURNS LOGICAL
    (ip-id AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Update-Task-Message Include 
FUNCTION Update-Task-Message RETURNS LOGICAL
    (ip-msg AS CHAR, ip-id AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Update-Task-Status Include 
FUNCTION Update-Task-Status RETURNS LOGICAL
    (ip-status AS CHAR, ip-id AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Create-History Include 
FUNCTION Create-History RETURNS LOGICAL
    (ip-task AS INT, ip-start AS DATE, ip-start-time AS INT):         
    FIND FIRST zen-task WHERE zen-task.taskid = ip-task NO-LOCK NO-ERROR.
    /* create task history */
    IF AVAIL zen-task THEN
    DO TRANSACTION:
        CREATE zen-taskhist.
        BUFFER-COPY zen-task TO zen-taskhist.
        ASSIGN zen-taskhist.startdate  = ip-start
               zen-taskhist.starttime  = ip-start-time
               zen-taskhist.enddate    = TODAY
               zen-taskhist.endtime    = TIME.
        RELEASE zen-taskhist NO-ERROR.
        RETURN YES.
    END. /* TRANSACTION */
    RETURN NO.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Delete-Task Include 
FUNCTION Delete-Task RETURNS LOGICAL
    (ip-id AS INT):

    FIND FIRST zen-task WHERE zen-task.taskid = ip-id EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL zen-task THEN
    DO:
       DELETE zen-task.
       RETURN YES.
    END.
    ELSE RETURN NO.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Put-Log Include 
FUNCTION Put-Log RETURNS LOGICAL
    (ip-logfile AS CHAR, ip-message AS CHAR):
    
    OUTPUT STREAM out-s to VALUE(ip-logfile) APPEND.
    put STREAM out-s unformatted
/*             fill("-",70) SKIP  */
            ip-message " " TODAY " " string(time,"HH:MM:SS") SKIP.
/*             fill("-",70) skip.  */
    OUTPUT STREAM out-s CLOSE.
    RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Update-Run-Info Include 
FUNCTION Update-Run-Info RETURNS LOGICAL
    (ip-id AS INT, vd-date AS DATE, lv-time AS INT):
    
    FIND FIRST zen-task WHERE zen-task.taskid = ip-id EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL zen-task THEN
    DO:
        ASSIGN zen-task.rundate = vd-date
               zen-task.runtime = lv-time.
        FIND CURRENT zen-task NO-LOCK NO-ERROR.
        RETURN YES.
    END.
    ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Update-Server-Status Include 
FUNCTION Update-Server-Status RETURNS LOGICAL
    (ip-server AS CHAR, ip-status AS CHAR):

    FIND FIRST zen-tserver WHERE zen-tserver.taskserver = ip-server
    EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL zen-tserver THEN
    DO:
       ASSIGN zen-tserver.tstatus = ip-status.
       FIND CURRENT zen-tserver NO-LOCK NO-ERROR.
       RETURN YES.
    END.
    ELSE RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Update-Task-Cycle Include 
FUNCTION Update-Task-Cycle RETURNS LOGICAL
    (ip-id AS INT):
    
    FIND FIRST zen-task WHERE zen-task.taskid = ip-id EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL zen-task THEN
    DO:
       ASSIGN zen-task.cycle = zen-task.cycle + 1.
       FIND CURRENT zen-task NO-LOCK.
       RETURN YES.
    END.
    ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Update-Task-Message Include 
FUNCTION Update-Task-Message RETURNS LOGICAL
    (ip-msg AS CHAR, ip-id AS INT):

    FIND FIRST zen-task WHERE zen-task.taskid = ip-id EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL zen-task THEN
    DO:
       ASSIGN zen-task.tmessage = ip-msg.
       FIND CURRENT zen-task NO-LOCK NO-ERROR.
       RETURN YES.
    END.
    ELSE RETURN NO.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Update-Task-Status Include 
FUNCTION Update-Task-Status RETURNS LOGICAL
    (ip-status AS CHAR, ip-id AS INT):

    FIND FIRST zen-task WHERE zen-task.taskid = ip-id EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL zen-task THEN
    DO:
       ASSIGN zen-task.tstatus = ip-status.
       FIND CURRENT zen-task NO-LOCK NO-ERROR.
       RETURN YES.
    END.
    ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

