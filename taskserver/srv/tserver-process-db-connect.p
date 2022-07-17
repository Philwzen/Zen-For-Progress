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

/* Task Server - APK 04/01 - Connects DB and fires off the task server program */

&glob serverprogram true
{app-paths.i justvars = true}


{{&tsk}paramdelim.i}

DEF STREAM out-s.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-put-log) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD put-log Procedure 
FUNCTION put-log RETURNS LOGICAL
    (ip-logfile AS CHAR, ip-message AS CHAR) FORWARD.

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

/* passed in session:param - '^' seperated */  
/* entry one is 'local' */
DEF VAR lv-tserver  AS CHAR   no-undo. /* task server name */
DEF VAR lv-task     AS CHAR   NO-UNDO. /* task id of task(s) to run */
DEF VAR lv-logfile  AS CHAR   NO-UNDO. /* task server log file */
DEF VAR lv-db       AS CHAR   NO-UNDO. /* database to connect to */
DEF VAR lv-logical  AS CHAR   NO-UNDO. /* db logical name */
DEF VAR lv-user     AS CHAR   NO-UNDO. /* db username */
DEF VAR lv-pass     AS CHAR   NO-UNDO. /* db password */
def var lv-context as char no-undo.

/*******************************************************************/

DEF VAR lv-error      AS CHAR NO-UNDO. /* error message */

ASSIGN lv-tserver  = ENTRY(2,SESSION:PARAMETER,lv-delim)
       lv-task     = ENTRY(3,SESSION:PARAMETER,lv-delim)
       lv-logfile  = ENTRY(4,SESSION:PARAMETER,lv-delim)
       lv-db       = ENTRY(5,SESSION:PARAMETER,lv-delim) 
       lv-context =  ENTRY(6,SESSION:PARAMETER,lv-delim).


if lv-logfile = '' then lv-logfile = '{&logs}' + lv-tserver + '.log'.
put-log(lv-logfile,program-name(1) +  'Attempting db connect ' + lv-db).
/* ATTEMPT TO CONNECT TO DB - LOG EVENTS ******************************/
RUN {&tsk}connect-db.p(lv-db, OUTPUT lv-error).

IF lv-error ne '' THEN
do:
   put-log(lv-logfile,program-name(1) +  "** Process: DB Connection Failed on Parameters").
   put-log(lv-logfile,program-name(1) +  lv-db).
   put-log(lv-logfile,program-name(1) +  lv-error).
   put-log(lv-logfile,program-name(1) +  '** Process: Unable to Run Task ' + lv-task).
   QUIT.
END.

put-log(lv-logfile,program-name(1) +  "Starting Taskserver Process for Task " + lv-task).
RUN {&tsk}{&srv}taskserver-process.p(lv-tserver, lv-task, lv-logfile).
put-log(lv-logfile,program-name(1) +  "Finished taskserver Process for Task " + lv-task).
quit. /* should be when returns from taskserver.p */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-put-log) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION put-log Procedure 
FUNCTION put-log RETURNS LOGICAL
    (ip-logfile AS CHAR, ip-message AS CHAR):
    
    OUTPUT STREAM out-s to VALUE(ip-logfile) APPEND.
    put STREAM out-s unformatted
/*             fill("-",70) SKIP  */
            ip-message " " TODAY " " string(time,"HH:MM:SS") SKIP
/*             fill("-",70) skip  */ .
    OUTPUT STREAM out-s CLOSE.
    RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

