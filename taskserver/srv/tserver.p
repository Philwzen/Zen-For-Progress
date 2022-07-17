&glob serverprogram true
/* passed in session:param - '^' seperated */  
/* entry one is 'local' */
DEF VAR lv-tserver  AS CHAR   no-undo. /* task server name */
DEF VAR lv-logfile  AS CHAR   NO-UNDO. /* task server log file */
DEF VAR lv-db       AS CHAR   NO-UNDO. /* database to connect to */
def var pv-context  as char   no-undo.
/*******************************************************************/
{{&core}taskserver/paramdelim.i}
                                
DEF VAR lv-error      AS CHAR NO-UNDO. /* error message */
ASSIGN lv-tserver  = ENTRY(2,SESSION:PARAMETER,lv-delim)
       lv-logfile  = ENTRY(3,SESSION:PARAMETER,lv-delim)
       lv-db       = ENTRY(4,SESSION:PARAMETER,lv-delim)
       pv-context  = ENTRY(5,SESSION:PARAMETER,lv-delim).
if lv-logfile = '' then lv-logfile = "logs/" + lv-tserver + '.log'.
/* ATTEMPT TO CONNECT TO DB - LOG EVENTS ******************************/

OUTPUT to VALUE(lv-logfile).
OUTPUT CLOSE.

RUN put-log("Task Server:Connecting to " + lv-db).
RUN {&core}taskserver/connect-db.p(lv-db, OUTPUT lv-error).

IF lv-error ne '' THEN do:
   RUN put-log("Task Server: DB Connection Failed ").
   RUN put-log(lv-db).
   RUN put-log(lv-error).
   OUTPUT CLOSE.
   QUIT.
END.
ELSE RUN put-log("Task Server: DB Connection Established ").



/***********************************************************************/

/* CONNECTION ESTABLISHED */
RUN {&core}taskserver/srv/taskserver.p (lv-tserver,lv-logfile,pv-context) no-error.
if error-status:error 
then RUN put-log("Task Server: Failed Startup " + Error-Status:Get-Message(Error-Status:Num-Messages)).
RUN put-log("Task Server: Quiting").

quit. /* should be when returns from taskserver.p */


/* PROCEDURES */

PROCEDURE put-log:
    DEFINE INPUT PARAMETER ip-message AS CHAR NO-UNDO.
    OUTPUT to VALUE(lv-logfile) append.
    put unformatted /*    fill("=",70) skip */
      ip-message " " today " " string(time,"HH:MM am")skip
    /* fill("-",70) skip(1) */ . 
    OUTPUT CLOSE.
END. /* put-log */

