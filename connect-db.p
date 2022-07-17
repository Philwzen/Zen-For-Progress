/* DB Connect Library */

DEF input  param vc-db       AS CHAR   NO-UNDO. /* database to connect to */
DEF input  param vc-logical  AS CHAR   NO-UNDO. /* db logical name */
DEF input  param vc-user     AS CHAR   NO-UNDO. /* db username */
DEF input  param vc-pass     AS CHAR   NO-UNDO. /* db password */
DEF OUTPUT PARAM VC-ERROR    AS CHAR   NO-UNDO. /* ERROR MESSAGE */

CONNECT value(vc-db) -ldb value(vc-logical) -U value(vc-user) -P value(vc-pass) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
VC-ERROR = 'Error ' + Error-Status:Get-Message(Error-Status:Num-Messages).
