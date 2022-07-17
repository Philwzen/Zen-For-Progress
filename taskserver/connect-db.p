/* DB Connect Library */

DEF input  param lv-db       AS CHAR   NO-UNDO. /* database to connect to */
DEF OUTPUT PARAM lv-ERROR    AS CHAR   NO-UNDO. /* ERROR MESSAGE */
CONNECT -pf value(lv-db) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
lv-ERROR = 'Error ' + Error-Status:Get-Message(Error-Status:Num-Messages).
