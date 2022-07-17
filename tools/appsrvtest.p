def var pv-connectstring as char no-undo init "pf/webappsrv.pf".
                                                              
def var h-appserver as handle no-undo.
def var lv-user as char init '' no-undo.
def var lv-password as char init '' no-undo.
def var lv-sessionid as char no-undo.
lv-sessionid = string(today,'99/99/9999') + ':00ee22aa55'.
if pv-connectstring ne '' and
   search(pv-connectstring) ne ? 
then do:
    CREATE SERVER h-appserver.
    IF NOT ERROR-STATUS:ERROR 
    THEN DO:     
        h-appserver:CONNECT("-pf " + pv-connectstring,lv-user,lv-password,lv-sessionid) No-error.
        If Error-status:error Then Do:
            message '1 Error ' Error-Status:Get-Message(Error-Status:Num-Messages) view-as alert-box.
            h-appserver = ?.           
        End.
        If valid-handle(h-appserver) and not h-appserver:connected() 
            or Error-status:error
        Then Do:
            message '2 Failed connect ' Error-Status:Get-Message(Error-Status:Num-Messages) view-as alert-box.
            h-appserver = ?.           
        End.
    End.
    ELSE do:
        message '3 ' Error-Status:Get-Message(Error-Status:Num-Messages) view-as alert-box.
        h-appserver = ?.
    end.
END.
else h-appserver = ?.
if valid-handle(h-appserver) 
then message '4 connected ' h-appserver:connected() skip
             ' Error ' error-status:error view-as alert-box.

