def var x as int no-undo.
Def Var h-qry   As Handle No-undo.
Def Var b-data  As Handle No-undo.
Def Var h-buff  As Handle No-undo.
Def Var h-field As Handle No-undo.
def var lv-prepstring as char no-undo.
def var array-extent as int no-undo.

If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().

DELETE OBJECT h-QrY no-error.

Create Query h-QrY.
Create Buffer b-data For Table pv-table no-error.
if error-status:error then do:
    pv-data = '** Error Invalid table ' + pv-table.
end.
else do:
    If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data).
    lv-prepstring = "For EACH  " + pv-table + "  where  " + pv-where + "  no-lock".
    
    h-QRY:QUERY-PREPARE(lv-prepstring) no-error.
    if error-status:error then do:
        pv-data = '** Error Invalid Query ' + lv-prepstring.
    end.
    else do:
        h-QRY:QUERY-OPEN no-error.
        if error-status:error then do:
           pv-data = '** Error On Query Open ' + lv-prepstring.
        end.
        else do:
            h-QRY:GET-last() no-error.
            if error-status:error then do:
               pv-data = '** Error On Get ' + lv-prepstring.
            end.
    
            if h-qry:num-results > 0 
            then do:
               if pv-datafield matches("*]") then do:
                assign
                  pv-datafield = substring(pv-datafield,1,length(pv-datafield) - 1)
                  array-extent = integer(entry(2,pv-datafield,"["))
                  pv-datafield = entry(1,pv-datafield,"[").         
               end.
            
                h-buff = h-qry:get-buffer-handle(1).
                do x = 1 to h-buff:num-fields:
                    h-field = h-buff:Buffer-field(X).
                    If h-field:name = pv-datafield then do:
                        if h-field:extent > 0 then pv-data = 
                           string(h-field:Buffer-value[array-extent]).
                        else pv-data = string(h-field:Buffer-value).
                        leave.
                    end.
            	else pv-data = '** Error Field Not Found ' + lv-prepstring.
                end.
            End.
            else pv-data = '** Error Record Not Found ' + lv-prepstring.
        end.
    end.
    DELETE OBJECT h-QrY no-error.  
end.

