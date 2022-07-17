def var x as int no-undo.
Def Var h-qry   As Handle No-undo.
Def Var b-data  As Handle No-undo.
Def Var h-buff  As Handle No-undo.
Def Var h-field As Handle No-undo.
def var lv-prepstring as char no-undo init "FOR ".
def var lv-fld as char no-undo.

If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().
If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
Create Query h-QrY.

do x = 1 to num-entries(pv-table,'{&Delim2}'):
    Create Buffer b-data For Table entry(x,pv-table,'{&Delim2}').
    If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data). 
    lv-prepstring = lv-prepstring + "EACH " + entry(x,pv-table,'{&Delim2}') 
                    + ' ' + entry(x,pv-Where,'{&Delim2}') + " no-lock, ".
end.

lv-prepstring = substring(lv-prepstring,1,length(lv-prepstring) - 2) + " " + pv-By + '.'.

h-QRY:QUERY-PREPARE(lv-prepstring) no-error.
if error-status:error then do:
    pv-codes = '** Error Invalid Query ' + lv-prepstring.
    pv-values = pv-codes.
end.
else do:
   h-QRY:QUERY-OPEN no-error.
   if error-status:error then do:
           pv-codes = '** Error on Open Query ' + lv-prepstring.
           pv-values = pv-codes.
   end.
   else do:
        h-buff = h-qry:get-buffer-handle(num-entries(pv-table)).
        h-QRY:GET-NEXT().
        DO while not h-QRY:Query-off-end:
            lv-fld = '[]'.
             do x = 1 to h-buff:num-fields:
                h-field = h-buff:Buffer-field(X).
                If h-field:name = pv-key  then do:
                    if lookup(string(h-field:Buffer-value),pv-codes,"{&comboDelim}") ne 0
                       then next.
                    if pv-codes = "" then pv-codes = 
                       string(h-field:Buffer-value).
                    else pv-codes  =
                       pv-codes + "{&ComboDelim}" + string(h-field:Buffer-value).
                end.
                if num-entries(pv-field,'{&Delim2}') < 2 then do:
                    If h-field:name = pv-field then do:
                        if pv-values = ""
                            then pv-values = string(h-field:Buffer-value).
                        else pv-values = pv-values + "{&ComboDelim}" + string(h-field:Buffer-value).
                    end.
                end.
                else do:
                    If h-field:name = ENTRY(1,pv-field,'{&Delim2}')
                    then ENTRY(1,LV-FLD,'[') = string(h-field:Buffer-value).
                    If h-field:name = ENTRY(2,pv-field,'{&Delim2}')
                    then ENTRY(2,LV-FLD,'[') = caps(string(h-field:Buffer-value)) + ']'.
                end.
            end.
            if num-entries(pv-field,'{&Delim2}') > 1 
            then do:
                if pv-values = "" 
                then pv-values = lv-fld.
                else pv-values = pv-values + "{&ComboDelim}" + lv-fld.
            end.   
            h-QRY:GET-NEXT().
        end.
        /* ak swapped code and calues around here as was wrong... */
        if pv-wild then
                assign pv-values = '{&All-text}{&ComboDelim}' + pv-values
                   pv-codes  = '*{&ComboDelim}' + pv-codes.
        
        if pv-none then assign
           pv-values = '{&none-text}{&ComboDelim}' + pv-values
           pv-codes   = '{&ComboDelim}' + pv-codes.
        else if pv-none = ? then assign
           pv-values = pv-values + '{&ComboDelim}{&none-text}' 
           pv-codes  = pv-codes + '{&ComboDelim}'.
   end.
end.

  If Valid-handle(h-QrY) Then    
       DELETE OBJECT h-QrY no-error. 
