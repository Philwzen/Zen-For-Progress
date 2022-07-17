Procedure MassUpdate:
    def var lv-where as char no-undo.

    run local-massupdate in this-procedure no-error.

    if not error-status:error
    then do:
        if return-value = 'override' 
        then return.
        else lv-where = return-value.
    end.
    else lv-where = 'where true'.

    run assignframes.

    empty temp-table ts-{&table-name}.    
    create ts-{&table-name}.
    buffer-copy t-{&table-name} to ts-{&table-name}.

    errorclear().


    {{&core}run.i &program   = "dynamic.p"
                &path      = "{&core}{&srv}"
                &Appsrv    = "System"
                &procedure = "massupdate"
                &params    = "('{&table-name}',
                               lv-where,
                               input table ts-{&table-name},
                               input table tb-{&table-name})"}


    run undo-trigger.
    run openquery.
end procedure.
