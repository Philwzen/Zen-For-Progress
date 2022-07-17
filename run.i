def var h-aproghand as handle no-undo.
def var h-appserver as handle no-undo.
def var lv-rprog    as char   no-undo.
def var lv-rproc    as char   no-undo.
def var lv-rapps    as char   no-undo.
def var lv-rpath    as char   no-undo.
def var lv-noper    as log    no-undo.
def var lv-api      as char   no-undo.
def var lv-async    as log    no-undo.
def var lv-forcelocal    as log    no-undo.
def var lv-prop as char no-undo.
def var lv-vals as char no-undo.         

&if defined(api) = 2 &then
    &if defined(tname) = 2 &then
        lv-api   = "{&api}" + '{&Delim2}' + "{&tname}".
    &else
        lv-api   = "{&api}" + '{&Delim2}' + "{&Table-name}".
    &endif

/*     &if defined(UseApi) ne 0 &then       */
/*         lv-api = "{&table-name}" + lv-api.  */
/*     &endif                                  */
		
    if not GetAPIDetail(lv-api,output lv-prop,output lv-vals) then
        message 'Invalid API ' skip lv-api
        view-as alert-box error.
    else
    assign
       lv-rpath = getStringentry('Path',lv-prop,lv-vals,'{&Delim2}')       
       lv-rprog = getStringentry('Program',lv-prop,lv-vals,'{&Delim2}')
       lv-rproc = getStringentry('Procedure',lv-prop,lv-vals,'{&Delim2}')
       lv-rapps = getStringentry('AppSrv',lv-prop,lv-vals,'{&Delim2}')
       lv-noper = getStringentry('NoPer',lv-prop,lv-vals,'{&Delim2}') = 'yes'
       /* this next line is wrong for async calls */
       lv-async = getStringentry('Async',lv-prop,lv-vals,'{&Delim2}') = 'yes'.
&else
    assign 
       lv-rprog = "{&program}"
       lv-rproc = "{&procedure}"
       lv-rapps = "{&appsrv}"
       lv-rpath = "{&path}"
       lv-noper = "{&noper}" = "true"
       lv-async = "{&async}" ne ""
       lv-forcelocal = "{&force}" = "true". 
    &if defined(programc) = 2 &then
        if lv-rprog = '' then lv-rprog = {&programc}.
    &endif
    &if defined(procedurec) = 2 &then
        if lv-rproc = '' then lv-rproc = {&procedurec}.
    &endif
    {&procedurex}

    &if defined(appsrvc) = 2 &then
        if lv-rapps = '' then lv-rapps = {&appsrvc}.
    &endif
    &if defined(pathc) = 2 &then
        if lv-rpath = '' then lv-rpath = {&pathc}.
    &endif    
&endif

/* def var lv-lm as char no-undo. */
/* def var lv-lmcnt as int no-undo. */
/* lv-lm = lv-rprog + ' ' + lv-rproc + ",". */
/* do lv-lmcnt = 1 to 10: */
/*     if program-name(lv-lmcnt) ne ? */
/*     then lv-lm = lv-lm + program-name(lv-lmcnt) + ','. */
/*     else leave. */
/* end. */
/* logmessage(lv-lm,"{&logs}" + "run-i.log",'file'). */

/* maybe do usersec here? if not canrun(lv-rproc etc ) */
IF entry(1,session:parameter,'^') ne "local" 
      Then h-appserver = getappserverhandle(lv-rapps).
      else h-appserver = ?.
&if defined(async) ne 0 &then
    h-asyncserver = h-appserver.
&endif
IF VALID-HANDLE(h-appserver)
    THEN h-aproghand = h-appserver:first-PROCEDURE.
    ELSE h-aproghand = SESSION:first-PROCEDURE.

DO WHILE h-aproghand ne ?:
    IF h-aproghand:private-data = lv-rprog THEN LEAVE.
    h-aproghand = h-aproghand:next-SIBLING no-error.
END.

if not session:remote then SetSession('wait').
&if defined(nomess) = 0 &then
etime(true). 
&endif
/*
message lv-rpath skip lv-rprog skip lv-rproc skip
        '{&params}'
        {&dbt}.
*/
if RunRemote(lv-rpath + lv-rprog + ':' + lv-rproc + ":{&Params}") and
   not lv-forcelocal
then do: /* client session */
    &if (defined(procedure)  <> 0 or 
         defined(procedurec) <> 0 or 
         defined(api) <> 0) 
    &then /* proper style apsrv call */
        IF h-aproghand = ? THEN DO:
	        lv-noper = true.
            IF VALID-HANDLE(h-appserver) And h-appserver:Connected()
            THEN do:
                RUN VALUE(LC(lv-rpath + lv-rprog)) ON h-appserver
                         TRANSACTION DISTINCT PERSISTENT SET h-aproghand
                no-error.
            End.
            else do:
                if num-dbs > 0 or lv-forcelocal 
                    then RUN VALUE(LC(lv-rpath + lv-rprog)) PERSISTENT SET h-aproghand 
                          no-error.
                    else assign h-aproghand = ?
                                error-status:error = true.
            End.
            if valid-handle(h-aproghand) 
            then h-aproghand:PRIVATE-DATA = lv-rprog.
        END.
        if not error-status:error then DO:
            RUN value(lv-rproc) IN h-aproghand {&async} {&Params} no-error.
            if lv-noper and not lv-async
                then  delete procedure h-aproghand.
        end.
    &else /* run it non pers with input params yuck! */
        IF VALID-HANDLE(h-appserver) And h-appserver:Connected()
        THEN do:
            RUN VALUE(LC(lv-rpath + lv-rprog)) ON h-appserver
               TRANSACTION DISTINCT {&async} {&Params} no-error.
        end.
        else do:
            if num-dbs > 0 or lv-forcelocal /* thick client connection */
                then RUN VALUE(LC(lv-rpath + lv-rprog)) {&Params} no-error.
                    else assign h-aproghand = ?
                                error-status:error = true.
        End.
    &endif
    &if defined(MaximiseLicenses) ne 0 &then
        IF VALID-HANDLE(h-appserver) And 
           h-appserver:Connected() and
           not lv-async
             then h-appserver:disconnect(). 
    &endif
End.
else do: /* run it local as we are already on appserver */
    &if (defined(procedure)  <> 0 or 
         defined(procedurec) <> 0 or
         defined(api) <> 0) 
    &then /* proper style apsrv call */
        h-appserver = ?.
        if not valid-handle(h-aproghand) 
        then RUN VALUE(LC(lv-rpath + lv-rprog)) PERSISTENT SET h-aproghand no-error.
        if valid-handle(h-aproghand) 
        then h-aproghand:PRIVATE-DATA = lv-rprog.
        RUN value(lv-rproc) IN h-aproghand {&async} {&Params} no-error.
	 if lv-noper and not lv-async and valid-handle(h-aproghand)
        then delete procedure h-aproghand no-error.
    &else /* run it non pers with input params yuck! */
        RUN VALUE(LC(lv-rpath + lv-rprog)) {&Params} no-error.
    &endif
End.

if not session:remote then SetSession('').

if (error-status:error and
   not session:remote) 
then Message 'Appserver handle :' h-appserver Skip
             'Failed in        :' program-name(1) skip
             'Program          :' lv-rprog skip
             'Procedure        :' lv-rproc skip
             'Params           :' "{&Params}" Skip
             'Error Was        :' Error-Status:Get-Message(Error-Status:Num-Messages) skip
             'Return-Value     :' return-value    skip
             '******* Calling Tree *******' skip
             Program-name(1) ':' Program-name(2) ':' skip
             Program-name(3) ':' Program-name(4) ':' skip
             Program-name(5) ':' Program-name(6) ':' skip
     View-as Alert-box error title "Please Print This Screen and Call Tech Support".
&if defined(nomess) = 0 &then
 DispExecMess(String(etime / 1000)).
&endif
