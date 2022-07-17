&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
&glob serverprogram true
/* ***************************  Definitions  ************************** */

{app-paths.i}
&glob table-name zen-context
&glob unique-key {&table-name}tableid 

{{&core}def-table.i &table-name = {&table-name}}
define query q-{&table-name} for {&table-name} scrolling.


define temp-table t-error no-undo
    field t-code    as int  column-label "Code"
    field t-message as char column-label 'Message'
    field t-Display as log  column-label 'Display'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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
         HEIGHT             = 20.38
         WIDTH              = 46.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}mainp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Clear-Table) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Clear-Table Procedure 
PROCEDURE Clear-Table :
empty temp-table t-{&table-name}.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearErrors Procedure 
PROCEDURE ClearErrors :
def input param pv-type as char no-undo.
def var lv-session as char no-undo.
lv-session = trim(sessionid()).
for each {&table-name} where Zen-Context.server-connection-id  = lv-session
                           and Zen-Context.Type = pv-type 
                         exclusive-lock:
    delete {&table-name}.
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-error) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-error Procedure 
PROCEDURE create-error :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-mesnum as int  no-undo.
def input param pv-type   as char no-undo.
def input param pv-extra1 as char no-undo.
def input param pv-extra2 as char no-undo.
def input param pv-extra3 as char no-undo.
def input param pv-extra4 as char no-undo.

def var pv-mestxt as char no-undo.
def var lv-session as char no-undo.
lv-session = trim(sessionid()).

pv-mestxt = msg(pv-mesnum,pv-extra1,pv-extra2,pv-extra3,pv-extra4).

create {&table-name}.
assign Zen-Context.ContextValue = string(pv-mesnum) + '{&Delim2}' +  pv-mestxt
       Zen-Context.server-connection-id  = lv-session
       Zen-Context.Type = pv-type.
LogMessage(pv-mestxt,'','').
release {&table-name}.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Delete-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete-record Procedure 
PROCEDURE Delete-record :
/* USES MANDATORY PARAMETERS
def input param pv-recid as recid no-undo.
def input-output param table for t-{&table-name}.
*/
   {{&core}apsrv-deleterecord.i}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteAll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteAll Procedure 
PROCEDURE DeleteAll :
def var lv-session as char no-undo.
lv-session = trim(sessionid()).
for each {&table-name} where Zen-Context.server-connection-id = lv-session 
                          or Zen-Context.server-connection-id = ''
                       exclusive-lock:
    delete zen-context.
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteSession Procedure 
PROCEDURE DeleteSession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    */

   
def input  param  lv-session as char no-undo.
for each {&table-name} where Zen-Context.server-connection-id = lv-session 
                       exclusive-lock:
    delete zen-context.
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteVar Procedure 
PROCEDURE DeleteVar :
def input param pv-key as char no-undo.
def var lv-session as char no-undo.

assign
    pv-key     = trim(pv-key)
    lv-session = trim(sessionid()).
find first Zen-Context where Zen-Context.server-connection-id = lv-session
                        and Zen-Context.type = pv-key  
                      exclusive-lock no-error.
  if avail Zen-Context then delete Zen-Context.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Get-Records) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Records Procedure 
PROCEDURE Get-Records :
/* USES MANDATORY PARAMETERS YOU NEED NOT WORRY ONLY 
def input param pv-direction as char no-undo.
def input  param pv-rowid     as recid no-undo.
def output param table        for  t-{&table-name}.
   can use &whereclause as where clause and &by for sorting
   e.g. &whereclause = "where {&table-name}.class = pv-class"       
        &by          = "by {&table-name}.name"                                                   
********************************************************/
def input param pv-all as log  no-undo.
def var lv-session as char no-undo.

if not pv-all
then lv-session = trim(sessionid()).
else lv-session = '*'.

   {{&core}apsrv-getrecords.i 
   &whereclause = "where {&table-name}.server-connection-id matches lv-session "
   &by = "by {&table-name}.server-connection-id"}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetErrors Procedure 
PROCEDURE GetErrors :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-type as char no-undo.
def output param table for t-error.

empty temp-table t-error.

def var lv-session as char no-undo.
lv-session = trim(sessionid()).
for each {&table-name} where Zen-Context.server-connection-id = lv-session
                         and Zen-Context.Type = pv-type
                       no-lock:
    create t-error.
    assign t-error.t-code    = int(entry(1,Zen-Context.ContextValue,'{&Delim2}'))
           t-error.t-message = entry(2,Zen-Context.ContextValue,'{&Delim2}')
           t-error.t-Display = true.
end.
if pv-type ne 'error' then
for each {&table-name} where Zen-Context.server-connection-id = lv-session
                         and Zen-Context.Type = pv-type
                       exclusive-lock:
   delete {&table-name}.
end.

return return-value. 
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetVar Procedure 
PROCEDURE GetVar :
def input  param pv-key   as char no-undo.
def output param pv-value as char no-undo.
def var lv-session as char no-undo.

assign
    pv-key     = trim(pv-key)
    lv-session = trim(sessionid()).  /* zenlibrary.p */

if not can-find(first {&table-name} where Zen-Context.server-connection-id = lv-session )
then do:
   pv-value = '** System Reset*' + lv-session + '*' + pv-key + '*' + string(pv-value).
   return.
end.

  find first {&table-name} where Zen-Context.server-connection-id = lv-session
                             and Zen-Context.type = pv-key 
                        no-lock no-error.
  if avail {&table-name} 
      then pv-value = Zen-Context.ContextValue.       
      else pv-value = ?.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HadAnError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HadAnError Procedure 
PROCEDURE HadAnError :
def input param pv-type as char no-undo.
def output param pv-err as log no-undo.
def var lv-session as char no-undo.
lv-session = trim(sessionid()).
pv-err = can-find(first zen-context where Zen-Context.server-connection-id  = lv-session
                                      and Zen-Context.Type = pv-type) .

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Procedure 
PROCEDURE open-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param table for t-error.

empty temp-table t-error.

def var lv-session as char no-undo.
lv-session = trim(sessionid()).
for each {&table-name} where {&table-name}.server-connection-id = lv-session
                         and {&table-name}.Type = "error"
                       no-lock:
    create t-{&table-name}.
    assign t-error.t-code    = ?
           t-error.t-message = {&table-name}.ContextValue
           t-error.t-Display = true.
end.
return return-value. 
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Save-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Save-record Procedure 
PROCEDURE Save-record :
/* USES MANDATORY PARAMETERS
def input param pv-recid as recid no-undo.
def input-output param table for t-{&table-name}.
*/
   {{&core}apsrv-saverecord.i}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetVar Procedure 
PROCEDURE SetVar :
def input param pv-key   as char no-undo.
def input param pv-value as char no-undo.
def var lv-session as char no-undo.

if index(pv-key,'=') ne 0
then do:
    run setvars (pv-key).
    return.
end.

assign
    pv-key     = trim(pv-key)
    lv-session = trim(sessionid()).
  find first {&table-name} where {&table-name}.server-connection-id = lv-session
                             and {&table-name}.type = pv-key 
                           exclusive-lock no-error.

  if not avail Zen-Context
      then create Zen-Context.

  assign Zen-Context.server-connection-id = lv-session
         Zen-Context.type  = pv-key
         Zen-Context.contextvalue = pv-value.
         
  release Zen-Context.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetVars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetVars Procedure 
PROCEDURE SetVars :
def input param pv-params as char no-undo.

def var lv-var as char no-undo.
def var lv-value as char no-undo.
def var x as int no-undo.
do x = 1 to num-entries(pv-params):
    lv-var  = entry(1,entry(x,pv-params),'=').
    lv-value = entry(2,entry(x,pv-params),'=').
    run setvar (lv-var,lv-value).
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

