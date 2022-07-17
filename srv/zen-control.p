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
01/19/10  Annerik   Set memptrs to zero to avoid Progress bug.
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
&glob serverprogram true
/* ***************************  Definitions  ************************** */
{app-paths.i}




/* changeable bits here change table-name to appropriate DB table */
&glob table-name zen-control
&glob unique-key {&table-name}tableid 

{{&core}def-table.i &table-name = {&table-name}}

define query q-{&table-name} for {&table-name} scrolling.

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
         HEIGHT             = 22.48
         WIDTH              = 50.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}mainp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-clear-table) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-table Procedure 
PROCEDURE clear-table :
for each t-{&table-name}:
    delete t-{&table-name}.
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-record Procedure 
PROCEDURE delete-record :
/* USES MANDATORY PARAMETERS
def input param pv-recid as recid no-undo.
def input-output param table for t-{&table-name}.
*/
   {{&core}apsrv-deleterecord.i}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Delete-Related-Tables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete-Related-Tables Procedure 
PROCEDURE Delete-Related-Tables :
/*------------------------------------------------------------------------------
  Purpose:     delete any child tables of this parent
  Parameters:  <none>
  Notes:       leave blank if not required    
------------------------------------------------------------------------------*/
/* EXAMPLE
   find prod-matrix of product exclusive-lock no-error.
   if avail prod-matrix 
      then delete prod-matrix.
   for each prod-curr of product exclusive-lock:
      delete prod-cur.
   end.
   */
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-validation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-validation Procedure 
PROCEDURE delete-validation :
/*------------------------------------------------------------------------------
  Purpose:     check ok to delete record
  Parameters:  <none>
  Notes:       return 'passed' if it's OK to delete.
------------------------------------------------------------------------------*/
/* example 
   if can-find(first ledger-item of product) then return 'failed'.
*/
    return 'passed'.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-find-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-record Procedure 
PROCEDURE find-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-code as char no-undo.

   {{&core}apsrv-findrecord.i 
        &where = "where zen-control.ctrl-idx = pv-code"}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-records) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-records Procedure 
PROCEDURE get-records :
/* USES MANDATORY PARAMETERS YOU NEED NOT WORRY ONLY 
def input param pv-direction as char no-undo.
def input  param pv-rowid     as recid no-undo.
def output param table        for  t-{&table-name}.

   can use &whereclause as where clause and &by for sorting
      e.g.  &where = "where {&table-name}.class = pv-class"       
        &by          = "by {&table-name}.name"                                                   
********************************************************/
def input  param lv-sysman as log no-undo.
def input param pv-direction as char no-undo.
def input  param pv-id     like {&table-name}.{&unique-key} no-undo.
def output param table        for  t-{&table-name}.

if not lv-sysman
then run normalget(pv-direction,pv-id,output table t-{&table-name}).
else run sysmanget(pv-direction,pv-id,output table t-{&table-name}).


end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetBlobControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetBlobControl Procedure 
PROCEDURE GetBlobControl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-idx   as char no-undo.
def output param pv-blob  as memptr no-undo.
def output param pv-value as char no-undo.


/* make sure there is nothing left over in the memory pointer! */
set-size(pv-blob) = 0.

def buffer b-zen-control for zen-control.
{{&core}find.i &table = "b-zen-control"
               &where = "where b-zen-control.ctrl-idx = pv-idx"
               &type  = "first"
               &lock  = "no"}
              
if avail b-zen-control and b-zen-control.ctrl-data = 'blob'
    then assign
            pv-blob  = b-zen-control.ctrl-blob
            pv-value = b-zen-control.Blob-Filename.
    else assign 
            pv-blob  = ?
            pv-value = ?.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetControl Procedure 
PROCEDURE GetControl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-idx   as char no-undo.
def output param pv-value as char no-undo.
def buffer b-zen-control for zen-control.

{{&core}find.i &table = "b-zen-control"
              &where = "where b-zen-control.ctrl-idx = pv-idx"
              &type  = "first"
              &lock  = "no"}
              
if avail b-zen-control 
    then pv-value = b-zen-control.ctrl-data.
    else pv-value = ?.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetVersions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetVersions Procedure 
PROCEDURE GetVersions :
def output param lv-sname  as char no-undo.
def output param lv-sver   as char no-undo.
def output param lv-zver   as char no-undo.

assign
    lv-sname = Getctrl('{&SystemName}')
    lv-sver  = Getctrl('{&SystemVersion}')
    lv-zver  = Getctrl('{&ZenVersion}').



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IntCounter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IntCounter Procedure 
PROCEDURE IntCounter :
def input param pv-idx   as char no-undo.
def input-output param pv-value as int no-undo.
def input param pv-allownegative as log no-undo.

def buffer b-zen-control for zen-control.
def var x as int no-undo.

{{&core}find.i &table = "b-zen-control"
              &where = "where b-zen-control.ctrl-idx = pv-idx"
              &type  = "first"
              &lock  = "exclusive"} 
              
if not avail b-zen-control 
   then if not locked b-zen-control
        then create b-zen-control.
        else return.
        
if IsNumeric(b-zen-control.ctrl-data) or 
   b-zen-control.ctrl-data = '' 
then do:
    x = int(b-zen-control.ctrl-data) + pv-value.
    if not pv-allownegative and x < 0 
    then x = 0 .
    assign b-zen-control.ctrl-data = string(x)
           b-Zen-Control.ctrl-idx  = pv-idx
           pv-value = x.
end.
else pv-value = ?.

    release b-zen-control.
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NormalGet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NormalGet Procedure 
PROCEDURE NormalGet :
{{&core}apsrv-getrecords.i &whereclause = "where {&table-name}.SysRecord = no"}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Procedure 
PROCEDURE open-query :
/* USES MANDATORY PARAMETERS
      output table for t-{&table-name}.
      returns ALL records matching the &where clause
   can use &whereclause as where clause and &by for sorting
      e.g.  &where = "where {&table-name}.class = pv-class"       
        &by          = "by {&table-name}.name"   
********************************************************/

   {{&core}apsrv-openquery.i}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pop-combo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pop-combo Procedure 
PROCEDURE pop-combo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param pv-codes as char no-undo.
def output param pv-values as char no-undo.

def var lv-data as char no-undo.
assign lv-data = BuildCombo(?,
                            '{&table-name}',
                            'Zen-Control.ctrl-idx',
                            'Zen-Control.ctrl-data',
                            '','by Zen-Control.ctrl-data',no,no)
        pv-codes  = entry(1,lv-data,'{&Delim2}')
        pv-values = entry(1,lv-data,'{&Delim2}').

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create Procedure 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:     just a hook incase we need it
  Parameters:  <none>
  Notes:   called after the buffer copy from temp-table to db.
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-update) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update Procedure 
PROCEDURE post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-save-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE save-record Procedure 
PROCEDURE save-record :
/* USES MANDATORY PARAMETERS
def input param pv-recid as recid no-undo. 
def input-output param table for t-{&table-name}.
*/
   {{&core}apsrv-saverecord.i}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendServerInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendServerInfo Procedure 
PROCEDURE SendServerInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-request as char no-undo.
def output param pv-data as char no-undo.

case pv-request:
    when 'today' then pv-data = string(today).
    when 'time' then pv-data = string(time).
    otherwise pv-data = ?.
end case.



end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetControl Procedure 
PROCEDURE SetControl :
def input param pv-idx   as char no-undo.
def input param pv-value as char no-undo.

def buffer b-zen-control for zen-control.
{{&core}find.i &table = "b-zen-control"
              &where = "where b-zen-control.ctrl-idx = pv-idx"
              &type  = "first"
              &lock  = "exclusive"}
              
if avail b-zen-control 
   then do:
        assign b-zen-control.ctrl-data = pv-value
               b-Zen-Control.ctrl-idx  = pv-idx.
        release b-zen-control.
   end.
else return 'failed'.         
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SysmanGet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SysmanGet Procedure 
PROCEDURE SysmanGet :
{{&core}apsrv-getrecords.i}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

