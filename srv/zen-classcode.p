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




/* changeable bits here change table-name to appropriate DB table */
&glob table-name zen-classcode
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
         HEIGHT             = 20.14
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

&IF DEFINED(EXCLUDE-CheckValidClassCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckValidClassCode Procedure 
procedure CheckValidClassCode :
def input param pv-class as char no-undo.
def input param pv-Code  as char no-undo.
def output param pv-ok   as log no-undo.
def output param pv-desc as char no-undo.

find first {&table-name} where class = pv-class and code  = pv-code no-lock no-error.

if avail {&table-name} then
assign pv-ok   = yes
       pv-desc = {&table-name}.DESCRIPTION.
else
assign pv-ok = no
       pv-desc = ''.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clear-table) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-table Procedure 
procedure clear-table :
for each t-{&table-name}:
    delete t-{&table-name}.
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-record Procedure 
procedure delete-record :
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
procedure Delete-Related-Tables :
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
procedure delete-validation :
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
procedure find-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-code as char no-undo.

   {{&core}apsrv-findrecord.i 
        &where = "where true"}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Get-Class-Codes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Class-Codes Procedure 
procedure Get-Class-Codes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param  pv-class  as char no-undo.
def output param  pv-values as char no-undo.
def output param  pv-codes  as char no-undo.

def var lv-list as char no-undo.
def var lv-where as char no-undo.

find first {&table-name} where {&table-name}.class = "{&class}"
                     and {&table-name}.code  = pv-class 
                no-lock no-error.
if not avail {&table-name} then return.

lv-where = " where {&table-name}.class = '" + pv-class + "'".

lv-list =  BuildCombo(?,
                      "{&table-name}",
                      "code",
                      "description",
                      lv-where,
                      " by {&table-name}.description",
                       {&table-name}.Noneflag,
                       {&table-name}.AllFlag).

assign
    pv-codes  = entry(1,lv-list,'{&Delim2}')
    pv-values = entry(2,lv-list,'{&Delim2}').

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-records) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-records Procedure 
procedure get-records :
   /* USES MANDATORY PARAMETERS YOU NEED NOT WORRY ONLY 
def input param pv-direction as char no-undo.
def input  param pv-rowid     as recid no-undo.
def output param table        for  t-{&table-name}.

   can use &whereclause as where clause and &by for sorting
      e.g.  &where = "where {&table-name}.class = pv-class"       
        &by          = "by {&table-name}.name"                                                   
********************************************************/

def input param pv-class as char no-undo.
   {{&core}apsrv-getrecords.i &whereclause = "where {&table-name}.class = pv-class"}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetClassCodeDesc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetClassCodeDesc Procedure 
procedure GetClassCodeDesc :
def input param pv-class as char no-undo.
def input param pv-Code  as char no-undo.
def output param pv-desc as char no-undo.

find first {&table-name} where class = pv-class 
                        and code  = pv-code
                      no-lock no-error.
pv-desc = if avail {&table-name} then {&table-name}.description
                              else 'Not Found'.                      
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetClasses) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetClasses Procedure 
procedure GetClasses :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param pv-codes as char no-undo.
def output param pv-values as char no-undo.


def var lv-list as char no-undo.
find {&table-name} where {&table-name}.class = "{&class}"
                  and {&table-name}.code  = "{&class}"
                no-lock no-error.

lv-list =  BuildCombo(?,
                      "{&table-name}",
                                  "code",
                                  "description",
                                  "where {&table-name}.class = 'class'
                         and {&table-name}.code ne 'class'",
                                  " by {&table-name}.description",
                                  {&table-name}.Noneflag,
                                  {&table-name}.AllFlag).
assign
    pv-codes  = entry(1,lv-list,'{&Delim2}')
    pv-values = entry(2,lv-list,'{&Delim2}').

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Procedure 
procedure open-query :
   /* USES MANDATORY PARAMETERS
        input-output table for t-{&table-name}.
        input-output pv-listcount as int.
   can use &whereclause as where clause and &by for sorting
      e.g.  &where = "where {&table-name}.class = pv-class"       
        &by          = "by {&table-name}.name"   
                                                  
********************************************************/
def input param pv-class as char no-undo.
{{&core}apsrv-openquery.i &whereclause = "where {&table-name}.class = pv-class"}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create Procedure 
procedure post-create :
/*------------------------------------------------------------------------------
  Purpose:     just a hook incase we need it
  Parameters:  <none>
  Notes:       called after record create before buffer copy.
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-update) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update Procedure 
procedure post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       called after buffer copy for create or update      
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pre-Create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pre-Create Procedure 
procedure Pre-Create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       run before record create
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pre-Save) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pre-Save Procedure 
procedure Pre-Save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       run as first thing in appsrv-saverecord.i      
------------------------------------------------------------------------------*/
   return "passed".
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-save-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE save-record Procedure 
procedure save-record :
   /* USES MANDATORY PARAMETERS
def input param pv-recid as recid no-undo. 
def input-output param table for t-{&table-name}.
*/
   {{&core}apsrv-saverecord.i}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

