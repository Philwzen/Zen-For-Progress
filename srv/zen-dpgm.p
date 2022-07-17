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
&glob table-name zen-dpgm
&glob unique-key {&table-name}tableid 

{{&core}def-table.i &table-name = {&table-name}}

define query q-{&table-name} for {&table-name} scrolling.

def var lv-pgmid as int no-undo.

define temp-table t-dpgm no-undo 
    field author  as char
    field created as date
    field nAME    as char
    field PGM     as char
    index order name.

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
         HEIGHT             = 19.19
         WIDTH              = 42.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}mainp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BuildList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildList Procedure 
PROCEDURE BuildList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input-output param table for t-dpgm.


for each zen-dpgm no-lock:
    if substring(zen-dpgm.pgm,index(zen-dpgm.pgm,'.'),2) = '.i' or
       substring(zen-dpgm.pgm,index(zen-dpgm.pgm,'.'),2) = '.r' or
       zen-dpgm.pgm begins 'scs' or 
       can-find(first t-dpgm where t-dpgm.pgm = zen-dpgm.pgm) 
    then next.
    create t-dpgm.
    assign t-Dpgm.author  = zen-Dpgm.author
           t-Dpgm.created = zen-Dpgm.created
           t-Dpgm.NAME    = zen-Dpgm.name
           t-Dpgm.PGM     = zen-Dpgm.pgm.        
end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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
   
   for each zen-dwidget where zen-dwidget.pgm = {&table-name}.pgm
                        exclusive-lock:
    delete zen-dwidget.
   end.
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

def input-output param table for t-{&table-name}.

run clear-table in this-procedure no-error.
{{&core}find.i &table = "{&table-name}"
              &where = "where zen-dpgm.pgm = unixpath(pv-code)"
              &type  = "first"
              &lock  = "no"}

if not avail {&table-name} then do:
{{&core}find.i &table = "{&table-name}"
              &where = "where zen-dpgm.pgm = dospath(pv-code)"
              &type  = "first"
              &lock  = "no"}
end.



if avail {&table-name} then do:
    create t-{&table-name}.
    buffer-copy {&table-name} to t-{&table-name}. 
    &if '{&unique-key}' = 't-recid' &then
    t-{&table-name}.{&unique-key} = recid({&table-name}).
    &endif
end.
else do:
   ErrorCreate(50,'{&table-name}','{&where}','','').
   return .    
end.

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
def input  param lv-pgm as char no-undo.

def input param pv-direction as char no-undo.
def input  param pv-id     like {&table-name}.{&unique-key} no-undo.
def output param table        for  t-{&table-name}.
run sysmanget(lv-pgm,pv-direction,pv-id,output table t-{&table-name}).

return return-value.
/* if not systemmanager(GetSysVar("user"))                                      */
/* then run normalget(lv-pgm,pv-direction,pv-id,output table t-{&table-name}).  */
/* else run sysmanget(lv-pgm,pv-direction,pv-id,output table t-{&table-name}).  */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetDefaults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDefaults Procedure 
PROCEDURE GetDefaults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param pv-pgm   as char no-undo.
def output param pv-text as char no-undo.

/*    {{&core}find.i &table = "zen-dpgm"                                                                  */
/*                 &where = "where zen-dpgm.pgm    = pv-pgm"                                              */
/*                 &type  = "first"                                                                       */
/*                 &lock  = "no"}                                                                         */
/* if avail zen-dpgm                                                                                      */
/* then pv-text = zen-dpgm.file-directory + '{&Delim2}' + zen-dpgm.NAME + '{&Delim2}' +  zen-dpgm.params. */
/* else pv-text = ''.                                                                                     */
/*                                                                                                        */
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetWinXY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetWinXY Procedure 
PROCEDURE GetWinXY :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-pgm   as char no-undo.
def output param pv-x as int  no-undo.
def output param pv-y as int  no-undo.

   find zen-dpgm where zen-dpgm.pgm = pv-pgm no-lock no-error.
   if avail zen-dpgm 
    then assign pv-x = Zen-Dpgm.win-x 
                pv-y = Zen-Dpgm.win-y.
    else assign pv-x = ?
                pv-y = ?.               
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Key-Assign) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Key-Assign Procedure 
PROCEDURE Key-Assign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NormalGet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NormalGet Procedure 
PROCEDURE NormalGet :
def input  param lv-pgm as char no-undo.
   {{&core}apsrv-getrecords.i &whereclause = "where zen-dpgm.pgm matches lv-pgm
                                      and zen-dpgm.sysrec = no" 
                          &by    = " by zen-dpgm.pgm "}

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

&IF DEFINED(EXCLUDE-Post-Create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Post-Create Procedure 
PROCEDURE Post-Create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pre-Create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pre-Create Procedure 
PROCEDURE Pre-Create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-PgmId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-PgmId Procedure 
PROCEDURE Proc-PgmId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-pgm as char no-undo.
def output param pv-dec as dec no-undo.

  {{&core}find.i &table = "zen-dpgm"
                &where = "where zen-dpgm.pgm    = pv-pgm"
                &type  = "first"            
                &lock  = "no"}

   pv-dec = if avail zen-dpgm then {&table-name}tableid
                              else 0.
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

&IF DEFINED(EXCLUDE-SetSecurityFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSecurityFields Procedure 
PROCEDURE SetSecurityFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-pgm as char no-undo.
def input param pv-not-users as char no-undo.
def input  param  pv-notgroups as char no-undo.
def input  param  pv-run-groups as char no-undo.
def input  param  pv-run-users as char no-undo.



  {{&core}find.i &table = "zen-dpgm"
                &where = "where zen-dpgm.pgm    = pv-pgm"
                &type  = "first"            
                &lock  = "exclusive"}

    if avail zen-dpgm then do:
        assign zen-dpgm.not-groups = pv-notgroups
               zen-dpgm.not-users  = pv-not-users
               zen-dpgm.run-groups = pv-run-groups
               zen-dpgm.run-users  = pv-run-users.
        release zen-dpgm.
    end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SysManGet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SysManGet Procedure 
PROCEDURE SysManGet :
def input  param lv-pgm as char no-undo.
   {{&core}apsrv-getrecords.i &whereclause = "where zen-dpgm.pgm matches lv-pgm" 
                          &by    = " by zen-dpgm.pgm "}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

