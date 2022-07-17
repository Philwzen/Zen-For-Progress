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
&glob table-name zen-dmenu
&glob unique-key {&table-name}tableid 

{{&core}def-table.i &table-name = {&table-name}
                   &extra-fields = "field t-node-id  as int
                                    field t-to-level as log
                                    field t-from-id  as int"}


define query q-{&table-name} for {&table-name} scrolling.

&glob OverrideMaxListCount 2000

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
         HEIGHT             = 26.48
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

&IF DEFINED(EXCLUDE-checkmenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkmenu Procedure 
procedure checkmenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param  pv-procedure as char no-undo.
def output param  pv-ok        as log  no-undo.

{{&core}find.i &table = "zen-dpgm"
              &where = "where zen-dpgm.pgm =  pv-procedure"
              &type  = "first"                
              &lock  = "no"}
            
if avail zen-dpgm then do:
  {{&core}find.i &table = "zen-dmenu"
                &where = "where zen-dmenu.menu-grp contains zen-dpgm.menu-grp"
                &type  = "first"            
                &lock  = "no"}

if zen-dpgm.menu-grp ne '' and
     avail zen-dmenu
   then pv-ok = true. 
   else pv-ok = false.
end.
else pv-ok = false.

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
def input param pv-group as char no-undo.
def input param pv-user as char no-undo.
def input param pv-ugroup as char no-undo.
def input param pv-direction as char no-undo.
def input  param pv-id     like {&table-name}.{&unique-key} no-undo.
def output param table        for  t-{&table-name}.

if not systemmanager(GetSysVar("user")) 
then run normalget(pv-group,pv-user,pv-ugroup,pv-direction,pv-id,output table t-{&table-name}).
else run sysmanget(pv-group,pv-user,pv-ugroup,pv-direction,pv-id,output table t-{&table-name}).

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMenu Procedure 
procedure GetMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param table for t-zen-dmenu.

  run clear-table.
  
  for each zen-dmenu no-lock:
      create t-zen-dmenu.
      buffer-copy zen-dmenu to t-zen-dmenu.
  end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NormalGet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NormalGet Procedure 
procedure NormalGet :
def input param pv-group as char no-undo.
def input param pv-user as char no-undo.
def input param pv-ugroup as char no-undo.

&glob OverrideMaxListCount 99999
   {{&core}apsrv-getrecords.i &whereclause = "where {&table-name}.sysrecord = no
                                      and can-do(Zen-Dmenu.menu-grp,pv-group)
  /*                                     and SecurityCheck(pv-user, */
/*                                  pv-ugroup, */
/*                                  zen-dmenu.not-users, */
/*                                  zen-dmenu.not-group, */
/*                                  zen-dmenu.run-users, */
/*                                  zen-dmenu.run-groups) */ "
                             &by = "by menu-parent by display-order"}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Procedure 
procedure open-query :
   /* USES MANDATORY PARAMETERS
      output table for t-{&table-name}.
      returns ALL records matching the &where clause
   can use &whereclause as where clause and &by for sorting
      e.g.  &where = "where {&table-name}.class = pv-class"       
        &by          = "by {&table-name}.name"   
********************************************************/
{{&core}apsrv-openquery.i &by = "by menu-parent by display-order"}

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
  Notes:   called after the buffer copy from temp-table to db.
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
  Notes:       
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pre-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create Procedure 
procedure pre-create :
find last zen-dmenu use-index id no-lock no-error.
 t-Zen-Dmenu.menu-id = if avail zen-dmenu then zen-dmenu.menu-id + 1
                                 else 1.
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

&IF DEFINED(EXCLUDE-SysManGet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SysManGet Procedure 
procedure SysManGet :
def input param pv-group as char no-undo.
def input param pv-user as char no-undo.
def input param pv-ugroup as char no-undo.

&glob OverrideMaxListCount 99999
   {{&core}apsrv-getrecords.i &whereclause = "where if pv-group ne '*' then can-do(Zen-Dmenu.menu-grp,pv-group)
                                                                    else true
/*                                              and SecurityCheck(pv-user, */
/*                                  pv-ugroup, */
/*                                  zen-dmenu.not-users, */
/*                                  zen-dmenu.not-group, */
/*                                  zen-dmenu.run-users, */
/*                                  zen-dmenu.run-groups) */ "
                             &by = "by menu-parent by display-order"}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

