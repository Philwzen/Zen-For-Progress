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
&glob table-name zen-icons
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
   Other Settings: CODE-ONLY
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

   {{&core}apsrv-getrecords.i}

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

   {{&core}apsrv-openquery.i}

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

