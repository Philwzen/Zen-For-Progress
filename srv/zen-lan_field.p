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
&glob table-name Zen-Lan_Field
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
PROCEDURE clear-table :
for each t-{&table-name}:
    delete t-{&table-name}.
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copy-translations) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-translations Procedure 
PROCEDURE copy-translations :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param vl-copy-opt      as log  no-undo. /* no  = current translation
                                                           yes = all translations */
def input param vc-english       as char no-undo. /* english word to translate */
def input param vl-blank-tran    as log  no-undo. /* yes if want to blank translation on copy */
def input param vi-from-language as int  no-undo. /* from country code */
def input param vi-to-language   as int  no-undo. /* to country code */
def input param vl-copy-records  as log  no-undo. /* no  = leave existing records
                                                           yes =  overwrite existing records */

def buffer b-lan_field for zen-lan_field.
def var vd-tableid as dec no-undo.

if not vl-copy-opt then
do:
    find first zen-lan_field where zen-lan_field.lan_lanid   = vi-from-language and
                                   zen-lan_field._field-name = vc-english no-lock no-error.

    if avail zen-lan_field then
    do:

       find first b-lan_field where b-lan_field.lan_lanid   = vi-to-language and
                                    b-lan_field._field-name = vc-english exclusive-lock no-error.

       if avail b-lan_field then 
       do:
          if vl-copy-records then
          do:
             assign b-lan_field.lan_altlabel = if vl-blank-tran then ''
                                               else zen-lan_field.lan_altlabel.
          end.
       end.
       else
       do:
          create b-lan_field.
          buffer-copy zen-lan_field except {&table-name}tableid lan_lanid lan_altlabel to b-lan_field.
          assign b-lan_field.lan_lanid    = vi-to-language
                 b-lan_field.lan_altlabel = if vl-blank-tran then ''
                                            else zen-lan_field.lan_altlabel.
       end.
    end. /* found current translation - should always find */
 end. /* copy selected */
 else
 do:  /* copy all */
 
   for each zen-lan_field where zen-lan_field.lan_lanid = vi-from-language no-lock:
      
       find first b-lan_field where b-lan_field.lan_lanid   = vi-to-language and
                                    b-lan_field._field-name = zen-lan_field._field-name exclusive-lock no-error.

       if avail b-lan_field then 
       do:
          if vl-copy-records then
          do:
             assign b-lan_field.lan_altlabel = if vl-blank-tran then ''
                                               else zen-lan_field.lan_altlabel.
          end.
       end.
       else
       do:
          create b-lan_field.
          buffer-copy zen-lan_field except {&table-name}tableid lan_lanid lan_altlabel to b-lan_field.
          assign b-lan_field.lan_lanid    = vi-to-language
                 b-lan_field.lan_altlabel = if vl-blank-tran then ''
                                            else zen-lan_field.lan_altlabel.
       end. /* avail buffer */
    end. /* for each */
 end. /* copy all */

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
def input param pv-id as int no-undo.

   {{&core}apsrv-findrecord.i 
        &where = "where Zen-Lan_Field.lan_lanid = pv-id"}

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

def input param pv-langid as int no-undo.
   {{&core}apsrv-getrecords.i &whereclause = " where Zen-Lan_Field.lan_lanid = pv-langid"}

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

&IF DEFINED(EXCLUDE-post-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create Procedure 
PROCEDURE post-create :
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
PROCEDURE post-update :
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
PROCEDURE Pre-Create :
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
PROCEDURE Pre-Save :
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

