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
&glob table-name zen-widlook
&glob unique-key {&table-name}tableid 

{{&core}def-table.i &table-name = {&table-name}}
{{&core}def-table.i &table-name = zen-fldlook}

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
         HEIGHT             = 12.19
         WIDTH              = 50.
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
for each t-zen-fldlook:
    delete t-zen-fldlook.
end.
END PROCEDURE.

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
END PROCEDURE.

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
END PROCEDURE.

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
    Return 'passed'.
END PROCEDURE.

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
def input  param pv-inprog   as char no-undo.
def input  param pv-inframe  as char no-undo.
def input  param pv-wid      as char no-undo.
def output param table for t-zen-fldlook.

for each t-zen-fldlook:
    delete t-zen-fldlook.
end.
{{&core}find.i &table = "zen-widlook"
               &where = "where pv-inprog Matches zen-widlook.In-Program  
                           And pv-inframe Matches zen-widlook.In-Frame    
                           And zen-widlook.look-field = pv-wid"
               &type  = "first"            
               &lock  = "no"}

if avail zen-widlook 
    then do:
    find zen-fldlook where zen-fldlook.lookupname = zen-widlook.lookupname 
                     no-lock no-error.
    if not avail zen-fldlook then do:
        ErrorCreate(5,'','','','').
        return 'failed'.    
    end.
    create t-zen-fldlook.
    buffer-copy zen-fldlook to t-zen-fldlook.
end.
else do:
   ErrorCreate(5,'','','','').
   return 'failed'.    
end.

return zen-widlook.lookupname.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-records) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-records Procedure 
PROCEDURE get-records :
def input  param pv-field     as char no-undo.
def input  param pv-value     as char no-undo.
def input  param pv-direction as char no-undo.
def input  param pv-id        like {&table-name}.{&unique-key} no-undo.
def output param table        for t-zen-widlook.

run clear-table.

Case pv-field:
    When 'field' Then Do:
        for each zen-widlook where Zen-widlook.look-field matches pv-value no-lock:
            create t-zen-widlook.
            buffer-copy zen-widlook to t-zen-widlook.
        end.
    End.
    When 'table' Then Do: 
        for each zen-widlook no-lock,
            each zen-fldlook WHERE zen-widlook.in-program = zen-fldlook.look-pgm AND
                                   Zen-fldlook.tablename matches pv-value no-lock:
            create t-zen-widlook.
            buffer-copy zen-widlook to t-zen-widlook.
        end.
    End.
    When 'program' Then Do:
        for each zen-widlook where Zen-widlook.in-Program matches pv-value no-lock:
            create t-zen-widlook.
            buffer-copy zen-widlook to t-zen-widlook.
        end.
    End.
End Case.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLookupName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetLookupName Procedure 
PROCEDURE GetLookupName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-inprog   as char no-undo.
def input  param pv-inframe  as char no-undo.
def input  param pv-wid      as char no-undo.
def output param table for t-zen-fldlook.

for each t-zen-fldlook:
    delete t-zen-fldlook.
end.
{{&core}find.i &table = "zen-widlook"
               &where = "where pv-inprog Matches zen-widlook.In-Program  
                           And pv-inframe Matches zen-widlook.In-Frame    
                           And zen-widlook.look-field = pv-wid"
               &type  = "first"            
               &lock  = "no"}

if avail zen-widlook 
    then do:
        Find First zen-fldlook where zen-fldlook.lookupname = Zen-widlook.LookupName
                               no-lock No-error.
        if avail zen-fldlook 
        then do:
            create t-zen-fldlook.
            buffer-copy zen-fldlook to t-zen-fldlook.
        end.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Procedure 
PROCEDURE open-query :
def input param pv-field as char no-undo.
def input param pv-value as char no-undo.
def output param table  for t-zen-widlook.

run clear-table.
case pv-field:
    When 'field' Then Do:
        for each zen-widlook where Zen-widlook.look-field matches pv-value no-lock:
            create t-zen-widlook.
            buffer-copy zen-widlook to t-zen-widlook.
        end.
    End.
    When 'table' Then Do:
        for each zen-widlook no-lock,
            each zen-fldlook WHERE zen-fldlook.lookupname = Zen-widlook.lookupname
                               AND Zen-fldlook.tablename matches pv-value
                             no-lock:
            create t-zen-widlook.
            buffer-copy zen-widlook to t-zen-widlook.
        end.
    End.
    When 'program' Then Do:
        for each zen-widlook where Zen-widlook.in-Program matches pv-value no-lock:
            create t-zen-widlook.
            buffer-copy zen-widlook to t-zen-widlook.
        end.
    End.    
    When 'lookup' Then Do:
        for each zen-widlook where Zen-widlook.lookupname matches pv-value no-lock:
            create t-zen-widlook.
            buffer-copy zen-widlook to t-zen-widlook.
        end.
    End.
End Case.


END PROCEDURE.

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
zen-widlook.sysrecord = true.
END PROCEDURE.

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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

