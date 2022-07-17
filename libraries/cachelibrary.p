&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
create widget-pool.
this-procedure:private-data = "library-cache".
&glob library-cache
&glob library-program


{app-paths.i}

{{&sys}sys-temptables.i}

{{&sys}sys-cachedtables.i}
{{&core}cachedtables.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-CachedCombo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CachedCombo Procedure 
FUNCTION CachedCombo RETURNS CHARACTER
  ( pv-table as char,
    pv-key   as char,
    pv-field as char,
    pv-where as char,
    pv-by    as char,
    pv-none  as log,
    pv-wild  as log,
    output pv-codes  as char,
    output pv-values as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSysCacheFieldWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetSysCacheFieldWhere Procedure 
FUNCTION GetSysCacheFieldWhere RETURNS CHARACTER
  ( pv-table as char,
    pv-where as char,
    pv-datafield as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSysCacheRecordWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetSysCacheRecordWhere Procedure 
FUNCTION GetSysCacheRecordWhere RETURNS HANDLE
  ( pv-table as char,
    pv-where as char,
    pv-retname as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsCached) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD IsCached Procedure 
FUNCTION IsCached RETURNS LOGICAL
  ( pv-table as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RefreshSysTempTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RefreshSysTempTables Procedure 
FUNCTION RefreshSysTempTables RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 21.14
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


{{&core}libmain.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PopulatesysTempTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulatesysTempTables Procedure 
PROCEDURE PopulatesysTempTables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* do not forget to add table to lv-list in iscached */
if not session:remote then 
    sysmsg("Creating System Cache Please Wait").

/* {{&core}run.i &program   = "BuildTables.p" */
/*               &path      = "{&core}{&srv}" */
/*               &Appsrv    = "System" */
/*               &direct    = "true" */
/* &noper     = 'true' */
/*               &procedure = "BuildSysTables" */
/*               &params    = "(output table t-sysopt, */
/*                              output table t-prac, */
/*                              output table t-div, */
/*                              output table t-dept, */
/*                              output table t-prov, */
/*                              output table t-clsf03, */
/*                              output table t-loc, */
/*                              output table t-s-user, */
/*                              output table t-printer, */
/*                              output table t-s-stat, */
/*                              output table t-state-file, */
/*                              output table t-c-relation, */
/*                              output table t-c-protocol, */
/*                              output table t-c-perstat)"} */
/* do not forget to add table to lv-list in iscached */
if not session:remote then sysmsg("off").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-CachedCombo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-CachedCombo Procedure 
PROCEDURE Proc-CachedCombo :
def input param pv-table as char no-undo.
def input param pv-key   as char no-undo.
def input param pv-field as char no-undo.
def input param pv-where as char no-undo.
def input param pv-by    as char no-undo.
def input param pv-none  as log  no-undo.
def input param pv-wild  as log  no-undo.
def output param pv-codes  as char no-undo.
def output param pv-values as char no-undo.

pv-table = 't-' + pv-table.

  {{&core}bldcombo.i} /* so we have same functionality here
                         as in srv/dynamic.p */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetsysCacheFieldWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetsysCacheFieldWhere Procedure 
PROCEDURE Proc-GetsysCacheFieldWhere :
def input  param pv-table     as char no-undo.
def input  param pv-where     as char no-undo. 
def input  param pv-datafield as char no-undo.
def output param pv-data      as char no-undo.

pv-table = 't-' + pv-table.
{{&core}getfieldwhere.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetSysCacheRecordWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetSysCacheRecordWhere Procedure 
PROCEDURE Proc-GetSysCacheRecordWhere :
{{&core}getrecordwhere.i &Usecache = true}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-CachedCombo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CachedCombo Procedure 
FUNCTION CachedCombo RETURNS CHARACTER
  ( pv-table as char,
    pv-key   as char,
    pv-field as char,
    pv-where as char,
    pv-by    as char,
    pv-none  as log,
    pv-wild  as log,
    output pv-codes  as char,
    output pv-values as char):
    
    run Proc-CachedCombo in this-procedure
                     (pv-table,
                      pv-key,
                      pv-field,
                      pv-where,
                      pv-by,
                      pv-none,
                      pv-wild,
                      output pv-codes,
                      output pv-values).
  RETURN "".   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSysCacheFieldWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetSysCacheFieldWhere Procedure 
FUNCTION GetSysCacheFieldWhere RETURNS CHARACTER
  ( pv-table as char,
    pv-where as char,
    pv-datafield as char) :
    
    def var pv-data as char no-undo.
    run proc-GetSysCacheFieldWhere in this-procedure
        (pv-table,pv-where,pv-datafield,output pv-data).

  RETURN pv-data.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSysCacheRecordWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetSysCacheRecordWhere Procedure 
FUNCTION GetSysCacheRecordWhere RETURNS HANDLE
  ( pv-table as char,
    pv-where as char,
    pv-retname as char ) :
    
    def var pv-data as handle no-undo.
    run proc-GetSysCacheRecordWhere in this-procedure
        (pv-table,pv-where,pv-retname,output pv-data).

  RETURN pv-data.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsCached) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION IsCached Procedure 
FUNCTION IsCached RETURNS LOGICAL
  ( pv-table as char ) :
/*------------------------------------------------------------------------------
  Purpose:  ,{&CachedZenTables}
    Notes:  
------------------------------------------------------------------------------*/
def var lv-list as char no-undo initial '{&CachedSysTables},{&CachedZenTables}'.
  if session:remote then return no.

  RETURN can-do(lv-list,pv-table).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RefreshSysTempTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RefreshSysTempTables Procedure 
FUNCTION RefreshSysTempTables RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   run populatesystemptables in this-procedure no-error.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

