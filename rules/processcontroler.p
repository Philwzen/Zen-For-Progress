&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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

&glob application processcontroller

{app-paths.i}



{{&core}logging.i}

DEF INPUT PARAM pv-params AS CHAR NO-UNDO.

def VAR pv-process as char no-undo. /* passedin */
DEF var lv-param   AS CHAR NO-UNDO.
ASSIGN
    pv-process = ENTRY(1,pv-params)
    lv-param   = ENTRY(2,pv-params).

def temp-table t-library
    field LibraryName   as char
    field LibraryHandle as handle
index order Libraryname.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-LoadLib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadLib Procedure 
FUNCTION LoadLib RETURNS HANDLE
  ( pv-libname as char )  FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR lv-rvalue AS CHAR NO-UNDO.

Run LoadLibraries.

Run ProcessRules.

lv-rvalue = RETURN-VALUE.

putlog('Rules processed ' + lv-rvalue).

RETURN lv-rvalue.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LoadLibraries) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadLibraries Procedure 
PROCEDURE LoadLibraries :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

putlog('Loading Libraries').

    for each zen-rule WHERE zen-rule.RuleGroup = pv-process
                        AND zen-rule.active
        no-lock:
        IF zen-rule.procedure NE '' AND
           not can-find(t-library where t-library.libraryname = zen-rule.program)
        then do:
            create t-library.
            t-library.libraryname   = zen-rule.program.
            t-library.libraryhandle = loadlib(zen-rule.program).        
        end.
    end.
putlog('Loaded Libraries').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessRules) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessRules Procedure 
PROCEDURE ProcessRules :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-passed as log no-undo.

putlog('processing Rules').
for each zen-rule WHERE zen-rule.RuleGroup = pv-process
                    AND zen-rule.active
        no-lock
        BY zen-rule.ExecuteOrder:    
    IF zen-rule.procedure NE '' 
    THEN do:
        find t-library where t-library.libraryname = zen-rule.program no-lock.
        run value(zen-rule.procedure) in t-library.libraryhandle no-error.
    END.
    ELSE run value(zen-rule.program) (lv-param) NO-ERROR.

    IF RETURN-VALUE NE 'passed' AND 
       NOT zen-rule.continueonerror
    THEN DO:
        putlog("failed in rule " + zen-rule.RuleName + ' ' + RETURN-VALUE).
        RETURN 'failed'.
    END.
end.
RETURN 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-LoadLib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadLib Procedure 
FUNCTION LoadLib RETURNS HANDLE
  ( pv-libname as char ) :
    def var lv-lib as handle no-undo.
    lv-lib = session:first-procedure.
    do while lv-lib <> ?:
        if lv-lib:private-data  = "LIBRARY-" + pv-libname then leave.
        lv-lib = lv-lib:next-sibling.
    end.  
    if lv-lib = ? then do:
        run value(pv-libname + '.p') persistent set lv-lib.
        lv-lib:private-data = "LIBRARY-" + pv-libname.
    end.

  RETURN lv-lib.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

