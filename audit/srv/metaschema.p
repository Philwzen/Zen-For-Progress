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




&glob Table-name _file
&glob Unique-key _file-number 
{{&core}def-table.i &table-name = {&table-name}}

define query q-{&table-name} FOR {&table-name} scrolling.

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
         HEIGHT             = 15.1
         WIDTH              = 45.2.
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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FieldList Procedure 
PROCEDURE FieldList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    Def Input Param pv-filename As Char No-undo.

    Def Var lv-ListItems As Char No-undo
        Initial "".
    Def Var lv-ListEntry As Char No-undo.

    Find _File 
        Where _File-Name = pv-filename 
    No-lock No-Error.

    For Each _Field Of _File
    No-lock:

        Assign
            lv-ListEntry = 
                Caps(Substr(_Field._Field-Name,1,1)) +
                      Substr(_Field._Field-Name,2,Length(_Field._Field-Name))
            lv-ListItems = 
                (if lv-ListItems = ""
                 Then lv-ListEntry
                 Else lv-ListItems + "," + lv-ListEntry).
    End.

    Return lv-ListItems.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillCombo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillCombo Procedure 
PROCEDURE FillCombo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

Def Output Param pv-codes As Char No-undo.
Def Output Param pv-values As Char No-undo.

def var lv-list as char no-undo.

lv-list =  BuildCombo(?,
                      "_file",
                      "_file-name",
                      "_file-name",
                      "WHERE _file._file-number > 0 and _file._file-number < 32000",
                      "",
                      no,
                      no).
assign
    pv-codes  = entry(1,lv-list,'{&Delim3}')
    pv-values = entry(2,lv-list,'{&Delim3}').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-records) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-records Procedure 
PROCEDURE get-records :
/* USES MANDATORY PARAMETERS YOU NEED NOT WORRY ONLY 
Def Input  Param pv-direction As Char  No-undo.
def Input  param pv-rowid     as recid no-undo.
def output param table for t-{&table-name}.

can use &where as where clause and &by for sorting
 eg  &where = "where {&table-name}.class = pv-class"       
     &by    = "by {&table-name}.name"                                                   
********************************************************/

{{&core}apsrv-getrecords.i &whereclause = "WHERE _file._file-number > 0
                                      and _file._file-number < 32000"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetOneRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOneRecord Procedure 
PROCEDURE GetOneRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    Def Input        Param pv-code As Char No-undo.
    Def input-output Param table   for t-{&table-name}.

    RUN clear-table.

    Find First {&table-name}
        where {&table-name}._file-name = trim(pv-code)
    No-lock No-error.

    If Available({&table-name}) 
    Then Do:
        Create t-{&table-name}.
        buffer-copy {&table-name} to t-{&table-name}. 
    End.

    Return string(Available({&table-name})).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Procedure 
PROCEDURE open-query :
/********* your parameters go here *********/
/***************************/
/* also uses standard appserver params
        input-output table  for t-{&table-name}.
        input-output pv-listcount as int.
********************************************************/
{{&core}apsrv-openquery.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

