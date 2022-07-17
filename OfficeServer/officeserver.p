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
/* ***************************  Definitions  ************************** */
{app-paths.i}
&glob stopmetag stopmenow.tag
&glob Section OfficeServer

Def Var h-parent As Handle No-undo.

Def Stream ip.
Def Stream op.

Def Temp-table t-toprocess No-undo
    Field t-filename As Char
    Field t-status As Char.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-GetTagEntries) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetTagEntries Procedure 
FUNCTION GetTagEntries RETURNS CHARACTER
  ( lv-ininame As Char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Stopme) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Stopme Procedure 
FUNCTION Stopme RETURNS LOGICAL
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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetDataFileList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDataFileList Procedure 
PROCEDURE GetDataFileList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Empty Temp-table t-toprocess.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessFiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessFiles Procedure 
PROCEDURE ProcessFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Def Input Param pv-parent As Handle No-undo.
h-parent = pv-parent.

Def Var lv-mergeprogram  As Char No-undo.
Def Var lv-Sourcedata    As Char No-undo.
Def Var lv-officeprogram As Char No-undo.
Def Var lv-datafile      As Char No-undo.
Def Var lv-templatefile  As Char No-undo.
Def Var lv-destfile      As Char No-undo.
Def Var lv-dstring As Char No-undo.
Def var X As Int.

Do While Not stopme():
    Run GetDataFileList In This-procedure.
    For Each t-toprocess:
        Run Refresh In h-parent ( 'Processing ' + t-toprocess.t-filename).
        lv-dstring = GetTagEntries(t-toprocess.t-filename).
        Assign lv-mergeprogram  = Entry(1,lv-dstring,'|')   
               lv-sourcedata    = Entry(2,lv-dstring,'|')   
               LV-OFFICEPROGRAM = Entry(3,lv-dstring,'|')   
               lv-datafile      = Entry(4,lv-dstring,'|')   
               lv-templatefile  = Entry(5,lv-dstring,'|')   
               lv-destfile      = Entry(6,lv-dstring,'|').   
    End.
End.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetTagEntries) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetTagEntries Procedure 
FUNCTION GetTagEntries RETURNS CHARACTER
  ( lv-ininame As Char) :

def Var pv-value as char no-undo.
Def Var lv-value As Char No-undo.
Def Var lv-keys  As Char No-undo.
Def var X        As Int  No-undo.
Def var Y        As Int  No-undo.

LOAD lv-ininame DIR './' BASE-KEY "INI" NO-ERROR.
USE lv-ininame NO-ERROR.

IF ERROR-STATUS:ERROR THEN Return '?'.

GET-KEY-VALUE SECTION "AvailableKeys" KEY "Keys" VALUE lv-Keys.

Do X = 1 To num-entries(lv-keys,"|"):
    GET-KEY-VALUE SECTION "{&section}" KEY entry(x,lv-keys,"|") VALUE lv-value.
    pv-value = If pv-value = '' Then lv-value Else pv-value  + "|" + lv-value.
End.
Unload lv-ininame NO-ERROR.

RETURN pv-value.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Stopme) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Stopme Procedure 
FUNCTION Stopme RETURNS LOGICAL
  ( /* parameter-definitions */ ) :

  RETURN Search('{&stopmetag}') Ne ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

