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
   create widget-pool.
/* ***************************  Definitions  ************************** */

this-procedure:private-data = "library-crystal".
&glob library-crystal
&glob library-program
{app-paths.i}

Def stream op.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-Call-Crystal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Call-Crystal Procedure 
FUNCTION Call-Crystal RETURNS LOGICAL
  ( pv-repname as char,
    pv-title   as char,
    pv-mode    as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSchema) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSchema Procedure 
FUNCTION SetSchema RETURNS LOGICAL
  ( pv-dir       as char,
   pv-tablelist as char)  FORWARD.

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
         HEIGHT             = 10.95
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}libmain.i}

PROCEDURE PEOpenEngine EXTERNAL "CRPE32.DLL":
  DEFINE RETURN param returnStatus  AS SHORT.
END PROCEDURE.

PROCEDURE PEClosePrintJob EXTERNAL "CRPE32.DLL" :
  def input param printJob       AS SHORT.
END PROCEDURE.

PROCEDURE PECloseEngine EXTERNAL "CRPE32.DLL":
  DEFINE RETURN param returnStatus  AS SHORT.
END PROCEDURE.


PROCEDURE PESetWindowOptions EXTERNAL "CRPE32.DLL" :
  def input param printJob AS SHORT.
  def input param options AS MEMPTR. 
  DEFINE RETURN param returnStatus   AS SHORT.
END PROCEDURE.  

PROCEDURE PEPrintReport EXTERNAL "CRPE32.DLL":
  def input param reportFilePath  as char.
  def input param toPrinter       as SHORT.
  def input param toWindow        AS SHORT.
  def input param windowTitle     as char.
  def input param left            AS SHORT.
  def input param top             AS SHORT. 
  def input param width           AS SHORT. 
  def input param height          AS SHORT. 
  def input param style           AS LONG. 
  def input param parentWindow    AS SHORT. 
  DEFINE RETURN param returnStatus   AS SHORT.
END PROCEDURE.

PROCEDURE PEOpenPrintJob EXTERNAL "CRPE32.DLL" :
  def input param reportFilePath as char.
  DEFINE RETURN param returnStaus   AS SHORT.    
END PROCEDURE.

PROCEDURE PEGetErrorCode EXTERNAL "CRPE32.DLL" :
  def input param printJob       AS SHORT.
  DEFINE RETURN param returnStatus  AS SHORT.
END PROCEDURE.  

PROCEDURE PEGetErrorText EXTERNAL "CRPE32.DLL":
   def input  param printjob      AS SHORT.
   def output param texthandle    AS LONG.
   def output param textlhandle   AS SHORT.
   DEFINE RETURN param returnStatus  AS SHORT.
END PROCEDURE.

PROCEDURE PEGetHandleString EXTERNAL "CRPE32.DLL":
   def input  param texthandle    AS LONG.
   def input  param mTEXT         AS MEMPTR.
   def input  param textl         AS SHORT.
   DEFINE RETURN param returnStatus  AS SHORT.
END PROCEDURE.

PROCEDURE PEOutputToPrinter EXTERNAL "CRPE32.DLL" :
  def input param printJob       AS SHORT.
  def input param ncopies        AS SHORT.
  DEFINE RETURN param returnStatus  AS SHORT.
END PROCEDURE.


PROCEDURE PEOutputToWindow EXTERNAL "CRPE32.DLL" :
  def input param printJob       AS SHORT.
  def input param windowTitle    as char.
  def input param left           AS SHORT.
  def input param top            AS SHORT.
  def input param width          AS SHORT.
  def input param height         AS SHORT.
  def input param style          AS LONG.
  def input param parentWindow   AS LONG.
  DEFINE RETURN param returnStatus  AS SHORT.
END PROCEDURE.

PROCEDURE PESelectPrinter EXTERNAL "CRPE32.DLL" :
  def input param printJob       AS SHORT.
  def input param driverName     as char.
  def input param printerName    as char.
  def input param portName       as char.
  def input param mode           AS LONG.
  DEFINE RETURN param returnStatus  AS SHORT.
END PROCEDURE.

PROCEDURE PEStartPrintJob EXTERNAL "CRPE32.DLL" :
  def input param printJob       AS SHORT.
  def input param waitUntilDone  AS SHORT.
  DEFINE RETURN param returnStatus  AS SHORT.
END PROCEDURE.

PROCEDURE PEGetWindowHandle EXTERNAL "CRPE32.DLL" :
  def input param printJob      AS SHORT.
  DEFINE RETURN param windowHnadle AS SHORT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PrintCrystalReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintCrystalReport Procedure 
PROCEDURE PrintCrystalReport :
/* ---------------------------------------------------------
   Purpose: Print Report Using Crystal Reports 
   params:  Report file name 
            name to be displayed on the preview window.
            destination ("WINDOW" or "Printer")
  set lv-command to be the directory your reports are in.            
-----------------------------------------------------------*/
def INPUT param ip-report-filename    AS CHAR NO-UNDO. 
def INPUT param ip-report-name        AS CHAR NO-UNDO. /* window caption */
def INPUT param ip-report-destination AS CHAR NO-UNDO. 

def var lv-command       AS CHAR   NO-UNDO init '.'.
def var lv-print-job     AS INT    NO-UNDO.  
def var lv-result        AS INT    NO-UNDO.
def var lv-return        AS INT    NO-UNDO.
def var lv-print-options AS MEMPTR NO-UNDO.

DEF VAR vi-text-handle   AS INT    NO-UNDO.
DEF VAR vi-textlength    AS INT    NO-UNDO.
DEF VAR vc-text          AS CHAR   NO-UNDO.
DEF VAR vi-error         AS INT    NO-UNDO.
DEF VAR vc-textp         AS MEMPTR NO-UNDO.

RUN PEOpenPrintJob(ip-report-filename, OUTPUT lv-print-job). 

IF lv-print-job = 0 
THEN DO:
    RUN PEGetErrorCode(lv-print-job, OUTPUT lv-result).
    
    IF lv-result = 512 
    THEN DO:
        lv-result = WapiLoadLibrary("CRPE32.DLL").
        RUN PEOpenEngine (OUTPUT lv-result).
        RUN PEOpenPrintJob(ip-report-filename,OUTPUT lv-print-job). 
    END.
    ELSE DO:
      /* get error message from code to output with message */
      ASSIGN vi-error = lv-result.
      RUN PEGetErrorText(lv-print-job, OUTPUT vi-text-handle, OUTPUT vi-textlength, OUTPUT lv-result).
      SET-SIZE(vc-textp) = vi-textlength.
      RUN PeGetHandleString(vi-text-handle, vc-textp, vi-textlength, OUTPUT lv-result).
      vc-text = GET-STRING(vc-textp,1,vi-textlength).
      MESSAGE "Could Not Open Report " + ip-report-filename + "." SKIP 
              "Error Number : " + STRING(vi-error) + " " + vc-text
      VIEW-AS ALERT-BOX ERROR.                                    
      RETURN 'failed'.                                                        
    END.
END.

IF ip-report-destination = "WINDOW" 
THEN RUN PEOutputToWindow(lv-print-job,ip-report-name,0,0,800,800,16777216 + 131072 + 65536 + 524288 + 262144,0,OUTPUT lv-result).
else RUN PEOutputToPrinter(INPUT lv-print-job, 1, OUTPUT lv-result).
IF lv-result = 0 
THEN DO:
    RUN PEGetErrorCode(lv-print-job,OUTPUT lv-result).

    /* get error message from code to output with message */
      ASSIGN vi-error = lv-result.
      RUN PEGetErrorText(lv-print-job, OUTPUT vi-text-handle, OUTPUT vi-textlength, OUTPUT lv-result).
      SET-SIZE(vc-textp) = vi-textlength.
      RUN PeGetHandleString(vi-text-handle, vc-textp, vi-textlength, OUTPUT lv-result).
      vc-text = GET-STRING(vc-textp,1,vi-textlength).
      MESSAGE "Failed Output For Report " + ip-report-filename + "." SKIP 
              "Error Number : " + STRING(vi-error) + " " + vc-text
      VIEW-AS ALERT-BOX ERROR.                                    
      RETURN 'failed'.                                                        
END.

SET-SIZE(lv-print-options) = 26.

/* Type PEWindowOptions
        StructSize as int              2
        hasGroupTree as int            2
        canDrillDown as int            2
        hasNavigationControls as int   2    
        hasCancelButton as int         2
        hasPrintButton as int          2
        hasExportButton as int         2
        hasZoomControl as int          2
        hasCloseButton as int          2
        hasProgressControls as int     2
        hasSearchButton as int         2
        hasPrintSetupButton as int     2
        hasRefreshButton as int        2
End Type */

PUT-SHORT(lv-print-options,1)  = 26.
PUT-SHORT(lv-print-options,3)  = 0.
PUT-SHORT(lv-print-options,5)  = 1.
PUT-SHORT(lv-print-options,7)  = 1.
PUT-SHORT(lv-print-options,9)  = 1.
PUT-SHORT(lv-print-options,11) = 1.
PUT-SHORT(lv-print-options,13) = 1.
PUT-SHORT(lv-print-options,15) = 1.
PUT-SHORT(lv-print-options,17) = 1.
PUT-SHORT(lv-print-options,19) = 1.
PUT-SHORT(lv-print-options,21) = 0.
PUT-SHORT(lv-print-options,23) = 1.
PUT-SHORT(lv-print-options,25) = 1.        

RUN PESetWindowOptions(lv-print-job,
                       lv-print-options,
                       OUTPUT lv-result).
RUN PEStartPrintJob(lv-print-job,
                    1,
                    OUTPUT lv-result).
IF lv-result = 0 
THEN DO:
      RUN PEGetErrorCode(lv-print-job,OUTPUT lv-result).
      /* get error message from code to output with message */
      ASSIGN vi-error = lv-result.
      RUN PEGetErrorText(lv-print-job, OUTPUT vi-text-handle, OUTPUT vi-textlength, OUTPUT lv-result).
      SET-SIZE(vc-textp) = vi-textlength.
      RUN PeGetHandleString(vi-text-handle, vc-textp, vi-textlength, OUTPUT lv-result).
      vc-text = GET-STRING(vc-textp,1,vi-textlength).
      MESSAGE "Unable to start print job for report " + ip-report-filename + "." SKIP 
              "Error Number : " + STRING(vi-error) + " " + vc-text
      VIEW-AS ALERT-BOX ERROR.                                    
      RETURN 'failed'.                                                    
                                           
END.
  
RUN PEClosePrintJob(INPUT lv-print-job). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-Call-Crystal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Call-Crystal Procedure 
FUNCTION Call-Crystal RETURNS LOGICAL
  ( pv-repname as char,
    pv-title   as char,
    pv-mode    as char) :

run PrintCrystalReport in this-procedure
         (pv-repname,pv-title,pv-mode) no-error.

  RETURN error-status:error.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSchema) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSchema Procedure 
FUNCTION SetSchema RETURNS LOGICAL
  ( pv-dir       as char,
   pv-tablelist as char) :

def var x as int no-undo.
pv-tablelist = 'params.dat,' + pv-tablelist.

output stream op to value(pv-dir + "schema.ini").
      do x = 1 to num-entries(pv-tablelist):
        SetIniValue(entry(x,pv-tablelist),
                    "ColNameHeader{&Delim3}Format{&Delim3}MaxScanRows{&Delim3}CharacterSet",
                    "True{&Delim3}Delimited({&Delim2}){&Delim3}25{&Delim3}OEM").
        put stream op unformatted '[' entry(x,pv-tablelist) ']' skip.
        put stream op unformatted 'ColNameHeader=True' skip.
        put stream op unformatted 'Format=Delimited({&Delim2})' skip. 
        put stream op unformatted 'MaxScanRows=25' skip.
        put stream op unformatted 'CharacterSet=OEM' skip(1).
      end.
output stream op close.
  RETURN True.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

