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

def input param PrinterName as char NO-UNDO. 
 /* Printer Name, as set in Printer properties */
def input param FILENAME as char NO-UNDO.
 /* Name of file to print */
 
/******************/
/* DLL Procedures */
/******************/
PROCEDURE GetLastError EXTERNAL "kernel32.dll" :
    DEFINE RETURN param X AS LONG.
END PROCEDURE.
 
PROCEDURE StartDocPrinterA EXTERNAL "winspool.drv" :
    def input param hPrinter AS LONG.
    def input param Level AS LONG.
    def input param pDocInfo AS MEMPTR.
    DEFINE RETURN param X AS LONG.
END PROCEDURE.
 
PROCEDURE EndDocPrinter EXTERNAL "winspool.drv" :
    def input param hPrinter AS LONG.
    DEFINE RETURN param X AS LONG.
END PROCEDURE.
 
PROCEDURE CreateFileA EXTERNAL "kernel32.dll" :
    def input param lpFileName as char.
    def input param dwDesiredAccess AS LONG.
    def input param dwShareMode AS LONG.
    def input param lpSecurityAttributes AS LONG.
    def input param dwCreationDistribution AS LONG.
    def input param dwFlagsAndAttributes AS LONG.
    def input param hTemplateFile AS LONG.
    DEFINE RETURN param hFile AS LONG.
END PROCEDURE.
 
PROCEDURE ReadFile EXTERNAL "kernel32.dll" :
    def input param hFile AS LONG.
    def input param lpBuffer AS MEMPTR.
    def input param nNumberOfBytesToRead AS LONG.
    def output param  lpNumberOfBytesRead AS LONG.
    def input param lpOverlapped AS LONG.
    DEFINE RETURN param X AS LONG.
END PROCEDURE.
 
PROCEDURE WritePrinter EXTERNAL "winspool.drv" :
    def input param hPrinter AS LONG.
    def input param  pBuf AS MEMPTR.
    def input param cbBuf AS LONG.
    def output param lpdwWritten AS LONG.
    DEFINE RETURN param X AS LONG.
END PROCEDURE.
 
PROCEDURE OpenPrinterA EXTERNAL "winspool.drv" :
    def input param pPrinterName as char.
    def output param phPrinter AS LONG.
    def input param pDefault AS LONG.
    DEFINE RETURN param X AS LONG.
END PROCEDURE.
 
PROCEDURE ClosePrinter EXTERNAL "winspool.drv" :
    def input param hPrinter AS LONG.
    DEFINE RETURN param X AS LONG.
END PROCEDURE.
 
PROCEDURE GetFileSize EXTERNAL "kernel32.dll" :
    def input param hFile AS LONG.
    def input param lpFileSizeHigh AS LONG.
    DEFINE RETURN param FileSize AS LONG.
END PROCEDURE.
 
PROCEDURE CloseHandle EXTERNAL "kernel32.dll" :
    def input param hObject AS LONG.
    DEFINE RETURN param X AS LONG.
END PROCEDURE.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

 
def var X as int NO-UNDO.
def var hPrinter as int NO-UNDO.
def var hFile as int NO-UNDO.
def var pBuf AS MEMPTR NO-UNDO.
def var FileSize as int NO-UNDO.
def var iSize as int NO-UNDO.
def var xSize as int NO-UNDO.
def var pFileName AS MEMPTR NO-UNDO.
def var OutFileName as char  NO-UNDO.
def var pOutFileName AS MEMPTR NO-UNDO.
def var DataType as char  NO-UNDO.
def var pDataType AS MEMPTR NO-UNDO.
def var pDocInfo AS MEMPTR NO-UNDO.
 
RUN OpenPrinterA (PrinterName,OUTPUT hPrinter,0, OUTPUT X).
IF X = 0
THEN MESSAGE "Error opening printer: " PrinterName VIEW-AS ALERT-BOX.
ELSE DO:
  RUN CreateFileA (FILENAME , -2147483648,0,0,3,128,0,OUTPUT hFile). /* -2147483648 = $80000000 */
  IF hFile = -1
  THEN MESSAGE "Error opening file: " FILENAME VIEW-AS ALERT-BOX.
  ELSE DO:
    RUN GetFileSize (hFile,0,OUTPUT FileSize).
    IF FileSize = -1
    THEN MESSAGE "Wrong file size" VIEW-AS ALERT-BOX.
    ELSE DO:
      SET-SIZE(pBuf) = FileSize.

      RUN ReadFile(hFile,pBuf,FileSize,OUTPUT iSize,0, OUTPUT X).
      IF X = 0
      THEN MESSAGE "Error reading file: " FILENAME VIEW-AS ALERT-BOX.
      ELSE DO:
        IF iSize = 0
        THEN MESSAGE "Attempt to read beyond end of file:" FILENAME VIEW-AS ALERT-BOX.
        ELSE DO:
          OutFileName = "".
          DataType = "RAW".
          SET-SIZE(pDocInfo) = 12.
          SET-SIZE(pFileName) = LENGTH(FILENAME) + 1.
          PUT-STRING(pFileName,1) = FILENAME.
          SET-SIZE(pOutFileName) = LENGTH(OutFileName) + 1.
          PUT-STRING(pOutFileName,1) = OutFileName.
          SET-SIZE(pDataType) = LENGTH(DataType) + 1.
          PUT-STRING(pDataType,1) = DataType.
          PUT-LONG(pDocInfo,1) = GET-POINTER-VALUE(pFileName).
          PUT-LONG(pDocInfo,5) = GET-POINTER-VALUE(pOutFileName).
          PUT-LONG(pDocInfo,9) = GET-POINTER-VALUE(pDataType).

          RUN StartDocPrinterA (hPrinter,1,pDocInfo,OUTPUT X).
          IF X = 0 THEN DO:
              RUN GetLastError(OUTPUT X).
              MESSAGE "Error : " X VIEW-AS ALERT-BOX.
          END.

          RUN WritePrinter(hPrinter,pBuf,iSize,OUTPUT xSize,OUTPUT X).
          IF X = 0 THEN DO:
              RUN GetLastError(OUTPUT X).
              MESSAGE "Error writing to printer: " PrinterName iSize xsize X VIEW-AS ALERT-BOX.
          END.

          RUN EndDocPrinter(hPrinter,OUTPUT X).
        END.
      END.
    END.
    RUN CloseHandle(hFile,OUTPUT X).
    IF X = 0 THEN MESSAGE "Error closing file: " FILENAME.
  END.


  RUN ClosePrinter(hPrinter,OUTPUT X).
  IF X = 0
  THEN MESSAGE "Error closing printer: " PrinterName VIEW-AS ALERT-BOX.
END.

SET-SIZE(pBuf) = 0.
SET-SIZE(pDocInfo) = 0.
SET-SIZE(pFileName) = 0.
SET-SIZE(pOutFileName) = 0.
SET-SIZE(pDataType) = 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


