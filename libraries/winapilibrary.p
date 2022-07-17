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

01/10/19  Annerik   Set some memptrs to zero to avoid Progress bug.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
 create widget-pool.
/* ***************************  Definitions  ************************** */
this-procedure:private-data = "library-winapi".
&glob library-winapi
&glob library-program
{app-paths.i}

def var lv-hmail as handle no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-GetLastErrorNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetLastErrorNum Procedure 
FUNCTION GetLastErrorNum returns integer
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetParent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetParent Procedure 
FUNCTION GetParent returns integer
     (hWnd as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUniqueId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetUniqueId Procedure 
FUNCTION GetUniqueId returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MapiErrorCodes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MapiErrorCodes Procedure 
FUNCTION MapiErrorCodes returns character
  ( pv-code as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShowError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ShowError Procedure 
FUNCTION ShowError returns integer
  ( pv-errnum as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShowLastError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ShowLastError Procedure 
FUNCTION ShowLastError returns integer () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UnZipToFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UnZipToFile Procedure 
FUNCTION UnZipToFile returns character
  ( pv-sourcefile as char,
    pv-sourceptr as memptr,
    pv-targetfile as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UnZipToMemptr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UnZipToMemptr Procedure 
FUNCTION UnZipToMemptr returns memptr
   ( pv-sourcefile as char,
     pv-sourceptr as memptr)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiCallWindowProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiCallWindowProc Procedure 
FUNCTION WapiCallWindowProc RETURNS INTEGER
  ( pv-procad as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiCreateProcess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiCreateProcess Procedure 
FUNCTION WapiCreateProcess returns integer
         (input CommandLine as char,
          input CurrentDir  as char,
          input wShowWindow as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFileCopy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiFileCopy Procedure 
FUNCTION WapiFileCopy returns integer
  ( pv-from as char,
    pv-to as char,
    pv-errdisp as log )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFileDelete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiFileDelete Procedure 
FUNCTION WapiFileDelete returns integer
  ( pv-file as char,
    pv-errdisp as log )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFileExecute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiFileExecute Procedure 
FUNCTION WapiFileExecute returns integer
  ( ipFileName as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFileExecuteWait) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiFileExecuteWait Procedure 
FUNCTION WapiFileExecuteWait returns logical
  ( ipFileName as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFilePrint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiFilePrint Procedure 
FUNCTION WapiFilePrint returns logical
  ( ipFileName as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFreeLibrary) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiFreeLibrary Procedure 
FUNCTION WapiFreeLibrary RETURNS INTEGER
  ( hlib as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFreezeWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiFreezeWindow Procedure 
FUNCTION WapiFreezeWindow returns logical
  (   pv-window as handle,
      pv-onoff  as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetPrinters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiGetPrinters Procedure 
FUNCTION WapiGetPrinters returns character
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetProcAddress) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiGetProcAddress Procedure 
FUNCTION WapiGetProcAddress RETURNS INTEGER
  ( hlib as int,
    smod as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetProcessName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiGetProcessName Procedure 
FUNCTION WapiGetProcessName returns character
  ( pid as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetShareName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiGetShareName Procedure 
FUNCTION WapiGetShareName returns character
  ( pv-drive as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetSysColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiGetSysColor Procedure 
FUNCTION WapiGetSysColor RETURNS INTEGER
  ( pv-colornum as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetUserName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiGetUserName Procedure 
FUNCTION WapiGetUserName RETURNS CHARACTER
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetWinVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiGetWinVersion Procedure 
FUNCTION WapiGetWinVersion returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGlobalLock) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiGlobalLock Procedure 
FUNCTION WapiGlobalLock RETURNS INT64
  ( pv-loc as int64 )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiIsRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiIsRunning Procedure 
FUNCTION WapiIsRunning returns integer
  ( pv-exename as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiKillProcess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiKillProcess Procedure 
FUNCTION WapiKillProcess returns logical
  ( ProcessId as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiListProcesses) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiListProcesses Procedure 
FUNCTION WapiListProcesses returns character
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiLoadLibrary) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiLoadLibrary Procedure 
FUNCTION WapiLoadLibrary RETURNS INTEGER
  ( pv-lib as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiMessageBox) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiMessageBox Procedure 
FUNCTION WapiMessageBox RETURNS INTEGER
  ( pv-hwnd as int,
    pv-mestxt as char,
    pv-title as char,
    pv-style as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiPlaySound) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiPlaySound Procedure 
FUNCTION WapiPlaySound RETURNS INTEGER
  ( pv-sound as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiPrintDlg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiPrintDlg Procedure 
FUNCTION WapiPrintDlg RETURNS INT64
  ( pv-printer as int64 )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiProPrintFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiProPrintFile Procedure 
FUNCTION WapiProPrintFile RETURNS INTEGER
  (pv-printerhandle as int,
   pv-flags as int,
   pv-hwnd as int,
   pv-fontnum as int,
   pv-filename as char,
   pv-pages as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiRawPrint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiRawPrint Procedure 
FUNCTION WapiRawPrint returns character
  ( input PrinterName as char,
    input FileName    as char,
    pv-copies as int /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiRegisterOcx) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiRegisterOcx Procedure 
FUNCTION WapiRegisterOcx RETURNS LOGICAL
  ( pv-control as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSendMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiSendMail Procedure 
FUNCTION WapiSendMail returns char
  ( pv-method as char,
    pv-FromName     as char,
    pv-ToNames      as char,
    pv-cc           as char,
    pv-Subject      as char,
    pv-MessageText  as char,
    pv-Attachments  as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSetCurrentDirectory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiSetCurrentDirectory Procedure 
FUNCTION WapiSetCurrentDirectory RETURNS INTEGER
  ( pv-dir as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSetDefaultPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiSetDefaultPrinter Procedure 
FUNCTION WapiSetDefaultPrinter RETURNS INTEGER
  ( pv-printer as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSetRegEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiSetRegEntry Procedure 
FUNCTION WapiSetRegEntry RETURNS INTEGER
  ( pv-section as char,
    pv-path as char,
    pv-key as char,
    pv-type as char,
    pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSetSysColors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiSetSysColors Procedure 
FUNCTION WapiSetSysColors RETURNS CHARACTER
  ( cDspElements   as int,
    lpnDspElements as int64,
    lpdwRgbValues  as int64 )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiShellExecute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiShellExecute Procedure 
FUNCTION WapiShellExecute RETURNS INTEGER
  (pv-prog   as char,
   pv-dir    as char,
   pv-params as char,
   pv-mode   as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSleep) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WapiSleep Procedure 
FUNCTION WapiSleep RETURNS CHARACTER
  ( vi-milliseconds as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Win-Exec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Win-Exec Procedure 
FUNCTION Win-Exec returns logical
   ( ProgramName  as char,
     Presentation as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WinErrorCodes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WinErrorCodes Procedure 
FUNCTION WinErrorCodes returns character
    (Pv-code as int) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ZipToFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ZipToFile Procedure 
FUNCTION ZipToFile returns character
  ( pv-sourcefile as char,
    pv-sourceptr as memptr,
    pv-targetfile as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ZipToMemptr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ZipToMemptr Procedure 
FUNCTION ZipToMemptr returns memptr
  ( pv-sourcefile as char,
    pv-sourceptr as memptr)  FORWARD.

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
         HEIGHT             = 30.14
         WIDTH              = 48.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
Procedure SendMessageA external {&user}:
    def input param hwnd as LONG.
    def input param wmsg as LONG.
    def input param wparam as LONG.
    def input  param lParam As long.
    define return param result as LONG.
end procedure.
  
procedure compress external "zlib1" cdecl:
    def input        param ppv-TargetBuffer    as memptr.
    def input-output param plv-TargetSize      as long.
    def input        param ppv-SourceBuffer    as memptr.
    def input        param plv-SourceSize      as long.
    def return       param ReturnValue         as long.
end procedure.
  
procedure uncompress external "zlib1" cdecl:
    def input        param ppv-TargetBuffer    as memptr.
    def input-output param plv-TargetSize      as long.
    def input        param ppv-SourceBuffer    as memptr.
    def input        param plv-SourceSize      as long.
    def return       param ReturnValue         as long.
end procedure.
  
procedure SHFileOperationA EXTERNAL {&shell}:
  def input param  lpFileOp  as  LONG.
  define return param iRetCode  as  LONG.
end.
  
procedure ClosePrinter  external {&winspool}:
    def input param hPrn as LONG. 
    define return param ReturnValue as LONG.
end procedure.
  
procedure GetUserNameA EXTERNAL {&ADVAPI} :
def input-output param lpBuffer    as char.
def input-output param nSize       as LONG.
define return       parameter ReturnValue as {&BOOL}.
end procedure.

procedure GetDefaultPrinterA external {&winspool}:
    def input param stPrinter as char.
    def input param liBufferSize as LONG.
    define return param ReturnValue as LONG.
end procedure.

procedure GetPrinterA external {&winspool}:
    def input param hPrn as LONG.
    def input param cLevel as LONG.
    def input param pPrinter as LONG.
    def input param cbBuf as LONG.
    def output param pcbNeeded as LONG. 
    define return param ReturnValue as LONG.
end procedure.

procedure GetVersionExA external {&kernel}:
    def input param v_version-info as long.
    define return param v_return-value as long.
end procedure.

procedure PrintDlgA external "comdlg32.dll":U:
    def input param lpStructure as LONG.
    define return param iRetCode as LONG.
end.

procedure GlobalLock EXTERNAL {&kernel}:
    def input param hDevMod as LONG.
    define return param lpDevMod as LONG.
end.

procedure GlobalUnlock EXTERNAL {&kernel}:
    def input param hDevMod as LONG.
end.

procedure DocumentPropertiesA external {&winspool}:
    def input param hwnd as Long.
    def input param hPrinter as Long. 
    def input param pDeviceName as char.
    def input param pDevModeOutput as Long.
    def input param pDevModeInput as Long.
    def input param fMode as Long.
    define return param ReturnValue as LONG.
end procedure.      

procedure OpenPrinter external {&winspool}:
    def input param pName as char.
    def output param hPrn as LONG.
    def input param pDefault as LONG. 
    define return param ReturnValue as LONG.
end procedure.

procedure OpenPrinterA EXTERNAL {&winspool}:
   def input param pPrinterName as char.
   def output param phPrinter as LONG.
   def input param pDefault as LONG.
   define return param X as LONG.
end procedure.

procedure SetDefaultPrinterA external {&winspool}:
  def input param pszPrinter as char.
  define return param returnvalue as Long.
end procedure.

procedure SetPrinterA EXTERNAL {&winspool}:
    def input param hPrinter as LONG.
    def input param Level as LONG.
    def input param pPrinter as memptr.
    def input param COMMAND as LONG.
    define return param X as LONG.
end procedure.

procedure DeviceCapabilitiesA external {&winspool}:
    def input param lpDeviceName as char.
    def input param lpPort as char.
    def input param iIndex as LONG.
    def input param lpOutPut as LONG.
    def input param dev as LONG. 
    define return param ReturnValue as LONG.
end procedure.

procedure CloseHandle EXTERNAL {&kernel} :
  def input param hObject     as LONG.
  define return param ReturnValue as LONG.
end procedure.

procedure WNetGetConnectionA external "mpr.dll" :
  def input param lpDrive    as char.
  def output param lpUNCName  as char.
  def input-output param lpnLength  as LONG.
  define return       parameter RetBool    as LONG.
end procedure.

procedure TerminateProcess EXTERNAL {&kernel} :
  def input param hProcess  as LONG.
  def input param uExitCode as LONG.
  define return param retval    as LONG.
end procedure.

procedure EnumPrintersA external {&winspool}:
    def input param v_flags as long.
    def input param v_name as char.
    def input param v_level as long.
    def input param v_printer-info as long.
    def input param v_num-bytes as long.
    def output param v_needed as long.
    def output param v_recs_returned as long.
    define return param v_return-value as long.
end procedure.

procedure PrinterProperties EXTERNAL {&winspool} :
  def input param VH_PARENT         as LONG.
  def input param VH_PRINTER_HANDLE as LONG.
  define return param VI_RETURN_VALUE   as {&BOOL}.
end procedure.

procedure EnumProcesses external "psapi.dll" :
  def input param lpIdProcess as LONG.
  def input param cb          as LONG.
  def output param cbNeeded    as LONG.
  define return param ReturnValue as LONG.
end procedure.
 
procedure EnumProcessModules external "psapi.dll" :
  def input param hProcess    as LONG.
  def input param lphModule   as LONG.  /* lp to array of module handles */
  def input param cb          as LONG.
  def output param cbNeeded    as LONG.
  define return param ReturnValue as LONG.
end procedure.
 
procedure GetModuleBaseNameA external "psapi.dll" :
  def input param hProcess      as LONG.
  def input param hModule       as LONG.
  def output param lpBaseName    as char.
  def input param nSize         as LONG.
  define return param nReturnedSize as LONG.
end procedure.
 
procedure OpenProcess EXTERNAL {&kernel} :
  def input param dwDesiredAccess as LONG.
  def input param bInheritHandle  as LONG.
  def input param dwProcessId     as LONG.
  define return param hProcess        as LONG.
end procedure.
 
procedure CreateToolhelp32Snapshot EXTERNAL {&kernel} :
  def input param dwFlags           as LONG.
  def input param th32ProcessId     as LONG.
  define return param hSnapShot         as LONG.
end procedure.
 
procedure Process32First EXTERNAL {&kernel} :
  def input param hSnapShot         as LONG.
  def input param lpProcessEntry32  as memptr.
  define return param ReturnValue       as LONG.
end procedure.
 
procedure Process32Next EXTERNAL {&kernel} :
  def input param hSnapShot         as LONG.
  def input param lpProcessEntry32  as memptr.
  define return param ReturnValue       as LONG.
end procedure.

procedure ShellExecuteA EXTERNAL {&shell} :
    def input param HWND as LONG.
    def input param lpOperation as char.
    def input param lpFile as char.
    def input param lpParameters as char.
    def input param lpDirectory as char.
    def input param nShowCmd as LONG.
    define return param hInstance as LONG.
end. 

procedure ShellExecuteExA EXTERNAL {&shell} :
  def input param lpExecInfo  as LONG.
  define return param ReturnValue as LONG.
end procedure.

procedure LockWindowUpdate EXTERNAL {&user} :
  def input param hWndLock as LONG.
  define return param IsLocked as LONG.
end procedure.

procedure GetSysColor external {&user} :
    def input param nIndex     as long.
    define return param dwRgbValue as long.
end procedure.

procedure SetSysColors external {&user} :
    def input param cDspElements   as long.
    def input param lpnDspElements as long.
    def input param lpdwRgbValues  as long.
end procedure.

procedure ProPrintFile external "PROPRINT.DLL":
    /*  Procedure call to MS-Windows DLL to Print Setup dialog.  */
    def input param hControl        as LONG.
    def input param fPrintFlags     as LONG.
    def input param hWndParent      as LONG.
    def input param nFontNo         as LONG.
    def input param lpszFile        as char.
    def input param nPages          as LONG.
    define return param Print_Result    as LONG.
end.

procedure ProPrintFile16 external "PROPRINT.DLL" ordinal 2:
    /* Procedure call to MS-Windows DLL to Print Setup dialog.  */
    def input param hControl        as SHORT.
    def input param fUseSetupDialog as SHORT.
    def input param hWndParent      as SHORT.
    def input param nFontNo         as SHORT.
    def input param lpszFile        as char.
    def input param nPages          as SHORT.
    define return param Print_Result    as SHORT.
end.

procedure WinExec EXTERNAL {&kernel}:
    def input param ProgramName as char.
    def input param Presentation as SHORT.
/*    def return param ret-code as short. */ 
end procedure.

procedure SetCurrentDirectoryA external {&kernel}:
    def input  param lpPathName as char.
    def return param ret-value as Long.
end procedure.

procedure FreeLibrary external {&kernel}:
    def input param hLibModule As Long.
    def return param ret-value as long.
end procedure.

procedure GetProcAddress external {&kernel}:
    def input param hModule As Long.
    def input param lpProcName As char.
    def return param ret-value as long.
end procedure.

procedure CallWindowProcA external {&user} :
    def input param lpPrevWndFunc As Long.
    def input param hWnd As Long.
    def input param Msg As long.
    def input param wParam As long.
    def input param lParam As long.
    def return param ret-value as long.
end procedure.

procedure LoadLibraryA EXTERNAL {&kernel}:U :
    def input param dllname as char.
    define return param hdll    as LONG.
end.
procedure UuidCreate external "rpcrt4.dll":U :
    def input-output param opi-guid as char no-undo.
end procedure.
 
procedure UuidCreateSequential external "rpcrt4.dll":U :
    def input-output param opi-guid as char no-undo.
end procedure.

procedure Sleep EXTERNAL {&kernel}:
  def input param lMilliseconds as LONG      no-undo.
end procedure.

procedure MessageBoxA EXTERNAL {&user}:
    def input param hwnd as LONG.
    def input param mbtext as char.
    def input param mbtitle as char.
    def input param style as LONG.
    define return param result as LONG.
end.

procedure PlaySoundA EXTERNAL {&mmedia} PERSISTENT :
    def input param  pszSound    as LONG.
    def input param  hmod        as LONG.
    def input param  fdwSound    as LONG.
    define return param ReturnValue as LONG.
end procedure.

/* Raw Printing */
procedure GetLastError external "kernel32.dll" :
    define return param X as LONG.
end procedure.
 
procedure StartDocPrinterA external "winspool.drv" :
    def input param hPrinter as LONG.
    def input param Level as LONG.
    def input param pDocInfo as memptr.
    define return param X as LONG.
end procedure.
 
procedure EndDocPrinter external "winspool.drv" :
    def input param hPrinter as LONG.
    define return param X as LONG.
end procedure.
 
procedure CreateFileA external "kernel32.dll" :
    def input param lpFileName as char.
    def input param dwDesiredAccess as LONG.
    def input param dwShareMode as LONG.
    def input param lpSecurityAttributes as LONG.
    def input param dwCreationDistribution as LONG.
    def input param dwFlagsAndAttributes as LONG.
    def input param hTemplateFile as LONG.
    define return param hFile as LONG.
end procedure.
 
procedure ReadFile external "kernel32.dll" :
    def input param hFile as LONG.
    def input param lpBuffer as memptr.
    def input param nNumberOfBytesToRead as LONG.
    def output param  lpNumberOfBytesRead as LONG.
    def input param lpOverlapped as LONG.
    define return param X as LONG.
end procedure.
 
procedure WritePrinter external "winspool.drv" :
    def input param hPrinter as LONG.
    def input param  pBuf as memptr.
    def input param cbBuf as LONG.
    def output param lpdwWritten as LONG.
    define return param X as LONG.
end procedure.
 
procedure GetFileSize external "kernel32.dll" :
    def input param hFile as LONG.
    def input param lpFileSizeHigh as LONG.
    define return param FileSize as LONG.
end procedure.

procedure WaitForSingleObject EXTERNAL {&KERNEL} :
  def input param hObject     as {&INT}.
  def input param dwTimeout   as LONG.
  define return param ReturnValue as LONG.
end procedure.

procedure CreateProcessA EXTERNAL {&KERNEL} :
  def input param lpApplicationName    as LONG. /* NULL */
  def input param lpCommandline        as char.
  def input param lpProcessAttributes  as LONG.
  def input param lpThreadAttributes   as LONG.
  def input param bInheritHandles      as {&BOOL}.
  def input param dCreationFlags       as LONG.
  def input param lpEnvironment        as LONG.
  def input param lpCurrentDirectory   as LONG.
  def input param lpStartupInfo        as LONG.
  def input param lpProcessInformation as LONG.
  define return param bResult              as {&BOOL}.
end procedure.

procedure FormatMessageA EXTERNAL {&KERNEL} :
  def input param dwFlags      as LONG.
  def input param lpSource     as LONG.
  def input param dwMessageID  as LONG.
  def input param dwLanguageID as LONG.
  def output param lpBuffer     as char.
  def input param nSize        as LONG.
  def input param lpArguments  as LONG.
  define return param nTextLength  as LONG.
end procedure.

procedure SetWindowPos EXTERNAL {&USER} :
  def input param hwnd            as {&HWND}.
  def input param hwndInsertAfter as {&HWND}.
  def input param x               as {&INT}.
  def input param y               as {&INT}.
  def input param cx              as {&INT}.
  def input param cy              as {&INT}.
  def input param fuFlags         as LONG.
  define return param ReturnValue     as {&BOOL}.
end procedure.

procedure SetForegroundWindow EXTERNAL {&USER} :
  def input param hwnd            as {&HWND}.
  define return param ReturnValue     as {&BOOL}.
end procedure.

procedure MAPISendMail EXTERNAL {&MAPI} :
  def input param lhSession  as LONG.
  def input param ulUIParam  as LONG.
  def input param lpMessage  as LONG. /* get-pointer-value(memptr) */
  def input param flFlags    as LONG.
  def input param ulReserved as LONG.
  define return param wretcode   as {&INT}.
end procedure.

procedure MoveWindow EXTERNAL {&USER} :
  def input param hwnd            as {&HWND}.
  def input param x               as {&INT}.
  def input param y               as {&INT}.
  def input param cx              as {&INT}.
  def input param cy              as {&INT}.
  def input param repaint         as {&BOOL}.
  define return param ReturnValue     as {&BOOL}.
end procedure.

procedure RegCreateKeyExA external {&ADVAPI} :
 def input param hkey                 as long.
 def input param lpszSubKey           as char.
 def input param dwReserved           as long.
 def input param plszClass            as char.
 def input param dwOptions            as long.
 def input param samDesired           as long.
 def input param lpSecurityAttributes as long.
 def output param phkResult            as long.
 def output param lpdwDisposition      as long.
 define return param lpResult             as long.
end procedure.
procedure RegSetValueExA external {&ADVAPI} :
   def input param hkey                  as long.
   def input param  lpValueName          as char.
   def input param  Reserved             as long.
   def input param  dwType               as long.
   def input param  lpBuffer             as memptr.
   def input param  cbData               as long.
   define return param lpResult              as long.
end procedure.
procedure RegCloseKey external {&ADVAPI} :
 def input param hkey                 as long.
 define return param lpResult             as long.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Proc-GetProcessName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetProcessName Procedure 
PROCEDURE Proc-GetProcessName :
def input  param pid as int no-undo.
def output param szProcessName as char    no-undo.

  def var hProcess      as int no-undo.
  def var cbNeeded      as int no-undo.
  def var lphMod        as memptr  no-undo.

  def var ReturnValue   as int no-undo.
 
  /* OpenProcess returns a handle (hProcess),
     needed for querying info about the process */
  run OpenProcess ( {&PROCESS_QUERY_INFORMATION} + {&PROCESS_VM_READ},
                    0,
                    PID,
                    output hProcess).
 
  /* some system processes can not be queried, 
     like "System" and "System Idle Process" and "csrss.exe".
     ProcessName will be initialized to [unknown] for these processes: */
  szProcessName = "[unknown]" + FILL(" ", {&MAX_PATH}).
  if hProcess ne 0 then do:
 
     /* EnumProcessModules fills an array of module handles */
     /* The first module handle is a handle to the main module, and that's the 
        only handle you need  */
     SET-SIZE (lphMod) = 4. /* need only one hMod  */
     run EnumProcessModules ( hProcess,
                              GET-POINTER-VALUE(lphMod),
                              GET-SIZE(lphMod),
                              output cbNeeded,
                              output ReturnValue).
     if ReturnValue ne 0 then do:
        /* GetModuleBaseNameA returns the name of a module.
           Because this module is the main module, it's also considered to 
           be the name of the process */
        run GetModuleBaseNameA (hProcess,
                                GET-LONG(lphMod,1),
                                output szProcessName,
                                LENGTH(szProcessName),
                                output ReturnValue).
        /* ReturnValue is the number of returned bytes (chars): */
        szProcessName = SUBSTRING(szProcessName,1,ReturnValue).
        SET-SIZE (lphMod) = 0.
     end.
     run CloseHandle ( hProcess, output ReturnValue).
  end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-WapiMAPIMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-WapiMAPIMail Procedure 
PROCEDURE Proc-WapiMAPIMail :
def input param pv-FromName    as char no-undo.
def input param pv-ToNames     as char no-undo.
def input param pv-Subject     as char no-undo.
def input param pv-MessageText as char no-undo.
def input param pv-Attachments as char no-undo.
def output param pv-result as char no-undo.

def var recipname as char extent 20 no-undo.
def var filepathname as char extent 20 no-undo.
def var lv-origdir as char no-undo.
assign file-info:filename = '.'
       lv-origdir = file-info:full-pathname.
/* Memptr Definitions */
def var SubjPtr as memptr no-undo.                                  /* Message Subject */
def var TextPtr as memptr no-undo.                                  /* Message Text */
def var OriginNamePtr as memptr.                                    /* Originator Name */
def var RecipNamePtr as memptr extent 20 no-undo.                   /* Array of pointers to Recipient Name */
def var RecipDescPtr as memptr extent 20 no-undo.                   /* Array of pointers to Recipient Description */
def var FilePathNamePtr as memptr extent 20 no-undo.                /* Array of pointers to Attachment Path */
def var FileNam as char extent 20 no-undo.                     /* Array of Attachment Name */
def var FileNamPtr as memptr extent 20 no-undo.                     /* Array of pointers to Attachment Name */
def var FileDescPtr as memptr extent 20 no-undo.                    /* Array of pointers to Attachment Description */ 
def var MessageDescPtr as memptr no-undo.                           /* Pointer To Message Structure */
def var FileArrayPtr as memptr no-undo.                             /* Pointer to Array of File Decription */
def var RecipArrayPtr as memptr no-undo.                            /* Pointer to Array of Recipient */
def var OriginDescPtr as memptr no-undo.

def var x as int no-undo.                                      /* General Purpose Integer */


/* --- POPULATE RECIPIENT & ATTACHMENT ARRAYS -------*/
do x = 1 to num-entries(pv-ToNames,','):
    recipname[x] = entry(x,pv-ToNames,',').
end.
do x = 1 to num-entries(pv-Attachments,','):
    Filepathname[x] = entry(x,pv-Attachments,',').
end.
                     
/* --- SET MESSAGE TEXT AND SUBJECT -----------------*/
SET-SIZE(SubjPtr) = LENGTH(pv-Subject) + 1.                                  /* maximum = 255 */ 
PUT-STRING(SubjPtr,1) = pv-Subject.
SET-SIZE(TextPtr) = 16000. 
PUT-STRING(TextPtr,1) = pv-MessageText. 
/* --- BUILD ORIGINATOR DETAILS ---------------------*/
SET-SIZE(OriginNamePtr) = LENGTH(pv-FromName) + 1.                           /* maximum = 255 */
PUT-STRING(OriginNamePtr,1) = pv-FromName.                                   /* Originator name */
SET-SIZE(OriginDescPtr) = 24.
PUT-LONG(OriginDescPtr,1) = 0.                                              /* Reserved */ 
PUT-LONG(OriginDescPtr,5) = 0.                                              /* RecipClass 0 = MAPI_ORIG */ 
PUT-LONG(OriginDescPtr,9) = GET-POINTER-VALUE(OriginNamePtr).               /* Name */
PUT-LONG(OriginDescPtr,13) = 0.                                             /* Address */ 
PUT-LONG(OriginDescPtr,17) = 0.                                             /* EID Size */ 
PUT-LONG(OriginDescPtr,21) = 0.                                             /* Entry ID */
/* ---------- BUILD RECIPIENT DETAILS -------------- */
do x = 1 to num-entries(pv-ToNames) :
    SET-SIZE(RecipNamePtr[x]) = LENGTH(RecipName[x]) + 1.                 /* maximum = 255 */ 
    PUT-STRING(RecipNamePtr[x],1) = RecipName[x].                         /* Recipient name */
    SET-SIZE(RecipDescPtr[x]) = 24.
    PUT-LONG(RecipDescPtr[x],1) = 0.                                       /* Reserved */ 
    PUT-LONG(RecipDescPtr[x],5) = 1.                                       /* RecipClass 1 = MAPI_TO */ 
    PUT-LONG(RecipDescPtr[x],9) = GET-POINTER-VALUE(RecipNamePtr[x]).     /* Name */
    PUT-LONG(RecipDescPtr[x],13) = 0.                                      /* Address */ 
    PUT-LONG(RecipDescPtr[x],17) = 0.                                      /* EID Size */ 
    PUT-LONG(RecipDescPtr[x],21) = 0.                                      /* Entry ID */
end.
/* Populate Memory Indicated By RecipArrayPtr */
SET-SIZE(RecipArrayPtr) = 24 * num-entries(pv-ToNames).
do x = 1 to num-entries(pv-ToNames) :
    PUT-BYTES(RecipArrayPtr, (x * 24) - 23)  = GET-BYTES(RecipDescPtr[x],1,24).
end.

/* ---------- BUILD FILE DETAILS ------------------- */
/* Build File Description Array */
do x = 1 to num-entries(pv-Attachments):
    SET-SIZE(FilePathNamePtr[x]) = LENGTH(FilePathName[x]) + 1.           /* maximum = 255 */
    PUT-STRING(FilePathNamePtr[x],1) = FilePathName[x].                   /* File pathname */
    FileNam[x] = SUBSTRING(FilePathName[x],R-INDEX(FilePathName[x],"\":U) + 1).
    SET-SIZE(FileNamPtr[x]) = LENGTH(FileNam[x]) + 1.                     /* maximum = 255 */ 
    PUT-STRING(FileNamPtr[x],1) = FileNam[x].                             /* File name */
    SET-SIZE(FileDescPtr[x]) = 24.
    PUT-LONG(FileDescPtr[x],1) = 0.                                        /* Reserved */ 
    PUT-LONG(FileDescPtr[x],5) = 0.                                        /* Flags 0 = data file */
    PUT-LONG(FileDescPtr[x],9) = -1.                                       /* Position */
    PUT-LONG(FileDescPtr[x],13) = GET-POINTER-VALUE(FilePathNamePtr[x]).  /* PathName */
    PUT-LONG(FileDescPtr[x],17) = GET-POINTER-VALUE(FileNamPtr[x]).       /* File Name */ 
    PUT-LONG(FileDescPtr[x],21) = 0.                                       /* FileType */
end.
/* Populate Memory Indicated By FileArrayPtr */
SET-SIZE(FileArrayPtr) = 24 * num-entries(pv-Attachments).
do x = 1 to num-entries(pv-Attachments):
    PUT-BYTES(FileArrayPtr, (x * 24) - 23)  = GET-BYTES(FileDescPtr[x],1,24).
end.

/* ---------- BUILD MESSAGE DETAILS ---------------- */
SET-SIZE(MessageDescPtr) = 48.
PUT-LONG(MessageDescPtr,1) = 0.                                             /* Reserved */
PUT-LONG(MessageDescPtr,5) = GET-POINTER-VALUE(SubjPtr).                    /* Subject */
PUT-LONG(MessageDescPtr,9) = GET-POINTER-VALUE(TextPtr).                    /* Text */
PUT-LONG(MessageDescPtr,13) = 0.                                            /* MessageType */ 
PUT-LONG(MessageDescPtr,17) = 0.                                            /* DateReceived */ 
PUT-LONG(MessageDescPtr,21) = 0.                                            /* ConversationID */ 
PUT-LONG(MessageDescPtr,25) = 1.                                            /* Flags */
PUT-LONG(MessageDescPtr,29) = GET-POINTER-VALUE(OriginDescPtr).             /* Originator */
PUT-LONG(MessageDescPtr,33) = num-entries(pv-ToNames).                                    /* RecipCount */
PUT-LONG(MessageDescPtr,37) = GET-POINTER-VALUE(RecipArrayPtr).             /* Recips */
PUT-LONG(MessageDescPtr,41) = num-entries(pv-Attachments).                                   /* FileCount */
PUT-LONG(MessageDescPtr,45) = GET-POINTER-VALUE(FileArrayPtr).              /* Files */

/* ---------- SEND MESSAGE ------------------------- */
def var ResultInt as int no-undo.

run MAPISendMail
 (input 0,
  input 0,
  input GET-POINTER-VALUE(MessageDescPtr),
  input 0,       /* 1 = MAPI_LOGON_UI + 2 = MAPI_NEW_SESSION + 8 = MAPI_DIALOG */
  input 0,     output ResultInt). 

if ResultInt <> 0 then 
message MapiErrorCodes(resultint)
view-as alert-box error title 'Mail Sending Error'.

SetWorkingDir(lv-origdir).
 
/* ---------- RELEASE RESOURCES -------------------- */
SET-SIZE(SubjPtr) = 0.
SET-SIZE(TextPtr) = 0. 
do x = 1 to num-entries(pv-Attachments) :
    SET-SIZE(FilePathNamePtr[x]) = 0.
    SET-SIZE(FileNamPtr[x])     = 0.
    SET-SIZE(FilePathNamePtr[x]) = 0.
    SET-SIZE(FileDescPtr[x])     = 0.
end.
do x = 1 to num-entries(pv-ToNames) :
    SET-SIZE(RecipNamePtr[x])    = 0.
    SET-SIZE(RecipDescPtr[x])    = 0.
end.
SET-SIZE(MessageDescPtr) = 0.
SET-SIZE(FileArrayPtr) = 0.
SET-SIZE(RecipArrayPtr) = 0.
SET-SIZE(OriginNamePtr) = 0.
pv-result = ''.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-WapiSendMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-WapiSendMail Procedure 
PROCEDURE Proc-WapiSendMail :
def input param pv-method       as char no-undo.
def input param pv-FromName     as char no-undo.
def input param pv-ToNames      as char no-undo.
def input param pv-cc           as char no-undo.
def input param pv-Subject      as char no-undo.
def input param pv-MessageText  as char no-undo.
def input param pv-Attachments  as char no-undo.

    
def var lv-mailhub  as char no-undo.
def var lv-user     as char no-undo.
def var lv-result   as char no-undo.  
def var lv-smtpexe  as char no-undo.
  
/* smtp vars */  
def var lv-MIMEHeader  as char no-undo init ''.
def var lv-BodyType    as char no-undo init 'text'.
def var lv-attachnames as char no-undo.

/* cdo vars */        
def var ch-msg      as com-handle no-undo.
def var lv-userid   as char no-undo init '????'.
def var lv-password as char no-undo init '????'.
def var lv-pid      as int  no-undo.
def var lv-srvwasup as log no-undo.
def var lv-scratch as log no-undo.
def var lv-scratch1 as char no-undo.
def var ch-outlook as com-handle no-undo.
def var x as int no-undo.

assign
    lv-mailhub  = GetCtrl('smtpserver')
    lv-user     = getsysvar('{&clv}user')
    lv-mailhub  = if lv-mailhub = '' then 'localhost' else lv-mailhub
    lv-userid   = GetCtrl('SmtpUser')
    Lv-password = GetCtrl('SmtpPassword')
    pv-fromname = if pv-fromname = '' then "guidev@practice-alt.com" else pv-fromname
    pv-subject  = if pv-subject  = '' then  'AutoEmail From ' + getctrl('system') else pv-subject.
    
case pv-method:
    when 'MAPI' then do:
    /*
    def input param pv-FromName    as char no-undo.
def input param pv-ToNames     as char no-undo.
def input param pv-Subject     as char no-undo.
def input param pv-MessageText as char no-undo.
def input param pv-Attachments as char no-undo.
def output param pv-result as char no-undo.
    */
        run proc-WapiMapiMail(pv-fromname,
                              pv-tonames,
                              pv-subject,
                              pv-messagetext,
                              pv-attachments,
                              output lv-result).
    end.
    when 'SMTP' then do:
       lv-attachnames = pv-attachments.
        
       run proc-WapiSmtpMail(lv-mailhub,
                            pv-ToNames,
                            pv-FromName, 
                            pv-CC,   
                            lv-Attachnames, 
                            pv-Attachments,  
                            pv-Subject,     
                            pv-MessageText,        
                            lv-MIMEHeader,  
                            lv-BodyType).    
                            
         lv-result = return-value.
     end.
     when 'Cdo' then do:
        &glob Schema http://schemas.microsoft.com/cdo/configuration/
                   
        def var lv-smtpproc   as int no-undo.
        def var lv-smtpserver as char no-undo.
        
        if lv-mailhub = 'localhost'
        then do:
            assign lv-smtpserver = GetCtrl('SmtpLocalServerPath')
                   lv-smtpexe = entry(num-entries(lv-smtpserver,'\'),lv-smtpserver,'\')
                   lv-smtpproc = WapiIsRunning(lv-smtpexe)
                   lv-srvwasup = lv-smtpproc > 0.
            if not lv-srvwasup then do:
                win-exec(lv-smtpserver,0).
                Wait(250).
                lv-smtpproc =  WapiIsRunning(lv-smtpexe).
/*                  lv-smtpproc = WapiCreateProcess(lv-smtpserver,'',0).  */
            end.
        end.        
        create "cdo.message" ch-msg.
         
        assign ch-msg:Subject  = pv-subject
               ch-msg:From     = pv-fromname
               ch-msg:to       = pv-tonames
               ch-msg:textbody = pv-messagetext
               ch-msg:CC       = pv-cc.
       
        do x = 1 to num-entries(pv-attachments):
            ch-msg:AddAttachment(entry(x,pv-attachments),'','').
        end.       
       
        ch-msg:configuration:fields:item("{&Schema}smtpserver") = lv-mailhub.
        if lv-user ne ' ' then do: /* logon to server */
            ch-msg:configuration:fields:item("{&Schema}sendusername") = lv-userid.
            ch-msg:configuration:fields:item("{&Schema}sendpassword") = lv-password.
        end.
        /*assorted stuff probably never changed */
        ch-msg:configuration:fields:item("{&Schema}sendusing") = 2.
/*         ch-msg:configuration:fields:item("{&Schema}smtpauthenticate") = 1. */
/*         ch-msg:configuration:fields:item("{&Schema}smtpserverport") = 25. */
/*         ch-msg:configuration:fields:item("{&Schema}smtpusessl") = false. */
/*         ch-msg:configuration:fields:item("{&Schema}smtpconnectiontimeout") = 60. */

        ch-msg:configuration:fields:update(). /* do this after any config changes */
        
/*        /* other things we can do with CDO */ */
/*        /* html trext */ */
/*             ch-msg:htmlBody = "<h1>Something to be in html</h1> */
/*        /* send a link */ */
/*             ch-msg:HtmlBody = "http:://www.google.com" */
/*        /* send webpage as local file */ */
/*             ch-msg:HtmlBody = "file://c:/rex/rex.htm" */
/*        /* BCC */ */
/*             ch-msg:Bcc = "anyemail@adress.com" */

        ch-msg:send.
        Wait(250).
        if lv-mailhub = 'localhost' and not lv-srvwasup
        then WapikillProcess(lv-smtpproc).
        release object ch-msg.
     end.
     when 'Unix' then do:
            errorclear().
            /*
                def input param pv-userid       as char  no-undo.
                def input param mailToAddress   as char  no-undo.
                def input param mailFromAddress as char  no-undo.
                def input param mailSubject     as char  no-undo.
                def input param mailMainBody    as char  no-undo.
                def output param emailCommand    as char  no-undo.
                def output param errorMessage    as char  no-undo.
            */
  /*        {{&core}run.i */
/*              &program   = "e-mail.p" */
/*              &path      = "{&server}{&general}" */
/*              &procedure = "email-command" */
/*              &Appsrv    = "System" */
/*              &params    = "(lv-user,pv-tonames,pv-fromname,pv-subject,pv-messagetext, */
/*                             output lv-scratch1,output lv-result)"} */
        anyerrors().
     end.
     when 'Outlook' then do:
          ch-outlook = MSOpenApplication('Outlook.Application','visible').
  
          /*  pv-applhandle as com-handle,pv-from as char,pv-to   as char,
              pv-subject as char,v-text as  char,pv-attach as char,pv-expires as char */
  
         MSMergeToOutlook(ch-outlook,
                         pv-fromname,
                         pv-ToNames,
                          pv-subject,
                         pv-messagetext,
                         pv-Attachments,
                         '').
         MSCLoseApplication(ch-outlook).
        end.
end case.

  return lv-result.   /* Function return value. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-WapiSmtpMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-WapiSmtpMail Procedure 
PROCEDURE Proc-WapiSmtpMail :
def input  param mailhub         as char no-undo.
def input  param EmailTo         as char no-undo.
def input  param EmailFrom       as char no-undo.
def input  param EmailCC         as char no-undo.
def input  param Attachments     as char no-undo.
def input  param LocalFiles      as char no-undo.
def input  param Subject         as char no-undo.
def input  param Body            as char no-undo.
def input  param MIMEHeader      as char no-undo.
def input  param BodyType        as char no-undo.


if not valid-handle(lv-hmail)
then run {&core}smtpmail.p persist set lv-hmail.

run sendmail in lv-hmail (mailhub,
                       emailto,
                       emailfrom,
                       emailcc,
                       attachments,
                       localfiles,
                       subject,
                       body,
                       mimeheader,
                       bodytype).

return return-value.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-Zip) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-Zip Procedure 
PROCEDURE Proc-Zip :
def input param pv-mode          as char   no-undo.
    def input param pv-SourceFile    as char   no-undo.
    def input param pv-SourceBuffer  as memptr no-undo.
    def input param pv-TargetFile    as char   no-undo.
    def output param pv-TargetBuffer as memptr no-undo.

    def var lv-TempBuffer as memptr no-undo.
    def var lv-SourceSize as int no-undo.
    def var lv-TargetSize as int no-undo.
    def var ReturnValue   as int no-undo.
    error-status:error = no.
    if pv-SourceBuffer = ? /* passing in a file name */
       then pv-SourceBuffer = InputFromFile(pv-SourceFile,'local').

    lv-SourceSize = get-size(pv-SourceBuffer).
    if lv-sourcesize = 0 then return 'Zero Source Size'.
    
    
    if pv-mode = 'compress' then 
       lv-TargetSize = (lv-SourceSize * 1.01) + 12.
    else 
       lv-TargetSize = lv-SourceSize * 100.

    set-size(lv-TempBuffer) = lv-TargetSize.
    run value(pv-mode) (input             lv-TempBuffer,
                        input-output      lv-TargetSize,
                        input             pv-SourceBuffer,
                        input             lv-SourceSize,
                        output            ReturnValue).

    if ReturnValue = 0 /* success */ then do:
        set-size(pv-TargetBuffer ) = lv-TargetSize.
        pv-TargetBuffer = get-bytes(lv-TempBuffer,1,lv-TargetSize ).
    end.
    else return 'Error in ' + pv-mode + ' ' + string(returnvalue).

    if pv-targetfile ne '' then 
       pv-TargetFile = OutputToFile(pv-TargetFile,pv-TargetBuffer,'local').

    set-size(pv-SourceBuffer) = 0.
    set-size(lv-TempBuffer)   = 0.

    if pv-TargetFile begins '** ' then
        return pv-TargetFile.

   return "".
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetLastErrorNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetLastErrorNum Procedure 
FUNCTION GetLastErrorNum returns integer
  ( /* parameter-definitions */ ) :
  
  /* GetLastError returns the Error code, set by the most recently 
   failed api-call. */

/* PROBLEM : GetLastError will always return 127. The reason is that 
   Progress will have called some api function AFTER the one you have
   called. (29 januari 1998) */
 
  def var dwMessageID as int no-undo.
  run GetLastError in this-procedure (output dwMessageID).
  return (dwMessageID). 

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetParent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetParent Procedure 
FUNCTION GetParent returns integer
     (hWnd as int) :
     
/* GetParent returns the hWnd of the parent window */
  def var hParent as int no-undo.
  
/*   run GetParent in this-procedure (hWnd, output hParent). */
  
  return (hParent).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUniqueId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetUniqueId Procedure 
FUNCTION GetUniqueId returns character
  ( /* parameter-definitions */ ) :
   def var X as char no-undo.
   def var i as int no-undo.
   def var j as int no-undo.
   def var r as char no-undo.
 
   X = FILL(' ':U, 16).
 
/*    IF RunningWindows2000() THEN                   */
/*       RUN UuidCreateSequential (INPUT-OUTPUT X).  */
/*    ELSE                                           */
      run UuidCreate (input-output X).
 
   do i = 11 to 16:
      r = r + ' ':U + inttohex(ASC(SUBSTR(X,i,1))). 
   end.
   return SUBSTR(R,2).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MapiErrorCodes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MapiErrorCodes Procedure 
FUNCTION MapiErrorCodes returns character
  ( pv-code as int) :
 
def var lv-txt as char no-undo.
 
if pv-code <> 0 then  /* 0 = Success */ 
   case pv-code:
     when  1 then lv-txt = "User Abort".
     when  2 then lv-txt = "Failure".
     when  3 then lv-txt = "Login Failure".
     when  4 then lv-txt = "Disk Full".
     when  5 then lv-txt = "Insufficient Memory".
     when  6 then lv-txt = "Blk Too Small".
     when  8 then lv-txt = "Too Many Sessions".
     when  9 then lv-txt = "Too Many Files".
     when 10 then lv-txt = "Too Many Recipients".
     when 11 then lv-txt = "Attachment Not Found".
     when 12 then lv-txt = "Attachment Open Failure".
     when 13 then lv-txt = "Attachment Write Failure".
     when 14 then lv-txt = "Unknown Recipient".
     when 15 then lv-txt = "Bad Recipient type".
     when 16 then lv-txt = "No Messages".
     when 17 then lv-txt = "Invalid Message".
     when 18 then lv-txt = "Bodytext Too Large".
     when 19 then lv-txt = "Invalid Session".
     when 20 then lv-txt = "Type Not Supported".
     when 21 then lv-txt = "Ambiguous Recipient".
     when 22 then lv-txt = "Message in use".
     when 23 then lv-txt = "Network failure".
     when 24 then lv-txt = "Invalid edit fields".
     when 25 then lv-txt = "Invalid recipients".
     when 26 then lv-txt = "Feature not supported".
     otherwise lv-txt    = "Unknown Mapi Error".
   end case.
 
   return string(pv-code) + ':' + lv-txt.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShowError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ShowError Procedure 
FUNCTION ShowError returns integer
  ( pv-errnum as int ) :
  def var txt as char no-undo.
  def var TxtLength as int no-undo.

  txt = fill(" ",300).
  run FormatMessageA in this-procedure (512 + 4096,  /* = FORMAT_MESSAGE_IGNORE_INSERTS  
                                                  + FORMAT_MESSAGE_FROM_SYSTEM */
                        0,
                        pv-errnum,
                        0,
                        output txt,
                        length(txt),
                        0,
                        output TxtLength).
   message  txt view-as alert-box error.
  return pv-errnum.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShowLastError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ShowLastError Procedure 
FUNCTION ShowLastError returns integer ():
  
/* ShowLastError calls GetLastError and shows the message text in a 
   alert-box. The Message text is simply only searched in the system
   module, using the default language and does not insert any 
   arguments (like in the P4GL 'substitute' function) */
  def var ErrorId as int no-undo.

/* Note: can't work because GetLastError doesn't work with Progress */

   ErrorId = GetLastErrorNum().

   showerror(errorid).  
    
   return ( ErrorId ).
   
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UnZipToFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UnZipToFile Procedure 
FUNCTION UnZipToFile returns character
  ( pv-sourcefile as char,
    pv-sourceptr as memptr,
    pv-targetfile as char) :
    
    def var lv-targetptr  as memptr no-undo.


   /* make sure there is nothing left over in the memory pointer! */
   set-size(lv-targetptr) = 0.

    run proc-zip ('uncompress',
                  pv-sourcefile,
                  pv-sourceptr,
                  pv-targetfile,
                  output lv-targetptr).
                  
  if return-value ne '' then message 
      program-name(1)  "problem:" return-value 
      view-as alert-box error.

  else return pv-targetfile.   

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UnZipToMemptr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UnZipToMemptr Procedure 
FUNCTION UnZipToMemptr returns memptr
   ( pv-sourcefile as char,
     pv-sourceptr as memptr) :
    
    def var lv-targetptr  as memptr no-undo.
    def var lv-targetfile as char   no-undo.

   /* make sure there is nothing left over in the memory pointer! */
   set-size(lv-targetptr) = 0.

    run proc-zip ('uncompress',
                  pv-sourcefile,
                  pv-sourceptr,
                  lv-targetfile,
                  output lv-targetptr).

    if return-value ne '' then message 
      program-name(1)  "problem:" return-value 
      view-as alert-box error.

    else return lv-targetptr.   
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiCallWindowProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiCallWindowProc Procedure 
FUNCTION WapiCallWindowProc RETURNS INTEGER
  ( pv-procad as int ) :
  def var lv-res as int no-undo.
    run callwindowproca (pv-procad,current-window:hWnd,0,0,0,output lv-res).
  RETURN lv-res.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiCreateProcess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiCreateProcess Procedure 
FUNCTION WapiCreateProcess returns integer
         (input CommandLine as char,
          input CurrentDir  as char,
          input wShowWindow as int) :    
/*

/* this CreateProcess function is a simplified version of the 
   CreateProcess API definition.
   Parameters:
   1. Commandline, for example "notepad.exe c:\config.sys"
   2. CurrentDir,  is default directory for new process
   3. wShowWindow, 0=hidden, 1=normal, 2=minimized, 3=maximized
   r. return       if <>0 then handle of new process
                   if  =0 then failed, check GetLastError  */
*/
   def var lpStartupInfo as memptr.
   set-size(lpStartupInfo)     = 68.
   put-long(lpStartupInfo,1)   = 68.
   put-long (lpStartupInfo,45) = 1. /* = STARTF_USESHOWWINDOW */
   put-short(lpStartupInfo,49) = wShowWindow.

   def var lpProcessInformation as memptr.
   set-size(lpProcessInformation)   = 16.

   def var lpCurrentDirectory as memptr.
   if CurrentDir<>"" then do:
      set-size(lpCurrentDirectory)     = 256.
      put-string(lpCurrentDirectory,1) = CurrentDir.
   end.   

   def var bResult as int.
   
   run CreateProcessA in this-procedure
     ( 0,
       CommandLine,
       0,
       0,
       0,
       0,
       0,
       if CurrentDir="" 
          then 0 
          else get-pointer-value(lpCurrentDirectory),
       get-pointer-value(lpStartupInfo),
       get-pointer-value(lpProcessInformation),
       output bResult
     ).

  def var hProcess as int no-undo.
  def var hThread  as int no-undo.
  hProcess = get-long(lpProcessInformation,1).
  hThread  = get-long(lpProcessInformation,5).

  /* I am pretty sure you are not interested in hThread 
     so let's invalidate the handle right now. 
     This does not mean the thread is terminated, it just 
     means that Kernel doesn't need to keep the object for US. */
  def var ReturnValue as int no-undo.
  run CloseHandle in this-procedure (hThread, output ReturnValue).
  
  set-size(lpStartupInfo)        = 0.
  set-size(lpProcessInformation) = 0.
  set-size(lpCurrentDirectory)   = 0.

  return ( hProcess ).


end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFileCopy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiFileCopy Procedure 
FUNCTION WapiFileCopy returns integer
  ( pv-from as char,
    pv-to as char,
    pv-errdisp as log ) :

def var lpFileOp as memptr no-undo.
def var lpTitle      as memptr no-undo.
def var lpFileFrom   as memptr no-undo.
def var lpFileTo     as memptr no-undo.
def var iRetCode     as int    no-undo.

assign set-size(lpFileOp)        = 32
       set-size(lpTitle)         = length('Copying ' + pv-from) + 1
       set-size(lpFileFrom)      = length(pv-from) + 3
       set-size(lpFileTo)        = length(pv-to) + 3
       put-string(lpTitle,1)     = 'Copying ' + pv-from
       put-string(lpFileFrom,1)  = pv-from
       put-byte(lpFileFrom,length(pv-from) + 1) = 0  /* double null terminate list */
       put-byte(lpFileFrom,length(pv-from) + 2) = 0
       put-string(lpFileTo,1)    = pv-to
       put-byte(lpFileTo, length(pv-to) + 1) = 0 /* double null terminate list */
       put-byte(lpFileTo, length(pv-to) + 2) = 0
       put-long(lpFileOp,1)      = 0               /* set up SHFileOP Structure */
       put-long(lpFileOp,5)      = {&FO_COPY}
       put-long(lpFileOp,9)      = get-pointer-value(lpFileFrom)
       put-long(lpFileOp,13)     = get-pointer-value(lpFileTo)
       put-long(lpFileOp,17)     = {&FOF_SIMPLEPROGRESS}
       put-long(lpFileOp,19)     = 0
       put-long(lpFileOp,23)     = 0
       put-long(lpFileOp,27)     = get-pointer-value(lpTitle).

run SHFileOperationA(input  get-pointer-value(lpFileOp),
                     output iRetCode).
  
/* dealloacte memory */
assign set-size(lpFileOp)        = 0
       set-size(lpTitle)         = 0
       set-size(lpFileFrom)      = 0
       set-size(lpFileTo)        = 0.
  
if iRetCode > 0 and 
   pv-errdisp 
then ShowError(iRetCode).

return iretcode.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFileDelete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiFileDelete Procedure 
FUNCTION WapiFileDelete returns integer
  ( pv-file as char,
    pv-errdisp as log ) :

define var lpFileOp     as  memptr  no-undo.
define var lpTitle      as  memptr  no-undo.
define var lpFileToDelete     as  memptr  no-undo.
define var iRetCode     as  int     no-undo.
def var lv-options as int no-undo.

    lv-options = {&FOF_SIMPLEPROGRESS}
                 + {&FOF_NOCONFIRMATION} 
                 + {&FOF_NOCONFIRMMKDIR}
                /* + {&FOF_NOERRORUI} */ .
assign
set-size(lpFileOp)= 32
set-size(lpTitle)         = length('Deleting ' + pv-file) + 1
set-size(lpFileToDelete)  = length(pv-file) + 3
put-string(lpTitle,1)     = 'Deleting ' + pv-file
put-string(lpFileToDelete,1)    = pv-file  /* double null terminate list */
put-byte(lpFileToDelete, length(pv-file) + 1) = 0
put-byte(lpFileToDelete, length(pv-file) + 2) = 0
put-long(lpFileOp,1)      = 0       /* set up SHFileOP Structure */
put-long(lpFileOp,5)      = {&FO_DELETE}
put-long(lpFileOp,9)      = get-pointer-value(lpFileToDelete)
put-long(lpFileOp,13)     = 0
put-long(lpFileOp,17)     = lv-options
put-long(lpFileOp,19)     = 0
put-long(lpFileOp,23)     = 0
put-long(lpFileOp,27)     = get-pointer-value(lpTitle).

run SHFileOperationA(input  get-pointer-value(lpFileOp),
                     output iRetCode).

/* dealloacte memory */
assign                        
  set-size(lpFileOp)        = 0
  set-size(lpTitle)         = 0
  set-size(lpFileToDelete)  = 0.
  
if iRetCode > 0 and 
   pv-errdisp 
then ShowError(iRetCode).

return iretcode.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFileExecute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiFileExecute Procedure 
FUNCTION WapiFileExecute returns integer
  ( ipFileName as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

def var hInstance as int no-undo.

run ShellExecuteA in THIS-PROCEDURE (active-window:hwnd,
                              "open",
                              ipFileName,
                              "",
                              "",
                              1,
                              output hInstance).
return hInstance.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFileExecuteWait) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiFileExecuteWait Procedure 
FUNCTION WapiFileExecuteWait returns logical
  ( ipFileName as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 
  def var hProcess as int no-undo. 
  def var ReturnValue as int no-undo.

  hProcess = WapiCreateProcess ("c:\Program Files\Microsoft Office\OFFICE\winword.exe " + ipFileName,
                           "",
                           1). 

  run WaitForSingleObject in this-procedure (hProcess, 
                                       -1,   /* -1=INFINITE */
                                       output ReturnValue).

  run CloseHandle in this-procedure (hProcess, output ReturnValue).


end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFilePrint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiFilePrint Procedure 
FUNCTION WapiFilePrint returns logical
  ( ipFileName as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

def var hInstance as int no-undo.

run ShellExecuteA in THIS-PROCEDURE (0,
                              "print",
                              ipFileName,
                              "",
                              "",
                              1,
                              output hInstance).
return true.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFreeLibrary) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiFreeLibrary Procedure 
FUNCTION WapiFreeLibrary RETURNS INTEGER
  ( hlib as int ) :
  def var lv-res as int no-undo.
    run FreeLibrary (hLib,output lv-res).
  RETURN lv-res.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiFreezeWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiFreezeWindow Procedure 
FUNCTION WapiFreezeWindow returns logical
  (   pv-window as handle,
      pv-onoff  as int ) :
  
  run LockWindowUpdate (0,output pv-onoff).
  
  if pv-onoff ne 0
      then run LockWindowUpdate (pv-window:hwnd,output pv-onoff).
        
/* /* newer recommended method still dont work! */ */
/*     if pv-onoff ne 0 */
/*         then run SendMessageA (pv-window:hwnd,{&WM_SETREDRAW},0,0,output pv-onoff). */
/*         else run SendMessageA (pv-window:hwnd,{&WM_SETREDRAW},1,0,output pv-onoff). */

  return true.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetPrinters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiGetPrinters Procedure 
FUNCTION WapiGetPrinters returns character
  (  ) :

  return session:get-printers().

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetProcAddress) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiGetProcAddress Procedure 
FUNCTION WapiGetProcAddress RETURNS INTEGER
  ( hlib as int,
    smod as char ) :

    def var lv-res as int no-undo.
    run GetProcAddress(hLib, sMod, output lv-res).
  RETURN lv-res.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetProcessName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiGetProcessName Procedure 
FUNCTION WapiGetProcessName returns character
  ( pid as int ) :

def var szProcessName as char    no-undo.

run proc-GetProcessName in this-procedure (pid,output szProcessName).


  return TRIM(szProcessName).
 

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetShareName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiGetShareName Procedure 
FUNCTION WapiGetShareName returns character
  ( pv-drive as char ) :

if index(pv-drive,':') = 0 then pv-drive = pv-drive + ':'.
def var lv-share as char no-undo.


def var namelen as int no-undo initial 100.
def var retBool as int no-undo.
 
lv-share = FILL("x",namelen).
run WNetGetConnectionA ( pv-drive,
                   output lv-share,
                   input-output namelen,
                   output retBool).
 
if retBool = 0 
    then lv-share = SUBSTRING(lv-share, 1, namelen).
    else lv-share = "".

  return trim(lv-share).   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetSysColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiGetSysColor Procedure 
FUNCTION WapiGetSysColor RETURNS INTEGER
  ( pv-colornum as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
run GetSysColor in this-procedure 
                (pv-colornum,
                 output pv-colornum).

  RETURN pv-colornum.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetUserName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiGetUserName Procedure 
FUNCTION WapiGetUserName RETURNS CHARACTER
  ( ) :
    def var nr as int NO-UNDO INITIAL 100.
    def var ReturnValue as int NO-UNDO.
    def var lv-user as char no-undo.
       lv-user = FILL(" ",nr).
 RUN GetUserName{&A} /*  IN lh-winapi */
                        (INPUT-OUTPUT lv-user,
                        INPUT-OUTPUT nr,
                        OUTPUT ReturnValue).

  RETURN lv-user.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGetWinVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiGetWinVersion Procedure 
FUNCTION WapiGetWinVersion returns character
  ( /* parameter-definitions */ ) :
/*-----------------------------------------------------------------------------
  Purpose: Calls the WINAPI function GetVersionExA to determine the version
           of the Windows operating system that is running on the machine.
    Notes: Returns "95" for Windows 95, "98" for Windows 98, "NT" for Windows NT
           Returns "undef" if unable to determine platform.
------------------------------------------------------------------------------*/

    def var v_version-buf as memptr.
    def var v_platform-id as int no-undo.
    def var v_platform-desc as char no-undo.
    def var v_major-version as int no-undo.
    def var v_minor-version as int no-undo.
    def var v_return-value  as int no-undo.

    set-size(v_version-buf)   = 148.
    put-long(v_version-buf,1) = 148.

    run GetVersionExA (input get-pointer-value(v_version-buf),
                       output v_return-value).

    v_platform-id = get-long(v_version-buf,17).

    case v_platform-id:
        when 1 then do:
            v_minor-version = get-byte(v_version-buf,15).
            v_major-version = get-byte(v_version-buf,16).
        end.
        otherwise do:
            v_major-version = get-long(v_version-buf,5).
            v_minor-version = get-long(v_version-buf,9).
        end.
    end.

    case v_platform-id:
        when 0 then v_platform-desc = "3.1".
        when 1 then
        do:
            if v_minor-version eq 0 then v_platform-desc = "95".
            else if v_minor-version gt 0 then v_platform-desc = "98".
            else v_platform-desc = "undef".
        end.
        when 2 then
            v_platform-desc = "NT".
        otherwise
            v_platform-desc = "undef".
    end.

    set-size(v_version-buf) = 0.

    return v_platform-desc.


end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiGlobalLock) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiGlobalLock Procedure 
FUNCTION WapiGlobalLock RETURNS INT64
  ( pv-loc as int64 ) :
  def var iretcode as int64 no-undo.
    run GlobalLock in this-procedure
        (pv-loc,output iRetCode).

  RETURN iretcode.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiIsRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiIsRunning Procedure 
FUNCTION WapiIsRunning returns integer
  ( pv-exename as char) :

def var lv-pid as int no-undo.
def var x as int no-undo.
def var lv-procinfo as char no-undo.
def var lv-proclist as char no-undo.

lv-proclist = WapiListProcesses().
do x = 1 to num-entries(lv-proclist,"{&combodelim}"):
   lv-procinfo = entry(x,lv-proclist,"{&combodelim}").
   if entry(2,lv-procinfo,':') = pv-exename
    then do:
      lv-pid = int(entry(1,lv-procinfo,':')).
      leave.
   end.
end.


  return lv-pid.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiKillProcess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiKillProcess Procedure 
FUNCTION WapiKillProcess returns logical
  ( ProcessId as int ) :
 
def var ProcessHandle as int no-undo.
def var ReturnValue   as int no-undo.
 
run OpenProcess ({&PROCESS_TERMINATE}, 0, ProcessId, output ProcessHandle).
if ProcessHandle ne 0 then do:
   run TerminateProcess (ProcessHandle, 0, output ReturnValue).
   run CloseHandle(ProcessHandle, output ReturnValue).
end.

return returnvalue = 0.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiListProcesses) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiListProcesses Procedure 
FUNCTION WapiListProcesses returns character
  (  ) :
def var hSnapShot   as int   no-undo.
def var lpPE        as memptr    no-undo. /* PROCESSENTRY32 structure */
def var ReturnValue as int   no-undo.
def var list        as char no-undo initial "Process-List:".
def var lv-pid as int no-undo.
def var lv-proc as int no-undo.

/*
def var nr as int NO-UNDO INITIAL 100.
def var ReturnValue as int NO-UNDO.
lv-user = FILL(" ",nr).
RUN GetUserName{&A} (INPUT-OUTPUT lv-user,
                    INPUT-OUTPUT nr,
                    OUTPUT ReturnValue).
*/


/* Create and open SnapShot-list */
run CreateToolhelp32Snapshot({&TH32CS_SNAPPROCESS},0,output hSnapShot).
if hSnapShot  ne -1 
then do:
    /* init buffer for lpPE */
    SET-SIZE(lpPE)    = 336.
    PUT-LONG(lpPE, 1) = GET-SIZE(lpPE).
    /* Cycle thru process-records */
    run Process32First(hSnapShot,lpPE,output ReturnValue).
    do while ReturnValue ne 0:
       lv-pid = GET-LONG(lpPE, 9).
/* check here for ownership of process */
       run OpenProcess ({&PROCESS_all_access},
                    0,
                    lv-PID,
                    output lv-proc).
       if lv-proc ne 0
       then
       do:
         list = list + "{&combodelim}".
         list = list + string(lv-pid) + ':' + WapiGetProcessName(lv-pid).
       end.
       run Process32Next(hSnapShot,lpPE,output ReturnValue).
    end.
    /* Close SnapShot-list */
    run CloseHandle(hSnapShot, output ReturnValue).
end.

return list.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiLoadLibrary) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiLoadLibrary Procedure 
FUNCTION WapiLoadLibrary RETURNS INTEGER
  ( pv-lib as char) :
  def var lv-result as int no-undo.
        run LoadLibraryA (pv-lib,output lv-result).
  RETURN lv-result.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiMessageBox) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiMessageBox Procedure 
FUNCTION WapiMessageBox RETURNS INTEGER
  ( pv-hwnd as int,
    pv-mestxt as char,
    pv-title as char,
    pv-style as int ) :
  def var lv-result as int no-undo.
    RUN MessageBoxA (pv-hwnd,pv-mestxt,pv-title,pv-style,OUTPUT lv-result).

  RETURN lv-result.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiPlaySound) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiPlaySound Procedure 
FUNCTION WapiPlaySound RETURNS INTEGER
  ( pv-sound as int ) :
&GLOB SND_ASYNC 1
&GLOB SND_NODEFAULT 2
&GLOB SND_LOOP 8
&GLOB SND_PURGE 64
&GLOB SND_APPLICATION 128
&GLOB SND_ALIAS 65536
&GLOB SND_FILENAME 131072
&GLOB SND_RESOURCE 262148

DEF VAR ReturnValue AS INT    NO-UNDO.

RUN PlaySoundA (pv-sound, 
                0, 
                {&SND_FILENAME} + {&SND_ASYNC} + {&SND_NODEFAULT},
                OUTPUT ReturnValue).              

  RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiPrintDlg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiPrintDlg Procedure 
FUNCTION WapiPrintDlg RETURNS INT64
  ( pv-printer as int64 ) :
  def var iretcode as int64 no-undo.
  run PrintDlgA in this-procedure (pv-printer,output iRetCode).  
  RETURN iretcode.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiProPrintFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiProPrintFile Procedure 
FUNCTION WapiProPrintFile RETURNS INTEGER
  (pv-printerhandle as int,
   pv-flags as int,
   pv-hwnd as int,
   pv-fontnum as int,
   pv-filename as char,
   pv-pages as int ) :
  
  def var lv-result as int no-undo.
  
        run proprintfile (pv-printerhandle,
                          pv-flags,
                          pv-hwnd,
                          pv-fontnum,
                          pv-filename,
                          pv-pages,
                          output lv-result).

  RETURN lv-result.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiRawPrint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiRawPrint Procedure 
FUNCTION WapiRawPrint returns character
  ( input PrinterName as char,
    input FileName    as char,
    pv-copies as int /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

def var X as int no-undo.
def var y as int no-undo.
def var hPrinter as int no-undo.
def var hFile as int no-undo.
def var pBuf as memptr no-undo.
def var FileSize as int no-undo.
def var iSize as int no-undo.
def var xSize as int no-undo.
def var pFileName as memptr no-undo.
def var OutFileName as char  no-undo.
def var pOutFileName as memptr no-undo.
def var DataType as char  no-undo.
def var pDataType as memptr no-undo.
def var pDocInfo as memptr no-undo.

if stringtolog(getctrl("Dbgwapirawprint"))
then  message 'Printing Using ' program-name(2) skip 
              'Printer : ' printername skip     
              'File : ' filename skip
              'Copies : ' pv-copies {&dbt}.
                     
if pv-copies = 0 or 
   pv-copies = ? or
   pv-copies < 0
then pv-copies = 1.

run OpenPrinterA (PrinterName,output hPrinter,0, output X).
if X = 0
then return "Error opening printer: " + PrinterName.
else do:
  run CreateFileA (FILENAME , -2147483648,0,0,3,128,0,output hFile). /* -2147483648 = $80000000 */
  if hFile = -1
  then return "Error opening file: " + FILENAME.
  else do:
    run GetFileSize (hFile,0,output FileSize).
    if FileSize = -1
    then return "Wrong file size".
    else do:
      SET-SIZE(pBuf) = FileSize.

      run ReadFile(hFile,pBuf,FileSize,output iSize,0, output X).
      if X = 0
      then return "Error reading file: " + FILENAME.
      else do:
        if iSize = 0
        then return "Attempt to read beyond end of file:" + FILENAME.
        else do:
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

          run StartDocPrinterA (hPrinter,1,pDocInfo,output X).
          if X = 0 then do:
              run GetLastError(output X).
              return "Error : " + string( X ).
          end.
/* do for num-copies */
do y = 1 to pv-copies:
          run WritePrinter(hPrinter,pBuf,iSize,output xSize,output X).
          if X = 0 then do:
              run GetLastError(output X).
              return "Error writing to printer: " + PrinterName.
          end.
end.          
/* */
          run EndDocPrinter(hPrinter,output X).
        end.
      end.
    end.
    run CloseHandle(hFile,output X).
    if X = 0 then return "Error closing file: " + FILENAME.
  end.


  run ClosePrinter(hPrinter,output X).
  if X = 0
  then return "Error closing printer: " + PrinterName.
end.

SET-SIZE(pBuf) = 0.
SET-SIZE(pDocInfo) = 0.
SET-SIZE(pFileName) = 0.
SET-SIZE(pOutFileName) = 0.
SET-SIZE(pDataType) = 0.

return "".

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiRegisterOcx) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiRegisterOcx Procedure 
FUNCTION WapiRegisterOcx RETURNS LOGICAL
  ( pv-control as char ) :

    def var hLib As int no-undo.
    Def var ProcAd as int no-undo.
    Def var sMod As char no-undo.
    Def var res as int no-undo.
      
    sMod = "DllRegisterServer". /* or dllunregisterserver */
    
    hLib   = Wapiloadlibrary(pv-control).
    ProcAd = WapiGetProcAddress(hLib, sMod).
    res = WapiCallWindowProc(ProcAd).
    Wapifreelibrary(hlib).
    return res = 0.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSendMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiSendMail Procedure 
FUNCTION WapiSendMail returns char
  ( pv-method as char,
    pv-FromName     as char,
    pv-ToNames      as char,
    pv-cc           as char,
    pv-Subject      as char,
    pv-MessageText  as char,
    pv-Attachments  as char) :

run proc-WapiSendMail(pv-method,
                      pv-fromname,
                      pv-tonames,
                      pv-cc,
                      pv-subject,
                      pv-messagetext,
                      pv-attachments).

return return-value.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSetCurrentDirectory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiSetCurrentDirectory Procedure 
FUNCTION WapiSetCurrentDirectory RETURNS INTEGER
  ( pv-dir as char ) :
def var lv-ok as int no-undo.
    run SetCurrentDirectoryA (pv-dir,output lv-ok).
  RETURN lv-ok.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSetDefaultPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiSetDefaultPrinter Procedure 
FUNCTION WapiSetDefaultPrinter RETURNS INTEGER
  ( pv-printer as char ) :
  
  def var lv-ret as int64 no-undo.
  
    run setdefaultprintera in this-procedure
        (pv-printer,output lv-ret).
        
  RETURN lv-ret.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSetRegEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiSetRegEntry Procedure 
FUNCTION WapiSetRegEntry RETURNS INTEGER
  ( pv-section as char,
    pv-path as char,
    pv-key as char,
    pv-type as char,
    pv-value as char ) :
   /* section = hkcr,hkcu,hklm,hku,hkcc,hkdd */
   /* path = "Software\Microsoft\Office\11.0\outlook\Options"*/
   /* key = "UseWordMail" */
   /* type = dword,binary,string,estring,mstring */
   
   
    &scoped-def KEY_ALL_ACCESS 983103
    &scoped-def REG_OPTION_NON_VOLATILE 0
    &scoped-def REG_OPTION_VOLATILE 1
    &scoped-def REG_CREATED_NEW_KEY 1
    &scoped-def REG_OPENED_EXISTING_KEY 2
    
    
    def var lv-hkey          as int no-undo.
    def var lv-dwDisposition as int no-undo.
    def var lv-result        as int no-undo.
    def var lv-memptr        as memptr      no-undo.
    def var lv-type as int no-undo.
    def var lv-section as int no-undo.
    
    case pv-section:
        when 'hkcr' then lv-section = -2147483648.
        when 'hkcu' then lv-section = -2147483647.
        when 'hklm' then lv-section = -2147483646.
        when 'hku'  then lv-section = -2147483645.
        when 'hkcc' then lv-section = -2147483643.
        when 'hkdd' then lv-section = -2147483642.
    end.
    
    run RegCreateKeyExA(lv-section,
                        pv-path,
                        0,
                        "",
                        0,
                        {&KEY_ALL_ACCESS},
                        0,
                        output lv-hkey,
                        output lv-dwDisposition,
                        output lv-result).
    case pv-type:
        when 'binary'  then lv-type = 3.
        when 'string'  then do:
                            lv-type = 1.
                            pv-value = replace(pv-value,'\','\\').
                            end.
        when 'estring' then lv-type = 2.
        when 'mstring' then lv-type = 7.
        when 'dword'   then lv-type = 4.
    end case.
                     
    /* create the value */
    set-size(lv-memptr)   = length(pv-value) + 1.
       
    put-string(lv-memptr,1) = pv-value. 

    run RegSetValueExA(lv-hkey,
                       pv-key,
                       0, /* must be 0 */
                       lv-type, 
                       lv-memptr,
                       get-size(lv-memptr),
                       output lv-result).
                     
    run RegCloseKey(lv-hkey,output lv-result).


  RETURN lv-result.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSetSysColors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiSetSysColors Procedure 
FUNCTION WapiSetSysColors RETURNS CHARACTER
  ( cDspElements   as int,
    lpnDspElements as int64,
    lpdwRgbValues  as int64 ) :


 run SetSysColors (cDspElements,          /* = number of elements */
                   lpnDspElements,
                   lpdwRgbValues).

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiShellExecute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiShellExecute Procedure 
FUNCTION WapiShellExecute RETURNS INTEGER
  (pv-prog   as char,
   pv-dir    as char,
   pv-params as char,
   pv-mode   as int) :
   def var h-instance as int no-undo.
 RUN ShellExecutea(0,
                   "open",
                   pv-prog,
                   pv-params,
                   pv-dir,
                   pv-mode,
                   OUTPUT h-Instance).
  RETURN h-instance.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WapiSleep) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WapiSleep Procedure 
FUNCTION WapiSleep RETURNS CHARACTER
  ( vi-milliseconds as int ) :
    run sleep in this-procedure (vi-milliseconds).
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Win-Exec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Win-Exec Procedure 
FUNCTION Win-Exec returns logical
   ( ProgramName  as char,
     Presentation as int) :
/* 0 hidden 1 normal 2 min 3 max */
run winexec (ProgramName,Presentation).      

  return true.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WinErrorCodes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WinErrorCodes Procedure 
FUNCTION WinErrorCodes returns character
    (Pv-code as int):
def var lv-txt as char no-undo.
 
if Pv-code < 0 or 
   Pv-code > 32 then return "". /* no error */
 
case Pv-code :
  when  0 then lv-txt = "The operating system is out of memory or resources.".
  when  2 then lv-txt = "The specified file was not found".
  when  3 then lv-txt = "The specified path was not found.".
  when  5 then lv-txt = "The operating system denied access to the specified file.".
  when  8 then lv-txt = "There was not enough memory to complete the operation.".
  when 10 then lv-txt = "Wrong Windows version".
  when 11 then lv-txt = "The .EXE file is invalid (non-Win32 .EXE or error in .EXE image).".
  when 12 then lv-txt = "Application was designed for a different operating system.".
  when 13 then lv-txt = "Application was designed for MS-DOS 4.0.".
  when 15 then lv-txt = "Attempt to load a real-mode program.".
  when 16 then lv-txt = "Attempt to load a second instance of an application with non-readonly data segments.".
  when 19 then lv-txt = "Attempt to load a compressed application file.".
  when 20 then lv-txt = "Dynamic-link library (DLL) file failure.".
  when 26 then lv-txt = "A sharing violation occurred.".
  when 27 then lv-txt = "The filename association is incomplete or invalid.".
  when 28 then lv-txt = "The DDE transaction could not be completed because the request timed out.".
  when 29 then lv-txt = "The DDE transaction failed.".
  when 30 then lv-txt = "The DDE transaction could not be completed because other DDE transactions were being processed.".
  when 31 then lv-txt = "There is no application associated with the given filename extension.".
  when 32 then lv-txt = "The specified dynamic-link library was not found.".
  otherwise    lv-txt = "Undocumented Win error".
end.
 
return string(pv-code) + ': ' + lv-txt.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ZipToFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ZipToFile Procedure 
FUNCTION ZipToFile returns character
  ( pv-sourcefile as char,
    pv-sourceptr as memptr,
    pv-targetfile as char) :
    
    def var lv-targetptr  as memptr no-undo.


   /* make sure there is nothing left over in the memory pointer! */
   set-size(lv-targetptr) = 0.

    run proc-zip ('compress',
                  pv-sourcefile,pv-sourceptr,
                  pv-targetfile,output lv-targetptr).
if return-value ne '' then message 
      program-name(1)  "problem:" return-value 
      view-as alert-box error.

  return pv-targetfile .   /* Function return value. */


end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ZipToMemptr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ZipToMemptr Procedure 
FUNCTION ZipToMemptr returns memptr
  ( pv-sourcefile as char,
    pv-sourceptr as memptr) :
    
    def var lv-targetptr  as memptr no-undo.
    def var lv-targetfile as char   no-undo.


   /* make sure there is nothing left over in the memory pointer! */
   set-size(lv-targetptr) = 0.

    run proc-zip ('compress',
                  pv-sourcefile,pv-sourceptr,
                  lv-targetfile,output lv-targetptr).
   if return-value ne '' then message 
      program-name(1)  "problem:" return-value 
      view-as alert-box error.
  return lv-targetptr.   /* Function return value. */
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

