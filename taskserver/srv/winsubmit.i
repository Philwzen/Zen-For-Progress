&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/* ***************************  Definitions  ************************** */

&GLOB A A
&Glob HWND long
&Glob BOOL long
&Glob HINSTANCE long
&Glob INT long
&GLOB INTSIZE 4

&GLOBAL-DEFINE STANDARD_RIGHTS_REQUIRED 983040
&GLOBAL-DEFINE SYNCHRONIZE 1048576
&GLOBAL-DEFINE PROCESS_ALL_ACCESS ({&STANDARD_RIGHTS_REQUIRED} + {&SYNCHRONIZE} + 4095)
&GLOBAL-DEFINE PROCESS_QUERY_INFORMATION 1024

&GLOB USER     "user32"
&GLOB PS       "psapi"
&GLOB KERNEL   "kernel32"
&GLOB SHELL    "shell32"
&GLOB MAPI     "mapi32"
&GLOB GDI      "gdi32"
&GLOB MMEDIA   "winmm"
&GLOB WINSPOOL "winspool.drv"
&GLOB ADVAPI   "advapi32"

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreatePro Include 
FUNCTION CreatePro RETURNS INTEGER
         (input  ipc-cmd-line    as CHAR,
          input  ipc-workdir     as CHAR,
          OUTPUT opi-pid as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EnumPro Include 
FUNCTION EnumPro RETURNS INTEGER
  (INPUT  hProcess AS INT, OUTPUT hModule    AS INT,
   INPUT  ibytes   AS INT, OUTPUT ineedbytes AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetError Include 
FUNCTION GetError RETURNS CHARACTER
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OpenPro Include 
FUNCTION OpenPro RETURNS INT
  (dwDesiredAccess  AS INT, bInheritHandle AS INT, dwProcessId AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TerminatePro Include 
FUNCTION TerminatePro RETURNS LOGICAL
  (hprocess AS INT, uExitCode AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */


PROCEDURE CreateProcess{&A} EXTERNAL {&KERNEL} :

  DEFINE INPUT  PARAMETER lpApplicationName    AS LONG. 
  DEFINE INPUT  PARAMETER lpCommandline        AS CHAR. /* NULL */
  DEFINE INPUT  PARAMETER lpProcessAttributes  AS LONG.
  DEFINE INPUT  PARAMETER lpThreadAttributes   AS LONG.
  DEFINE INPUT  PARAMETER bInheritHandles      AS {&BOOL}.
  DEFINE INPUT  PARAMETER dCreationFlags       AS LONG.
  DEFINE INPUT  PARAMETER lpEnvironment        AS LONG.
  DEFINE INPUT  PARAMETER lpCurrentDirectory   AS LONG.
  DEFINE INPUT  PARAMETER lpStartupInfo        AS LONG.
  DEFINE INPUT  PARAMETER lpProcessInformation AS LONG.
  DEFINE RETURN PARAMETER bResult              AS {&BOOL}.

END PROCEDURE.


PROCEDURE EnumProcessModules EXTERNAL {&ps} :

  DEFINE INPUT  PARAMETER hProcess    AS LONG. 
  DEFINE OUTPUT PARAMETER hModule     AS LONG.
  DEFINE INPUT  PARAMETER ibytes      AS LONG.
  DEFINE OUTPUT PARAMETER ineedbytes  AS LONG.
  DEFINE RETURN PARAMETER bResult     AS {&BOOL}.

END PROCEDURE.


PROCEDURE GetLastError EXTERNAL {&kernel} :

  DEFINE RETURN PARAMETER bResult     AS CHAR.

END PROCEDURE.


/* can also use exitprocess - exitprocess is prefered method accprding to MS 
   but can only seem to exit the current process you are in - not specify one - gay! */
PROCEDURE TerminateProcess EXTERNAL {&KERNEL} :

  DEFINE INPUT  PARAMETER hProcess  AS LONG.
  DEFINE INPUT  PARAMETER uExitCode AS LONG.
  DEFINE RETURN PARAMETER bResult   AS {&bool}.

END PROCEDURE.


PROCEDURE OpenProcess EXTERNAL {&KERNEL} :

  DEFINE INPUT  PARAMETER dwDesiredAccess  AS LONG.
  DEFINE INPUT  PARAMETER bInheritHandle   AS LONG.
  DEFINE INPUT  PARAMETER dwProcessId      AS LONG.
  DEFINE RETURN PARAMETER hprocess         AS LONG.

END PROCEDURE.

/* old method for 16bit apps. if need */
PROCEDURE WinExec EXTERNAL  {&KERNEL}:

    DEF INPUT PARAMETER ProgramName as char.
    DEF INPUT PARAMETER Presentation AS long.
    def return parameter ret-code as long.

END PROCEDURE.


PROCEDURE CloseHandle EXTERNAL {&KERNEL} :

  DEFINE INPUT  PARAMETER hObject     AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.

END PROCEDURE.


PROCEDURE GetShortPathNameA EXTERNAL {&KERNEL} :

  DEFINE INPUT  PARAMETER  lpszLongPath  AS CHAR.
  DEFINE OUTPUT PARAMETER  lpszShortPath AS CHAR.
  DEFINE INPUT  PARAMETER  cchBuffer     AS LONG.
  DEFINE RETURN PARAMETER  ReturnValue   AS LONG.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreatePro Include 
FUNCTION CreatePro RETURNS INTEGER
         (input  ipc-cmd-line    as CHAR,
          input  ipc-workdir     as CHAR,
          OUTPUT opi-pid as int) :    

   def var vmp-startup-info as MEMPTR NO-UNDO.
   def var vmp-directory    as memptr NO-UNDO.
   def var vmp-process-info as memptr NO-UNDO.
   
   def var lv-result  as int NO-UNDO.
   def var vh-process as int no-undo.
   def var vh-thread  as int no-undo.
   DEF VAR lv-tid     as int NO-UNDO. /* thread ID */
   DEF VAR lv-Return  as int NO-UNDO.

   /* set memptr size - creating startup window information for process */
   /* not sure of the positions within vmp-startup-info - can find out what to set values 
      inside that memory pointer to - just not sure what position in the MP is what??
      or how the hell to find out! */

   set-size(vmp-startup-info)     = 68.
   put-long(vmp-startup-info,1)   = 68.
   put-long (vmp-startup-info,45) = 1.  /* pos. 45 = dwflags attribute - 1 = startif_usesshowwindow */                
   put-short(vmp-startup-info,49) = 0.  /* do not show window - allegedly !*/
   set-size(vmp-process-info)     = 16. /* set memptr size */
   
   if ipc-workdir <> "" then do:
      set-size(vmp-directory)     = 256.
      put-string(vmp-directory,1) = ipc-workdir.
   end.   

   /* can do more than this - can create with high prioirty or tell windows to use only
      idle processor strokes to run this new process etc. etc. */
   run CreateProcess{&A} in this-procedure
     ( 0, /* lpApplicationName */
       ipc-cmd-line, /* lpipc-cmd-line */
       0, /* lpProcessAttributes */
       0, /* lpThreadAttributes */
       0, /* bInheritHandles */
       8, /* dCreationFlags 0 or detached_process 0x00000008 */
       0, /* lpEnvironment */
       if ipc-workdir="" then 0 
       else get-pointer-value(vmp-directory), /* vmp-directory */
       get-pointer-value(vmp-startup-info), /* vmp-startup-info */
       get-pointer-value(vmp-process-info), /* vmp-process-info */
       output lv-result /* lv-result */
     ).

  ASSIGN vh-process = GET-LONG(vmp-process-info,1)   /* first 4 digits - handle is 4 long */
         opi-pid    = get-long(vmp-process-info,9)   /* process id - next 4 digits ?? */
         vh-thread  = get-long(vmp-process-info,5)   /* second 4 digits - handle is 4 long */
         lv-tid     = GET-LONG(vmp-process-info,13). /* thread id - not sure yet - hopefully correct */

  /* we are not interested in keeping vh-process/vh-thread so let's invalidate the handle right now. 
     This DOES NOT mean the process/thread is terminated, it just means that Kernel doesn't need to keep 
     the object for US. We can get these handle back using the openprocess function etc. 
  */

  RUN CloseHandle in this-procedure (vh-thread, output lv-return). 
  RUN CloseHandle in this-procedure (vh-process, output lv-return). 

  /* reset memory pointers */
  set-size(vmp-startup-info) = 0.
  set-size(vmp-process-info) = 0.
  set-size(vmp-directory)    = 0.

  RETURN(vh-process). /* windows handle to the process */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EnumPro Include 
FUNCTION EnumPro RETURNS INTEGER
  (INPUT  hProcess AS INT, OUTPUT hModule    AS INT,
   INPUT  ibytes   AS INT, OUTPUT ineedbytes AS INT):

  DEF VAR lv-result AS INT NO-UNDO.
  
  run EnumProcessModules in this-procedure
     ( hprocess, 
       OUTPUT hmodule,
       ibytes, 
       OUTPUT ineedbytes, 
       OUTPUT lv-result).  

  RETURN lv-result.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetError Include 
FUNCTION GetError RETURNS CHARACTER
  (  ) :

  DEF VAR lv-error AS CHAR NO-UNDO.

  RUN getlasterror IN THIS-PROCEDURE (OUTPUT lv-error).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OpenPro Include 
FUNCTION OpenPro RETURNS INT
  (dwDesiredAccess  AS INT, bInheritHandle AS INT, dwProcessId AS INT):

  DEF VAR lv-result AS INT NO-UNDO.
  
  /* as job will be passed through as null then this function will check if process
     is running or not */

  run OpenProcess in this-procedure
     (dwDesiredAccess,
       bInheritHandle,
       dwProcessId,
       OUTPUT lv-result).

  RETURN lv-result.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TerminatePro Include 
FUNCTION TerminatePro RETURNS LOGICAL
  (hprocess AS INT, uExitCode AS INT):
  
  DEF VAR lv-result AS INT NO-UNDO.

  run TerminateProcess in this-procedure
     ( hprocess,
       uexitcode,
       OUTPUT lv-result
     ).

  RETURN IF lv-result = 0 THEN FALSE ELSE TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

