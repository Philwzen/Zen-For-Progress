&glob core   .
&glob ComboDelim  ,
{winconst.i}  /* windows preprocessor constants*/

/*** windows dll calls ****/

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

procedure OpenProcess EXTERNAL {&kernel} :
  def input param dwDesiredAccess as LONG.
  def input param bInheritHandle  as LONG.
  def input param dwProcessId     as LONG.
  define return param hProcess        as LONG.
end procedure.

procedure Process32Next EXTERNAL {&kernel} :
  def input param hSnapShot         as LONG.
  def input param lpProcessEntry32  as memptr.
  define return param ReturnValue       as LONG.
end procedure.

procedure CloseHandle EXTERNAL {&kernel} :
  def input param hObject     as LONG.
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
/**** end DLL nternal procedures *****/


FUNCTION WapiGetProcessName returns character
  ( pid as int ) :
  def var szProcessName as char    no-undo.
  run proc-GetProcessName in this-procedure (pid,output szProcessName).
  return TRIM(szProcessName).
end function.

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

def var v-list as char no-undo
    view-as Selection-list inner-chars 50 inner-lines 10 .

form  v-list with frame a.
def frame a. 
   .
v-list:list-items in frame a = WapiListProcesses().

 enable v-list  
 with frame a .
