function WapiGetSysColor    returns int (pv-colornum as int) in {&lhandle}.
Function WapiSetSysColors   returns char (cDspElements   as int,
                                            lpnDspElements as int64,
                                            lpdwRgbValues  as int64 ) in {&lhandle}.

function WapiShellExecute RETURNS INTEGER
  (pv-prog   as char,
   pv-dir    as char,
   pv-params as char,
   pv-mode   as int)  in {&lhandle}.
function WapiProPrintFile RETURNS INTEGER
  (pv-printerhandle as int,
   pv-flags as int,
   pv-hwnd as int,
   pv-fontnum as int,
   pv-filename as char,
   pv-pages as int )  in {&lhandle}.
Function WapiRegisterOcx RETURNS log  ( pv-lib as char)  in {&lhandle}.
Function WapiLoadLibrary RETURNS INTEGER  ( pv-lib as char)  in {&lhandle}.
function WapiSetCurrentDirectory RETURNS INTEGER  ( pv-dir as char )  in {&lhandle}.
function wapiplaysound RETURNS INTEGER  ( pv-sound as int ) in {&lhandle}.
function WapiMessageBox RETURNS INTEGER ( pv-hwnd as int,
                                          pv-mestxt as char,
                                          pv-title as char,
                                          pv-style as int ) in {&lhandle}.
function wapigloballock RETURNS INT64  ( pv-loc as int64 ) in {&lhandle}.
function wapiprintdlg RETURNS INT64  ( pv-printer as int64) in {&lhandle}.
function WapiSleep RETURNS CHARACTER  ( vi-milliseconds as int ) in {&lhandle}.
function WapiSetDefaultPrinter RETURNS INTEGER  ( pv-printer as char )  in {&lhandle}.

/* Functions in winapilibrary predeclare any Functions */
function GetUniqueId         returns char   () 	in {&lhandle}.
function ShowError          returns int  (errnum as int) in {&lhandle}.
function ShowLastError returns int ()	 in {&lhandle}.
function GetLastErrorNum returns int ()	 in {&lhandle}.
function GetParent returns int (hwnd as int)	 in {&lhandle}.

function WapiCreateProcess   returns int (commandline as char,dir as char,showwindow as int) in {&lhandle}.
function WapiFreezeWindow    returns log (Win    as handle,
                                          OnOff  as int)  in {&lhandle}.
function WapiListProcesses   returns char ()              in {&lhandle}.
function WapiGetProcessName  returns char (Pid   as int)  in {&lhandle}.
function WapiKillProcess     returns log  (Pid   as int)  in {&lhandle}.
function WapiGetPrinters     returns char ()              in {&lhandle}.
function WapiGetWinVersion   returns char ()              in {&lhandle}.
function WapiGetShareName    returns char (Drive as char) in {&lhandle}.
Function WapiGetUserName RETURNS CHARACTER  ( ) in {&lhandle}.

function WapiIsRunning       returns int  (ExeName as char) in {&lhandle}.

function WapiFileCopy       returns int  (FromName as char,fromname as char,disperrors as log) in {&lhandle}.
function WapiFileDelete     returns int  (FileName as char,disperrors as log) in {&lhandle}.

function WapiRawPrint        returns char (PrinterName as char,
                                           FileName    as char,
                                           numcopies   as int) 
                                                          in {&lhandle}.
function WapiFilePrint       returns log  (FileName as char) in {&lhandle}.
function WapiFileExecute     returns int  (FileName as char) in {&lhandle}.
function WapiFileExecuteWait returns log  (FileName as char) in {&lhandle}.
function WinErrorCodes       returns char (code as int)   in {&lhandle}.
function Win-Exec	     returns log   (progname as char,presentation as int)    	    in {&lhandle}.
function WapiSendMail        returns char  (pv-method      as char,
		 			    pv-FromName    as char,
					    pv-ToNames     as char,
				            pv-cc          as char,
					    pv-Subject     as char,							
					    pv-MessageText as char,
					    pv-Attachments as char)  in {&lhandle}.
function WapiSetRegEntry returns int  (pv-section as char,
 		 			    pv-path  as char,
					    pv-key   as char,
				           pv-type  as char,
					    pv-value as char)  in {&lhandle}.
function UnZipToFile    return char (pv-sourcefile as char,
                                        pv-sourceptr as memptr,
                                        pv-targetfile as char)  in {&lhandle}.
function ZipToFile    return char (pv-sourcefile as char,
                                    pv-sourceptr as memptr,
                                    pv-targetfile as char)  in {&lhandle}.
function UnZipToMemptr    return memptr (pv-sourcefile as char,
                                        pv-sourceptr as memptr)  in {&lhandle}.
function ZipToMemptr    return memptr (pv-sourcefile as char,
                                        pv-sourceptr as memptr)  in {&lhandle}.

/* these need to be added as functions not runs
setdefaultprintera  generallibrary.p print/prtque.p
shellexecutea  zen-library
closehandle ok but could be better
getsyscolor zenlibrary.p
proprintfile  proprint.p officeserver.w prtque.p
proprintfile16 proprint.p
setcurrentdirectorya zenlibrary.p
loadlibrarya crystallibrary.p
sleep generallibrary.p
messageboxa zenlibrary.p
playsounda zenlibrary.p

*/
