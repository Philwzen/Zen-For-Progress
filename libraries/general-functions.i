Function AddLastSlash       Returns Char   (pv-directoryName as char) in {&lhandle}.
Function Backup             Returns Log    (Pv-from         as Char, 
                                            pv-to           as Char) 	in {&lhandle}.
Function BuildCombo         Returns Char   (cb-combo        as Handle,
	   		   	      	                    pv-table        as Char,
					                        pv-key          as Char,
					                        pv-field        as Char,
					                        pv-where        as Char,
					                        pv-by           as Char,
					                        pv-none         as Log,
					                        pv-all          as Log) 	in {&lhandle}.    
Function CenterWindow       Returns Log    (WinHandle       as Handle)  in {&lhandle}.
Function chartime           Returns Char   (pv-time         as int)  	in {&lhandle}.
Function cleanupQuery       returns logical (hQuery         as handle)     in {&lhandle}.
Function ConvPcl            Returns Char (IPFile as char,ConvType as char) in {&lhandle}.
Function CreateFile         Returns Char   (pv-file         as Char)    in {&lhandle}.
Function DateInWords        Returns Char   (ip-date         AS DATE,
                                            ip-long-name    AS Log) 	in {&lhandle}. 
Function DayName            Returns Char   (ip-date         AS DATE)  	in {&lhandle}.
Function DirectoryNotFound  Returns Log    (pv-fname        as Char)    in {&lhandle}.
Function DoNotFire          Returns Log    (WidList         as Char)    in {&lhandle}.
Function DosPath            Returns Char   (PathString      as Char)    in {&lhandle}.
Function ExecHandle         Returns Handle (pv-appsrv       as Char,
                                            pv-path         as Char,
                                            pv-prog         as Char)    in {&lhandle}.
Function ExportBrowse       Returns Log    (pv-Handle       as Handle)  in {&lhandle}.
Function filenotfound       Returns Log    (pv-fname        as Char)    in {&lhandle}.
Function FixedString        Returns char   (pv-str as char, pv-allownumeric as log)  in {&lhandle}.
Function FixPath            Returns Char   (pv-fullpath     as Char)    in {&lhandle}.
Function Framechanged       Returns Log    (FrameHandle     as Handle) 	in {&lhandle}.
Function GetAttribute       returns char   (pv-wid          as handle,
                                            pv-attrib       as char)    in {&lhandle}.
Function GetButPos          returns char   (pv-frame        as handle)  in {&lhandle}.
Function GetComboKey        returns Char   (ComboHandle     as Handle) 	in {&lhandle}.
Function GetDlcBin          returns Char   ()    in {&lhandle}.
Function GetFieldValue      returns Char   (pv-buffer as handle,fieldname as char,pv-extent as int) in {&lhandle}.
Function GetFileName        returns Char   (pv-fullpath     as Char)    in {&lhandle}.
Function GetFullPath        returns Char   (pv-file         as Char)    in {&lhandle}.
Function GetHdr             returns char   (framehandle     as handle)	in {&lhandle}.
Function GetIniValue   	    returns Char   (Char,Char) 	                in {&lhandle}.
Function GetLockingCulprit  returns Char   (tableRecid      as recid) in {&lhandle}.
Function GetNamedValue      Returns Char   ( pv-name as char,
                                             pv-values as char )    in {&lhandle}.
Function GetServerValueFor  Returns char   (pvproperty as char)  				            in {&lhandle}.
Function GetStringEntry     returns Char   (pvitem           as Char,
                                            pvproperties    as Char,
                                            pvvalues        as Char,
                                            pvdelim         as Char)    in {&lhandle}.
Function GetSystemName      Returns Char   ()                           in {&lhandle}.
Function GetWidHandle       returns handle (framehandle     as handle,
                                            widname         as char) 	in {&lhandle}.
Function InputFromFile	    Returns Memptr (pv-file as char,pv-local as char) in {&lhandle}.
Function InDevMode          Returns Log    ()                     		in {&lhandle}.
Function IntegerTime        Returns Int    (TimeString      as Char)    in {&lhandle}.
Function IntToHex           Returns Char   (pv-num 	        as Int)     in {&lhandle}.
Function IsInteger          Returns Log    (stringToCheck   as Char)    in {&lhandle}.
Function IsNull             Returns Log    (pv-string       as Char)    in {&lhandle}.
Function IsNumeric          Returns Log    (StringToCheck   As Char)    in {&lhandle}.
Function IsRunning          Returns Log    (pv-proc         as Char)    in {&lhandle}.
Function JumpTo             Returns Handle (pv-wid          as Char)    in {&lhandle}.
Function lastday            Returns Int    (Dte             as DATE)    in {&lhandle}.   
Function LoadDefBackGround  Returns Log    (LayoutName      as char,
                                            framehandle     as handle,
                                            winhandle       as handle)  in {&lhandle}.   
Function LogicalAnd Returns Int             (pi1 as int, 
                                            pi2 as int )   in {&lhandle}.   
Function MonthName          Returns Char   (ip-date         AS DATE)  	in {&lhandle}.
Function NumRecords         Returns Int    (pv-msg          as char,
                                            pv-data         as handle)      in {&lhandle}.
Function PrintBrowse        Returns Log    (pv-Handle       as Handle,
                                            pv-title        as char)    in {&lhandle}.
Function properform         Returns char    (pv-string      as char)  in {&lhandle}.                   
Function OutputToPdf returns char (textfile as char) in {&lhandle}.
Function OutputToScreen returns log (textfile as char) in {&lhandle}.
Function OutputToFile	    Returns char   (pv-file as char,pv-mptr as memptr,pv-local as char) in {&lhandle}.
Function RelabelBrowse      Returns Log    (pv-hand         as Handle,
                                            pv-DateFormat   as Char)    in {&lhandle}.
Function RunChild           Returns Handle (pv-child        as Char,
                                            pv-parent       as Handle)  in {&lhandle}.
Function SelectedItems      Returns Char   (pv-sellist as handle) in {&lhandle}.
Function SetAuditMode       Returns Log    (pv-table as char,pv-mode as char) in {&lhandle}.
Function SetAllLkBut        Returns Log    (pv-frame        as handle) in {&lhandle}.
Function SetAttributes      returns char   (pv-wid          as handle,
                                            pv-attrib       as char,
                                            pv-value        as char,
                                            pv-datatype     as char)    in {&lhandle}.
Function SetComboValue      Returns Char   (ComboValue      as Char,
                                            ComboHandle     as Handle)  in {&lhandle}.
Function SetCursor          Returns Log    (pv-Handle       as Handle,
                                            pv-cursor       as Char)	in {&lhandle}.
Function SetFrameFocus      Returns Handle (pv-frameHandle  as Handle)	in {&lhandle}.
Function SetIniValue   	    Returns Char   (Char,Char,char) 	        in {&lhandle}.
Function SetLkBut           Returns Log    (pv1-handle      as handle,
                                            pv2-handle      as handle,
                                            pv-log          as log) in {&lhandle}.
Function SetNamedValue      Returns Char   ( pv-name as char,
                                             pv-value as char,
                                             pv-list as char )    in {&lhandle}.
Function SetNotModified     Returns Log    (FrameHandle     as Handle) 	in {&lhandle}.
Function SetOpDest          Returns Char   (input-output pv-params as char)  				            in {&lhandle}.
Function SetRegValue   	 Returns Char   (pv-section as char,pv-key as char,pv-value as char) 	        in {&lhandle}.
Function SetSession         Returns Log    (pv-state        as Char)	in {&lhandle}.
Function SetWinState        Returns Log    (winhandle       as handle,    
                                            pv-state        as int)	    in {&lhandle}.
Function StringToDate       Returns Date   (Datestring      as Char) 	in {&lhandle}.                   
Function StringToDec        Returns Dec    (StringVar       As Char,
                                            sep             as Char,
                                            poInt           as Char)    in {&lhandle}.
Function StringToInt        Returns Int    (StringVar       As Char,
                                            sep             as Char)    in {&lhandle}.
Function StringToLog        Returns Log    (StringVar       As Char)    in {&lhandle}.
Function ToLower            Returns Log    (winhand         as handle)  in {&lhandle}.
Function ToUpper            Returns Log    (winhand         as handle)  in {&lhandle}.
Function Touch              Returns Log    (Filename        as char)    in {&lhandle}.
Function UnixPath           Returns Char   (PathString      as Char)    in {&lhandle}.
Function ValidUserSec       returns log    (notusers        as char,
                                            notgroups       as char,
							                runusers        as char,
                                            rungroups       as char,
							                username        as char,
                                            usergroup       as char)    in {&lhandle}.
Function ValidateDirectory  returns log    (dirname as char)  in {&lhandle}.
Function WAIT               Returns Log    (milliseconds    AS Int)     in {&lhandle}.       
Function WidgetExists       Returns log    (FrameHandle     as Handle,
                                            WidgetName      As Char)    in {&lhandle}.
Function WidInfo	        Returns Char   ()				      	    in {&lhandle}.
