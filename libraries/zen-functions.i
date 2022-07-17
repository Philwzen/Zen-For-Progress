Function AltLanguage        returns char   (TextString      as char)        		in {&lhandle}.
Function AnyErrors          Returns Log    ()                           in {&lhandle}.
Function AttachMenu         returns log    (pv-window       as handle,pv-frame as handle,pv-procedure as handle) in {&lhandle}.
Function AnyServerMessages  Returns Log    ()                           in {&lhandle}.

Function BtnHelp            Returns Char   (prog            as handle,pv-on as log)    in {&lhandle}.

Function CanFind      Returns Log   (pv-table        as Char,
					                    pv-where        as Char) 	in {&lhandle}.    
function CanEdit             returns log    (ProgramName     as char) in {&lhandle}.
Function CanRun             returns log    (ProgramName     as char) in {&lhandle}.
Function CheckForBackGroundErrors Returns Log (pv-prog as handle,pv-email as char) in  {&lhandle}.
Function ClassCodeDesc      returns char   (Class           as char,
		   					                Code            as char)            in {&lhandle}.
Function ClassCodes         RETURNS CHAR   (Class           as char,
							                output descriptions as char) in {&lhandle}.
Function CleanSession       Returns Log    ()                           in {&lhandle}.
Function ClearAppserver     Returns Log    ()                           in {&lhandle}.
Function CreateButs         Returns Log    (pv-butparams    as Char ) 	in {&lhandle}.
Function CtrlCounter        Returns Int    (countername as char ,increment as int,allownegative as log) in {&lhandle}.
Function DateSep            returns char   (LangCode        as int)        			in {&lhandle}.
Function DeleteAllSysVars   returns log    ()                                   in {&lhandle}.
Function DeleteSysVar       returns log    (VarName         As char)                    in {&lhandle}.
Function DispConnections    Returns Char   ()                           in {&lhandle}.
Function DispExecMess       Returns Char   (pv-mess as char) in {&lhandle}.
Function Errorclear         Returns Log    ()                           in {&lhandle}.
Function Errorcreate        Returns Log    (ErrNum          as Int,  
                                            Sub#1           as Char,
                                            Sub#2           as Char,
                                            Sub#3           as Char,
                                            Sub#4           as Char)   in {&lhandle}.
Function Execute            Returns Log    (Program         as Char,
                                            Dir             as Char,
                                            Params          as Char,
                                            mode            as Char)    in {&lhandle}.
Function Fkey               Returns Char   (pv-prochandle as handle)	in {&lhandle}.
Function ForceLocal         Returns Log    (pv-onoff        as Log)     in {&lhandle}.
Function freezewindow       Returns Log    (pv-window       as Handle,
                                            pv-onoff        as Int)     in {&lhandle}.                   
Function GetApiDetail       RETURNS Log    (pvapiname       as char,output pvproperties as char,output pvvalues as char) in {&lhandle}.
Function getappserverHandle Returns Handle (AppSrvName      As Char) 	in {&lhandle}.
Function GetAppServerNames  returns char   ()                                  in {&lhandle}.
Function GetBlobCtrl        Returns memptr (CtrlKey as char,output filename as char)        	      in {&lhandle}.
Function GetBlobCtrlRename  returns memptr (pv-idx          as char,
                                            input-output pv-file as char) in {&lhandle}.
Function GetClientVersion   returns Decimal ()   in {&lhandle}.
function GetColumnHandle    returns handle (pv-browsehandle as handle,
                                            pv-columnlabel  as char)   in {&lhandle}.
Function GetColour          RETURNS INT    (ip-colour-name  AS CHAR) in {&lhandle}.
Function GetCtrl            Returns char   (CtrlKey         as char)        	      in {&lhandle}.
Function GetCurrency        RETURNS CHAR   (ip-country      as int) in {&lhandle}.
Function GetField           Returns Char   (pv-table        as Char,
					                    pv-keyfield     as Char,
				   	                    pv-keydata      as Char,
  				                            pv-datafield    as Char) 	in {&lhandle}.    
Function GetFieldWhere      Returns Char   (pv-table        as Char,
					                    pv-where        as Char,
  				                            pv-datafield    as Char) 	in {&lhandle}.    
Function GetOsFile          Returns Char   (filename        as Char)    in {&lhandle}.   
Function GetLogFileName     Returns Char   (pv-prog as handle) in {&lhandle}. 
Function GetLookupInfo      Returns Char   (pv-focus as handle,pv-mode as char) in {&lhandle}.
Function GetParentHandle    Returns Handle (pv-child        as Handle) in {&lhandle}.
Function GetProperty        Returns Char   (pv-type       as Char,
                                            pv-parent     as Char,
                                            pv-property   as Char)   in {&lhandle}.

Function GetProcHandle      Returns Handle (pv-appsrv       as Char,
                                            pv-proc         as Char)   in {&lhandle}.
Function GetRegEntry        Returns Char   (pv-hkey         as Char,
					    pv-base as char,
					    pv-section as char,
                                            pv-item         as Char)   in {&lhandle}.
function GetScratchName returns char (
    pv-extension as char,
    pv-fullpath as log) in {&lhandle}.
Function GetSysVar          Returns Char   (Pv-var          as Char) 	in {&lhandle}.
Function GetUserid          Returns Char   ()   in {&lhandle}.
Function HadErrors          Returns Log    ()                          in {&lhandle}.
Function InitLibraries      Returns Log    (pv-user as char)				            in {&lhandle}.
Function LoadFieldDefaults  Returns Log    (pv-procedure    as handle,
                                            pv-frame        as handle)  in {&lhandle}.
Function LogAction          Returns Log    (pv-prog as char ,
                                            pv-action as char,
                                            pv-msg as char)             in {&lhandle}.
Function LogMessage         Returns Log    (pv-msg as char,pv-logfile as char,pv-always as char)             in {&lhandle}.
Function MakeButton         Returns Handle (pv-proc         as Handle,
					          pv-name         as Char,
					          pv-frame        as Handle,
					          pv-sensitive    as Log,
					          pv-flat         as Log,
 					          pv-label        as Char,
					          pv-width        as dec,
					          pv-height       as dec,
					          pv-row          as dec,
					          pv-col          as dec,
					          pv-help         as Char,
					          pv-visible      as Log,
						   pv-icon         as char) 	in {&lhandle}.

Function MakeLookupButtons  Returns Log    (pv-procedure    as handle,
                                            pv-frame        as handle)  in {&lhandle}.
Function MaxDataGuess       returns int    (ProgramName     as char)         		in {&lhandle}.
Function MenuMsg            returns handle (txt     as char)         		in {&lhandle}.
Function MenuLabel	       returns Log    (WinHandle 	    as handle)         		in {&lhandle}.
Function MenuOptionFrom     returns Char   (pv-procedure as handle,pv-mode as char) in {&lhandle}.
Function Msg                Returns Char   (MsgNum          as Int,
                                            Sub#1           as Char,
                                            Sub#2           as Char,
                                            Sub#3           as Char,
                                            Sub#4           as Char)   in {&lhandle}.
Function PgmAuthor          returns char   (Program         as char)   in {&lhandle}.
Function PgmComments        returns char   (Program         as char)   in {&lhandle}.
Function PgmMenuGroup       returns char   (Program         As char)                   in {&lhandle}.
Function PgmMenuParent      returns char   (Program         As char)                   in {&lhandle}.
Function PgmId              RETURNS dec    (programname     AS char)                  in {&lhandle}.
Function PgmMultiinstance   returns Log    (Program         As char)                   in {&lhandle}.
Function PgmName            RETURNS CHAR   (programid       AS DEC)                  in {&lhandle}.
Function PgmProperty        returns char   (Program         as char,property as char)   in {&lhandle}.
Function PgmRepInfo         returns char   (Program         As char)                   in {&lhandle}.
Function PgmUseDefaults     returns Log    (Program         As handle)                   in {&lhandle}.
Function ProgramDescription returns char   (ProgramName     as char)        		in {&lhandle}.
Function ProgramTitle       returns char   (ProgramName     as char,PV-MODE AS CHAR)        		in {&lhandle}.
Function RefreshTempTables  RETURNS log    () in {&lhandle}.
Function ReLabel            RETURNS log    (fhand as handle,mode as char)  in {&lhandle}.
Function RGBColour          RETURNS CHAR   (ip-colour-name  AS CHAR)  in {&lhandle}.
Function RunRemote          Returns Log    (pv-params as char)                     		in {&lhandle}.
Function SecurityCheck      Returns Log    (pv-user as char,  
                                            pv-group           as Char,
                                            pv-notusers        as Char,
                                            pv-notgroups       as Char,
                                            pv-okusers         as Char,
                                            pv-okgroups as char)   in {&lhandle}.
Function ServerMessagesClear        Returns Log    ()                           in {&lhandle}.
Function ServerMessageCreate        Returns Log    (ErrNum          as Int,  
                                            Sub#1           as Char,
                                            Sub#2           as Char,
                                            Sub#3           as Char,
                                            Sub#4           as Char)   in {&lhandle}.
Function ScreenChanged      returns log    (FrameHandle     as Handle)  		in {&lhandle}.
Function SessionId          returns char    ()  		in {&lhandle}.
Function SetBgColour        returns log    (pv-frame as handle,pv-widlist as char,pv-colour as char) in {&lhandle}.
Function SetCtrl            returns log    (CtrlKey         as char,Ctrl-Value as char) in {&lhandle}.
function SetExecMessHandle  returns char   (pv-hand as handle )   in {&lhandle}.

Function SetSensitive       Returns Log    (pv-enable       as Log ,
					                        pv-ix           as Char,
					                        pv-exc-list     as Char,
					                        pv-frame-Handle as Handle)  in {&lhandle}.
Function SetSessionLangFormats returns log (LangCode        as int)         		in {&lhandle}.
Function SetSystem          returns Log    (SystemName      as  char)                                  in {&lhandle}.
Function SetSysVar          Returns Log    (Pv-var          as Char,
                                            pv-value        as Char) 	in {&lhandle}.
Function SetTableAudit      Returns char   (pv-tablelist             As Char,
                                            pv-state             as Char)    in {&lhandle}.

Function SetUsrid	    Returns Log    (usr             as char)    in {&lhandle}.
Function Setusrpwd          Returns Log    (usr             As Char,
                                            pwd             as Char)    in {&lhandle}.
Function SetWinPosition     Returns Log    (WinHandle       as Handle,
					    prog            as char,
					    xadjust as int,
					    yadjust as int) in {&lhandle}.
Function SetWorkingDir      Returns Log    (DirString       as Char)    in {&lhandle}.
Function Sound              returns log    (SoundFile       As char)      in {&lhandle}.
Function SpellCheck	        Returns Char   ()				      	    in {&lhandle}.
Function SysMsg             Returns Log    (Msg             As Char)   in {&lhandle}.
Function SystemManager      returns log    (UserName        as char)         			 in {&lhandle}.
Function TabLabel           RETURNS log    (chand 	as com-handle,fhand as handle)  in {&lhandle}.
Function Tooltip            Returns Log    (pv-frame        as Handle,changefont as log)  in {&lhandle}.
Function Usercountry        returns CHAR   (UserName        as char)         			 in {&lhandle}.
Function UserGroup          returns char   (UserName        as char)      			 in {&lhandle}.
Function UserName           returns char   (UserName        as char)           			 in {&lhandle}.
Function UserAutotimeclock  returns log    (UserName        as char)         			 in {&lhandle}.
Function UserLanguage       returns int    (UserName        as char)         			 in {&lhandle}.
Function ValidApi           RETURNS log    (APIName         as char)                   in {&lhandle}.
Function ValidClassCode     returns log    (Class           as char,
							                Code            as char,
						                    OUTPUT lv-DESC  AS CHAR)     in {&lhandle}.     
Function ValidDate          Returns Log    (DateString      As Char)    in {&lhandle}.
Function ValidScreenValue   Returns Log    (output extras   As Char)    in {&lhandle}.
Function ValidUser          Returns Log    (pv-user         As Char, 
                                            pv-pass         as Char)    in {&lhandle}.
Function WidgetInfo          returns char    ()  		in {&lhandle}.
Function WidSecCheck	        Returns Log   (fhand as handle,progname as char)    	    in {&lhandle}.
Function WHelp		        Returns Char   ()				      	    in {&lhandle}.

