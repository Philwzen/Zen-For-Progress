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
create widget-pool.
this-procedure:private-data = "library-zen".
&glob library-zen
&glob library-program
/* &glob bug * */

/* switch on debug log for these routines can use * for all 
   must be before app-paths.i*/
/* &glob bug screen,proc-createbuts,makebutton */

{app-paths.i}

{{&core}zen-temptables.i}

def stream zenlib.
def var lv-enable         as log  no-undo.
def var lv-ix             as char no-undo.
def var lv-exc-list       as char no-undo.
def var lv-lookupcolour   as int  NO-UNDO.
def var lv-activitylog    as char no-undo.
def var lv-logmessages    as char no-undo.
def var lv-logappserver   as char no-undo.
def var lv-SecurityAlgorithm  as int no-undo.
def var lv-appservernames as char no-undo.
def var lv-appcontext     as char no-undo.
def var lv-appvals        as char no-undo.
def var h-Appserverhandle as handle no-undo extent 10.

def var lv-system         as char   no-undo.
h-Appserverhandle = ?.
lv-appcontext = string(today) + ':' + string(session).

def var h-msg         as handle no-undo.
def var lv-scrfont as char no-undo.

define temp-table t-error no-undo
    field t-code    as int  column-label "Code"
    field t-message as char column-label 'Message'
    field t-Display as log  column-label 'Display'.

def workfile w-clientvar no-undo
    field varname as char
    field varvalue as char .

def var lv-sysvar   as char no-undo init 'system'.
def var lv-grey    as log  no-undo.
def var lv-group   as char no-undo.
def var lv-user    as char no-undo.
def var lv-country as char no-undo.
def var lv-uselookupbuttons as log no-undo.
def var lv-defbutx      as int no-undo.
def var lv-defbuty      as int no-undo.
def var lv-cachedone as log no-undo.

define temp-table w-appsrv no-undo
    field w-name as char
    field w-param as char
    field w-handle as handle
    field w-sessionid as char format 'x(255)'.

def var lv-UseAppserver as log no-undo.
Def Var lv-password As char No-undo.
def var lv-uniqueid as char no-undo.
def var lv-execmesshandle as handle no-undo.
&if defined(MaximiseLicenses) ne 0 &then
    def var lv-SessionId as char no-undo format 'x(255)'.
&endif
lv-UseAppserver = entry(1,session:parameter,'^') eq "appserver".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-AltLanguage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AltLanguage Procedure 
FUNCTION AltLanguage RETURNS CHARACTER
  ( pv-string as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AnyErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AnyErrors Procedure 
FUNCTION AnyErrors RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AnyServerMessages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AnyServerMessages Procedure 
FUNCTION AnyServerMessages RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AttachMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AttachMenu Procedure 
FUNCTION AttachMenu RETURNS LOGICAL
  ( pv-window as handle,
    pv-frame  as handle,
    pv-procedure as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BtnHelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BtnHelp Procedure 
FUNCTION BtnHelp RETURNS CHARACTER
  ( pv-prog as handle,
    pv-on as log )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CanEdit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CanEdit Procedure 
FUNCTION CanEdit RETURNS LOGICAL
  ( pv-proc-name   as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CanFind) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CanFind Procedure 
FUNCTION CanFind RETURNS LOGICAL
  (pv-table as char,
   pv-where as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CanRun) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CanRun Procedure 
FUNCTION CanRun RETURNS LOGICAL
  ( pv-proc-name   as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckForBackGroundErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckForBackGroundErrors Procedure 
FUNCTION CheckForBackGroundErrors RETURNS LOGICAL
  ( pv-prog as handle,
    pv-email as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClassCodeDesc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClassCodeDesc Procedure 
FUNCTION ClassCodeDesc RETURNS CHARACTER
  ( pv-class as char,
    pv-code  as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClassCodes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClassCodes Procedure 
FUNCTION ClassCodes RETURNS CHARACTER
  ( pv-class as char,
    output pv-desc  as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CleanSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CleanSession Procedure 
FUNCTION CleanSession RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearAppserver) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearAppserver Procedure 
FUNCTION ClearAppserver RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateButs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateButs Procedure 
FUNCTION CreateButs RETURNS LOGICAL
  ( pv-butparams    as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CtrlCounter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CtrlCounter Procedure 
FUNCTION CtrlCounter RETURNS INTEGER
  ( pv-idx as char,
    pv-data as int,
    pv-allownegative as log )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DateSep) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DateSep Procedure 
FUNCTION DateSep RETURNS CHARACTER
    ( pv-lanid as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteAllSysVars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteAllSysVars Procedure 
FUNCTION DeleteAllSysVars RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteSysVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteSysVar Procedure 
FUNCTION DeleteSysVar RETURNS LOGICAL
  ( pv-key As Char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DispConnections) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DispConnections Procedure 
FUNCTION DispConnections RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DispExecMess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DispExecMess Procedure 
FUNCTION DispExecMess RETURNS CHARACTER
  ( pv-mess as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ErrorClear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ErrorClear Procedure 
FUNCTION ErrorClear RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ErrorCreate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ErrorCreate Procedure 
FUNCTION ErrorCreate RETURNS LOGICAL
  ( pv-code as int,
    pv-extra1 as char,
    pv-extra2 as char,
    pv-extra3 as char,
    pv-extra4 as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Execute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Execute Procedure 
FUNCTION Execute RETURNS LOGICAL
  (pv-prog   as char,
   pv-dir    as char,
   pv-params as char,
   pv-mode   as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Fkey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Fkey Procedure 
FUNCTION Fkey RETURNS CHARACTER
  (pv-fromprocedure as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ForceLocal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ForceLocal Procedure 
FUNCTION ForceLocal RETURNS LOGICAL
  ( pv-onoff as log )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FreezeWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FreezeWindow Procedure 
FUNCTION FreezeWindow RETURNS LOGICAL
  (   pv-window as handle,
      pv-onoff  as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetApiDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetApiDetail Procedure 
FUNCTION GetApiDetail RETURNS LOGICAL
  (pv-apiname as char,
   output pv-properties as char,
   output pv-values as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAppserverHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetAppserverHandle Procedure 
FUNCTION GetAppserverHandle RETURNS HANDLE
  ( pv-name as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAppserverNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetAppserverNames Procedure 
FUNCTION GetAppserverNames RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetBlobCtrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetBlobCtrl Procedure 
FUNCTION GetBlobCtrl RETURNS MEMPTR
  ( pv-idx as char,
    output lv-file as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetBlobCtrlRename) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetBlobCtrlRename Procedure 
FUNCTION GetBlobCtrlRename RETURNS MEMPTR (
   input pv-idx as char,
   input-output pv-file as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetClientVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetClientVar Procedure 
FUNCTION GetClientVar RETURNS CHARACTER
  ( pv-varname as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetClientVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetClientVersion Procedure 
FUNCTION GetClientVersion RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetColour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetColour Procedure 
FUNCTION GetColour RETURNS INTEGER
    (ip-colour-name     as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetColumnHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetColumnHandle Procedure 
FUNCTION GetColumnHandle RETURNS HANDLE
  ( pv-browse as handle,
    pv-collabel as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetCtrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetCtrl Procedure 
FUNCTION GetCtrl RETURNS CHARACTER
  ( pv-idx as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetCurrency) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetCurrency Procedure 
FUNCTION GetCurrency RETURNS CHARACTER
    (ip-country     as int) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetField Procedure 
FUNCTION GetField RETURNS CHARACTER
  (pv-table as char,
   pv-keyfield as char,
   pv-keydata  as char,
   pv-datafield as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFieldWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldWhere Procedure 
FUNCTION GetFieldWhere RETURNS CHARACTER
  (pv-table as char,
   pv-where as char,
   pv-datafield as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFkeyProperty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFkeyProperty Procedure 
FUNCTION GetFkeyProperty RETURNS CHARACTER
  ( pv-mode as char,
    pv-key as char,
    pv-pgm as char ,
    pv-user as char ,
    pv-property as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLogFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetLogFileName Procedure 
FUNCTION GetLogFileName RETURNS CHARACTER
  ( pv-prog as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLookupInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetLookupInfo Procedure 
FUNCTION GetLookupInfo RETURNS CHARACTER
  ( pv-focus as handle,
    pv-mode as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetOsFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetOsFile Procedure 
FUNCTION GetOsFile RETURNS CHARACTER
  ( pv-filename as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetParentHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetParentHandle Procedure 
FUNCTION GetParentHandle RETURNS HANDLE
  ( pv-child as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetProcHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetProcHandle Procedure 
FUNCTION GetProcHandle RETURNS HANDLE
  (pv-appsrv as char,
   pv-proc as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetProperty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetProperty Procedure 
FUNCTION GetProperty RETURNS CHARACTER
  (pv-type as char,
   pv-parent as char,
   pv-property as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetRegEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetRegEntry Procedure 
FUNCTION GetRegEntry returns char
 (pv-hkey as char,
  pv-base as char,
  pv-section as char,
  pv-item as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetScratchName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetScratchName Procedure 
FUNCTION GetScratchName RETURNS CHARACTER
  ( pv-extension as char,
    pv-fullpath as log)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSysVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetSysVar Procedure 
FUNCTION GetSysVar RETURNS CHARACTER
  ( pv-key As Char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUserid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetUserid Procedure 
FUNCTION GetUserid RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetWidgetProperty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetWidgetProperty Procedure 
FUNCTION GetWidgetProperty RETURNS CHARACTER
  ( pv-pgm as char,
    pv-frame as char,
    pv-widname as char,
    pv-property as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HadErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HadErrors Procedure 
FUNCTION HadErrors RETURNS LOGICAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitLibraries) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitLibraries Procedure 
FUNCTION InitLibraries RETURNS LOGICAL
  ( pv-user as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadFieldDefaults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadFieldDefaults Procedure 
FUNCTION LoadFieldDefaults RETURNS LOGICAL
  (pv-prog as handle,
   pv-frame as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LogAction Procedure 
FUNCTION LogAction RETURNS LOGICAL
  ( pv-prog as char ,
    pv-action as char,
    pv-msg as  char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogAppserver) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LogAppserver Procedure 
FUNCTION LogAppserver RETURNS LOGICAL
  (pv-msg as  char,
   pv-logfile as char,
   pv-override as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LogMessage Procedure 
FUNCTION LogMessage RETURNS LOGICAL
  (pv-msg as  char,
   pv-logfile as char,
   pv-override as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MakeButton) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MakeButton Procedure 
FUNCTION MakeButton RETURNS HANDLE
  ( pv-parentproc as handle,
    pv-name      as char,
    pv-frame     as handle,
    pv-sensitive as log,
    pv-flat      as log,
    pv-label     as char,
    pv-width     as dec,
    pv-height    as dec,
    pv-row       as dec,
    pv-col       as dec,
    pv-help      as char,
    pv-visible   as log,
    pv-icon      as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MakeLookupButtons) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MakeLookupButtons Procedure 
FUNCTION MakeLookupButtons RETURNS LOGICAL
  (pv-proc as handle,
   pv-frame as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MaxDataGuess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MaxDataGuess Procedure 
FUNCTION MaxDataGuess RETURNS INTEGER
  ( pv-pgm as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MenuLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MenuLabel Procedure 
FUNCTION MenuLabel RETURNS LOGICAL
  (pv-hand as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MenuMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MenuMsg Procedure 
FUNCTION MenuMsg RETURNS HANDLE
  ( pv-text as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MenuOptionFrom) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MenuOptionFrom Procedure 
FUNCTION MenuOptionFrom RETURNS CHARACTER
  ( pv-prog as handle,
    pv-mode as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Msg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Msg Procedure 
FUNCTION Msg RETURNS CHARACTER
    (pv-msgid as int,
     pv-extra1 as char,
     pv-extra2 as char,
     pv-extra3 as char,
     pv-extra4 as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmAuthor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PgmAuthor Procedure 
FUNCTION PgmAuthor RETURNS CHARACTER
    ( pv-pgm as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmComments) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PgmComments Procedure 
FUNCTION PgmComments RETURNS CHARACTER
  ( pv-pgm as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PgmId Procedure 
FUNCTION PgmId RETURNS DECIMAL
  ( pv-pgm as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmMenuGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PgmMenuGroup Procedure 
FUNCTION PgmMenuGroup RETURNS CHARACTER
    ( pv-pgm as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmMenuParent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PgmMenuParent Procedure 
FUNCTION PgmMenuParent RETURNS CHARACTER
  ( pv-prog as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmMultiinstance) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PgmMultiinstance Procedure 
FUNCTION PgmMultiinstance RETURNS LOGICAL
    ( pv-pgm as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PgmName Procedure 
FUNCTION PgmName RETURNS CHARACTER
  ( pv-pgmid as dec)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmProperty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PgmProperty Procedure 
FUNCTION PgmProperty RETURNS CHARACTER
    ( pv-pgm as char,
      pv-property as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmRepInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PgmRepInfo Procedure 
FUNCTION PgmRepInfo RETURNS CHARACTER
    ( pv-pgm as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmUseDefaults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PgmUseDefaults Procedure 
FUNCTION PgmUseDefaults RETURNS LOGICAL
    ( pv-pgm as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProgramDescription) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ProgramDescription Procedure 
FUNCTION ProgramDescription RETURNS CHARACTER
  ( pv-pgm as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProgramTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ProgramTitle Procedure 
FUNCTION ProgramTitle RETURNS CHARACTER
  ( pv-pgm as char,
    pv-mode as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RefreshTempTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RefreshTempTables Procedure 
FUNCTION RefreshTempTables RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReLabel Procedure 
FUNCTION ReLabel RETURNS LOGICAL
  ( pv-handle as handle,
    pv-mode as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RgbColour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RgbColour Procedure 
FUNCTION RgbColour RETURNS CHARACTER
  ( pv-name AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RunRemote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RunRemote Procedure 
FUNCTION RunRemote RETURNS LOGICAL
  ( pv-params as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ScreenChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ScreenChanged Procedure 
FUNCTION ScreenChanged RETURNS LOGICAL
      ( pv-frame as handle) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SecurityCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SecurityCheck Procedure 
FUNCTION SecurityCheck RETURNS LOGICAL
  (pv-user as char,
   pv-group as char,
   pv-notusers as char,
   pv-notgroups as char,
   pv-okusers as char,
   pv-okgroups as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ServerMessageCreate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ServerMessageCreate Procedure 
FUNCTION ServerMessageCreate RETURNS LOGICAL
  ( pv-code as int,
    pv-extra1 as char,
    pv-extra2 as char,
    pv-extra3 as char,
    pv-extra4 as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ServerMessagesClear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ServerMessagesClear Procedure 
FUNCTION ServerMessagesClear RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SessionId Procedure 
FUNCTION SessionId RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetBgColour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetBgColour Procedure 
FUNCTION SetBgColour RETURNS LOGICAL
  ( pv-frame as handle,
    pv-wl as char ,
    pv-colour as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetClientVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetClientVar Procedure 
FUNCTION SetClientVar RETURNS CHARACTER
  ( pv-varname as char,
    pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetCtrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetCtrl Procedure 
FUNCTION SetCtrl RETURNS LOGICAL
  ( pv-idx as char,
    pv-data as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetExecMessHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetExecMessHandle Procedure 
FUNCTION SetExecMessHandle RETURNS CHARACTER
  ( pv-hand as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSensitive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSensitive Procedure 
FUNCTION SetSensitive RETURNS LOGICAL
  ( pv-enable       as log ,
    pv-ix           as char,
    pv-exc-list     as char,
    pv-frame-handle as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSessionLangFormats) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSessionLangFormats Procedure 
FUNCTION SetSessionLangFormats RETURNS LOGICAL
  ( pv-lanid as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSystem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSystem Procedure 
FUNCTION SetSystem RETURNS LOGICAL
  ( pv-system as char  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSysVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSysVar Procedure 
FUNCTION SetSysVar RETURNS LOGICAL
  ( pv-key As Char,
    pv-value As Char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetTableAudit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetTableAudit Procedure 
FUNCTION SetTableAudit RETURNS CHARACTER
  ( pv-tablelist as char,
    pv-state as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetUsrid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetUsrid Procedure 
FUNCTION SetUsrid RETURNS LOGICAL
  ( pv-user as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetUsrPwd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetUsrPwd Procedure 
FUNCTION SetUsrPwd RETURNS LOGICAL
  ( pv-usr As char,
    pv-pwd As char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetWinPosition) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetWinPosition Procedure 
FUNCTION SetWinPosition RETURNS LOGICAL
  ( pv-win-handle as handle,
    pv-pgm        as char,
    pv-xadjust as int,
    pv-yadjust as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetWorkingDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetWorkingDir Procedure 
FUNCTION SetWorkingDir RETURNS LOGICAL
  ( pv-dir as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Sound) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Sound Procedure 
FUNCTION Sound RETURNS LOGICAL
  ( pv-file as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SpellCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SpellCheck Procedure 
FUNCTION SpellCheck RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SysMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SysMsg Procedure 
FUNCTION SysMsg RETURNS LOGICAL
  ( pv-msg as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SystemManager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SystemManager Procedure 
FUNCTION SystemManager RETURNS LOGICAL
    ( pv-user as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TabLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabLabel Procedure 
FUNCTION TabLabel RETURNS LOGICAL
  ( pv-chand as com-handle,
    pv-hand as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Tooltip) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Tooltip Procedure 
FUNCTION Tooltip RETURNS LOGICAL
  ( pv-frame as handle,
    pv-changefont as log )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserAutoTimeclock) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UserAutoTimeclock Procedure 
FUNCTION UserAutoTimeclock RETURNS LOGICAL
    ( pv-user as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserCountry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UserCountry Procedure 
FUNCTION UserCountry RETURNS CHARACTER
    ( pv-user as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UserGroup Procedure 
FUNCTION UserGroup RETURNS CHARACTER
    ( pv-user as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserLanguage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UserLanguage Procedure 
FUNCTION UserLanguage RETURNS INTEGER
    ( pv-user as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UserName Procedure 
FUNCTION UserName RETURNS CHARACTER
    ( pv-user as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidApi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidApi Procedure 
FUNCTION ValidApi RETURNS LOGICAL
  ( pv-name as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidClassCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidClassCode Procedure 
FUNCTION ValidClassCode RETURNS LOGICAL
  ( pv-class as char,
    pv-code  as char, 
    OUTPUT lv-desc AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidDate Procedure 
FUNCTION ValidDate RETURNS LOGICAL
    ( pv-date As Char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidScreenValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidScreenValue Procedure 
FUNCTION ValidScreenValue RETURNS LOGICAL
  (Output pv-extras As Char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidUser Procedure 
FUNCTION ValidUser RETURNS LOGICAL
  ( lv-user as char ,
    lv-pass as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Whelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Whelp Procedure 
FUNCTION Whelp RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidgetInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WidgetInfo Procedure 
FUNCTION WidgetInfo RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidSecCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WidSecCheck Procedure 
FUNCTION WidSecCheck RETURNS LOGICAL
  (pv-fhand as handle,pv-progname as char)  FORWARD.

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
         HEIGHT             = 33.62
         WIDTH              = 53.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
lv-uniqueid = if opsys = 'win32' then GetUniqueId()
                                 else string(today) + string(time).
/* either entry 4 in -param or availablesystems/systemname in ini file */
lv-system = GetSystemName(). /* from ini file */
run GetAppservers(lv-system).  

if return-value ne '' and return-value ne ? and
   not lv-useappserver then do:
    message return-value.
    quit.
end.

if session:remote then refreshsystemptables(). 

{{&core}libmain.i}

/* if opsys ne 'unix' then run setsysfonts. */

if session:remote then refreshtemptables().

.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BldMenuBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BldMenuBar Procedure 
PROCEDURE BldMenuBar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM PV-WINdowHANDLE AS HANDLE   NO-UNDO.
    DEF INPUT PARAM PV-FRAMEHANDLE  AS HANDLE   NO-UNDO.
    DEF INPUT PARAM PV-GROUP        AS CHAR     NO-UNDO.
    def input param pv-called-from  as  handle  no-undo.

    def var h-menu        as handle        no-undo. 
    def var h-sub-menu    as handle        no-undo.
    def var h-menu-item   as handle        no-undo.
    def var lv-can-menu   as log init true no-undo.
    def var lv-mes-handle as handle        no-undo.
    def var lv-gotsome as log no-undo.
    def buffer bmenu for t-zen-dmenu.
    def buffer cmenu for t-zen-dmenu.  
{{&core}bug.i}        
    if not can-find(first t-zen-dmenu) 
        then return.

    run {&core}mnu-mes.w persistent set lv-mes-handle.       
    
    create menu h-menu.
    h-menu:private-data = '0'.
  
    /* create first level menus ie. File Edit Help */
    for each t-zen-dmenu where t-zen-dmenu.menu-parent = '' 
                           and can-do(t-zen-dmenu.menu-grp,pv-group) 
                         no-lock
                         by menu-parent
                         by display-order:
        lv-gotsome = true.
        lv-can-menu = SecurityCheck(lv-user,
                                 lv-group,  
                                 t-zen-dmenu.not-users,
                                 t-zen-dmenu.not-group,
                                 t-zen-dmenu.run-users,
                                 t-zen-dmenu.run-groups).
/*    */
/*             if can-do(t-zen-dmenu.not-users,lv-user) or */
/*                can-do(t-zen-dmenu.not-group,lv-group) */
/*                 then lv-can-menu = false. */
/*             else */
/*             if not can-do(t-zen-dmenu.run-users,lv-user) or */
/*               not can-do(t-zen-dmenu.run-groups,lv-group) */
/*                 then lv-can-menu = false. */
            
/*             IF CAN-DO(t-zen-dmenu.hide-in-countries,lv-country) THEN */
/*             lv-can-menu = FALSE.                                     */
/*                                                                      */

        if not lv-can-menu and
           not lv-grey then next.

       CREATE SUB-MENU h-sub-menu
       ASSIGN PARENT = h-menu
               LABEL = menu-name.
       if not lv-can-menu and
           lv-grey then h-sub-menu:sensitive = false.
        h-menu:private-data = string(int(h-menu:private-data) + 3 + length(h-sub-menu:label)). 
        run buildmenu (input t-zen-dmenu.menu-id, h-menu, h-sub-menu,pv-called-from).
    end.  
    if lv-gotsome then do:
    assign
        pv-windowhandle:MENUBAR = h-menu.
    end.
    else delete object h-menu.
    
    if valid-handle(lv-mes-handle) then do:
        apply 'close' to lv-mes-handle.
/*         assign                                                                                            */
/*             pv-windowhandle:width-chars = int(max(int(h-menu:private-data),pv-windowhandle:width-chars))  */
/*             pv-framehandle:width-chars  = max(pv-windowhandle:width-chars,pv-framehandle:width-chars).    */
    end.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildMenu Procedure 
PROCEDURE BuildMenu :
def var h-menu-item as handle        no-undo.
def var lv-can-menu as log init true no-undo. 
def buffer amenu for t-zen-dmenu.
def buffer bmenu for t-zen-dmenu.
def buffer cmenu for t-zen-dmenu. 

def input param v-id           like t-zen-dmenu.menu-id no-undo.
def input param h-menu         as handle              no-undo.
def input param h-sub-menu     as handle              no-undo.
def input param pv-called-from as handle              no-undo.
{{&core}bug.i}
find amenu where amenu.menu-id = v-id no-lock no-error.


/* each menu-item of parent menu */
for each bmenu where bmenu.menu-parent = amenu.menu-action 
               /*  And can-do(bmenu.menu-grp,lv-group) */
                no-lock
                  by menu-parent
                  by display-order:
    /* 04/18/06 AMD: added following line - to prevent endless recursion when
       bad menu records exist */
    if bmenu.menu-action = "" and menu-name ne "<rule>" then next.

    /* menu user security */
    lv-can-menu = SecurityCheck(lv-user,
                                lv-group,  
                                bmenu.not-users,
                                bmenu.not-group,
                                bmenu.run-users,
                                bmenu.run-groups).
       
    IF CAN-DO(bmenu.hide-in-countries,lv-country) 
    THEN lv-can-menu = FALSE.

    if not lv-can-menu and
       not lv-grey then next.

    /* check its not a sub menu itself */
    if not can-find(first cmenu where cmenu.menu-parent = bmenu.menu-action)
        or bmenu.menu-name begins '<' 
    then do:
      CREATE MENU-ITEM h-menu-item
        ASSIGN PARENT = h-sub-menu
               LABEL  = bmenu.menu-name 
               private-data = string(bmenu.menu-id) + '{&Delim2}' + lv-system
               subtype = if bmenu.menu-name = '<Rule>' then 'RULE'
                           else if bmenu.menu-name = '<Skip>' then 'SKIP'
                                 else 'normal'
        TRIGGERS:
           ON CHOOSE                
               persistent run menudoit in this-procedure (input pv-called-from).
           END TRIGGERS.

          if not lv-can-menu and
                 lv-grey then h-menu-item:sensitive = false.
    end. 
    else do: /* do it again recursively */   
        CREATE SUB-MENU h-menu-item
        ASSIGN PARENT = h-sub-menu
               LABEL  = bmenu.menu-name. 
          if not lv-can-menu and
                 lv-grey then do:
                h-menu-item:sensitive = false.
                next.
          end.
        run buildmenu (input bmenu.menu-id, h-sub-menu, h-menu-item, pv-called-from).
    end. 
end.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateAppserver) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateAppserver Procedure 
PROCEDURE CreateAppserver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param pv-connectstring as char   no-undo.
def output param h-appserver     as handle no-undo.
{{&core}bug.i}

if pv-connectstring ne '' and
   Not FileNotFound(pv-connectstring)
then do:
    CREATE SERVER h-appserver.
    IF valid-handle(h-appserver) 
    THEN DO:  
        &if defined(MaximiseLicenses) ne 0 &then
            h-appserver:CONNECT("-pf " + pv-connectstring,lv-user,lv-password,lv-sessionid) No-error.
        &else
            h-appserver:CONNECT("-pf " + pv-connectstring,lv-user,lv-password,"") No-error.
        &endif
        If (Error-status:error AND ERROR-STATUS:NUM-MESSAGES > 0) or not h-appserver:CONNECTed() Then Do:
/*             message msg(3,'1 Connect','Appserver',pv-connectstring,Error-Status:Get-Message(Error-Status:Num-Messages)) */
            h-appserver = ?.
            return '1 Connect Failed for Appserver ' + string(pv-connectstring) + ' ' +
                    Error-Status:Get-Message(Error-Status:Num-Messages).           
        End.
    End.
    ELSE do:
/*         message msg(3,'2 Create','Appserver',pv-connectstring,Error-Status:Get-Message(Error-Status:Num-Messages))  */
/*         view-as alert-box error.                                                                                  */
        h-appserver = ?.
        return '2 Create Failed for Appserver ' + string(pv-connectstring) + ' ' + 
                    Error-Status:Get-Message(Error-Status:Num-Messages).           
    end.
END.
else do:
/*     message msg(50,'3 param File',pv-connectstring,'','') */
/*     view-as alert-box error.                                */
    h-appserver = ?.
    return '3 param file ' + string(pv-connectstring) + ' not Found'.
end.
return ''.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAppservers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAppservers Procedure 
PROCEDURE GetAppservers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param pv-system as char   no-undo. 
/* set by login prog and used for appsrv connection 
purely to get session context for this session.*/
def var x          as int  no-undo.
/* only neede as initial check that they are up 
    and to set context for each client */
def var lv-names    as char no-undo.
def var lv-params   as char no-undo.

{{&core}bug.i program-name(2)}

for each w-appsrv:
    delete w-appsrv.
End.
lv-sessionid = string(today,'99/99/9999') + ':' + lv-uniqueid.
IF lv-useappserver 
THEN DO:
    lv-names  = GetIniValue(pv-system,'Appservers').
    lv-params = GetIniValue(pv-system,'Paramfiles').

    if num-entries(lv-names,'{&Delim2}') < 1 or
       num-entries(lv-params,'{&Delim2}') < 1 
    then do:
        lv-UseAppserver = false.
        return ' Appserver not defined in ini '.
    End.
    do x = 1 to num-entries(lv-names,'{&Delim2}'):
        CREATE W-APPSRV.
        assign w-name  = entry(x,lv-names,'{&Delim2}')
               w-param = entry(x,lv-params,'{&Delim2}').
        run CreateAppServer in this-procedure (w-param,output w-handle).
        if w-handle = ? or return-value ne '' then do:
            message return-value {&dbt}.
            w-sessionid = lv-sessionid.
            lv-UseAppserver = false.
            return ' Appserver Not Found ' +
                             string(w-name) + ' ' + 
                             string(w-param).   
        end.

        &if defined(MaximiseLicenses) ne 0 &then
            w-sessionid = lv-sessionid.
            w-handle:disconnect().  
        &else w-sessionid = w-handle:client-connection-id.
        &endif
    end.
    lv-UseAppserver = true.
end.
else if not session:remote
   then lv-params = ' '.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMessage Procedure 
PROCEDURE GetMessage :
def input param pv-mesnum  as int  no-undo.
def input param pv-extra1  as char no-undo.
def input param pv-extra2  as char no-undo.
def input param pv-extra3  as char no-undo.
def input param pv-extra4  as char no-undo.
def output param pv-mestxt as char no-undo.
def var lv-user as char no-undo.
{{&core}bug.i}
lv-user = GetSysVar("user").
     
   pv-mestxt = GetField('zen-mesfil','mesnum',string(pv-mesnum),'mestxt').
   if pv-mestxt = ''
   then do:
       pv-mestxt =  "MESSAGE No: " + string(pv-mesnum) + " not found".
       return.
   end.
   /*ak added code to stop progress making the whole message NULL 
        eg. ? if a '?' value is part of the message */
   IF pv-extra1 = ? THEN pv-extra1 = altlanguage('"Unknown"').
   IF pv-extra2 = ? THEN pv-extra2 = altlanguage('"Unknown"').
   IF pv-extra3 = ? THEN pv-extra3 = altlanguage('"Unknown"').
   IF pv-extra4 = ? THEN pv-extra4 = altlanguage('"Unknown"').

   if not lv-user begins '**' then do:
      if UserLanguage(lv-user) ne 0  /* in zenlibrary.p */
      then do:
       if pv-extra4 ne '' then pv-extra4 = AltLanguage(pv-extra4).
       if pv-extra3 ne '' then pv-extra3 = AltLanguage(pv-extra3).
       if pv-extra2 ne '' then pv-extra2 = AltLanguage(pv-extra2).             
       if pv-extra1 ne '' then pv-extra1 = AltLanguage(pv-extra1).
      end.
   end.
   
   assign pv-mestxt = replace(pv-mestxt,'#4',pv-extra4)
          pv-mestxt = replace(pv-mestxt,'#3',pv-extra3)
          pv-mestxt = replace(pv-mestxt,'#2',pv-extra2)
          pv-mestxt = replace(pv-mestxt,'#1',pv-extra1)
          pv-mestxt = string('(' + string(pv-mesnum) + ') ' + pv-mestxt).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initialise-libraries) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise-libraries Procedure 
PROCEDURE initialise-libraries :
def var lv-hand as handle no-undo.
lv-hand = session:first-procedure.
{{&core}bug.i}
do while valid-handle(lv-hand):
    if lv-hand:private-data begins "library-"
        then run initialise in lv-hand (lv-user) no-error.
    lv-hand = lv-hand:next-sibling.
end.


RefreshTempTables(). 

run set-back-color in THIS-PROCEDURE ('on').
officesetsuite(GetCtrl("officeSuite")).

if not getctrl("{&sonicbroker}") begins 'none' then do:
    SetMessageConnections(GetSysVar('System'),    
                          GetSysVar('UserGroup'), 
                          GetSysVar('User')).     
end.

if not session:remote then do:
    run {&core}reg-ocx.p.
end.

/* lv-user = GetSysVar("user"). */
/* setsysvar("initialised","true"). */
lv-scrfont = getctrl('{&screenfontnumber}').
lv-activitylog = getctrl('{&ActivityLog}').
lv-logmessages = getctrl('{&LogMessages}').
lv-logappserver = getctrl('{&LogAppserver}').
lv-SecurityAlgorithm = int(Getctrl('SecurityAlgorithm')).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MenuDoIt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuDoIt Procedure 
PROCEDURE MenuDoIt :
def input param pv-called-from as handle no-undo.
def var lv-proghandle as handle no-undo.
def var lv-rcode as char no-undo.
current-window = self:window.
{{&core}bug.i}
    find t-zen-dmenu where t-zen-dmenu.menu-id = int(entry(1,self:private-data,'{&Delim2}'))
                     no-lock no-error.
/*     t-zen-dmenu.menu-action   = t-zen-dmenu.menu-action.  */

lv-rcode = substring(t-zen-dmenu.menu-action,1,r-index(t-zen-dmenu.menu-action,'.')) + 'r'.
  if session:first-server ne ? and 
     search(lv-rcode) = ?  
  then do:
    message msg(50,t-zen-dmenu.menu-action,'No R-Code Found','','')
           view-as alert-box error.
    return.
  end.
  
 /*  /* now check callrec for any messages */
/*    bit of a bodge really need sonic */ */
/*   CheckforMessages(lv-user,yes). /* in rexlibrary.p */ */
  
  if search(t-zen-dmenu.menu-action) ne ? or
     search(lv-rcode) ne ? 
  then do:
        lv-proghandle = runchild(t-zen-dmenu.menu-action,pv-called-from).
/*     if t-zen-dmenu.menu-pers then                                                                  */
/*            if t-zen-dmenu.menu-input = ''                                                          */
/*                 then run value(t-zen-dmenu.menu-action) persistent.                                */
/*                 else run value(t-zen-dmenu.menu-action) persistent (input t-zen-dmenu.menu-input). */
/*     else                                                                                           */
      if t-zen-dmenu.menu-input ne ''
           then run refresh in lv-proghandle (t-zen-dmenu.menu-input).
  end.
  else 
      if can-do(pv-called-from:internal-entries,t-zen-dmenu.menu-action) then
      do:
        if t-zen-dmenu.menu-input = ''
            then run value(t-zen-dmenu.menu-action) in pv-called-from. 
            else run value(t-zen-dmenu.menu-action) in pv-called-from (input t-zen-dmenu.menu-input) .
      end.        
      else message msg(50,t-zen-dmenu.menu-action,'Invalid Action','','')
           view-as alert-box error.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PopulateTempTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateTempTables Procedure 
PROCEDURE PopulateTempTables :
{{&core}bug.i}
if lv-user = '' then return.

if not session:remote then 
    sysmsg("Creating Core Cache Please Wait").


empty temp-table t-zen-lan_field.
empty temp-table t-zen-dpgm.
empty temp-table t-zen-fielddefault.
empty temp-table t-zen-dwidget.
empty temp-table t-zen-control.
empty temp-table t-zen-duser.
empty temp-table t-zen-mesfil.
empty temp-table t-zen-fldlook.
empty temp-table t-zen-widlook.
empty temp-table t-zen-dmenu.
empty temp-table t-zen-colours.
empty temp-table t-zen-f-hotkey.
empty temp-table t-zen-dfkey.
empty temp-table t-zen-widgetproperty.
empty temp-table t-zen-dpgm.
empty temp-table t-zen-auditdetail.

        {{&core}run.i &program   = "buildtables.p"
                     &path      = "{&core}{&srv}"
                     &Appsrv    = "System"
                     &direct    = "true"
                     &nomess    = 'true'
                     &noper     = 'true'
                     &procedure = "BuildTables"
                     &params    = "(lv-user,
                                    output table t-zen-lan_field,
                                    output table t-zen-dpgm,
                                    output table t-zen-dwidget,
                                    output table t-zen-control,
                                    output table t-zen-duser,
                                    output table t-zen-dmenu,
                                    output table t-zen-widlook,
                                    output table t-zen-fldlook,
                                    output table t-zen-colours,
                                    output table t-zen-f-hotkey,
                                    output table t-zen-dfkey,
                                    output table t-zen-fielddefault,
                                    output table t-zen-widgetproperty,
                                    output table t-zen-property, 
                                    output table t-zen-mesfil,
                                    output table t-zen-auditdetail)"}
   RefreshSysTempTables().
 /*   def var h-buf as handle no-undo. */
/*    create buffer h-buf for table 't-zen-dpgm'. */
/*   message numrecords('dpgm',h-buf) {&dbt}. */
lv-lookupcolour = GetColour("{&LookupHighlight}").
lv-uselookupbuttons = Getctrl("{&uselookupbuttons}") = 'yes'.
if opsys ne 'unix' then run setregistrycolors.
lv-group = usergroup(lv-user).

lv-cachedone = true.
if not session:remote then sysmsg("off").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-AltLanguage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-AltLanguage Procedure 
PROCEDURE Proc-AltLanguage :
def input-output param pv-string as char no-undo.
{{&core}bug.i} 
FIND FIRST t-zen-lan_field WHERE t-zen-lan_field._field-name = pv-string
                           NO-LOCK NO-ERROR.    

IF AVAIL t-zen-lan_field 
    THEN pv-string =  t-zen-lan_field.lan_altlabel.
    else pv-string = pv-string.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-AnyErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-AnyErrors Procedure 
PROCEDURE Proc-AnyErrors :
def input  param  pv-type as char no-undo.
def output param lv-ok    as log no-undo init no.
  def var lv-hand as handle no-undo.
  if return-value ne 'error' then return.
  
 {{&core}bug.i} 
  for each t-error:
      delete t-error.
  end.
  
  {{&core}run.i &program   = "zen-context.p"       
               &path      = "{&core}{&srv}"
               &Appsrv    = "System"   
               &direct    = True
               &nomess    = 'true'
               &noper     = true
               &procedure = "GetErrors"             
               &params    = "(pv-type,output table t-error)"} 
          
  find first t-error no-lock no-error.
  if not avail t-error then lv-ok = false.
  else do:
      if not session:remote then do:
        lv-hand = getprochandle('local','{&core}err-display.w').
        if not valid-handle(lv-hand) 
         then run {&core}err-display.w persist set lv-hand.
        run display-errors in lv-hand (table t-error).   
      end.
      lv-ok = true.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-AttachMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-AttachMenu Procedure 
PROCEDURE Proc-AttachMenu :
DEF INPUT PARAM PV-WINDOW    AS HANDLE NO-UNDO.
DEF INPUT PARAM PV-frame     AS HANDLE NO-UNDO.
DEF INPUT PARAM PV-procedure AS HANDLE NO-UNDO.
{{&core}bug.i}
def var lv-mok as log  no-undo.
def var lv-grp as char no-undo.
assign
    lv-grey    = getctrl('{&menu-grey}') = 'yes'
    lv-grp = PgmMenuGroup(pv-procedure:file-name).
/* {{&core}run.i &program   = "zen-dmenu.p" */
/*              &path      = "{&core}{&srv}" */
/*              &Appsrv    = "System" */
/*              &noper     = true */
/*              &procedure = "checkmenu" */
/*              &params    = "(unixpath(pv-procedure:file-name), */
/*                             OUTPUT lv-mok)"} */
/* if lv-mok then do: */
    RUN bldmenubar (pv-window,
                    pv-frame,
                    lv-grp,
                    pv-procedure).
                    
    if pv-window:type = 'window' and
        can-query(pv-window,'menu-bar')
    then do:
        if valid-handle(pv-window:menu-bar)
        then do:
            if int(GetSysVar("{&clv}language")) ne 0
            then MenuLabel(pv-window).
        end.
    end.
/* end. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-ClassCodeDesc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-ClassCodeDesc Procedure 
PROCEDURE Proc-ClassCodeDesc :
def input param  pv-class as char no-undo.
def input param  pv-code  as char no-undo.
def output param lv-desc  as char no-undo.
{{&core}bug.i} 
    {{&core}run.i &program   = "zen-ClassCode.p"
                 &path      = "{&core}{&srv}"
                 &Appsrv    = "System"  
                 &NoPer     = True
                 &procedure = "GetClassCodeDesc"
                 &params    = "(pv-class,
                                pv-code,
                                output lv-desc)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-ClassCodes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-ClassCodes Procedure 
PROCEDURE Proc-ClassCodes :
def input param pv-class as char no-undo.
def output param pv-desc as char no-undo.
def output param lv-keys as char no-undo.
   {{&core}bug.i}                                      
    {{&core}run.i &program   = "zen-ClassCode.p"
                 &path      = "{&core}{&srv}"
                 &Appsrv    = "System"  
                 &Noper     = True
                 &procedure = "Get-Class-Codes"
                 &params    = "(pv-class,
                                output pv-desc,
                                output lv-keys)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-CleanSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-CleanSession Procedure 
PROCEDURE Proc-CleanSession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-session as char no-undo.
def var lv-f       as char no-undo.
def var lv-wordtemplates as char no-undo.
def var lv-scratch as char no-undo.
{{&core}bug.i} 
assign
   lv-session = replace(sessionid(),'/','-')
   lv-session = replace(lv-session,':','')
   lv-session = replace(lv-session,' ','-')
   lv-wordtemplates = getctrl('{&WordTemplatePath}')
   lv-scratch = getctrl('{&ScratchPath}').
   
lv-f = lv-wordtemplates + lv-session + '*.*'.
os-command silent del/f value(lv-f).
lv-f = lv-wordtemplates + lv-user + "-template.doc".
os-command silent del/f value(lv-f).

lv-f = lv-scratch + 'docscratch*.*'.
os-command silent del/f value(lv-f).
lv-f = lv-scratch + 'docdata*.*'.
os-command silent del/f value(lv-f).

lv-f = lv-scratch + lv-user + '*temp.*'. 
os-command silent del/f value(lv-f). 

lv-f = lv-scratch + lv-user + '*.rep'.
os-command silent del/f value(lv-f).

/* tidy up context table on server */
{{&core}run.i &program   = "zen-context.p"
               &path      = "{&core}{&srv}"
               &Appsrv    = "System"
               &procedure = "DeleteAll"
               &params    = "~{~}"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-ClearAppserver) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-ClearAppserver Procedure 
PROCEDURE Proc-ClearAppserver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var x as int no-undo.   
{{&core}bug.i} 

for each w-appsrv:
    {{&core}run.i &program   = "clearall.p"
                 &path      = "{&sys}{&srv}"
                 &Appsrvc    = w-name
                 &procedure = "clearallprocs"
                 &params    = "~{~}"}
    message msg(999,w-name,' Cleared','','') view-as alert-box.
End.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-CreateButs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-CreateButs Procedure 
PROCEDURE Proc-CreateButs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param lv-butlist    as char   no-undo.
def input param lv-commonlist as char   no-undo.
{{&core}bug.i "'begin'"} 
def var lv-horizontal as log    no-undo.
def var lv-butrow     as dec    no-undo.
def var lv-butcol     as dec    no-undo.
def var lv-butheight  as dec    no-undo.
def var lv-butwidth   as dec    no-undo.
def var lv-procedure  as handle no-undo.
def var lv-window     as handle no-undo.
def var lv-frame      as handle no-undo.
def var lv-flat       as log    no-undo.
def var lv-butname    as char   no-undo.
def var lv-params     as char   no-undo.
def var lv-sensitive  as log    no-undo.
def var lv-visible    as log    no-undo.
def var lv-overlay    as log    no-undo.
def var lv-recheight  as dec    no-undo.
def var lv-recwidth   as dec    no-undo. 
def var lv-label      as char   no-undo.
def var lv-help       as char   no-undo.
def var lv-icon       as char   no-undo.
def var x             as int    no-undo.
def var h-rect        as handle no-undo.
def var h-but         as handle no-undo.
def var lv-w          as dec    no-undo.
Def Var lv-center     As Log    No-undo.
def var lv-exclist    as char   no-undo init '*'.
def var lv-progmode   as char   no-undo.
def var y as int no-undo.

assign       
    lv-procedure  = widget-handle(entry(1,lv-commonlist,','))
    lv-window     = widget-handle(entry(2,lv-commonlist,','))
    lv-frame      = widget-handle(entry(3,lv-commonlist,','))    
    lv-horizontal = StringToLog(entry(4,lv-commonlist,','))
    lv-flat       = StringToLog(entry(5,lv-commonlist,','))
    lv-butcol     = StringToDec(entry(6,lv-commonlist,','),',','.')
    lv-butrow     = StringToDec(entry(7,lv-commonlist,','),',','.')
    lv-butheight  = StringToDec(entry(8,lv-commonlist,','),',','.')
    lv-butwidth   = StringToDec(entry(9,lv-commonlist,','),',','.')
    lv-center     = StringToLog(entry(10,lv-commonlist,','))
    lv-recwidth   = lv-butwidth + 3
    lv-recheight  = lv-butheight + 4
/*     lv-window:hidden = true  */
    X = 1. 

If lv-center Then Do:
if lv-horizontal then do:     
    do x = 1 to num-entries(lv-butlist):
        lv-params    = entry(2,entry(x,lv-butlist),'^').
        if substring(lv-params,3,1) = 't' then y = y + 1.
    end.
    lv-butcol = (lv-frame:Width-pixels / 2) - (lv-butwidth * (num-entries(lv-butlist) - y) / 2).
End.
else do:
    do x = 1 to num-entries(lv-butlist):
        lv-params    = entry(2,entry(x,lv-butlist),'^').
        if substring(lv-params,3,1) = 't' then y = y + 1.
    end.
    lv-butrow = (lv-frame:Height-pixels / 2) - (lv-butheight * (num-entries(lv-butlist) - y) / 2).
end.
End.

  create rectangle h-rect 
  assign
     name          = "butrect" 
     frame         = lv-frame
     edge-pixels   = 2
     graphic-edge  = true
     Width-pixels  = lv-recwidth 
     Height-pixels = lv-recheight
     X             = max(lv-butcol - 1,1) 
     Y             = max(lv-butrow - 1,1)
     filled        = false
     bgcolor       = lv-frame:bgcolor
     fgcolor       = lv-frame:fgcolor
     visible       = false.

Assign lv-butcol = h-rect:X + 1
       lv-butrow = h-rect:Y + 1.

/* get the exclusion list using pgm security settings */
run Proc-pgmproperties(lv-procedure:private-data,'ExcludeButtons',output lv-exclist).
/* exclude buttons if run time read-only mode */
run SendMode in lv-procedure (output lv-progmode) no-error.

if not error-status:error then do:
   if lv-progmode = 'read-only' then lv-exclist = 
      "!new,!edit,!delete,!undo,!save,!savenew,*," + lv-exclist.
   else if lv-progmode = 'change-only' then lv-exclist =
      "!new,!savenew,*," + lv-exclist.
end.
do x = 1 to num-entries(lv-butlist):
    assign lv-butname   = entry(1,entry(x,lv-butlist),'^')
           lv-params    = entry(2,entry(x,lv-butlist),'^')
           lv-help      = if num-entries(entry(x,lv-butlist),'^') > 2 
                            then entry(3,entry(x,lv-butlist),'^')
                            else lv-butname
           lv-icon      = if num-entries(entry(x,lv-butlist),'^') > 3 
                            then entry(4,entry(x,lv-butlist),'^')
                            else lv-butname
           lv-sensitive = substring(lv-params,1,1) = 't'
           lv-visible   = substring(lv-params,2,1) = 't'
           lv-overlay   = substring(lv-params,3,1) = 't'.
            
    if not can-do(lv-exclist,lv-butname) or
       lv-butname = '' then next.
       
             
    if lv-help = '' then 
    case lv-butname:
        when "new"    then lv-help = "Create New record". 
        when "edit"   then lv-help = "Edit the current record". 
        when "save"   then lv-help = "Save the current record". 
        when "delete" then lv-help = "Delete the current record".                       
        when "undo"   then lv-help = "Undo Changes". 
        when "query"  then lv-help = "Get All". 
        when "audit"  then lv-help = "Audit". 
        when "exit"   then lv-help = "Exit".            
        when "help"   then lv-help = "Help".            
        when "ok"     then lv-help = "Save Changes".
        when "prt"    then lv-help = "Print".
        otherwise lv-help = lv-butname.
    end case.           
                           
    h-but = MakeButton(lv-procedure,lv-butname,lv-frame,lv-sensitive,lv-flat,lv-label,
                       lv-butwidth,lv-butheight,lv-butrow + 1,lv-butcol,lv-help,lv-visible,lv-icon).
                 
    lv-w = max(lv-butwidth,h-but:width-pixels).
    If lv-horizontal 
        Then Assign lv-recwidth  = lv-recwidth  + (if lv-overlay Then 0 Else lv-w)
                    lv-frame:width-pixels   = max(lv-recwidth + h-rect:X,lv-frame:width-pixels)
                    lv-butcol    = lv-butcol    + (if lv-overlay Then 0 Else lv-w).

        Else Assign lv-recheight = lv-recheight + (if lv-overlay Then 0 Else lv-butheight) 
                    lv-frame:height-pixels  = max(lv-recheight + h-rect:Y,lv-frame:height-pixels)
                    lv-butrow    = lv-butrow    + (if lv-overlay Then 0 Else lv-butheight).

end.

   assign     
     h-rect:width-pixels = lv-recwidth - (if lv-Horizontal Then lv-butwidth Else 0)
     h-rect:height-pixels = lv-recheight - (if lv-horizontal Then 0 Else lv-butheight)
     lv-window:height-pixels = max(lv-frame:height-pixels + 1,lv-window:height-pixels)
     lv-window:width-pixels  = max(lv-frame:width-pixels + 1,lv-window:width-pixels)
     h-rect:visible   = true
/*      lv-window:hidden = false  */
.
{{&core}bug.i "'End'"} 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-CtrlCounter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-CtrlCounter Procedure 
PROCEDURE Proc-CtrlCounter :
def input param pv-idx  as char no-undo.
def input-output param pv-data as int no-undo.
def input param pv-allownegative as log no-undo.
{{&core}bug.i} 
  {{&core}run.i &program   = "zen-control.p"
               &path      = "{&core}{&srv}"
               &Appsrv    = "System"  
               &procedure = "IntCounter"
               &params    = "(pv-idx,
                              input-output pv-data,
                              pv-allownegative)"}
        
    if return-value = 'failed' 
        then return 'failed'.
    
find t-zen-control where t-zen-control.ctrl-idx = pv-idx
                   no-error.
if avail t-zen-control 
    then t-zen-control.ctrl-data = string(pv-data).
/* instead of    RefreshTempTables(). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-DateSep) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-DateSep Procedure 
PROCEDURE Proc-DateSep :
def input  param pv-lanid as int  no-undo.
def output param lv-value as char no-undo.
{{&core}bug.i} 
    {{&core}run.i &program   = "zen-language.p"
                 &path      = "{&core}{&srv}"
                 &Appsrv    = "System"  
                 &noper     = true
                 &procedure = "getdatesep"
                 &params    = "(pv-lanid,
                                output lv-value)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-DeleteAllSysVars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-DeleteAllSysVars Procedure 
PROCEDURE Proc-DeleteAllSysVars :
{{&core}bug.i} 
{{&core}run.i &program   = "zen-context.p"       
             &path      = "{&core}{&srv}"
             &Appsrv    = "System"  
             &noper     = true                
             &procedure = "Deleteall"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-DeleteSysVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-DeleteSysVar Procedure 
PROCEDURE Proc-DeleteSysVar :
def input param pv-key as char no-undo.
{{&core}bug.i} 
  {{&core}run.i &program   = "zen-context.p"       
                &path      = "{&core}{&srv}"
                &Appsrv    = "System"  
                &noper     = true                
                &procedure = "Deletevar"             
                &params    = "(INPUT PV-key)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-ErrorClear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-ErrorClear Procedure 
PROCEDURE Proc-ErrorClear :
def input param pv-type as char no-undo.
{{&core}bug.i} 
return.

{{&core}run.i &program   = "zen-context.p"
              &path      = "{&core}{&srv}"
              &Appsrv    = "System"
              &nomess    = 'true'
              &procedure = "clearerrors"
              &params    = "(pv-type)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-ErrorCreate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-ErrorCreate Procedure 
PROCEDURE Proc-ErrorCreate :
def input param pv-code as int no-undo.
def input param pv-type as char no-undo.
  def input param pv-extra1 as  char no-undo.
  def input param pv-extra2 as  char no-undo.
  def input param pv-extra3 as  char no-undo.
  def input param pv-extra4 as  char no-undo.      
{{&core}bug.i} 
  {{&core}run.i &program   = "zen-context.p"
                &path      = "{&core}{&srv}"
                &Appsrv    = "System"
                &nomess    = 'true'
                &procedure = "create-error"
                &params    = "(pv-code,
                               pv-type,
                               pv-extra1,
                               pv-extra2,
                               pv-extra3,
                               pv-extra4)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-Fkey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-Fkey Procedure 
PROCEDURE Proc-Fkey :
def input param v-pgmhandle as handle no-undo.
def var v-pgm       as char   no-undo.
def var pv-action   as char   no-undo.
def var lv-frame as handle no-undo.
def var lv-focus as handle no-undo.
def var lv-curprog as handle no-undo.
def var lv-progmode as char no-undo.
def var lv-exclist as char no-undo.

{{&core}bug.i} 

lv-focus = focus.
lv-curprog = widget-handle(current-window:private-data).
v-pgm = v-pgmhandle:name.

pv-action = GetFkeyProperty('fkey',key-label(lastkey),v-pgm,lv-user,'Action').

run SendMode in v-pgmhandle (output lv-progmode) no-error.
if not error-status:error then do:
   if lv-progmode = 'read-only' 
   then lv-exclist = "new,edit,delete,save,savenew".
   else if lv-progmode = 'change-only' 
        then lv-exclist = "new,savenew".
end.

if can-do(lv-exclist,entry(1,pv-action,'-')) then return.

if not pv-action = ''  then do:
    case entry(1,pv-action,','):
        when 'JumpTo' then do:
            JumpTo(entry(2,pv-action,',')).
            return.
        end.
        when 'Audit' then do:
        end.
        otherwise do:
            if search(pv-action) = ? and 
               search(substring(pv-action,1,length(pv-action) - 1) + 'r') = ? and
               not can-do(v-pgmhandle:internal-entries,pv-action) and
               not can-do(this-procedure:internal-entries,pv-action)
            then do:
                message msg(50,'Program',pv-action,'','') skip
                    v-pgm skip
                    lv-curprog:name
                view-as alert-box error.
                return.
            end.
            if search(pv-action) ne ? or 
              search(substring(pv-action,1,length(pv-action) - 1) + 'r')
              ne ? then runchild(pv-action,v-pgmhandle).
           else do:
                run sendframehandle in v-pgmhandle (output lv-frame) no-error.
                If not can-do(v-pgmhandle:internal-entries,pv-action)
                Then v-pgmhandle = This-procedure.
                run value(pv-action) in v-pgmhandle no-error.
                /*
                if valid-handle(lv-focus) 
                   then apply 'entry' to lv-focus. 
                else if valid-handle(lv-frame) 
                   then setframefocus(lv-frame).
               */

                return.
           end.
        end.
    end case.
end.
else do:
/*     message 'No Action defined for ' key-label(lastkey) */
/*     view-as alert-box error. */
    return.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetApiDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetApiDetail Procedure 
PROCEDURE Proc-GetApiDetail :
def input param  pv-apiname    as char no-undo.
def output param pv-properties as char no-undo.
def output param pv-values     as char no-undo.
{{&core}bug.i} 
{{&core}run.i &program   = "zen-apidetail.p"
             &path      = "{&core}{&srv}"
             &Appsrv    = "System"
             &direct    = true
&nomess    = 'true'
             &noper     = true  
             &procedure = "ApiDetail"                   
             &params    = "(pv-apiname,
                            output pv-properties,
                            output pv-values)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetBlobCtrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetBlobCtrl Procedure 
PROCEDURE Proc-GetBlobCtrl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-idx as char no-undo.

def output param pv-blob as memptr no-undo.
def output param pv-value as char no-undo.
{{&core}bug.i} 
def var lv-read as log no-undo.
/* make sure there is nothing left over in the memory pointer! */
set-size(pv-blob) = 0.
find first t-zen-control where t-zen-control.ctrl-idx = pv-idx
                         no-lock no-error.

if not avail t-zen-control 
then lv-read = true.
else lv-read = t-Zen-Control.ReRead.

if lv-read then do:
  {{&core}run.i &program   = "zen-control.p"
                &path      = "{&core}{&srv}"
                &Appsrv    = "System"  
                &nomess    = 'true'
                &procedure = "getblobcontrol"
                &params    = "(pv-idx,
                               output pv-blob,
                               output pv-value)"}
end.
else do:
    if avail t-zen-control and t-zen-control.ctrl-data = 'blob'
    then assign
            pv-blob  = t-zen-control.ctrl-blob
            pv-value = t-zen-control.Blob-Filename.
    else assign 
            pv-blob  = ?
            pv-value = ?.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetColour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetColour Procedure 
PROCEDURE Proc-GetColour :
def input  param ip-colour      as char                     NO-UNDO.
def output param op-colour      as int      init 0          NO-UNDO.
{{&core}bug.i} 
find t-zen-colours where t-zen-colours.colour_name = ip-colour
         no-lock no-error.

    if available t-zen-colours then
        assign op-colour = t-zen-colours.colour_value.
/* else message 'No Colour Found For ' + ip-colour .  */
/*                                                    */
/* for each t-zen-colours:                            */
/* message t-zen-colours.colour_name skip             */
/*         t-zen-colours.colour_value.                */
/* end.                                               */

/*                                                      */
/*     {{&core}run.i &program     = "zen-colours.p"     */
/*                  &path      = "{&core}{&srv}"        */
/*                  &Appsrv    = "System"               */
/*                   &nomess    = 'true'   */
/*                  &direct    = "true"                 */
/*                  &noper     = true                   */
/*                  &procedure = "get-colour"           */
/*                  &params    = "(input  ip-colour,    */
/*                                 output op-colour)"}  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetCtrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetCtrl Procedure 
PROCEDURE Proc-GetCtrl :
def input param pv-idx as char no-undo.
def var lv-read as log no-undo.
def var lv-value as char no-undo init ''.
{{&core}bug.i} 
find first t-zen-control where t-zen-control.ctrl-idx = pv-idx
                         no-lock no-error.

if not avail t-zen-control 
then lv-read = true.
else assign lv-read  = t-Zen-Control.ReRead
            lv-value = t-zen-control.ctrl-data.
/*
message pv-idx skip
        'avail ' avail t-zen-control skip
        'reread ' lv-read skip
        lv-value 
        {&dbt}.
*/
if lv-read then do:
  {{&core}run.i &program   = "zen-control.p"
               &path      = "{&core}{&srv}"
               &Appsrv    = "System"  
               &nomess    = 'true'
               &procedure = "getcontrol"
               &params    = "(pv-idx,
                              output lv-value)"}
        
    if lv-value = ? then lv-value = ''.
end.
    RETURN lv-value.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetCurrrency) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetCurrrency Procedure 
PROCEDURE Proc-GetCurrrency :
def input  param ip-country         as int                  NO-UNDO.
    def output param op-currency        as char     init ""     NO-UNDO.
{{&core}bug.i} 

    {{&core}run.i &program   = "zen-country.p"
                 &path      = "{&core}{&srv}"
                 &Appsrv    = "System" 
&nomess    = 'true' 
                 &NoPer     = True
                 &procedure = "get-currency"
                 &params    = "(input  ip-country,
                                output op-currency)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetFieldWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetFieldWhere Procedure 
PROCEDURE Proc-GetFieldWhere :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-table     as char no-undo.
def input  param pv-where     as char no-undo. 
def input  param pv-datafield as char no-undo.
def output param pv-data      as char no-undo.
{{&core}bug.i} 
if not iscached(pv-table) then do:
  {{&core}run.i &program   = "dynamic.p"       
               &path      = "{&core}{&srv}"
               &Appsrv    = "system"  
               &nomess    = 'true'    
               &procedure = "Dyn-GetFieldWhere"             
               &params    = "(pv-table,
                              pv-where,
                              pv-datafield,
                              output pv-data)"}
end.
else do:
if pv-table begins 'zen' 
    then run proc-GetZenCacheFieldWhere (pv-table,pv-where,pv-datafield,output pv-data).
    else pv-data = GetSysCacheFieldWhere (pv-table,pv-where,pv-datafield).

end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetOsFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetOsFile Procedure 
PROCEDURE Proc-GetOsFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var okpressed as log no-undo. 
   def input-output param v-filename as char no-undo.
 {{&core}bug.i} 
   
   system-dialog get-file v-filename
         title "Choose File"
         filters "Select Files" '*.*'
         ask-overwrite create-test-file use-filename
         update okpressed.
if not okpressed then v-filename = '?'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetParentHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetParentHandle Procedure 
PROCEDURE Proc-GetParentHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param pv-child as handle no-undo.
def output param pv-parent as handle no-undo.
{{&core}bug.i} 
def var h as handle no-undo.
def var x as int no-undo.
def var lv-pname as char no-undo.

outerblock:
do x = 1 to 99:
    lv-pname = program-name(x).
    if lv-pname = ? then leave.
    if num-entries(lv-pname,' ') > 1 then next.
    h = session:first-procedure.
    do while valid-handle(h):
        if h:file-name = lv-pname then leave outerblock.
        h = h:next-sibling.
    End.
End.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetProcHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetProcHandle Procedure 
PROCEDURE Proc-GetProcHandle :
def input param pv-appsrv as char no-undo.
def input param pv-proc as char no-undo.
def output param lv-wid-handle  as handle no-undo.

{{&core}bug.i} 

pv-proc = unixpath(pv-proc).

if pv-appsrv = 'local' or
   pv-appsrv = ''
then lv-wid-handle = session.
else lv-wid-handle = getappserverhandle(pv-appsrv). 

    lv-wid-handle = lv-wid-handle:first-procedure.
    do while valid-handle(lv-wid-handle):  
      if lv-wid-handle:private-data = pv-proc
          then leave.
      lv-wid-handle = lv-wid-handle:next-sibling.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetSysVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetSysVar Procedure 
PROCEDURE Proc-GetSysVar :
def input param pv-key    as char no-undo.
def output param pv-value as char no-undo.    
{{&core}bug.i} 

{{&core}run.i &program   = "zen-context.p"       
             &path      = "{&core}{&srv}"
             &Appsrv    = "System" 
             &procedure = "GetVar"      
             &nomess    = 'true'       
             &params    = "(INPUT PV-key,
                           Output pv-value)"}

/* if pv-value = ? then pv-value = ''.  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-proc-GetZenCacheFieldWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-GetZenCacheFieldWhere Procedure 
PROCEDURE proc-GetZenCacheFieldWhere :
def input  param pv-table     as char no-undo.
def input  param pv-where     as char no-undo. 
def input  param pv-datafield as char no-undo.
def output param pv-data      as char no-undo.

pv-table = 't-' + pv-table.
{{&core}getfieldwhere.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-Haderrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-Haderrors Procedure 
PROCEDURE Proc-Haderrors :
def input param pv-type as char no-undo.
def output param pv-haderr as log no-undo.
  {{&core}bug.i} 
  {{&core}run.i &program   = "zen-context.p"       
                &path      = "{&core}{&srv}"
                &Appsrv    = "System"   
&nomess    = 'true'
                &procedure = "HadAnError"             
                &params    = "(pv-type,output pv-haderr)"} 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-Log) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-Log Procedure 
PROCEDURE Proc-Log :
def input param pv-type    as char no-undo.
def input param pv-prog    as char no-undo.
def input param pv-action  as char no-undo.
def input param pv-msg     as char no-undo.
def input param pv-logfile as char no-undo.

{{&core}run.i &program   = "zen-log.p"
             &path      = "{&core}{&srv}"
             &Appsrv    = "System"
             &nomess    = 'true'
             &noper     = 'true'
             &procedure = "makelog"
             &params    = "(pv-type,
                            pv-prog,
                            pv-action,
                            pv-msg,
                            pv-logfile)"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-PgmProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-PgmProperties Procedure 
PROCEDURE Proc-PgmProperties :
def input param  pv-pgm      as char no-undo.
def input param  pv-property as char no-undo.
def output param lv-value    as char no-undo.
 {{&core}bug.i} 
def var lv-edit as log no-undo init true.      
     

FIND FIRST t-zen-dpgm where t-zen-dpgm.pgm = pv-pgm NO-ERROR.  

if not avail t-zen-dpgm then
FIND FIRST t-zen-dpgm where t-zen-dpgm.pgm = dospath(pv-pgm) NO-ERROR.  

if not avail t-zen-dpgm then
FIND FIRST t-zen-dpgm where t-zen-dpgm.pgm = unixpath(pv-pgm) NO-ERROR. 
 
case pv-property:
    when 'AsyncReport' then do:
        lv-value = if avail t-zen-dpgm then string(t-zen-dpgm.AsyncReport)
                                       else 'no'.
    end.
    when 'author' then do:
        lv-value = if avail t-zen-dpgm then t-zen-dpgm.author
                                       else '?'.
    end.
    when 'multiinstance' then do:
        lv-value = if avail t-zen-dpgm then string(t-zen-dpgm.multiinstance)
                                       else 'no'.
    end. /*
    when 'checkreadonly' then do:
        lv-value = if avail t-zen-dpgm then string(t-zen-dpgm.checkreadonly)
                                       else 'no'.
    end. */
    when 'usedefaults' then do:
        lv-value = if avail t-zen-dpgm then string(t-zen-dpgm.UseDefaults)
                                       else 'no'.
    end.
    when 'id' then do:
        lv-value = if avail t-zen-dpgm then string(t-zen-dpgm.zen-dpgmtableid)
                                       else ?.
    end.
    when 'ValidApi'then do:
        lv-value = if avail t-zen-dpgm then 'passed'
                                       else 'failed'.
    end.
    when 'Menu'  then do:
        lv-value = if avail t-zen-dpgm then t-zen-dpgm.menu-grp
                                       else ''.
    end.
    when 'description' then do:
        lv-value = if avail t-zen-dpgm then t-zen-dpgm.description
                                       else ''. 
    end.
    when 'Title' then do:
        lv-value = if avail t-zen-dpgm then t-zen-dpgm.name
                                       else pv-pgm. 
    end.
    when 'Name' then do:
        if not avail t-zen-dpgm then
         FIND FIRST t-zen-dpgm where t-zen-dpgm.Zen-DpgmTableId = dec(pv-pgm) NO-ERROR.
        lv-value = if avail t-zen-dpgm then t-zen-dpgm.name
                                       else ''. 
    end.
    when 'ShortCut' then do:
       lv-value = if avail t-zen-dpgm then t-zen-dpgm.pgm
                                      else ''.
    end.
    when 'Position' then do:
        lv-value = if avail t-zen-dpgm then string(t-Zen-Dpgm.win-x) + '{&Delim2}' + string(t-Zen-Dpgm.win-y)
                                       else '?'.
    end.
    when 'MaxDataGuess' then do:
        lv-value = if avail t-zen-dpgm then string(t-zen-dpgm.DataGuess)
                                       else getctrl('MaxListCount').  
        if int(lv-value) = 0 then lv-value = getctrl('MaxListCount').
    end.
    when 'Comments' then do:
        lv-value = if avail t-zen-dpgm then string(t-zen-dpgm.comments) else "".
    end.
    when 'repinfo' then do:
        if avail t-zen-dpgm 
        then do: /* sort out weather crystal or normal report */
/*             if not t-zen-dpgm.UseCystal                          */
/*             then lv-value = t-Zen-Dpgm.reppath + "{&Delim2}" +   */
/*                             t-Zen-Dpgm.repprog + "{&Delim2}" +   */
/*                             t-Zen-Dpgm.repproc + "{&Delim2}" +   */
/*                             t-Zen-Dpgm.reptitle + "{&Delim2}" +  */
/*                             t-Zen-Dpgm.params.                   */
/*            else */ 
                 lv-value = t-Zen-Dpgm.RepTitle + '{&Delim2}' + 
                            t-Zen-Dpgm.reppath + "{&Delim2}" +
                            t-Zen-Dpgm.repprog + "{&Delim2}" +
                            t-Zen-Dpgm.repproc + "{&Delim2}" + 
                            t-zen-dpgm.ExtractFilePath + '{&Delim2}' +
                            t-Zen-Dpgm.ExtractFileName + '{&Delim2}' + 
                            t-zen-dpgm.CrystalPath + '{&Delim2}' +
                            t-Zen-Dpgm.CryFilename + '{&Delim2}' + 
                            t-Zen-Dpgm.params.
        end.
        else lv-value = '**no Dpgm Record'.
    end.
    when 'editable' then do:
    if avail t-zen-dpgm then 
        lv-edit = SecurityCheck(GetUserid(),
                                lv-group,  
                                t-zen-dpgm.NotEditUsers,
                                t-zen-dpgm.NotEditGroups,
                                t-Zen-Dpgm.EditUsers,
                                t-Zen-Dpgm.EditGroups).
        else lv-edit = no.
        if lv-edit then lv-value = 'passed'.
                  else lv-value ='failed'.            
    end.
    when 'ExcludeButtons' then do:
       if avail t-zen-dpgm then 
        lv-edit = SecurityCheck(GetUserid(),
                                lv-group,  
                                t-zen-dpgm.NotEditUsers,
                                t-zen-dpgm.NotEditGroups,
                                t-Zen-Dpgm.EditUsers,
                                t-Zen-Dpgm.EditGroups).
       else /* if GetCtrl('{&SystemMode}') = 'Dev'
            then lv-edit = true.
            else */ lv-edit = no.
 /*       if avail t-zen-dpgm then do: */
/*            if can-do(t-zen-dpgm.NotEditUsers,lv-user) or */
/*               can-do(t-zen-dpgm.NotEditGroups,lv-GROUP) */
/*            then lv-edit = false. */
/*            else if not can-do(t-Zen-Dpgm.EditUsers,lv-user) or */
/*                    not can-do(t-Zen-Dpgm.EditGroups,lv-GROUP) */
/*                 then lv-edit = false. */
/*        end. */
/*        else lv-edit = true. */
                   
       if lv-edit then lv-value = "*". /* replace with db field t-Zen-Dpgm.buttonlist */  
                  else lv-value = "!new,!edit,!delete,!undo,!save,!savenew,*".
    end.
    when 'DefButtons' then do:
       lv-value = if avail t-zen-dpgm then t-Zen-Dpgm.buttonlist 
                                      else "".
    end.
    when 'security' then do:
            lv-value = 'failed'.
            if GetUserid() = '' 
            then return.
            if avail t-zen-dpgm 
            then do:     
                if SecurityCheck(GetUserid(),
                                 lv-group,  
                                 t-zen-dpgm.not-users,
                                 t-zen-dpgm.not-group,
                                 t-zen-dpgm.run-users,
                                 t-zen-dpgm.run-groups)
                then  lv-value = 'passed'.
                else  lv-value = 'failed'.
            end.                                     
    end.
    when 'ProgGroup' then do:
       if avail t-zen-dpgm
          then lv-value = t-Zen-Dpgm.type.
          else lv-value = "".
    end.
    when 'UseCrystal' then do:
         lv-value = if avail t-zen-dpgm then string(t-zen-dpgm.UseCystal)
                                        else ''.
    end.
    when 'ReportRunType' then do:
         lv-value = if avail t-zen-dpgm then t-zen-dpgm.ReportRunType
                                        else ''.
    end.
    When 'ReportTitle' then do:
        lv-value = if avail t-zen-dpgm then t-Zen-Dpgm.RepTitle 
                                       else ''.
    end.
    otherwise lv-value = 'no'.
end case.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-ReLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-ReLabel Procedure 
PROCEDURE Proc-ReLabel :
DEF INPUT PARAM pv-hand AS HANDle NO-UNDO.
def input param pv-type as char no-undo.
      
def var x        as int    no-undo init 1.
def var y        as int    no-undo init 2.
def var lv-set   as char   no-undo.
DEF VAR h-label  AS HANDle NO-UNDO.
def var lv-label as char   no-undo.
{{&core}bug.i} 
if int(getsysvar("{&clv}language")) = 0 then return.
        
def var v-sep         as char no-undo.
def var lv-dateformat as char no-undo. 

assign v-sep         = DateSep(UserLanguage(GetSysVar("user")))
       lv-dateformat = '99' + v-sep + '99' + v-sep + '9999'.
    
if pv-hand:type = 'frame' 
    then pv-hand = pv-hand:current-iteration.
       
pv-hand = pv-hand:FIRST-CHILD.

if pv-type = 'browse' then
DO WHILE VALID-HANDLE(pv-hand): 
    if pv-hand:TYPE eq "browse" then relabelbrowse(pv-hand,lv-dateformat).
    else if pv-hand:TYPE eq "frame" then relabel(pv-hand,'browse').
    pv-hand = pv-hand:NEXT-SIBLING.
END.
else
if pv-type = 'tab' then tablabel(?,pv-hand).
else 
DO WHILE VALID-HANDLE(pv-hand): 
    case pv-hand:type:
        when "browse" then ReLabelBrowse(pv-hand,lv-dateformat).
        when "frame"  then relabel(pv-hand,'screen').
        when "radio-set" 
            THEN do:
                assign x = 1
                       y = 1
                       lv-set = pv-hand:radio-buttons.
                do x = 1 to pv-hand:num-buttons:
                    entry(y,lv-set) = AltLanguage(replace(entry(y,lv-set),'&','')).
                    y = y + 2.
                end.
                pv-hand:radio-buttons = lv-set.
            end.        
        otherwise do:
            IF CAN-SET(pv-hand,"SIDE-LABEL-HANDLE":U) OR 
               pv-hand:TYPE = "BUTTON" 
            THEN DO:
                IF pv-hand:TYPE = "BUTTON" THEN h-label = pv-hand.
                                           ELSE h-label = pv-hand:SIDE-LABEL-HANDLE NO-ERROR.
                IF VALID-HANDLE(h-label) THEN DO: 
                   lv-label = AltLanguage(replace(pv-hand:LABEL,'&','')).
                       if pv-hand:TYPE = "BUTTON" then do:
                          if pv-hand:image ne '' 
                            then pv-hand:tooltip = lv-label.
                            else assign pv-hand:LABEL   = lv-label
                                        pv-hand:tooltip = lv-label NO-ERROR.
    
                       end.
                       else
                       assign pv-hand:LABEL       = lv-label
                              h-label:WIDTH-CHARS = max(length(pv-hand:LABEL,"column"), 
                                                        h-label:WIDTH-CHARS)
                              pv-hand:tooltip     = lv-label NO-ERROR.
                    
                    if pv-hand:data-type = 'date' and 
                       can-set(pv-hand,'format')
                    then pv-hand:format = lv-dateformat.                               
                END.
                else IF CAN-SET(pv-hand,"SCREEN-VALUE":U) 
                     THEN DO:
                        lv-label = AltLanguage(pv-hand:SCREEN-VALUE).
                        ASSIGN pv-hand:WIDTH-CHARS  = max(LENGTH(lv-label,"column"),pv-hand:WIDTH-CHARS)
                               pv-hand:SCREEN-VALUE = lv-label NO-ERROR.
                      END.
            END.      
            ELSE DO:
                if pv-hand:type = 'toggle-box' 
                    then pv-hand:label = AltLanguage(pv-hand:label).
                    else IF CAN-SET(pv-hand,"SCREEN-VALUE":U) 
                    THEN DO:
                        lv-label = AltLanguage(pv-hand:SCREEN-VALUE).
                        ASSIGN pv-hand:WIDTH-CHARS  = max(LENGTH(lv-label,"column"),pv-hand:WIDTH-CHARS)
                               pv-hand:SCREEN-VALUE = lv-label NO-ERROR.
                    END.
            END.
        end.
    end case.
    pv-hand = pv-hand:NEXT-SIBLING.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-RgbColour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-RgbColour Procedure 
PROCEDURE Proc-RgbColour :
def input param pv-name as char no-undo.
def output param o-rgb  as char no-undo.
{{&core}bug.i} 
    find t-zen-colours where t-zen-colours.colour_name = pv-name
         no-lock no-error.

    if available t-zen-colours then
    assign o-rgb = string(t-zen-colours.red_value) + ',' +
                   string(t-zen-colours.green_value) + ',' +
                   string(t-zen-colours.blue_value).
/*     {{&core}run.i &program   = "colours.p"       */
/*                  &path      = "{&core}{&srv}"    */
/*                  &Appsrv    = "System"           */
/*                  &NoPer     = True               */
/*                  &procedure = "Get-colour-rgb"   */
/*                  &params    = "(pv-name,         */
/*                                 OUTPUT o-rgb)"}  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-SecurityCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SecurityCheck Procedure 
PROCEDURE Proc-SecurityCheck :
def input param pv-user as char no-undo.
def input param pv-group as char no-undo.
def input param pv-notusers as char no-undo.
def input param pv-notgroups as char no-undo.
def input param pv-okusers as char no-undo.
def input param pv-okgroups as char no-undo.
def output param lv-return as char no-undo.  
{{&core}bug.i} 

  def var lv-canaccess as char no-undo init ',,,,,,,,,,'.
   if Lv-SecurityAlgorithm = 0 then Lv-SecurityAlgorithm = 2.
    case Lv-SecurityAlgorithm:
        when 1 then do: /* old style zen type access logic */
            entry(lv-securityAlgorithm,lv-canaccess) = 'Passed'.
            if can-do(pv-notusers,pv-user) or
               can-do(pv-notgroups,pv-GROUP) 
            then entry(1,lv-canaccess) = '**Failed is in NOT Strings'.
            else if not can-do(pv-okusers,pv-user) or
                    not can-do(pv-okgroups,pv-GROUP)
                  then entry(1,lv-canaccess) = '**Failed is not in Ok strings'.
        end. 
        when 2 then do: /* newer JimS Style logic */
            entry(lv-securityAlgorithm,lv-canaccess) = '**Failed by Default'.
            if can-do(pv-notusers,pv-user) 
            then entry(2,lv-canaccess) = '**Failed is in NOT Users'.
            else if can-do(pv-okusers,pv-user)  
                 then entry(2,lv-canaccess) = 'Passed'.
                 else if can-do(pv-notgroups,pv-GROUP) 
                      then entry(2,lv-canaccess) = '**Failed is in NOT Groups'.
                      else if can-do(pv-okgroups,pv-GROUP)  
                           then entry(2,lv-canaccess) = 'Passed'.
        end.
        otherwise entry(lv-securityAlgorithm,lv-canaccess) = '**Faild No Such Algorithm'.
    end case.
    
  lv-return = entry(lv-securityAlgorithm,lv-canaccess).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-SetCtrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SetCtrl Procedure 
PROCEDURE Proc-SetCtrl :
def input param pv-idx  as char no-undo.
def input param pv-data as char no-undo.
{{&core}bug.i} 
  {{&core}run.i &program   = "zen-control.p"
               &path      = "{&core}{&srv}"
               &Appsrv    = "System"  
               &nomess    = 'true'
               &procedure = "setcontrol"
               &params    = "(pv-idx,
                              pv-data)"}
        
    if return-value = 'failed' 
        then return 'failed'.
    
find t-zen-control where t-zen-control.ctrl-idx = pv-idx
                   no-error.
if avail t-zen-control 
    then t-zen-control.ctrl-data = pv-data.
/* instead of    RefreshTempTables(). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-SetSensitive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SetSensitive Procedure 
PROCEDURE Proc-SetSensitive :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-enable       as log   no-undo.
def input param pv-ix           as char  no-undo.
def input param pv-exc-list     as char  no-undo.
def input param pv-frame-handle as handle no-undo.
    {{&core}bug.i} 
def var lv-wid-handle as handle no-undo.
def var lv-in-list    as log    no-undo.
def var lv-wid-name   as char   no-undo.
def var lv-wid-table  as char   no-undo.
def var lv-wid-dbase  as char   no-undo.
def var lv-cnt        as int    no-undo.
/* message program-name(1) skip  */
/*         program-name(2) skip  */
/* program-name(3) skip          */
/* program-name(4) skip          */
/* lv-disbgcolor.                */

if pv-exc-list ne '' then
    do lv-cnt = 1 to num-entries(pv-exc-list):
       entry(lv-cnt,pv-exc-list) = trim(entry(lv-cnt,pv-exc-list)).
    end.

assign lv-wid-handle = pv-frame-handle
       lv-wid-handle = lv-wid-handle:first-child
       lv-wid-handle = lv-wid-handle:first-child.

do while valid-handle(lv-wid-handle):
    if lv-wid-handle:type ne 'literal' then do:
        assign lv-wid-name  = lv-wid-handle:name
               lv-wid-table = if can-query(lv-wid-handle,'table')
                               then lv-wid-handle:table
                               else ''
               lv-wid-dbase = if can-query(lv-wid-handle,'dbname')
                               then lv-wid-handle:dbname
                               else ''.

        /* make a var for in the list as its v confusing otherwise */
        lv-in-list = if can-do(pv-exc-list,lv-wid-name) or
                        can-do(pv-exc-list,lv-wid-table + '.' + lv-wid-name) or
                        can-do(pv-exc-list,lv-wid-dbase + '.' + lv-wid-table + '.' + lv-wid-name)
                     then true
                     else false.

        if ((pv-ix begins 'exc' and not lv-in-list) or
            (pv-ix begins 'inc' and lv-in-list)) and
           lv-wid-handle:sensitive ne pv-enable 
        then do: 
            if can-do('excludeh,includeh',pv-ix)
                then lv-wid-handle:VISIBLE = pv-enable.
            lv-wid-handle:sensitive = pv-enable.

            if lv-wid-handle:type = 'fill-in' then lv-wid-handle:fgcolor = ?.
            if lv-wid-handle:data-type = 'date' and lv-wid-handle:screen-value = '  /  /  '
                then lv-wid-handle:screen-value = '?'.
        end.
/*         if not lv-wid-handle:sensitive and                */
/*         can-do(lv-wl,lv-wid-handle:type)                  */
/*             then lv-wid-handle:bgcolor = lv-disbgcolour.  */
/*             else lv-wid-handle:bgcolor = ?.               */
    end.
    if can-set(lv-wid-handle,'modified') 
            then lv-wid-handle:modified = false. 
    lv-wid-handle = lv-wid-handle:NEXT-sibling.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-SetSessionLangFormats) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SetSessionLangFormats Procedure 
PROCEDURE Proc-SetSessionLangFormats :
def input param pv-lanid as int no-undo.

def var lv-value as char no-undo.
{{&core}bug.i} 
{{&core}run.i &program   = "zen-language.p"
             &path      = "{&core}{&srv}"
             &Appsrv    = "System"  
&nomess    = 'true'
             &noper     = true
             &procedure = "GetLangFormats"
             &params    = "(pv-lanid,
                           output lv-value)"}

session:set-numeric-format(entry(1,lv-value,'{&Delim2}'),entry(2,lv-value,'{&Delim2}')).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-SetSysVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SetSysVar Procedure 
PROCEDURE Proc-SetSysVar :
def input param pv-key   as char no-undo.
def input param pv-value as char no-undo.
{{&core}bug.i} 


if index(pv-key,'=') ne 0
then do:
    def var lv-var as char no-undo.
    def var lv-value as char no-undo.
    def var x as int no-undo.
    do x = 1 to num-entries(pv-key):
        lv-var  = entry(1,entry(x,pv-key),'=').
        lv-value = entry(2,entry(x,pv-key),'=').
        setclientvar (lv-var,lv-value).
    end.
end.
else SetClientVar(pv-key,pv-value).
if not pv-key begins "{&clv}" 
then do:  
  {{&core}run.i &program   = "zen-context.p"       
               &path      = "{&core}{&srv}"
               &Appsrv    = "System"    
               &procedure = "setvar"       
               &nomess    = 'true'       
               &params    = "(INPUT PV-key,
                              Input pv-value)"}
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-Sound) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-Sound Procedure 
PROCEDURE Proc-Sound :
def input param pv-file as char no-undo.
{{&core}bug.i} 
DEF VAR szSound     AS MEMPTR NO-UNDO.
SET-SIZE(szSound) = 0.
def var lv-soundfile as char no-undo.

{{&core}run.i &program   = "zen-sound.p"
             &path      = "{&core}{&srv}"
             &Appsrv    = "System"  
             &noper     = true
             &direct    = "true"
             &nomess    = 'true'
             &procedure = "GetSound"
             &params    = "(pv-file,
                            output lv-soundfile)"}
if lv-soundfile = '' then return.
WapiPlaySound(GET-POINTER-VALUE(szSound)).
SET-SIZE(szSound) = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-SpellCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SpellCheck Procedure 
PROCEDURE Proc-SpellCheck :
/*------------------------------------------------------------------------------
  Purpose:     Spell check editor and fill in widgets
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&core}bug.i} 
  DEFINE var PV-HANDLE AS HANDLE NO-UNDO.      
  pv-handle = focus.
  IF pv-handle:TYPE = "editor"  THEN
  DO:
      DEFINE VAR wordAppl AS COM-HANDLE.
      DEFINE VAR i as int.

    LOAD "Word.Application" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR. /* Open Registry key */
    IF error-status:error THEN
    DO:
        CREATE "Word.Basic" wordAppl.
        NO-RETURN-VALUE wordAppl:FileNew.
        NO-RETURN-VALUE wordAppl:Insert(pv-handle:SCREEN-VALUE).
        ASSIGN i = wordAppl:ToolsSpelling NO-ERROR.
        NO-RETURN-VALUE wordAppl:AppHide("Microsoft Word").
        NO-RETURN-VALUE wordAppl:EditSelectAll.
        pv-handle:SCREEN-VALUE = wordAppl:Selection().
        NO-RETURN-VALUE wordAppl:FileClose(2).
        NO-RETURN-VALUE wordAppl:AppClose("Microsoft Word").
    END.
    ELSE
    DO:
        UNLOAD "Word.Application".
        CREATE "Word.Application" wordAppl.
        wordAppl:Documents:Add().
        wordAppl:Documents:Item(1):Range(0,0):InsertAfter(pv-handle:SCREEN-VALUE).
        wordAppl:Options:CheckGrammarWithSpelling = TRUE.
        wordAppl:Documents:Item(1):CheckGrammar().
        wordAppl:Visible = FALSE.
        pv-handle:SCREEN-VALUE=wordAppl:Documents:Item(1):Range(0,wordAppl:Documents:Item(1):Characters:Count):Text.
        wordAppl:Quit(0).
    END.

     RELEASE OBJECT wordAppl.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-SysMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SysMsg Procedure 
PROCEDURE Proc-SysMsg :
def input param pv-msg as char no-undo.
 {{&core}bug.i} 
if session:remote then return 'true'.

if pv-msg ne 'off' then do:
    if not valid-handle(h-msg) then 
        run {&core}sys-msg.w persist set h-msg.
    run refresh in h-msg (pv-msg).
end.
else delete procedure h-msg NO-ERROR.

  RETURN 'true'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-Tooltip) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-Tooltip Procedure 
PROCEDURE Proc-Tooltip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input param frame-handle as handle no-undo.
    def input param pv-changefont as log no-undo.
    def var lv-wid-handle   as handle no-undo.
{{&core}bug.i} 
    if lv-scrfont ne '' and pv-changefont
      then frame-handle:font = int(lv-scrfont).

    assign
        lv-wid-handle = frame-handle
        lv-wid-handle = lv-wid-handle:first-child
        lv-wid-handle = lv-wid-handle:first-child.
        
    do while valid-handle(lv-wid-handle):
        if can-query(lv-wid-handle,'type') then do:
            if lv-wid-handle:type = 'frame' or
               lv-wid-handle:type = 'dialog-box'
            then run proc-tooltip (lv-wid-handle,pv-changefont).
            else do:
                if can-set(lv-wid-handle,'delimiter')
                    then lv-wid-handle:delimiter = '{&combodelim}'.
                if lv-wid-handle:private-data = 'ExcludeFromDefaults'
                then do:
                    if can-set(lv-wid-handle,'context-help-id')
                    then lv-wid-handle:context-help-id = {&ExcludeFromDefaultsID}.
                end.
                if can-query(lv-wid-handle,'tooltip') then do:
                    if lv-wid-handle:tooltip = '' or
                       lv-wid-handle:tooltip = ?
                       then lv-wid-handle:tooltip = lv-wid-handle:help.
                end.
            end.
          end.
         lv-wid-handle = lv-wid-handle:next-sibling.
    end.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-UserProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-UserProperties Procedure 
PROCEDURE Proc-UserProperties :
def input param  pv-user     as char no-undo.
def input param  pv-property as char no-undo.
def output param lv-value    as char no-undo.
{{&core}bug.i} 
    if pv-user = '' then do:
        lv-value = ?.
        return.
    end.
    find t-zen-duser where t-zen-duser.duser = pv-user no-error.    

    if not avail t-zen-duser then do:
          for each t-zen-duser:
            delete t-zen-duser.
          end.
          {{&core}run.i &program   = "zen-duser.p"
                       &path      = "{&core}{&srv}"
                       &Appsrv    = "System"  
                       &noper     = true               
                       &direct    = "true"
                       &nomess    = 'true'
                       &procedure = "find-record"
                       &params    = "(pv-user,
                                      input-output table t-zen-duser)"}
          find first t-zen-duser no-error.  
          if not avail t-zen-duser then do:
            lv-value = ?.
            return.
          end.
    end.
   
case pv-property:
    when 'Country'  then lv-value = string(t-zen-duser.country).
    when 'Group'    then lv-value = t-zen-duser.U-GROUP.
    when 'language' then lv-value = string(t-zen-duser.lan_lanid).
    when 'name'     then lv-value = t-zen-duser.user-name.
    when 'password' then lv-value = t-Zen-Duser.dpassword.
    when 'email'    then lv-value = t-zen-duser.email.
    when 'SystemManager' Then lv-value = if t-zen-duser.sys-man then 'yes' else 'no'.
    otherwise lv-value = pv-property + ' Not Found'.
end case.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-ValidClassCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-ValidClassCode Procedure 
PROCEDURE Proc-ValidClassCode :
def output param lv-ok    as log  no-undo.
    def input param  pv-class as char no-undo.
    def input param  pv-code  as char no-undo.
    def output param lv-desc  AS CHAR no-undo.
    {{&core}bug.i} 
    
    {{&core}run.i &program   = "zen-ClassCode.p"
                 &path      = "{&core}{&srv}"
                 &Appsrv    = "System"  
                 &NoPer     = True
&nomess    = 'true'
                 &procedure = "CheckValidClassCode"
                 &params    = "(pv-class,
                                pv-code,
                                output lv-ok,
                                OUTPUT lv-desc)"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-ValidUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-ValidUser Procedure 
PROCEDURE Proc-ValidUser :
def input param pv-user as char no-undo.
    def input param pv-pass as char no-undo.
    def output param pv-ok  as log no-undo.
{{&core}bug.i} 
&if defined(no_user) = 0 &then
{{&core}run.i &program   = "validuser.p"
             &path      = "{&core}{&srv}"
             &Appsrv    = "System"
             &nomess    = 'true'
             &noper     = 'true'
             &procedure = "checkuser"
             &params    = "(pv-user,
                            pv-pass,
                           Output pv-ok)"}
&else
{{&core}run.i &program   = "validuser.p"
             &path      = "{&core}{&srv}"
             &Appsrv    = "System"
             &nomess    = 'true'
             &noper     = 'true'
             &procedure = "no_user"
             &params    = "(pv-user,
                            pv-pass,
                           Output pv-ok)"}
&endif
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-WidSecCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-WidSecCheck Procedure 
PROCEDURE Proc-WidSecCheck :
/*------------------------------------------------------------------------------
  Purpose:   widget level security  
------------------------------------------------------------------------------*/
def input param frame-handle as handle no-undo.
def input param pv-progname  as char   no-undo.
{{&core}bug.i} 
def var lv-wid-handle as handle no-undo.
def var lv-group      as char   no-undo.
def var lv-parent     as handle no-undo.        
def var lv-origx as int no-undo.
def var lv-origy as int no-undo.
    
def var lv-wid-name  as char no-undo.
def var lv-wid-table as char no-undo.
def var lv-wid-dbase as char no-undo.
def var lv-in-list   as log  no-undo.
def var lv-cnt       as int  no-undo.
def var lv-fhidden   as log  no-undo init false.
find t-zen-dpgm where t-zen-dpgm.pgm = pv-progname no-error.
if avail t-zen-dpgm
then do:
    assign lv-wid-handle = frame-handle
           lv-wid-handle = lv-wid-handle:first-child
           lv-wid-handle = lv-wid-handle:first-child
           lv-group      = UserGroup(GetUserid()).
           
    if lv-exc-list ne '' then
    do lv-cnt = 1 to num-entries(lv-exc-list):
       entry(lv-cnt,lv-exc-list) = trim(entry(lv-cnt,lv-exc-list)).
    end.
    
    do while valid-handle(lv-wid-handle):
        if lv-wid-handle:type = 'frame' then do:
            run Proc-WidSecCheck (lv-wid-handle,pv-progname).
             if can-set(lv-wid-handle,'modified') then
                lv-wid-handle:modified = false. 
            lv-wid-handle = lv-wid-handle:NEXT-sibling.
            next.
        end.
        
        lv-parent = lv-wid-handle:frame.                
        find first t-zen-dwidget where t-Zen-Dwidget.pgm      = t-zen-dpgm.pgm
                                   and frame-handle:name matches t-Zen-Dwidget.WidFrame
                                   and lv-wid-handle:name matches t-Zen-Dwidget.wid-name
                                 no-lock no-error.
        if avail t-zen-dwidget then do:   
/*         message program-name(1) skip */
/*         lv-user t-zen-dwidget.not-users can-do(t-zen-dwidget.not-users,lv-user) skip */
/*         lv-group t-zen-dwidget.not-groups can-do(t-zen-dwidget.not-groups,lv-GROUP). */

/*         lv-wid-handle:sensitive = SecurityCheck(lv-user,lv-group,  
                                                   t-zen-dwidget.not-users,t-zen-dwidget.not-groups,
                                                   '','').

*/
            if can-do(t-zen-dwidget.not-users,GetUserid()) or
               can-do(t-zen-dwidget.not-groups,lv-GROUP)
            then lv-wid-handle:sensitive = false.
        end.
                
        assign
           lv-wid-name = if lv-wid-handle:table ne ? then lv-wid-handle:table + '.' else ''
           lv-wid-name = lv-wid-name + lv-wid-handle:name.
        
/*         if not lv-wid-handle:hidden and                                                        */
/*           lv-wid-handle:sensitive  and                                                         */
/*           can-find(first t-zen-widlook where t-zen-dpgm.PGM Matches t-Zen-widlook.in-Program   */
/*                                          And frame-handle:name Matches t-zen-widlook.In-Frame  */
/*                                          And t-zen-widlook.look-field = lv-wid-name)           */
/*         then lv-wid-handle:bgcolor = lv-lookupcolour.                                          */
/*         else if can-do(lv-wl,lv-wid-handle:type)           */
/*              then lv-wid-handle:bgcolor = lv-disbgcolour.  */
        
        if can-set(lv-wid-handle,'modified') 
            then lv-wid-handle:modified = false. 
            
        lv-wid-handle = lv-wid-handle:NEXT-sibling.
    end.
end.
else do while valid-handle(lv-wid-handle):
/*         if not lv-wid-handle:sensitive and                */
/*            can-do(lv-wl,lv-wid-handle:type)               */
/*             then lv-wid-handle:bgcolor = lv-disbgcolour.  */
        if can-set(lv-wid-handle,'modified') then
                lv-wid-handle:modified = false. 
        if lv-wid-handle:type = 'frame' then do:
            run Proc-WidSecCheck (lv-wid-handle,pv-progname).
            lv-wid-handle = lv-wid-handle:NEXT-sibling.
        end.
     end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-WinMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-WinMessage Procedure 
PROCEDURE Proc-WinMessage :
def input param pv-mesnum as int  no-undo.
def input param pv-extra1 as char no-undo.
def input param pv-extra2 as char no-undo.
def input param pv-extra3 as char no-undo.
def input param pv-extra4 as char no-undo.
def output param pv-ok as log no-undo.
{{&core}bug.i} 
def var lv-mestxt as char no-undo.
def var lv-title  as char no-undo.
def var lv-style  as int no-undo.
def var lv-result as int no-undo.

for each t-zen-mesfil:
    delete t-zen-mesfil.
end.

{{&core}run.i &program   = "zen-mesfil.p"
             &path      = "{&core}{&srv}"
             &Appsrv    = "System"
&nomess    = 'true'
             &noper     = true
             &procedure = "find-record"
             &params    = "(pv-mesnum,
                            input-output table t-zen-mesfil)"}
find t-zen-mesfil where t-zen-mesfil.mesnum = pv-mesnum no-error.

   if not avail t-zen-mesfil then
   do:
       message "MESSAGE No: " string(pv-mesnum) " not found" view-as alert-box.
       return.
   end.

   lv-mestxt = t-zen-mesfil.mestxt.
   
   IF pv-extra1 = ? THEN pv-extra1 = altlanguage('"Unknown"').
   IF pv-extra2 = ? THEN pv-extra2 = altlanguage('"Unknown"').
   IF pv-extra3 = ? THEN pv-extra3 = altlanguage('"Unknown"').
   IF pv-extra4 = ? THEN pv-extra4 = altlanguage('"Unknown"').
   
   if UserLanguage(GetSysVar("user")) ne 0 
   then do:
        if pv-extra4 ne '' then pv-extra4 = AltLanguage(pv-extra4).
        if pv-extra3 ne '' then pv-extra3 = AltLanguage(pv-extra3).
        if pv-extra2 ne '' then pv-extra2 = AltLanguage(pv-extra2).             
        if pv-extra1 ne '' then pv-extra1 = AltLanguage(pv-extra1).
   end.
   
   assign lv-mestxt = replace(lv-mestxt,'#4',pv-extra4)
          lv-mestxt = replace(lv-mestxt,'#3',pv-extra3)
          lv-mestxt = replace(lv-mestxt,'#2',pv-extra2)
          lv-mestxt = replace(lv-mestxt,'#1',pv-extra1)
          lv-mestxt = string('(' + string(t-zen-mesfil.mesnum) + ') ' + lv-mestxt).

   case t-zen-mesfil.mestyp:
        when 'm' then lv-title = 'Message'.
        when 'q' then lv-title = 'Question'.
        when 'w' then lv-title = 'Warning'.
        when 'i' then lv-title = 'Information'.
        when 'e' then lv-title = 'Error'.
   end case.

   lv-style = t-zen-mesfil.style.
   if lv-style = 6 or
      lv-style = 7 or
      lv-style = 9 then lv-style = 0.

   lv-result = WapiMessageBox(0,lv-mestxt,lv-title,lv-style).

   case lv-result:
        when 1 then pv-ok = true.
        when 2 then pv-ok = ?.
        when 3 then pv-ok = false.
        when 4 then pv-ok = true.
        when 5 then pv-ok = ?.
        when 6 then pv-ok = true.
        when 7 then pv-ok = false.
        otherwise pv-ok = false.
   end case.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-Wlook) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-Wlook Procedure 
PROCEDURE Proc-Wlook :
/*------------------------------------------------------------------------------
  Purpose:   widget level lookups  
------------------------------------------------------------------------------*/
def var lv-wid as handle no-undo.
{{&core}bug.i} 


if self:type = 'fill-in'
then RUN widget-help in THIS-PROCEDURE ('look','').
else do: /* we pressed a button */
    lv-wid = widget-handle(self:private-data) no-error.
                    
    if not valid-handle(lv-wid) then return.
    if not lv-wid:sensitive then return.
    if lv-wid:type ne 'fill-in' then return.
    focus = lv-wid.

    RUN widget-help in THIS-PROCEDURE ('look',string(lv-wid)).
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-ZenCachedCombo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-ZenCachedCombo Procedure 
PROCEDURE Proc-ZenCachedCombo :
def input param pv-table as char no-undo.
def input param pv-key   as char no-undo.
def input param pv-field as char no-undo.
def input param pv-where as char no-undo.
def input param pv-by    as char no-undo.
def input param pv-none  as log  no-undo.
def input param pv-wild  as log  no-undo.
def output param pv-codes  as char no-undo.
def output param pv-values as char no-undo.

pv-table = 't-' + pv-table.

  {{&core}bldcombo.i} /* so we have same functionality here
                         as in srv/dynamic.p */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Report-Procs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Report-Procs Procedure 
PROCEDURE Report-Procs :
{{&core}bug.i} 
run {&core}pprocs.w persistent.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Set-Back-Color) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Back-Color Procedure 
PROCEDURE Set-Back-Color :
/*------------------------------------------------------------------------------
  Purpose:  change disabled widget foreground colour makes native widgets look ok
------------------------------------------------------------------------------*/
  def input param pv-mode as char no-undo.
{{&core}bug.i} 
  def var rgbGrayText   as int no-undo.
  def var lpElements    as memptr.
  def var lpRgb         as memptr.
  def var lv-orig-color as char no-undo.
  def var lv-color      as char no-undo.
 
  set-size(lpElements)   = 4.   /* = sizeof(long)   */
  set-size(lpRgb)        = 4.
  put-long(lpElements,1) = 17.  /* = COLOR_GRAYTEXT */
 
  if pv-mode ne 'off' then do:
      rgbgraytext = WapiGetSysColor(17). /* = COLOR_GRAYTEXT */

      lv-orig-color = substring(string(rgbGrayText),1,2) + ',' +
                      substring(string(rgbGrayText),3,2) + ',' +
                      substring(string(rgbGrayText),5,2).
     SetSysVar('{&clv}orig-back-color',lv-orig-color).
  end.
  else do:
      lv-orig-color = GetSysVar('{&clv}orig-back-color').
      
      put-long(lpRgb,1) = rgb-value(int(entry(1,lv-orig-color)),
                                    int(entry(2,lv-orig-color)),
                                    int(entry(3,lv-orig-color))).

      WapiSetSysColors(1,          /* = number of elements */
                       get-pointer-value(lpElements),
                       get-pointer-value(lpRgb)).
 
      return.
  end.
  lv-color = getctrl("{&DisabledTextColour}").
/* lv-color = '1,1,1'.  */
  if lv-color ne ""
  THEN do:
    put-long(lpRgb,1) = rgb-value(INT(ENTRY(1,lv-color)),
                                  INT(ENTRY(2,lv-color)),
                                  INT(ENTRY(3,lv-color)) ).
    WapiSetSysColors(1,          /* = number of elements */
                       get-pointer-value(lpElements),
                       get-pointer-value(lpRgb)).

  end.
  
  set-size(lpElements) = 0.
  set-size(lpRgb)      = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Set-Browse-Columns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Browse-Columns Procedure 
PROCEDURE Set-Browse-Columns :
/*------------------------------------------------------------------------------
  Purpose: enable/disable browse columns    
------------------------------------------------------------------------------*/
def input param pv-browse as handle no-undo.
def input param pv-enable as log no-undo.
{{&core}bug.i} 
    def var lv-wid-handle as handle no-undo.
    def var lv-first-widget as handle no-undo. 

    assign         
        lv-wid-handle   = pv-browse
        lv-wid-handle   = lv-wid-handle:first-column
        lv-first-widget = lv-wid-handle.

    do while valid-handle(lv-wid-handle):
        assign lv-wid-handle:read-only = not pv-enable
               lv-wid-handle = lv-wid-handle:next-column.
        if lv-wid-handle = lv-first-widget then leave.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetMovable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetMovable Procedure 
PROCEDURE SetMovable :
def input param pv-moveable as log    no-undo.
def input param pv-frame    as handle no-undo.
{{&core}bug.i} 
def var lv-wid-handle as handle no-undo.
def var lv-parent     as handle no-undo.        

/*if z-country = 44 then return.*/

assign lv-wid-handle = pv-frame
       lv-wid-handle = lv-wid-handle:first-child
       lv-wid-handle = lv-wid-handle:first-child.

do while valid-handle(lv-wid-handle):
    if lv-wid-handle:type = 'frame' then
        run setmovable (pv-moveable,lv-wid-handle).
    else assign lv-wid-handle:movable = pv-moveable
                lv-wid-handle:sensitive = true.
    lv-wid-handle = lv-wid-handle:NEXT-sibling.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetRegistryColors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetRegistryColors Procedure 
PROCEDURE SetRegistryColors :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-colornum as char no-undo.
def var NewKeyValue as char no-undo.
def var lv-num as char no-undo.
   /* loop through colours, saving to registry file */
{{&core}bug.i} 
load 'progress'.
use 'progress'.

for each t-zen-colours no-lock:
    if colour_value le 15 then next.

    lv-colornum = 'Color' + string(colour_value).
    NewKeyValue = 
        string(red_value) + "," + 
        string(green_value) + "," + 
        string(blue_value).
    put-key-value section 'Colors' key lv-colornum value NewKeyValue.
end. /* for each */

load 'progress'.
use 'progress'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSysFonts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSysFonts Procedure 
PROCEDURE SetSysFonts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-ctrlfont as char no-undo.
def var lv-regfont as char no-undo.
def var lv-fontnum as char no-undo.
{{&core}bug.i}
lv-fontnum = getctrl('printerfontnumber').
if lv-fontnum = '' then lv-fontnum = '2'.
lv-fontnum = 'font' + lv-fontnum.
get-key-value section 'fonts' key lv-fontnum value lv-regfont.
lv-ctrlfont = getctrl('printerfont').
if lv-ctrlfont ne lv-regfont
then do:
    if lv-ctrlfont = '' then lv-ctrlfont = 'Courier New, size 8'.
    put-key-value section 'fonts' key lv-fontnum value lv-ctrlfont.
end.
  
get-key-value section 'startup' key 'printerfont' value lv-regfont.
lv-ctrlfont = getctrl('printerfont').
if lv-ctrlfont ne lv-regfont
then do:
    if lv-ctrlfont = '' then lv-ctrlfont = 'Courier New, size 10'.
    put-key-value section 'startup' key 'printerfont' value lv-ctrlfont.
end.
  
lv-fontnum = getctrl('screenfontnumber').
if lv-fontnum = '' then lv-fontnum = '1'.
lv-fontnum = 'font' + lv-fontnum.
get-key-value section 'fonts' key lv-fontnum value lv-regfont.
lv-ctrlfont = getctrl('screenfontname').
if lv-ctrlfont ne lv-regfont
then do:
    if lv-ctrlfont = '' then lv-ctrlfont = 'MS Sans Serif, size=8'.
    put-key-value section 'fonts' key lv-fontnum value lv-ctrlfont.
end.
  
lv-fontnum = getctrl('boldfontnumber').
if lv-fontnum = '' then lv-fontnum = '6'.
lv-fontnum = 'font' + lv-fontnum.
get-key-value section 'fonts' key lv-fontnum value lv-regfont.
lv-ctrlfont = getctrl('boldfontname').
if lv-ctrlfont ne lv-regfont
then do:
    if lv-ctrlfont = '' then lv-ctrlfont = 'MS Sans Serif, size=8, bold'.
    put-key-value section 'fonts' key lv-fontnum value lv-ctrlfont.
end.
  
load 'progress'.
use 'progress'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Widget-Help) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Widget-Help Procedure 
PROCEDURE Widget-Help :
def input param pv-mode         as char     no-undo.
def input param pv-wid as char no-undo.
/* define buffer bzen-fldlook for t-zen-fldlook. */
{{&core}bug.i} 
def var v-command     as char   no-undo.
def var v-ok          as log    no-undo.
Def Var v-Filespec    As Char   No-undo.
Def Var v-Filename    As Char   No-undo.
Def Var v-Filepath    As Char   No-undo.
Def Var lv-valid      As Log    No-undo.
def var lv-function   as char   no-undo.
def var lv-initvalue  as char   no-undo.
Def Var lv-extra      As Char   No-undo.
def var lv-lname      as char   no-undo.
def var lv-focus      as handle no-undo.
def var lv-returnvalue as char  no-undo.


if pv-wid ne '' then lv-focus = widget-handle(pv-wid).
                else lv-focus = focus.
                
def var lv-id as int no-undo.

def var lv-helpfile as char no-undo.
def var lv-helpref  as int  no-undo.
find first t-zen-fldlook no-error.

lv-id = int(GetLookupInfo(lv-focus,'tableid')).

find first t-zen-fldlook where t-zen-fldlook.zen-fldlooktableid = lv-id
                         no-lock no-error.
if avail t-zen-fldlook then do:
     case pv-mode:
         when 'help' then do:
             lv-helpfile = GetLookupInfo(lv-focus,'help').
             if not lv-helpfile begins '**' then do: /* widget specific help from zen-widlook */
                  lv-helpref  = int(entry(2,lv-helpfile,'{&delim2}')).
                  lv-helpfile = entry(1,lv-helpfile,'{&delim2}').
             end.
             else do: /* lookup Specific help fom zen-fldlook */
                  lv-helpfile = entry(1,t-zen-fldlook.help-file,'.').
                  lv-helpref  = t-zen-fldlook.Help-ref.
             end.
             lv-helpfile = search(lv-helpfile + '.' + getctrl("{&HelpExtension}")).
             if lv-helpfile ne ?
             then do:  
                 file-info:file-name = search(lv-helpfile).
                 if t-zen-fldlook.Help-ref ne 0
                     then system-help file-info:full-pathname context lv-helpref.
                     else system-help file-info:full-pathname contents.
                  return.
             end.
             else message msg(153,lv-helpfile,'','','') view-as alert-box.
         end.
         When 'look' then do:
             if valid-handle(lv-focus) then do:
                 Apply "entry" To lv-focus.
                 focus = lv-focus.
             end.
             If t-zen-fldlook.Look-pgm = "GetFile" 
             Then Do:
/*                 Run adecomm/_osprefx.p                           */
/*                          (v-Filespec                             */
/*                          ,output v-Filepath                      */
/*                          ,output v-filename).                    */
/*                lv-focus:screen-value = v-Filepath + v-filename.  */
              lv-focus:screen-value = getosfile(lv-focus:screen-value).
             End.
             Else Do:
                If search(t-zen-fldlook.Prog-Path + Look-pgm) ne ? or
                   search(t-zen-fldlook.Prog-Path + substring(t-zen-fldlook.look-pgm,1,length(t-zen-fldlook.look-pgm) - 1) + 'r') ne ?
                Then do:
                     lv-function = GetLookupInfo(lv-focus,'initial').
                     lv-initvalue = if lv-function ne '' and not lv-function begins '**' 
                                    then dynamic-function(lv-function,lv-focus:Screen-value,lv-focus,output lv-extra)
                                    else if stringtolog(GetNamedValue('useinitval',t-zen-fldlook.extra-details)) 
                                         then lv-focus:Screen-value else ' '. 
                       /* t-zen-fldlook.extra-details = 'useinitval' */ 
                     run value(t-zen-fldlook.Prog-Path + t-zen-fldlook.look-pgm)
                     (input Table t-zen-fldlook,t-zen-fldlook.zen-fldlooktableid,lv-initvalue).
                     find first t-zen-fldlook where t-zen-fldlook.zen-fldlooktableid = lv-id
                         no-lock no-error.
/*                     (input Table t-zen-fldlook,t-zen-fldlook.zen-fldlooktableid,lv-focus:Screen-value).  */
                end.
                else message msg(154,t-zen-fldlook.Prog-Path + t-zen-fldlook.look-pgm,'','','')
                     view-as alert-box error.
                lv-returnvalue = return-value.
             End.
             if valid-handle(lv-focus) then do:
                 Apply "entry" To lv-focus.
                 focus = lv-focus.
             end.
             if lv-returnvalue ne '{&Delim2}' then Do:
                If t-zen-fldlook.look-pgm = "GetFile" 
                Then Do:
                     Run adecomm/_osprefx.p
                         (v-Filespec
                         ,output v-Filepath
                         ,output v-filename).

                     assign
                         lv-focus:private-data = v-Filepath
                         lv-focus:screen-value = v-filename.
                End.
                Else Do:
                    Assign 
                        lv-focus:private-data = If t-zen-fldlook.ReturnOrder 
                                                Then Entry(2,lv-returnvalue,'{&Delim2}') 
                                                Else Entry(1,lv-returnvalue,'{&Delim2}')
                        lv-focus:private-data = (if Num-entries(lv-returnvalue,"{&Delim2}") = 3
                                                Then lv-focus:private-data + "{&Delim2}" + Entry(3,lv-returnvalue,'{&Delim2}')
                                                Else lv-focus:Private-data)
                        lv-focus:screen-value = If t-zen-fldlook.ReturnOrder 
                                                Then Entry(1,lv-returnvalue,'{&Delim2}') 
                                                Else Entry(2,lv-returnvalue,'{&Delim2}')
                    No-error.
                End.
                if valid-handle(lv-focus) then do:
                    Apply "leave" To lv-focus.
                end.
             End.
         end.
         when 'info' then do:
                if filenotfound(t-zen-fldlook.infoPath + t-zen-fldlook.infopgm)
                Then message msg(154,t-zen-fldlook.infoPath + t-zen-fldlook.infopgm,'','','')
                     view-as alert-box error.
                else if lv-focus:data-type ne 'date' then do:
                    if lv-focus:name begins 'zip' 
                    then run value(t-zen-fldlook.infoPath + t-zen-fldlook.infopgm)
                                (input Table t-zen-fldlook,t-zen-fldlook.zen-fldlooktableid,substring(lv-focus:Screen-value,1,5),'data').
                    else run value(t-zen-fldlook.infoPath + t-zen-fldlook.infopgm)
                    (input Table t-zen-fldlook,t-zen-fldlook.zen-fldlooktableid,lv-focus:Screen-value,'data').
                end.
         end.
         When 'Validate' then do:
/*               If lhvalidation:Get-signature(t-zen-fldlook.ValidationFunction) = ''  */
/*                   Then Return 'False{&Delim2}Validation Function Not Found'.                */
              /* function MUST be Defined with following definition
                 returns log. (input char,output char) */
              lv-function = GetLookupInfo(lv-focus,'validate').
              lv-valid = if lv-function ne '' and not lv-function begins '**' 
                         then dynamic-function(lv-function,lv-focus:Screen-value,lv-focus,output lv-extra)
                         else true.
              Return String(lv-valid) + '{&Delim2}' + lv-extra.
         end.
     end case.
end.
Else Do: /* "No Lookup Defined" */
    case pv-mode:
        when 'validate' then do:
        end.
        when 'Help' then do:
                Message msg(155,'Help','','','') View-as Alert-box.        
        end.
        when 'Look' or when 'info' then do:
            if lv-focus:type = 'fill-in' then
                Message msg(155,'Lookup/Info','For "' + lv-focus:name + '"','','') View-as Alert-box.
        end.       
    end case.
End.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-AltLanguage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AltLanguage Procedure 
FUNCTION AltLanguage RETURNS CHARACTER
  ( pv-string as char ) :
{{&core}bug.i} 
    RUN Proc-AltLanguage in THIS-PROCEDURE (input-output pv-string).
    return pv-string.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AnyErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AnyErrors Procedure 
FUNCTION AnyErrors RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
  {{&core}bug.i} 
  def var lv-ok as log no-undo.
  run Proc-AnyErrors  in this-procedure ('error',output lv-ok).
  return lv-ok.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AnyServerMessages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AnyServerMessages Procedure 
FUNCTION AnyServerMessages RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
  {{&core}bug.i} 
  def var lv-ok as log no-undo.
  run Proc-AnyErrors  in this-procedure ('message',output lv-ok).
  return lv-ok.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AttachMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AttachMenu Procedure 
FUNCTION AttachMenu RETURNS LOGICAL
  ( pv-window as handle,
    pv-frame  as handle,
    pv-procedure as handle ) :
    {{&core}bug.i} 
&if defined(UseZenMenus) ne 0 &then
  run Proc-AttachMenu (pv-window,pv-frame,pv-procedure).
&else
  attachsysmenu(pv-window).
&endif
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BtnHelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BtnHelp Procedure 
FUNCTION BtnHelp RETURNS CHARACTER
  ( pv-prog as handle,
    pv-on as log ) :
{{&core}bug.i} 
def var lv-helpfile as char no-undo.

if pv-prog = ? then
   find first t-zen-dpgm where t-zen-dpgm.pgm = "releaseHelpID" no-error.
else do:
   FIND FIRST t-zen-dpgm where t-zen-dpgm.pgm = dospath(pv-prog:name) NO-ERROR.  
   if not avail t-zen-dpgm then
      FIND FIRST t-zen-dpgm where t-zen-dpgm.pgm = unixpath(pv-prog:name) NO-ERROR.  
end.
if avail t-zen-dpgm then do:
   if t-zen-dpgm.help-file ne '' then do:
      lv-helpfile = search(entry(1,t-zen-dpgm.help-file,'.') + '.' + getctrl("{&HelpExtension}")).
        if lv-helpfile ne ?
        then do:  
            file-info:file-name = lv-helpfile.
            if pv-on then do:
               if t-zen-dpgm.Help-ref ne 0
                   then system-help file-info:full-pathname context t-zen-dpgm.Help-ref.
                   else system-help file-info:full-pathname contents.
            end.
            else system-help file-info:full-pathname quit.
            return ''.
        end. 
        else runchild ('{&core}about.w',pv-prog).  
    end.
    else runchild ('{&core}about.w',pv-prog).
end.
else runchild ('{&core}about.w',pv-prog).
return ''.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CanEdit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CanEdit Procedure 
FUNCTION CanEdit RETURNS LOGICAL
  ( pv-proc-name   as char ) :
  {{&core}bug.i} 
   def var lv-value as char no-undo.
   def var lv-ok as log no-undo.
   pv-proc-name = unixpath(pv-proc-name).
   run Proc-PgmProperties in THIS-PROCEDURE (pv-proc-name,'editable',output lv-value).
   lv-ok = lv-value = 'passed'.
   RETURN lv-ok.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CanFind) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CanFind Procedure 
FUNCTION CanFind RETURNS LOGICAL
  (pv-table as char,
   pv-where as char):
{{&core}bug.i} 
def var lv-data as char no-undo.


  lv-data = GetFieldWhere(pv-table,pv-where,pv-table + 'tableid').
  RETURN not (lv-data begins '** error').


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CanRun) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CanRun Procedure 
FUNCTION CanRun RETURNS LOGICAL
  ( pv-proc-name   as char ) :
  {{&core}bug.i} 
   def var lv-value as char no-undo.
   def var lv-ok as log no-undo.
   pv-proc-name = unixpath(pv-proc-name).
   
   run Proc-PgmProperties in THIS-PROCEDURE (pv-proc-name,'security',output lv-value).
   
   lv-ok = lv-value = 'passed'.
   if not lv-ok 
    then message msg(151,'','','','') 
           view-as alert-box title pv-proc-name.
   if not lv-ok 
   then do:
       /* override security if in dev mode to test programs */
       if GetCtrl('{&SystemMode}') = 'Dev'  
       then do:
        lv-ok = true.            
        message msg(152,'Dev Mode','Execution','Allowed',pv-proc-name) 
               view-as alert-box title pv-proc-name.
       end.
   end.

   RETURN lv-ok.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckForBackGroundErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckForBackGroundErrors Procedure 
FUNCTION CheckForBackGroundErrors RETURNS LOGICAL
  ( pv-prog as handle,
    pv-email as char ) :
    {{&core}bug.i} 
/*------------------------------------------------------------------------------
  Purpose:  check logfile created by pv-prog if exists email to pv-email
    Notes:  
------------------------------------------------------------------------------*/
   def var lv-text as char no-undo.
   def var lv-cc as char no-undo.
   def var irec as char no-undo.
   def var lv-scratch as char no-undo.
   def var lv-err as char no-undo.
   
   lv-logfile = GetLogFileName(pv-prog).
   lv-logfile = search(lv-logfile).
   
   if num-entries(pv-email,';') > 1
   then do:
    lv-cc = substring(pv-email,index(pv-email,';') + 1).
    pv-email = entry(1,pv-email,';').
   end.
   
   if lv-logfile ne ? then do:
        if pv-email ne '' then do:
            input stream zenlib from value(lv-logfile) no-echo.
            repeat:
               import stream zenlib unformatted irec.
               lv-text = lv-text + irec + chr(13).
            end.
            input stream zenlib close.
            
            WapiSendMail(GetCtrl('EmailMethod'),
                         GetCtrl('SystemEmailAddress'),
                         pv-email,
                         lv-cc,
                        'Error List',
                        lv-text,
                        '').
       end.
       os-delete Value(lv-logfile + '.previous').
       os-rename value(lv-logfile) Value(lv-logfile + '.previous').
       return true.
  end.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClassCodeDesc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClassCodeDesc Procedure 
FUNCTION ClassCodeDesc RETURNS CHARACTER
  ( pv-class as char,
    pv-code  as char ) :
{{&core}bug.i} 
    def var lv-desc as char no-undo.
    run Proc-ClassCodeDesc (pv-class,pv-code,output lv-desc).
    return lv-desc.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClassCodes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClassCodes Procedure 
FUNCTION ClassCodes RETURNS CHARACTER
  ( pv-class as char,
    output pv-desc  as char) :
 {{&core}bug.i} 
    def var lv-keys as char no-undo.
    run Proc-ClassCodes(pv-class,output pv-desc,output lv-keys).   
                                
    RETURN lv-keys.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CleanSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CleanSession Procedure 
FUNCTION CleanSession RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
{{&core}bug.i} 
    run Proc-CleanSession.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearAppserver) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearAppserver Procedure 
FUNCTION ClearAppserver RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
{{&core}bug.i} 
    run Proc-ClearAppserver.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateButs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateButs Procedure 
FUNCTION CreateButs RETURNS LOGICAL
  ( pv-butparams    as char ) :

  {{&core}bug.i} 
def var lv-butlist    as char no-undo.
def var lv-commonlist as char no-undo.
Def Var X As Int No-undo.

If Index(pv-butparams,'{&Delim2}') = 0 Then
       pv-butparams = GetCtrl('DefButList') + pv-butparams.

ASSIGN
    lv-butlist    = entry(1,pv-butparams,'{&Delim2}') 
    lv-commonlist = entry(2,pv-butparams,'{&Delim2}'). 
    
do x = 1 to num-entries(lv-butlist):
    entry(x,lv-butlist) = trim(entry(x,lv-butlist)).
end.
    
Do while Num-entries(lv-commonlist,',') < 10:
   lv-commonlist = lv-commonlist + ','. 
End.


X = 4.
Do X = 4 To 10:
    If string(entry(X,lv-commonlist,',')) = '' Then
    Case X:
        When 4  Then entry(X,lv-commonlist,',') = Getctrl('DefButHoriz').
        When 5  Then entry(X,lv-commonlist,',') = GetCtrl('DefButFlat').
        When 6  Then entry(X,lv-commonlist,',') = GetCtrl('DefButX').
        When 7  Then entry(X,lv-commonlist,',') = GetCtrl('DefButY').
        When 8  Then entry(X,lv-commonlist,',') = GetCtrl('DefButHeight').
        When 9  Then entry(X,lv-commonlist,',') = GetCtrl('DefButWidth').
        When 10 Then entry(X,lv-commonlist,',') = GetCtrl('CenterButs').
    End Case.
End.

  run Proc-CreateButs (lv-butlist,lv-commonlist).
  
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CtrlCounter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CtrlCounter Procedure 
FUNCTION CtrlCounter RETURNS INTEGER
  ( pv-idx as char,
    pv-data as int,
    pv-allownegative as log ) :
    {{&core}bug.i} 
   run Proc-Ctrlcounter in THIS-PROCEDURE (pv-idx,input-output pv-data,pv-allownegative).
   
   return pv-data.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DateSep) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DateSep Procedure 
FUNCTION DateSep RETURNS CHARACTER
    ( pv-lanid as int ) :
    def var lv-value as char no-undo.
  {{&core}bug.i}   
    run Proc-DateSep (pv-lanid,output lv-value).
    RETURN lv-value.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteAllSysVars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteAllSysVars Procedure 
FUNCTION DeleteAllSysVars RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
  {{&core}bug.i} 
   Run Proc-DeleteAllSysVars.
   Return True.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteSysVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteSysVar Procedure 
FUNCTION DeleteSysVar RETURNS LOGICAL
  ( pv-key As Char ) :
  {{&core}bug.i} 
  Run Proc-DeleteSysVar (pv-key).
RETURN True.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DispConnections) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DispConnections Procedure 
FUNCTION DispConnections RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
def var x as int no-undo.
{{&core}bug.i} 
for each w-appsrv:
    if valid-handle(w-handle) 
        then message w-name skip
                     w-handle:connected() skip
                     w-handle:client-connection-id
             view-as alert-box.
End.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DispExecMess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DispExecMess Procedure 
FUNCTION DispExecMess RETURNS CHARACTER
  ( pv-mess as char ) :
  {{&core}bug.i} 
if pv-mess = ? then pv-mess = ''.
if valid-handle(lv-execmesshandle) and lv-execmesshandle:visible
   then lv-execmesshandle:screen-value = pv-mess.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ErrorClear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ErrorClear Procedure 
FUNCTION ErrorClear RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
  {{&core}bug.i} 
    run Proc-ErrorClear ('error'). 
  RETURN true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ErrorCreate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ErrorCreate Procedure 
FUNCTION ErrorCreate RETURNS LOGICAL
  ( pv-code as int,
    pv-extra1 as char,
    pv-extra2 as char,
    pv-extra3 as char,
    pv-extra4 as char) :
    {{&core}bug.i} 
  run Proc-ErrorCreate(pv-code,'error',pv-extra1,pv-extra2,pv-extra3,pv-extra4).
  RETURN true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Execute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Execute Procedure 
FUNCTION Execute RETURNS LOGICAL
  (pv-prog   as char,
   pv-dir    as char,
   pv-params as char,
   pv-mode   as char ) :
   {{&core}bug.i} 
  def var h-instance as int no-undo.
  def var lv-mode as int no-undo.
  if pv-mode = '' then pv-mode = 'normal'.
  if not can-do('hidden,normal,min,max',pv-mode) then pv-mode = 'normal'.
  lv-mode = lookup(pv-mode,"hidden,normal,min,max",pv-mode).

  h-instance = WapiShellExecute(pv-prog,pv-params,pv-dir,lv-mode).

                   
  if h-instance >= 0 and 
     h-instance <=33 
  then do:
    WinErrorCodes (h-instance).
    return false.
  end.
    
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Fkey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Fkey Procedure 
FUNCTION Fkey RETURNS CHARACTER
  (pv-fromprocedure as handle) :
{{&core}bug.i} 
    run proc-fkey in this-procedure (pv-fromprocedure).

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ForceLocal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ForceLocal Procedure 
FUNCTION ForceLocal RETURNS LOGICAL
  ( pv-onoff as log ) :
  {{&core}bug.i} 
 lv-useappserver = pv-onoff.
  RETURN lv-useappserver.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FreezeWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FreezeWindow Procedure 
FUNCTION FreezeWindow RETURNS LOGICAL
  (   pv-window as handle,
      pv-onoff  as int ) :
      {{&core}bug.i} 
      WapiFreezeWindow(pv-window,pv-onoff).

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetApiDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetApiDetail Procedure 
FUNCTION GetApiDetail RETURNS LOGICAL
  (pv-apiname as char,
   output pv-properties as char,
   output pv-values as char) :
  {{&core}bug.i} 
  run Proc-GetApiDetail in this-procedure (pv-apiname,output pv-properties,output pv-values).
  
  RETURN return-value ne 'failed'.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAppserverHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetAppserverHandle Procedure 
FUNCTION GetAppserverHandle RETURNS HANDLE
  ( pv-name as char) :
{{&core}bug.i pv-name}
IF lv-useappserver 
THEN DO:
    if pv-name = 'system' 
    then do:
        pv-name = GetSystemName().
        pv-name = entry(1,GetIniValue(pv-name,'Appservers')).
    end.
    
    find first w-appsrv where w-name = pv-name no-error.
    if avail w-appsrv 
    then do:
        &if defined(MaximiseLicenses) ne 0 &then
            run CreateAppServer in this-procedure (w-param,output w-handle).
        &endif
        RETURN w-handle.
    End.
    else return ?.
end.
else return ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAppserverNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetAppserverNames Procedure 
FUNCTION GetAppserverNames RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  {{&core}bug.i} 
    RETURN lv-appservernames.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetBlobCtrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetBlobCtrl Procedure 
FUNCTION GetBlobCtrl RETURNS MEMPTR
  ( pv-idx as char,
    output lv-file as char ) :
    {{&core}bug.i} 
def var lv-blob as memptr no-undo.

   /* make sure there is nothing left over in the memory pointer! */
   set-size(lv-blob) = 0.
  run Proc-GetblobCtrl in THIS-PROCEDURE (pv-idx,output lv-blob,output lv-file).
  lv-file = getFullPath(lv-file). /* generallibrary.p */
  if lv-blob ne ? then do:
   lv-file = OutputToFile(lv-file,lv-blob,'local').
  end.
  return lv-blob.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetBlobCtrlRename) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetBlobCtrlRename Procedure 
FUNCTION GetBlobCtrlRename RETURNS MEMPTR (
   input pv-idx as char,
   input-output pv-file as char ) :
{{&core}bug.i} 
   /* same as getBlobCtrl, except this provides a param for the file name to 
      write it to, instead of just using the control-idx */

  def var lv-blob   as memptr     no-undo.
  def var temp-file as char  no-undo.

  run Proc-GetblobCtrl in THIS-PROCEDURE (
      input pv-idx,        /* blob control to get */
      output lv-blob,      /* memptr */
      output temp-file).   /* filename, which we won't use here */
  if lv-blob ne ? then do:
      pv-file = OutputToFile(pv-file,lv-blob,'local').
  end.
  return lv-blob.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetClientVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetClientVar Procedure 
FUNCTION GetClientVar RETURNS CHARACTER
  ( pv-varname as char ) :
/*------------------------------------------------------------------------------
  Purpose: return a session var ONLY stored on client 
    Notes:  
------------------------------------------------------------------------------*/
def var lv-value as char no-undo init '** Not Found'.

    find first w-clientvar where varname = pv-varname no-error.
    if avail w-clientvar then lv-value = varvalue.
/* message pv-varname skip lv-value {&dbt}. */
  RETURN lv-value.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetClientVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetClientVersion Procedure 
FUNCTION GetClientVersion RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
def var lv-version as dec decimals 2 no-undo.

lv-version = decimal(GetIniValue(lv-system,'version')).

  RETURN lv-version.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetColour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetColour Procedure 
FUNCTION GetColour RETURNS INTEGER
    (ip-colour-name     as char):
{{&core}bug.i} 
    def var lv-colour       as int      init 0          NO-UNDO.


    run proc-GetColour in this-procedure
            (input  ip-colour-name,
             output lv-colour).
if lv-colour = 0 then lv-colour = ?.
    return lv-colour.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetColumnHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetColumnHandle Procedure 
FUNCTION GetColumnHandle RETURNS HANDLE
  ( pv-browse as handle,
    pv-collabel as char ) :
{{&core}bug.i} 
def var lv-colhandle as handle no-undo.
lv-colhandle = pv-browse:first-column.
do while valid-handle(lv-colhandle):
    if lv-colhandle:label = pv-collabel
    then leave.
    lv-colhandle = lv-colhandle:next-column.
end.
  RETURN lv-colhandle.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetCtrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetCtrl Procedure 
FUNCTION GetCtrl RETURNS CHARACTER
  ( pv-idx as char ) :
  {{&core}bug.i} 
   run Proc-GetCtrl in THIS-PROCEDURE (pv-idx).
   return return-value.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetCurrency) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetCurrency Procedure 
FUNCTION GetCurrency RETURNS CHARACTER
    (ip-country     as int):
{{&core}bug.i} 
    def var lv-currency     as char     init ""         NO-UNDO.

    run proc-getcurrency in this-procedure
            (input  ip-country,
             output lv-currency).

    return lv-currency.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetField Procedure 
FUNCTION GetField RETURNS CHARACTER
  (pv-table as char,
   pv-keyfield as char,
   pv-keydata  as char,
   pv-datafield as char):
{{&core}bug.i} 
def var lv-where as char no-undo.
lv-where = pv-keyfield + " = '" + pv-keydata + "'".

return GetFieldWhere(pv-table,lv-where,pv-datafield).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFieldWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldWhere Procedure 
FUNCTION GetFieldWhere RETURNS CHARACTER
  (pv-table as char,
   pv-where as char,
   pv-datafield as char):
{{&core}bug.i} 
def var lv-data as char no-undo.

  run Proc-GetFieldWhere in this-procedure
        (pv-table,pv-where,pv-datafield,output lv-data).

  RETURN lv-data. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFkeyProperty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFkeyProperty Procedure 
FUNCTION GetFkeyProperty RETURNS CHARACTER
  ( pv-mode as char,
    pv-key as char,
    pv-pgm as char ,
    pv-user as char ,
    pv-property as char ) :

/*  pv-mode is a valid keyfield on zen-dfkey
    pv-key is the value of the pv-mode field
    pv-property is the field to return
there are four different scenarios for the hot keys.
   1 = both user and program are specific/lists
   2 = user is an asterisk and program is specific/list
   3 = user is specific/list and program is an asterisk
   4 = both user and program have an asterisk 
*/

def var lv-where    as char   no-undo.
def var lv-value as char no-undo.
def var x as int no-undo.

do x = 1 to 4:
    case x:
        when 1 then lv-where = pv-mode + ' = "' + pv-key +
                               '" And in-prog ne "*" and can-do(in-prog,"'  + pv-pgm   + '")' +
                               '  And forusers ne "*" and can-do(forusers,"' + pv-user + '")'.
    
        when 2 then lv-where = pv-mode + ' = "' + pv-key +
                               '" And in-prog ne "*" and can-do(in-prog,"'  + pv-pgm   + '")' +
                               '  And forusers = "*"'.
        
        when 3 then lv-where = pv-mode + ' = "' + pv-key +
                               '" And  in-prog = "*"' +
                               '  And forusers ne "*" and can-do(forusers,"' + pv-user + '")'.

        when 4 then lv-where = pv-mode + ' = "' + pv-key +
                               '" And can-do(in-prog,"'  + pv-pgm   + '")' +
                               '  And can-do(forusers,"' + pv-user + '")'.
    end case.
    
    lv-value = getfieldwhere("zen-dfkey",
                              lv-where,
                              pv-property).

   if not lv-value begins "**" and lv-value ne "" then leave.
end.


  RETURN lv-value.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLogFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetLogFileName Procedure 
FUNCTION GetLogFileName RETURNS CHARACTER
  ( pv-prog as handle ) :
  {{&core}bug.i} 
/*------------------------------------------------------------------------------
  Purpose:  create a standard log filename
    Notes:  
------------------------------------------------------------------------------*/
   def var lv-logfile as char no-undo.
   lv-logfile = unixpath(pv-prog:name).
   lv-logfile = entry(num-entries(lv-logfile,'/'),lv-logfile,'/').
   lv-logfile = '{&logs}' + entry(1,lv-logfile,'.') + '.elog'.

  RETURN lv-logfile.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLookupInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetLookupInfo Procedure 
FUNCTION GetLookupInfo RETURNS CHARACTER
  ( pv-focus as handle,
    pv-mode as char) :
{{&core}bug.i} 
def var v-wid         as char   no-undo Initial ''.
Def Var v-PrivateData As Char   No-undo.
Def Var v-fhand       As Handle No-undo.
Def Var v-whand       as Handle No-undo.
def var lv-lookupname as char no-undo.
def var v-frame as char no-undo.

 assign v-wid    = if pv-focus:table ne ? then pv-focus:table + '.' else ''.
        v-wid    = v-wid + pv-focus:name.
       
find first t-zen-widlook where t-zen-widlook.In-Program = 'resettoblank'
                         no-lock no-error.
                     
if pv-mode begins 'browse'
then do:
   assign v-privatedata = substring(pv-mode,7)
          v-frame = 'browse'.
   find first t-zen-widlook where t-zen-widlook.In-Program = v-privatedata
                             And v-frame matches t-zen-widlook.In-Frame
                             And t-zen-widlook.look-field = v-wid
                            no-lock no-error.      
    if not avail t-zen-widlook
    then find first t-zen-widlook where can-do(t-zen-widlook.In-Program,v-privatedata)
                                 And v-frame matches t-zen-widlook.In-Frame
                                 And t-zen-widlook.look-field = v-wid
                                no-lock no-error.                                                 
    if not available t-zen-widlook
     or not can-do(t-zen-widlook.In-frame,'browse')
    then return ?.
end.
else do:
   if not can-query(pv-focus,'frame')
   then assign v-FHAND  = pv-Focus:parent:Frame
               v-whand  = pv-Focus:parent:Window no-error.
   else assign v-FHAND  = pv-Focus:Frame
               v-whand  = pv-Focus:Window no-error.
   
   if error-status:error then return ?.

   assign
       v-frame  = v-fhand:name
       v-PrivateData = /* If V-FHAND:type = "dialog-box" THEN */ unixPath(V-fHAND:private-data)
/*                                                      else unixPath(V-wHAND:private-data) */
       v-privatedata = entry(num-entries(v-PrivateData,'/'),v-PrivateData,'/').
end.
/* if pv-mode = 'tableid' then v-wid = pv-focus:name. */

/* ?? GetWidId(v-privatedata,v-fhand,v-wid). ?? */
if not avail t-zen-widlook
then find first t-zen-widlook where can-do(t-zen-widlook.In-Program,v-privatedata)
                          And v-frame Matches t-zen-widlook.In-Frame
                          And t-zen-widlook.look-field = v-wid
                         no-lock no-error.

if not avail t-zen-widlook
then find first t-zen-widlook where can-do(t-zen-widlook.In-Program,v-privatedata)
                                And v-frame Matches t-zen-widlook.In-Frame
                                And v-wid matches t-zen-widlook.look-field
                              no-lock no-error.

case pv-mode:
/*    when 'by' then */
/*        if avail t-zen-widlook then return t-Zen-widlook.byclause. */
/*                               else return '** No By Defined'. */
   when  'name' then
       if avail t-zen-widlook then return entry(1,t-Zen-widlook.LookupName,'|').
                              else return '** No Lookup Defined'.
   when 'initial' then
       if avail t-zen-widlook then return t-Zen-widlook.InitialValueFunction.
                              else return '** No Initial Value Function Defined'.
   
   when 'help' then
       if avail t-zen-widlook then return  t-zen-widlook.help-file + '{&delim2}' + string(t-Zen-widlook.help-ref).
                              else return '** No Help Defined'.
   when 'validate' then
       if avail t-zen-widlook then return t-Zen-widlook.ValidationFunction.
                              else return '** No Validation Defined'.
end case.

lv-lookupname = 'nolookupdefined'.

if avail t-zen-widlook
then do:
  if pv-focus:data-type = 'date' and 
     pv-mode begins 'browse'
  then lv-lookupname = entry(2,t-Zen-widlook.LookupName,'|') no-error.
  else lv-lookupname = entry(1,t-Zen-widlook.LookupName,'|') no-error.
end.

Find First t-zen-fldlook where t-zen-fldlook.lookupname = lv-lookupname 
                              no-error.

if avail t-zen-fldlook then return string(t-zen-fldlook.zen-fldlooktableid).
                       else return ?. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetOsFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetOsFile Procedure 
FUNCTION GetOsFile RETURNS CHARACTER
  ( pv-filename as char ) :
{{&core}bug.i} 
  run Proc-GetOsFIle(input-output pv-filename)  .

  RETURN pv-filename.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetParentHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetParentHandle Procedure 
FUNCTION GetParentHandle RETURNS HANDLE
  ( pv-child as handle) :
  {{&core}bug.i} 
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
def var lv-parent as handle no-undo.

  run Proc-GetParentHandle in this-procedure (pv-child,output lv-parent).

  RETURN lv-parent.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetProcHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetProcHandle Procedure 
FUNCTION GetProcHandle RETURNS HANDLE
  (pv-appsrv as char,
   pv-proc as char) :
   {{&core}bug.i} 
def var lv-phandle as handle no-undo.
  run Proc-GetProcHandle in this-procedure (pv-appsrv,pv-proc,output lv-phandle).
  RETURN lv-phandle.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetProperty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetProperty Procedure 
FUNCTION GetProperty RETURNS CHARACTER
  (pv-type as char,
   pv-parent as char,
   pv-property as char):
{{&core}bug.i} 

def var lv-data as char no-undo.
def var lv-where as char no-undo.

lv-where = ' type = "' + pv-type + 
           '" and parent = "' + pv-parent +
           '" and name  = "' + pv-property + '"'.

  run Proc-GetFieldWhere in this-procedure
        ('zen-property',lv-where,'PropertyValue',output lv-data).

  RETURN lv-data. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetRegEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetRegEntry Procedure 
FUNCTION GetRegEntry returns char
 (pv-hkey as char,
  pv-base as char,
  pv-section as char,
  pv-item as char):
                    {{&core}bug.i}  
/*
GetRegEntry("hkcu",
            "system\currentcontrolset\control",
            "timezoneInformation",
            'ActiveTimeBias').
*/
    def var lv-hkey as char no-undo.
    def var pv-return as char no-undo.
    case pv-hkey:
        when 'hkcr' then lv-hkey = 'hkey_classes_root'.
        when 'hkcu' then lv-hkey = 'hkey_current_user'.
        when 'hklm' then lv-hkey = 'hkey_local_machine'.
        when 'hku'  then lv-hkey = 'hkey_users'.
        when 'hkcc' then lv-hkey = 'hkey_current_config'.
        otherwise lv-hkey = 'hkey_local_machine'.
    end case.                 

    load pv-base base-key lv-hkey no-error.
    use pv-base no-error.
    GET-KEY-VALUE SECTION pv-section
                  KEY pv-item
                  VALUE pv-return.
    unload pv-base.
    return pv-return.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetScratchName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetScratchName Procedure 
FUNCTION GetScratchName RETURNS CHARACTER
  ( pv-extension as char,
    pv-fullpath as log) :
/*------------------------------------------------------------------------------
  Purpose: create unique scratchfilename in valid temp directory 
    Notes:  
------------------------------------------------------------------------------*/
def var x as int no-undo.
def var lv-fname as char no-undo.

lv-fname = replace(sessionid(),'/','').
lv-fname = replace(lv-fname,':','').
lv-fname = getctrl('{&ScratchPath}') + 
           getuserid() + 
           replace(lv-fname,' ','').
if pv-extension ne '' 
then lv-fname = lv-fname + 
    if index(pv-extension,'.') = 0 
    then '.' + pv-extension
    else pv-extension.
if pv-fullpath then do:
    if touch(lv-fname) then
    do:
        lv-fname = getfullpath(lv-fname).
        os-delete value(lv-fname).
    end.
    else return error.
end.
  RETURN lv-fname.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSysVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetSysVar Procedure 
FUNCTION GetSysVar RETURNS CHARACTER
  ( pv-key As Char ) :
  {{&core}bug.i} 
   Def Var lv-value As Char No-undo.
  
   if pv-key begins '{&clv}'
   then lv-value = GetClientVar(pv-key).
   else do:   
    run Proc-GetSysVar(pv-key,output lv-value).
 
    if lv-value begins '** system reset' and 
       lv-useappserver 
    then do:
       message
          'System appears to have been reset.' skip
          'You must login again to continue.' skip skip
          'System Information' skip
          'Variable ' pv-key skip
          'Session ' sessionid() skip 
          'Maximise Licenses ' &if defined(MaximiseLicenses) ne 0 &then 'yes' &else 'No' &endif skip 
          'System Name ' GetSystemName() skip 
          'Session id ' lv-sessionid skip
          'Use Appserver ' lv-useappserver skip
          'User ' lv-user skip
          'Session param ' session:parameter
          view-as alert-box information title Lv-Value.
       cleansession().
       quit.
    end.
   end.
   
   Return lv-value.       

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUserid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetUserid Procedure 
FUNCTION GetUserid RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
{{&core}bug.i} 
if session:remote then lv-user = getsysvar('user').

  RETURN lv-user.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetWidgetProperty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetWidgetProperty Procedure 
FUNCTION GetWidgetProperty RETURNS CHARACTER
  ( pv-pgm as char,
    pv-frame as char,
    pv-widname as char,
    pv-property as char) :
    {{&core}bug.i} 
def var lv-value as char no-undo.
def var lv-where as char no-undo.
lv-where =  ' pgm = ' + '"' + pv-pgm + '"' + 
            ' and widframe matches "' + pv-frame + '"' +
            ' and wid-name = "' + pv-widname + '"' +
            ' and property = "' + pv-property + '"'.

lv-value = getfieldwhere('zen-widgetproperty',
                          lv-where,
                          'propertyvalue').
if lv-value begins '** error'
 then lv-value = ''. 
 
 RETURN lv-value.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HadErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HadErrors Procedure 
FUNCTION HadErrors RETURNS LOGICAL
  ( ) :
  {{&core}bug.i} 
  def var lv-haderr as log no-undo.
  run Proc-HadErrors ('error',output lv-haderr).
  return lv-haderr. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitLibraries) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitLibraries Procedure 
FUNCTION InitLibraries RETURNS LOGICAL
  ( pv-user as char) :
  {{&core}bug.i} 
  lv-user    = pv-user.

  RUN initialise-libraries in this-procedure.

assign
    lv-group   = usergroup(pv-user)
    lv-country = string(UserCountry(pv-user)).

/*
GetSysVar('System'),    
GetSysVar('UserGroup'), 
GetSysVar('User')
*/
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadFieldDefaults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadFieldDefaults Procedure 
FUNCTION LoadFieldDefaults RETURNS LOGICAL
  (pv-prog as handle,
   pv-frame as handle ) :
   {{&core}bug.i} 
if not can-find(first t-Zen-FieldDefault where 
   t-Zen-FieldDefault.duser       =      lv-user and 
   t-Zen-FieldDefault.ProgramName begins pv-prog:name)
then return false.

def var lv-from as char no-undo.
def var lv-wid   as handle no-undo.
def var lv-extra as char   no-undo.
def var lv-prog as char no-undo.
def var lv-found as log no-undo.


run GetCalledFrom in pv-prog (output lv-from) no-error.
/* message '1#' lv-from '#' {&dbt}. */
    
if num-entries(lv-from,'{&delim4}') > 1
then lv-from = '{&delim4}' + entry(2,lv-from,'{&delim4}').
lv-from = pv-prog:name + lv-from.

/* 10/09/08 AMD */
if lv-from = ? then lv-from = pv-prog:name.

/* message '2#' lv-from '#' {&dbt}. */

assign
   lv-wid = pv-frame:first-child
   lv-wid = lv-wid:first-child.
   lv-prog = lv-from.
   
do while valid-handle(lv-wid):
   if lv-wid:private-data ne 'excludefromdefaults'
   then do:
      if (can-query(lv-wid,'context-help-id') and
         lv-wid:context-help-id ne {&ExcludeFromDefaultsID})
         or lv-wid:type = 'frame'
      then do:
         if lv-wid:type = 'frame' then do:
            loadfielddefaults(pv-prog,lv-wid).
         end.
         else do:
            if can-set(lv-wid,'screen-value') and
               lv-wid:type ne 'literal'
            then do: /*  t-Zen-FieldDefault.duser  */
               lv-prog = lv-from.
/*      message "3 " skip */
/*                   '=' lv-user skip */
/*                   '=' lv-prog skip */
/*                   '=' pv-frame:name skip */
/*                   '=' string(lv-wid:table) skip */
/*                   '=' lv-wid:name skip */
/*                   '=' lv-wid:index {&dbt}. */                               
               find first t-Zen-FieldDefault where 
                  t-Zen-FieldDefault.duser       = lv-user              and 
                  t-Zen-FieldDefault.ProgramName = lv-prog              and 
                  t-Zen-FieldDefault.FrameName   = pv-frame:name        and 
                  t-Zen-FieldDefault.tablename   = string(lv-wid:table) and 
                  t-Zen-FieldDefault.WidgetName  = lv-wid:name          and 
                  t-Zen-FieldDefault.WidExtent   = lv-wid:index         
                  no-lock no-error.
               if not avail t-Zen-FieldDefault then do:
                  if index(lv-from,'{&delim4}') ne 0 
                  then do:
                     lv-prog = substring(lv-from,1,r-index(lv-from,'{&delim4}')).
                     find first t-Zen-FieldDefault where t-Zen-FieldDefault.duser       = lv-user
                                                     and t-Zen-FieldDefault.ProgramName = lv-prog 
                                                     and t-Zen-FieldDefault.FrameName   = pv-frame:name
                                                     and t-Zen-FieldDefault.tablename   = string(lv-wid:table)
                                                     and t-Zen-FieldDefault.WidgetName  = lv-wid:name
                                                     and t-Zen-FieldDefault.WidExtent   = lv-wid:index
                                                   no-lock no-error.
                  end.
               end.
               if not avail t-Zen-FieldDefault then do:
/*      message "3a " skip */
/*              '=' lv-user skip */
/*              'begins' lv-prog skip */
/*              '=' pv-frame:name skip */
/*              '=' string(lv-wid:table) skip */
/*              '=' lv-wid:name skip */
/*              '=' lv-wid:index {&dbt}. */
                  find first t-Zen-FieldDefault where 
                     t-Zen-FieldDefault.duser       = lv-user              and 
                     t-Zen-FieldDefault.ProgramName begins lv-prog              and 
                     t-Zen-FieldDefault.FrameName   = pv-frame:name        and 
                     t-Zen-FieldDefault.tablename   = string(lv-wid:table) and 
                     t-Zen-FieldDefault.WidgetName  = lv-wid:name          and 
                     t-Zen-FieldDefault.WidExtent   = lv-wid:index         
                  no-lock no-error.

               end.
               if avail t-zen-fielddefault then do:
                  lv-found = true.
                  if t-zen-FieldDefault.DefaultValue begins 'UseFunctionValue'
                  then lv-wid:screen-value = dynamic-function(t-Zen-FieldDefault.PopulationRoutine,
                                                              t-Zen-FieldDefault.DefaultValue,
                                                              lv-wid,
                                                              output lv-extra).
                  else do:
                     if can-do('selection-list',lv-wid:type) and
                        not substring(string(lv-wid:context-help-id),5,1) = "8"
                      then do:
                          if can-set(lv-wid,'list-items') then lv-wid:list-items = t-Zen-FieldDefault.DefaultValue.
                                                          else lv-wid:list-item-pairs = t-Zen-FieldDefault.DefaultValue.
                      end.
                      else
                     lv-wid:screen-value = t-Zen-FieldDefault.DefaultValue.
                  end.

                  /* 07/14/09 EKS, widget absolutely needed to set the
                     private-data (selection list where the order things
                     were selected mattered, so order was stored in
                     private-data).  As such, the field default was set
                     to the private-data (corresponding logic added to the
                     appropriate procedure in generallibrary.p) which will
                     work the same as setting it to the screen-value, but here,
                     I can set the private-data to the correct value */
                  if substring(string(lv-wid:context-help-id),6,1) = "8" then
                     lv-wid:private-data = t-zen-fieldDefault.defaultValue.

                  /* 07/14/09 EKS, need to run a trigger for some fields before
                     default values can be set for other fields.
                     apply a "leave" or "value-changed" trigger to the widget */
                  if substring(string(lv-wid:context-help-id),4,1) = "8"
                  then case lv-wid:data-type:
                     when "fill-in" then apply "leave" to lv-wid.
                     otherwise apply "value-changed" to lv-wid.
                  end case.
/*     message "4 " skip */
/*            t-Zen-FieldDefault.duser skip */
/*            t-Zen-FieldDefault.ProgramName skip */
/*            t-Zen-FieldDefault.FrameName skip */
/*            t-Zen-FieldDefault.tablename skip */
/*            t-Zen-FieldDefault.WidgetName t-Zen-FieldDefault.Widextent skip */
/*            t-Zen-FieldDefault.DefaultValue {&dbt}. */
               end.
            end.
         end.
      end.
   end.
   lv-wid = lv-wid:next-sibling.
end.
return lv-found.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LogAction Procedure 
FUNCTION LogAction RETURNS LOGICAL
  ( pv-prog as char ,
    pv-action as char,
    pv-msg as  char) :
    
{{&core}bug.i} 
    case lv-activitylog:
        when 'file' then do:
            output stream zenlib to value("{&logs}" + lv-user + "Activity.log") append.
            put stream zenlib unformatted
                    string(today,'99/99/9999') ',' string(time,'hh:mm:ss') ','
                    pv-prog ',' pv-action ',' pv-msg skip.
            output stream zenlib close.
        end.
        when 'db' then run proc-log('Activity',pv-prog,pv-action,pv-msg,'').
        otherwise.
    end case.
    
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogAppserver) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LogAppserver Procedure 
FUNCTION LogAppserver RETURNS LOGICAL
  (pv-msg as  char,
   pv-logfile as char,
   pv-override as char) :
   
   /*pv-prog as char ,
    pv-action as char,
    pv-msg as  */
    
{{&core}bug.i} 

def var lv-origlog as char no-undo.
lv-origlog = lv-logappserver.

if lv-logappserver ne 'no' or pv-override ne ''
then do:
    if pv-override ne '' then lv-logappserver = pv-override.
    case lv-logappserver:
        when 'file' then do:
            output stream zenlib to value("{&logs}" + lv-user + "Appserver.log") append.
            put stream zenlib unformatted
                      string(today,'99/99/9999') ',' string(time,'hh:mm:ss') ',' pv-msg skip.
            output stream zenlib close. 
        end.
        when 'db' then run proc-log('Appserver',entry(1,pv-msg),entry(2,pv-msg),entry(3,pv-msg),'').
    end case.
end.
lv-logappserver = lv-origlog.

  RETURN true.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LogMessage Procedure 
FUNCTION LogMessage RETURNS LOGICAL
  (pv-msg as  char,
   pv-logfile as char,
   pv-override as char) :
{{&core}bug.i} 

def var lv-origlog as char no-undo.
lv-origlog = lv-logmessages.

if lv-logmessages ne 'no' or pv-override ne '' 
then do:
  if pv-override ne '' then lv-logmessages = pv-override.
  case lv-logmessages:
    when 'file' then do:
      if pv-logfile = '' then pv-logfile = "{&logs}" + "ErrorMessage.log".
      
      output stream zenlib to value(pv-logfile) append.
      
      put stream zenlib unformatted
            string(today,'99/99/9999') ',' string(time,'hh:mm:ss') ','
            lv-user ',' pv-msg skip.
      
      output stream zenlib close.
    end.
    when 'DB' then run proc-log('ErrorMessage',program-name(2),pv-override,pv-msg,'').
    end case.
end.
lv-logmessages = lv-origlog.

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MakeButton) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MakeButton Procedure 
FUNCTION MakeButton RETURNS HANDLE
  ( pv-parentproc as handle,
    pv-name      as char,
    pv-frame     as handle,
    pv-sensitive as log,
    pv-flat      as log,
    pv-label     as char,
    pv-width     as dec,
    pv-height    as dec,
    pv-row       as dec,
    pv-col       as dec,
    pv-help      as char,
    pv-visible   as log,
    pv-icon      as char) :
{{&core}bug.i "pv-name + ' begin'"} 

def var h-but      as handle no-undo.
def var lv-icon as char no-undo.
def var lv-trigger as char   no-undo.
def var lv-icon-insensitive as char no-undo.
def var lv-butaction as char no-undo.
def var lv-where as char no-undo.
def var lv-menulist as char no-undo.
def var lv-fkeyhelp as char no-undo.

if pv-name = 'skip' then
    assign pv-visible = false
           pv-sensitive = false.
assign 
    lv-trigger = pv-name + '-trigger'
    pv-label   = if pv-label = '' 
                    then '&' + pv-name
                    else pv-label.
if index(pv-label,'&') = 0 then pv-label = '&' + pv-label.  
  
if pv-name begins 's-'
then do:  
    assign pv-icon = if pv-icon = pv-name then substring(pv-name,3) else pv-icon
           pv-name = substring(pv-name,3)
           lv-where = "'" + lv-user + "' matches duser 
                        and shortcut begins '" + pv-name + "'"
           lv-butaction = getfieldwhere("zen-f-hotkey",
                                         lv-where,
                                         "f-program") 
           pv-help      = getfieldwhere("zen-f-hotkey",
                                         lv-where,
                                         "f-desc") 
           lv-trigger   = 'usrbut-trigger'
           pv-visible   = lv-butaction ne ''
           pv-sensitive = pv-visible.
    if lv-butaction begins '**' 
        then assign pv-help = 'Shortcut ' + pv-name + ' Not Defined'
                    lv-butaction = 'Not Defined'
                    pv-sensitive = false.
   else if num-entries(lv-butaction,':') ne 3
        then do:
               lv-menulist = pgmmenuparent(entry(1,lv-butaction,':')).
               if lv-menulist = '' 
               then lv-butaction = lv-butaction + ':' + 
                                   'btn-' + pv-name + ':button'.
               else do:
                   lv-menulist = entry(2,entry(num-entries(lv-menulist,','),lv-menulist,','),'{&delim3}').
                   lv-butaction = lv-butaction + ':' + lv-menulist + ':menu-item'.
               end.
        end.
end.
else do:
    lv-fkeyhelp = GetFkeyProperty('action',lv-trigger,pv-parentproc:name,lv-user,'fkey'). 
    if not lv-fkeyhelp begins '**' 
    then pv-help = pv-help + ' (' + Caps(lv-fkeyhelp) + ')'.
end.

create BUTTON h-but 
  assign
     name          = 'btn-' + pv-name
     frame         = pv-frame
     NO-FOCUS      = false
     tab-stop      = false     
     sensitive     = pv-sensitive
     FLAT-BUTTON   = pv-flat 
     LABEL         = pv-label
     Width-pixels  = pv-width 
     Height-pixels = pv-height 
     TOOLTIP       = pv-help
     Y             = pv-row 
     X             = pv-col 
     HELP          = pv-help
     visible       = pv-visible
     private-data  = lv-butaction
     TRIGGERS:
         on entry persistent run SetLastFocus in pv-parentproc.
         ON CHOOSE persistent run value(lv-trigger) in pv-parentproc.
     END TRIGGERs.

assign
   lv-icon             = lc('{&core}{&bmp}' + pv-icon + '-su.ico')
   lv-icon-insensitive = lc('{&core}{&bmp}' + pv-icon + '-si.ico').


/* standard image */
if search(lv-icon) ne ? then 
    h-but:load-image(lv-icon) no-error.  
else do:
    lv-icon = lc('{&ico}' + pv-icon + '-su.ico').
    if search(lv-icon) ne ? then 
        h-but:load-image(lv-icon) no-error.  
end.

/* insensitive */
if search(lv-icon-insensitive) ne ? then
   h-but:load-image-insensitive(lv-icon-insensitive) no-error.
else do:
    lv-icon-insensitive = lc('{&ico}' + pv-icon + '-si.ico').
    if search(lv-icon-insensitive) ne ? then 
        h-but:load-image-insensitive(lv-icon-insensitive) no-error.  
end.


/* else h-but:width-chars = length(h-but:label).  */
{{&core}bug.i "pv-name + ' end'"} 

RETURN h-but.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MakeLookupButtons) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MakeLookupButtons Procedure 
FUNCTION MakeLookupButtons RETURNS LOGICAL
  (pv-proc as handle,
   pv-frame as handle ) :
   {{&core}bug.i} 
/*------------------------------------------------------------------------------
  Purpose: create buttons next to fields with lookups  
    Notes:  
------------------------------------------------------------------------------*/
def var lv-wid as  handle no-undo.
def var h-but as handle no-undo.
def var lv-id as int no-undo.

lv-wid = pv-frame:first-child.
lv-wid = lv-wid:first-child.
def var v-wid as char no-undo.

def var lv-graphic as char   no-undo.
if pv-frame:private-data = ? then pv-frame:private-data = '*'.
do while valid-handle(lv-wid):
   if lv-wid:type = 'frame' 
   then makelookupbuttons(pv-proc,lv-wid).
   else do:
      if lv-wid:type = 'fill-in' 
      then do:
         assign v-wid    = if lv-wid:table ne ? then lv-wid:table + '.' else ''.
                v-wid    = v-wid + lv-wid:name.
         if can-find(first t-zen-widlook where can-do(t-zen-widlook.In-Program,pv-proc:file-name)
                            And pv-frame:name Matches t-zen-widlook.in-frame
                            And v-wid matches t-zen-widlook.look-field)
                      
/*          lv-id = int(GetLookupInfo(lv-wid,'tableid')).  */
/*          if lv-id ne ?                                  */
         then do:
            if lv-uselookupbuttons 
            then do:
               if pv-frame:width-pixels < dec(lv-wid:x + lv-wid:width-pixels + 1) + 20
                     then pv-frame:width-pixels = pv-frame:width-pixels + 21.
               create BUTTON h-but 
               assign
                  name          = 'btn-lkup'
                  frame         = pv-frame
                  private-data  = string(lv-wid) /* tie button to widget */
                  NO-FOCUS      = lv-wid:private-data ne 'focus'    /*false for diag-look.i */
                  sensitive     = true
                  FLAT-BUTTON   = false
                  LABEL         = 'Lkup'
                  Width-pixels  = 20.0 
                  Height-pixels = 20.0 
                  TOOLTIP       = 'Lookup'
                  Y             = dec(lv-wid:y) 
                  X             = dec(lv-wid:x + lv-wid:width-pixels + 1) 
                  HELP          = 'Lookup'
                  visible       = false
                  TRIGGERS:
                    on entry  persistent run SetLastFocus in pv-proc.
                    ON CHOOSE persistent run proc-wlook in this-procedure.
                  END TRIGGERS.
               /*                 lv-wid:bgcolor = lv-lookupcolour. */
               lv-wid:private-data = string(h-but). /* tie widget to button */
               if lv-wid:data-type = 'date' 
               then lv-graphic = '{&core}{&bmp}cal-su.ico'.
               else lv-graphic = '{&core}{&bmp}lkup-su.ico'.
               h-but:load-image(lv-graphic) no-error.  
               /* else h-but:width-chars = length(h-but:label). */
            end.
            else lv-wid:bgcolor = lv-lookupcolour.
         end.
      end.
   end.
   lv-wid = lv-wid:next-sibling.
end.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MaxDataGuess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MaxDataGuess Procedure 
FUNCTION MaxDataGuess RETURNS INTEGER
  ( pv-pgm as char ) :
  {{&core}bug.i} 
    def var lv-value as char no-undo.
    run Proc-PgmProperties in THIS-PROCEDURE (pv-pgm,'MaxDataGuess',output lv-value).    
    RETURN int(lv-value).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MenuLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MenuLabel Procedure 
FUNCTION MenuLabel RETURNS LOGICAL
  (pv-hand as handle ) :
{{&core}bug.i} 
def var v-hand   as handle no-undo.

if pv-hand:type = 'window' 
    then if valid-handle(pv-hand:menu-bar) 
         then pv-hand = pv-hand:menu-bar.   
         else return true.

v-hand = pv-hand:first-child. 
do while valid-handle(v-hand):
    if v-hand:type = 'sub-menu' 
        then menulabel(v-hand).
    if can-set(v-hand,'label') 
        then v-hand:label = AltLanguage(replace(v-hand:label,'&','')).
    v-hand = v-hand:next-sibling.
end.
return true.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MenuMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MenuMsg Procedure 
FUNCTION MenuMsg RETURNS HANDLE
  ( pv-text as char ) :

  def var lv-menubar as handle no-undo.
  def var lv-win as handle no-undo.
  def var lv-menuitem as handle no-undo.

  lv-win = widget-handle(getsysvar("{&clv}top-win")).
  lv-menubar = lv-win:MENUBAR.
  
  if pv-text = 'off' then do:
    lv-menuitem  = lv-menubar:last-child.
    delete widget lv-menuitem.
    lv-menuitem = ?.
  end.
  else do:
       CREATE SUB-MENU lv-menuitem
       ASSIGN PARENT = lv-menubar
               LABEL = fill(' ',5) + pv-text.

  end.
  RETURN lv-menuitem.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MenuOptionFrom) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MenuOptionFrom Procedure 
FUNCTION MenuOptionFrom RETURNS CHARACTER
  ( pv-prog as handle,
    pv-mode as char) :
    {{&core}bug.i} 
/*------------------------------------------------------------------------------
  Purpose:  To return the text or id  of the menu option that called current program.
------------------------------------------------------------------------------*/
def var lv-from as handle no-undo.
def var lv-data as char   no-undo.
def var lv-type as char   no-undo.
def var lv-wid  as handle no-undo.
def var lv-id   as int    no-undo.

run GetCalledFrom in pv-prog (output lv-data) no-error.
do while num-entries(lv-data,':') < 3:
   lv-data = lv-data + ':'.
end.

lv-type = entry(3,lv-data,':').

if entry(1,lv-data,':') = '' 
 then entry(1,lv-data,':') = pv-prog:name.

case lv-type:
   when 'menu-item' or 
   when 'sub-menu' or 
   when 'button' or 
   when 'server' /* ???? dont know how ew get a type of server but we do !! */
   then do:
      lv-id = int(entry(2,lv-data,':')).
   end.
/*    when 'button' then do: */
/*    */
/*       lv-wid = widget-handle(entry(2,lv-data,':')) no-error. */
/*    */
/*       if valid-handle(lv-wid) and num-entries(lv-wid:private-data,":") ge 2 */
/*          then lv-id = int(entry(2,lv-wid:private-data,':')). */
/*       else lv-id = 0. */
/*    end. */
   otherwise lv-id = 0.
end case.

case pv-mode:
   when 'name' then do:
      find t-zen-dmenu where t-zen-dmenu.menu-id = lv-id
                       no-lock no-error.
      lv-data = if avail t-zen-dmenu then t-zen-dmenu.menu-name else lv-data.
   end.
   when 'id'     then lv-data = string(lv-id).
   when 'type'   then lv-data = lv-type.
   when 'handle' then lv-data = string(lv-wid).
   otherwise lv-data = lv-data.
end case.

/* message program-name(1) skip */
/*         program-name(2) skip */
/*        pv-prog:name skip     */
/*        pv-mode skip          */
/*        lv-data skip          */
/* lv-type skip                 */
/* lv-id  skip                  */
/* asc(substring(lv-data,1,1)). */

/* An & is part of the menu name as it allows the user to press the following
   letter plus the alt key to quickly jump to the menu option.  Consequently,
   this causes the & to be added to lv-data, and we must strip it out. */
lv-data = replace(lv-data,chr(38),"").

return lv-data.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Msg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Msg Procedure 
FUNCTION Msg RETURNS CHARACTER
    (pv-msgid as int,
     pv-extra1 as char,
     pv-extra2 as char,
     pv-extra3 as char,
     pv-extra4 as char) :
{{&core}bug.i} 
     def var lv-mestxt as char no-undo.
     
     run GetMessage in THIS-PROCEDURE (pv-msgid,pv-extra1,pv-extra2,pv-extra3,pv-extra4,output lv-mestxt).

return lv-mestxt.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmAuthor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PgmAuthor Procedure 
FUNCTION PgmAuthor RETURNS CHARACTER
    ( pv-pgm as char ) :
  {{&core}bug.i} 
    def var lv-value as char no-undo.
    run Proc-PgmProperties in THIS-PROCEDURE (pv-pgm,'Author',output lv-value).    
    RETURN lv-value.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmComments) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PgmComments Procedure 
FUNCTION PgmComments RETURNS CHARACTER
  ( pv-pgm as char) :  
  {{&core}bug.i} 
    def var lv-value as char no-undo.
    run Proc-PgmProperties in THIS-PROCEDURE (pv-pgm,'comments',output lv-value).    
    RETURN lv-value.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PgmId Procedure 
FUNCTION PgmId RETURNS DECIMAL
  ( pv-pgm as char) :
  {{&core}bug.i} 
   def var lv-value as char no-undo.
   run Proc-PgmProperties in THIS-PROCEDURE (pv-pgm,'Id',output lv-value).
   RETURN Dec(lv-value).
  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmMenuGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PgmMenuGroup Procedure 
FUNCTION PgmMenuGroup RETURNS CHARACTER
    ( pv-pgm as char ) :
  {{&core}bug.i} 
    def var lv-value as char no-undo.
    run Proc-PgmProperties in THIS-PROCEDURE (pv-pgm,'Menu',output lv-value).    
    RETURN lv-value.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmMenuParent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PgmMenuParent Procedure 
FUNCTION PgmMenuParent RETURNS CHARACTER
  ( pv-prog as char ) :
  {{&core}bug.i} 
def var lv-list as char no-undo.
def var lv-id  as char no-undo.
def var lv-backslash as char no-undo.

&if "{&OPSYS}" = "unix" 
    &then lv-backslash = "\\".
    &else lv-backslash = "~\".
&endif

  find first t-zen-dmenu where t-zen-dmenu.menu-action = pv-prog
                  no-lock no-error.
  if avail t-zen-dmenu then do:  
     lv-id = string(t-zen-dmenu.menu-id).
     if index(pv-prog,'/') ne 0 or
        index(pv-prog,lv-backslash) ne 0 then pv-prog = t-zen-dmenu.menu-name.
     if t-zen-dmenu.menu-parent ne '' 
     then do:
         lv-list = pgmmenuparent(t-zen-dmenu.menu-parent) + ',' + 
                   pv-prog + '{&delim3}' + lv-id.
     end.
     else lv-list = pv-prog + '{&delim3}' + lv-id + lv-list.
  end.

  RETURN lv-list.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmMultiinstance) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PgmMultiinstance Procedure 
FUNCTION PgmMultiinstance RETURNS LOGICAL
    ( pv-pgm as char ) :
  {{&core}bug.i} 
    def var lv-value as char no-undo.
    run Proc-PgmProperties in THIS-PROCEDURE (pv-pgm,'Multiinstance',output lv-value).    
    RETURN StringTOlog(lv-value).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PgmName Procedure 
FUNCTION PgmName RETURNS CHARACTER
  ( pv-pgmid as dec) :
  {{&core}bug.i} 
   def var lv-value as char no-undo.
   run Proc-PgmProperties in THIS-PROCEDURE (string(pv-pgmid),'Name',output lv-value).
   RETURN lv-value.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmProperty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PgmProperty Procedure 
FUNCTION PgmProperty RETURNS CHARACTER
    ( pv-pgm as char,
      pv-property as char ) :
  {{&core}bug.i} 
    def var lv-value as char no-undo.
    run Proc-PgmProperties in THIS-PROCEDURE (pv-pgm,pv-property,output lv-value).    
    RETURN lv-value.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmRepInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PgmRepInfo Procedure 
FUNCTION PgmRepInfo RETURNS CHARACTER
    ( pv-pgm as char ) :
  {{&core}bug.i} 
    def var lv-value as char no-undo.
    run Proc-PgmProperties in THIS-PROCEDURE (pv-pgm,'repinfo',output lv-value).    
    RETURN lv-value.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PgmUseDefaults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PgmUseDefaults Procedure 
FUNCTION PgmUseDefaults RETURNS LOGICAL
    ( pv-pgm as handle) :
  {{&core}bug.i} 
    def var lv-value as char no-undo.
    run Proc-PgmProperties in THIS-PROCEDURE (pv-pgm:name,'UseDefaults',output lv-value).    
    RETURN StringTOlog(lv-value).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProgramDescription) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ProgramDescription Procedure 
FUNCTION ProgramDescription RETURNS CHARACTER
  ( pv-pgm as char) :  
  {{&core}bug.i} 
    def var lv-value as char no-undo.
    run Proc-PgmProperties in THIS-PROCEDURE (pv-pgm,'description',output lv-value).    
    RETURN lv-value.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProgramTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ProgramTitle Procedure 
FUNCTION ProgramTitle RETURNS CHARACTER
  ( pv-pgm as char,
    pv-mode as char) :  
{{&core}bug.i} 
    def var lv-value as char no-undo.
    def var lv-pgm   as char no-undo.
    def var lv-wid as handle no-undo.
    def var lv-id  as int no-undo.

case pv-mode:
   when 'program' then do:
       lv-wid = widget-handle(pv-pgm) no-error.
       if not error-status:error
          then lv-pgm = lv-wid:name.
          else lv-pgm = pv-pgm.
       run Proc-PgmProperties in THIS-PROCEDURE (lv-pgm,'title',output lv-value).    
   end.
   when 'programname' then do:
       run Proc-PgmProperties in THIS-PROCEDURE (lv-pgm,'title',output lv-value).    
   end.
   when 'menu' then do:
       lv-id = int(pv-pgm) no-error.
       if not error-status:error
       then do:
         lv-value = getfield('zen-dmenu','menu-id',pv-pgm,'menu-name').
         if lv-value begins '** Error' 
         then lv-value = 'Menu id ' + string(lv-id) + ' not found'.
/*       find t-zen-dmenu where t-zen-dmenu.menu-id = lv-id
                          no-lock no-error.
         lv-value = if avail t-zen-dmenu then t-zen-dmenu.menu-name else 'Menu id ' + string(lv-id) + ' not found'.
*/     end.
       else lv-value = pv-pgm + ' Invalid'.
   end.
   when 'menuhandle' then do:
       lv-wid = widget-handle(pv-pgm) no-error.
       if not error-status:error
       then do:
         lv-value = getfield('zen-dmenu','menu-id',entry(1,lv-wid:private-data,'|'),'menu-name').
         if lv-value begins '** Error' 
         then lv-value = lv-wid:private-data + ' Invalid'.
/*         find t-zen-dmenu where t-zen-dmenu.menu-id = int(entry(1,lv-wid:private-data,'|'))
                          no-lock no-error.
         lv-value = if avail t-zen-dmenu then t-zen-dmenu.menu-name else lv-wid:private-data + ' Invalid'.
*/
       end.
       else lv-value = pv-pgm.
   end.
end case.


    RETURN lv-value.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RefreshTempTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RefreshTempTables Procedure 
FUNCTION RefreshTempTables RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
  {{&core}bug.i} 
    run PopulateTempTables in this-procedure no-error.   
/*     RefreshSysTempTables().  */
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReLabel Procedure 
FUNCTION ReLabel RETURNS LOGICAL
  ( pv-handle as handle,
    pv-mode as char ) :
{{&core}bug.i} 
    run proc-ReLabel(pv-handle,pv-mode).

  RETURN true.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RgbColour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RgbColour Procedure 
FUNCTION RgbColour RETURNS CHARACTER
  ( pv-name AS CHAR) :
 {{&core}bug.i} 
    def var o-rgb as char no-undo.
    run Proc-RgbColour (pv-name,output o-rgb).
    RETURN o-rgb.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RunRemote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RunRemote Procedure 
FUNCTION RunRemote RETURNS LOGICAL
  ( pv-params as char ) :
  {{&core}bug.i} 
 def var lv-remote as log no-undo.
 
  if session:remote or
     opsys = 'unix' or
     not lv-useappserver or
     indevmode()
    then lv-remote = false.
    else if entry(1,session:parameter,'^') ne "local"  
            then lv-remote = true.
            else lv-remote = false.

if lv-remote and lv-logappserver ne 'no' then
 logappserver(string(program-name(4)) + ',' +
              string(program-name(3)) + ',' +
              string(program-name(2)),
              '',''). 
              
/*  message program-name(4) skip */
/*          program-name(3) skip */
/*          program-name(2) skip */
/*          entry(1,pv-params,':') skip */
/*          entry(2,pv-params,':') skip */
/*          entry(3,pv-params,':') */
/*  {&dbt}.              */
 return lv-remote.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ScreenChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ScreenChanged Procedure 
FUNCTION ScreenChanged RETURNS LOGICAL
      ( pv-frame as handle):
     {{&core}bug.i} 
     return framechanged(pv-frame).  /* in generallibrary.p */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SecurityCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SecurityCheck Procedure 
FUNCTION SecurityCheck RETURNS LOGICAL
  (pv-user as char,
   pv-group as char,
   pv-notusers as char,
   pv-notgroups as char,
   pv-okusers as char,
   pv-okgroups as char) :
  {{&core}bug.i} 
  def var lv-return as log no-undo.
  def var lv-value as char no-undo.
    
  run proc-securitycheck (pv-user,pv-group,pv-notusers,
                          pv-notgroups,pv-okusers,pv-okgroups,
                          output lv-value).
  
    lv-return = lv-value = 'passed'.
    
/* if not lv-return Then do:
        message 'Security Failed For' Skip
                 'pv-user ' In ' pv-Group Skip.
                 lv-value skip
                 'Using Alogorithm ' lv-securityAlogorithm
                 view-as alert-box Information.
 */
    RETURN lv-return.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ServerMessageCreate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ServerMessageCreate Procedure 
FUNCTION ServerMessageCreate RETURNS LOGICAL
  ( pv-code as int,
    pv-extra1 as char,
    pv-extra2 as char,
    pv-extra3 as char,
    pv-extra4 as char) :
    {{&core}bug.i} 
  run Proc-ErrorCreate(pv-code,'message',pv-extra1,pv-extra2,pv-extra3,pv-extra4).
  RETURN true.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ServerMessagesClear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ServerMessagesClear Procedure 
FUNCTION ServerMessagesClear RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
  {{&core}bug.i} 
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    run Proc-ErrorClear ('message'). 
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SessionId Procedure 
FUNCTION SessionId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
def var lv-id as char no-undo.
{{&core}bug.i program-name(2)}
IF SESSION:REMOTE 
    THEN lv-id = SESSION:server-connection-context.
    ELSE do:
        find first w-appsrv where w-name = GetSystemName() no-error.
        if avail w-appsrv 
            then lv-id = w-sessionid. 
            else lv-id = string(today) + ':' + lv-uniqueid.
    End.

  RETURN lv-id.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetBgColour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetBgColour Procedure 
FUNCTION SetBgColour RETURNS LOGICAL
  ( pv-frame as handle,
    pv-wl as char ,
    pv-colour as char) :
    {{&core}bug.i} 
def var lv-wid-handle  as handle no-undo.
def var lv-wname       as char   no-undo.
def var lv-disbgcolour as int    no-undo.
def var lv-wtype       as char   no-undo 
         init 'fill-in,editor,combo-box,selection-list,radio-set,toggle-box'.


lv-disbgcolour = getcolour(pv-colour).

assign lv-wid-handle = pv-frame
       lv-wid-handle = lv-wid-handle:first-child
       lv-wid-handle = lv-wid-handle:first-child.

do while valid-handle(lv-wid-handle):
    if lv-wid-handle:type = 'frame' then 
       setbgcolour(lv-wid-handle,pv-wl,pv-colour).
    else do:
        if can-do(lv-wtype,lv-wid-handle:type) then do:
            if not (lv-wid-handle:type = 'fill-in' and
               valid-handle(widget-handle(lv-wid-handle:private-data)))
            then do:
                lv-wname = if lv-wid-handle:table ne ? then lv-wid-handle:table + '.' else ''.
                lv-wname = lv-wname + lv-wid-handle:name.
                if can-query(lv-wid-handle,'index')
                then if lv-wid-handle:index > 0 
                     then lv-wname = lv-wname + '[' + string(lv-wid-handle:index) + ']'.
                if can-do(pv-wl,lv-wname)
                then do:
                    lv-wid-handle:bgcolor = lv-disbgcolour.
                end.
            end.
        end.
    end.
    lv-wid-handle = lv-wid-handle:NEXT-sibling.
end.


  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetClientVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetClientVar Procedure 
FUNCTION SetClientVar RETURNS CHARACTER
  ( pv-varname as char,
    pv-value as char ) :
/*------------------------------------------------------------------------------
  Purpose: set a session var ONLY stored on client 
    Notes:  
------------------------------------------------------------------------------*/
    if not pv-varname begins "{&clv}" 
    then pv-varname = "{&clv}" + pv-varname. 
    
    find first w-clientvar where varname = pv-varname no-error.
    
    if not avail w-clientvar 
    then do:
        create w-clientvar.
        w-clientvar.varname = pv-varname.
    end.
    w-clientvar.varvalue = pv-value.
  RETURN ''.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetCtrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetCtrl Procedure 
FUNCTION SetCtrl RETURNS LOGICAL
  ( pv-idx as char,
    pv-data as char ) :
    {{&core}bug.i} 
   run Proc-SetCtrl in THIS-PROCEDURE (pv-idx,pv-data).
   
   return return-value ne 'failed'.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetExecMessHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetExecMessHandle Procedure 
FUNCTION SetExecMessHandle RETURNS CHARACTER
  ( pv-hand as handle ) :
{{&core}bug.i} 
lv-execmesshandle = pv-hand.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSensitive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSensitive Procedure 
FUNCTION SetSensitive RETURNS LOGICAL
  ( pv-enable       as log ,
    pv-ix           as char,
    pv-exc-list     as char,
    pv-frame-handle as handle ) :
    {{&core}bug.i} 
   run Proc-SetSensitive(pv-enable,pv-ix,pv-exc-list,pv-frame-handle).

RETURN true.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSessionLangFormats) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSessionLangFormats Procedure 
FUNCTION SetSessionLangFormats RETURNS LOGICAL
  ( pv-lanid as int) :
{{&core}bug.i} 
  run Proc-SetSessionLangFormats in this-procedure (pv-lanid).

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSystem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSystem Procedure 
FUNCTION SetSystem RETURNS LOGICAL
  ( pv-system as char  ) :
  {{&core}bug.i} 
    lv-system = pv-system.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSysVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSysVar Procedure 
FUNCTION SetSysVar RETURNS LOGICAL
  ( pv-key As Char,
    pv-value As Char ) :
{{&core}bug.i} 
  run Proc-SetSysVar(pv-key,pv-value).
RETURN True.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetTableAudit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetTableAudit Procedure 
FUNCTION SetTableAudit RETURNS CHARACTER
  ( pv-tablelist as char,
    pv-state as char) :
{{&core}bug.i} 
def var x as int no-undo.

do x = 1 to num-entries(pv-tablelist):
    if pv-state ne 'off' 
    then DeleteSysVar('DoNotAudit' + entry(x,pv-tablelist)).
    else SetSysVar('DoNotAudit' + entry(x,pv-tablelist),'true').
end.

  RETURN string(x).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetUsrid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetUsrid Procedure 
FUNCTION SetUsrid RETURNS LOGICAL
  ( pv-user as char ) :
  {{&core}bug.i} 
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 lv-user = pv-user.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetUsrPwd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetUsrPwd Procedure 
FUNCTION SetUsrPwd RETURNS LOGICAL
  ( pv-usr As char,
    pv-pwd As char) :
    {{&core}bug.i} 
/*------------------------------------------------------------------------------
  Purpose:  SET UP VARS IN LIB FOR USERID PWD FOR APPSERVER CONNECT
    Notes:  
------------------------------------------------------------------------------*/
  Assign
        lv-user = pv-usr
        lv-password = Encode(pv-pwd).
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetWinPosition) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetWinPosition Procedure 
FUNCTION SetWinPosition RETURNS LOGICAL
  ( pv-win-handle as handle,
    pv-pgm        as char,
    pv-xadjust as int,
    pv-yadjust as int ) :
    {{&core}bug.i} 
/*------------------------------------------------------------------------------
  Purpose:   set window position centered or not 
------------------------------------------------------------------------------*/
    pv-pgm = unixpath(pv-pgm).
    pv-pgm = entry(num-entries(pv-pgm,'/'),pv-pgm,'/').
    if can-do('{&AlwaysCenterList}',pv-pgm) 
    then do:                              
        centerwindow(pv-win-handle). 
        return true.
     end.
                                         
    def var x as int no-undo.
    def var y as int no-undo.
    def var x-offset as int no-undo.
    def var y-offset as int no-undo.
    def var lv-value as char no-undo.

    def var lv-mainwin as handle no-undo.                                
                                                                       
    lv-mainwin = widget-handle(getsysvar("{&clv}top-win")) no-error.                    
                                                                         
    if valid-handle(lv-mainwin) then do:                                 
        assign                                                           
               x-offset = if lv-mainwin:x ne ? then lv-mainwin:x else 0  
               y-offset = if lv-mainwin:y ne ? then lv-mainwin:y else 0. 
    end.                                                                                                  
    else do:
        centerwindow(pv-win-handle). 
        return true.
    end.
    run Proc-PgmProperties in THIS-PROCEDURE (pv-pgm,'position',output lv-value).

    if lv-value ne '?' 
        then assign x = int(entry(1,lv-value,'{&Delim2}'))
                    y = int(entry(2,lv-value,'{&Delim2}')).
if y = -1 
then y = ((lv-mainwin:height-pixels - pv-win-handle:height-pixels) / 2) + 2.
if x = -1 
then x = ((lv-mainwin:width-pixels - pv-win-handle:width-pixels) / 2) + 2.   
    if x > -1 or y > -1 
    then assign 
                
    
    x = x-offset + x - pv-xadjust
                y = y-offset + y - pv-yadjust
                pv-win-handle:x = x
                pv-win-handle:y = y.
    else iF GetCtrl("{&centre-windows}") = 'yes' 
         then centerwindow(pv-win-handle). 
            
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetWorkingDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetWorkingDir Procedure 
FUNCTION SetWorkingDir RETURNS LOGICAL
  ( pv-dir as char ) :
  {{&core}bug.i} 
RETURN WapiSetCurrentDirectory(pv-dir) ne 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Sound) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Sound Procedure 
FUNCTION Sound RETURNS LOGICAL
  ( pv-file as char ) :
  {{&core}bug.i} 
  run proc-Sound (pv-file).
  RETURN true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SpellCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SpellCheck Procedure 
FUNCTION SpellCheck RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  {{&core}bug.i} 
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    run proc-SpellCheck in this-procedure.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SysMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SysMsg Procedure 
FUNCTION SysMsg RETURNS LOGICAL
  ( pv-msg as char ) :
  {{&core}bug.i} 
  run proc-sysmsg (pv-msg).
  return return-value = "true".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SystemManager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SystemManager Procedure 
FUNCTION SystemManager RETURNS LOGICAL
    ( pv-user as char ) :
    {{&core}bug.i} 
    def var lv-id as char no-undo.
    run Proc-UserProperties in THIS-PROCEDURE (pv-user,'SystemManager',output lv-id).
    return lv-id = 'yes'.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TabLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabLabel Procedure 
FUNCTION TabLabel RETURNS LOGICAL
  ( pv-chand as com-handle,
    pv-hand as handle ) :
{{&core}bug.i} 
if int(GetSysVar("language")) = 0 then return true.
   def var h-tab      as com-handle no-undo.
   def var x          as int        no-undo init 1.

if pv-chand ne ? then do:

   do x = 1 to 10:
       h-tab = pv-chand:tabs(X) no-error.
       IF not valid-handle(h-tab) THEN return true.
       h-tab:caption = AltLanguage(h-tab:caption).
   end.
end.
Else do:
/*   use this way if you send in a normal handle */
/*   def input param pv-hand as handle no-undo. */

   def var h-frame    as com-handle no-undo.
   def var h-control  as com-handle no-undo.
   def var h-tabstrip as com-handle no-undo.

    h-frame = pv-hand:com-handle no-error.
    h-control = h-frame:controls.
    h-tabstrip = h-control:item(1).

   do x = 1 to 10:
       h-tab = h-tabstrip:tabs(X) no-error.
       IF not valid-handle(h-tab) THEN LEAVE.
       h-tab:caption = AltLanguage(h-tab:caption).
   end.
end.

  RETURN true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Tooltip) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Tooltip Procedure 
FUNCTION Tooltip RETURNS LOGICAL
  ( pv-frame as handle,
    pv-changefont as log ) :
{{&core}bug.i} 
 run proc-tooltip (pv-frame,pv-changefont).

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserAutoTimeclock) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UserAutoTimeclock Procedure 
FUNCTION UserAutoTimeclock RETURNS LOGICAL
    ( pv-user as char ) :
    {{&core}bug.i} 
    def var lv-id as char no-undo.
    run Proc-UserProperties in THIS-PROCEDURE (pv-user,'AutoTimeClock',output lv-id).
    return stringtolog(lv-id).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserCountry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UserCountry Procedure 
FUNCTION UserCountry RETURNS CHARACTER
    ( pv-user as char ) :
    {{&core}bug.i} 
    def var lv-id as char no-undo.
    run Proc-UserProperties in THIS-PROCEDURE (pv-user,'Country',output lv-id).
    return lv-id.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UserGroup Procedure 
FUNCTION UserGroup RETURNS CHARACTER
    ( pv-user as char ) :
    {{&core}bug.i} 
    def var lv-id as char no-undo.
    run Proc-UserProperties in THIS-PROCEDURE (pv-user,'Group',output lv-id).
    return lv-id.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserLanguage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UserLanguage Procedure 
FUNCTION UserLanguage RETURNS INTEGER
    ( pv-user as char ) :
    def var lv-id as char no-undo.
    {{&core}bug.i} 
    run Proc-UserProperties in THIS-PROCEDURE (pv-user,'Language',output lv-id).
    return int(lv-id).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UserName Procedure 
FUNCTION UserName RETURNS CHARACTER
    ( pv-user as char ) :
    {{&core}bug.i} 
    def var lv-id as char no-undo.
    run Proc-UserProperties in THIS-PROCEDURE (pv-user,'Name',output lv-id).
    return lv-id.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidApi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidApi Procedure 
FUNCTION ValidApi RETURNS LOGICAL
  ( pv-name as char) :
  {{&core}bug.i} 
  def var lv-value as char no-undo.
    
  run Proc-PgmProperties in THIS-PROCEDURE (pv-name,'ValidApi',output lv-value).      
  RETURN lv-value = 'passed'.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidClassCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidClassCode Procedure 
FUNCTION ValidClassCode RETURNS LOGICAL
  ( pv-class as char,
    pv-code  as char, 
    OUTPUT lv-desc AS CHAR) :
{{&core}bug.i} 
    def var lv-ok as log no-undo.
    run Proc-ValidClassCode (output lv-ok,pv-class,pv-code,output lv-desc).
    return lv-ok.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidDate Procedure 
FUNCTION ValidDate RETURNS LOGICAL
    ( pv-date As Char ) :
    {{&core}bug.i} 
    Def Var lv-date As Date No-undo.
    lv-date = Date(pv-date) No-error.
    if not error-status:error 
     then do:
         pv-date = replace(pv-date,'/','').
         if length(pv-date) > 8 then lv-date = ?. 
     end.

    RETURN Not lv-date = ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidScreenValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidScreenValue Procedure 
FUNCTION ValidScreenValue RETURNS LOGICAL
  (Output pv-extras As Char) :
  Def Var lv-ok As Log No-undo.
{{&core}bug.i} 
  run widget-help in THIS-PROCEDURE ('Validate','').
  
  Assign
    lv-ok     = StringToLog(Entry(1,Return-value,'{&Delim2}'))
    pv-extras = Entry(2,Return-value,'{&Delim2}') no-error.
    
    if not lv-ok then do:
        if pv-extras begins 'Validation' then do:
         if not session:remote 
            then message pv-extras view-as alert-box.
        end.
    end.
  RETURN lv-ok.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidUser Procedure 
FUNCTION ValidUser RETURNS LOGICAL
  ( lv-user as char ,
    lv-pass as char ) :
    {{&core}bug.i} 
    def var lv-ok as log no-undo.
    run Proc-ValidUser in this-procedure (lv-user,lv-pass,output lv-ok).
    return lv-ok.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Whelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Whelp Procedure 
FUNCTION Whelp RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  {{&core}bug.i} 
    run widget-help in THIS-PROCEDURE ('help','').
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidgetInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WidgetInfo Procedure 
FUNCTION WidgetInfo RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
{{&core}bug.i} 
    run widget-help in this-procedure ('info','').
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidSecCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WidSecCheck Procedure 
FUNCTION WidSecCheck RETURNS LOGICAL
  (pv-fhand as handle,pv-progname as char) :
{{&core}bug.i} 
        RUN Proc-WidSecCheck in this-procedure (pv-fhand,pv-progname).
  RETURN true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

