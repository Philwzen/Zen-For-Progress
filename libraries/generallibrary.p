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
  ------------------------`----------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
create widget-pool.
/* ***************************  Definitions  ************************** */
this-procedure:private-data = "library-general".
&glob library-general
&glob library-program

{app-paths.i}
{{&core}{&libraries}calcUtils.i}
def stream ini.
def var lv-path     as char no-undo.
def var lv-ininame  as char no-undo.
def var lv-user as char no-undo.
lv-path = entry(2,session:param,'^') no-error.
if error-status:error then lv-path = '{&ini-path}'.

lv-ininame = entry(3,session:param,'^') no-error.
if error-status:error then lv-ininame = '{&ini-filename}'.

if lv-path = '' then lv-path = '{&ini-path}'.
if lv-ininame = '' then lv-ininame = '{&ini-filename}'.

lv-ininame = lv-ininame + '.ini'.

{{&core}def-table.i &table-name = zen-fielddefault}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-AddLastSlash) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddLastSlash Procedure 
FUNCTION AddLastSlash returns character
  ( input directoryName as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AuditedField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AuditedField Procedure 
FUNCTION AuditedField returns logical
  ( pv-table as char,
    pv-field as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BackUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BackUp Procedure 
FUNCTION BackUp returns logical
  (pv-from as char,
   pv-to as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildCombo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BuildCombo Procedure 
FUNCTION BuildCombo returns character
  (pv-combo as handle,
   pv-table as char,
   pv-key   as char,
   pv-field as char,
   pv-where as char,
   pv-by    as char,
   pv-none  as log,
   pv-wild  as log) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CenterWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CenterWindow Procedure 
FUNCTION CenterWindow returns logical
  ( pv-win-handle as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CharTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CharTime Procedure 
FUNCTION CharTime returns character
  ( pv-time       as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-cleanupQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cleanupQuery Procedure 
FUNCTION cleanupQuery returns logical
        ( input hQuery as handle ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvPcl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvPcl Procedure 
FUNCTION ConvPcl returns character
  ( pv-ipfile as char,
    pv-type as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateFile Procedure 
FUNCTION CreateFile returns character
  ( pv-file as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DateInWords) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DateInWords Procedure 
FUNCTION DateInWords returns character
    (ip-date            as date,
     ip-long-name       as log) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DayName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DayName Procedure 
FUNCTION DayName returns character
    (ip-date        as date) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DirectoryNotFound) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DirectoryNotFound Procedure 
FUNCTION DirectoryNotFound returns logical
  ( pv-fname as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoNotFire) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoNotFire Procedure 
FUNCTION DoNotFire returns logical
  ( PV-WIDLIST as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DosPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DosPath Procedure 
FUNCTION DosPath returns character
  ( pv-string as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExecHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExecHandle Procedure 
FUNCTION ExecHandle returns handle
  ( pv-appsrv as char,
    pv-path as char,
    pv-prog as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExportBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExportBrowse Procedure 
FUNCTION ExportBrowse returns logical
  ( pv-handle as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FileNotFound) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FileNotFound Procedure 
FUNCTION FileNotFound returns logical
  ( pv-fname as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FindFirstFrame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FindFirstFrame Procedure 
FUNCTION FindFirstFrame returns widget-handle
  (pv-prog as handle,
   pv-frame as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixedString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixedString Procedure 
FUNCTION FixedString returns character
 ( pv-str as char,
   pv-allownumeric as log )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixPath Procedure 
FUNCTION FixPath returns character
  ( pv-path as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FrameChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FrameChanged Procedure 
FUNCTION FrameChanged returns logical
  ( pv-frame as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetAttribute Procedure 
FUNCTION GetAttribute returns character
  ( h-wid    as handle,
    pv-param as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetButPos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetButPos Procedure 
FUNCTION GetButPos returns character
  ( pv-frame as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetComboKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetComboKey Procedure 
FUNCTION GetComboKey returns character
 (pv-handle as widget-handle) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetDlcBin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetDlcBin Procedure 
FUNCTION GetDlcBin returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetExcelColumnName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetExcelColumnName Procedure 
FUNCTION GetExcelColumnName returns character
  ( lv-col as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFieldValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue Procedure 
FUNCTION GetFieldValue returns character
  ( pv-buffer as handle,
    pv-fieldname as char,
    pv-extent as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFileName Procedure 
FUNCTION GetFileName returns character
  ( pv-FullPath as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFullPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFullPath Procedure 
FUNCTION GetFullPath returns character
  ( pv-file as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetHdr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetHdr Procedure 
FUNCTION GetHdr returns character
  ( pv-handle as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetIniValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetIniValue Procedure 
FUNCTION GetIniValue returns character
  ( input pv-section as char,
    input pv-key as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLockingCulprit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetLockingCulprit Procedure 
FUNCTION GetLockingCulprit returns character
  (tableRecid    as recid)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetNamedValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetNamedValue Procedure 
FUNCTION GetNamedValue returns character
  ( pv-name as char,
    pv-list as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetServerValueFor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetServerValueFor Procedure 
FUNCTION GetServerValueFor returns character
  ( pv-var as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetStringEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetStringEntry Procedure 
FUNCTION GetStringEntry returns character
 ( pv-item as char,
   pv-properties as char,
   pv-values as char,
   pv-delim as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSystemName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetSystemName Procedure 
FUNCTION GetSystemName returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetWidHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetWidHandle Procedure 
FUNCTION GetWidHandle returns handle
  ( pv-frame       as handle,
    pv-widname     as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InDevMode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InDevMode Procedure 
FUNCTION InDevMode returns logical
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InputFromFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InputFromFile Procedure 
FUNCTION InputFromFile returns memptr
  ( pv-filename as char,
    pv-local as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IntegerTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD IntegerTime Procedure 
FUNCTION IntegerTime returns integer
  ( pv-time as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IntToHex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD IntToHex Procedure 
FUNCTION IntToHex returns character
(input i as int) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsInteger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD IsInteger Procedure 
FUNCTION IsInteger returns logical
      (pv-Value as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsNull) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD IsNull Procedure 
FUNCTION IsNull returns logical
    ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsNumeric) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD IsNumeric Procedure 
FUNCTION IsNumeric returns logical
    ( pv-Value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD IsRunning Procedure 
FUNCTION IsRunning returns logical
  (input pv-proc as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Jumpto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Jumpto Procedure 
FUNCTION Jumpto returns handle
  ( pv-widname as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDay Procedure 
FUNCTION LastDay returns integer
    (lv-date as date) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadDefBackGround) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadDefBackGround Procedure 
FUNCTION LoadDefBackGround returns logical
  ( pv-name  as char,
    pv-frame as handle,
    pv-win   as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogicalAnd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LogicalAnd Procedure 
FUNCTION LogicalAnd returns integer
    (pi1 as int, 
     pi2 as int ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MonthName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MonthName Procedure 
FUNCTION MonthName returns character
    (ip-date        as date) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NumRecords) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NumRecords Procedure 
FUNCTION NumRecords returns integer
  (pv-display as char,
   pv-data as handle   )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutputToFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OutputToFile Procedure 
FUNCTION OutputToFile returns character
  ( pv-file  as char,
    pv-mfile as memptr,
    pv-local as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutputToPdf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OutputToPdf Procedure 
FUNCTION OutputToPdf returns character
  ( pv-txtfile as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutPutToScreen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OutPutToScreen Procedure 
FUNCTION OutPutToScreen returns logical
  ( pv-file as char  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PrintBrowse Procedure 
FUNCTION PrintBrowse returns logical
  ( pv-handle as handle,
    pv-title as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProperForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ProperForm Procedure 
FUNCTION ProperForm returns character
  ( pv-string as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReLabelBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReLabelBrowse Procedure 
FUNCTION ReLabelBrowse returns logical
  ( br-hand       as handle,
    lv-dateformat as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RunChild) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RunChild Procedure 
FUNCTION RunChild returns handle
  ( pv-prog as char,
    pv-parent as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SelectedItems) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SelectedItems Procedure 
FUNCTION SelectedItems returns character
  ( pv-hand as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetAllLkBut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetAllLkBut Procedure 
FUNCTION SetAllLkBut returns logical
  ( pv-frame   as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetAttributes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetAttributes Procedure 
FUNCTION SetAttributes returns character
  ( h-wid    as char,
    pv-prop  as char,
    pv-value as char,
    pv-datatype  as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetAuditMode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetAuditMode Procedure 
FUNCTION SetAuditMode returns logical
  ( pv-table as char,
    pv-mode as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetComboValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetComboValue Procedure 
FUNCTION SetComboValue returns character
  (input pv-key   as char,
   input pv-combo as handle) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetCursor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetCursor Procedure 
FUNCTION SetCursor returns logical
  ( pv-handle as handle,
    pv-state as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetFrameFocus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetFrameFocus Procedure 
FUNCTION SetFrameFocus returns handle
  ( pv-frame-handle as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetIniValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetIniValue Procedure 
FUNCTION SetIniValue returns logical
  ( input pv-section as char,
    input pv-key     as char,
    input pv-value   as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetLkBut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetLkBut Procedure 
FUNCTION SetLkBut returns logical
  ( pv-frame   as handle,
    pv-widhand as handle,   
    pv-mode    as log)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetNamedValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetNamedValue Procedure 
FUNCTION SetNamedValue returns character
  ( pv-name as char,
    pv-value as char,
    pv-list as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetNotModified) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetNotModified Procedure 
FUNCTION SetNotModified returns logical
  ( pv-frame as handle) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetOpDest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetOpDest Procedure 
FUNCTION SetOpDest returns character
  ( input-output pv-param as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetRegValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetRegValue Procedure 
FUNCTION SetRegValue returns logical
  ( input pv-section as char,
    input pv-key     as char,
    input pv-value   as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSession Procedure 
FUNCTION SetSession returns logical
  ( pv-state as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetWinState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetWinState Procedure 
FUNCTION SetWinState returns logical
  ( pv-win-handle as handle,
    pv-state      as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StringToDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StringToDate Procedure 
FUNCTION StringToDate returns date
  (lv-string as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StringToDec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StringToDec Procedure 
FUNCTION StringToDec returns decimal
  ( pv-string as char,
    pv-sep as char,
    pv-point as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StringToInt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StringToInt Procedure 
FUNCTION StringToInt returns integer
  ( pv-string as char,
    pv-sep as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StringToLog) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StringToLog Procedure 
FUNCTION StringToLog returns logical
    ( pv-String as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToLower) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ToLower Procedure 
FUNCTION ToLower returns logical
  ( pv-handle as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Touch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Touch Procedure 
FUNCTION Touch returns logical
  ( lv-fname as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToUpper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ToUpper Procedure 
FUNCTION ToUpper returns logical
  ( pv-handle as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UnixPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UnixPath Procedure 
FUNCTION UnixPath returns character
  ( pv-string as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidateDirectory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateDirectory Procedure 
FUNCTION ValidateDirectory returns logical
   ( pv-dir as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidUserSec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidUserSec Procedure 
FUNCTION ValidUserSec returns logical
  ( pv-notusers  as char,
    pv-notgroups as char,
    pv-runusers  as char,
    pv-rungroups as char,
    pv-user      as char,
    pv-group     as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Wait) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Wait Procedure 
FUNCTION Wait returns logical
  ( vi-milliseconds as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidgetExists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WidgetExists Procedure 
FUNCTION WidgetExists returns logical
  ( pv-framehandle as handle,
    pv-wid as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WidInfo Procedure 
FUNCTION WidInfo returns character
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
         HEIGHT             = 20.05
         WIDTH              = 64.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
procedure PrintDlgExA external "comdlg32.dll":U:
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Call-SetWidgetDefaults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Call-SetWidgetDefaults Procedure 
PROCEDURE Call-SetWidgetDefaults :
def input param pv-prog as handle no-undo.

def var lv-wid as handle no-undo.
def var lv-user as char no-undo.
def var lv-userName as char no-undo.
def var lv-from as char no-undo.
def var lv-dodates as log no-undo init true.
if not pgmusedefaults(pv-prog) then return.
assign
   lv-user     = getsysvar('{&clv}user')
   lv-userName = getfield('zen-duser',
                          'duser',       
                          lv-user,
                          'user-name').

run GetCalledFrom in pv-prog (output lv-from) no-error.
if lv-from = '' then do:
    self:sensitive = false.
    message
       'A Refresh has been executed since you opened this program.' skip
       'Please exit this program and re-run it before trying to save defaults.'
       view-as alert-box information.
    return.
end.
if num-entries(lv-from,'{&delim4}') > 1
   then lv-from = '{&delim4}' + entry(2,lv-from,'{&delim4}').

lv-from = pv-prog:name + (if lv-from ne ? then lv-from else "").

message 'Update' programtitle(string(pv-prog),'program') 'defaults for' 
   lv-userName + '?' skip
   view-as alert-box question buttons yes-no 
   title 'From ' + lv-from
   update lv-ok as log.

if not lv-ok then return.

assign
   lv-wid = widget-handle(GetSysVar("{&clv}top-window"))
   lv-wid = lv-wid:current-window
   lv-wid = lv-wid:first-child.

if lv-wid:private-data ne pv-prog:name 
then lv-wid = FindFirstFrame(pv-prog,lv-wid).
/* getwidhandle frame handle, *datatype;except these fields  */
if valid-handle(GetWidHandle(lv-wid,'*date;dentered,dchanged')) then
message 'Choose defaults for date fields? (All other fields will be given' skip
         'the values you just entered on the screen)'
view-as alert-box question buttons yes-no update lv-dodates.

empty temp-table t-Zen-FieldDefault.

run Proc-SetWidgetDefaults(lv-user,lv-from,lv-wid,lv-dodates).


/* write new values back to table on server side */
 {{&core}run.i &program   = "zen-fielddefault.p"       
               &path      = "{&core}{&srv}"
               &Appsrv    = "System"   
               &noper     = true
               &procedure = "UserBuild"             
               &params    = "(input lv-user,
                              input lv-from,
                              table t-zen-fielddefault)"} 


message 'Screen defaults have been saved.' skip
        'They will take effect when you press the Refresh button or log out and in.'
view-as alert-box information.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateDefault) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateDefault Procedure 
PROCEDURE CreateDefault :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-user   as char no-undo.
def input param pv-prog   as char no-undo.
def input param pv-frame  as handle no-undo.
def input param pv-wid    as handle no-undo.
def input param pv-function as char no-undo.
def input param pv-data   as char no-undo.

def var lv-ocolor as int no-undo.
def var lv-extra as char no-undo.

/* message program-name(1) skip      */
/*         pv-wid:name skip          */
/*         pv-data skip              */
/*         'Function ' pv-function.  */
if pv-function = 'noDefault' then return.

   create t-Zen-FieldDefault.
   assign
      lv-ocolor = pv-wid:bgcolor
      pv-wid:bgcolor = 12
      t-Zen-FieldDefault.duser             = pv-user
      t-Zen-FieldDefault.ProgramName       = pv-prog
      t-Zen-FieldDefault.FrameName         = pv-frame:name
      t-Zen-FieldDefault.tablename         = pv-wid:table
      t-Zen-FieldDefault.WidgetName        = pv-wid:name
      t-Zen-FieldDefault.WidExtent         = pv-wid:index
      t-Zen-FieldDefault.PopulationRoutine = pv-function.

   if pv-function = 'manual' or 
      pv-function = 'asentered'
   then assign
      pv-wid:screen-value             =
         if can-do('selection-list',pv-wid:type) and
            not substring(string(pv-wid:context-help-id),5,1) = "8" then
         pv-wid:screen-value else pv-data
      t-Zen-FieldDefault.DefaultValue = pv-data.
   else assign
      pv-wid:screen-value             = dynamic-function(
         pv-function,
         pv-data,
         pv-wid,
         output lv-extra)
      t-Zen-FieldDefault.DefaultValue = 'UseFunctionValue'.

   pv-wid:bgcolor = lv-ocolor.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initialise) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise Procedure 
PROCEDURE initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-user as char no-undo.
    lv-user = pv-user.
     
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-BuildCombo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-BuildCombo Procedure 
PROCEDURE Proc-BuildCombo :
def input param pv-table as char no-undo.
def input param pv-key   as char no-undo.
def input param pv-field as char no-undo.
def input param pv-where as char no-undo.
def input param pv-by    as char no-undo.
def input param pv-none  as log  no-undo.
def input param pv-wild  as log  no-undo.

def output param pv-codes  as char no-undo.
def output param pv-values as char no-undo.
 

if not iscached(pv-table) or
   pv-table begins 'usedb-'
then do:
   if pv-table begins 'usedb-'
   then pv-table = substring(pv-table,7).

  {{&core}run.i &program   = "dynamic.p"       
               &path      = "{&core}{&srv}"
               &Appsrv   = "system"
               &noper     = true            
               &procedure = "Dyn-BuildCombo"             
               &params    = "(pv-table,
                              pv-key,
                              pv-field,
                              pv-where,
                              pv-by,
                              pv-none,
                              pv-wild,
                              output pv-codes,
                              output pv-values)"}
end.
else do:
    CachedCombo(pv-table,
                      pv-key,
                      pv-field,
                      pv-where,
                      pv-by,
                      pv-none,
                      pv-wild,
                      output pv-codes,
                      output pv-values).

end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-CenterWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-CenterWindow Procedure 
PROCEDURE Proc-CenterWindow :
def input param pv-win-handle as handle no-undo.
 
    def var x as int no-undo.
    def var y as int no-undo.
    
    if pv-win-handle:type = 'frame' or
       pv-win-handle:type = 'window'
    then assign pv-win-handle:row    = (session:height-chars / 2) -
                                       (pv-win-handle:height-chars / 2)
                pv-win-handle:column = (session:width-chars / 2) -
                                       (pv-win-handle:width-chars / 2) no-error.
    else assign y = ((active-window:height-chars - pv-win-handle:height-chars) / 2) + 2
                x = ((active-window:width-chars - pv-win-handle:width-chars) / 2) + 2
                x = if x < 1 then 1 else x
                y = if y < 1 then 1 else y
                pv-win-handle:row    = y
                pv-win-handle:column = x no-error.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-ExportBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-ExportBrowse Procedure 
PROCEDURE Proc-ExportBrowse :
def input  param hBrowse as handle no-undo.

    if hBrowse:type ne 'browse' then return.

    def var lv-row         as int    no-undo init 0.
    def var hColumn        as handle no-undo.
    def var lv-col         as int    no-undo init 0.
    def var lv-origrow     as int    no-undo.
    def var hQuery         as handle no-undo.
    def var ch-application as com-handle no-undo.
    def var ch-WorkSheet   as com-handle no-undo.
   
    ch-application = OfficeStartApplication('spreadsheet','visible').
    ch-WorkSheet = OfficeNewWorkSheet(OfficeNewWorkBook(ch-application)).
   
    wapiFreezeWindow(hBrowse,1).       
   
    apply 'home' to hBrowse.
   
    hBrowse:select-row(1).
   
    assign hColumn     = hBrowse:first-column
           hQuery      = hBrowse:query
           lv-origrow  = hQuery:current-result-row.  
          
    if OfficeGetSuite() ne 'openoffice'          
    then assign lv-row = 1
                lv-col = 1 .  /* (Calc starts at 0 not 1) */
    
    /*column headers from browse column labels */
    do while valid-handle(hColumn):
        OfficeWriteCellData(ch-worksheet,lv-col,lv-row,' ' + hColumn:label).
        assign lv-col   = lv-col + 1
               hColumn  = hColumn:next-column.  
    end.
    /* ok to here !!!!!! */
    
  
    assign lv-row = lv-row + 1
           lv-col = if OfficeGetSuite() ne 'openoffice' then 1 else 0.
  
    do while not hQuery:Query-off-end:
        hColumn = hBrowse:first-column.
        do while valid-handle(hColumn):
             OfficeWriteCellData(ch-worksheet,lv-col,lv-row,' ' + hColumn:screen-value).
             lv-col   = lv-col + 1.
             hColumn   = hColumn:next-column.
        end.
        lv-col = if OfficeGetSuite() ne 'openoffice' then 1 else 0.
        lv-row = lv-row + 1.
        if not hBrowse:select-next-row() then leave.
    end.
    OfficeWorkSheetCellsAutoFit(ch-worksheet,0,hBrowse:num-columns).

    ch-application:Visible = true no-error.
     
    release object ch-application no-error.
    release object ch-WorkSheet no-error.
 
    hBrowse:query:reposition-to-row(lv-origRow).
    hBrowse:select-row(1).
    wapiFreezeWindow(hBrowse,0).
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-ExportQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-ExportQuery Procedure 
PROCEDURE Proc-ExportQuery :
/* The associated logic for OpenOffice has not been done here yet, as this
   procedure is not called from anywhere as of 07/07/2009 */
def input param pv-browse as handle no-undo.
def var h-qry as handle no-undo.
h-qry = pv-browse:query.
if not valid-handle(h-qry) then return.

def var lv-excluding as char no-undo init
'blob,class,clob,com-handle,datetime,datetime-tz,handle,memptr,recid,rowid,widget-handle'.

def var x as int no-undo.
def var y as int no-undo.
def var z as int no-undo.
def var q as int no-undo.
def var h-buff   as handle no-undo.
def var h-field  as handle no-undo.
def var lv-range as char   no-undo.
def var lv-r     as char   no-undo.
def var lv-col   as int    no-undo.
def var lv-ocol  as int    no-undo.
def var chExcelApplication as com-handle no-undo.
def var chWorkbook         as com-handle no-undo.
def var chWorkSheet        as com-handle no-undo.

create "Excel.Application" chExcelApplication.

assign chWorkbook  = chExcelApplication:Workbooks:Add()
       chWorkSheet = chExcelApplication:Sheets:Item(1)
       y           = 1
       lv-col      = 0.

h-QRY:GET-first().
do z = 1 to h-qry:num-buffers:
    h-buff = h-qry:get-buffer-handle(z).
    x = 1.
    do x = 1 to h-buff:num-fields:
        assign lv-col   = lv-col + 1
               h-field  = h-buff:Buffer-field(x) 
               lv-range = getexcelcolumnname(lv-col) + '1'
               chWorkSheet:Range(lv-range):Value = h-field:label.
    end.
end.
lv-col      = 0.

do while not h-QRY:Query-off-end:
    do z = 1 to h-qry:num-buffers:
        h-buff = h-qry:get-buffer-handle(z).
        x = 1.
        do x = 1 to h-buff:num-fields:
            h-field  = h-buff:Buffer-field(x).                  
            if not can-do(lv-excluding,h-field:data-type)
            then do:
                assign q = 1
                       lv-col = lv-col + 1
                       lv-r   = getexcelcolumnname(lv-col).
                lv-range = lv-r + string(y + 1).
                if h-field:extent = 0 
                then chWorkSheet:Range(lv-range):Value = h-field:BUFFER-VALUE.
                else do q = 1 to h-field:extent:
                    assign chWorkSheet:Range(lv-range):Value = h-field:BUFFER-VALUE(q)
                           lv-col = lv-col + 1
                           lv-r = getexcelcolumnname(lv-col)
                           lv-range = lv-r + string(y + 1).
                end.
            end.
        end.
    end.
    assign y = y + 1
           lv-col = 0.
    h-QRY:GET-NEXT().
end.

chWorkSheet:Cells:Select.
chWorkSheet:Cells:EntireColumn:AutoFit.
chExcelApplication:Visible = true.
release object chExcelApplication no-error.
release object chWorkbook no-error.
release object chWorksheet no-error.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-FrameChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-FrameChanged Procedure 
PROCEDURE Proc-FrameChanged :
def input  param pv-frame   as handle no-undo.
def output param lv-changed as log    no-undo.
  
    def var lv-wid-handle as handle no-undo.  
    assign 
        lv-wid-handle = pv-frame
        lv-wid-handle = lv-wid-handle:first-child
        lv-wid-handle = lv-wid-handle:first-child.

    do while valid-handle(lv-wid-handle) and not lv-changed:  
               
      if lv-wid-handle:type = 'frame' then do:
        lv-changed = FrameChanged(lv-wid-handle).
        if lv-changed then leave.
      end.
      if can-query(lv-wid-handle,'modified') and 
         lv-wid-handle:SENSITIVE  and
         can-do('editor,combo-box,fill-in,selection-list,toggle-box,radio-set,text-group',
                    lv-wid-handle:type)
      then do:
          if lv-wid-handle:modified = true then do:
            /* message lv-wid-handle:name. */
            lv-changed = true.

            leave.
          end.

      end.
      lv-wid-handle = lv-wid-handle:next-sibling.         
    end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-GetIniValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-GetIniValue Procedure 
PROCEDURE Proc-GetIniValue :
def input  param pv-section as char no-undo.
def input  param pv-key     as char no-undo.   
def output param pv-value   as char no-undo.
        
&if "{&OPSYS}" = 'win32' 
&then
    load lv-ininame dir lv-path base-key "INI" no-error.
    use lv-ininame no-error.
    if error-status:error then do:
        pv-value = lv-path + '*' + lv-ininame + '*' + pv-section + '*' + pv-key + '*' + Error-Status:Get-Message(Error-Status:Num-Messages).
        return.
    end.
    get-key-value section pv-section key pv-key value pv-value.
    unload lv-ininame no-error.
    if pv-value = ? 
    then get-key-value section pv-section key pv-key value pv-value.
&else
    def var irec as char no-undo.
    if filenotfound(lv-ininame) 
    then do:
      pv-value = lv-path + '*' + lv-ininame + '*' + pv-section + '*' + pv-key + '* File Not Found'.
      return.
    end.
    input stream ini from value(lv-ininame) no-echo.
    pv-value = lv-path + '*' + lv-ininame + '*' + pv-section + '*' + pv-key + '* Section Not Found'.
    outerloop:
    repeat:
        import stream ini unformatted irec.
        irec = trim(irec).
        if irec = '[' + pv-section + ']' 
        then repeat:
                pv-value = lv-path + '*' + lv-ininame + '*' + pv-section + '*' + pv-key + '* Key Not Found'.
                import stream ini unformatted irec.
                irec = trim(irec).
                if trim(entry(1,irec,'=')) = pv-key
                then do:
                    pv-value = replace(trim(entry(2,irec,'=')),'"','').
                    pv-value = replace(pv-value,"'",'').
                    input stream ini close.
                    leave outerloop.
                end.
             end.
    end.
    input stream ini close.
&Endif
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-LoadDefBackGround) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-LoadDefBackGround Procedure 
PROCEDURE Proc-LoadDefBackGround :
def input param pv-name  as char   no-undo.
def input param pv-frame as handle no-undo.
def input param pv-win   as handle no-undo.

def var h-wid       as handle no-undo.
def var x as int no-undo.
def var y as int no-undo.
def var lv-win    as handle no-undo.
def var lv-param  as char no-undo.
def var lv-list   as char no-undo.
def var lv-item   as char no-undo.
def var lv-type   as char no-undo.
def var lv-x      as dec no-undo.
def var lv-y      as dec no-undo.
def var lv-width  as dec no-undo.
def var lv-height as dec no-undo.
def var lv-extra  as char no-undo.
def var lv-origstate as log no-undo.

def var lv-props  as char no-undo.
def var lv-values as char no-undo.



pv-name = pv-name + "Background".
lv-param = GetIniValue(pv-name,"Windowsize").

if lv-param = '?' or 
   lv-param = ?  then return.

assign
    lv-win        = pv-win
    lv-origstate  = lv-win:hidden
    lv-win:hidden = true.

FreezeWindow(lv-win,1).

assign
      pv-frame:width-pixels  = int(entry(1,lv-param))
      pv-frame:height-pixels = int(entry(2,lv-param))
      lv-win:width-pixels = pv-frame:width-pixels
      lv-win:height-pixels = pv-frame:height-pixels.
      
lv-list = GetIniValue(pv-name,"ItemNames").
do x = 1 to num-entries(lv-list):  
    assign        
        lv-item   = entry(x,lv-list)
        lv-type   = Getinivalue(lv-item,'type')
        lv-x      = StringToDec(Getinivalue(lv-item,'X'),',','.')
        lv-y      = StringToDec(Getinivalue(lv-item,'Y'),',','.')
        lv-width  = StringToDec(Getinivalue(lv-item,'width'),',','.')
        lv-height = StringToDec(Getinivalue(lv-item,'height'),',','.')
        lv-extra  = Getinivalue(lv-item,'extra')
        lv-props  = ''
        lv-values = ''.
    do y = 1 to num-entries(lv-extra):
        assign
            lv-props  = lv-props  + ',' + entry(y,lv-extra)
            lv-values = lv-values + ',' + entry(y + 1,lv-extra)
            y = y + 1.
    end.
    assign
        lv-props  = substring(lv-props,2)
        lv-values = substring(lv-values,2)
        y = 1.

    case lv-type:
        when 'rectangle' then do:
            create rectangle h-wid
            assign frame = pv-frame.
            do y = 1 to num-entries(lv-props):
                if not can-set(h-wid,entry(y,lv-props)) then next.
                case entry(y,lv-props):
                    when 'graphic-edge' then h-wid:graphic-edge = StringToLog(entry(y,lv-values)).
                    when "edge-pixels"  then h-wid:edge-pixels  = StringToInt(entry(y,lv-values),',').
                    when "bgcolor"      then h-wid:bgcolor      = StringToInt (entry(y,lv-values),',').
                    when "filled"       then h-wid:filled       = StringToLog(entry(y,lv-values)).
                end case.
            end.
        end.
        when 'image' then do:
            create image h-wid
            assign
                stretch-to-fit = true
                frame = pv-frame.
            h-wid:load-image(search(entry(lookup('image',lv-extra) + 1,lv-extra))).
            h-wid:move-to-top().
        end.
        when 'Text' then do:
            create text h-wid
            assign
                screen-value = entry(lookup('text',lv-extra) + 1,lv-extra)
                frame = pv-frame.
            h-wid:width = length(h-wid:screen-value).
        end.
        otherwise next.
    end case.

    assign
          h-wid:x = lv-x
          h-wid:y = lv-y
          h-wid:height-pixels = lv-height
          h-wid:width-pixels  = lv-width.
end.

lv-win:hidden = lv-origstate.
FreezeWindow(lv-win,0).

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-PrintBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-PrintBrowse Procedure 
PROCEDURE Proc-PrintBrowse :
def input param pv-handle as handle no-undo.
def input param pv-title  as char   no-undo.
def var h-browse as handle no-undo.
def var h-qry    as handle no-undo.
def var lv-err as char no-undo.
def var lv-user as char no-undo.
def var lv-filename as char no-undo.
def var lv-copies as int init 1.
lv-user = getsysvar('{&clv}user').
lv-filename = getctrl("{&scratchpath}") + lv-user + 'temp.txt'.
touch(lv-filename).
file-info:filename = lv-filename.
lv-filename = file-info:full-pathname.

output stream op to lv-filename paged page-size 80.

form header pv-title " for Sys." getsysvar('{&clv}gs-sys-cd') "  Prac." getsysvar('{&clv}practice')
            lv-user  at 53
            "Page:" at 62 string(page-num(op)) form "x(4)"
            today   to 80 skip(2)
            with page-top no-box no-labels frame pagetop.
view stream op frame pagetop.

if pv-handle:type = 'query'
    then assign h-qry = pv-handle
                h-browse = ?.
    else assign h-qry = pv-handle:query
                h-browse = pv-handle.

def var h-buff  as handle no-undo.
def var h-field as handle no-undo.
def var v-hand as handle no-undo.
def var v-flist as  char no-undo.
def var x as int no-undo.
def var y as int no-undo.
def var z as int no-undo.
def var lv-len as int no-undo.
def var lv-fmt as char no-undo.
if valid-handle(h-browse) 
then do:
    v-hand = h-browse:first-column.
    do while valid-handle(v-hand):
        assign
            V-FLIST = v-flist  + ',' + v-hand:table + '.' + v-hand:name
            v-hand  = v-hand:next-column.
    end.
end.

h-QRY:GET-first().
y = 1.
/* column labels */

do while not h-QRY:Query-off-end:
    do z = 1 to h-qry:num-buffers:
        h-buff = h-qry:get-buffer-handle(z).
        do x = 1 to h-buff:num-fields:
            h-field = h-buff:Buffer-field(x).
            if valid-handle(h-browse) then
                If not can-do(v-flist,h-field:table + '.' + h-field:name) then next.
            if y = 1 then do: /* get column labels */
                lv-len = max(length(h-field:buffer-value),length(h-field:column-label)).
                lv-fmt = 'x(' + string(lv-len) + ')'.
                put stream op h-field:column-label format lv-fmt ' '.
            end.            
        end.
    end.
    put stream op skip.
    leave.
end.
assign  x = 1
        y = 1
        z = 1.

do while not h-QRY:Query-off-end:
    do z = 1 to h-qry:num-buffers:
        h-buff = h-qry:get-buffer-handle(z).
        do x = 1 to h-buff:num-fields:
            h-field = h-buff:Buffer-field(x).
            if valid-handle(h-browse) then
                If not can-do(v-flist,h-field:table + '.' + h-field:name) then next.
            put stream op h-field:BUFFER-VALUE format h-field:format ' '.
        end.
    end.
    put stream op skip.
    h-QRY:GET-NEXT().
end.
output stream op close.
wait(500).
lv-err = WapiRawPrint (session:printer-name, lv-filename,lv-copies).     
if lv-err ne '' then message lv-err {&dbt}.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-RelabelBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-RelabelBrowse Procedure 
PROCEDURE Proc-RelabelBrowse :
def input param br-hand       as handle no-undo.
    def input param lv-dateformat as char   no-undo. 

        def var v-qry    as handle no-undo.
        def var v-hand   as handle no-undo.        
        def var v-pstr   as char   no-undo.
        def var v-field  as handle no-undo.
        def var x        as int    no-undo.
        def var y        as int    no-undo.
        def var Z        as int    no-undo.
        
        def var h-buff   as handle no-undo.
        def var v-FLIST  as char   no-undo.
        def var v-format as char   no-undo.
        def var v-width  as char   no-undo.
        def var v-labels as char   no-undo.
        def var v-hasdates as log  no-undo.
        
        v-hand = br-hand:first-column.
            
        do while valid-handle(v-hand):
            assign
                V-FLIST    = v-flist  + '{&Delim2}' + string(v-hand:name)
                v-format   = v-format + '{&Delim2}' + v-hand:format
                v-width    = v-width  + '{&Delim2}' + string(v-hand:width-chars)
                v-labels   = v-labels + '{&Delim2}' + v-hand:label
                v-hasdates = if not v-hasdates then v-hand:data-type = 'date' 
                                               else v-hasdates
                v-hand     = v-hand:next-column.
        end.
        
        assign v-flist  = substring(v-flist,2)
               v-format = substring(v-format,2)
               v-width  = substring(v-width,2)
               v-labels = substring(v-labels,2).
        
        if lv-dateformat = '99/99/9999' then v-hasdates = false.
        
        if v-hasdates
        then do:
/*             assign                                                                             */
/*                 v-qry         = br-hand:query                                                  */
/*                 x             = 1                                                              */
/*                 y             = 1                                                              */
/*                 z             = 1                                                              */
/*                 br-hand:query = ?                                                              */
/*                 br-hand:query = v-qry.                                                         */
/*                                                                                                */
/*             do x = 1 to num-entries(v-flist,'{&Delim2}'):                                              */
/*                    z = 1.                                                                      */
/*                    BUFF-LOOP:                                                                  */
/*                    do z = 1 to v-qry:num-buffers:                                              */
/*                        h-buff = v-qry:get-buffer-handle(z).                                    */
/*                        y = 1.                                                                  */
/*                        do y = 1 to h-buff:num-fields:                                          */
/*                            v-field = h-buff:Buffer-field(y).                                   */
/*                            If v-field:name = entry(x,v-flist,'{&Delim2}')                              */
/*                             then leave BUFF-LOOP.                                              */
/*                        end.                                                                    */
/*                    end.                                                                        */
/*                                                                                                */
/*                    if not valid-handle(v-field) then next.                                     */
/*                    v-field:format = if v-field:data-type = 'date' then lv-dateformat           */
/*                                                                   else entry(x,v-format,'{&Delim2}').  */
/*                    v-field:label = entry(x,v-labels,'{&Delim2}').                                      */
/*                 br-hand:ADD-LIKE-column(v-field).                                              */
/*             end.                                                                               */
        end.
        
        v-hand = br-hand:first-column.
        x = 1.
        do while valid-handle(v-hand):
            assign
                v-hand:label = altlanguage(v-hand:label)
                v-hand:width = max(int(entry(x,v-width,'{&Delim2}')),length(v-hand:label))
                v-hand       = v-hand:next-column
                x = x + 1 no-error.
        end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-SetIniValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SetIniValue Procedure 
PROCEDURE Proc-SetIniValue :
def input param pv-section as char no-undo.
def input param pv-key     as char no-undo.   
def input param pv-value   as char no-undo.

def var x as int no-undo.

load lv-ininame dir lv-path base-key "INI" no-error.
use lv-ininame no-error.

if error-status:error then return 'failed'.
do x = 1 to num-entries(pv-key,'{&Delim3}'):
    put-key-value section pv-section key entry(x,pv-key,'{&Delim3}') value entry(x,pv-value,'{&Delim3}').
end.
unload lv-ininame no-error.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-SetOpDest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SetOpDest Procedure 
PROCEDURE Proc-SetOpDest :
def input-output param pv-params as char no-undo.
    
define var lpPrintDlg as memptr no-undo.
define var lpDevMod   as memptr no-undo.

/* define var lpDevNames as memptr no-undo. */
define var iRetCode as int no-undo.
def var lv-dlgoptions as int no-undo.
def var lv-printerparams as char no-undo.

if stringtolog(GetNamedValue('ShowPrtDlg',pv-params))
then do:
    run {&core}{&prt}printerselect.w (?,output lv-printerparams).
    /* this is the form of lv-printerparams
    lv-printerparams = string(t-printer.printertableid) + '{&Delim2}' +
                             t-printer.printer-name + '{&Delim2}' +
                             string(lv-copies) + '{&Delim2}' +
                             string(lv-tray) + '{&Delim2}' +
                             lv-filename + '{&Delim2}' + 
                             string(lv-batch) + '{&Delim2}' + 
                             lv-taskserver + '{&Delim2}'.    */
    
    if lv-printerparams = 'none' 
    then do:
        pv-params = '**Failed=true,' + pv-params.
        return.
    end.
    else assign pv-params = SetNamedValue('Printer',entry(2,lv-printerparams,'{&Delim2}'),pv-params)    
                pv-params = SetNamedValue('Copies',entry(3,lv-printerparams,'{&Delim2}'),pv-params)    
/*                pv-params = SetNamedValue('FromPage',entry(,lv-printerparams,'{&Delim2}'),pv-params)
                pv-params = SetNamedValue('ToPage',entry(,lv-printerparams,'{&Delim2}'),pv-params)
                pv-params = SetNamedValue('MinPage',entry(,lv-printerparams,'{&Delim2}'),pv-params)
                pv-params = SetNamedValue('MaxPage',entry(,lv-printerparams,'{&Delim2}'),pv-params) */.
   def var lv-ret         as int  no-undo.
   if stringtolog(GetNamedValue('SetSessionPrinter',pv-params))
   then do:
        session:printer-name = entry(2,lv-printerparams,'{&Delim2}') no-error.
        if error-status:error or 
           error-status:num-messages ne 0 
        then do:
            message 'Invalid Printer ' + entry(2,lv-printerparams,'{&Delim2}') skip
                    'Please Reselect'
            view-as alert-box error .
            return.
        end.
        WapiSetDefaultPrinter(entry(2,lv-printerparams,'{&Delim2}')).
        
   end.
end.

    lv-dlgoptions = if StringToLog(getctrl('HideWinPrtDlg'))
                         then {&PD_RETURNDEFAULT} + {&PD_RETURNDC}
                         else {&PD_RETURNDC}.
            
    set-size(lpdevmod) = 200.
            
    /* set up PRINTDLG Structure */
    assign /* allocate memory */
        set-size(lpPrintDlg) = 66
        put-long(lpPrintDlg,1) = get-size(lpPrintDlg) /* structuresize */
        put-long(lpPrintDlg,5) = current-window:hwnd /* hwndOwner*/
        put-long(lpPrintDlg,9) =  0 /* hDevMod   /* pointer to DEVMOD structure */ */
        put-long(lpPrintDlg,13) = 0 /* hDevNames  /* pointer to device names */  */
        put-long(lpPrintDlg,17) = 0 /* hDC    */ 
        put-long(lpPrintDlg,21) = lv-dlgoptions 
        put-short(lpPrintDlg,25) = max(1,int(GetNamedValue('frompage',pv-params))) /* nFromPage    */
        put-short(lpPrintDlg,27) = max(1,int(GetNamedValue('topage',pv-params))) /* nToPage    */
        put-short(lpPrintDlg,29) = max(1,int(GetNamedValue('Minpage',pv-params))) /* nMinPage    */
        put-short(lpPrintDlg,31) = max(1,int(GetNamedValue('Maxpage',pv-params))) /* nMaxPage    */
        put-short(lpPrintDlg,33) = max(1,int(GetNamedValue('Copies',pv-params))) /* nCopies    */
        put-long(lpPrintDlg,35) = 0
        put-long(lpPrintDlg,39) = 0
        put-long(lpPrintDlg,43) = 0
        put-long(lpPrintDlg,47) = 0
        put-long(lpPrintDlg,51) = 0
        put-long(lpPrintDlg,55) = 0
        put-long(lpPrintDlg,59) = 0
        put-long(lpPrintDlg,63) = 0.
    
    set-size(lpdevmod) = 200.
    WapiGlobalLock(get-long(lpPrintDlg,9)).
    
    set-pointer-value(lpDevMod) = iRetCode.
    set-size(lpdevmod) = 200.
    /***************************
    lvdevmod http://msdn.microsoft.com/en-us/library/ms646843(VS.85).aspx
    ****************************/
    put-long(lpDevMod,41) = int(GetNamedValue('Copies',pv-params)).
    
    if stringtolog(getctrl("Dbgwapirawprint"))
    then message 'Passed in : ' skip  
                 'Params : ' pv-params skip
                 'Copies : ' string(get-long(lpDevMod,41)) {&dbt}.  
    iRetCode = WapiPrintDlg (input get-pointer-value(lpPrintDlg)).
      
    if iRetCode > 0  
    then do: /* ok pressed */ /*wapi globallockmemory etc */
        set-pointer-value(lpDevMod) = get-long(lpPrintDlg,9). 
        if stringtolog(getctrl("Dbgwapirawprint"))
        then message 'Print Dlg returns ' skip 
                     'Printer : ' session:printer-name skip 
                     'Devmode : '  string(get-long(lpDevMod,41)) skip 
                     'None DevMode : ' string(get-short(lpPrintDlg,33)) {&dbt}.    
    
    /*    if LogicalAnd({&DM_COPIES},get-long(lpDevMod,41) ) > 0 /* num copies is stored on DEVMOD structure */
        then iretcode = get-short(lpDevMod,41).
        else */
        iretcode = get-short(lpPrintDlg, 33). /* Device does not support copies, stored in PrintDlg structure */
        
        assign 
            pv-params = SetNamedValue('Copies',string(iretcode),pv-params)    
            pv-params = SetNamedValue('FromPage',string(get-short(lpPrintDlg, 25)),pv-params)
            pv-params = SetNamedValue('ToPage',string(get-short(lpPrintDlg, 27)),pv-params)
            pv-params = SetNamedValue('MinPage',string(get-short(lpPrintDlg, 29)),pv-params)
            pv-params = SetNamedValue('MaxPage',string(get-short(lpPrintDlg, 31)),pv-params).
    
        if stringtolog(getctrl("Dbgwapirawprint"))
        then message 'So Will Use' skip 
                     'Copies : ' iretcode skip 
                     'Params : ' pv-params skip 
                     'Devmode : ' string(get-long(lpDevMod,41))
                     'None DevMode : 'string(get-short(lpPrintDlg, 33)) {&dbt}.
      
    end.
    else do:
         set-size(lpPrintDlg) = 0.
         pv-params = '**Failed=true,' + pv-params.
    end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-SetWidgetDefaults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SetWidgetDefaults Procedure 
PROCEDURE Proc-SetWidgetDefaults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-user as char no-undo.
def input param pv-prog as char no-undo.
def input param pv-frame as handle no-undo.
def input param pv-dodates as log no-undo.

def var lv-wid as handle no-undo.
def var lv-data as char no-undo.
def var lv-function as char no-undo.
def var lv-f1d1data as char no-undo.
def var lv-fld2data as char no-undo.

assign
   lv-wid = pv-frame:first-child
   lv-wid = lv-wid:first-child.

do while valid-handle(lv-wid):
   if lv-wid:type = 'frame' 
      then run Proc-SetWidgetDefaults(pv-user,pv-prog,lv-wid,pv-dodates).
   else if can-do('editor,combo-box,fill-in,selection-list,toggle-box,radio-set,text-group',
                lv-wid:type) /* and lv-wid:data-type ne 'date' */
         and lv-wid:sensitive 
         and lv-wid:name ne 'edinfobox'
   then do:
      if lv-wid:context-help-id ne {&ExcludeFromDefaultsID}
      then do:
         if lv-wid:data-type = 'date' and pv-dodates 
         then do:
               run {&client}{&general}dialog-GetDateDefault.w (
                  lv-wid,
                  output lv-data).
               assign lv-function = entry(1,lv-data,'{&delim2}')
                      lv-data     = if num-entries(lv-data,'{&delim2}') > 1
                                    then entry(2,lv-data,'{&delim2}') else "".
               run CreateDefault (pv-user,pv-prog,pv-frame,lv-wid,lv-function,lv-data).
         end.
         else do:
            if lv-wid:private-data = 'fiscal:period' then do:
               lv-wid:private-data = 'fiscal:both'.
               run {&client}{&general}dialog-FiscalPeriodDefault.w (
                  lv-wid,
                  output lv-data) .
               assign 
                  lv-function = entry(1,lv-data,'{&delim2}')
                  lv-data     = if num-entries(lv-data,'{&delim2}') > 1
                                then entry(2,lv-data,'{&delim2}') else ""
                  lv-f1d1data = if lv-data = "" then "" else entry(1,lv-data,':')
                  lv-fld2data = if lv-data = "" then "" else entry(2,lv-data,':')
                  lv-wid:private-data = 'fiscal:Period'.
               run CreateDefault (pv-user,pv-prog,pv-frame,lv-wid,lv-function,lv-f1d1data).
               /* now do it for associated year field */
               lv-wid = lv-wid:next-tab-item.
               run CreateDefault (pv-user,pv-prog,pv-frame,lv-wid,lv-function,lv-fld2data).
            end.
            else do:
                if can-do('selection-list',lv-wid:type) and
                  not substring(string(lv-wid:context-help-id),5,1) = "8"
                then do:
                   if can-set(lv-wid,'list-items') then lv-data = lv-wid:list-items.
                                                   else lv-data = lv-wid:list-item-pairs.
                end.
                else lv-data = lv-wid:screen-value.

               /* 07/14/09 EKS, needed to use the private data to set the
                  defaults for a widget, so used help-id in order
                  to use private-data instead of screen-value.  The number
                  chosen is one less than the help-id used to eliminate a
                  widget from using defaults. */
               if substring(string(lv-wid:context-help-id),6,1) = "8" then
                  lv-data = lv-wid:private-data.

               run CreateDefault (pv-user,
                                  pv-prog,
                                  pv-frame,
                                  lv-wid,
                                  'asentered',
                                  lv-data).
            end.
         end.
      end.
   end.
   lv-wid = lv-wid:next-sibling.
end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proc-StringToDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-StringToDate Procedure 
PROCEDURE Proc-StringToDate :
def input  param pv-string as char no-undo.
def output param pv-date  as date no-undo format '99/99/9999' init ?.
def output param pv-sdate as date no-undo init ?.

if length(pv-string) < 10 
    then pv-sdate = date(pv-string) no-error.
    else pv-date = date(pv-string) no-error.

/*   def var lv-day   as char no-undo.                                         */
/*   def var lv-month as char no-undo.                                         */
/*   def var lv-year  as char no-undo.                                         */
/*   def var lv-y     as int no-undo.                                          */
/*                                                                             */
/*   case session:date-format:                                                 */
/*     when 'dmy' then assign lv-day   = substring(pv-string,1,2)              */
/*                            lv-month = substring(pv-string,4,2)              */
/*                            lv-year  = substring(pv-string,7).               */
/*     when 'mdy' then assign lv-day   = substring(pv-string,4,2)              */
/*                            lv-month = substring(pv-string,1,2)              */
/*                            lv-year  = substring(pv-string,7).               */
/*   end case.                                                                 */
/* message pv-string lv-year.                                                  */
/*   if length(pv-string) < 10                                                 */
/*     then pv-sdate = date(int(lv-month),int(lv-day),int(lv-year)) no-error.  */
/*     else pv-date = date(int(lv-month),int(lv-day),int(lv-year)) no-error.   */

  if error-status:error 
    then assign
            pv-date = ?
            pv-sdate = ?.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-AddLastSlash) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddLastSlash Procedure 
FUNCTION AddLastSlash returns character
  ( input directoryName as char ) :
/*------------------------------------------------------------------------------
  Purpose:  make sure last character of directory name is a "/" or a "\" 
    Notes:  can't use quoted characters, especially the backslash, in unix
------------------------------------------------------------------------------*/
   def var backSlashChar         as int    no-undo init 92.
   def var forwardSlashChar      as int    no-undo init 47.


   if r-index(directoryName,chr(backSlashChar)) ne 0 and
      asc(substring(directoryName,length(directoryName),1)) ne backSlashChar then
      directoryName = directoryName + chr(backSlashChar).
   else if r-index(directoryName,chr(forwardSlashChar)) ne 0 and
      asc(substring(directoryName,length(directoryName),1)) ne forwardSlashChar then
      directoryName = directoryName + chr(forwardSlashChar).

  return directoryName.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AuditedField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AuditedField Procedure 
FUNCTION AuditedField returns logical
  ( pv-table as char,
    pv-field as char ) :
def var pv-ok as log no-undo.

if pv-table ne ? then do:
if pv-table begins 't-' 
then pv-table = substring(pv-table,3).
    {{&core}run.i &program   = "zen-auditconfig.p"
                 &path      = "{&aud}{&srv}"
                 &Appsrv    = "System"
                 &procedure = "auditedfield"
                 &params    = "(pv-table,pv-field,output pv-ok)"}

end.

  return pv-ok.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BackUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BackUp Procedure 
FUNCTION BackUp returns logical
  (pv-from as char,
   pv-to as char ) :

   assign pv-from = fixpath(pv-from)
          pv-to   = fixpath(pv-to).
   os-rename value(pv-from) value(pv-to).

  return true.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildCombo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BuildCombo Procedure 
FUNCTION BuildCombo returns character
  (pv-combo as handle,
   pv-table as char,
   pv-key   as char,
   pv-field as char,
   pv-where as char,
   pv-by    as char,
   pv-none  as log,
   pv-wild  as log):

def var lv-codes  as char no-undo.
def var lv-values as char no-undo.
def var x         as int  no-undo.           

  run Proc-BuildCombo(pv-table,
                      pv-key,
                      pv-field,
                      pv-where,
                      pv-by,
                      pv-none,
                      pv-wild,
                      output lv-codes,
                      output lv-values).

/* if list-item-pairs */
/*  do x = 1 to num-entries(lv-codes,"{&ComboDelim}"):                                        */
/*   pv-combo:add-last(entry(x,lv-values,"{&ComboDelim}"),entry(x,lv-codes,"{&ComboDelim}")). */
/*  End.                                                                                      */
/* else */
if valid-handle(pv-combo) then 
    assign
        pv-combo:list-items = lv-values
        pv-combo:private-data = lv-codes
        pv-combo:screen-value = entry(1,pv-combo:list-items,'{&ComboDelim}').

  return lv-codes + '{&Delim2}' + lv-values. 

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CenterWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CenterWindow Procedure 
FUNCTION CenterWindow returns logical
  ( pv-win-handle as handle) :
    run Proc-CenterWindow in this-procedure (pv-win-handle).
return true.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CharTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CharTime Procedure 
FUNCTION CharTime returns character
  ( pv-time       as int) :

 
return substring(string(pv-time,'HH:MM'),1,2)
               + substring(string(pv-time,'HH:MM'),4,2).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-cleanupQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cleanupQuery Procedure 
FUNCTION cleanupQuery returns logical
        ( input hQuery as handle ):
/*-----------------------------------------------------------------------------
  Purpose: Deletes a query object and all its buffers.
    Notes: DO NOT CALL THIS WITH STATIC BUFFERS.
-----------------------------------------------------------------------------*/
   def var queryBufferList as char  no-undo.
   def var counter         as int    no-undo.
   def var hQueryBuffer    as handle     no-undo.
   def var hBuffer         as handle     no-undo.


   /* we're not interested if the query or dataset has invalid handle */
   if not valid-handle(hQuery) then return true.

   /* close the query if it is open */
   if hQuery:is-open then hQuery:query-close().

   /* We can't delete the buffers before we have deleted the query object.
      We also can't get the list of buffers that the query uses after we have
      deleted it. So we're going to build up a string with all the handles to
      the buffers before we delete the query. */

   /* iterate through all the buffers and build up a list with their handles */
   do counter = 1 to hQuery:num-buffers:
      queryBufferList = queryBufferList +
         (if queryBufferList = "" then "" else ",") +
         string(hQuery:get-buffer-handle(counter)).
   end. /* number of buffers in query */

   /* Now delete the query object */
   if valid-handle(hQuery) then delete object hQuery.

   /* Now iterate through the string */
   do counter = 1 to num-entries(queryBufferList):
      assign
         /* convert each entry to a handle to the buffer */
         hQueryBuffer = widget-handle(entry(counter,queryBufferList))
         /* change the buffer handle to a table handle, so we can delete it
            (note: cannot delete a tables default buffer handle) */
         hBuffer = hQueryBuffer:table-handle.

      /* delete handle to buffer if valid */
      if valid-handle(hBuffer) then delete object hBuffer.
   end. /* number of buffers in query */

   return true.   /* Function return value. */
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvPcl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvPcl Procedure 
FUNCTION ConvPcl returns character
  ( pv-ipfile as char,
    pv-type as char ) :

def var lv-cmd     as char no-undo.
def var lv-opdir   as char no-undo.
def var lv-opfile  as char no-undo.
def var lv-ext     as char no-undo.
def var lv-origdir as char no-undo.
def var lv-appdir  as char no-undo.
def var lv-extra   as char no-undo.

assign
    lv-appdir = GetCtrl('PclConvDir')
    file-info:filename = '.'
    lv-origdir = file-info:full-pathname
    file-info:file-name = pv-ipfile
    pv-ipfile           = file-info:full-pathname
    lv-opdir = session:temp-directory
    lv-opfile = entry(num-entries(pv-ipfile,chr(92)),entry(1,pv-ipfile,'.'),chr(92)).

case pv-type:
    when 'pdf'  
    then assign lv-ext = '.pdf'
                lv-opfile = lv-opfile + lv-ext
                lv-cmd = 'pcltopdf -i "' + pv-ipfile + '" -o "' + lv-opdir + lv-opfile + '"'.

    when 'Tiff' 
    then assign lv-ext = '.tif'
                lv-opfile = lv-opfile + lv-ext
                lv-cmd = 'pcltotiff -i "' + pv-ipfile + '" -o "' + substring(lv-opdir,1,length(lv-opdir) - 1) + '" -m'. 
    otherwise return pv-ipfile.
end case.

  SetWorkingDir(lv-appdir).

  os-command silent value(lv-cmd).
  
  SetWorkingDir(lv-origdir).

  return search(lv-opdir + lv-opfile).
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateFile Procedure 
FUNCTION CreateFile returns character
  ( pv-file as char) :

output stream op to value(pv-file).
output stream op close.

assign
    file-info:filename = pv-file
    pv-file = file-info:full-pathname.
    
  return pv-file.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DateInWords) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DateInWords Procedure 
FUNCTION DateInWords returns character
    (ip-date            as date,
     ip-long-name       as log):
    
    return if ip-long-name 
            then DayName(ip-date)          + " " +
                 string(day(ip-date),"99") + " " +
                 MonthName(ip-date)        + " " +
                 string(year(ip-date),"9999")
           else substring(DayName(ip-date),1,3)   + " " +
                string(day(ip-date),">9")         + " " +
                substring(MonthName(ip-date),1,3) + " " +
                substring(string(year(ip-date),"9999"),3,2).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DayName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DayName Procedure 
FUNCTION DayName returns character
    (ip-date        as date):

    return /* altlanguage( */
   entry(weekday(ip-date),"Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday")
      /* ) */.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DirectoryNotFound) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DirectoryNotFound Procedure 
FUNCTION DirectoryNotFound returns logical
  ( pv-fname as char) :

   def var lv-bad as log    no-undo.
   
                                    
   assign
      file-info:file-name = pv-fname
      lv-bad              = 
         file-info:file-type = ? or index(file-info:file-type,'d') = 0.

  return lv-bad.   /* Function return value. */
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoNotFire) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoNotFire Procedure 
FUNCTION DoNotFire returns logical
  ( PV-WIDLIST as char ) :
/*------------------------------------------------------------------------------
  Purpose: /* put in any leave logic to stop it firing if we hit cancel or whatever we want
   &widlist is list of widgetnames we dont want it to fire on choose of */ 
    Notes:  if donotfire(btn-exit
------------------------------------------------------------------------------*/
/* message last-event:function. */

def var lv-WidHandle as handle  no-undo.
def var lv-wid as handle no-undo.



if last-event:function = 'leave' or
   last-event:function = 'choose' or
   last-event:function = 'container-event' 
then do: 
   /* 11/15/06 AMD put in to handle pseudo-widget situation in
      appointment change logic */
   if valid-handle(last-event:widget-leave) then 
      lv-widhandle = last-event:widget-leave.
   else return false.
   

/*    if last-event:widget-enter:frame ne  */
/*       lv-widhandle:frame  */
/*       then return true.   */

   if not valid-handle(last-event:widget-enter) then return true.
   if not can-query(last-event:widget-enter,'frame') then return true.
   if not valid-handle(last-event:widget-enter:frame) then return true.
   if last-event:widget-enter:frame:name = 'win-tab-frame'       or
      last-event:widget-enter:frame:name = 'fr-assignUserButton' or
      last-event:widget-enter:name = 'cb-currentpractice'        or
      last-event:widget-enter:name = 'btn-defaults'              or
      can-do(pv-widlist,last-event:widget-enter:name) 
   then do:
/*       apply 'entry' to lv-WidHandle.     */
/*       lv-widhandle:selected = true.      */
/*       lv-widhandle:set-selection(1,20).  */
       return true.
   end.
end.

  return false.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DosPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DosPath Procedure 
FUNCTION DosPath returns character
  ( pv-string as char ) :
  def var lv-slash as char no-undo.

  &if "{&OPSYS}" = "unix" 
    &then lv-slash = "\\".
    &else lv-slash = "~\".
  &endif
  
  return replace(pv-string,'~/',lv-slash).
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExecHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExecHandle Procedure 
FUNCTION ExecHandle returns handle
  ( pv-appsrv as char,
    pv-path as char,
    pv-prog as char) :

def var h-phand as handle no-undo.
def var h-app   as handle no-undo.

    h-phand = getprochandle(pv-appsrv,pv-prog).
    if not valid-handle(h-phand) 
    then do:
        if pv-appsrv ne 'local' and
           pv-appsrv ne ''
        then run value(pv-path + pv-prog) on GetAppserverhandle(pv-appsrv) 
                 transaction distinct persistent set h-phand no-error.
        else run value(pv-path + pv-prog) persistent set h-phand no-error.
    end.


  return h-phand.   

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExportBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExportBrowse Procedure 
FUNCTION ExportBrowse returns logical
  ( pv-handle as handle) :
if not Valid-handle(pv-handle) then return error.

if getctrl('ExportBrowseType') ne 'query' 
    then run proc-ExportBrowse(pv-handle). /* just screen contents */
    else run proc-ExportQuery(pv-handle). /* use query not browse */

    return not error-status:error.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FileNotFound) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FileNotFound Procedure 
FUNCTION FileNotFound returns logical
  ( pv-fname as char) :

def var lv-bad    as log  no-undo.

lv-bad = search(substring(pv-fname,1,index(pv-fname,'.')) + 'r') = ?.

if lv-bad 
then lv-bad = search(pv-fname) = ?.

  return lv-bad.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FindFirstFrame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FindFirstFrame Procedure 
FUNCTION FindFirstFrame returns widget-handle
  (pv-prog as handle,
   pv-frame as handle ) :
/*------------------------------------------------------------------------------
  Purpose: return the first frame in procedure  
    Notes:  
------------------------------------------------------------------------------*/
def var lv-wid as handle no-undo.

assign
   lv-wid = pv-frame:first-child
   lv-wid = lv-wid:first-child.

do while valid-handle(lv-wid):
   if lv-wid:type = 'frame' 
   then do:
      if lv-wid:private-data = pv-prog:name 
         then return lv-wid.
         else FindFirstFrame(pv-prog,lv-wid).
   end.
   lv-wid = lv-wid:next-sibling.
end.

  return lv-wid.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixedString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixedString Procedure 
FUNCTION FixedString returns character
 ( pv-str as char,
   pv-allownumeric as log ) :

def var pv-out  as char no-undo.
def var lv-char as char no-undo.
def var lv-num as char no-undo init '0123456789'.
def var lv-alpha as char no-undo init 'abcdefghijklmnopqrstuvwxyz'.
def var x as int no-undo.
if pv-allownumeric then lv-alpha = lv-alpha + lv-num.

do x = 1 to length(pv-str):
   lv-char = substring(pv-str,x,1).
   if index(lv-alpha,lv-char) > 0
   then pv-out = pv-out + lv-char.  
end.

return pv-out.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixPath Procedure 
FUNCTION FixPath returns character
  ( pv-path as char ) :
   &if "{&OPSYS}" = "win32"
        &then pv-path = replace(pv-path,'~/','~\').
        &else pv-path = replace(pv-path,'~\','~/').
   &endif

  return pv-path.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FrameChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FrameChanged Procedure 
FUNCTION FrameChanged returns logical
  ( pv-frame as handle) :
    def var lv-changed    as log    no-undo.
    run Proc-FrameChanged (pv-frame,output lv-changed).
    return lv-changed.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetAttribute Procedure 
FUNCTION GetAttribute returns character
  ( h-wid    as handle,
    pv-param as char ) :

def var pv-value as char   no-undo.
def var hcall as handle no-undo.

create call hCall. 
assign
    hCall:IN-HANDLE = h-wid
    HCALL:CALL-TYPE = get-attr-call-type 
    hCall:CALL-NAME = pv-param.
 
hCall:INVOKE. 

pv-value = hCall:RETURN-VALUE.

delete procedure hCall:IN-HANDLE. 
delete object hCall. 

  return pv-value.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetButPos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetButPos Procedure 
FUNCTION GetButPos returns character
  ( pv-frame as handle) :

    def var lv-wid-handle as handle no-undo.
    def var x as int no-undo.
    def var y as int no-undo.

    assign         
        lv-wid-handle = pv-frame:first-child
        lv-wid-handle = lv-wid-handle:first-child.
        
    do while valid-handle(lv-wid-handle):
        assign
            x = max(x,lv-wid-handle:x)
            y = max(y,(lv-wid-handle:y + lv-wid-handle:height-pixels)).
        lv-wid-handle = lv-wid-handle:next-sibling.
    end.       

  return string(1) + '{&Delim2}' + string(y + 1).   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetComboKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetComboKey Procedure 
FUNCTION GetComboKey returns character
 (pv-handle as widget-handle):
/*------------------------------------------------------------------------------
  Purpose: Returns a private data entry for a widget based on screen value  
    Notes:  
------------------------------------------------------------------------------*/
def var lv-pos as int no-undo.
      
if can-query(pv-handle,'list-item-pairs')
then do:      
    lv-pos = LOOKUP(pv-handle:SCREEN-VALUE,pv-handle:LIST-ITEM-pairs,'{&ComboDelim}').
    if lv-pos > 0 
        then return ENTRY(lv-pos,pv-handle:LIST-ITEM-pairs,'{&ComboDelim}').
        else return ' '.
end.
else do:
    lv-pos = LOOKUP(pv-handle:SCREEN-VALUE, pv-handle:LIST-ITEMS,'{&ComboDelim}').
    if lv-pos > 0 
        then return ENTRY(lv-pos, pv-handle:PRIVATE-DATA,'{&ComboDelim}').
        else return ' '.
end.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetDlcBin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetDlcBin Procedure 
FUNCTION GetDlcBin returns character
  ( /* parameter-definitions */ ) :
def var lv-dlc as char format 'x(20)' no-undo.

&if "{&OPSYS}" ne 'unix'  &then   
   get-key-value section "startup" key "dlc" value lv-dlc.
   lv-dlc = lv-dlc + "\\bin\\".
&else lv-dlc = OS-GETENV("DLC") + '/bin/'.
&endif

  return lv-dlc.   /* Function return value. */


end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetExcelColumnName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetExcelColumnName Procedure 
FUNCTION GetExcelColumnName returns character
  ( lv-col as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
def var lv-r as char no-undo.
def var lv-letter as char no-undo init 'a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z'.

    lv-r = if lv-col < 27 then entry(lv-col,lv-letter) 
                          else entry(int(truncate(lv-col / 27,0)),lv-letter) + 
                               entry((lv-col mod 26) + 1,lv-letter) no-error.

   if error-status:error 
   then message lv-col skip 
                lv-r.

  return lv-r.   /* Function return value. */


end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFieldValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldValue Procedure 
FUNCTION GetFieldValue returns character
  ( pv-buffer as handle,
    pv-fieldname as char,
    pv-extent as int) :
           
    def var z        as int no-undo.
    def var lv-value as char no-undo.
    def var h-fld    as handle no-undo.
    z = 1.
    do z = 1 to pv-buffer:num-fields:
        h-fld = pv-buffer:buffer-field(z).
        if h-fld:name = pv-fieldname then do:
            if pv-extent > 0 
            then lv-value = h-fld:buffer-value(pv-extent) no-error.
            else lv-value = h-fld:buffer-value no-error.
            if error-status:error then do:
                ErrorCreate(999,'GetFieldValue',pv-buffer:name,pv-fieldname,'').
            end.
            leave.
        end.
    end.

  return lv-value.   /* Function return value. */
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFileName Procedure 
FUNCTION GetFileName returns character
  ( pv-FullPath as char) :

def var lv-file  as char no-undo.
def var lv-delim as char no-undo.

&if "{&OPSYS}" = "win32"
&THEN lv-delim = '~\'.
&else lv-delim = '~/'.
&endif

pv-fullpath = fixpath(pv-fullpath) .

lv-file = if num-entries(pv-fullpath,lv-delim) > 1
            then entry(num-entries(pv-fullpath,lv-delim),pv-fullpath,lv-delim)
            else pv-fullpath.

    
  return lv-file.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFullPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFullPath Procedure 
FUNCTION GetFullPath returns character
  ( pv-file as char) :

assign
    file-info:filename = pv-file
    pv-file = file-info:full-pathname.
    
  return pv-file.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetHdr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetHdr Procedure 
FUNCTION GetHdr returns character
  ( pv-handle as handle) :

    def var lv-text as char no-undo init 'Not in System : '.
    def var lv-icon as char no-undo.

/*     lv-icon = getctrl("{&general-icon}").   */
/*     if lv-icon ne '' and                    */
/*        SEARCH(lv-icon) ne ?                 */
/*     THEN current-window:load-icon(lv-icon). */

/*     if getctrl("{&Change-Title}") = 'yes' */
/*     THEN DO:                              */
/*         lv-text = AltLanguage(ProgramTitle(pv-handle:private-data)).  */
lv-text = 
/* 'User: ' + getsysvar('{&clv}user') + ' Practice: ' + getsysvar('{&clv}practice') + */
/*           ' ' +                                                        */
ProgramTitle(string(pv-handle),'program').
/*     END.  */
return lv-text.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetIniValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetIniValue Procedure 
FUNCTION GetIniValue returns character
  ( input pv-section as char,
    input pv-key as char) :
    def var pv-value   as char no-undo.
    
    run Proc-GetIniValue(pv-section,pv-key,output pv-value).

return pv-value.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLockingCulprit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetLockingCulprit Procedure 
FUNCTION GetLockingCulprit returns character
  (tableRecid    as recid) :

   def var lv-locker as char no-undo init 'Unknown'.

 /*   find _lock where _lock._lock-recid = int(tableRecid) no-lock no-error. */
/*    if avail _lock */
/*    then lv-locker = _lock._lock-name */. 
   
   return lv-locker.   
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetNamedValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetNamedValue Procedure 
FUNCTION GetNamedValue returns character
  ( pv-name as char,
    pv-list as char ) :
/*------------------------------------------------------------------------------
  Purpose:  returns the entry for pv-name in pv-list 
    Notes:  eg copies=1,orientation=lansdacpe
------------------------------------------------------------------------------*/
def var lv-value as char no-undo.
def var lv-item as char no-undo.
def var x as int no-undo.

    do x = 1 to num-entries(pv-list):
        lv-item  = entry(1,entry(x,pv-list),'=').
            
        if lv-item = pv-name 
        then do:
            lv-value = entry(2,entry(x,pv-list),'=').
            leave.
        end.
    end.
   
  return lv-value.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetServerValueFor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetServerValueFor Procedure 
FUNCTION GetServerValueFor returns character
  ( pv-var as char) :
  
    def var lv-value as char no-undo.
    
   {{&core}run.i &program   = "zen-control.p"       
               &path      = "{&core}{&srv}"
               &Appsrv   = "system"
               &noper     = true            
               &procedure = "sendservervalue"             
               &params    = "(pv-var,output lv-value)"}

  return lv-value.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetStringEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetStringEntry Procedure 
FUNCTION GetStringEntry returns character
 ( pv-item as char,
   pv-properties as char,
   pv-values as char,
   pv-delim as char ) :
/*
returns value of item in values dependant on                                                                                           
item = 'name' properties = 'id,num,name,add'
values = 'a1,10,smith,1 est street'.
delim = ','   
will return 'smith'
really superceded by GetNamedValue()
*/

  return entry(lookup(pv-item,pv-properties,pv-delim),pv-values,pv-delim).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSystemName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetSystemName Procedure 
FUNCTION GetSystemName returns character
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
def var lv-sys as char no-undo.

/* local,.,sys,nfa
lv-ininame dir lv-path

case num-entries(session:parameter,'^'):
    when > 3 then lv-sys = entry(4,session:parameter,'^').
    when = 3 then
end case.
*/

if num-entries(session:parameter,'^') > 3 
then lv-sys = entry(4,session:parameter,'^').
else if session:param ne ''
      then lv-sys = GetIniValue('AvailableSystems','SystemName').
      else lv-sys = 'unknown'.

  return  lv-sys.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetWidHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetWidHandle Procedure 
FUNCTION GetWidHandle returns handle
  ( pv-frame       as handle,
    pv-widname     as char) :

def var lv-wid-handle as handle no-undo.
def var lv-rethand    as handle no-undo.
def var lv-exc as char no-undo.

assign 
    lv-wid-handle = pv-frame:first-child
    lv-wid-handle = lv-wid-handle:first-child.
    
if num-entries(pv-widname,';') > 1 
then assign lv-exc = entry(2,pv-widname,';')
            pv-widname = entry(1,pv-widname,';').
else lv-exc = ''.   

do while valid-handle(lv-wid-handle):  
    if lv-wid-handle:type = 'frame'
    then do:
        lv-rethand = getwidhandle(lv-wid-handle,pv-widname).
        if valid-handle(lv-rethand) then return lv-rethand.
    end.
    else do:  /* a particular widget name */
      if not pv-widname begins '*' 
      then do: 
         if lv-wid-handle:name = pv-widname
            then return lv-wid-handle.
      end.
      else do: /* *datatype;except these fields */                 
         if not can-do(lv-exc,lv-wid-handle:name)
         then do:
            if can-query(lv-wid-handle,'data-type') 
            then if lv-wid-handle:data-type = substring(pv-widname,2)
                 then return lv-wid-handle.
         end.
      end.
    end.
    lv-wid-handle = lv-wid-handle:next-sibling.
end.        
return ?.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InDevMode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InDevMode Procedure 
FUNCTION InDevMode returns logical
  ( /* parameter-definitions */ ) :
    def var lv-wid-handle            as handle no-undo.
    lv-wid-handle = session:first-procedure.
    do while valid-handle(lv-wid-handle):     
      if lv-wid-handle:filename = 'adeuib/_semain.w' then return true.
      lv-wid-handle = lv-wid-handle:next-sibling.
    end.

  return false.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InputFromFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InputFromFile Procedure 
FUNCTION InputFromFile returns memptr
  ( pv-filename as char,
    pv-local as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
def var lv-mfile as memptr no-undo.

set-size(lv-mfile) = 0.  /* new line 12/29/09 AMD */ 

if pv-local = 'local' then do:
    if search(pv-filename) ne ? 
    then do:
        pv-filename = search(pv-filename).
        file-info:file-name = pv-filename.              
        set-size(lv-mfile) = file-info:file-size.       
        copy-lob file pv-filename to lv-mfile.
    end.

/*                                                      */
/*     input from value(pv-filename) binary no-convert. */
/*     import lv-mfile.                                 */
/*     input close.                                     */
end.
else do:
  {{&core}run.i &program   = "dynamic.p"       
               &path      = "{&core}{&srv}"
               &Appsrv    = "System"      
               &procedure = "Proc-InputFromFile"             
               &params    = "(pv-filename,
                              output lv-mfile)"}
end.


  return lv-mfile.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IntegerTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION IntegerTime Procedure 
FUNCTION IntegerTime returns integer
  ( pv-time as char ) :
/* convert char string hh:mm to valid integer time */   
   return (INT(SUBSTRING(pv-time,1,2)) * 3600) + 
          (INT(SUBSTRING(pv-time,4,2)) * 60).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IntToHex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION IntToHex Procedure 
FUNCTION IntToHex returns character
(input i as int): 
   /* only for 0..255 integer values */
   def var cHex as char no-undo init '0123456789ABCDEF':U.
   def var j1   as int no-undo.
   def var j2   as int no-undo.
 
   j1 = TRUNCATE(i / 16, 0) .
   j2 = i - (j1 * 16).
   return SUBSTR(cHex, j1 + 1, 1) + SUBSTR(cHex, j2 + 1, 1).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsInteger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION IsInteger Procedure 
FUNCTION IsInteger returns logical
      (pv-Value as char):

   def var lv-int as int    no-undo.


   lv-int = int(pv-value) no-error.
   return not error-status:error.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsNull) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION IsNull Procedure 
FUNCTION IsNull returns logical
    ( pv-value as char ) :

    return CAN-DO("{&Null-String}",pv-value).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsNumeric) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION IsNumeric Procedure 
FUNCTION IsNumeric returns logical
    ( pv-Value as char ) :

def var lv-dec as dec no-undo.

lv-dec = DEC(pv-value) no-error.
return not error-status:error.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION IsRunning Procedure 
FUNCTION IsRunning returns logical
  (input pv-proc as char) :
/*------------------------------------------------------------------------------
  Purpose: is a procedure already running only really usefull in tty 
           uses private data in frame name to identify running procedures 
    Notes:  
------------------------------------------------------------------------------*/   
   return valid-handle(getprochandle('local',pv-proc)). 

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Jumpto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Jumpto Procedure 
FUNCTION Jumpto returns handle
  ( pv-widname as char) :

    def var lv-wid-handle           as handle no-undo.

    lv-wid-handle = GetWidhandle(current-window:first-child,
                                 pv-widname). 

    if valid-handle(lv-wid-handle) then do:
        if lv-wid-handle:type ne 'rectangle' and 
           can-query(lv-wid-handle,'sensitive') 
        then do:
           if lv-wid-handle:sensitive
               then do:
                    if lv-wid-handle:type = 'button'
                    then apply 'choose' to lv-wid-handle. 
                    else apply 'entry' to lv-wid-handle.
                    return lv-wid-handle.
           end.
        end.
    end.
return ?.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDay Procedure 
FUNCTION LastDay returns integer
    (lv-date as date):

    def var lv-enddate as date no-undo.

    /* find last day of month returned as date  */

lv-enddate = ((DATE(MONTH(lv-date),28,YEAR(lv-date)) + 4) - 
        DAY(DATE(MONTH(lv-date),28,YEAR(lv-date)) + 4)).

    return day(lv-enddate).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadDefBackGround) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadDefBackGround Procedure 
FUNCTION LoadDefBackGround returns logical
  ( pv-name  as char,
    pv-frame as handle,
    pv-win   as handle ) :

    run Proc-LoadDefBackGround(pv-name,pv-frame,pv-win).

return true.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogicalAnd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LogicalAnd Procedure 
FUNCTION LogicalAnd returns integer
    (pi1 as int, 
     pi2 as int ):
def var i1 as int extent 32 init 0 no-undo.
def var i2 as int extent 32 init 0 no-undo.
def var iIdx as int no-undo.
def var iResult as int no-undo.

do iIdx = 1 to extent(i1):
    assign i1[iIdx] = pi1 mod 2
           pi1 = truncate( pi1 / 2, 0 ).
end.

do iIdx = 1 to extent(i2):
    assign i2[iIdx] = pi2 mod 2
           pi2 = truncate(pi2 / 2,0).
end.

do iIdx = 1 to extent(i1):
    if i1[iIdx] = 1 and
       i2[iIdx] = 1 
    then iResult = iResult + exp( 2, iIdx - 1 ).
end.

return iResult. 


end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MonthName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MonthName Procedure 
FUNCTION MonthName returns character
    (ip-date        as date):

    return AltLanguage(entry(month(ip-date),"January,February,March,April,May,June,July,August,September,October,November,December")).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NumRecords) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NumRecords Procedure 
FUNCTION NumRecords returns integer
  (pv-display as char,
   pv-data as handle   ) :
/*------------------------------------------------------------------------------
  Purpose: return number of records in temp-table 
    Notes:  
------------------------------------------------------------------------------*/
    def var h-buffer as handle no-undo.
    def var h-query  as handle no-undo.
    def var lv-file as char no-undo.

    if not valid-handle(pv-data) then do:
        if pv-display ne '' then 
            message pv-display skip ' has no table created'.
        return 0.
    end.
    
    create buffer h-buffer for table pv-data.
/*  h-buffer = pv-data:default-buffer-handle.  */
    lv-file = h-buffer:table + '-' + pv-display + '.xml'.
    
    if pv-display begins 'dump' 
    then do:
        pv-data:WRITE-XML ('file',lv-file,true).
        return ?.
    end.
    else do:
        create query h-query.
        h-query:Add-buffer(h-buffer).
        h-query:query-prepare('preselect each '  + h-buffer:table + ' where true no-lock').
        h-query:query-open.
        h-query:get-last().
        if pv-display ne ''
        then message pv-display skip 
                     h-buffer:table ' has ' h-query:num-results ' records'.
     end.                         

  return h-query:num-results.   /* Function return value. */


end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutputToFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OutputToFile Procedure 
FUNCTION OutputToFile returns character
  ( pv-file  as char,
    pv-mfile as memptr,
    pv-local as char) :

    if pv-local = 'local' then do:
       if get-size(pv-mfile) = 0 then touch(pv-file). /* generallibrary.p */
       else do:
          copy-lob pv-mfile to file pv-file no-error.
          if error-status:error then return error
             '** ' + string(error-status:get-message(error-status:num-messages)).
          /*
          output stream op to value(pv-file) binary no-convert.
          file-info:file-name = pv-file.
          pv-file = file-info:full-pathname.
          /* PUT stream op CONTROL NULL. /* should force passthrough printing */  */
          export stream op pv-mfile.
          output stream op close.
          */
       end. /* non-zero file size */
    end.
    else do:
      {{&core}run.i &program   = "dynamic.p"       
                   &path      = "{&core}{&srv}"
                   &Appsrv    = "System"      
                   &procedure = "Proc-OutputToFile" /* in generallibrary.p */             
                   &params    = "(input-output pv-file,
                                  pv-mfile)"}
    end. /* not local */
    
    return pv-file.   /* Function return value. */
    
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutputToPdf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OutputToPdf Procedure 
FUNCTION OutputToPdf returns character
  ( pv-txtfile as char ) :

def var PDFCreator as com-handle no-undo.
def var opt as com-handle no-undo.
def var lv-origprt as char no-undo.
def var lv-pdf as char no-undo.
 
create 'PDFCreator.clsPDFCreator' PDFCreator connect no-error.
if error-status:error
then create 'PDFCreator.clsPDFCreator' PDFCreator no-error.

    PDFCreator:cClearCache().
assign
    file-info:file-name = pv-txtfile
    pv-txtfile          = file-info:full-pathname
    opt = PDFCreator:cOptions
    opt:autosavedirectory = session:temp-directory
    opt:AutosaveFormat = 0
    opt:AutosaveFilename = entry(num-entries(pv-txtfile,chr(92)),entry(1,pv-txtfile,'.'),chr(92))
    opt:UseAutosave = 1
    opt:UseAutosaveDirectory = 1
    PDFCreator:cOptions = opt 
    lv-origprt = pdfcreator:cDefaultPrinter
    pdfcreator:cDefaultPrinter = "PDFCreator".
    
    pdfcreator:cSaveOptions().
    wait(500). /* give options time to settle */
    
    lv-pdf = opt:autosavedirectory + opt:autosavefilename + '.pdf'.
    os-delete value(lv-pdf).
    PDFCreator:cPrintFile(pv-txtfile).  
    etime(true).
    file-info:file-name = lv-pdf.
    do while file-info:file-size = ?:
     wait(500).
     file-info:file-name = lv-pdf.
     if etime > 10000 then do:
        message 'pdf create may have failed ' + lv-pdf.
        leave.
     end.
    end.
    wait(1000).
assign
    PDFCreator:cPrinterStop = false
    PDFCreator:cPrinterStop = true
    pdfcreator:cDefaultPrinter = lv-origprt.
    
release object pdfcreator.
  
  return lv-pdf.   /* Function return value. */
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutPutToScreen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OutPutToScreen Procedure 
FUNCTION OutPutToScreen returns logical
  ( pv-file as char  ) :
def var lv-wripath     as char   no-undo.
    load "wrifile" base-key "hkey_classes_root".
    use "wrifile".
    GET-KEY-VALUE SECTION 'shell\open\command' 
                  KEY default
                  VALUE lv-wripath.
    unload 'wrifile'.
    lv-wripath = replace(lv-wripath,'%1',pv-file).
    win-exec(lv-wripath,1).
  return false.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PrintBrowse Procedure 
FUNCTION PrintBrowse returns logical
  ( pv-handle as handle,
    pv-title as char ) :
if not Valid-handle(pv-handle) then return error.

run proc-printBrowse(pv-handle,pv-title).


    return not error-status:error.


end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProperForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ProperForm Procedure 
FUNCTION ProperForm returns character
  ( pv-string as char ) : 
  
  def var n         as int  no-undo.
  def var lv-proper as char no-undo.
  pv-string = lc(pv-string).
        
  do n = 1 to num-entries(pv-string,' '):
      lv-proper = lv-proper + CAPS(SUBSTRING(ENTRY(n,pv-string,' '),1,1)) +
                         SUBSTRING(ENTRY(n,pv-string,' '),2,LENGTH(ENTRY(n,pv-string,' '))) + ' '.
  end.
  
  return lv-proper.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReLabelBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReLabelBrowse Procedure 
FUNCTION ReLabelBrowse returns logical
  ( br-hand       as handle,
    lv-dateformat as char ) :

  run Proc-RelabelBrowse (br-hand,lv-dateformat).

  return true.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RunChild) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RunChild Procedure 
FUNCTION RunChild returns handle
  ( pv-prog as char,
    pv-parent as handle ) :
  
  def var lv-h as handle no-undo.

 if filenotfound(pv-prog)
  then do:
    message "Invalid Program." skip
            pv-prog 
    view-as alert-box error.
    return error ?.
 end.

  /* check if prog is allowed to be run multiple instances */
   lv-h = getprochandle('local',pv-prog).
   if valid-handle(lv-h)
   then do:
      if not PgmMultiInstance(pv-prog) 
      then do:
         message msg(158,pgmproperty(pv-prog,'name'),'','','')
         view-as alert-box information.
      end.
      else lv-h = ?.
   end.

  if not valid-handle(lv-h) 
  then do:
     run value(pv-prog) persist set lv-h.
     if valid-handle(lv-h) 
     then do:
         LogAction(program-name(2),if valid-handle(focus) then focus:name else '','Run ' + pv-prog).
         run SubscribeToAll in pv-parent (lv-h,'Parent') no-error.
         run SubscribeToAll in lv-h (pv-parent,'Child') no-error.
         run Initialise in lv-h no-error.
         run Initial-Action in lv-h no-error.
     end.
  end.    
  else run update-child-procedures in pv-parent (lv-h) no-error.

  return lv-h.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SelectedItems) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SelectedItems Procedure 
FUNCTION SelectedItems returns character
  ( pv-hand as handle ) :
   
   def var lv-list as char   no-undo.
   def var v-cnt     as int  no-undo.

   do v-cnt = 1 to pv-hand:num-items:
      if pv-hand:is-selected(v-cnt) then lv-list = 
         lv-list + entry(v-cnt,pv-hand:list-items,"{&comboDelim}") + ','.
   end.
   
   if lv-list ne '' then
      lv-list = substring(lv-list,1,length(lv-list) - 1).

   return lv-list.   /* Function return value. */
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetAllLkBut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetAllLkBut Procedure 
FUNCTION SetAllLkBut returns logical
  ( pv-frame   as handle) :
def var lv-wid-handle as handle no-undo.
def var lv-h as handle no-undo.

assign 
   lv-wid-handle = pv-frame:first-child
   lv-wid-handle = lv-wid-handle:first-child.

do while valid-handle(lv-wid-handle):
   if lv-wid-handle:type = 'frame' 
      then SetallLkBut(lv-wid-handle).
   else if lv-wid-handle:type = 'button' then do: 
      lv-h = widget-handle(lv-wid-handle:private-data).

      if valid-handle(lv-h) then
         assign 
            lv-wid-handle:sensitive = lv-h:sensitive
            lv-wid-handle:hidden    = not lv-h:sensitive.
    end. 
    lv-wid-handle = lv-wid-handle:next-sibling.
end.
return true.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetAttributes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetAttributes Procedure 
FUNCTION SetAttributes returns character
  ( h-wid    as char,
    pv-prop  as char,
    pv-value as char,
    pv-datatype  as char ) :

def var hcall       as handle no-undo.
def var x           as int    no-undo.
def var lv-prop     as char   no-undo.
def var lv-datatype as char   no-undo.
def var lv-value    as char   no-undo.

create call hCall. 
assign
    hCall:IN-HANDLE = h-wid 
    HCALL:CALL-TYPE = set-attr-call-type 
    hCall:NUM-PARAMETERS = 1. 

do x = 1 to num-entries(pv-prop):
    assign
        lv-prop     = entry(x,pv-prop)
        lv-datatype = entry(x,pv-datatype)
        lv-value    = entry(x,pv-value)
        hCall:CALL-NAME = lv-prop.

    case lv-datatype:
        when 'Char' then hCall:SET-PARAMETER(1,lv-datatype,"INPUT",lv-value). 
        when 'Dec'  then hCall:SET-PARAMETER(1,lv-datatype,"INPUT",StringToDec(lv-value,',','.')). 
        when 'Int'  then hCall:SET-PARAMETER(1,lv-datatype,"INPUT",StringToInt(lv-value,',')). 
        when 'Log'  then hCall:SET-PARAMETER(1,lv-datatype,"INPUT",StringToLog(lv-value)). 
        when 'Hand' then hCall:SET-PARAMETER(1,lv-datatype,"INPUT",widget-handle(lv-value)). 
    end case.
    
    hCall:INVOKE. 
    delete procedure hCall:IN-HANDLE. 
end.
delete object hCall. 

  return "".   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetAuditMode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetAuditMode Procedure 
FUNCTION SetAuditMode returns logical
  ( pv-table as char,
    pv-mode as char) :

   def var lv-mode as log no-undo.

   if pv-table = 'all' then setsysvar('{&AuditSystemStatus}',pv-mode).
   else do:
      lv-mode = pv-mode = 'on'.
      {{&core}run.i &program   = "Zen-AuditConfig.p"       
                    &path      = "{&aud}{&srv}"
                    &Appsrv    = "system"
                    &procedure = "SetAudit"             
                    &params    = "(pv-table,
                                   lv-mode)"}
   end.
                               
  return false.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetComboValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetComboValue Procedure 
FUNCTION SetComboValue returns character
  (input pv-key   as char,
   input pv-combo as handle):

  def var lv-value as char no-undo.
if can-query(pv-combo,'list-item-pairs')
then do:      
  lv-value = if lookup(pv-key,pv-combo:LIST-ITEM-pairs,"{&ComboDelim}") = 0 
            then entry(1,pv-combo:LIST-ITEM-pairs,"{&ComboDelim}")
            else ENTRY(LOOKUP(pv-key,pv-combo:LIST-ITEM-pairs,"{&ComboDelim}"),
                               pv-combo:LIST-ITEM-pairs,"{&ComboDelim}") no-error.
end.
else do:
  lv-value = if lookup(pv-key,pv-combo:private-data,"{&ComboDelim}") = 0 
            then entry(1,pv-combo:list-items,"{&ComboDelim}")
            else ENTRY(LOOKUP(pv-key,pv-combo:PRIVATE-DATA,"{&ComboDelim}"),
                               pv-combo:LIST-ITEMS,"{&ComboDelim}") no-error.
end.
  pv-combo:screen-value = lv-value.
  
  return lv-value.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetCursor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetCursor Procedure 
FUNCTION SetCursor returns logical
  ( pv-handle as handle,
    pv-state as char) :

if pv-state = '' or 
   pv-state = 'wait' /* in generallibrary.p */
then return pv-handle:set-wait-state(pv-state). /* in generallibrary.p */
else return pv-handle:load-mouse-pointer(pv-state).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetFrameFocus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetFrameFocus Procedure 
FUNCTION SetFrameFocus returns handle
  ( pv-frame-handle as handle ) :

def var lv-wid-handle           as handle no-undo.
def var lv-prg                  as char   no-undo.

if Can-set(pv-frame-handle, "selected") 
then pv-frame-handle:selected = true.

assign
    lv-wid-handle = pv-frame-handle
    lv-wid-handle = lv-wid-handle:first-child
    lv-wid-handle = lv-wid-handle:first-child.

get-wid:
do while valid-handle(lv-wid-handle):  
    if lv-wid-handle:type ne 'rectangle' and 
        can-query(lv-wid-handle,'sensitive') and 
        can-query(lv-wid-handle,'tab-stop')
    then 
        if lv-wid-handle:sensitive and
           lv-wid-handle:tab-stop
        then do:     
            if lv-wid-handle:type = 'browse' 
            then do:
                if lv-wid-handle:num-iterations > 0 
                then leave get-wid.
            end.    
            else leave get-wid.     
        end.
    lv-wid-handle = lv-wid-handle:next-sibling.
end.  
/*      
lv-wid-handle:selected = true no-error.
if not error-status:error 
then  */
pv-frame-handle:move-to-top() no-error.
if valid-handle(lv-wid-handle)
then apply 'entry' to lv-wid-handle.

return lv-wid-handle.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetIniValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetIniValue Procedure 
FUNCTION SetIniValue returns logical
  ( input pv-section as char,
    input pv-key     as char,
    input pv-value   as char) :
    
    run Proc-SetIniValue(pv-section,pv-key,pv-value).
    
return return-value ne 'failed'.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetLkBut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetLkBut Procedure 
FUNCTION SetLkBut returns logical
  ( pv-frame   as handle,
    pv-widhand as handle,   
    pv-mode    as log) :
   def var lv-wid-handle as handle no-undo.
   
   
   assign 
      lv-wid-handle = pv-frame:first-child
      lv-wid-handle = lv-wid-handle:first-child.
   
   do while valid-handle(lv-wid-handle):
      if lv-wid-handle:type = 'frame' then do:
         if SetLkBut(lv-wid-handle,pv-widhand,pv-mode)
            then return true.
      end.
      else if lv-wid-handle:type = 'button' then do: 
         if valid-handle(widget-handle(lv-wid-handle:private-data)) and
            widget-handle(lv-wid-handle:private-data) = pv-widhand  
         then do:
            assign 
               lv-wid-handle:sensitive = pv-mode
               lv-wid-handle:visible = pv-mode.
            return true.
         end.
       end. 
       lv-wid-handle = lv-wid-handle:next-sibling.
   end.
   return false.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetNamedValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetNamedValue Procedure 
FUNCTION SetNamedValue returns character
  ( pv-name as char,
    pv-value as char,
    pv-list as char ) :
/*------------------------------------------------------------------------------
  Purpose:  set the entry for pv-name in pv-list 
    Notes:  eg copies=1,orientation=lansdacpe
------------------------------------------------------------------------------*/
def var lv-item as char no-undo.
def var x as int no-undo.

  do x = 1 to num-entries(pv-list):
    lv-item  = entry(1,entry(x,pv-list),'=').
    if lv-item = pv-name 
    then do:
        entry(x,pv-list) = lv-item + '=' + pv-value.
        return pv-list.
    end.
  end.
  pv-list = pv-list + ',' + pv-name + '=' + pv-value.
  if pv-list begins ',' then pv-list = substring(pv-list,2). 
  
  return pv-list.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetNotModified) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetNotModified Procedure 
FUNCTION SetNotModified returns logical
  ( pv-frame as handle):
    def var lv-wid-handle as handle no-undo.
    assign         
        lv-wid-handle = pv-frame:first-child
        lv-wid-handle = lv-wid-handle:first-child.

    do while valid-handle(lv-wid-handle):
        if lv-wid-handle:type = 'frame' 
            then Setnotmodified(lv-wid-handle).
        if can-set(lv-wid-handle,'modified') 
            then lv-wid-handle:modified = false. 
        lv-wid-handle = lv-wid-handle:next-sibling.
    end.
        
  return true.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetOpDest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetOpDest Procedure 
FUNCTION SetOpDest returns character
  ( input-output pv-param as char ) :

   def var lv-ok as log no-undo init true.

  run proc-setopdest(input-output pv-param).
  lv-ok = not pv-param begins '**Failed'. 
  
  return string(lv-ok).   /* Function return value. */
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetRegValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetRegValue Procedure 
FUNCTION SetRegValue returns logical
  ( input pv-section as char,
    input pv-key     as char,
    input pv-value   as char) :
    
def var x as int no-undo.

do x = 1 to num-entries(pv-key,'{&Delim3}'):
    put-key-value section pv-section key entry(x,pv-key,'{&Delim3}') 
        value entry(x,pv-value,'{&Delim3}') no-error.
end.
    
return error-status:error.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSession Procedure 
FUNCTION SetSession returns logical
  ( pv-state as char) :

  return setcursor(session,pv-state).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetWinState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetWinState Procedure 
FUNCTION SetWinState returns logical
  ( pv-win-handle as handle,
    pv-state      as int ) :

/*------------------------------------------------------------------------------
  Purpose:  set window state   1 = max 2 = min 3 = normal
------------------------------------------------------------------------------*/
if session:display-type = 'gui' then 
        pv-win-handle:window-state = pv-state.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StringToDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StringToDate Procedure 
FUNCTION StringToDate returns date
  (lv-string as char):
  
  def var lv-date  as date no-undo format '99/99/9999'.
  def var lv-sdate as date no-undo.
  run Proc-StringToDate (lv-string,output lv-date,output lv-sdate).

  if length(lv-string) > 9 
    then return lv-date.
    else return lv-sdate.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StringToDec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StringToDec Procedure 
FUNCTION StringToDec returns decimal
  ( pv-string as char,
    pv-sep as char,
    pv-point as char) :

  def var lv-dec   as dec  no-undo.
  def var lv-sep   as char no-undo.
  def var lv-point as char no-undo.
  assign
    lv-sep   = session:numeric-separator
    lv-point = session:numeric-decimal-point.
  session:set-numeric-format(pv-sep,pv-point).
  lv-dec = dec(pv-string).
  session:set-numeric-format(lv-sep,lv-point).
  
  return lv-dec.   /* Function return value. */
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StringToInt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StringToInt Procedure 
FUNCTION StringToInt returns integer
  ( pv-string as char,
    pv-sep as char) :

  def var lv-int   as int  no-undo.
  def var lv-sep   as char no-undo.
  def var lv-point as char no-undo.

  assign
    lv-sep   = session:numeric-separator
    lv-point = session:numeric-decimal-point.

  session:set-numeric-format(pv-sep,lv-point).
  lv-int = int(pv-string).
  session:set-numeric-format(lv-sep,lv-point).
  
  return lv-int.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StringToLog) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StringToLog Procedure 
FUNCTION StringToLog returns logical
    ( pv-String as char ) :

      if Can-do('{&log-true}',pv-string)
      then return true.
      
      if Can-do('{&log-false}',pv-string)
      then return false.

      return false.   /* Function return value. */


end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToLower) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ToLower Procedure 
FUNCTION ToLower returns logical
  ( pv-handle as handle ) :
   if not pv-handle:modified then return true.
   pv-handle:screen-value = lc(pv-handle:screen-value). 
  return true.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Touch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Touch Procedure 
FUNCTION Touch returns logical
  ( lv-fname as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   output to value(lv-fname). 
   put ''. 
   output close.

return true.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToUpper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ToUpper Procedure 
FUNCTION ToUpper returns logical
  ( pv-handle as handle ) :
   if not pv-handle:modified then return true.
   pv-handle:screen-value = caps(pv-handle:screen-value). 
  return true.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UnixPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UnixPath Procedure 
FUNCTION UnixPath returns character
  ( pv-string as char ) :
  def var lv-slash as char no-undo.
  
&if "{&OPSYS}" = "unix" 
    &then lv-slash = "\\".
    &else lv-slash = "~\".
&endif
  return replace(pv-string,lv-slash,"~/").

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidateDirectory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateDirectory Procedure 
FUNCTION ValidateDirectory returns logical
   ( pv-dir as char ) :

 def var lv-dir as char no-undo.

 &if "{&OPSYS}" = "win32" 
    &THEN pv-dir = dospath(pv-dir).
    &else pv-dir = unixpath(pv-dir).
 &endif

 file-info:filename = PV-DIR.

 if file-info:file-type ne ? 
 then return true.

 lv-dir = substring(pv-dir,1,r-index(pv-dir,'{&dirdelim}') - 1).
 
 os-create-dir value(pv-dir) no-error.
   
  return os-error = 0.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidUserSec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidUserSec Procedure 
FUNCTION ValidUserSec returns logical
  ( pv-notusers  as char,
    pv-notgroups as char,
    pv-runusers  as char,
    pv-rungroups as char,
    pv-user      as char,
    pv-group     as char ) :

    def var lv-can-run as log no-undo init true.

    if can-do(pv-notusers,pv-user) or
       can-do(pv-notgroups,pv-group)
    then lv-can-run = false.
    else if not can-do(pv-runusers,pv-user) or
            not can-do(pv-rungroups,pv-group)
            then lv-can-run = false.

  return lv-can-run.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Wait) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Wait Procedure 
FUNCTION Wait returns logical
  ( vi-milliseconds as int) :
&if "{&OPSYS}" = 'win32' 
    &then wapisleep(vi-milliseconds).
    &else  /* do it without api call */
          def var vi-etime as int no-undo.
          vi-etime = ETIME.
          do while ETIME < vi-etime + vi-milliseconds:
          end.
&endif.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidgetExists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WidgetExists Procedure 
FUNCTION WidgetExists returns logical
  ( pv-framehandle as handle,
    pv-wid as char ) :

  return valid-handle(GetWidHandle(pv-framehandle,pv-wid)).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WidInfo Procedure 
FUNCTION WidInfo returns character
  ( /* parameter-definitions */ ) :
def var v-lab   as char no-undo.
def var v-db    as char no-undo.
def var v-table as char no-undo.
def var v-wid   as char no-undo.
def var lv-ex   as char no-undo.
def var lv-parent as char no-undo.
def var lv-path as char no-undo. 
def var lv-repinfo as char no-undo.
def var x as int no-undo.
def var lv-curprog as handle no-undo.
assign
   v-lab   = if can-query(self,'label')  then self:label
                                         else ''
   v-db    = if can-query(self,'dbname') then self:dbname
                                         else ''
   v-table = if can-query(self,'table') then self:table
                                        else ''
   v-wid   = if can-query(self,'name')  then self:name
                                        else ''
   lv-ex   = if can-query(self,'index') then string(self:index)
                                        else ''                                     
   v-lab   = if v-lab   = ? then '?' else v-lab
   v-db    = if v-db    = ? then '?' else v-db
   v-table = if v-table = ? then '?' else v-table
   v-wid   = if v-wid   = ? then '?' else v-wid
   lv-curprog = widget-handle(current-window:private-data)
   lv-parent = pgmmenuparent(lv-curprog:name) 
   lv-repinfo = pgmrepinfo(lv-curprog:name).

   if lv-ex   = ? or
      lv-ex   = '?' or 
      lv-ex   = '0' 
   then lv-ex = ''.
   else lv-ex = '[' + lv-ex + ']'.
   

   do x = 1 to num-entries(lv-parent,','):
      lv-path = lv-path + ':' + entry(1,entry(x,lv-parent,','),'{&delim3}').
   end.
   lv-path = substring(lv-path,2).

   message 'Menu   ~t:' lv-path skip
           'Program~t:' lv-curprog:name skip
           'Author ~t:' PgmAuthor(lv-curprog:name) skip
           'DB     ~t:' v-db skip
           'Table  ~t:' v-table skip
           'Field  ~t:' v-wid lv-ex skip
           'Frame  ~t:' self:frame:name skip
           'Label  ~t:' v-lab skip
           'Lookup ~t:' GetLookupInfo(self,'name') skip
           'Report ~t:' entry(2,lv-repinfo,'{&Delim2}') +
                      entry(3,lv-repinfo,'{&Delim2}') + ' ' +
                      entry(4,lv-repinfo,'{&Delim2}') skip
           'Audited ~t:' AuditedField(v-table,v-wid)         
   view-as alert-box information title 'You are in '.

  return "".   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

