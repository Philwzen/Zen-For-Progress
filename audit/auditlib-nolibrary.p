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

/* {app-paths.i}  */
/* &glob justpaths */
{app-paths.i &justpaths = true}
{{&core}control.i}

/* &glob library-general    */
/* &glob library-cache      */
/* &glob library-winapi     */
/* &glob library-msoffice   */
/* &glob library-validation */
/*                          */
/* {{&core}loadlibraries.i} */
/*                          */
{{&aud}auditfunc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-GetFieldHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldHandle Procedure 
FUNCTION GetFieldHandle RETURNS HANDLE
  ( pv-buffer as handle,
    pv-fname as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSysVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetSysVar Procedure 
FUNCTION GetSysVar RETURNS CHARACTER
  ( pv-key as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUniqueId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetUniqueId Procedure 
FUNCTION GetUniqueId RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IntToHex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD IntToHex Procedure 
FUNCTION IntToHex RETURNS CHARACTER
(INPUT i as int) FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


PROCEDURE UuidCreate EXTERNAL "rpcrt4.dll":U :
    def input-OUTPUT param opi-guid AS CHAR NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Audit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Audit Procedure 
PROCEDURE Audit :
def input param b-before     As Handle No-undo.
Def input param b-after      As Handle No-undo.
DEF input param pv-action    AS char   NO-UNDO.
def input param pv-tablekey  as char   no-undo.
if pv-action = '' then pv-action = 'create'.
Def Var h-beforefield As Handle No-undo.
Def Var h-afterfield  As Handle No-undo.
def var lv-searchfield as char  no-undo.
Def Var X             As Int    no-undo.
def var y             as int    no-undo.
DEF VAR pv-AuditDetailKey AS DEC NO-UNDO.

IF b-after:new THEN pv-action = 'create'.

FIND FIRST Zen-AuditConfig WHERE Zen-AuditConfig.TableName = b-after:table
                           no-lock no-error.
if not avail Zen-AuditConfig then return.
/* If not can-find(FIRST Zen-Auditfield Of Zen-AuditConfig where zen-auditfield.active)  */
/* then return.                                                                          */

if zen-auditconfig.SearchFieldName ne '' then
do x = 1 to b-after:num-fields:
    h-afterfield = b-after:Buffer-field(X).
    if h-afterfield:name = zen-auditconfig.SearchFieldName
        then lv-searchfield = string(h-afterfield:buffer-value).
end.
else lv-searchfield = ?.
RUN WriteAuditDetail (b-after:table,
                      pv-tablekey,
                      pv-action,
                      lv-searchfield,
                      output pv-AuditDetailKey).

if pv-action = 'update' 
then do:                              
    If can-find(FIRST Zen-Auditfield where zen-auditfield.tablename = Zen-AuditConfig.tablename
                                    /* and zen-auditfield.active */ )
    then do x = 1 to b-before:num-fields:
        assign
            h-beforefield = b-before:Buffer-field(X).
            h-afterfield  = b-after:Buffer-field(X).
      /* we only bother with sensible fields no blobs recids etc */
        if not can-do('character,date,logical,decimal,integer,datetime',h-beforefield:data-type) 
         then next.
        If can-find(FIRST Zen-Auditfield where zen-auditfield.tablename = Zen-AuditConfig.tablename
                                           and zen-auditfield.fieldname = h-afterfield:name
                                           and zen-auditfield.active)
        Then DO:
            if h-beforefield:extent ne 0 
            then do y = 1 to h-beforefield:extent:
                if h-beforefield:Buffer-value[y] ne h-afterfield:Buffer-value[y]
                then RUN WriteAuditLine (pv-AuditDetailKey,h-beforefield,y).
            end.
            else do:
                if h-beforefield:Buffer-value ne h-afterfield:Buffer-value
                then RUN WriteAuditLine (pv-AuditDetailKey,h-beforefield,0).
            end.
        End.
    end.
end.
else do:
    RUN WriteAuditLine (pv-AuditDetailKey,
                        GetFieldHandle(b-before,zen-auditconfig.keyfield),0).
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WriteAuditDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WriteAuditDetail Procedure 
PROCEDURE WriteAuditDetail :
def input  param pv-TableName       AS CHAR NO-UNDO.
def input  param pv-SourceKey       AS char NO-UNDO.
def input  param pv-action          AS char  NO-UNDO.
def input  param pv-searchfield     as char no-undo.
DEF OUTPUT PARAM pv-AuditDetailKey  AS DEC  NO-Undo.
def var lv-auser as char no-undo.
lv-auser = getsysvar('user')  + '1'.
if lv-auser begins '**'
 then do:
   lv-auser = USERID(ldbname(1)) + '2'.
   if lv-auser = '' then lv-auser = 'System' + '3'.
 end.
        CREATE Zen-AuditDetail.

        ASSIGN Zen-AuditDetail.Byname      = lv-auser
               Zen-AuditDetail.AUDitDate   = TODAY
               Zen-AuditDetail.AUDitTime   = TIME
               Zen-AuditDetail.TableName   = pv-TableName
               Zen-AuditDetail.SourceKey   = pv-SourceKey
               zen-auditdetail.SearchFieldData = pv-searchfield
               zen-auditdetail.auditaction = pv-action.
        find current Zen-AuditDetail no-lock.
               pv-auditdetailkey = zen-auditdetail.zen-auditdetailtableid.
        release Zen-Auditdetail.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WriteAuditLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WriteAuditLine Procedure 
PROCEDURE WriteAuditLine :
def input param pv-AuditDetailKey AS DEC    NO-UNDO.
def input param pv-field          as handle no-undo.
def input param pv-extent         as int    no-undo. 

    CREATE Zen-AuditLine.
    ASSIGN zen-auditline.zen-auditdetailtableid = pv-AuditDetailKey.
if pv-extent = 0 
then do:
      Zen-AuditLine.FieldName      = pv-field:name.
        case pv-field:Data-Type:
        when 'character' then Zen-AuditLine.DataChar = substring(pv-field:buffer-value,1,100).
        when 'Date'      then Zen-AuditLine.DataDate = pv-field:buffer-value.
        when 'Decimal'   then Zen-AuditLine.DataDec  = pv-field:buffer-value.
        when 'Integer'   then Zen-AuditLine.DataInt  = pv-field:buffer-value.
        when 'logical'   then Zen-AuditLine.DataLog  = pv-field:buffer-value.
        when 'datetime'  then Zen-AuditLine.DataDateTime  = pv-field:buffer-value.
        otherwise Zen-AuditLine.DataChar = 'unknowntype ' + pv-field:Data-Type + substring(pv-field:buffer-value,1,100).
     end case.
end.
else do:
      Zen-AuditLine.FieldName      = pv-field:name + '[' + string(pv-extent) + ']'.
        case pv-field:Data-Type:
        when 'character' then Zen-AuditLine.DataChar = substring(pv-field:buffer-value[pv-extent],1,100).
        when 'Date'      then Zen-AuditLine.DataDate = pv-field:buffer-value[pv-extent].
        when 'Decimal'   then Zen-AuditLine.DataDec  = pv-field:buffer-value[pv-extent].
        when 'Integer'   then Zen-AuditLine.DataInt  = pv-field:buffer-value[pv-extent].
        when 'logical'   then Zen-AuditLine.DataLog  = pv-field:buffer-value[pv-extent].
        when 'datetime'  then Zen-AuditLine.DataDateTime  = pv-field:buffer-value[pv-extent].
        otherwise Zen-AuditLine.DataChar = 'unknowntype ' + pv-field:Data-Type + substring(pv-field:buffer-value[pv-extent],1,100).
     end case.
end.
    release Zen-Auditline.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetFieldHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldHandle Procedure 
FUNCTION GetFieldHandle RETURNS HANDLE
  ( pv-buffer as handle,
    pv-fname as char ) :
    Def Var X       As Int    no-undo.
    def var h-field as handle no-undo.

    do x = 1 to pv-buffer:num-fields:
        h-field = pv-buffer:Buffer-field(X).
        If h-field:name = pv-fname Then RETURN h-field.
    end.
    RETURN ?.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSysVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetSysVar Procedure 
FUNCTION GetSysVar RETURNS CHARACTER
  ( pv-key as char ) :
def var pv-value as char no-undo.
def var lv-id as char no-undo.
def var lv-uniqueid as char no-undo.

   lv-uniqueid = if opsys = 'win32' then GetUniqueId()
                                    else string(today) + string(time).
   if session:remote 
   then lv-id = SESSION:server-connection-context.
   else lv-id = string(today) + ':' + lv-uniqueid.

  find first zen-context where Zen-Context.server-connection-id = trim(lv-id)
                           and Zen-Context.type = pv-key 
                         no-lock no-error.
  if avail zen-context 
      then pv-value = Zen-Context.ContextValue.       
      Else pv-value = '** ' + pv-key + ' Not Found'.
  RETURN pv-value.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUniqueId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetUniqueId Procedure 
FUNCTION GetUniqueId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
   DEF VAR X AS CHAR NO-UNDO.
   DEF VAR i AS INT NO-UNDO.
   DEF VAR j AS INT NO-UNDO.
   DEF VAR r AS CHAR NO-UNDO.
 
   X = FILL(' ':U, 16).
 
/*    IF RunningWindows2000() THEN                   */
/*       RUN UuidCreateSequential (INPUT-OUTPUT X).  */
/*    ELSE                                           */
      RUN UuidCreate (INPUT-OUTPUT X).
 
   DO i = 11 TO 16:
      r = r + ' ':U + inttohex(ASC(SUBSTR(X,i,1))). 
   END.
   RETURN SUBSTR(R,2).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IntToHex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION IntToHex Procedure 
FUNCTION IntToHex RETURNS CHARACTER
(INPUT i as int): 
   /* only for 0..255 integer values */
   DEF VAR cHex AS CHAR NO-UNDO INIT '0123456789ABCDEF':U.
   DEF VAR j1   AS INT NO-UNDO.
   DEF VAR j2   AS INT NO-UNDO.
 
   j1 = TRUNCATE(i / 16, 0) .
   j2 = i - (j1 * 16).
   RETURN SUBSTR(cHex, j1 + 1, 1) + SUBSTR(cHex, j2 + 1, 1).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

