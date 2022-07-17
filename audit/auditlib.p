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
/* &glob library-general */
/* &glob library-cache */
/* &glob library-msoffice */
/* &glob library-validation */

{app-paths.i}
/* {app-paths.i justvars = true} */
/* {{&core}control.i} */


/* {{&core}loadlibraries.i &AsSuper = 'true'} */

{{&aud}auditfunc.i}

def var lv-auser as char no-undo.
def var lv-HasAuditedFields as log no-undo.
this-procedure:private-data = 'library-auditlib'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-FieldIsAudited) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FieldIsAudited Procedure 
FUNCTION FieldIsAudited RETURNS LOGICAL
  ( pv-table as char,
    pv-field as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFieldHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldHandle Procedure 
FUNCTION GetFieldHandle RETURNS HANDLE
  ( pv-buffer as handle,
    pv-fname as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TableIsAudited) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TableIsAudited Procedure 
FUNCTION TableIsAudited RETURNS LOGICAL
  ( pv-tablename as char,
    pv-user as char )  FORWARD.

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

/* new stuff for ecxlude a userid ot table from audit */
lv-auser = getsysvar('user').
if lv-auser begins '**'
 then do:
   lv-auser = USERID(ldbname(1)).
   if lv-auser = '' then lv-auser = 'System'.
 end.
 if not TableIsAudited(b-after:table,lv-auser) 
 then return.
/* end new stuff */

FIND FIRST Zen-AuditConfig WHERE Zen-AuditConfig.TableName = b-after:table
                             and Zen-AuditConfig.active
                           no-lock no-error.
if not avail Zen-AuditConfig then return.

case pv-action:
/* can add in usrid specific logic here need new fields on auditconfig records */
   when 'create' then if not zen-auditconfig.recordcreates then return.
   when 'delete' then if not zen-auditconfig.recorddeletes then return.
/*   when 'update' then */
end case.
/* mod below to allow multiple searchfields */
if zen-auditconfig.SearchFieldName ne '' then
do y = 1 to num-entries(zen-auditconfig.SearchFieldName,'|'):
    do x = 1 to b-after:num-fields:
        h-afterfield = b-after:Buffer-field(X).
        if h-afterfield:name = entry(y,zen-auditconfig.SearchFieldName,'|')
            then lv-searchfield = lv-searchfield + '|' + string(h-afterfield:buffer-value).
    end.
end.
else lv-searchfield = ?.
lv-searchfield = substring(lv-searchfield,2) no-error.

y = 1.
/* Create the Event header record */
RUN WriteAuditDetail (b-after:table,
                      pv-tablekey,
                      pv-action,
                      lv-searchfield,
                      output pv-AuditDetailKey).
lv-HasAuditedFields = no.
if pv-action = 'update' 
then do:                              
/*   do x = 1 to b-before:num-fields:      /* we only bother with sensible fields no blobs recids etc */
            if not can-do('character,date,logical,decimal,integer,datetime',
                     b-before:Buffer-field(zen-auditfield.fieldname):data-type) 
      then next.      */
      
  /* more efficient to only process fields with audit entries in table */
   for each Zen-Auditfield where zen-auditfield.tablename = b-after:table
                             and zen-auditfield.fieldname > ''
                             and zen-auditfield.active
                           no-lock:

      assign
         h-beforefield = b-before:Buffer-field(zen-auditfield.fieldname)
         h-afterfield  = b-after:Buffer-field(zen-auditfield.fieldname) no-error.
      if not valid-handle(h-beforefield) then next.
      if not can-do('character,date,logical,decimal,integer,datetime',h-afterfield:data-type) 
      then next.

      if h-beforefield:extent ne 0 /* its an array so check every element */
      then 
         do y = 1 to h-beforefield:extent:
            if h-beforefield:Buffer-value[y] ne h-afterfield:Buffer-value[y]
            then If FieldIsAudited(Zen-AuditConfig.tablename,h-afterfield:name)
                 Then RUN WriteAuditLine (pv-AuditDetailKey,h-beforefield,y).
         end.
      else do: /* Normal Field */
         if h-beforefield:Buffer-value ne h-afterfield:Buffer-value
            then If FieldIsAudited(Zen-AuditConfig.tablename,h-afterfield:name)
               Then RUN WriteAuditLine (pv-AuditDetailKey,h-beforefield,0).
      end.
   end.
   if not lv-HasAuditedFields then do:
       /* now remove header if no auditable fields where changed, messsy but what they want! */
        find zen-auditdetail where zen-auditdetail.zen-auditdetailtableid = pv-auditdetailkey 
                             exclusive-lock.
        delete zen-auditdetail.
   end.
end.
else do: /* its a create so just write away keyfield */
    if stringtolog(getctrl('AuditCreateTableid')) then
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

/* program-name(4) will give program causing change */

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
    ASSIGN zen-auditline.zen-auditdetailtableid = pv-AuditDetailKey
           lv-HasAuditedFields = true.
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

&IF DEFINED(EXCLUDE-FieldIsAudited) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FieldIsAudited Procedure 
FUNCTION FieldIsAudited RETURNS LOGICAL
  ( pv-table as char,
    pv-field as char ) :

   return can-find(FIRST Zen-Auditfield where zen-auditfield.tablename = pv-table
                                          and zen-auditfield.fieldname = pv-field
                                          and zen-auditfield.active).

/* or we could use find.i if we are using multiple indexs 
   {{&core}find.i
           &table = "zen-auditfield"
           &where = "Where zen-auditfield.tablename = pv-table 
                       and zen-auditfield.fieldname = pv-field
                       and zen-auditfield.active"
           &type  = "first"
           &lock  = "no"}
   if avail zen-auditfield
   Then return true.
   else return false.
*/
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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

&IF DEFINED(EXCLUDE-TableIsAudited) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TableIsAudited Procedure 
FUNCTION TableIsAudited RETURNS LOGICAL
  ( pv-tablename as char,
    pv-user as char ) :

def var lv-var as char no-undo.

if not can-find(first zen-control where zen-control.ctrl-idx = '{&AuditSystemStatus}'
                                    and zen-control.ctrl-data = 'on')
then return false.

if can-find(first zen-control where zen-control.ctrl-idx = '{&AuditExcludeList}'
                                and can-do(zen-control.ctrl-data,pv-user))
then return false.

if not CAN-FIND(FIRST Zen-AuditConfig WHERE Zen-AuditConfig.TableName = pv-TableName
                                        and Zen-AuditConfig.active)
then return false.

lv-var = getsysvar('DoNotAudit' + pv-tablename).
if lv-var ne ? and not lv-var begins '**'
then return false.

return true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

