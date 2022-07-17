&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
&glob serverprogram true
/* ***************************  Definitions  ************************** */
{app-paths.i}




/* CHANGEABLE BITS HERE change table-name to appropriate db table */
&glob Table-name zen-auditdetail
&glob Unique-key {&table-name}tableid 

{{&core}def-table.i &table-name = {&table-name}}
{{&core}def-table.i &table-name = zen-auditline}

define query q-{&table-name} FOR {&table-name},zen-auditline scrolling.
def stream op.
def stream op2.


define temp-table t-auditdata no-undo
    field t-value as char
    field t-date  as date
    field t-time  as int
    field t-user  as char
index order t-date descending
            t-time descending.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-FieldInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FieldInfo Procedure 
FUNCTION FieldInfo RETURNS CHARACTER
  (pv-table as char,
   pv-field as char,
   pv-mode as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Validtype) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Validtype Procedure 
FUNCTION Validtype RETURNS LOGICAL
  ( pv-adds as log,
    pv-updates as log,
    pv-deletes as log,
    pv-rectype as log)  FORWARD.

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
         HEIGHT             = 20.14
         WIDTH              = 50.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}mainp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Cleardown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cleardown Procedure 
PROCEDURE Cleardown :
def input param pv-table as char no-undo.
def input param lv-searchkey as char no-undo.
def input param lv-auduser as char no-undo.
def input param from-date as date no-undo.
def input param to-date  as date no-undo.
def input param rs-mode as char no-undo.
def output param x as int no-undo.

def var lv-tstamp as char no-undo.
lv-tstamp = string(today,'99/99/9999') +  string(time,'hh:mm') + '.d'.

if rs-mode = 'p' then do:
    output stream op to value('auditdetailpurge' + lv-tstamp).
    output stream op2 to value('auditlinepurge' + lv-tstamp).
end.

for each zen-auditdetail where zen-auditdetail.tablename matches pv-table
                           and zen-auditdetail.SearchFieldData matches lv-searchkey
                           and zen-auditdetail.byname matches lv-auduser
                           and zen-auditdetail.auditdate > from-date 
                           and zen-auditdetail.auditdate < to-date 
                           no-lock:
   if rs-mode = 'p' then do: 
    export stream op zen-auditdetail.                          
    for each zen-auditline of zen-auditdetail no-lock.
       export stream op2 zen-auditline.                          
       delete zen-auditline.
    end.
    delete zen-auditdetail.
   end.
   x = x + 1.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FieldDataType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FieldDataType Procedure 
PROCEDURE FieldDataType :
def input param pv-table as char no-undo.
def input param pv-field as char no-undo.
def output param pv-datatype as char no-undo.


pv-datatype = FieldInfo(pv-table,pv-field,'Data-Type').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FieldDesc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FieldDesc Procedure 
PROCEDURE FieldDesc :
def input param pv-table as char no-undo.
def input param pv-field as char no-undo.
def output param pv-desc as char no-undo.

pv-desc = FieldInfo(pv-table,pv-field,'label').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAuditLines) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAuditLines Procedure 
PROCEDURE GetAuditLines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

      FOR each Zen-Auditline Of Zen-AuditDetail no-lock:
        create t-zen-auditline.
        buffer-copy zen-auditline to t-zen-auditline. 
      end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFieldAuditinfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFieldAuditinfo Procedure 
PROCEDURE GetFieldAuditinfo :
def input param pv-key   as char no-undo.
def input param pv-table as char no-undo.
def input param pv-field as char no-undo.
def output param table   for t-auditdata.

def var lv-datatype as char no-undo.

if pv-table begins 't-' 
    then pv-table = substring(pv-table,3).

run fielddatatype(pv-table,pv-field,output lv-datatype).

for each zen-auditdetail where zen-auditdetail.tablename = pv-table
                           and zen-auditdetail.sourcekey = pv-key
                         NO-lock,
    each zen-auditline of zen-auditdetail 
                       where zen-auditline.fieldname = pv-field
                       no-lock:
    create t-auditdata.
    assign t-date  = zen-auditdetail.auditdate
           t-time  = zen-auditdetail.audittime 
           t-user  = zen-auditdetail.byname.

    case lv-datatype:
        when 'date'      then t-value = string(Zen-AuditLine.DataDate).
        when 'decimal'   then t-value = string(Zen-AuditLine.DataDec).
        when 'integer'   then t-value = string(Zen-AuditLine.DataInt).
        when 'logical'   then t-value = string(zen-auditline.datalog).
        when 'character' then t-value = zen-auditline.datachar.
    end case.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSearchLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSearchLabel Procedure 
PROCEDURE GetSearchLabel :
def input  param pv-table as char no-undo.
def output param pv-label as char no-undo.

FIND FIRST Zen-AuditConfig WHERE Zen-AuditConfig.TableName = pv-table
                           no-lock no-error.
if avail Zen-AuditConfig
    then pv-label = zen-auditconfig.SearchFieldName.
    else pv-label = 'None'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Open-QueryKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Open-QueryKey Procedure 
PROCEDURE Open-QueryKey :
def input param pv-table     as char no-undo.
def input param pv-searchkey as char no-undo.
def input param pv-fromdate  as date no-undo.
def input param pv-todate    as date no-undo.
def input param pv-adds      as log  no-undo.
def input param pv-updates   as log  no-undo.
def input param pv-deletes   as log  no-undo.
def input param pv-by        as  char no-undo.

def output param table for t-{&table-name}.
def output param table for t-zen-auditline.

def var lv-validaction as char no-undo.
def var lv-audtime as int no-undo init 0.
if pv-adds    then lv-validaction = lv-validaction + ',create'.
if pv-updates then lv-validaction = lv-validaction + ',Update'.
if pv-deletes then lv-validaction = lv-validaction + ',Delete'.
if lv-validaction ne '' 
    then lv-validaction = substring(lv-validaction,2).

empty temp-table t-zen-auditdetail.
empty temp-table t-zen-auditline.

find first Zen-AuditDetail WHERE Zen-AuditDetail.TableName  = pv-table
                       AND Zen-AuditDetail.SourceKey  = pv-Searchkey
                       and Zen-AuditDetail.auditaction = 'create'
                     no-lock no-error.
if avail zen-auditdetail 
then assign pv-fromdate = Zen-AuditDetail.AUDitDate
            lv-audtime = zen-auditdetail.audittime.
define query q-detail for Zen-AuditDetail.

case pv-by:
    when '' then open query q-detail
        FOR EACH Zen-AuditDetail
            WHERE Zen-AuditDetail.TableName  = pv-table
              AND Zen-AuditDetail.AUDitDate >= pv-FROMDate
              AND Zen-AuditDetail.AUDitDate <= pv-tODate
              and (zen-auditdetail.audittime >= lv-audtime or 
                    zen-auditdetail.auditdate > pv-fromdate)
              and can-do(lv-validaction,Zen-AuditDetail.auditaction)
            NO-LOCK.
    when 'k'THEN open query q-detail
        FOR EACH Zen-AuditDetail
            WHERE Zen-AuditDetail.TableName  = pv-table
              AND zen-auditdetail.SearchFieldData  = pv-searchkey
              AND Zen-AuditDetail.AUDitDate >= pv-FROMDate
              AND Zen-AuditDetail.AUDitDate <= pv-tODate
              and (zen-auditdetail.audittime >= lv-audtime or 
                    zen-auditdetail.auditdate > pv-fromdate)
              and can-do(lv-validaction,Zen-AuditDetail.auditaction)
            NO-LOCK.
    when 't' then open query q-detail
        FOR EACH Zen-AuditDetail
           WHERE Zen-AuditDetail.TableName  = pv-table
             AND Zen-AuditDetail.SourceKey  = pv-Searchkey
             AND Zen-AuditDetail.AUDitDate >= pv-FROMDate
             AND Zen-AuditDetail.AUDitDate <= pv-tODate
             and (zen-auditdetail.audittime >= lv-audtime or 
                    zen-auditdetail.auditdate > pv-fromdate)
             and can-do(lv-validaction,Zen-AuditDetail.auditaction)
           NO-Lock.
end case.
get first q-detail.

do while not query-off-end('q-detail'): 
    create t-{&table-name}.
    buffer-copy {&table-name} to t-{&table-name}. 
    run GetAuditLines.
    get next q-detail.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RelatedFunction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RelatedFunction Procedure 
PROCEDURE RelatedFunction :
def input param pv-table as char no-undo.
def input param pv-field as char no-undo.
def output param pv-function as char No-undo Init ?.

find Zen-AuditConfig WHERE Zen-AuditConfig.TableName = pv-table
                     No-lock No-error.
If Not Avail zen-auditconfig Then Return.
Find zen-auditfield Where zen-auditfield.tablename = zen-auditconfig.tablename
                      and Zen-Auditfield.FieldName = pv-field
     no-lock no-error.

if avail zen-auditfield 
    then pv-function = Zen-Auditfield.Relatedfunction.
    else pv-function = ?.
                                   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-FieldInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FieldInfo Procedure 
FUNCTION FieldInfo RETURNS CHARACTER
  (pv-table as char,
   pv-field as char,
   pv-mode as char ) :

def var h-db     as handle no-undo.
def var h-field  as handle no-undo.
def var y        as int    no-undo.
def var lv-value as char   no-undo.

Create Buffer h-db For Table pv-table no-error.

if error-status:error then return ?.

do y = 1 to h-db:num-fields:
   h-field = h-db:Buffer-field(y).
   If h-field:name = pv-field 
   then do:  
      case pv-mode:
         when 'data-type' then lv-value = h-field:data-type.
         when 'label'     then lv-value = h-field:label.
      end case.
      leave.
   end.
end.
  RETURN lv-value.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Validtype) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Validtype Procedure 
FUNCTION Validtype RETURNS LOGICAL
  ( pv-adds as log,
    pv-updates as log,
    pv-deletes as log,
    pv-rectype as log) :
    return ((pv-adds    AND pv-rectype = ?)   or
            (pv-Updates AND pv-rectype = YES) or
            (pv-Deletes AND pv-rectype = NO)).
  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

