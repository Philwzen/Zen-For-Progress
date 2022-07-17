&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/******************************************************************************/
/*  PROGRAM ID.     : ????????                                                */
/*  PROGRAM TITLE   : ????????                                                */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    : Zennor Computing LTD                                    */
/*  VERSION NO.     : 1.0                                                     */
/******************************************************************************/
/******************************************************************************/
/*                          PATCH HISTORY                                     */
/*                                                                            */
/*           PATCH  USR     Job                                               */
/* DATE      NO.    ID      REF DESCRIPTION                                   */
/* -------------------------------------------------------------------------- */
/* ??/??/??  P00    Philw   00  initial release                               */
/******************************************************************************/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
create widget-pool.
/* ***************************  Definitions  ************************** */
this-procedure:private-data = "library-api".
&glob library-api

&glob library-program
{app-paths.i}

{{&core}def-table.i &table-name = zen-apidetail}


define temp-table apicall no-undo
    field params as char extent 20
    field apiname as char.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-CallApi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CallApi Procedure 
FUNCTION CallApi RETURNS CHARACTER
  ( pv-api as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CallProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CallProc Procedure 
FUNCTION CallProc RETURNS CHARACTER
  ( pv-proc as char,
    pv-name as char,
    pv-paramdata as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CallProg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CallProg Procedure 
FUNCTION CallProg RETURNS CHARACTER
  ( pv-name as char,
    pv-params as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvCharacter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvCharacter Procedure 
FUNCTION ConvCharacter RETURNS CHARACTER
  ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvDate Procedure 
FUNCTION ConvDate RETURNS DATE
  ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvDecimal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvDecimal Procedure 
FUNCTION ConvDecimal RETURNS DECIMAL
  ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvInteger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvInteger Procedure 
FUNCTION ConvInteger RETURNS INTEGER
  ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvLogical) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvLogical Procedure 
FUNCTION ConvLogical RETURNS LOGICAL
  ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvMemptr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvMemptr Procedure 
FUNCTION ConvMemptr RETURNS MEMPTR
  ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvRaw) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvRaw Procedure 
FUNCTION ConvRaw RETURNS RAW
  ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvRecid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvRecid Procedure 
FUNCTION ConvRecid RETURNS RECID
  ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvRowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvRowid Procedure 
FUNCTION ConvRowid RETURNS ROWID
  ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvWidget-Handle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvWidget-Handle Procedure 
FUNCTION ConvWidget-Handle RETURNS WIDGET-HANDLE
  ( pv-value as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetApiRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetApiRecord Procedure 
FUNCTION GetApiRecord RETURNS CHARACTER
  ( pv-api as char )  FORWARD.

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
   Other Settings: CODE-ONLY
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

{{&core}libmain.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-CallApi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CallApi Procedure 
FUNCTION CallApi RETURNS CHARACTER
  ( pv-api as handle) :

def var hcall        as handle no-undo.
def var lv-api       as char no-undo.
def var h            as handle no-undo.
def var x            as int    no-undo.
def var lv-paramlist as char   no-undo.
def var lv-datatype  as char   no-undo.
def var lv-value     as char   no-undo.
def var lv-mode      as char   no-undo.
def var lv-rvalue    as char   no-undo.

pv-api:find-first.
lv-api = pv-api:BUFFER-FIELD("apiname"):BUFFER-VALUE.

empty temp-table t-zen-apidetail.
GetApiRecord(lv-api).
find t-zen-apidetail where t-zen-apidetail.apiname = lv-api no-lock no-error.
if not avail t-zen-apidetail 
then return error 'No Api Found' + lv-api.

CREATE CALL hCall. 
assign
    HCALL:CALL-TYPE  = procedure-CALL-TYPE 
    hCall:CALL-NAME  = t-zen-apidetail.programname
    hCall:PERSISTENT = true.

h = ?. /* getappserverhandle(t-zen-apidetail.defaultapserver).*/
if not valid-handle(h) 
    then h = session:first-procedure.
    else hcall:server = h.

do while valid-handle(h):
    if h:private-data = t-zen-apidetail.programname
    then leave.
    h = h:next-sibling.
end.

if not valid-handle(h) 
    then hcall:INVOKE. 
    else hCall:IN-HANDLE = h.

assign
    h                    = hCall:IN-HANDLE
    hcall:asynchronous   = t-zen-apidetail.async 
    lv-paramlist         = h:get-signature(t-zen-apidetail.procedurename)
    lv-paramlist         = substring(lv-paramlist,index(lv-paramlist,',') + 2) /* mode name type */
    hCall:CALL-NAME      = t-zen-apidetail.procedurename
    hCall:NUM-PARAMETERS = num-entries(lv-paramlist).

/* if num-entries(lv-paramlist) ne num-entries(pv-paramdata) */
/* then do:                                                  */
/*     DELETE PROCEDURE h.                                   */
/*     DELETE OBJECT hCall.                                  */
/*     return error 'Mismatched Params ' + pv-api.           */
/* end.                                                      */

do x = 1 to num-entries(lv-paramlist):
    assign
        lv-datatype = entry(3,entry(x,lv-paramlist),' ')
        lv-mode     = entry(1,entry(x,lv-paramlist),' ').
    hCall:SET-PARAMETER(x,lv-datatype,lv-mode,pv-api:BUFFER-FIELD('params'):BUFFER-VALUE(x)).
end.

hCall:INVOKE.
lv-rvalue = string(hcall:return-value).
DELETE PROCEDURE h.
DELETE OBJECT hCall.

RETURN  string(hcall) + ',' + 
        string(h).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CallProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CallProc Procedure 
FUNCTION CallProc RETURNS CHARACTER
  ( pv-proc as char,
    pv-name as char,
    pv-paramdata as char ) :

def var hcall        as handle no-undo.
def var h            as handle no-undo.
def var x            as int    no-undo.
def var lv-data      as char   no-undo.
def var lv-paramlist as char   no-undo.

def var lv-datatype  as char   no-undo.
def var lv-value     as char   no-undo.
def var lv-mode      as char   no-undo.

/*
zen-apidetail.apiname 
zen-apidetail.async             hcall:asynchronous log
zen-apidetail.defaultapserver   hcall:server       handle
zen-apidetail.noper 

zen-apidetail.programpath       hCall:CALL-NAME
zen-apidetail.programname       hCall:CALL-NAME

zen-apidetail.procedurename     hCall:CALL-NAME second call
*/

CREATE CALL hCall. 
assign
    HCALL:CALL-TYPE = procedure-CALL-TYPE 
    hCall:CALL-NAME = pv-proc
    hCall:PERSISTENT = true.

hcall:INVOKE. 
assign
    h = hCall:IN-HANDLE
    lv-paramlist = h:get-signature(pv-name)
    lv-paramlist = substring(lv-paramlist,index(lv-paramlist,',') + 1) /* name mode type */
    hCall:CALL-NAME = pv-name
    hCall:NUM-PARAMETERS = num-entries(lv-paramlist).

do x = 1 to num-entries(lv-paramlist):
    assign
        lv-datatype = entry(3,entry(x,lv-paramlist),' ')
        lv-mode     = entry(2,entry(x,lv-paramlist),' ')
        lv-data     = entry(x,pv-paramdata). 
    hCall:SET-PARAMETER(x,lv-datatype,lv-mode,dynamic-function('Conv' + lv-datatype,lv-data)).
end.

hCall:INVOKE. 
DELETE PROCEDURE h.
DELETE OBJECT hCall. 

  RETURN "".   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CallProg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CallProg Procedure 
FUNCTION CallProg RETURNS CHARACTER
  ( pv-name as char,
    pv-params as char ) :

def var hcall       as handle no-undo.

def var x           as int    no-undo.
def var lv-data     as char   no-undo.
def var lv-param    as char   no-undo.
def var lv-datatype as char   no-undo.
def var lv-value    as char   no-undo.

CREATE CALL hCall. 
assign
    HCALL:CALL-TYPE = procedure-CALL-TYPE 
    hCall:CALL-NAME = pv-name
    hCall:NUM-PARAMETERS = num-entries(pv-params).

do x = 1 to num-entries(pv-params):
    lv-data = entry(x,pv-params,'{&Delim2}').
    assign
        lv-param    = entry(1,lv-data)
        lv-datatype = entry(2,lv-data)
        lv-value    = entry(3,lv-data).
    case lv-datatype:
        when 'Char' then hCall:SET-PARAMETER(x,lv-datatype,"INPUT",lv-value). 
        when 'Dec'  then hCall:SET-PARAMETER(x,lv-datatype,"INPUT",StringToDec(lv-value,',','.')). 
        when 'Int'  then hCall:SET-PARAMETER(x,lv-datatype,"INPUT",StringToInt(lv-value,',')). 
        when 'Log'  then hCall:SET-PARAMETER(x,lv-datatype,"INPUT",StringToLog(lv-value)). 
        when 'Date' then hCall:SET-PARAMETER(x,lv-datatype,"INPUT",StringToDate(lv-value)). 
        when 'Hand' then hCall:SET-PARAMETER(x,lv-datatype,"INPUT",widget-handle(lv-value)). 
    end case.
end.

hCall:INVOKE. 

DELETE OBJECT hCall. 

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvCharacter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvCharacter Procedure 
FUNCTION ConvCharacter RETURNS CHARACTER
  ( pv-value as char ) :

  RETURN pv-value.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvDate Procedure 
FUNCTION ConvDate RETURNS DATE
  ( pv-value as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN StringToDate(pv-value).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvDecimal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvDecimal Procedure 
FUNCTION ConvDecimal RETURNS DECIMAL
  ( pv-value as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN stringToDec(pv-value,session:numeric-separator,session:numeric-decimal-point).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvInteger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvInteger Procedure 
FUNCTION ConvInteger RETURNS INTEGER
  ( pv-value as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN StringToInt(pv-value,session:numeric-separator).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvLogical) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvLogical Procedure 
FUNCTION ConvLogical RETURNS LOGICAL
  ( pv-value as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN StringToLog(pv-value).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvMemptr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvMemptr Procedure 
FUNCTION ConvMemptr RETURNS MEMPTR
  ( pv-value as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/* get-long(int(pv-value),1) */
  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvRaw) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvRaw Procedure 
FUNCTION ConvRaw RETURNS RAW
  ( pv-value as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
def var x as raw no-undo.
/* x = raw(pv-value).  */
  RETURN x.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvRecid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvRecid Procedure 
FUNCTION ConvRecid RETURNS RECID
  ( pv-value as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
def var lv-x as recid no-undo.
lv-x = int(pv-value).
  RETURN lv-x.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvRowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvRowid Procedure 
FUNCTION ConvRowid RETURNS ROWID
  ( pv-value as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN to-rowid(pv-value).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvWidget-Handle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvWidget-Handle Procedure 
FUNCTION ConvWidget-Handle RETURNS WIDGET-HANDLE
  ( pv-value as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN widget-handle(pv-value).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetApiRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetApiRecord Procedure 
FUNCTION GetApiRecord RETURNS CHARACTER
  ( pv-api as char ) :
    {{&core}run.i &program   = "zen-apidetail.p"
                  &path      = "{&core}{&srv}"
                  &Appsrv    = "System"  
                  &procedure = "find-record"
                  &params    = "(pv-api,input-output table t-zen-apidetail)"}
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

