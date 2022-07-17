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
 create widget-pool.
/* ***************************  Definitions  ************************** */
this-procedure:private-data = "library-office".
&global-define library-office
&global-define library-program
{app-paths.i}

define variable lv-wasup as log extent 10 no-undo init no.
define variable lv-applist as character no-undo init
'TextMaker,Microsoft Word,Microsoft Excel,Outlook,LibreOffice'.

define stream ip.
define stream op.


&global-define wdOpenFormatAuto 0
DEF var lv-officesuite as character no-undo.
Def Var ch-LoSrvMgr As Com-handle No-undo.
DEF VAR ch-doclistener AS COM-HANDLE NO-UNDO.
DEF VAR ch-applistener AS COM-HANDLE NO-UNDO.
DEF VAR ch-lotools AS COM-HANDLE NO-UNDO.
DEF VAR Loargs AS RAW /* EXTENT 10*/  NO-UNDO.

/*   CREATE 'Lotools.Lodispatch' ch-lotools. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-CreateUnoService) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateUnoService Procedure 
FUNCTION CreateUnoService RETURNS COM-HANDLE
  ( pv-service AS CHAR )  FORWARD.

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

&IF DEFINED(EXCLUDE-LibreDispatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LibreDispatch Procedure 
FUNCTION LibreDispatch RETURNS CHARACTER
  ( pv-service AS COM-HANDLE,
    pv-desktop AS COM-HANDLE,
    pv-action AS CHAR,
    pv-names AS CHAR,
    pv-values AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LibreHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LibreHandle Procedure 
FUNCTION LibreHandle RETURNS COM-HANDLE
  ( pv-name AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LibreParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LibreParam Procedure 
FUNCTION LibreParam RETURNS COM-HANDLE
     ( pv-name AS CHAR , 
       pv-value AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LibrePrinters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LibrePrinters Procedure 
FUNCTION LibrePrinters RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MakePropertyValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MakePropertyValue Procedure 
FUNCTION MakePropertyValue RETURNS COM-HANDLE EXTENT 10
 ( pv-names AS CHAR , 
   pv-values AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeAppendText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeAppendText Procedure 
FUNCTION OfficeAppendText RETURNS CHARACTER
  ( pv-application AS COM-HANDLE,
    pv-document AS COM-HANDLE,
    pv-text AS CHAR,
    pv-skipbefore AS INT,
    pv-skipafter AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeApplicationName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeApplicationName Procedure 
FUNCTION OfficeApplicationName returns character
  ( pv-applhandle as com-handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeCloseApplication) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeCloseApplication Procedure 
FUNCTION OfficeCloseApplication returns logical
  ( pv-applhandle as com-handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeCloseDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeCloseDocument Procedure 
FUNCTION OfficeCloseDocument returns logical
  ( pv-document as com-handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeEditTemplate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeEditTemplate Procedure 
FUNCTION OfficeEditTemplate RETURNS COM-HANDLE
    ( pv-application AS COM-HANDLE,
      pv-template AS COM-HANDLE,
      lv-fieldlist AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeGetDocHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeGetDocHandle Procedure 
FUNCTION OfficeGetDocHandle returns com-handle
( pv-applhandle as com-handle,
  pv-document   as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeGetDocName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeGetDocName Procedure 
FUNCTION OfficeGetDocName RETURNS CHARACTER
  ( pv-application As Com-handle,
    pv-document As Com-handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeGetSuite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeGetSuite Procedure 
FUNCTION OfficeGetSuite returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeMailMerge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeMailMerge Procedure 
FUNCTION OfficeMailMerge RETURNS COM-HANDLE
  (pv-application as com-handle,
   pv-template as com-handle,
   pv-datafile as char, 
   pv-destination as char,
   output pv-numrecs as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeNewWorkBook) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeNewWorkBook Procedure 
FUNCTION OfficeNewWorkBook returns com-handle
  ( pv-application as com-handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeNewWorkSheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeNewWorkSheet Procedure 
FUNCTION OfficeNewWorkSheet returns com-handle
  ( pv-workbook as com-handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeOpenDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeOpenDocument Procedure 
FUNCTION OfficeOpenDocument returns com-handle
  ( pv-applhandle as com-handle,
    pv-Document as char,
    pv-readonly as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeOpenTemplate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeOpenTemplate Procedure 
FUNCTION OfficeOpenTemplate RETURNS COM-HANDLE
    ( pv-applhandle as com-handle,
      pv-Template as char ,
      pv-fieldlist AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficePickPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficePickPrinter Procedure 
FUNCTION OfficePickPrinter RETURNS CHARACTER
    (pv-application AS COM-HANDLE,
     pv-document AS COM-HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficePrintMe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficePrintMe Procedure 
FUNCTION OfficePrintMe returns character
    ( pv-application AS COM-HANDLE,
      pv-document as com-handle,
      pv-copies AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficePrintPreview) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficePrintPreview Procedure 
FUNCTION OfficePrintPreview returns logical
  ( ch-application as com-handle,
    ch-document as com-handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficePropertyAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficePropertyAction Procedure 
FUNCTION OfficePropertyAction RETURNS CHARACTER
  ( pv-app AS com-handle,
    pv-doc AS COM-HANDLE,
    pv-propertyAction AS CHAR,
    pv-value AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeSaveAs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeSaveAs Procedure 
FUNCTION OfficeSaveAs RETURNS COM-HANDLE
    ( pv-applhandle as com-handle,
      pv-document   as COM-HANDLE,    /* current doc */
      pv-name       as char,    /* name to save as */
      pv-type       AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeSearchReplace) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeSearchReplace Procedure 
FUNCTION OfficeSearchReplace RETURNS CHARACTER
    ( pv-app AS com-handle,
      pv-doc AS COM-HANDLE,
      pv-direction AS CHAR,
      pv-wrap as char,
      pv-from AS CHAR,
      pv-to AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeSendEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeSendEmail Procedure 
FUNCTION OfficeSendEmail returns logical
  ( pv-applhandle as com-handle,
    pv-from as char,
    pv-to   as char,
    pv-subject as char,
    pv-text as  char,
    pv-attach as char,
    pv-expires as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeSetListener) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeSetListener Procedure 
FUNCTION OfficeSetListener RETURNS COM-HANDLE
  ( pv-app AS com-handle,
    pv-doc AS COM-HANDLE, /* not used but may be usefull later*/
    pv-type AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeSetSuite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeSetSuite Procedure 
FUNCTION OfficeSetSuite returns character
  ( pv-suite AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeStartApplication) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeStartApplication Procedure 
FUNCTION OfficeStartApplication returns com-handle
  ( pv-appName as char,
    pv-hidden as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeToFront) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeToFront Procedure 
FUNCTION OfficeToFront RETURNS CHARACTER
  ( ch-application AS COM-HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeToUrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeToUrl Procedure 
FUNCTION OfficeToUrl returns character
  ( pv-doc as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeWorkSheetCellsAutoFit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeWorkSheetCellsAutoFit Procedure 
FUNCTION OfficeWorkSheetCellsAutoFit returns character
   ( pv-worksheet as com-handle,
    pv-startcol as int,
    pv-endcol as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeWriteCellData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OfficeWriteCellData Procedure 
FUNCTION OfficeWriteCellData returns character
  ( pv-worksheet as com-handle,
    pv-col as int,
    pv-row as int,
    pv-data as char )  FORWARD.

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
         HEIGHT             = 30.48
         WIDTH              = 51.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
procedure SetParams  external 'Lotools':
    def input param pv-names  as CHAR.
    DEF INPUT PARAM pv-values AS CHAR.
    define return parameter ReturnValue as LONG.
end procedure.

procedure SetParam  external 'Lotools':
    def input param pv-name  as CHAR.
    DEF INPUT PARAM pv-value AS CHAR.
    define return parameter ReturnValue as long.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-CreateUnoService) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateUnoService Procedure 
FUNCTION CreateUnoService RETURNS COM-HANDLE
  ( pv-service AS CHAR ) :

    /* com.sun.star.lang.ServiceManager */
    DEF VAR ch-service AS COM-HANDLE NO-UNDO.
    ASSIGN
    ch-service = ?
    ch-service = ch-LoSrvMgr:createInstance(pv-service) NO-ERROR.

    RETURN ch-service.

END FUNCTION.

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
define variable lv-r as character no-undo.
define variable lv-letter as character no-undo init 'a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z'.



    lv-r = if lv-col < 27 then entry(lv-col,lv-letter) 
                          else entry(int(truncate(lv-col / 27,0)),lv-letter) + 
                               entry((lv-col mod 26) + 1,lv-letter) no-error.        
   if error-status:error 
   then message 'Column Name Failure for ' lv-col {&dbt}.

  return lv-r.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LibreDispatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LibreDispatch Procedure 
FUNCTION LibreDispatch RETURNS CHARACTER
  ( pv-service AS COM-HANDLE,
    pv-desktop AS COM-HANDLE,
    pv-action AS CHAR,
    pv-names AS CHAR,
    pv-values AS CHAR) :
   
/* allow calls to libreoffice dispatcher using parameter and value object arrays
   done via dll lotools.dll */
IF pv-names  = '' THEN pv-names ='dummy'.
IF pv-values = '' THEN pv-values ='dummy'.
IF NOT pv-action BEGINS ".uno:" THEN pv-action = ".uno:" + pv-action.

/* MESSAGE 'action :' pv-action SKIP         */
/*         'names  :' pv-names SKIP          */
/*         'VALUES : ' pv-values             */
/* VIEW-AS ALERT-BOX title program-name(2).  */

  ch-lotools:dispatcher(pv-service,pv-desktop,pv-action,pv-names,pv-values).
  
/*   RELEASE OBJECT ch-lotools. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LibreHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LibreHandle Procedure 
FUNCTION LibreHandle RETURNS COM-HANDLE
  ( pv-name AS CHAR ) :
  DEF VAR lv-handle AS COM-HANDLE NO-UNDO.

  CASE pv-name:
      WHEN 'ServiceManager'     THEN lv-handle = ch-LoSrvMgr.
/*       WHEN 'Desktop'     THEN lv-handle = ch-application.  */
      WHEN 'doclistener' THEN lv-handle = ch-doclistener.
      WHEN 'applistener' THEN lv-handle = ch-applistener.
  END CASE.

  RETURN lv-handle.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LibreParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LibreParam Procedure 
FUNCTION LibreParam RETURNS COM-HANDLE
     ( pv-name AS CHAR , 
       pv-value AS CHAR) :

     DEF VAR lv-param AS COM-HANDLE NO-UNDO.

        assign                  
            lv-param = ch-LoSrvMgr:Bridge_GetStruct("com.sun.star.beans.PropertyValue")
            lv-param:NAME  = pv-Name
            lv-param:Value = pv-value.

       RETURN lv-param.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LibrePrinters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LibrePrinters Procedure 
FUNCTION LibrePrinters RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
   Def var oPrintServer as com-handle no-undo.  /* The print server service. */
   def var oCore as com-handle no-undo.  /* Get classes and other objects by name. */
   def var oClass as com-handle no-undo.  /* XPrinterServer class object. */
   def var oMethod as com-handle no-undo.  /* getPrinterNames method from the XPrinterServer class. */
   def var lv-list as char no-undo.
   def var lv-arg as raw extent 2.

   oPrintServer = CreateUnoService("com.sun.star.awt.PrinterServer").
   oCore = CreateUnoService("com.sun.star.reflection.CoreReflection").
   oClass = oCore:forName("com.sun.star.awt.XPrinterServer").
   oMethod = oClass:getMethod("getPrinterNames").
   lv-list = oMethod:invoke(oPrintServer, lv-arg).
/*
com.sun.star.awt.PrinterServer
libredispatch(ch-LoSrvMgr,pv-app,"InsertText","Text",pv-value).
*/
  release object oprintserver.
  release object ocore.
  release object oclass.
  release object omethod.
 
  return lv-list.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MakePropertyValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MakePropertyValue Procedure 
FUNCTION MakePropertyValue RETURNS COM-HANDLE EXTENT 10
 ( pv-names AS CHAR , 
   pv-values AS CHAR) :
 
 DEF VAR lv-params AS COM-HANDLE EXTENT 10 NO-UNDO.
 
 DEF VAR X AS INT NO-UNDO. 
 DEF VAR lv-val AS CHAR NO-UNDO.

 DO X = 1 TO NUM-ENTRIES(pv-names):
    lv-val = REPLACE(':',',',ENTRY(X,pv-values)).
    RUN setparam(ENTRY(X,pv-names),lv-val,OUTPUT lv-params[X]).
    /* or maybe do 
    assign                  
        lv-params[X] = ch-LoSrvMgr:Bridge_GetStruct("com.sun.star.beans.PropertyValue")
        lv-params[X]:NAME  = entry(x,pv-Names)
        lv-params[X]:Value = lv-val.
    */
 
 END.

   RETURN lv-params.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeAppendText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeAppendText Procedure 
FUNCTION OfficeAppendText RETURNS CHARACTER
  ( pv-application AS COM-HANDLE,
    pv-document AS COM-HANDLE,
    pv-text AS CHAR,
    pv-skipbefore AS INT,
    pv-skipafter AS INT ) :

DEF VAR X AS INT NO-UNDO.
DEF VAR lv-wasprotected AS INT NO-UNDO.

    case lv-officesuite:
        when 'Microsoft' or when 'Softmaker' OR 
        WHEN 'kingsoft' then lv-wasprotected = pv-Document:protectionType. 
        when 'LibreOffice' then lv-wasprotected = 1. /* todo not sure yet */            
        otherwise  return error.
    end case.
    
/* protect the document again, so we can easily move from
  field to field. As far as the number in the method goes,
  I believe it works something like this;
     1 = wdAllowOnlyComments
     2 = wdAllowOnlyFormFields
     3 = wdAllowOnlyReading
     4 = wdAllowOnlyRevisions
     5 = wdNoProtection  . . .  
  I could always be wrong!  Be sure to test any changes that you make. */

    officepropertyaction(pv-application,pv-document,'protection','5').  

    officepropertyaction(pv-application,pv-document,"GoToEnd","").
     DO X = 1 TO pv-skipbefore:
         officepropertyaction(pv-application,pv-document,"insertpara","").
     END.
    
    officepropertyaction(pv-application,pv-document,"InsertText",pv-text).
    
     DO X = 1 TO pv-skipafter:
         officepropertyaction(pv-application,pv-document,"insertpara","").
     END.
     officepropertyaction(pv-application,pv-document,'protection',string(lv-wasprotected)).             
    
     RETURN "".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeApplicationName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeApplicationName Procedure 
FUNCTION OfficeApplicationName returns character
  ( pv-applhandle as com-handle ) :

define variable lv-appname as character no-undo init 'Invalid'.

if valid-handle(pv-applhandle) 
then 
case lv-officesuite:
    when 'Microsoft' or when 'Softmaker' OR 
    WHEN 'Kingsoft'    then lv-appname = pv-applhandle:application:NAME NO-ERROR.
    when 'LibreOffice' then lv-appname = 'LibreOffice'.
    
 otherwise lv-appname = 'Unknown'.
end case.

  return lv-appname.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeCloseApplication) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeCloseApplication Procedure 
FUNCTION OfficeCloseApplication returns logical
  ( pv-applhandle as com-handle ) :

   if not valid-handle(pv-applhandle) then return true.

   define variable x as integer no-undo.

    x = lookup(OfficeApplicationName(pv-Applhandle),lv-applist).

    if not lv-wasup[x]
    then do:
        case lv-officesuite:
            when 'Microsoft' or when 'Softmaker' OR 
            WHEN 'kingsoft' then do:
                pv-applhandle:QUIT(NO,,) NO-ERROR.
                DO WHILE VALID-HANDLE(pv-applhandle):
                    WAIT(500).
                    PROCESS EVENTS.
                END.
            END.
            when 'LibreOffice' 
            then do:
                pv-applhandle:TERMINATE() no-error.
                pv-applhandle:QUIT() no-error.
                RELEASE OBJECT ch-doclistener no-error.
                RELEASE OBJECT ch-applistener no-error.
                ch-doclistener = ?.
                ch-applistener = ?.
            end.
            otherwise  return error.
        end case.

        release object pv-applhandle no-error.
        if error-status:error then return error.
    end.

return not error-status:error.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeCloseDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeCloseDocument Procedure 
FUNCTION OfficeCloseDocument returns logical
  ( pv-document as com-handle) :

case lv-officesuite:
    when 'Microsoft' or when 'Softmaker' OR 
    WHEN 'kingsoft' then pv-document:close({&wdDoNotSaveChanges}) no-error. 
    when 'LibreOffice' 
    then do:
        pv-document:Close(true) no-error.
        RELEASE OBJECT ch-doclistener no-error.
        ch-doclistener = ?.
    END.
    otherwise return error.
end case.
    
    
return not error-status:error.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeEditTemplate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeEditTemplate Procedure 
FUNCTION OfficeEditTemplate RETURNS COM-HANDLE
    ( pv-application AS COM-HANDLE,
      pv-template AS COM-HANDLE,
      lv-fieldlist AS CHAR) :

DEF var readOnly      as logical    no-undo initial true.
def var ch-odbContext as com-handle no-undo.
def var sURL          as character  no-undo.
DEF VAR lv-doc        AS COM-HANDLE NO-UNDO.
DEF VAR lv-wasprotected AS INT NO-UNDO.
      /* protection prob needs to be moved to officepropertyvalue and called 
      before and after offcice edittemplate to allow editing to happen */ 
                                                                 
case OfficeGetSuite():
  when "Microsoft" then do:  
     lv-wasprotected = pv-template:protectionType. 
     pv-template:unprotect() no-error.
         
     pv-template:MailMerge:MainDocumentType = {&wdFormLetters}.
     /* OpenHeaderSource(Name As String, [Format], [ConfirmConversions],[ReadOnly], [AddToRecentFiles], [PasswordDocument],
        [PasswordTemplate], [Revert], [WritePasswordDocument],[WritePasswordTemplate], [open exclusive]) */
     If lv-fieldlist ne '' 
     then pv-template:MailMerge:OpenHeaderSource(lv-fieldlist,{&wdOpenFormatAuto},true,true,,,,,,,).
     pv-template:protect(lv-wasprotected).
     
     pv-template:MailMerge:HighlightMergeFields = true.  /* highlights the merge fields */
     pv-Application:WindowState = 0. /* 0=normal, 1=maximized, 2=minimized; moves Word to front */
     pv-Application:Activate().      /* puts cursor in window for editing */
     lv-doc = pv-Application:activedocument. 
  end.
  when "LibreOffice" then do:
       /* this logic expects a pre-existing .odb */
      /* from template-mnt.w */
     libreDispatch(ch-LoSrvMgr,pv-application,"SwitchControlDesignMode","SwitchControlDesignMode","TRUE").
     ch-odbcontext = ch-LoSrvMgr:createInstance("com.sun.star.sdb.DatabaseContext").
     ch-oDBContext:revokeObject("MergeFieldList") no-error.
     ch-oDBContext:revokeObject("Bibliography") no-error.
     ch-oDBContext:revokeObject("accountsfieldlist") no-error.
     ch-oDBContext:revokeObject("accountsfieldlist1") no-error.

     /* sURL will end out being something like file:///C:/rex/temp/mergeFields.odb */    
  /*   lv-fieldlist = "mergeFields.odb".*/ 
     sURL = officetourl(getfullpath(lv-fieldlist)).

     ch-oDBcontext:registerDatabaseLocation("MergeFieldList",surl).
     ch-oDBcontext:GetByName("MergeFieldList"):GetConnection("","").
       /* not sure this actually does anyhting!!!!! */
     libreDispatch(ch-LoSrvMgr,pv-application,"SwitchControlDesignMode","SwitchControlDesignMode","FALSE").
     release object ch-odbContext no-error.
  end. /* LibreOffice */
end case. /* lv-officeSuite */

RETURN lv-doc.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeGetDocHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeGetDocHandle Procedure 
FUNCTION OfficeGetDocHandle returns com-handle
( pv-applhandle as com-handle,
  pv-document   as char) :

define variable x          as integer no-undo.
define variable lv-name    as character no-undo init ?.

pv-document = entry(num-entries(pv-document,'{&BackSlash}'),pv-document,'{&BackSlash}').

case lv-officesuite:
    when 'Microsoft' or when 'Softmaker' OR WHEN 'kingsoft' then 
    do x = 1 to 99:
       lv-name =  pv-applhandle:documents:ITEM(x):name no-error.
       if lv-name = ? then leave. 
       if lv-name = pv-document 
       then do:
           pv-applhandle = pv-applhandle:documents:ITEM(x) no-error.
           leave.
       end.
    end.
    when 'LibreOffice' then pv-applhandle = pv-applhandle:Sheets:getByIndex(0).
    otherwise pv-applhandle = ?.
end case.
if lv-name = ? or x > 99 then pv-applhandle = ?.

return pv-applhandle.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeGetDocName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeGetDocName Procedure 
FUNCTION OfficeGetDocName RETURNS CHARACTER
  ( pv-application As Com-handle,
    pv-document As Com-handle ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
Def Var lv-name As Char No-undo.

case OfficeApplicationName(pv-application):
    when 'Microsoft Word' then do:
        lv-name = pv-document:Name.
    end.
    when 'Microsoft Excel' then do:
       lv-name = pv-document:Name.
    end.
    when 'LibreOffice'     then do:
        lv-name = pv-document:GetUrl().        
    end.
end case.
  RETURN lv-name.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeGetSuite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeGetSuite Procedure 
FUNCTION OfficeGetSuite returns character
  ( /* parameter-definitions */ ) :

    if lv-officeSuite = '' 
    then lv-officeSuite = OfficeSetSuite('').
    
  return lv-officeSuite.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeMailMerge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeMailMerge Procedure 
FUNCTION OfficeMailMerge RETURNS COM-HANDLE
  (pv-application as com-handle,
   pv-template as com-handle,
   pv-datafile as char, 
   pv-destination as char,
   output pv-numrecs as int) :

   DEF VAR lv-wasprotected AS INT NO-UNDO.
   define variable ch-mm as com-handle no-undo.

case lv-officesuite:
    when 'Microsoft' or when 'Softmaker' OR 
    WHEN 'kingsoft' then do:
        ch-mm = pv-template:MailMerge.
     /* OpenDataSource(Name,ConfirmConversions,ReadOnly,LinkToSource, 
                       AddToRecentFiles, PasswordDocument, PasswordTemplate,
                       Revert, WritePasswordDocument, WritePasswordTemplate, 
                       Connection, SQLStatement, SQLStatement1) */
     message pv-datafile skip search(pv-datafile).
        ch-mm:OpenDataSource(pv-datafile,{&wdOpenFormatAuto},true,true,true,false).
/*         ch-mm:DataSource:FirstRecord = 2. /* set start rec to second record bodge for 2003 */ */
        ch-mm:Destination = {&wdSendToNewDocument}.
        ch-mm:Execute(true). /* disp dialog box if we have errors */
        ch-mm:DataSource:ActiveRecord = {&wdlastDataSourceRecord}.
        pv-numrecs = ch-mm:DataSource:ActiveRecord.
        ch-mm = pv-application:ActiveDocument.
        ch-mm:BuiltInDocumentProperties(1) = pv-destination. /* document title ie: filename */
    end.
    when 'LibreOffice' then do:  
        def var lv-tDoc as char no-undo .
        def var lv-sdoc as char no-undo.
        lv-tdoc = GetScratchName("0.ott",yes).
        lv-sdoc = GetScratchName("odt",yes).

        ch-mm = OfficeSaveAs(pv-application,ch-mm,lv-tdoc,'template').                                  
        ch-mm = CreateUnoService("com.sun.star.text.MailMerge").
        ch-mm:DocumentUrl = officetourl(pv-destination) no-error.
         assign          
            ch-mm:DataSourceName     = "MergeFieldList"
            ch-mm:CommandType        = 0 /* 0 = table, 1 = query? */
            ch-mm:Command            = "AccountData"
            ch-mm:OutputType         = 2 /* 1 = print, 2 = file, 3 = e-mail */
            ch-mm:OutputUrl          = OfficeToUrl(substring(lv-sdoc,1,r-index(lv-sdoc,'\'))) 
            ch-mm:FileNamePrefix     = entry(num-entries(lv-sdoc,'\'),lv-sdoc,'\')
            ch-mm:FileNameFromColumn = false /* use a column name for the output file name */
            ch-mm:SaveAsSingleFile   = yes.
         /* do the actual mail merge */
         ch-mm:Execute(loArgs).

         ch-mm = pv-application:Sheets:getByIndex(0).
         ch-mm = OfficeSaveAs(pv-application,ch-mm,pv-destination,'document').
if search(lv-tdoc) ne ? then os-delete value(lv-tdoc).       
if search(lv-sdoc) ne ? then os-delete value(lv-sdoc).       
    end.
    otherwise return error.

end case.
    RETURN ch-mm.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeNewWorkBook) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeNewWorkBook Procedure 
FUNCTION OfficeNewWorkBook returns com-handle
  ( pv-application as com-handle ) :

def var ch-workbook as com-handle no-undo.
def var args as raw       no-undo.
    /*    
case OfficeApplicationName(pv-application):
*/
case lv-officesuite:
    when 'Microsoft' or when 'Softmaker'  OR WHEN 'kingsoft' then ch-WorkBook  = pv-application:Workbooks:Add().
    when 'LibreOffice' then ch-WorkBook  = pv-application:loadComponentFromURL("private:factory/scalc", "_blank", 0, args).
end case.

  return ch-workbook.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeNewWorkSheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeNewWorkSheet Procedure 
FUNCTION OfficeNewWorkSheet returns com-handle
  ( pv-workbook as com-handle ) :

def var ch-worksheet as com-handle no-undo.

    /*    
case OfficeApplicationName(pv-application):
*/
case lv-officesuite:
    when 'Microsoft' or when 'Softmaker'  OR WHEN 'kingsoft' then ch-WorkSheet = pv-WorkBook:Sheets:Item(1).
    when 'LibreOffice' then ch-WorkSheet = pv-WorkBook:Sheets:getByIndex(0).
end case.

  return ch-worksheet.   /* Function return value. */
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeOpenDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeOpenDocument Procedure 
FUNCTION OfficeOpenDocument returns com-handle
  ( pv-applhandle as com-handle,
    pv-Document as char,
    pv-readonly as char) :

   def var ch-document as com-handle  no-undo.
   /* ms and kingsoft style 
   (FileName,
   ConfirmConversions,
   ReadOnly,
   AddToRecentFiles,
   PasswordDocument,
   PasswordTemplate, 
   Revert,
   WritePasswordDocument,
   WritePasswordTemplate,
   Format,
   Encoding,
   Visible,
   OpenAndRepair,
   DocumentDirection,
   NoEncodingDialog,
   XMLTransform
   */
   define variable lv-readonly as INTEGER no-undo.
   lv-readonly = if pv-readonly = 'readonly' then 1 else 0.
   case OfficeApplicationName(pv-applhandle):
      when 'Microsoft Excel' then ch-document = 
         pv-applhandle:Workbooks:Open(pv-document,no,lv-readonly) no-error.
      when 'Microsoft Word'  or when 'textmaker' then do:
          if pv-document ne "" 
              then ch-document = pv-applhandle:Documents:Open(pv-document,no,lv-readonly,,).
              else ch-document = pv-applhandle:Documents:Add.
      end.
      when 'LibreOffice'     then do:  
     /*  Loargs = MakePropertyValue('readonly','true'). */
/*
            libredispatch(Service,Desktop,Action,Params,Values) 
            libredispatch(ch-LoSrvMgr ,   /* librehandle('servicemanager') */
                          ch-application, /* librehandle('desktop') */
                          "",             /* Action */
                          "",             /* Params */
                          "")             /* Values */
                          */
/*             RUN setparam IN THIS-PROCEDURE ('readonly','true',OUTPUT loargs).  */

            IF pv-document = '' THEN pv-document = "private:factory/swriter".
                                ELSE pv-document = OfficeToUrl(pv-Document).

            ch-document = pv-applhandle:loadComponentFromURL(pv-Document,"_blank",0,Loargs).
       END.

   end case.

   return ch-document.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeOpenTemplate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeOpenTemplate Procedure 
FUNCTION OfficeOpenTemplate RETURNS COM-HANDLE
    ( pv-applhandle as com-handle,
      pv-Template as char ,
      pv-fieldlist AS CHAR) :
DEF VAR lv-template AS COM-HANDLE NO-UNDO.  

  case lv-officesuite:
   when 'Microsoft' or when 'Softmaker' OR WHEN 'kingsoft' 
        then do:
          lv-template = pv-applhandle:Documents:Open(pv-Template,no,1,,,,).
          IF pv-fieldlist NE '' and 
             lv-officesuite ne 'kingsoft'
          THEN lv-template:MailMerge:OpenHeaderSource(pv-fieldlist).          
      END.
      when 'LibreOffice'
        then do:
          DEF VAR ch-odbcontext AS COM-HANDLE NO-UNDO.
          IF pv-Template = '' THEN pv-Template = "private:factory/swriter".
                              ELSE pv-Template = OfficeToUrl(pv-Template). 
       
          lv-template = pv-applhandle:loadComponentFromURL(pv-Template,"_blank",0,LoArgs).
          /* need to attach header source this logic expects a pre-existing .odb */                                                         
          IF pv-fieldlist NE '' THEN DO:
            ch-odbcontext = CreateUnoService("com.sun.star.sdb.DatabaseContext"). 
            if ch-oDBContext:hasByName("MergeFieldList")     then ch-oDBContext:revokeObject("MergeFieldList"). 
            if ch-oDBContext:hasByName("Bibliography")       then ch-oDBContext:revokeObject("Bibliography").
            if ch-oDBContext:hasByName("accountsfieldlist")  then ch-oDBContext:revokeObject("accountsfieldlist").
            if ch-oDBContext:hasByName("accountsfieldlist1") then ch-oDBContext:revokeObject("accountsfieldlist1").

            ch-odbcontext:registerObject("MergeFieldList",
                                         ch-odbContext:getByName(officetourl(pv-fieldlist))).
            release object ch-odbcontext.  
          END.
        END.
  end case.
  
  RETURN lv-template.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficePickPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficePickPrinter Procedure 
FUNCTION OfficePickPrinter RETURNS CHARACTER
    (pv-application AS COM-HANDLE,
     pv-document AS COM-HANDLE):

     define variable ch-dialogs          as com-handle no-undo.
     define variable ch-printDialog      as com-handle no-undo.
     DEF VAR lv-printer AS CHAR NO-UNDO.

      CASE lv-officesuite:
          when 'Microsoft' THEN DO:
           ch-Dialogs      = pv-Application:Dialogs.
           ch-PrintDialog  = ch-Dialogs:Item({&wdDialogFilePrint}).
           lv-printer      = string(ch-PrintDialog:Show). /* 0 = cancel, -1 = OK */
          END.
          WHEN 'libreoffice'  or when 'Softmaker' OR
          WHEN 'kingsoft' THEN DO:
            lv-printer = SetNamedValue('ShowPrtDlg','true', lv-printer).
            SetOpDest(input-output lv-printer).
/*              system-dialog printer-setup. */
          END.
      END CASE.
    RETURN lv-printer. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficePrintMe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficePrintMe Procedure 
FUNCTION OfficePrintMe returns character
    ( pv-application AS COM-HANDLE,
      pv-document as com-handle,
      pv-copies AS INT) :

case lv-officesuite:
    when 'Microsoft' or when 'Softmaker' OR 
    WHEN 'kingsoft' then do:
        IF pv-copies = ? 
            THEN pv-document:PrintOut() no-error.
            ELSE pv-document:PrintOut(,,,,,,,string(pv-copies)) no-error.
    END.
    when 'LibreOffice' then do:
        libredispatch(ch-LoSrvMgr,pv-application,'print','copies',STRING(pv-copies)).
    end.
    otherwise return ''.
end case.

  return "".   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficePrintPreview) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficePrintPreview Procedure 
FUNCTION OfficePrintPreview returns logical
  ( ch-application as com-handle,
    ch-document as com-handle ) :
          
case lv-officesuite:
    when 'Microsoft' or 
    when 'Softmaker' or
    when 'kingsoft' then do:
        if lv-officesuite = 'microsoft' 
        then do:
            Ch-document:activeWindow:View:Type = {&wdPrintPreview}.
/*             Ch-document:activeWindow:View:zoom = {&wdPageFitBestFit}.  */
        end.
        else Ch-document:PrintPreview().   
    end.
    when 'libreoffice' then do:
        libredispatch(ch-LoSrvMgr,ch-application,"PrintPreview",'a','1').
        /*.uno:ZoomPlus 
        dispatcher.executeDispatch(document, "Zoom100Percent", "", 0, Array())
    `   */
        libredispatch(ch-LoSrvMgr,ch-application,"PreviewZoom",'a','1').
    end.
end case.

  return  true.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficePropertyAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficePropertyAction Procedure 
FUNCTION OfficePropertyAction RETURNS CHARACTER
  ( pv-app AS com-handle,
    pv-doc AS COM-HANDLE,
    pv-propertyAction AS CHAR,
    pv-value AS CHAR ) :

DEF VAR lv-event AS Char NO-UNDO.
DEF VAR lv-suite AS CHAR NO-UNDO.
def var lv-ret as char no-undo.

lv-suite = officegetsuite().

CASE lv-suite:
    when 'Microsoft' or when 'Softmaker'   OR
    WHEN 'kingsoft'    THEN DO:
        CASE pv-PropertyAction:
            when "insertpara" then pv-App:Selection:TypeParagraph.
            when 'inserttext' then pv-App:Selection:TypeText(pv-value).
            when 'protection' then pv-doc:Protect(int(pv-value)) NO-ERROR.      
            when 'gotoend' then  pv-App:Selection:EndKey({&wdStory}) no-error.
            WHEN 'saved' THEN pv-doc:Saved = pv-value = 'TRUE'.
            WHEN 'activeprinter' THEN pv-app:ActivePrinter = pv-value.
            WHEN 'PASTE' THEN  pv-app:Selection:Paste().
            when 'cut' then pv-app:Cut().
            WHEN "SeekCurrentPageHeader" THEN pv-app:ActiveWindow:ActivePane:View:SeekView = {&wdSeekCurrentPageHeader}.
            wHEN "insertfile" THEN pv-app:Selection:InsertFile(pv-value,,no,,).
            WHEN "selectionHomekey" THEN pv-app:Selection:HomeKey({&wdStory}).
            WHEN "SelectTypeParagraph" THEN pv-app:Selection:TypeParagraph().
            WHEN "SelectWholeStory" THEN pv-app:Selection:WholeStory().
            WHEN "SelectDelete" THEN pv-app:Selection:Delete(1,1).
    END CASE.
    END.
    WHEN 'libreoffice' THEN DO: /*** calls to LibreDispatch are CASE SENSITIVE!!!! AARGH!! */
        CASE pv-propertyAction:
            when "insertpara" then libredispatch(ch-LoSrvMgr,pv-app,"InsertPara","","").
            when 'inserttext' then do:
                DEF VAR ch-cursor AS COM-HANDLE NO-UNDO.
                DEF VAR ch-txt AS COM-HANDLE NO-UNDO.
                ch-cursor = pv-doc:getCurrentController:getViewCursor.
                ch-txt = ch-cursor:text.
                ch-txt:insertString(ch-cursor,pv-value,False).
                /* dispatch doesnt work!! surprise surprise !!
                 libredispatch(ch-LoSrvMgr,pv-app,"InsertText","Text",pv-value).
                */
            END.
            when 'protection' then libreDispatch(ch-LoSrvMgr,pv-app,"SwitchControlDesignMode","SwitchControlDesignMode",pv-value).
            when 'gotoend' then libredispatch(ch-LoSrvMgr,pv-app,"GoToEndOfDoc","","").
            WHEN 'activeprinter' THEN libredispatch(ch-LoSrvMgr,pv-app,"printer","printer",pv-value).
            when 'cut' then libredispatch(ch-LoSrvMgr,pv-app,"Cut","","").
            WHEN 'PASTE' THEN libredispatch(ch-LoSrvMgr,pv-app,"Paste","","").
            WHEN "SelectWholeStory" THEN libredispatch(ch-LoSrvMgr,pv-app,"GoToEndOfDoc","","").
            WHEN "insertfile" THEN libredispatch(ch-LoSrvMgr,pv-app,"InsertDoc","Name,Filter",pv-value + ",Text").
            
 /* todo */          WHEN "SeekCurrentPageHeader" THEN pv-app:ActiveWindow:ActivePane:View:SeekView = {&wdSeekCurrentPageHeader}.
 /* todo */          WHEN 'saved' THEN pv-doc:Saved = pv-value = 'TRUE'.
 /* todo */          WHEN "selectionHomekey" THEN pv-app:Selection:HomeKey({&wdStory}).

            WHEN "SelectTypeParagraph" THEN libredispatch(ch-LoSrvMgr,pv-app,"EndOfParaSel","","").
            WHEN "SelectDelete" THEN libredispatch(ch-LoSrvMgr,pv-app,"Delete","","").
        END CASE.
    END.
END CASE.
  RETURN lv-ret.   /* Function return value. */
/**********************************
oText = oDoc.getText().
oText.insertString(oText.getEnd(), oString, false)

libredispatch(ch-LoSrvMgr,pv-app,"GoToStartOfLine","","").
libredispatch(ch-LoSrvMgr,pv-app,"GoToEndOfLine","","").
libredispatch(ch-LoSrvMgr,pv-app,"GoToStartOfLine","","").
libredispatch(ch-LoSrvMgr,pv-app,"StartOfLineSel","","").
libredispatch(ch-LoSrvMgr,pv-app,"EndOfLineSel","","").
libredispatch(ch-LoSrvMgr,pv-app,"PageDownSel","","").


 fnDispatch("InsertText", array("Text","Some text"))
       fnDispatch("InsertPara")
       fnDispatch("InsertText", array("Text","A new paragraph with a "))
       fnDispatch("Bold", array("Bold",true))
       fnDispatch("InsertText", array("Text","bold"))
       fnDispatch("Bold", array("Bold",false))
       fnDispatch("InsertText", array("Text"," word in it."))
   end sub
api style sane thing
ch-cursor:thisComponent.getCurrentController.getViewCursor.
ch-txt = ch-cursor:text.
ch-txt:insertString(ch-cursor,pv-value,False).
oVC = thisComponent.getCurrentController.getViewCursor
       oText = oVC.text
       oText.insertString(oVC, "Some text", False)
       oText.insertControlCharacter(oVC, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False)
       oText.insertString(oVC, "A new paragraph with a ", False)
       oVC.setPropertyValue("CharWeight", com.sun.star.awt.FontWeight.BOLD)
       oText.insertString(oVC, "bold", false)
       oVC.setPropertyValue("CharWeight", com.sun.star.awt.FontWeight.NORMAL)
       oText.insertString(oVC, " word in it.", false)                    
                    
                    
                    
 WHEN 'saved' THEN pv-doc:Saved = pv-value = 'TRUE'.
 WHEN "SeekCurrentPageHeader" THEN pv-app:ActiveWindow:ActivePane:View:SeekView = {&wdSeekCurrentPageHeader}.
 WHEN "selectionHomekey" THEN pv-app:Selection:HomeKey({&wdStory}).
 WHEN "SelectWholeStory" THEN pv-app:Selection:WholeStory().

mailmerge set header source
save as – text doc fhtml pdf default
tofront
**********************************/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeSaveAs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeSaveAs Procedure 
FUNCTION OfficeSaveAs RETURNS COM-HANDLE
    ( pv-applhandle as com-handle,
      pv-document   as COM-HANDLE,    /* current doc */
      pv-name       as char,    /* name to save as */
      pv-type       AS CHAR) :   /* save type */

  define variable lv-store   as character no-undo.
  def var ch-doc as com-handle no-undo.
  DEF VAR lv-dtype AS INT NO-UNDO.

 IF pv-name = '' THEN 
     pv-name = OfficeGetDocName(pv-applhandle,pv-document).
         /* word format types
           wdFormatDocument                =  0 (Word format)
           wdFormatDOSText                 =  4
           wdFormatDOSTextLineBreaks       =  5 (preserves line breaks)
           wdFormatEncodedText             =  7
           wdFormatFilteredHTML            = 10
           wdFormatHTML                    =  8
           wdFormatRTF                     =  6
           wdFormatTemplate                =  1 (Word template)
           wdFormatText                    =  2 (Windows text format)
           wdFormatTextLineBreaks          =  3 (preserves line breaks)
           wdFormatUnicodeText             =  7
           wdFormatWebArchive              =  9
           wdFormatXML                     = 11
           wdFormatDocument97              =  0 (Word 97, yes duplicate value)
           wdFormatDocumentDefault         = 16 (default document, docx or doc)
           wdFormatPDF                     = 17
           wdFormatTemplate97              =  1 (Word 97 template, yes duplicate)
           wdFormatXMLDocument             = 12
           wdFormatXMLDocumentMacroEnabled = 13
           wdFormatXMLTemplate             = 14
           wdFormatXMLTEmplateMacroEnabled = 15
           wdFormatXPS                     = 18 */
if pv-type = 'default' or
   IsNull(pv-type)
then pv-type = 'doc'.

pv-name = entry(1,pv-name,'.') + '.' + pv-type.

  case OfficeApplicationName(pv-applhandle):
      when 'Microsoft Word' then do:
          CASE pv-type :
              WHEN 'TXT' THEN lv-dtype = {&wdFormatText}.
              WHEN 'doc' or when 'document' 
                THEN do:
                    lv-dtype = {&wdFormatDocument}.
                    pv-name = entry(1,pv-name,'.') + '.doc'.
                end.
              WHEN 'pdf' THEN lv-dtype = {&wdFormatPDF}.
              When 'htm' Then lv-dtype = {&wdFormatFilteredHTML}.
              WHEN 'template'  or when 'dot' THEN 
                do:
                    lv-dtype = {&wdFormatDocument}.
                    pv-name = entry(1,pv-name,'.') + '.dot'.
                end.
              OTHERWISE lv-dtype = {&wdFormatDocument}.
          END CASE.
          IF pv-document = ?
              THEN pv-document = pv-applhandle:ActiveDocument. 
          pv-document:SAVEAS(pv-name,lv-dtype).
      end.
      when 'Microsoft Excel' then do:
          /* what about type ??? 
              WHEN 'csv'  THEN lv-dtype = "Text - txt - csv (StarCalc)".
              WHEN 'xls'  THEN lv-dtype = "Calc MS Excel 2007 Binary".
              when 'rtf'  then lv-dtype= wdFormatRTF.
          */
            pv-document:SaveAs(pv-name,,,,,,).
      end.
      when 'LibreOffice'     then do:
          def var lv-command as char no-undo init 'SaveAs'.
          CASE pv-type:
              when 'odt' or when 'document' then do:
                pv-type = 'writer8'.
                pv-name = entry(1,pv-name,'.') + '.odt'.
              end.
              WHEN 'TXT' THEN pv-type = "Text".
              WHEN 'doc'  THEN pv-type = "MS Word 97".
              WHEN 'pdf'  THEN do:
                pv-type = "writer_pdf_Export".
                lv-command = "ExportToPDF".
              end.
              WHEN 'template' or when 'odt' THEN do:
                pv-type = "writer8_template".
                pv-name = entry(1,pv-name,'.') + '.ott'.
              end.
              WHEN 'htm' THEN pv-type = "HTML".
              WHEN 'csv' THEN pv-type = "Text - txt - csv (StarCalc)".
              WHEN 'xls' THEN pv-type = "Calc MS Excel 2007 Binary".
              when 'rtf' then pv-type= "Rich Text Format".
              OTHERWISE  pv-type = "MS Word 97".    
          END CASE.
          libredispatch(ch-LoSrvMgr,pv-applhandle,
                        lv-command,
                        "URL,FilterName,FilterData",
                        OfficeToUrl(pv-name) + "," + pv-type + ",''").
      end.
  end case.

    return  pv-document.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeSearchReplace) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeSearchReplace Procedure 
FUNCTION OfficeSearchReplace RETURNS CHARACTER
    ( pv-app AS com-handle,
      pv-doc AS COM-HANDLE,
      pv-direction AS CHAR,
      pv-wrap as char,
      pv-from AS CHAR,
      pv-to AS CHAR ) :
                     
CASE officegetsuite():
    when 'Microsoft' or when 'Softmaker'   OR
    WHEN 'kingsoft'    THEN DO:
         assign
            pv-app:SELECTION:FIND:FORWARD = pv-direction = 'forward'
            pv-app:SELECTION:FIND:wrap = if stringtolog(pv-wrap) then 1 else 0
            pv-app:SELECTION:FIND:TEXT = pv-from
            pv-app:SELECTION:FIND:replacement:TEXT = pv-to.
         pv-app:SELECTION:FIND:EXECUTE(,,,,,,,,,,2).
    END.
    WHEN 'libreoffice' THEN DO:
         def var losrc as com-handle no-undo.
/*         WHEN "findforward" THEN pv-app:SELECTION:FIND:FORWARD = pv-value = 'TRUE'.
         WHEN "findWrap" THEN pv-app:SELECTION:FIND:wrap = INT(pv-value).    */
        libredispatch(ch-LoSrvMgr,pv-app,
                      "ExecuteSearch",
                      "SearchItem.SearchString,SearchItem.ReplaceString,Quiet",
                      pv-from + "," + pv-to + ",true").
/*
          losrc = pv-doc:createReplaceDescriptor.
          loSrc:setSearchString(pv-from).
          loSrc:setReplaceString(pv-to).
          pv-Doc:replaceAll(loSrc).
  */
 /* search replace alternative method
 libredispatch(ch-LoSrvMgr,pv-app,
               ".uno:ExecuteSearch",
               "SearchItem.SearchString,SearchItem.ReplaceString,Quiet",
               pv-from + "," + pv-to + ",true").
        */ 
        END.
    END CASE.
   RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeSendEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeSendEmail Procedure 
FUNCTION OfficeSendEmail returns logical
  ( pv-applhandle as com-handle,
    pv-from as char,
    pv-to   as char,
    pv-subject as char,
    pv-text as  char,
    pv-attach as char,
    pv-expires as char ) :

define variable lv-mail as com-handle no-undo.
def var namespace as com-handle no-undo.

        namespace = pv-applhandle:Getnamespace("MAPI").
        namespace:logon(getSysVar('user')).
    
        assign lv-mail         = pv-applhandle:createItem(0)
               lv-mail:subject = pv-subject
               lv-mail:body    = pv-text
               lv-mail:TO      = pv-to
               pv-attach       = getfullpath(pv-attach)
               lv-mail:CC      = '' no-error. 
    
        if pv-expires ne '' then 
            assign
                lv-mail:ExpiryTime = pv-expires 
                lv-mail:Permission = 2  no-error.
        else do:
            if not error-status:error then 
                lv-mail:Attachments:Add(pv-attach) no-error.
            if not error-status:error then 
                lv-mail:Attachments(1):DisplayName = "pv-text" no-error.
        end.
    
        if not error-status:error 
        then lv-mail:SEND() no-error.
        else message 'Email Error ' skip 
                     error-status:get-message(error-status:num-messages) 
             view-as alert-box error.
         
    release object lv-mail.
    release object namespace.
  return not error-status:error.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeSetListener) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeSetListener Procedure 
FUNCTION OfficeSetListener RETURNS COM-HANDLE
  ( pv-app AS com-handle,
    pv-doc AS COM-HANDLE, /* not used but may be usefull later*/
    pv-type AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: set the handle to listen on for any office events 
    Notes:  
------------------------------------------------------------------------------*/

DEF VAR lv-event AS Char NO-UNDO.
DEF VAR lv-suite AS CHAR NO-UNDO.

lv-suite = officegetsuite().

CASE pv-type:
    WHEN 'wordprocessor' THEN lv-event = "DocEvents".
    OTHERWISE lv-event = 'undefined'.
END CASE.

CASE lv-suite:
    when 'Microsoft' or 
    when 'Softmaker' OR
    WHEN 'kingsoft' THEN do:
        iF VALID-HANDLE(pv-app) THEN pv-app:enable-events(lv-event) no-error.
        if error-status:error 
        then Message Lv-suite ' Does not Support Event Trapping'.
    end.
    WHEN 'libreoffice' THEN DO:
        IF VALID-HANDLE(pv-app) THEN DO:
            create "OOoevent.Listener" ch-appListener.
            ch-applistener:enable-events("OOoEvents").       
            pv-app:addEventListener(ch-appListener). 
        END.                           
        IF VALID-HANDLE(pv-doc) THEN DO:
            create "OOoDocumentEvent.Listener" ch-doclistener.
            ch-docListener:enable-events(lv-event).
            pv-doc:addEventListener(ch-docListener).     
        END.
     END.
END CASE.

  RETURN ch-doclistener.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeSetSuite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeSetSuite Procedure 
FUNCTION OfficeSetSuite returns character
  ( pv-suite AS CHAR ) :
    
    define variable lv-office as character no-undo init '** usedefault'.
IF pv-suite NE '' THEN DO:
    lv-office = pv-suite.
END.
ELSE DO:
/*     lv-office = getSysoptField(getSysCode(),getPractice(),"officeSuite") no-error.  */
/*     if lv-office begins '** ' or lv-office = '?' or lv-office = ?                   */
/*     then                                                                            */
 lv-office = GetCtrl("officeSuite").    
END.

    lv-officeSuite = lv-office.
    return lv-office.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeStartApplication) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeStartApplication Procedure 
FUNCTION OfficeStartApplication returns com-handle
  ( pv-appName as char,
    pv-hidden as char ) :

def var ch-applhandle as com-handle no-undo.
define variable lv-appname    as char       no-undo.

lv-officesuite = OfficeSetSuite(lv-officesuite).

define variable x as integer no-undo.
define variable lv-up as log no-undo.
def var ch-LibreDesktop as com-handle no-undo.
/*
maybe need to use wapiisrunning('winword.exe') to 
see if its already up and then connect or run
*/
pv-hidden = if session:remote then 'hidden'
                              else pv-hidden.
if pv-appname = 'word97' then pv-appname = 'Word97.Application'.
else
case lv-officesuite:
    when 'Microsoft' then do:
        case pv-appname:
            when 'word97'        then pv-appname = 'Word97.Application'.
            when 'wordprocessor' then pv-appname = 'Word.Application'.
            when 'spreadsheet'   then pv-appname = 'Excel.Application'.
            when 'email'         then pv-appname = 'outlook.Application'.
            when 'db'            then pv-appname = 'Access.application'.
            when 'presentation'  then pv-appname = 'powerpoint.application'. 
        end case.
    end.
    WHEN 'kingsoft' then do:
        case pv-appname:
            when 'wordprocessor' then pv-appname = 'KWPS.Application'.
            when 'spreadsheet'   then pv-appname = 'KET.Application'.
            when 'presentation'  then pv-appname = 'kwpp.application'. 
        end case.
    end.
     WHEN 'SoftMaker' then do:
        case pv-appname:
            when 'wordprocessor' then pv-appname = 'TextMaker.Application'.
            when 'spreadsheet'   then pv-appname = 'PlanMaker.Application'.
            when 'presentation'  then pv-appname = 'SoftMakerPresentations.application'. 
        end case.
    end.
    when 'LibreOffice' then pv-appname = "com.sun.star.ServiceManager".
end case.

  create value(pv-appname) ch-Applhandle connect no-error.
  if error-status:error 
  then do:
       create value(pv-appname) ch-Applhandle no-error.
       if error-status:error 
       then do:
        message 'Cannot Start ' lv-officesuite pv-appname skip
                Error-Status:Get-Message(Error-Status:Num-Messages)
        view-as alert-box error.
        return error.
       end.
       lv-up = false.
  end.
  else lv-up = true.
  
  if pv-appname = 'outlook.Application' 
    then lv-appname = ch-Applhandle:application:name.
    else lv-appname = OfficeApplicationName(ch-Applhandle).

  x = lookup(lv-appname,lv-applist).
  lv-wasup[x] = lv-up. 

  if lv-officesuite = 'LibreOffice'
  then do:
        ch-LibreDesktop = ch-Applhandle:createInstance("com.sun.star.frame.Desktop").
        ch-LoSrvMgr = ch-Applhandle.
        setsysvar('LibreOfficeServiceManager',string(ch-LoSrvMgr)).
        ch-Applhandle = ch-LibreDesktop.
  end.
 if lv-officesuite = 'microsoft' THEN DO:
    case entry(1,string(ch-applhandle:version),'.'):
           when "7" then /* "95" */.
           when "8" then /* "97" */.
           when "9" then /* "2000" */.
           when "10" then /* "2002" */.
           when "11" then /* "2003" */.
           when "12" then /* "2007" */.
           when "14" then do: /* "2010" */
        /* funny problem where some print requests would get stuck waiting for
         other print jobs to finish.  Only certain templates were affected,
         but it appears that the following line was necessary for those
         templates to print. Note: printing in the background means the user
         can continue working in Word while a document is printing.  We don't
         actually care if the user can do anything because they couldn't in
         the first place. */
            ch-applhandle:Options:PrintBackground = false.
           end.
     end case.
  END.
  ch-applhandle:visible = (pv-hidden = 'visible') no-error.  
  IF pv-hidden = 'VISIBLE' THEN officetofront(ch-applhandle).

  return ch-applhandle.  

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeToFront) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeToFront Procedure 
FUNCTION OfficeToFront RETURNS CHARACTER
  ( ch-application AS COM-HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

 CASE lv-officesuite:
    when 'Microsoft' or when 'Softmaker' OR 
    WHEN 'kingsoft' then do:
        ASSIGN 
            Ch-Application:visible = yes.
            /* 0=normal, 1=maximized, 2=minimized; moves Word to front */
/*             Ch-Application:WindowState = 2  */
/*             Ch-Application:WindowState = 0. */
        Ch-Application:Activate(). /* puts cursor in window for editing */
    END.
    WHEN "libreoffice" THEN  .
 END CASE.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeToUrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeToUrl Procedure 
FUNCTION OfficeToUrl returns character
  ( pv-doc as char ) :
  define variable lv-backslash as char  no-undo.


  if opsys = "unix" then lv-backslash = "\\".
  else lv-backslash = "~\".
  
  return if pv-doc begins "file:///" 
    then pv-doc 
    else "file:///" + replace(pv-doc,lv-backslash,"/").
  
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeWorkSheetCellsAutoFit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeWorkSheetCellsAutoFit Procedure 
FUNCTION OfficeWorkSheetCellsAutoFit returns character
   ( pv-worksheet as com-handle,
    pv-startcol as int,
    pv-endcol as int ) :
  
  define variable x as integer no-undo.
  
case lv-officesuite:
    when 'Microsoft' or when 'Softmaker' or WHEN 'kingsoft' then do:
         pv-WorkSheet:Cells:Select.
         pv-WorkSheet:Cells:EntireColumn:AutoFit.
    end.
    when 'LibreOffice' then do:
        /* pv-WorkSheet:SetOptimalColumnWidth. */
        
        do x = pv-startcol to pv-endcol:
            pv-WorkSheet:columns(x):OptimalWidth = true. 
        end.
        
    end.
end case.
 
  return "".   /* Function return value. */
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OfficeWriteCellData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OfficeWriteCellData Procedure 
FUNCTION OfficeWriteCellData returns character
  ( pv-worksheet as com-handle,
    pv-col as int,
    pv-row as int,
    pv-data as char ) :
    
    define variable lv-range as character no-undo.
    def var ch-cell as com-handle no-undo.
    
case lv-officesuite:
    when 'Microsoft' or when 'Softmaker' or WHEN 'kingsoft' then do:
        lv-range = getexcelcolumnname(pv-col) + string(pv-row).
        pv-WorkSheet:Range(lv-range):Value = ' ' + pv-data.
        if isnumeric(pv-data) and pv-data begins '0'
        then pv-WorkSheet:Range(lv-range):NumberFormat = fill('0',length(trim(pv-data))).
    end.
    when 'LibreOffice' then do:
        ch-Cell = pv-WorkSheet:GetCellByPosition(pv-col,pv-row).
        if isnumeric(pv-data) and pv-data begins '0'
        then do:
            if IsInteger(pv-data) 
            then ch-Cell:setValue(integer(pv-data)).
            else do:
                ch-Cell:setValue(decimal(pv-data)).
                /* GOTTO BE A BETTER WAY !!!!
                formatNumber = ch-Workbook:getNumberFormats:queryKey(formatToUse,ch-Workbook:charLocale,false).
               /* if our desired format is not in the work book, then formatNumber will be
                  set to "-1".  In that case, we will create our desired format. */
               if formatNumber = -1 
               then formatNumber = chWorkbook:getNumberFormats:addNew(formatToUse,ch-Workbook:charLocale).
               /* set the cell's format to the correct format number */
               ch-Cell:NumberFormat = formatNumber.
               */
            end.
        end.
        else ch-Cell:setString(pv-data).
    end.
end case.
 
  return "".   /* Function return value. */
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

