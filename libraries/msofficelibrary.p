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
this-procedure:private-data = "library-msoffice".
&glob library-msoffice
&glob library-program
{app-paths.i}

def var lv-wasup as log extent 3 no-undo init no.
def var lv-applist as char no-undo init
'Microsoft Word,Microsoft Excel,Outlook'.
def stream ip.
def stream op.

&glob wdOpenFormatAuto 0

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-MsAlignColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsAlignColumns Procedure 
FUNCTION MsAlignColumns RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-tablehandle as com-handle,
    pv-columns as char,
    pv-value as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsAlignSelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsAlignSelection Procedure 
FUNCTION MsAlignSelection RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-value as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsApplicationNAme) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsApplicationNAme Procedure 
FUNCTION MsApplicationNAme RETURNS CHARACTER
  ( pv-applhandle as com-handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsApplicationRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsApplicationRunning Procedure 
FUNCTION MsApplicationRunning RETURNS COM-HANDLE
  ( pv-appName as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSCenterAll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSCenterAll Procedure 
FUNCTION MSCenterAll RETURNS LOGICAL
  ( pv-applhandle as com-handle) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSCloseApplication) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSCloseApplication Procedure 
FUNCTION MSCloseApplication RETURNS LOGICAL
  ( pv-applhandle as com-handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsCloseDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsCloseDocument Procedure 
FUNCTION MsCloseDocument RETURNS LOGICAL
  ( pv-applhandle as com-handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSCopySelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSCopySelection Procedure 
FUNCTION MSCopySelection RETURNS LOGICAL
  ( pv-applhandle as com-handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsDeleteSelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsDeleteSelection Procedure 
FUNCTION MsDeleteSelection RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-extrachars as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSGetDocHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSGetDocHandle Procedure 
FUNCTION MSGetDocHandle RETURNS COM-HANDLE
( pv-applhandle as com-handle,
  pv-document   as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsHighlightColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsHighlightColumns Procedure 
FUNCTION MsHighlightColumns RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-columns as char,
    pv-value as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsInsertTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsInsertTable Procedure 
FUNCTION MsInsertTable RETURNS COM-HANDLE
  ( pv-applhandle as com-handle,
    pv-data as char,
    pv-font as char,
    pv-size as int,
    pv-extras as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSInsertWorkbook) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSInsertWorkbook Procedure 
FUNCTION MSInsertWorkbook RETURNS COM-HANDLE
  ( pv-applhandle as com-handle,
    pv-data as char,
    pv-extra as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSMailMerge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSMailMerge Procedure 
FUNCTION MSMailMerge RETURNS CHARACTER(
   pv-applhandle as com-handle,
   pv-datafile as char, 
   pv-destination as int,
   output pv-numrecs as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsMergeToExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsMergeToExcel Procedure 
FUNCTION MsMergeToExcel RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-data as char,
    pv-template as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsMergeToOutlook) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsMergeToOutlook Procedure 
FUNCTION MsMergeToOutlook RETURNS LOGICAL
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

&IF DEFINED(EXCLUDE-MsMergeToWord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsMergeToWord Procedure 
FUNCTION MsMergeToWord RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-data as char,
    pv-template as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsNumColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsNumColumns Procedure 
FUNCTION MsNumColumns RETURNS INTEGER
  ( pv-tablehandle as com-handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSOpenApplication) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSOpenApplication Procedure 
FUNCTION MSOpenApplication RETURNS COM-HANDLE
  ( pv-appName as char,
    pv-hidden as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSOpenDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSOpenDocument Procedure 
FUNCTION MSOpenDocument RETURNS com-handle
  ( pv-applhandle as com-handle,
    pv-Document as char,
    pv-readonly as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSOpenTemplate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSOpenTemplate Procedure 
FUNCTION MSOpenTemplate RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-Document as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSPrint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSPrint Procedure 
FUNCTION MSPrint RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-tofile as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsPrintMe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsPrintMe Procedure 
FUNCTION MsPrintMe RETURNS LOGICAL
  ( pv-doc as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSSaveAs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSSaveAs Procedure 
FUNCTION MSSaveAs RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-name       as char,
    pv-document   as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSSetMargins) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSSetMargins Procedure 
FUNCTION MSSetMargins RETURNS LOGICAL
  ( pv-handle as com-handle,
    pv-top as dec,
    pv-bottom as dec,
    pv-left as dec,
    pv-right as  dec )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsSetPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsSetPrinter Procedure 
FUNCTION MsSetPrinter RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-printername as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSSetSaved) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSSetSaved Procedure 
FUNCTION MSSetSaved RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-document as char,
    pv-mode as log)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsSetSelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsSetSelection Procedure 
FUNCTION MsSetSelection RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-string as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSSetSelectionFont) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSSetSelectionFont Procedure 
FUNCTION MSSetSelectionFont RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-font as char,
    pv-size as int,
    pv-extras as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSShadeSelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSShadeSelection Procedure 
FUNCTION MSShadeSelection RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-value as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSSignDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSSignDocument Procedure 
FUNCTION MSSignDocument RETURNS LOGICAL
  (pv-Document as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsSizeColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsSizeColumns Procedure 
FUNCTION MsSizeColumns RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-tablehandle as com-handle,
    pv-columns as char,
    pv-value as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsUpdateFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MsUpdateFields Procedure 
FUNCTION MsUpdateFields RETURNS LOGICAL
  (pv-applhandle as com-handle )  FORWARD.

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
         HEIGHT             = 30.19
         WIDTH              = 47.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-MsAlignColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsAlignColumns Procedure 
FUNCTION MsAlignColumns RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-tablehandle as com-handle,
    pv-columns as char,
    pv-value as int ) :

def var lv-columnhandle as com-handle no-undo.
def var x as int no-undo.
def var i as int no-undo.

do x = 1 to num-entries(pv-columns,':'):
    i = int(entry(x,pv-columns,':')).
    lv-columnhandle = pv-tablehandle:columns:item(i) no-error.
    if error-status:error then return error.
    lv-columnhandle:select() no-error.
    if error-status:error then return error.
    MSAlignSelection(pv-applhandle,pv-value) no-error.
    if error-status:error then return error.
end.

  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsAlignSelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsAlignSelection Procedure 
FUNCTION MsAlignSelection RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-value as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*     Selection.ParagraphFormat.Alignment = wdAlignParagraphRight */
    pv-applhandle:selection:ParagraphFormat:Alignment = pv-value no-error.

  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsApplicationNAme) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsApplicationNAme Procedure 
FUNCTION MsApplicationNAme RETURNS CHARACTER
  ( pv-applhandle as com-handle ) :

  RETURN pv-applhandle:application:name.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsApplicationRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsApplicationRunning Procedure 
FUNCTION MsApplicationRunning RETURNS COM-HANDLE
  ( pv-appName as char ) :


/*  ??? check its not already running
If Tasks.Exists(Name:="Microsoft Excel") = True Then
    Set myobject = GetObject("", "Excel.Application")
    MsgBox myobject.Application.StartupPath
    Set myobject = Nothing
End If
*/

  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSCenterAll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSCenterAll Procedure 
FUNCTION MSCenterAll RETURNS LOGICAL
  ( pv-applhandle as com-handle):

/*   ch-table = pv-applhandle:Selection:Tables:item(1) */
/* pv-applhandle:selection: = pv-font.  */

  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSCloseApplication) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSCloseApplication Procedure 
FUNCTION MSCloseApplication RETURNS LOGICAL
  ( pv-applhandle as com-handle ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
if not valid-handle(pv-applhandle) then return true.
def var x as int no-undo.
x = lookup(MsApplicationName(pv-Applhandle),lv-applist).
    if not lv-wasup[x]
    then do:
        pv-applhandle:QUIT() no-error. 
        RELEASE OBJECT pv-applhandle no-error.
        if error-status:error then return error.   
    End.

RETURN not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsCloseDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsCloseDocument Procedure 
FUNCTION MsCloseDocument RETURNS LOGICAL
  ( pv-applhandle as com-handle) :
/* com handle of document to close */
/* (SaveChanges, OriginalFormat, RouteDocument) */
    pv-applhandle:close({&wdDoNotSaveChanges}) no-error.
return not error-status:error.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSCopySelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSCopySelection Procedure 
FUNCTION MSCopySelection RETURNS LOGICAL
  ( pv-applhandle as com-handle ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  pv-applhandle:selection:COPY() no-error.
  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsDeleteSelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsDeleteSelection Procedure 
FUNCTION MsDeleteSelection RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-extrachars as int ) :
   
    pv-applhandle:SELECTION:delete() no-error.
    if error-status:error then return error.
    pv-applhandle:SELECTION:collapse() no-error.
    if error-status:error then return error.
    pv-applhandle:SELECTION:delete(1,pv-extrachars) no-error.
    
  RETURN not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSGetDocHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSGetDocHandle Procedure 
FUNCTION MSGetDocHandle RETURNS COM-HANDLE
( pv-applhandle as com-handle,
  pv-document   as char) :

def var x          as int no-undo.
def var lv-name    as char no-undo.

pv-document = entry(num-entries(pv-document,'\'),pv-document,'\').

do x = 1 to 99:
   lv-name =  pv-applhandle:documents:ITEM(x):name no-error.
   if lv-name = ? then leave. 
   if lv-name = pv-document 
   then do:
       pv-applhandle = pv-applhandle:documents:ITEM(x) no-error.
       leave.
   End.
End.
if lv-name = ? or x > 99 then pv-applhandle = ?.

return pv-applhandle.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsHighlightColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsHighlightColumns Procedure 
FUNCTION MsHighlightColumns RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-columns as char,
    pv-value as int ) :

def var lv-columnhandle as com-handle no-undo.
def var x as int no-undo.

do x = 1 to num-entries(pv-columns,':'):
    lv-columnhandle = pv-applhandle:columns:item(int(entry(x,pv-columns,':'))) no-error.
    if error-status:error then return error.
    MSShadeSelection(lv-columnhandle,pv-value) no-error.
    if error-status:error then return error.
end.

  RETURN not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsInsertTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsInsertTable Procedure 
FUNCTION MsInsertTable RETURNS COM-HANDLE
  ( pv-applhandle as com-handle,
    pv-data as char,
    pv-font as char,
    pv-size as int,
    pv-extras as char) :

def var ch-table  as com-handle no-undo.
def var ch-titles as com-handle no-undo.

    assign   /* default border settings */
        pv-applhandle:Options:DefaultBorderLineStyle = {&wdLineStyleSingle}
        pv-applhandle:Options:DefaultBorderLineWidth = {&wdLineWidth050pt}
        pv-applhandle:Options:DefaultBorderColor     = {&wdColorAutomatic} no-error.
   if error-status:error then return error.
    pv-applhandle:Selection:range:InsertDatabase(0,32,False,"","","","","","","",pv-data,-1,-1,True) no-error.
    if error-status:error then return error.
    pv-applhandle:Selection:rows:headingformat = true no-error.
    if error-status:error then return error.
    ch-table = pv-applhandle:Selection:Tables:item(1) no-error. /* get new table handle */
    if error-status:error then return error.
    ch-titles = ch-table:rows:item(1) no-error.                 /* get title row handle */
    if error-status:error then return error.
    ch-Table:Select() no-error.
    if error-status:error then return error.
    MSSetSelectionFont(pv-applhandle,pv-font,pv-size,'') no-error.
    if error-status:error then return error.

    assign      /* remove borders from body of report */
        ch-table:TopPadding = 0
        ch-table:BottomPadding = 0
        ch-table:LeftPadding = 5
        ch-table:RightPadding = 0
        ch-table:Spacing = 0
        ch-table:Borders:item({&wdBorderHorizontal}):LineStyle   = {&wdLineStyleNone} 
        ch-table:Borders:item({&wdBorderVertical}):LineStyle     = {&wdLineStyleNone}
        ch-table:Borders:item({&wdBorderDiagonalDown}):LineStyle = {&wdLineStyleNone}
        ch-table:Borders:item({&wdBorderDiagonalUp}):LineStyle   = {&wdLineStyleNone}
        ch-table:Borders:Shadow                                  = False no-error.
    if error-status:error then return error.
    ch-Titles:Select() no-error. 
    if error-status:error then return error.
    assign       /* put border around title row */
        pv-applhandle:Selection:Cells:Borders:item({&wdBorderTop}):LineStyle    = {&wdLineStyleSingle}
        pv-applhandle:Selection:Cells:Borders:item({&wdBorderBottom}):LineStyle = {&wdLineStyleSingle} no-error.
    if error-status:error then return error.
    
   RETURN  ch-table.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSInsertWorkbook) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSInsertWorkbook Procedure 
FUNCTION MSInsertWorkbook RETURNS COM-HANDLE
  ( pv-applhandle as com-handle,
    pv-data as char,
    pv-extra as char) :

def var lv-command as char no-undo.
def var lv-r as com-handle no-undo.

  pv-applhandle:Workbooks:Add() no-error.

assign
    lv-command  = "TEXT;" + getfullpath(pv-data)
    lv-r        = pv-applhandle:ActiveSheet:range('A1')
    pv-applhandle:ActiveSheet:Name              = 'Data Target'
    pv-applhandle = pv-applhandle:ActiveSheet:QueryTables:Add(lv-command,lv-r)
    pv-applhandle:FieldNames                    = True
    pv-applhandle:RefreshStyle                  = {&xlInsertDeleteCells}
    pv-applhandle:AdjustColumnWidth             = True
    pv-applhandle:TextFileParseType             = {&xlDelimited}
    pv-applhandle:TextFileTextQualifier         = {&xlTextQualifierDoubleQuote}
    pv-applhandle:TextFileConsecutiveDelimiter  = False
    pv-applhandle:TextFileTabDelimiter          = False
    pv-applhandle:TextFileSemicolonDelimiter    = False
    pv-applhandle:TextFileCommaDelimiter        = False
    pv-applhandle:TextFileSpaceDelimiter        = False
    pv-applhandle:TextFileOtherDelimiter        = "{&Delim2}" no-error.
    if error-status:error then return error.
    pv-applhandle:Refresh() no-error.
    if error-status:error then return error.
  RETURN pv-applhandle.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSMailMerge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSMailMerge Procedure 
FUNCTION MSMailMerge RETURNS CHARACTER(
   pv-applhandle as com-handle,
   pv-datafile as char, 
   pv-destination as int,
   output pv-numrecs as int) :

   def var lv-ch as com-handle no-undo.

   lv-ch = pv-applhandle:activeDocument:mailmerge no-error.
   if error-status:error then return '1:' + Error-Status:Get-Message(Error-Status:Num-Messages).

/* OpenDataSource(Name, ConfirmConversions, ReadOnly, LinkToSource, 
AddToRecentFiles, PasswordDocument, PasswordTemplate, Revert, WritePasswordDocument, 
WritePasswordTemplate, Connection, SQLStatement, SQLStatement1) */

/* The following macro was used to get the call correct:

NO-RETURN-VALUE <com-handle>: OpenDataSource ( 
          Character-Name,
          <anytype>-Format BY-VARIANT-POINTER,
          <anytype>-ConfirmConversions BY-VARIANT-POINTER,
          <anytype>-ReadOnly BY-VARIANT-POINTER,
          <anytype>-LinkToSource BY-VARIANT-POINTER,
          <anytype>-AddToRecentFiles BY-VARIANT-POINTER,
          <anytype>-PasswordDocument BY-VARIANT-POINTER,
          <anytype>-PasswordTemplate BY-VARIANT-POINTER,
          <anytype>-Revert BY-VARIANT-POINTER,
          <anytype>-WritePasswordDocument BY-VARIANT-POINTER,
          <anytype>-WritePasswordTemplate BY-VARIANT-POINTER,
          <anytype>-Connection BY-VARIANT-POINTER,
          <anytype>-SQLStatement BY-VARIANT-POINTER,
          <anytype>-SQLStatement1 BY-VARIANT-POINTER ).

    ActiveDocument.MailMerge.OpenDataSource 
    Name:="C:\rex\temp\docdata.dat", 
    ConfirmConversions:=False, 
    ReadOnly:=False, 
    LinkToSource:=True, 
    AddToRecentFiles:=False, 
    PasswordDocument:="", 
    PasswordTemplate:="", 
    WritePasswordDocument:="", 
    WritePasswordTemplate:="", 
    Revert:=False, 
    Format:=wdOpenFormatAuto, 
    Connection:="", 
    SQLStatement:="", 
    SQLStatement1:=""
  where wdOpenFormatAuto = 0
*/


/*     lv-ch:opendatasource(pv-datafile,false,false,true,false,,,,,false,0).                         */
/*     if error-status:error then return '2:' + Error-Status:Get-Message(Error-Status:Num-Messages). */

/*     lv-ch:opendatasource(pv-datafile,,false,false,true,false,,,,,false,0,,,). */
   lv-ch:opendatasource(pv-datafile,{&wdOpenFormatAuto},true,true,true,false) no-error.
   if error-status:error then return '2:' + Error-Status:Get-Message(Error-Status:Num-Messages).

/* set start rec to second record bodge for 2003 */
   lv-ch:DataSource:FirstRecord = 2. 

   lv-ch:destination = pv-destination no-error.
   if error-status:error then return '3:' + Error-Status:Get-Message(Error-Status:Num-Messages).

   lv-ch:execute(true) no-error. /* disp dialog box if we have errors */
   if error-status:error then return '4:' + Error-Status:Get-Message(Error-Status:Num-Messages).

   lv-ch:datasource:activerecord = {&wdlastDataSourceRecord} no-error.

   /* if error-status:error then return '5:' + Error-Status:Get-Message(Error-Status:Num-Messages). */
   pv-numrecs = lv-ch:datasource:activerecord no-error.

   if not error-status:error then do:
/*       message "returning" pv-applhandle:activeDocument:name  */
/*          view-as alert-box info buttons OK.                  */
      RETURN pv-applhandle:activeDocument:name.
   end.
   else do:
      message "returning error"
         view-as alert-box info buttons OK.
      pv-numrecs = 0.
      return error.
   End.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsMergeToExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsMergeToExcel Procedure 
FUNCTION MsMergeToExcel RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-data as char,
    pv-template as char ) :

/*  msopendocument(h-applhandle,pv-template) no-error.  
    if not error-status:error then
*/
        MsInsertWorkbook(pv-applhandle,pv-data,'') no-error.


    RETURN not error-status:error.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsMergeToOutlook) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsMergeToOutlook Procedure 
FUNCTION MsMergeToOutlook RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-from as char,
    pv-to   as char,
    pv-subject as char,
    pv-text as  char,
    pv-attach as char,
    pv-expires as char ) :

DEF VAR lv-mail AS COM-HANDLE NO-UNDO.
def var namespace as com-handle no-undo.
namespace = pv-applhandle:Getnamespace("MAPI").
namespace:logon(getsysvar('{&clv}user')).

    ASSIGN lv-mail         = pv-applhandle:createItem(0)
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
                 Error-Status:Get-Message(Error-Status:Num-Messages) 
         view-as alert-box error.
        
    release object lv-mail.
    release object namespace.
  RETURN not error-status:error.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsMergeToWord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsMergeToWord Procedure 
FUNCTION MsMergeToWord RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-data as char,
    pv-template as char ) :

    msopendocument(pv-applhandle,pv-template,'') no-error.
    if not error-status:error then do:
        MSSetSelection(pv-Applhandle,' ') no-error.
        if not error-status:error then
            MsInsertTable(pv-applhandle,pv-data,'arial',8,'') no-error.
    End.

    RETURN not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsNumColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsNumColumns Procedure 
FUNCTION MsNumColumns RETURNS INTEGER
  ( pv-tablehandle as com-handle) :

  RETURN pv-tablehandle:columns:count.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSOpenApplication) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSOpenApplication Procedure 
FUNCTION MSOpenApplication RETURNS COM-HANDLE
  ( pv-appName as char,
    pv-hidden as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var ch-applhandle as com-handle no-undo.

def var x as int no-undo.
/*
maybe need to use wapiisrunning('winword.exe') to 
see if its already up and then connect or run
*/
  CREATE value(pv-appname) ch-Applhandle connect no-error.

  if error-status:error 
  then do:
       CREATE value(pv-appname) ch-Applhandle no-error.
       x = lookup(MsApplicationName(ch-Applhandle),lv-applist).
       lv-wasup[x] = false.
  End.
  else do:
      x = lookup(MsApplicationName(ch-Applhandle),lv-applist).
      lv-wasup[x] = true.
  end.
  pv-hidden = if session:remote then 'hidden'
                                else pv-hidden.
  ch-applhandle:visible = (pv-hidden = 'visible') no-error.    
  RETURN ch-applhandle.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSOpenDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSOpenDocument Procedure 
FUNCTION MSOpenDocument RETURNS com-handle
  ( pv-applhandle as com-handle,
    pv-Document as char,
    pv-readonly as char) :

   def var ch-document as com-handle  no-undo.
   /*
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
   Visible)
   */
   def var lv-readonly as log no-undo.

   
   lv-readonly = pv-readonly = 'readonly'.

   case MsApplicationName(pv-applhandle):
      when 'Microsoft Excel' then ch-document = 
         pv-applhandle:Workbooks:Open(pv-document,no,lv-readonly) no-error.
      when 'Microsoft Word'  then ch-document =
         pv-applhandle:documents:Open(pv-document,no,lv-readonly) no-error.
   end case.

   RETURN ch-document.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSOpenTemplate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSOpenTemplate Procedure 
FUNCTION MSOpenTemplate RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-Document as char) :
/*
   DocumentName
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
        XMLTransform).
*/
    case MsApplicationName(pv-applhandle):
        when 'Microsoft Excel' 
            then  pv-applhandle:Workbooks:Open(pv-document,,true) no-error.
        when 'Microsoft Word' 
            then  do:
               pv-applhandle:documents:OPEN(pv-document,,true,,,,) no-error.
         /*    pv-applhandle:documents:OPEN(pv-document,,"true",,,,) no-error. */
            end.
    end case.
  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSPrint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSPrint Procedure 
FUNCTION MSPrint RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-tofile as char ) :


    case MsApplicationName(pv-applhandle):
        when 'Microsoft Excel' 
            then do:
                /*  MSExcel format 
                PrintOut(From,To,Copies,Preview,ActivePrinter,PrintToFile,Collate,PrToFileName) */
                if pv-tofile = '' 
                then pv-applhandle:ActiveWorkbook:PrintOut no-error.
                else pv-applhandle:ActiveWorkbook:PrintOut(,,,,,true,,pv-tofile) no-error.
        End.
        when 'Microsoft Word' 
            then do:
                /*  MSWord format
                PrintOut(Background,Append,Range,OutputFileName,From,To,Item,Copies,Pages,PageType, 
                         PrintToFile,Collate,FileName,ActivePrinterMacGX,ManualDuplexPrint, 
                         PrintZoomColumn,PrintZoomRow,PrintZoomPaperWidth,PrintZoomPaperHeight) */
                if pv-tofile = '' 
                then pv-applhandle:PrintOut(true) no-error.
                else pv-applhandle:PrintOut(true,false,{&wdPrintAllDocument},
                                            pv-tofile,,,,,,true) no-error.

            End.
        end case.

  RETURN not error-status:error. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsPrintMe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsPrintMe Procedure 
FUNCTION MsPrintMe RETURNS LOGICAL
  ( pv-doc as char ) :

def var lv-apphandle as com-handle no-undo.

lv-apphandle = msopenapplication('word.application','hidden') no-error.

MsOpenDocument(lv-apphandle,pv-doc,'readonly') no-error.
    if error-status:error then return error.
MSSetPrinter(lv-apphandle,Session:printer-name) no-error.
    if error-status:error then return error.
MSPrint(lv-apphandle,'') no-error.
    if error-status:error then return error.
MSSetSaved(lv-apphandle,pv-doc,true) no-error.
    if error-status:error then return error.
MSCloseApplication(lv-apphandle).

RETURN not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSSaveAs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSSaveAs Procedure 
FUNCTION MSSaveAs RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-name       as char,
    pv-document   as char) :

def var lv-store   as char no-undo.
def var ch-doc as com-handle no-undo.
def var lv-name as char no-undo.

&if '{&opsys}' = 'win32' &then
assign pv-document = replace(pv-document,'\','/')
       lv-store = pv-name
       pv-name = getfullpath(pv-name)
       lv-name = MsApplicationName(pv-applhandle) no-error.
if pv-name = ? then pv-name = lv-store.

case lv-name:
    when 'Microsoft Word' then do:
        ch-doc = MsGetDocHandle(pv-applhandle,pv-document).
        if valid-handle(ch-doc) 
        then ch-doc:SAVEAS(pv-name).
    End.
    when 'Microsoft Excel' then do:
        pv-applhandle:ActiveWorkbook:SaveAs(pv-name,,,,,,) no-error.
    End.
end case.
&endif

  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSSetMargins) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSSetMargins Procedure 
FUNCTION MSSetMargins RETURNS LOGICAL
  ( pv-handle as com-handle,
    pv-top as dec,
    pv-bottom as dec,
    pv-left as dec,
    pv-right as  dec ) :

def var lv-pointmult as dec no-undo init 28.5.    

pv-handle:range:select().
assign pv-top    = pv-top    * lv-pointmult
       pv-bottom = pv-bottom * lv-pointmult
       pv-left   = pv-left   * lv-pointmult
       pv-right  = pv-right  * lv-pointmult.

if pv-top ne ? then
    pv-handle:PageSetup:TopMargin = pv-top no-error.

if pv-bottom ne ? then
    pv-handle:PageSetup:BottomMargin = pv-bottom no-error.

if pv-left ne ? then
    pv-handle:PageSetup:LeftMargin = pv-left no-error.

if pv-right ne ? then
    pv-handle:PageSetup:RightMargin = pv-right no-error.

  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsSetPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsSetPrinter Procedure 
FUNCTION MsSetPrinter RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-printername as char) :
    
case MSApplicationName(pv-applhandle):
    when 'Microsoft Excel' then 
        pv-applhandle:ActivePrinter = pv-printername no-error.
    when 'Microsoft Word' then 
        pv-applhandle:ActivePrinter = entry(1,pv-printername,' ') no-error.
    when 'Microsoft Outlook' then .
end case.

    RETURN not error-status:error.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSSetSaved) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSSetSaved Procedure 
FUNCTION MSSetSaved RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-document as char,
    pv-mode as log) :

def var x as int no-undo.
def var lv-name as char no-undo.

pv-document = GetFileName(pv-document).

Case MsApplicationName(pv-applhandle):
    when 'Microsoft Excel' then do:
        pv-applhandle = pv-applhandle:ActiveWorkbook no-error.
        pv-applhandle:saved = pv-mode no-error.
    end.
    when 'Microsoft Word' then 
        do x = 1 to 99:
            lv-name =  pv-applhandle:documents:ITEM(x):name no-error.
            if lv-name = ? then leave.
            if lv-name matches pv-document 
            then do:
                pv-applhandle =  pv-applhandle:documents:ITEM(x) no-error.
                pv-applhandle:saved = pv-mode no-error.
            End.
        End.
end Case.
        
  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsSetSelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsSetSelection Procedure 
FUNCTION MsSetSelection RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-string as char ) :
/* move to top of doc so we find it */
pv-applhandle:Selection:HomeKey({&wdStory}) no-error.
    if error-status:error then return error.
pv-applhandle:Selection:EndKey({&wdStory},{&wdExtend}) no-error.
    if error-status:error then return error.

if pv-string ne "*all*" then do:
    pv-applhandle:selection:FIND:EXECUTE(pv-string,,TRUE) no-error.
    if error-status:error then return error.
    pv-applhandle:SELECTION:expand() no-error.
    if error-status:error then return error.
    pv-applhandle:SELECTION:select() no-error.  
    if error-status:error then return error.
End.

  RETURN not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSSetSelectionFont) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSSetSelectionFont Procedure 
FUNCTION MSSetSelectionFont RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-font as char,
    pv-size as int,
    pv-extras as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var x as int no-undo.
  pv-applhandle:selection:FONT:NAME = pv-font no-error.
    if error-status:error then return error.
  pv-applhandle:selection:font:SIZE = pv-size no-error.
/*
  do x = 1 to num-entries(pv-extras):
    pv-applhandle:SELECTION:FONT:value(entry(x,pv-extras) = 1.
  End.
*/
  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSShadeSelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSShadeSelection Procedure 
FUNCTION MSShadeSelection RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-value as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
/*    Selection.Cells.                wdTexture20Percent */

    pv-applhandle:Shading:Texture = pv-value no-error.

  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MSSignDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSSignDocument Procedure 
FUNCTION MSSignDocument RETURNS LOGICAL
  (pv-Document as char) :

   def var h-Application as com-handle no-undo.

   /* open application */
   h-Application = MSOpenApplication("Word","hidden").

   /* open document */
   MSOpenDocument(h-application,pv-Document,"").  /* not readonly */

   /* move to end of document */
   h-Application:Selection:HomeKey({&wdStory}) no-error.
   if error-status:error then return error.
   h-Application:Selection:EndKey({&wdStory},{&wdExtend}) no-error.
   if error-status:error then return error.
   
   /* insert text */
   h-Application:Insert("signed today").
   
   /* save document */
   MSSaveAs(h-application,pv-Document,pv-Document).

   /* close document */
   MSCloseDocument(h-application).

   /* close Word */
   MSCloseApplication(h-application).

  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsSizeColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsSizeColumns Procedure 
FUNCTION MsSizeColumns RETURNS LOGICAL
  ( pv-applhandle as com-handle,
    pv-tablehandle as com-handle,
    pv-columns as char,
    pv-value as int ) :

def var lv-columnhandle as com-handle no-undo.
def var x as int no-undo.
def var lv-points as dec no-undo init 0.15.

if pv-value ne ? then
    lv-points = lv-points + (pv-value * 4.95). /* guess for font size * number of chars */

do x = 1 to num-entries(pv-columns,':'):
    lv-columnhandle = pv-tablehandle:columns:item(int(entry(x,pv-columns,':'))) no-error.
    if not valid-handle(lv-columnhandle) then return false.
    lv-columnhandle:select() no-error.
    if pv-value ne ? 
    then pv-applhandle:selection:columns:item(1):width = lv-points no-error. 
    else pv-applhandle:selection:columns:item(1):autofit() no-error.
    if error-status:error then return error.
end.

  RETURN  not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MsUpdateFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MsUpdateFields Procedure 
FUNCTION MsUpdateFields RETURNS LOGICAL
  (pv-applhandle as com-handle ) :
    pv-applhandle:ActiveDocument:Fields:Update() no-error.
  RETURN not error-status:error.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

