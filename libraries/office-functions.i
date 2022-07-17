FUNCTION libredispatch RETURNS CHARACTER (
    pv-doc AS COM-HANDLE,
    pv-action AS CHAR,
    pv-names AS CHAR,
    pv-values AS CHAR) IN {&lhandle}.

FUNCTION OfficePickPrinter RETURNS CHARACTER (
    INPUT ch-application AS COM-HANDLE,
    INPUT ch-document AS COM-HANDLE) IN {&lhandle}.

FUNCTION librehandle RETURNS COM-HANDLE
  ( pv-name AS CHAR ) IN {&lhandle}.

FUNCTION OfficeAppendText RETURNS CHARACTER (
   Input pv-applicATION As Com-handle,
   Input pv-document As Com-handle,
   Input pv-text As Character,
   Input pv-skipbefore As Int,
   Input pv-skipafter As Int)     In {&lhandle}.

function OfficeApplicationName       returns character(
   input  pv-applhandle as com-handle )                   in {&lhandle}.

function OfficeCloseApplication      returns logical(
   input  pv-applhandle as com-handle)                    in {&lhandle}.

function OfficeCloseDocument         returns logical(
   input  pv-document as com-handle)                      in {&lhandle}.

function OfficeGetDocHandle          returns com-handle(
   input  pv-applhandle as com-handle,
   input  pv-document   as character)                     in {&lhandle}.

function GetExcelColumnName          returns character(
   input  lv-col as integer)                              in {&lhandle}.

Function OfficeGetDocName Returns Character (
    Input pv-application As Com-handle,
    Input pv-document As Com-handle)     In {&lhandle}.
function OfficeGetSuite              returns character()  in {&lhandle}.

function OfficeMailMerge             returns COM-HANDLE(
   input  pv-applhandle  as com-handle,
   input  pv-template as com-handle,
   input  pv-datafile    as character,
   input  pv-destination as char,
   output pv-numrecs     as integer)                      in {&lhandle}.

FUNCTION OfficeEditTemplate RETURNS COM-HANDLE
      ( pv-application AS COM-HANDLE,
        pv-template AS COM-HANDLE,
        lv-fieldlist AS CHAR) IN {&lhandle}.

function OfficeNewWorkBook           returns com-handle(
   input  pv-application as com-handle)                   in {&lhandle}.
function OfficeNewWorkSheet          returns com-handle(
   input  pv-workbook as com-handle)                      in {&lhandle}.
function OfficeOpenDocument          returns com-handle(
   input  pv-applhandle as com-handle,
   input  pv-Document   as character,
   input  pv-readonly   as character)                     in {&lhandle}.
function OfficeOpenTemplate          returns COM-HANDLE(
   input  pv-applhandle as com-handle,
   input  pv-Document   as character,
   INPUT  pv-fieldlist AS CHARACTER)                     in {&lhandle}.
FUNCTION OfficePrintme RETURNS CHARACTER (
    INPUT pv-application AS COM-HANDLE,
    INPUT ch-document AS COM-HANDLE,
    INPUT pv-copies AS INT) IN {&lhandle}.

FUNCTION OfficePrintPreview RETURNS LOGICAL (
    INPUT ch-application AS COM-HANDLE,
    INPUT ch-document AS COM-HANDLE) IN {&lhandle}.
function OfficeSaveAs                returns com-handle (
   input  pv-applhandle as com-handle,
	input  pv-document       as com-handle,
	input  pv-asname   as character,
    INPUT  pv-type AS CHAR)                     in {&lhandle}.

function OfficeSearchReplace RETURNS CHARACTER
    ( pv-app AS com-handle,
      pv-doc AS COM-HANDLE,
      pv-direction AS CHAR,
      pv-wrap as char,
      pv-from AS CHAR,
      pv-to AS CHAR ) in {&lhandle}.

function OfficeSendEmail             returns logical(
   input  pv-applhandle as com-handle,
   input  pv-from       as character,
   input  pv-to         as character,
   input  pv-subject    as character,
   input  pv-text       as character,
   input  pv-attach     as character,
	input  pv-expires    as character)                     in {&lhandle}.
FUNCTION OfficePropertyAction RETURNS CHARACTER (
    INPUT pv-application AS COM-HANDLE,
    INPUT pv-document AS COM-HANDLE,
    INPUT pv-property AS CHAR,
    INPUT pv-value AS CHAR)   IN {&lhandle}.

function OfficeSetSuite              returns CHARACTER (INPUT pv-suite AS CHAR)  in {&lhandle}.
FUNCTION OfficeSetListener  RETURNS COM-HANDLE (
    INPUT pv-app AS COM-HANDLE,
    INPUT pv-doc AS COM-HANDLE,
    INPUT pv-type AS CHAR) IN {&lhandle}.
function OfficeStartApplication      returns com-handle(
   input  pv-appName as character,
   input  pv-hidden  as character)                        in {&lhandle}.
function OfficeToFront                 returns character(
   input  pv-application as COM-HANDLE)                            in {&lhandle}.

function OfficeToUrl                 returns character(
   input  pv-doc as character)                            in {&lhandle}.
function OfficeWorkSheetCellsAutoFit returns character(
   input  pv-worksheet as com-handle,
   input  pv-startcol  as integer,
   input  pv-endcol    as integer)                        in {&lhandle}.
function OfficeWriteCellData         returns character(
   input  pv-worksheet as com-handle,
   input  pv-col       as integer,
   input  pv-row       as integer,
   input  pv-data      as character)                      in {&lhandle}.
