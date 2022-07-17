{app-paths.i}
define input parameter pv-report   as char  no-undo.
define input parameter pv-data     as char  no-undo.
define input parameter pv-savedata as log    no-undo.
def input param pv-dest as char no-undo.
def input param pv-from as handle no-undo.

def var chApplication      as com-handle no-undo.
def var chReport           as com-handle no-undo.
def var chSection          as com-handle no-undo.


create "CrystalRuntime.Application" chApplication.
create "CrystalRuntime.Report"      chReport.
create "CrystalRuntime.Report"      chSection.

file-info:filename = pv-report.
pv-report = file-info:full-pathname.
file-info:filename = pv-data.
pv-data = file-info:full-pathname.


run printreport ('pdf',pv-dest). 
/* really needs to be passd in param for type maybe from dpgm table? */

release object chreport.
release object chapplication.
   wait(500).
if not pv-savedata then do:
   OS-DELETE VALUE(entry(1,pv-data,'.') + '.xsd').
   OS-DELETE VALUE(pv-data).
end.
/********************************************/

procedure printreport:
def input param pv-type as char no-undo.
def input param pv-dest as char no-undo.
def var lv-dest as int no-undo init 0.
lv-dest = LOOKUP(pv-dest,'file,mailmapi,mailvim,exchange,application,lotus') no-error.

def var chexport as com-handle no-undo.
def var lv-ext as char no-undo.

   def var i     as int   no-undo.
   def var j     as int    no-undo.
   def var sItem as char no-undo.
   def var sLoc  as char no-undo.
   
   chReport = chApplication:OpenReport(pv-report,1).
   /*
   works - but only if data is in c:\rex
   chReport:Database:Tables:Item(1):ConnectionProperties:Item("Local XML File") = pv-data.  
   */
   chReport:discardSavedData.
   
   /* this solution requires only the .rpt file in a relatively pathed
      directory */
   /* set tables */
   do i = 1 to chReport:Database:Tables:Count:
      sLoc = chReport:Database:Tables:Item(i):Location.
      sItem = 'Local XML File='      + entry(1,pv-data,".") + ".xml" +
              ';Local Schema File=' + entry(1,pv-data,".") + ".xsd".
      chReport:Database:Tables:Item(i):SetTableLocation(sLoc,"",sItem).
   end. /* loop through tables */ 
   
   /* set sub reports 
   do i = 1 to chReport:Sections:Count:
      do j = 1 to chReport:Sections(i):ReportObjects:Count:
         /* need logic ... */
      end.
   end.
   */

chexport = chReport:exportoptions.

case pv-type:
   when 'pdf' then assign lv-ext = '.pdf'
                          chexport:DiskFileName = entry(1,pv-data,".") + lv-ext
                          chexport:ApplicationFileName = chexport:DiskFileName
                          chexport:FormatType = 31
                          chexport:PDFExportAllPages = 1.
   otherwise message pv-dest ' not catered for yet'.
end case.

if pv-dest = 'file' then
if valid-handle(pv-from)
   then run dispcrystalop in  pv-from (chexport:DiskFileName) no-error.
else message 'invalid parent prog'.

chexport:DestinationType = lv-dest.
chreport:export(false).

release object chexport.

sysmsg('off').
end procedure.

/*
Crystal Reports Developer's Help
ExportOptions Object
The ExportOptions object provides properties and methods for retrieving information and setting options for exporting your report (that is, export format, destination, etc.). An ExportOptions Object is obtained from the ExportOptions property of the Report Object.

Property 
Description 
Read/Write 
Restriction in event handler 

ApplicationFileName
 String. Gets or sets the destination application file name.
 Read/Write
 Set before exporting report to an application destination.
 
CharFieldDelimiter
 String. Gets or sets the character used to separate fields in character separated text formats. This character delimits every field in the file.
 Read/Write
 None
 
CharStringDelimiter
 String. Gets or sets the character used to enclose string values in character separated text formats. 

In general, this character delimits only string fields (number, date fields, etc., have no delimiter). However, if a field that is generally not delimited contains either 1) a field separator character, 2) a field delimiter character, or 3) a newline, then that field must be delimited.
 Read/Write
 None
 
DestinationDLLName
 String. Gets the Internal Name property of the DLL used to export the report to a certain destination. The destination is set in the DestinationType property.
 Read only
 None
 
DestinationType
 CRExportDestinationType. Gets or sets the export destination type.
 Read/Write
 None
 /* 
crEDTApplication  5
crEDTDiskFile     1
crEDTEMailMAPI         2
crEDTEMailVIM          3
crEDTLotusDomino       6
crEDTMicrosoftExchange 4
crEDTNoDestination    0
*/
DiskFileName
 String. Gets or sets the file name if the report is exported to a disk.

When exporting to HTML use HTMLFileName. When exporting to XML use XMLFileName.
 Read/Write
 None
 
ExcelAreaGroupNumber
 Integer. Gets or sets the base area group number if the area type is group area when exporting to Excel.
 Read/Write
 None
 
ExcelAreaType
 CRAreaKind. Gets or sets the base area type if not using constant column width when exporting to Excel.
 Read/Write
 None
 
ExcelChopPageHeader
 Boolean. Specifies whether page headers are to be simplified when exporting to Excel format.
 Read/Write
 None
 
ExcelConstantColumnWidth
 Double. Gets or sets the column width when exporting to Excel.
 Read/Write
 None
 
ExcelConvertDateToString
 Boolean. Gets or sets export to Excel has converting date values to strings option.
 Read/Write
 None
 
ExcelExportAllPages
 Boolean. Gets or sets export to Excel with all pages.
 Read/Write
 None
 
ExcelExportImagesInDataOnly
 Boolean. Specifies whether the images are exported.
 Read/Write
 None
 
ExcelExportPageAreaPair
 CRExcelExportPageAreaPair. Specifies how the page header and footer pair are to be exported.
 Read/Write
 None
 
ExcelFirstPageNumber
 Long. Gets or sets export to Excel for first page number.
 Read/Write
 None
 
ExcelLastPageNumber
 Long. Gets or sets export to Excel for last page number.
 Read/Write
 None
 
ExcelMaintainColumnAlignment
 Boolean. Specifies whether column alignment is maintained when exporting to Excel format.
 Read/Write
 None
 
ExcelMaintainRelativeObjectPosition
 Boolean. Specifies whether relative object position is maintained upon export.
 Read/Write
 None
 
ExcelPageBreaks
 Boolean. Gets or sets export to Excel has page break option.
 Read/Write
 None
 
ExcelShowGridLines
 Boolean. Specifies whether gridlines will show in the exported report.
 Read/Write
 None
 
ExcelTabHasColumnHeadings
 Boolean. Gets or sets exporting to Excel has column headings option.
 Read/Write
 None
 
ExcelUseConstantColumnWidth
 Boolean. Gets or sets export to Excel to use constant column width.
 Read/Write
 None
 
ExcelUseFormatInDataOnly
 Boolean. Specifies whether object formats like font attributes and text color are exported in Excel Data Only format.
 Read/Write
 None
 
ExcelUseTabularFormat
 Boolean. Gets or sets exporting to Excel to use tabular format.
 Read/Write
 None
 
ExcelUseWorksheetFunctions
 Boolean. Gets or sets export to Excel to use worksheet functions to represent subtotal fields in the report.
 Read/Write
 None
 
ExchangeDestinationType
 CRExchangeDestinationType. Gets or sets the Exchange destination type for reports exported to Exchange folders.
 Read/Write
 None
 
ExchangeFolderPath
 String. Gets or sets the path of the Exchange folder for reports exported to Exchange (for example, "MyFolders@Inbox").
 Read/Write
 None
 
ExchangePassword
 String. Sets Exchange password.
 Write only
 None
 
ExchangeProfile
 String. Gets or sets a user profile for accessing an Exchange folder for reports exported to Exchange.
 Read/Write
 None
 
ExchangePathHasColumnHeadings
 Boolean. Gets or sets the column heading option when exporting to Exchange.
 Read/Write
 None
 
FormatDLLName
 String. Gets the Internal Name property of the DLL used to export the report to a certain format type. The export format type is set in the FormatType property.
 Read only
 None
 
FormatType
 CRExportFormatType. Gets or sets the format type for the exported report (for example, text, Excel, etc.).
 Read/Write
 None
 /***************************
CRExportFormatType Constant Value 
crEFTCharSeparatedValues   7
crEFTCommaSeparatedValues  5
crEFTCrystalReport         1
crEFTCrystalReport70       33
crEFTDataInterchange       2
crEFTExactRichText         35 (&H23)
crEFTExcel50               21 (&H15)
crEFTExcel50Tabular        22 (&H16)
crEFTExcel70               27 (&H1B)
crEFTExcel70Tabular        28 (&H1C)
crEFTExcel80               29 (&H1D)
crEFTExcel80Tabular        30 (&H1E)
crEFTExcel97               36 (&H24)
crEFTExplorer32Extend      25 (&H19)
crEFTHTML32Standard        24 (&H18)
crEFTHTML40                32 (&H20)
crEFTLotus123WK1           12 
crEFTLotus123WK3           13
crEFTLotus123WKS           11
crEFTNoFormat              0
crEFTODBC                  23 (&H17)
crEFTPaginatedText         10
crEFTPortableDocFormat     31 (&H1F)
crEFTRecordStyle           3
crEFTReportDefinition      34 (&H22)
crEFTTabSeparatedText      9
crEFTTabSeparatedValues    6
crEFTText                  8
crEFTWordForWindows        14
crEFTXML                   37 (&H25)
**************************/

HTMLEnableSeperatedPages
 Boolean. Gets or sets the option to create seperated pages when exporting to HTML format.
 Read/Write
 None
 
HTMLFileName
 String. Gets or sets the HTML file name for reports exported to HTML format.
 Read/Write
 None
 
HTMLHasPageNavigator
 Boolean. Gets or sets the option to display a page navigator on each page of a report exported to HTML format.
 Read/Write
 None
 
LotusDominoComments
 String. Gets or sets the destination Lotus Domino comments.
 Read/Write
 None
 
LotusDominoDatabaseName
 String. Gets or sets the destination Lotus Domino database name.
 Read/Write
 None
 
LotusNotesFormName
 String. Gets or sets the destination Lotus Domino form name.
 Read/Write
 None
 
MailBccList
 String. Gets or sets a Blind Carbon Copy (BCC) list for reports e-mailed to a VIM e-mail account.
 Read/Write
 None
 
MailCcList
 String. Gets or sets a Carbon Copy (CC) list for reports e-mailed.
 Read/Write
 None
 
MailMessage
 String. Gets or sets the e-mail message included with e-mailed reports.
 Read/Write
 None
 
MailSubject
 String. Gets or sets the e-mail subject heading for reports being e-mailed.
 Read/Write
 None
 
MailToList
 String. Gets or sets the To list for reports being e-mailed.
 Read/Write
 None
 
MailUserName
 String. Gets or sets mail user name.
 Read/Write
 None
 
NumberOfLinesPerPage
 Integer. Gets or sets the number of lines to appear per page option for reports exported to a paginated text format.
 Read/Write
 None
 
ODBCDataSourceName
 String. Gets or sets the ODBC data source for reports exported to ODBC.
 Read/Write
 None
 
ODBCDataSourcePassword
 String. Sets the ODBC data source password.
 Write only
 None
 
ODBCDataSourceUserID
 String. Gets or sets the user name used to access an ODBC data source for reports exported to ODBC.
 Read/Write
 None
 
ODBCExportTableName
 String. Gets or sets the database table in the ODBC data source that the report file exported to ODBC will be appended to. You can also create a new table using this property.
 Read/Write
 None
 
Parent
 Report Object. Reference to the parent object.
 Read only
 None
 
PDFExportAllPages
 Boolean. Gets or sets whether or not to export all pages of the report to Portable Document Format(PDF). PDFExportAllPages must be set to false when setting PDFFirstPageNumber and PDFLastPageNumber.
 Read/Write
 None
 
PDFFirstPageNumber
 Long. Gets or sets the start page, of a page export range, when exporting to PDF. PDFExportAllPages must be set to False or this value is ignored.
 Read/Write
 None
 
PDFLastPageNumber
 Long. Gets or sets the end page, of a page export range, when exporting to PDF. PDFExportAllPages must be set to False or this value is ignored.
 Read/Write
 None
 
RTFExportAllPages
 Boolean. Gets or sets whether or not to export all pages of the report to Rich Text Format(RTF). RTFExportAllPages must be set to false when setting RTFFirstPageNumber and RTFLastPageNumber.
 Read/Write
 None
 
RTFFirstPageNumber
 Long. Gets or sets the start page, of a page export range, when exporting to RTF. RTFExportAllPages must be set to False or this value is ignored.
 Read/Write
 None
 
RTFFLastPageNumber
 Long. Gets or sets the end page, of a page export range, when exporting to RTF. RTFExportAllPages must be set to False or this value is ignored.
 Read/Write
 None
 
UseDefaultCharactersPerInch
 Boolean. Gets or sets use default characters per inch option.
 Read/Write
 None
 
UserDefinedCharactersPerInch
 Long. Gets or sets user defined characters per inch.
 Read/Write
 None
 
UseReportDateFormat
 Boolean. Gets or sets whether the date format used in the report should also be used in the exported report. Can be used for Data Interchange Format (DIF), Record Style Format, and comma, tab, or character separated format.
 Read/Write
 None
 
UseReportNumberFormat
 Boolean. Gets or sets whether the number format used in the report should also be used in the exported report. Can be used for Data Interchange Format (DIF), Record Style Format, and comma, tab, or character separated format.
 Read/Write
 None
 
XMLAllowMultipleFiles
 Boolean. Gets or sets allow multiple files, when exporting to XML. When set to True the Schema file for the report will be exported along with the XML file. The Schema file will be either an XML schema (.xsd) or a Document Type Definition (.dtd), depending on the options selected in the XML Forma dilaog box. For more information see "Customizing XML report definitions" in the Crystal Reports User's Guide
 Read/Write
 None
 
XMLFileName
 String. Gets or sets the file name if the report is exported to a disk.
 Read/Write
 None

Remarks
For backwards compatibility the FormatDllName and DestinationDllName properties return the Internal Name property of the associated DLL. The Internal Name property of the DLL is found in the DLLs Properties Dialog box under the Version tab. For a list of export DLLs see "Export Destination" and "Export Format" in the Runtime help (Runtime.hlp). 
*/
