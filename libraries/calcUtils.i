&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/***************************************************************************
    File      : {&core}libraries/calcUtils.i
    Purpose   : Procedures for use with Open Office Calc (spreadsheet)
                 programming
    Author(s) : Patrick Hulst
    Created   : 2004-03-18

    Usage (REALLY basic idea):
        Start Calc:             run calc-open.
        Open an existing book:  run open_book(filename).
        Create a new book:      run book-new.
        Column Headers:         run write_col_hdr(column,"Label").
        Column Data:            run cell-writeData(column,row,"Data",data type).
        Save the Sheet:         run save_book ("Directory","Sheet Name").
        Close Calc:             run close_calc.
        Clean up com-handles:   run CleanUp.
***************************************************************************/

/************************** def vars ******************************/
def var chOpenOffice    as com-handle no-undo.
def var chWorkBook      as com-handle no-undo.
def var chDesktop       as com-handle no-undo.
def var chWorkSheet     as com-handle no-undo.
def var chCell          as com-handle no-undo.

def var cc              as raw        no-undo.

/* a just in case measure */
assign
   chOpenOffice = ?
   chWorkBook   = ?
   chDesktop    = ?
   chWorkSheet  = ?.

FUNCTION get-columnLetter returns character 
        (input columnNumber as int) forward.

FUNCTION get-columnNumber returns integer 
        (input ip_ColLetter as char) forward.

FUNCTION release-comHandle returns logical 
        (input chObject as com-handle) forward.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-columnletter Include 
FUNCTION get-columnletter RETURNS CHARACTER
   (input columnNumber as int) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Get-columnnumber Include 
FUNCTION Get-columnnumber RETURNS INTEGER
   (input ip_ColLetter as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD release-comhandle Include 
FUNCTION release-comhandle RETURNS LOGICAL
   (input chObject as com-handle) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 38.52
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Align_cell Include 
PROCEDURE Align_cell :
/*-----------------------------------------------------------------------------
  Purpose: Align the cell specified.
  Notes:   
-----------------------------------------------------------------------------*/
   def input  param columnNumber         as int    no-undo.
   def input  param rowNumber         as int    no-undo.
   /* values: block, center, left, repeat, right, standard */
   def input  param ip_Horizontal  as char  no-undo.
   /* values: bottom, center, standard, top */
   def input  param ip_Vertical    as char  no-undo.
   /* values: bottomtop, standard, stacked, topbottom */
   def input  param ip_Orientation as char  no-undo.


   /* Get the current cell */
   chCell  = chWorkSheet:GetCellByPosition(columnNumber,rowNumber).

   /* Set the various attributes (if applicable) */
   if ip_Horizontal  > "" then chCell:HoriJustify = ip_Horizontal.
   if ip_Vertical    > "" then chCell:VertJustify = ip_Vertical.
   if ip_Orientation > "" then chCell:Orientation = ip_Orientation.

   /* Clean up after ourselves */
   release-comHandle(chCell).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Align_Cell_Range Include 
PROCEDURE Align_Cell_Range :
/* Purpose:     Align the row specified.                           */
/*******************************************************************/
    def input  param ip_Range        AS CHAR         no-undo.        /* eg B2:D4 */
    def input  param ip_Horizontal   AS CHAR         no-undo.        /* Values: BLOCK, CENTER, LEFT, REPEAT, RIGHT, STANDARD */
    def input  param ip_Vertical     AS CHAR         no-undo.        /* Values: BOTTOM, CENTER, STANDARD, TOP */
    def input  param ip_Orientation  AS CHAR         no-undo.        /* Values: BOTTOMTOP, STANDARD, STACKED, TOPBOTTOM */

    /* Get the cell range */
    ASSIGN chCell  = chWorkSheet:GetCellRangeByName(ip_Range).

    /* Set the various attributes (if applicable) */
    IF ip_Horizontal  > "" THEN chCell:HoriJustify = ip_Horizontal.
    IF ip_Vertical    > "" THEN chCell:VertJustify = ip_Vertical.
    IF ip_Orientation > "" THEN chCell:Orientation = ip_Orientation.

    /* Clean up after ourselves */
    IF chCell <> ? THEN DO:
        RELEASE OBJECT chCell.
        ASSIGN chCell  = ?.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Autofit_Column Include 
PROCEDURE Autofit_Column :
/*-----------------------------------------------------------------------------
  Purpose: Use the Calc OptimalWidth attribute to set widths for columns.
  Notes:   
-----------------------------------------------------------------------------*/
    def input  param columnNumber  as int    no-undo.


    chWorkSheet:columns(columnNumber):OptimalWidth = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Autofit_Row Include 
PROCEDURE Autofit_Row :
/* Purpose:     Use the Calc OptimalHeight attribute to set height */
/*              for row                                            */
/* Parameters:  Pass in as Row#                                    */
/*******************************************************************/
    def input  param rowNumber  AS INT  no-undo.       /* row number */


    chWorkSheet:Rows(rowNumber):OptimalHeight = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE book-new Include 
PROCEDURE book-new :
/* Purpose:     Open a new "BOOK" in Calc and create a worksheet.  */
/*******************************************************************/

    chWorkBook  = chDesktop:loadComponentFromURL("private:factory/scalc", "_blank", 0, cc).
    chWorkSheet = chWorkBook:Sheets:getByIndex(0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-open Include 
PROCEDURE calc-open :
/* Purpose:     Start OpenOffice & open a DDE conversation with    */
/*              the Calc System topic                              */
/*******************************************************************/

    /* Try to connect to existing instance of OpenOffice */
    CREATE "com.sun.star.ServiceManager" chOpenOffice CONNECT NO-ERROR.

    /* If some error happened then most likely there was no */
    /* instance of OO running so start a new one. */
    IF ERROR-STATUS:GET-MESSAGE(1) <> "" THEN
        CREATE "com.sun.star.ServiceManager" chOpenOffice.

    /* Start up the OO desktop now.  Everything fires from there. */
    chDesktop = chOpenOffice:createInstance("com.sun.star.frame.Desktop").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cell-format Include 
PROCEDURE cell-format :
/*-----------------------------------------------------------------------------
  Purpose: Formats a cell with a specific format.
  Notes:   
-----------------------------------------------------------------------------*/
   def input  param columnNumber  as int    no-undo.
   def input  param rowNumber     as int    no-undo.
   def input  param formatToUse   as char  no-undo.

   def var formatNumber as int    no-undo.


   /* Get the current cell */
   chCell = chWorkSheet:GetCellByPosition(columnNumber,rowNumber).

   /* formats are stored as numbers in the work book, so find the number of
      the format we want to use if it exists */
   formatNumber = chWorkbook:getNumberFormats:queryKey(
      formatToUse,
      chWorkbook:charLocale,
      false).

   /* if our desired format is not in the work book, then formatNumber will be
      set to "-1".  In that case, we will create our desired format. */
   if formatNumber = -1 then
      formatNumber = chWorkbook:getNumberFormats:addNew(
         formatToUse,
         chWorkbook:charLocale).

   /* set the cell's format to the correct format number */
   chCell:NumberFormat = formatNumber.

   /* Clean up after ourselves */
   release-comHandle(chCell).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cell-writedata Include 
PROCEDURE cell-writedata :
/*-----------------------------------------------------------------------------
  Purpose: Write data into a worksheet.
  Notes:   
-----------------------------------------------------------------------------*/
   def input  param columnNumber as int    no-undo.
   def input  param rowNumber    as int    no-undo.
   def input  param dataToWrite  as char  no-undo.
   def input  param dataType     as char  no-undo.


   chCell = chWorkSheet:GetCellByPosition(columnNumber,rowNumber).

   /* depending on the cell's data-type, use a different method to write
      out the value.  setValue is used with numbers, and allows a user to
      easily write formulas for numeric columns. */
   case dataType:
      when "integer" then chCell:setValue(integer(dataToWrite)).
      when "decimal" then do:
         chCell:setValue(decimal(dataToWrite)).

         /* set the cell's format */
         run cell-format(
            input  columnNumber,
            input  rowNumber,
            input  "#,###,###,##0.00"). /* format to use */
      end. /* decimal */
      otherwise chCell:setString(dataToWrite).
   end case. /* data type */

   release-comHandle(chCell).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cleanup Include 
PROCEDURE cleanup :
/* Purpose:     cleans up the com-handles                                   */
/****************************************************************************/

    /* Release all the com handles */
    IF chOpenOffice     <> ? THEN RELEASE OBJECT chOpenOffice.
    IF chWorkBook       <> ? THEN RELEASE OBJECT chWorkBook.
    IF chDesktop        <> ? THEN RELEASE OBJECT chDesktop.
    IF chWorkSheet      <> ? THEN RELEASE OBJECT chWorkSheet.

    ASSIGN
        chOpenOffice    = ?
        chWorkBook      = ?
        chDesktop       = ?
        chWorkSheet     = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close_calc Include 
PROCEDURE close_calc :
/* Purpose:     Close the Calc program.                            */
/*******************************************************************/

    /* Close the workbook */
    IF chWorkbook <> ? THEN
        chWorkbook:Close(TRUE).
    
    /* Close the program */
    IF chDesktop <> ? THEN
        chDesktop:TERMINATE().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE col_function Include 
PROCEDURE col_function :
/* Purpose:     Provides a utility to create a simple "column"     */
/*              function                                           */
/*              Valid functions are:                               */
/*              SUM: sums all numerical values                     */
/*              COUNT: total # of all values (including chars)     */
/*              COUNTNUMS: total number of all numerical cells     */
/*              AVERAGE: average of all numerical cells            */
/*              MAX: largest numerical value                       */
/*              MIN: smallest numerical value                      */
/*              PRODUCT: product of all numerical values           */
/*              STDEV: standard deviation                          */
/*              VAR: variance                                      */
/*              STDEVP: standard devt'n based on total population  */
/*              VARP: varianced based on total population          */
/*******************************************************************/
    def input  param columnNumber          AS INT  no-undo.        /* Column Number */
    def input  param rowNumber          AS INT  no-undo.        /* Row Number */
    def input  param ip_StartRow     AS INT  no-undo.
    def input  param ip_EndRow       AS INT  no-undo.
    def input  param ip_Function     AS CHAR no-undo.
    def var         chrFormula      AS CHAR no-undo.

    /* Get the current cell */
    ASSIGN chCell  = chWorkSheet:GetCellByPosition(columnNumber,rowNumber).

    /* And set the value */
    ASSIGN
        chrFormula = "=" +
                        ip_Function +
                        "(" +
                        get-columnLetter(columnNumber) + STRING(ip_StartRow) +
                        ":" +
                        get-columnLetter(columnNumber) + STRING(ip_EndRow) +
                        ")".

    chCell:SetFormula(chrFormula).

    /* Clean up after ourselves */
    IF chCell <> ? THEN DO:
        RELEASE OBJECT chCell.
        ASSIGN chCell  = ?.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete_col Include 
PROCEDURE delete_col :
/* Purpose:     Deletes columns below the passed in col #          */
/*******************************************************************/
    def input  param columnNumber      AS INT  no-undo.
    def input  param ip_NumCols  AS INT  no-undo.

    IF ip_NumCols < 1 THEN RETURN.

    /* delete columns */
    chWorkSheet:Columns:DeleteByIndex(columnNumber, ip_NumCols).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete_row Include 
PROCEDURE Delete_row :
/* Purpose:     Deletes rows below the passed in row #             */
/*******************************************************************/
    def input  param rowNumber      AS INT  no-undo.
    def input  param ip_NumRows  AS INT  no-undo.

    /* Make sure we're deleting at least one */
    IF ip_NumRows < 1 THEN RETURN.

    /* delete rows */
    chWorkSheet:Rows:DeleteByIndex(rowNumber, ip_NumRows).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE freeze_panes Include 
PROCEDURE freeze_panes :
/* Purpose:     freezes panes with the specified number of columns */
/*              and rows. To freeze only horizontally, specify     */
/*              rowNumber as 0. To freeze only vertically, specify    */
/*              columnNumber as 0.                                       */
/*******************************************************************/
    def input  param columnNumber          AS INT  no-undo.        /* Column Number */
    def input  param rowNumber          AS INT  no-undo.        /* Row Number */

    chWorkBook:getCurrentController():freezeAtPosition(columnNumber, rowNumber).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_cell_data Include 
PROCEDURE Get_cell_data :
/* Purpose:     Gets the cell data (char) from a worksheet         */
/*******************************************************************/
    def input  param columnNumber          AS INT  no-undo.        /* Column Number */
    def input  param rowNumber          AS INT  no-undo.        /* Row Number */
    DEF OUTPUT PARAM op_CellData    AS CHAR no-undo.

    /* Get the current cell */
    ASSIGN chCell  = chWorkSheet:GetCellByPosition(columnNumber,rowNumber).

    /* And get the value */
    ASSIGN op_CellData = STRING(chCell:FORMULA).

    /* Clean up after ourselves */
    IF chCell <> ? THEN DO:
        RELEASE OBJECT chCell.
        ASSIGN chCell  = ?.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get_last_row Include 
PROCEDURE get_last_row :
/* Purpose:     Returns the last row as an integer.                */
/*******************************************************************/
    def input  param columnNumber      AS INT  no-undo.
    DEF OUTPUT PARAM op_LastRow AS INT  no-undo.

    def var         chrRange    AS CHAR no-undo.

    /* Set up the range */
    ASSIGN chrRange = get-columnLetter(columnNumber) + "10:" +
       get-columnLetter(columnNumber) + "14".

    /* Get the current row */
    ASSIGN chCell  = chWorkSheet:GetCellRangeByName(chrRange).
    op_LastRow = chCell:getRows:COUNT.

    /* Clean up after ourselves */
    IF chCell <> ? THEN DO:
        RELEASE OBJECT chCell.
        ASSIGN chCell  = ?.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Insert_col Include 
PROCEDURE Insert_col :
/* Purpose:     Inserts columns below the passed in col #          */
/*******************************************************************/
    def input  param columnNumber      AS INT  no-undo.
    def input  param ip_NumCols  AS INT  no-undo.

    /* Make sure we're deleting at least one */
    IF ip_NumCols < 1 THEN RETURN.

    /* Insert columns */
    chWorkSheet:Columns:InsertByIndex(columnNumber, ip_NumCols).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE insert_row Include 
PROCEDURE insert_row :
/* Purpose:     Inserts rows below the passed in row #             */
/*******************************************************************/
    def input  param rowNumber      AS INT  no-undo.
    def input  param ip_NumRows  AS INT  no-undo.

    IF ip_NumRows < 1 THEN RETURN.

    chWorksheet:rows:insertByIndex(rowNumber, ip_NumRows).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE maximize_window Include 
PROCEDURE maximize_window :
/* Purpose:     Maximizes the OO screen.                                    */
/****************************************************************************/
    def var chFrame     AS COM-HANDLE   no-undo.
    def var chWindow    AS COM-HANDLE   no-undo.
    def var chRect      AS COM-HANDLE   no-undo.

    chFrame  = chDesktop:getCurrentFrame().
    chWindow = chFrame:getContainerWindow().
    chRect   = chWindow:setPosSize(1,1,800,600,15).

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE minimize_window Include 
PROCEDURE minimize_window :
/* Purpose:     Minimizes the OO screen.                                    */
/****************************************************************************/
    def var chFrame     AS COM-HANDLE   no-undo.
    def var chWindow    AS COM-HANDLE   no-undo.
    def var chRect      AS COM-HANDLE   no-undo.

    chFrame  = chDesktop:getCurrentFrame().
    chWindow = chFrame:getContainerWindow().
    chRect   = chWindow:setPosSize(1,1,1,1,15).

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open_book Include 
PROCEDURE open_book :
/* Purpose:     open a "BOOK" in Calc.                             */
/*******************************************************************/
    def input  param ip_FileName    AS CHAR   no-undo. /* spreadsheet name */

    IF chWorkbook = ? THEN LEAVE.

    ASSIGN
        ip_FileName = "file:///" + TRIM(ip_FileName)
        ip_FileName = REPLACE(ip_FileName, "{&BackSlash}", "/").

    chWorkbook  = chDesktop:loadComponentFromURL(ip_FileName, "_blank", 0, cc).
    chWorksheet = chWorkbook:Worksheets:Item(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE page_break Include 
PROCEDURE page_break :
/* Purpose:     Inserts a manual page break for reporting          */
/*******************************************************************/
    def input  param rowNumber      AS INT  no-undo.

    def var         chrRange    AS CHAR no-undo.

    /* Set up the range */
    ASSIGN chrRange = "A" + TRIM(STRING(rowNumber)) + ":A" +
       TRIM(STRING(rowNumber)).

    /* Get the current row */
    ASSIGN chCell  = chWorkSheet:GetCellRangeByName(chrRange).
    chCell:getRows:IsStartOfNewPage = TRUE.

    /* Clean up after ourselves */
    IF chCell <> ? THEN DO:
        RELEASE OBJECT chCell.
        ASSIGN chCell  = ?.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE row_function Include 
PROCEDURE row_function :
/* Purpose:     Provides a utility to create a simple "row"        */
/*              function                                           */
/*              SUM: sums all numerical values                     */
/*              COUNT: total # of all values (including chars)     */
/*              COUNTNUMS: total number of all numerical cells     */
/*              AVERAGE: average of all numerical cells            */
/*              MAX: largest numerical value                       */
/*              MIN: smallest numerical value                      */
/*              PRODUCT: product of all numerical values           */
/*              STDEV: standard deviation                          */
/*              VAR: variance                                      */
/*              STDEVP: standard devt'n based on total population  */
/*              VARP: varianced based on total population          */
/*******************************************************************/
    def input  param columnNumber          AS INT  no-undo.        /* Column Number */
    def input  param rowNumber          AS INT  no-undo.        /* Row Number */
    def input  param ip_StartCol     AS INT  no-undo.
    def input  param ip_EndCol       AS INT  no-undo.
    def input  param ip_Function     AS CHAR no-undo.
    def var         chrFormula      AS CHAR no-undo.

    /* Get the current cell */
    ASSIGN chCell  = chWorkSheet:GetCellByPosition(columnNumber,rowNumber).

    /* And set the value */
    ASSIGN
        chrFormula = "=" +
                        ip_Function +
                        "(" +
                        get-columnLetter(ip_StartCol) + STRING(rowNumber) +
                        ":" +
                        get-columnLetter(ip_EndCol) + STRING(rowNumber) +
                        ")".

    chCell:SetFormula(chrFormula).

    /* Clean up after ourselves */
    IF chCell <> ? THEN DO:
        RELEASE OBJECT chCell.
        ASSIGN chCell  = ?.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE save_book Include 
PROCEDURE save_book :
/* Purpose:     Save a "BOOK" in Calc                              */
/*******************************************************************/
    def input  param ip_OutputPath   AS CHAR no-undo.    /* directory path */
    def input  param ip_FileName     AS CHAR no-undo.    /* spreadsheet name */

    def var chrFileName             AS CHAR no-undo.

    ASSIGN
        chrFileName = "file:///" + ip_OutputPath + ip_FileName + ".sxc"
        chrFileName = chrFileName + "{&BackSlash}"
        chrFileName = REPLACE(
           chrFileName,
           "{&BackSlash}{&BackSlash}",
           "{&BackSlash}")
        chrFileName = REPLACE(chrFileName, "/{&BackSlash}", "{&BackSlash}")
        chrFileName = REPLACE(chrFileName, "{&BackSlash}", "/").

    chWorkBook:storeAsURL(chrFileName, cc).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set_col_width Include 
PROCEDURE set_col_width :
/* Purpose:     Set the width of a column.                         */
/*******************************************************************/
    def input  param columnNumber          AS INT  no-undo.
    def input  param ip_ColWidth     AS DEC  no-undo.

    IF columnNumber > 0 THEN chWorksheet:Columns(columnNumber):Width = ip_ColWidth.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set_font_style Include 
PROCEDURE set_font_style :
/* Purpose:     Set font style for a cell                          */
/* Parameters:  Row #                                              */
/*              Column #                                           */
/*              Font Name                                          */
/*              Size (points)                                      */
/*              Bold (TRUE/FALSE)                                  */
/*              Underline (See below)                              */
/*******************************************************************/
    def input  param columnNumber          AS INT  no-undo.        /* Column Number */
    def input  param rowNumber          AS INT  no-undo.        /* Row Number */
    def input  param ip_Font         AS CHAR no-undo.        /* Font Name */
    def input  param ip_Size         AS INT  no-undo.        /* Point Size */
    def input  param ip_Bold         AS LOG  no-undo.        /* Bold (weight = 150 for bold, 100 for normal) */
    def input  param ip_Underline    AS INT  no-undo.        /* NONE = 0, SINGLE =1, DOUBLE=2, DOTTED = 3
                                                               DONTKNOW=4, DASH=5, LONGDASH=6, DASHDOT=7,
                                                               DASHDOTDOT=8, SMALLWAVE=9, WAVE =10, DOUBLEWAVE=11,
                                                               BOLD=12, BOLDDOTTED=13, BOLDLONGDASH= 14,
                                                               BOLDDASHDOT=15, BOLDDASHDOTDOT=16, BOLDWAVE = 17  */

    /* Get the current cell */
    ASSIGN chCell  = chWorkSheet:GetCellByPosition(columnNumber,rowNumber).

    /* Set the various attributes (if applicable) */
    chCell:CharFontName  = IF ip_Font > ""      THEN ip_Font        ELSE chCell:CharFontName.
    chCell:CharHeight    = IF ip_Size > 0       THEN ip_Size        ELSE chCell:CharHeight.
    chCell:CharWeight    = IF ip_Bold           THEN 150            ELSE 100.
    chCell:CharUnderline = IF ip_Underline > 0  THEN ip_UnderLine   ELSE 0.

    /* Clean up after ourselves */
    IF chCell <> ? THEN DO:
        RELEASE OBJECT chCell.
        ASSIGN chCell  = ?.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set_footer Include 
PROCEDURE set_footer :
/* Purpose:     Creates a footer for the document                  */
/*******************************************************************/
    def input  param ip_Text     AS CHAR         no-undo.

    def var chStyleFamilies     AS COM-HANDLE   no-undo.
    def var chPageStyles        AS COM-HANDLE   no-undo.
    def var chDefaultPage       AS COM-HANDLE   no-undo.
    def var chFooterText        AS COM-HANDLE   no-undo.
    def var chFooterContent     AS COM-HANDLE   no-undo.

    /* first we need to get the default page style for this workbook/sheet */
    chStyleFamilies = chWorkBook:StyleFamilies.
    chPageStyles = chStyleFamilies:getByName("PageStyles").
    chDefaultPage = chPageStyles:getByName("Default").

    /* Turn on Footers */
    chDefaultPage:FooterIsOn = TRUE.

    /* Same Footer for both left (even) & right (odd) pages */
    chDefaultPage:FooterIsShared = TRUE.

    /* Set up the Footer */
    chFooterContent = chDefaultPage:RightPageFooterContent.
    chFooterText = chFooterContent:CenterText.
    chFooterText:STRING = ip_Text.
    chDefaultPage:RightPageFooterContent = chFooterContent.

    /* Clean up now */
    IF chStyleFamilies <> ? THEN DO:
        RELEASE OBJECT chStyleFamilies.
        ASSIGN chStyleFamilies = ?.
    END.

    IF chPageStyles <> ? THEN DO:
        RELEASE OBJECT chPageStyles.
        ASSIGN chPageStyles = ?.
    END.

    IF chDefaultPage <> ? THEN DO:
        RELEASE OBJECT chDefaultPage.
        ASSIGN chDefaultPage = ?.
    END.

    IF chFooterText <> ? THEN DO:
        RELEASE OBJECT chFooterText.
        ASSIGN chFooterText = ?.
    END.

    IF chFooterContent <> ? THEN DO:
        RELEASE OBJECT chFooterContent.
        ASSIGN chFooterContent = ?.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set_header Include 
PROCEDURE set_header :
/* Purpose:     Creates a header for the document                  */
/*******************************************************************/
    def input  param ip_Text     AS CHAR         no-undo.

    def var chStyleFamilies     AS COM-HANDLE   no-undo.
    def var chPageStyles        AS COM-HANDLE   no-undo.
    def var chDefaultPage       AS COM-HANDLE   no-undo.
    def var chHeaderText        AS COM-HANDLE   no-undo.
    def var chHeaderContent     AS COM-HANDLE   no-undo.

    /* first we need to get the default page style for this workbook/sheet */
    chStyleFamilies = chWorkBook:StyleFamilies.
    chPageStyles = chStyleFamilies:getByName("PageStyles").
    chDefaultPage = chPageStyles:getByName("Default").

    /* Turn on headers */
    chDefaultPage:HeaderIsOn = TRUE.

    /* Same header for both left (even) & right (odd) pages */
    chDefaultPage:HeaderIsShared = TRUE.

    /* Set up the header */
    chHeaderContent = chDefaultPage:RightPageHeaderContent.
    chHeaderText = chHeaderContent:CenterText.
    chHeaderText:STRING = ip_Text.
    chDefaultPage:RightPageHeaderContent = chHeaderContent.

    /* Clean up now */
    IF chStyleFamilies <> ? THEN DO:
        RELEASE OBJECT chStyleFamilies.
        ASSIGN chStyleFamilies = ?.
    END.

    IF chPageStyles <> ? THEN DO:
        RELEASE OBJECT chPageStyles.
        ASSIGN chPageStyles = ?.
    END.

    IF chDefaultPage <> ? THEN DO:
        RELEASE OBJECT chDefaultPage.
        ASSIGN chDefaultPage = ?.
    END.

    IF chHeaderText <> ? THEN DO:
        RELEASE OBJECT chHeaderText.
        ASSIGN chHeaderText = ?.
    END.

    IF chHeaderContent <> ? THEN DO:
        RELEASE OBJECT chHeaderContent.
        ASSIGN chHeaderContent = ?.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set_row_height Include 
PROCEDURE set_row_height :
/* Purpose:     Set the height of a row.                           */
/* Parameters:  rowNumber    row #                                    */
/*              ip_RowHeight in inches 1/4 = 0.25 passed in        */
/*******************************************************************/
    def input  param rowNumber          AS INT  no-undo.
    def input  param ip_RowHeight    AS DEC  no-undo.

    IF rowNumber > 0 THEN chWorksheet:Rows(rowNumber):HEIGHT = ip_RowHeight.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show_col Include 
PROCEDURE show_col :
/* Purpose:     Shows or hides a column                            */
/*******************************************************************/
    def input  param columnNumber      AS INT  no-undo.
    def input  param ip_Show     AS LOG  no-undo.        /* TRUE = visible, FALSE = hidden */

    def var         chrRange    AS CHAR no-undo.

    /* Set up the range */
    ASSIGN chrRange = get-columnLetter(columnNumber) + "1:" +
       get-columnLetter(columnNumber) + "1".

    /* Get the current column */
    ASSIGN chCell  = chWorkSheet:GetCellRangeByName(chrRange).
    chCell:getColumns:IsVisible = ip_Show.

    /* Clean up after ourselves */
    IF chCell <> ? THEN DO:
        RELEASE OBJECT chCell.
        ASSIGN chCell  = ?.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show_row Include 
PROCEDURE show_row :
/* Purpose:     Shows or hides a row                               */
/*******************************************************************/
    def input  param rowNumber      AS INT  no-undo.
    def input  param ip_Show     AS LOG  no-undo.        /* TRUE = visible, FALSE = hidden */

    def var         chrRange    AS CHAR no-undo.

    /* Set up the range */
    ASSIGN chrRange = "A" + TRIM(STRING(rowNumber)) + ":A" + TRIM(STRING(rowNumber)).

    /* Get the current row */
    ASSIGN chCell  = chWorkSheet:GetCellRangeByName(chrRange).
    chCell:getRows:IsVisible = ip_Show.

    /* Clean up after ourselves */
    IF chCell <> ? THEN DO:
        RELEASE OBJECT chCell.
        ASSIGN chCell  = ?.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE write_col_hdr Include 
PROCEDURE write_col_hdr :
/* Purpose:     Write row 1 column headers in a worksheet.         */
/*******************************************************************/
    def input  param columnNumber      AS INT  no-undo.
    def input  param ip_Label    AS CHAR no-undo.

    RUN cell-writeData(
       columnNumber,
       0,
       replace(ip_Label,"!","~n"),
       "character").

    RUN set_font_style
        (columnNumber,    /* Column columnNumber */
         0,         /* Row 1 */
         "",        /* Default font */
         10,        /* 12 points */
         TRUE,      /* Bold */
         1).        /* Single Underline */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-columnletter Include 
FUNCTION get-columnletter RETURNS CHARACTER
   (input columnNumber as int):
/*-----------------------------------------------------------------------------
  Purpose: Returns the column letter for the column number.
  Notes:   
-----------------------------------------------------------------------------*/
   def var res as char  no-undo.
   def var l1  as int    no-undo.
   def var l2  as int    no-undo.


   /* Columns in OO start with 0; this function expects column 1 to be "A" */
   ASSIGN columnNumber = columnNumber + 1.

   /* Now get the column letter... */
   ASSIGN
      l2  = TRUNC((columnNumber - 1) / 26, 0).
      l1  = columnNumber - (26 * l2).
      res = CHR(64 + l2) + CHR(64 + l1).
      res = TRIM(res, CHR(64)).

   /* And return the value */
   RETURN res.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Get-columnnumber Include 
FUNCTION Get-columnnumber RETURNS INTEGER
   (input ip_ColLetter as char):
/*-----------------------------------------------------------------------------
  Purpose: Returns the column number for the column letter.
  Notes:   
-----------------------------------------------------------------------------*/
   def var i         as int    no-undo.
   def var intCurr   as int    no-undo.
   def var intReturn as int    no-undo.


   /* upper case */
   ip_ColLetter = caps(ip_ColLetter).

   DO i = 1 TO LENGTH(ip_ColLetter) -  1 :
      ASSIGN
         intCurr   = ASC(SUBSTR(ip_ColLetter, i, 1)) - 64.
         intReturn = intReturn + (intCurr * 26).
   END.

   /* add the last letter. */
   ASSIGN 
      intReturn = intReturn + ASC(SUBSTR(ip_ColLetter, LENGTH(ip_ColLetter), 1)) - 64
      intReturn = intReturn - 1.      /* Subtract 1 -> 00 columns start at 0! */

   RETURN intReturn.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION release-comhandle Include 
FUNCTION release-comhandle RETURNS LOGICAL
   (input chObject as com-handle):
/*-----------------------------------------------------------------------------
  Purpose: Release a com-handle and reset the variable.
  Notes:   
-----------------------------------------------------------------------------*/ 
   if valid-handle(chObject) then release object chObject.
   chObject = ?.

   return true.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

