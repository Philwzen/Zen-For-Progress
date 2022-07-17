&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :  Marcel FONDACCI (4GL) optimised by philw
    Created     :
    Notes       :
 
***************************  Definitions  ************************** */
 
Def var Document        as COM-Handle NO-UNDO.
Def var Selection       as COM-Handle NO-UNDO.
Def var Application     as COM-Handle NO-UNDO.

&glob WordAppVersion Word.application.8

&glob wdGoToBookmark           -1
&glob wdCell                   12
&glob wdRow                    10
&glob wdSendToNewDocument      0
&glob wdSendToPrinter          1
&glob wdSendToFax              3
&glob wdSendToEmail            2

&glob wdAlignParagraphCenter      1
&glob wdAlignParagraphJustify     3
&glob wdAlignParagraphLeft        0
&glob wdAlignParagraphRight       2

&glob wdBorderBottom              -3
&glob wdBorderLeft                -2
&glob wdBorderRight               -4
&glob wdBorderTop                 -1

&glob wdLineWidth025pt            2
&glob wdLineWidth050pt            4
&glob wdLineWidth075pt            6
&glob wdLineWidth100pt            8

&glob wdPageBreak              7
&glob wdSectionBreakNextPage   2
&glob wdGotoSection            0

&glob wdCharacter              1
&glob wdLowerCase              0
&glob wdLine                   5
&glob wdScreen                 7
&glob wdMove                   0
&glob wdStory                  6
&glob wdSection                8

&glob wdPrintView              3
&glob wdSeekCurrentPageHeader  9
&glob wdNormalView             1
&glob wdAdjustNone             0

&glob wdDoNotSaveChanges       0
&glob wdSaveChanges            -1

&glob wdTableFormatClassic1    4
&glob wdTableFormatColumns1    11
&glob wdTableFormatGrid1       16
&glob wdTableFormatNone        0
&glob wdTableFormatSimple2     2

&glob wdAllowOnlyFormFields    2

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-addBookMark) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addBookMark Procedure 
FUNCTION addBookMark RETURNS LOGICAL
  (  A as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BackSpace) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BackSpace Procedure 
FUNCTION BackSpace RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Borders) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Borders Procedure 
FUNCTION Borders RETURNS LOGICAL
  (  wBordure as int, wOmbrage as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CentimetersToPoints) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CentimetersToPoints Procedure 
FUNCTION CentimetersToPoints RETURNS DECIMAL
  (  i as dec )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteBookMark) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD deleteBookMark Procedure 
FUNCTION deleteBookMark RETURNS LOGICAL
  (  A as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndFBold) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndFBold Procedure 
FUNCTION EndFBold RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndFItalic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndFItalic Procedure 
FUNCTION EndFItalic RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndFUnderline) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndFUnderline Procedure 
FUNCTION EndFUnderline RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndKey Procedure 
FUNCTION EndKey RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndOfDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndOfDoc Procedure 
FUNCTION EndOfDoc RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExistBookMark) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExistBookMark Procedure 
FUNCTION ExistBookMark RETURNS LOGICAL
  (  a as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExpandSectionSelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExpandSectionSelection Procedure 
FUNCTION ExpandSectionSelection RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FBold) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FBold Procedure 
FUNCTION FBold RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FileClose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FileClose Procedure 
FUNCTION FileClose RETURNS LOGICAL
  (  Withsave as log  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FilePrint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FilePrint Procedure 
FUNCTION FilePrint RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FileQuit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FileQuit Procedure 
FUNCTION FileQuit RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FileSave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FileSave Procedure 
FUNCTION FileSave RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FileSaveAs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FileSaveAs Procedure 
FUNCTION FileSaveAs RETURNS LOGICAL
  (  a as Char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FItalic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FItalic Procedure 
FUNCTION FItalic RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FNormal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FNormal Procedure 
FUNCTION FNormal RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FormatParagraph) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FormatParagraph Procedure 
FUNCTION FormatParagraph RETURNS LOGICAL
  (  X as dec, Y as dec )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FormatTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FormatTable Procedure 
FUNCTION FormatTable RETURNS LOGICAL
  (  I as int,
     tableFormat as int,
     ApplyBorders as log)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FUnderline) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FUnderline Procedure 
FUNCTION FUnderline RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAutoVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetAutoVersion Procedure 
FUNCTION GetAutoVersion RETURNS CHARACTER
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GotoBookMark) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GotoBookMark Procedure 
FUNCTION GotoBookMark RETURNS LOGICAL
  (  A AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-gotoNextSection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD gotoNextSection Procedure 
FUNCTION gotoNextSection RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertCR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertCR Procedure 
FUNCTION InsertCR RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertDateTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertDateTime Procedure 
FUNCTION InsertDateTime RETURNS LOGICAL
  (  A as char, asField as log)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-insertFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD insertFile Procedure 
FUNCTION insertFile RETURNS LOGICAL
  (  A as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertImage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertImage Procedure 
FUNCTION InsertImage RETURNS LOGICAL
  (  A as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertRow Procedure 
FUNCTION InsertRow RETURNS LOGICAL
  (  I as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertSection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertSection Procedure 
FUNCTION InsertSection RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-insertSectionSkip) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD insertSectionSkip Procedure 
FUNCTION insertSectionSkip RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertTable Procedure 
FUNCTION InsertTable RETURNS LOGICAL
  (  X as int, Y as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertText Procedure 
FUNCTION InsertText RETURNS LOGICAL
  (  A as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isAutomation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isAutomation Procedure 
FUNCTION isAutomation RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Justify) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Justify Procedure 
FUNCTION Justify RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeftJustify) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LeftJustify Procedure 
FUNCTION LeftJustify RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LowerCase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LowerCase Procedure 
FUNCTION LowerCase RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MailToDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MailToDoc Procedure 
FUNCTION MailToDoc RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MailToPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MailToPrinter Procedure 
FUNCTION MailToPrinter RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MoveDown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MoveDown Procedure 
FUNCTION MoveDown RETURNS LOGICAL
  (  I as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MoveLeft) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MoveLeft Procedure 
FUNCTION MoveLeft RETURNS LOGICAL
  (  I as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewFile Procedure 
FUNCTION NewFile RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NextColumn Procedure 
FUNCTION NextColumn RETURNS Logical
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextPageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NextPageFooter Procedure 
FUNCTION NextPageFooter RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextPageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NextPageHeader Procedure 
FUNCTION NextPageHeader RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NextRow Procedure 
FUNCTION NextRow RETURNS Logical
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NextWindow Procedure 
FUNCTION NextWindow RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NormalView) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NormalView Procedure 
FUNCTION NormalView RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpenDOC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OpenDOC Procedure 
FUNCTION OpenDOC RETURNS LOGICAL
  (  A AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpenDOCReadOnly) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OpenDOCReadOnly Procedure 
FUNCTION OpenDOCReadOnly RETURNS LOGICAL
  (  A AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpenWord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OpenWord Procedure 
FUNCTION OpenWord RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PageHeader Procedure 
FUNCTION PageHeader RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageView) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PageView Procedure 
FUNCTION PageView RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PCenter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PCenter Procedure 
FUNCTION PCenter RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PreviousWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PreviousWindow Procedure 
FUNCTION PreviousWindow RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProtectDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ProtectDocument Procedure 
FUNCTION ProtectDocument RETURNS LOGICAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RightJustify) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RightJustify Procedure 
FUNCTION RightJustify RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetFont) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetFont Procedure 
FUNCTION SetFont RETURNS LOGICAL
  (  A as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSize Procedure 
FUNCTION SetSize RETURNS LOGICAL
  (  I as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startOfDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startOfDoc Procedure 
FUNCTION startOfDoc RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidthColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WidthColumn Procedure 
FUNCTION WidthColumn RETURNS LOGICAL
  (  i as int, j as int, Decimal-ColumnWidth as dec)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WindowClose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WindowClose Procedure 
FUNCTION WindowClose RETURNS LOGICAL
  (  )  FORWARD.

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
         HEIGHT             = 39.57
         WIDTH              = 77.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-addBookMark) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addBookMark Procedure 
FUNCTION addBookMark RETURNS LOGICAL
  (  A as char) :

   Selection:BookMarks:Add (A).
   
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BackSpace) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BackSpace Procedure 
FUNCTION BackSpace RETURNS LOGICAL
  (  ) :

  Selection:Range:Delete(, -1).

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Borders) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Borders Procedure 
FUNCTION Borders RETURNS LOGICAL
  (  wBordure as int, wOmbrage as int) :
/*

FormatBordureEtTrame Ombre, BordureSup, BordureG, BordureInf, BordureD, BordureHoriz, BordureVert, 
                     CouleurHaut, CouleurGauche, CouleurBas, CouleurDroite, CouleurHoriz, CouleurVert, TrameFine, 
                     DistAuTexte, Trame, PremierPlan, ArrièrePlan, Onglet       
With ActiveDocument.Paragraphs(1).Borders    
            .Shadow = True    
            .DistanceFromBottom = numéro    
            .DistanceFromTop = numéro    
            .DistanceFromLeft = numéro    
            .DistanceFromRight = numéro
            End With
With Selection.Paragraphs.Shading    
            .Texture = WdTextureIndex    
            .BackgroundPatternColorIndex = WdColorIndex    
            .ForegroundPatternColorIndex = WdColorIndex
            End With
With ActiveDocument.Paragraphs(1)    
            .Borders(WdBorderType)
            .LineStyle = WdLineStyle    
            .Borders(WdBorderType)
            .LineWidth = WdLineWidth    
            .Borders(WdBorderType)
            .ColorIndex = WdColorIndex
            End With
With Dialogs(wdDialogFormatBordersAndShading)    
            .DefaultTab = WdWordDialogTab    
            .Show
            End With
            
*/            

    Document:Paragraphs(1):Borders({&wdBorderBottom}):LineWidth = wBordure.
    Document:Paragraphs(1):Borders({&wdBorderTop}):LineWidth    = wBordure.
    Document:Paragraphs(1):Borders({&wdBorderLeft}):LineWidth   = wBordure.
    Document:Paragraphs(1):Borders({&wdBorderRight}):LineWidth  = wBordure.
    
    Document:Paragraphs(1):Shadow = true.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CentimetersToPoints) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CentimetersToPoints Procedure 
FUNCTION CentimetersToPoints RETURNS DECIMAL
  (  i as dec ) :
  
  RETURN I * 28.35 .   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteBookMark) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION deleteBookMark Procedure 
FUNCTION deleteBookMark RETURNS LOGICAL
  (  A as char ) :

  Document:BookMarks:Item( A BY-VARIANT-POINTER ):Delete().
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndFBold) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndFBold Procedure 
FUNCTION EndFBold RETURNS LOGICAL
  (  ) :


  Selection:Font:Bold = False.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndFItalic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndFItalic Procedure 
FUNCTION EndFItalic RETURNS LOGICAL
  (  ) :


  Selection:Font:Italic = False.

  RETURN TRUE.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndFUnderline) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndFUnderline Procedure 
FUNCTION EndFUnderline RETURNS LOGICAL
  (  ) :

  Selection:Font:underline = False.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndKey Procedure 
FUNCTION EndKey RETURNS LOGICAL
  (  ) :

  Selection:EndKey({&wdLine}).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndOfDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndOfDoc Procedure 
FUNCTION EndOfDoc RETURNS LOGICAL
  (  ) :

  Selection:Endkey( {&wdStory} ).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExistBookMark) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExistBookMark Procedure 
FUNCTION ExistBookMark RETURNS LOGICAL
  (  a as char ) :
/*
    ActiveDocument.Bookmarks.Exists(nom)
*/

  RETURN Document:Bookmarks:Exists( a ).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExpandSectionSelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExpandSectionSelection Procedure 
FUNCTION ExpandSectionSelection RETURNS LOGICAL
  (  ) :

  Selection:ExtendMode = true.
  Selection:Expand( {&wdSection} BY-VARIANT-POINTER).
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FBold) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FBold Procedure 
FUNCTION FBold RETURNS LOGICAL
  (  ) :

  Selection:Font:Bold = True.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FileClose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FileClose Procedure 
FUNCTION FileClose RETURNS LOGICAL
  (  Withsave as log  ) :
/*
 <com-handle>: Close ( 
          <anytype>-SaveChanges    BY-VARIANT-POINTER,
          <anytype>-OriginalFormat BY-VARIANT-POINTER,
          <anytype>-RouteDocument  BY-VARIANT-POINTER ).
*/
          
  Document:Close( If WithSave then {&wdSaveChanges} 
                              else {&wdDoNotSaveChanges} ).       
  Release Object Selection.
  Release Object Document.
  
  If Application:Documents:Count > 0 then
              Document    =   Application:ActiveDocument.
              
  If valid-Handle(Document) then
          SELECTION   =   Document:Application:Selection.
      
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FilePrint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FilePrint Procedure 
FUNCTION FilePrint RETURNS LOGICAL
  (  ) :
/*
NO-RETURN-VALUE <com-handle>: PrintOut ( 
          <anytype>-Background BY-VARIANT-POINTER,
          <anytype>-Append BY-VARIANT-POINTER,
          <anytype>-Range BY-VARIANT-POINTER,
          <anytype>-OutputFileName BY-VARIANT-POINTER,
          <anytype>-From BY-VARIANT-POINTER,
          <anytype>-To BY-VARIANT-POINTER,
          <anytype>-Item BY-VARIANT-POINTER,
          <anytype>-Copies BY-VARIANT-POINTER,
          <anytype>-Pages BY-VARIANT-POINTER,
          <anytype>-PageType BY-VARIANT-POINTER,
          <anytype>-PrintToFile BY-VARIANT-POINTER,
          <anytype>-Collate BY-VARIANT-POINTER,
          <anytype>-ActivePrinterMacGX BY-VARIANT-POINTER,
          <anytype>-ManualDuplexPrint BY-VARIANT-POINTER ).
*/
NO-RETURN-VALUE Document:PrintOut (  ).
                  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FileQuit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FileQuit Procedure 
FUNCTION FileQuit RETURNS LOGICAL
  (  ) :

  NO-RETURN-VALUE  Application:QUIT().
  
  If Valid-Handle(Selection)   then Release Object Selection.
  If Valid-Handle(Document)    then Release Object Document.
  If Valid-Handle(Application) then Release Object Application.              
                        
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FileSave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FileSave Procedure 
FUNCTION FileSave RETURNS LOGICAL
  (  ) :
    NO-RETURN-VALUE Document:Save (  ).

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FileSaveAs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FileSaveAs Procedure 
FUNCTION FileSaveAs RETURNS LOGICAL
  (  a as Char) :
/*
NO-RETURN-VALUE <com-handle>: SaveAs ( 
          <anytype>-FileName BY-VARIANT-POINTER,
          <anytype>-FileFormat BY-VARIANT-POINTER,
          <anytype>-LockComments BY-VARIANT-POINTER,
          <anytype>-Password BY-VARIANT-POINTER,
          <anytype>-AddToRecentFiles BY-VARIANT-POINTER,
          <anytype>-WritePassword BY-VARIANT-POINTER,
          <anytype>-ReadOnlyRecommended BY-VARIANT-POINTER,
          <anytype>-EmbedTrueTypeFonts BY-VARIANT-POINTER,
          <anytype>-SaveNativePictureFormat BY-VARIANT-POINTER,
          <anytype>-SaveFormsData BY-VARIANT-POINTER,
          <anytype>-SaveAsAOCELetter BY-VARIANT-POINTER ).
*/

    NO-RETURN-VALUE Document:SaveAs ( A BY-VARIANT-POINTER ).         

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FItalic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FItalic Procedure 
FUNCTION FItalic RETURNS LOGICAL
  (  ) :
/*
Selection.Font.Italic = True
*/

  Selection:Font:Italic = True.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FNormal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FNormal Procedure 
FUNCTION FNormal RETURNS LOGICAL
  (  ) :

  Selection:Font:Bold = False.
  Selection:Font:Underline = False.
  Selection:Font:Italic = False.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FormatParagraph) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FormatParagraph Procedure 
FUNCTION FormatParagraph RETURNS LOGICAL
  (  X as dec, Y as dec ) :
/*
FormatParagraphes RetraitGauche, RetraitDroit, Avant, 
    Après, ModeInterligne, Interligne, Alignement, LigneVeuve, 
    ParagraphesSolidaires, LignesSolidaires, SautPage, 
    SansNumLigne, SansCoupureMots, Onglet, RetraitPremLigne     
    With ActiveDocument.Paragraphs(1)    
            .LeftIndent = numéro    
            .RightIndent = numéro    
            .SpaceBefore = numéro    
            .SpaceAfter = numéro    
            .LineSpacingRule = WdLineSpacing    
            .LineSpacing = numéro    
            .Alignment = WdParagraphAlignment    
            .WidowControl = True    
            .KeepWithNext = True    
            .KeepTogether  = True    
            .PageBreakBefore = True    
            .NoLineNumber = True    
            .Hyphenation = True    
            .FirstLineIndent = numéroEnd 
        With Dialogs(wdDialogFormatParagraph)    
            .DefaultTab = WdWordDialogTab    
            .ShowEnd
*/

  Document:Paragraphs(1):LeftIndent = X.
  Document:Paragraphs(1):LeftIndent = Y.
    
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FormatTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FormatTable Procedure 
FUNCTION FormatTable RETURNS LOGICAL
  (  I as int,
     tableFormat as int,
     ApplyBorders as log) :

NO-RETURN-VALUE Document:Tables:Item(I):AutoFormat ( 
          TableFormat BY-VARIANT-POINTER,
          ApplyBorders BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER
/*        ,ApplyShading BY-VARIANT-POINTER,
 *        ApplyFont BY-VARIANT-POINTER,
 *        ApplyColor BY-VARIANT-POINTER,
 *        ApplyHeadingRows BY-VARIANT-POINTER,
 *        ApplyLastRow BY-VARIANT-POINTER,
 *        ApplyFirstColumn BY-VARIANT-POINTER,
 *        ApplyLastColumn BY-VARIANT-POINTER,
 *        AutoFit BY-VARIANT-POINTER 
 */  ).
          
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FUnderline) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FUnderline Procedure 
FUNCTION FUnderline RETURNS LOGICAL
  (  ) :

  Selection:Font:underline = True.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetAutoVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetAutoVersion Procedure 
FUNCTION GetAutoVersion RETURNS CHARACTER
  (  ) :
  RETURN "3.0".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GotoBookMark) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GotoBookMark Procedure 
FUNCTION GotoBookMark RETURNS LOGICAL
  (  A AS CHAR ) :
 
  If not valid-Handle(Selection) then   do :
                    Message "Cannot GOTO" A "," skip
                            "a Selection is not active !"
                            view-as alert-box error.
                    return FALSE.
                    end.
    
  Selection:GoTo ( 
                    {&wdGoToBookmark} BY-VARIANT-POINTER,,,
                    A              BY-VARIANT-POINTER
                  ).   
       
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-gotoNextSection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION gotoNextSection Procedure 
FUNCTION gotoNextSection RETURNS LOGICAL
  (  ) :
  Selection:GotoNext( {&wdGotoSection} ).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertCR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertCR Procedure 
FUNCTION InsertCR RETURNS LOGICAL
  (  ) :
  Selection:TypeParagraph (  ).

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertDateTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertDateTime Procedure 
FUNCTION InsertDateTime RETURNS LOGICAL
  (  A as char, asField as log) :
NO-RETURN-VALUE Selection:InsertDateTime ( 
          A BY-VARIANT-POINTER,
          asField BY-VARIANT-POINTER
/*        <anytype>-InsertAsFullWidth BY-VARIANT-POINTER,
 *        <anytype>-DateLanguage BY-VARIANT-POINTER,
 *        <anytype>-CalendarType BY-VARIANT-POINTER */
          ).
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-insertFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION insertFile Procedure 
FUNCTION insertFile RETURNS LOGICAL
  (  A as char ) :
/* InsertFile ( 
 *        Character-FileName,
 *        <anytype>-Range BY-VARIANT-POINTER,
 *        <anytype>-ConfirmConversions BY-VARIANT-POINTER,
 *        <anytype>-Link BY-VARIANT-POINTER,
 *        <anytype>-Attachment BY-VARIANT-POINTER ).*/

  Selection:InsertFile( A,
                        ,
                        ,
                        false BY-VARIANT-POINTER,
                        false BY-VARIANT-POINTER
                        ).        
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertImage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertImage Procedure 
FUNCTION InsertImage RETURNS LOGICAL
  (  A as char ) :

        Document:Shapes:AddPicture ( 
          A,
          TRUE BY-VARIANT-POINTER,
          FALSE BY-VARIANT-POINTER ).
          
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertRow Procedure 
FUNCTION InsertRow RETURNS LOGICAL
  (  I as int ) :
If I > 0 then
  Selection:InsertRows( I by-variant-pointer).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertSection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertSection Procedure 
FUNCTION InsertSection RETURNS LOGICAL
  (  ) :

    NO-RETURN-VALUE  Selection:Range:InsertBreak({&wdSectionBreakNextPage} BY-VARIANT-POINTER ).
 
    RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-insertSectionSkip) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION insertSectionSkip Procedure 
FUNCTION insertSectionSkip RETURNS LOGICAL
  (  ) :
  Selection:insertBreak( {&wdSectionBreakNextPage} BY-VARIANT-POINTER ).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertTable Procedure 
FUNCTION InsertTable RETURNS LOGICAL
  (  X as int, Y as int) :
/*
expression.Add(Range, NumRows, NumColumns)

newDoc.Tables.Add(Selection.Range, 3, 5)

*/
Def var hRange as com-handle no-undo.

    hRange = Selection:Range.
    
    Document:Tables:Add ( 
          hRange BY-POINTER,
          X,
          Y ).
          
  Release object hRange.          
          
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InsertText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertText Procedure 
FUNCTION InsertText RETURNS LOGICAL
  (  A as char ) :

  IF A <> ? then
              Selection:TypeText ( A ).   

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isAutomation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isAutomation Procedure 
FUNCTION isAutomation RETURNS LOGICAL
  (  ) :
 
  CREATE "{&WordAppVersion}" Application NO-ERROR.
  
  IF NOT ERROR-STATUS:ERROR then do :
                FileQuit().
                Return TRUE.
                end.
  else
                Return False.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Justify) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Justify Procedure 
FUNCTION Justify RETURNS LOGICAL
  (  ) :
  Selection:Paragraphs:Alignment = {&wdAlignParagraphJustify}.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeftJustify) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LeftJustify Procedure 
FUNCTION LeftJustify RETURNS LOGICAL
  (  ) :
  Selection:Paragraphs:Alignment = {&wdAlignParagraphLeft}.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LowerCase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LowerCase Procedure 
FUNCTION LowerCase RETURNS LOGICAL
  (  ) :

  Selection:range:case( {&wdLowerCase} ).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MailToDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MailToDoc Procedure 
FUNCTION MailToDoc RETURNS LOGICAL
  (  ) :
/*
FusionVersDoc   Documents(nom).MailMerge.Destination = wdSendToNewDocument
FusionVersImprimante    ActiveDocument.MailMerge.Destination = wdSendToPrinter

*/

    Document:MailMerge:Destination = {&wdSendToNewDocument}.
    Document:MailMerge:Execute (  ).
  

  
  Document  = Application:ActiveDocument.
  Selection = Application:Selection.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MailToPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MailToPrinter Procedure 
FUNCTION MailToPrinter RETURNS LOGICAL
  (  ) :
/*
FusionVersDoc   Documents(nom).MailMerge.Destination = wdSendToNewDocument
FusionVersImprimante    ActiveDocument.MailMerge.Destination = wdSendToPrinter

*/

    Document:MailMerge:Destination = {&wdSendToPrinter}.
    Document:MailMerge:Execute (  ).
  

  
  Document  = Application:ActiveDocument.
  Selection = Application:Selection.
  
  RETURN TRUE.   /* Function return value. */
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MoveDown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MoveDown Procedure 
FUNCTION MoveDown RETURNS LOGICAL
  (  I as int ) :
/*------------------------------------------------------------------------------
  Purpose:  I = # of movedown to do
*/

    Selection:MoveDown( {&wdScreen}, I, {&wdMove}).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MoveLeft) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MoveLeft Procedure 
FUNCTION MoveLeft RETURNS LOGICAL
  (  I as int ) :
  Selection:MoveLeft( {&wdCharacter}  BY-VARIANT-POINTER, I  BY-VARIANT-POINTER).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewFile Procedure 
FUNCTION NewFile RETURNS LOGICAL
  (  ) :

  Application:Documents:Add().
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NextColumn Procedure 
FUNCTION NextColumn RETURNS Logical
  (  ) :
/*
Selection.Move Unit:=wdCell, Count:=1
*/


    Selection:MoveRight ( 
          {&wdCell} BY-VARIANT-POINTER,
          1 BY-VARIANT-POINTER ).
          
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextPageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NextPageFooter Procedure 
FUNCTION NextPageFooter RETURNS LOGICAL
  (  ) :
  NextPageHeader().
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextPageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NextPageHeader Procedure 
FUNCTION NextPageHeader RETURNS LOGICAL
  (  ) :

  Application:ActiveWindow:View:NextHeaderFooter (  ).

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NextRow Procedure 
FUNCTION NextRow RETURNS Logical
  (  ) :

    Selection:Move ( 
          {&wdRow} BY-VARIANT-POINTER,
          1 BY-VARIANT-POINTER ).
          
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NextWindow Procedure 
FUNCTION NextWindow RETURNS LOGICAL
  (  ) :
Def var nWindow as INT        NO-UNDO.

  nWindow = Application:ActiveWindow:WindowNumber.  /* Current Window number */
 
  If nWindow = Application:Windows:Count 
  then do :
    If nWindow = 1 then Return FALSE.
                   else nWindow = 0.
  END.
               

    nWindow = nWindow + 1.

    Application:Windows:Item(nWindow by-Variant-pointer):Activate().
    Document    = Application:Documents:Item( nWindow by-Variant-pointer).  
    SELECTION   =   Document:Application:Selection.
    Return TRUE.            

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NormalView) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NormalView Procedure 
FUNCTION NormalView RETURNS LOGICAL
  (  ) :

  Application:ActiveWindow:View:Type = {&wdNormalView}.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpenDOC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OpenDOC Procedure 
FUNCTION OpenDOC RETURNS LOGICAL
  (  A AS CHAR ) :

  Document    =   Application:Documents:Open ( A ).
  
  SELECTION   =   Document:Application:Selection.
    
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpenDOCReadOnly) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OpenDOCReadOnly Procedure 
FUNCTION OpenDOCReadOnly RETURNS LOGICAL
  (  A AS CHAR ) :

  Document    =   Application:Documents:Open ( A, , True ).
  
  SELECTION   =   Document:Application:Selection.
    
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpenWord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OpenWord Procedure 
FUNCTION OpenWord RETURNS LOGICAL
  (  ) :

  CREATE "{&WordAppVersion}" Application NO-ERROR.
  
  RETURN (NOT ERROR-STATUS:ERROR).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PageHeader Procedure 
FUNCTION PageHeader RETURNS LOGICAL
  (  ) :

  Application:ActiveWindow:View:Type      = {&wdPrintView}.
  Application:ActiveWindow:View:SeekView  = {&wdSeekCurrentPageHeader}.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageView) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PageView Procedure 
FUNCTION PageView RETURNS LOGICAL
  (  ) :

  Application:ActiveWindow:View:Type = {&wdPrintView}.
  
  RETURN TRUE.   /* Function return value. */
 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PCenter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PCenter Procedure 
FUNCTION PCenter RETURNS LOGICAL
  (  ) :
  Selection:Paragraphs:Alignment = {&wdAlignParagraphCenter}.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PreviousWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PreviousWindow Procedure 
FUNCTION PreviousWindow RETURNS LOGICAL
  (  ) :
  /* Get the current window number
     ===========================*/

Def var nWindow as INT        NO-UNDO.

  nWindow = Application:ActiveWindow:WindowNumber.  /* Current Window number */
  If nWindow = 1 
  then DO :
    iF Application:Windows:Count = 1 
        then Return FALSE.
        else nWindow = Application:Windows:Count + 1.
    END.
               

    nWindow = nWindow - 1.

    Application:Windows:Item(nWindow by-Variant-pointer):Activate().
    Document = Application:Documents:Item( nWindow by-Variant-pointer).  
    SELECTION   =   Document:Application:Selection.
    Return TRUE.
       

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProtectDocument) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ProtectDocument Procedure 
FUNCTION ProtectDocument RETURNS LOGICAL
  ( ) :

    NO-RETURN-VALUE Document:Protect ({&wdAllowOnlyFormFields}).        

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RightJustify) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RightJustify Procedure 
FUNCTION RightJustify RETURNS LOGICAL
  (  ) :
  Selection:Paragraphs:Alignment = {&wdAlignParagraphRight}.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetFont) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetFont Procedure 
FUNCTION SetFont RETURNS LOGICAL
  (  A as char) :
  Selection:Font:Name = A.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSize Procedure 
FUNCTION SetSize RETURNS LOGICAL
  (  I as int ) :
  Selection:Font:Size = I.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startOfDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startOfDoc Procedure 
FUNCTION startOfDoc RETURNS LOGICAL
  (  ) :
          
  Selection:HomeKey( {&wdStory} BY-VARIANT-POINTER, {&wdMove} BY-VARIANT-POINTER).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidthColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WidthColumn Procedure 
FUNCTION WidthColumn RETURNS LOGICAL
  (  i as int, j as int, Decimal-ColumnWidth as dec) :
/*
ActiveDocument.Tables(1).Columns.SetWidth ColumnWidth:=num, 


Selection.Tables(1).Columns(1).PreferredWidth = CentimetersToPoints(2.2)
NO-RETURN-VALUE <com-handle>: SetWidth ( 
          Decimal-ColumnWidth AS FLOAT,
          Integer-RulerStyle ).
          
*/

  NO-RETURN-VALUE Document:Tables:Item( I ):columns:Item( J ):SetWidth ( 
          Decimal-ColumnWidth AS FLOAT,
          {&wdAdjustNone} ).
  RETURN True.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WindowClose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WindowClose Procedure 
FUNCTION WindowClose RETURNS LOGICAL
  (  ) :
  Application:ActiveWindow:Close().
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

