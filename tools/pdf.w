&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{app-paths.i}
/* Parameters Definitions ---                                           */
&glob nobuttons
&undefine suppresswindow
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-file lv-status BUTTON-1 btnpcl btndll ~
btnoak2 btn-done btntiff 
&Scoped-Define DISPLAYED-OBJECTS lv-file lv-status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-file 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvPcl C-Win 
FUNCTION ConvPcl RETURNS CHARACTER
  ( pv-ipfile as char,
    pv-type as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EscapeeToPdf C-Win 
FUNCTION EscapeeToPdf RETURNS CHARACTER
  ( pv-txtfile as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OutPutToPdf C-Win 
FUNCTION OutPutToPdf RETURNS CHARACTER
  ( pv-txtfile as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PclToPdf C-Win 
FUNCTION PclToPdf RETURNS CHARACTER
  ( pv-txtfile as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-done 
     LABEL "Done" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btndll 
     LABEL "by dll to pdf" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnoak2 
     LABEL "oak pcl2pdf" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnpcl 
     LABEL "pcl to pdf" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btntiff 
     LABEL "To Tiff" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-1 
     LABEL "To Pdf" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\rex~\a.txt" 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE lv-status AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lv-file AT ROW 2.43 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     lv-status AT ROW 3.62 COL 9 COLON-ALIGNED WIDGET-ID 6
     BUTTON-1 AT ROW 5.05 COL 3 WIDGET-ID 2
     btnpcl AT ROW 6.24 COL 3 WIDGET-ID 8
     btndll AT ROW 7.43 COL 3 WIDGET-ID 10
     btnoak2 AT ROW 8.86 COL 3 WIDGET-ID 22
     btn-done AT ROW 9.1 COL 63 WIDGET-ID 14
     btntiff AT ROW 10.05 COL 3 WIDGET-ID 26
     "OakDoc Exe" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 9.1 COL 21 WIDGET-ID 24
     "OakDoc Dll" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 7.67 COL 20 WIDGET-ID 16
     "PdfCreator Api" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 5.29 COL 19 WIDGET-ID 18
     "Using Escapee" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 6.52 COL 20 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 10.48 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 10.48
         WIDTH              = 80
         MAX-HEIGHT         = 46.33
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.33
         VIRTUAL-WIDTH      = 256
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN lv-file IN FRAME DEFAULT-FRAME
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-done C-Win
ON CHOOSE OF btn-done IN FRAME DEFAULT-FRAME /* Done */
DO:
  run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btndll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btndll C-Win
ON CHOOSE OF btndll IN FRAME DEFAULT-FRAME /* by dll to pdf */
DO:
     assign frame {&frame-name} {&list-1}.
  if search(lv-file) = ? then do:
  message 'Please enter a filename'.
  return no-apply.
  end.
  lv-status = 'Created ' + PclToPdf(lv-file).
  disp lv-status with frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnoak2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnoak2 C-Win
ON CHOOSE OF btnoak2 IN FRAME DEFAULT-FRAME /* oak pcl2pdf */
DO:
     assign frame {&frame-name} {&list-1}.
  if search(lv-file) = ? then do:
  message 'Please enter a filename'.
  return no-apply.
  end.
  lv-status = 'Created ' + ConvPcl(lv-file,'pdf').
  disp lv-status with frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnpcl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnpcl C-Win
ON CHOOSE OF btnpcl IN FRAME DEFAULT-FRAME /* pcl to pdf */
DO:
    assign frame {&frame-name} {&list-1}.
  if search(lv-file) = ? then do:
  message 'Please enter a filename'.
  return no-apply.
  end.
  lv-status = 'Created ' + EscapeeToPdf(lv-file).
  disp lv-status with frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btntiff
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btntiff C-Win
ON CHOOSE OF btntiff IN FRAME DEFAULT-FRAME /* To Tiff */
DO:
      assign frame {&frame-name} {&list-1}.
  if search(lv-file) = ? then do:
  message 'Please enter a filename'.
  return no-apply.
  end.
  lv-status = 'Created ' + ConvPcl(lv-file,'Tiff').
  disp lv-status with frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* To Pdf */
DO:
  assign frame {&frame-name} {&list-1}.
  if search(lv-file) = ? then do:
  message 'Please enter a filename'.
  return no-apply.
  end.
  lv-status = 'Created ' + OutPutToPdf(lv-file).
  disp lv-status with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-file C-Win
ON MOUSE-SELECT-DBLCLICK OF lv-file IN FRAME DEFAULT-FRAME
DO:
  self:screen-value = getosfile(lv-file).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{{&core}commonmaint.i}

procedure oakBegin external "pcltopdf.dll": 
def return param op As long.
end procedure.

procedure oakEnd external "pcltopdf.dll": 
def input param id As long.
end procedure.

procedure oakExport external "pcltopdf.dll": 
def input param id As long.
def return param op As long.
end procedure.

procedure oakSetOption external "pcltopdf.dll": 
def input param id As long.
def input param code As long. 
def input param nPara1 As long. 
def input param nPara2 As long. 
def input param sPara1 As char. 
def input param sPara2 As char. 
def return param op As long.
end procedure.

procedure oakGetOption external "pcltopdf.dll": 
def input param id As long.
def input param code As long. 
def input param nPara1 As long. 
def input param nPara2 As long. 
def input param sPara1 As char. 
def input param sPara2 As char. 
def return param op As long.
end procedure.


/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  {{&core}sec-chk.i}
  RUN enable_UI.
  {{&core}wid-chk.i}
  {{&core}focus.i}

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY lv-file lv-status 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE lv-file lv-status BUTTON-1 btnpcl btndll btnoak2 btn-done btntiff 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvPcl C-Win 
FUNCTION ConvPcl RETURNS CHARACTER
  ( pv-ipfile as char,
    pv-type as char ) :

def var lv-cmd     as char no-undo.
def var lv-opdir   as char no-undo.
def var lv-opfile  as char no-undo.
def var lv-ext     as char no-undo.
def var lv-origdir as char no-undo.
def var lv-appdir  as char no-undo.
def var lv-extra   as char no-undo.

assign
    lv-appdir = GetCtrl('PclConvDir')
    file-info:filename = '.'
    lv-origdir = file-info:full-pathname
    file-info:file-name = pv-ipfile
    pv-ipfile           = file-info:full-pathname
    lv-opdir = session:temp-directory
    lv-opfile = entry(num-entries(pv-ipfile,'\'),entry(1,pv-ipfile,'.'),'\').


/*
pcltoxxx -i "c:\my1.pcl" -o "d:\my_pdf"
*/

case pv-type:
    when 'pdf'  
    then assign lv-ext = '.pdf'
                lv-opfile = lv-opfile + lv-ext
                lv-cmd = 'pcltopdf -i "' + pv-ipfile + '" -o "' + lv-opdir + lv-opfile + '"'.

    when 'Tiff' 
    then assign lv-ext = '.tif'
                lv-opfile = lv-opfile + lv-ext
                lv-cmd = 'pcltotiff -i "' + pv-ipfile + '" -o "' + substring(lv-opdir,1,length(lv-opdir) - 1) + '" -m'. 
end case.

  SetWorkingDir(lv-appdir).
  os-command silent value(lv-cmd).
  
  SetWorkingDir(lv-origdir).

  RETURN search(lv-opdir + lv-opfile).
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EscapeeToPdf C-Win 
FUNCTION EscapeeToPdf RETURNS CHARACTER
  ( pv-txtfile as char ) :

def var lv-cmd as char no-undo.
def var lv-pdf as char no-undo.

assign
    file-info:file-name = pv-txtfile
    pv-txtfile          = file-info:full-pathname
    lv-pdf = session:temp-directory + entry(num-entries(pv-txtfile,'\'),entry(1,pv-txtfile,'.'),'\') + '.pdf'
    lv-cmd = '"C:\Program Files\RedTitan\software\ESCAPEE.EXE" ' + 
             pv-txtfile + ' /TO ' + 
             lv-pdf + ' /BORDER 0 /PDF /X'.
    
   wapicreateprocess(lv-cmd,session:temp-directory,0).

  RETURN lv-pdf.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OutPutToPdf C-Win 
FUNCTION OutPutToPdf RETURNS CHARACTER
  ( pv-txtfile as char ) :

def var lv-cmd as char no-undo.

def var PDFCreator as com-handle no-undo.
def var opt as com-handle no-undo.
def var lv-origprt as char no-undo.
def var lv-pdf as char no-undo.
  
CREATE 'PDFCreator.clsPDFCreator' PDFCreator connect no-error.
if error-status:error
then CREATE 'PDFCreator.clsPDFCreator' PDFCreator no-error.

    PDFCreator:cClearCache().
assign
    file-info:file-name = pv-txtfile
    pv-txtfile          = file-info:full-pathname
    opt = PDFCreator:cOptions
    opt:autosavedirectory = session:temp-directory
    opt:AutosaveFormat = 0
    opt:AutosaveFilename = entry(num-entries(pv-txtfile,'\'),entry(1,pv-txtfile,'.'),'\')
    opt:UseAutosave = 1
    opt:UseAutosaveDirectory = 1
    PDFCreator:cOptions = opt 
    lv-origprt = pdfcreator:cDefaultPrinter
    pdfcreator:cDefaultPrinter = "PDFCreator".
    
    pdfcreator:cSaveOptions().
    wait(500). /* give options time to settle */
    
    lv-pdf = opt:autosavedirectory + opt:autosavefilename + '.pdf'.
    os-delete value(lv-pdf).
    PDFCreator:cPrintFile(pv-txtfile).  
    etime(true).
    file-info:file-name = lv-pdf.
    do while file-info:file-size = ?:
     wait(500).
     file-info:file-name = lv-pdf.
     if etime > 10000 then do:
        message 'pdf create may have failed ' + lv-pdf.
        leave.
     end.
    end.
    wait(1000).
assign
    PDFCreator:cPrinterStop = False
    PDFCreator:cPrinterStop = true
    pdfcreator:cDefaultPrinter = lv-origprt.
    
release object pdfcreator.
  

  RETURN lv-pdf.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PclToPdf C-Win 
FUNCTION PclToPdf RETURNS CHARACTER
  ( pv-txtfile as char ) :

def var lv-cmd as char no-undo.
def var lv-pdf as char no-undo.
Def var id As Integer no-undo.
def var scratch as int no-undo.

assign
    file-info:file-name = pv-txtfile
    pv-txtfile          = file-info:full-pathname
    lv-pdf = session:temp-directory + entry(num-entries(pv-txtfile,'\'),entry(1,pv-txtfile,'.'),'\') + '.pdf'.
lv-pdf = 'output.pdf'.
&scope OAK_Set_Output  2101
&scope OAK_Set_Input  2102
&scope OAK_Set_PDFInfo 2105
os-delete value(lv-pdf).

run oakBegin(output id).
message '1 ' pv-txtfile search(pv-txtfile) skip
    lv-pdf search(lv-pdf) skip id.


If id <> 0 Then do:
    run oakSetOption(id,{&OAK_Set_Output},0,0,lv-pdf,'',output scratch).
    message '2 ' scratch.


    run oakSetOption(id,{&OAK_Set_Input},0,0,pv-txtfile,'',output scratch).
    message '3 ' scratch.

    run oakSetOption(id,{&OAK_Set_PDFInfo},0,0,"Title","Title1",output scratch).
    message '2a ' scratch.
    
    run oakExport(id,output scratch).
    message '4 ' scratch.
    
    run oakEnd(id).
    message '5'.
end.

RETURN search(lv-pdf).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

