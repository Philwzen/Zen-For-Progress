&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-fldlook NO-UNDO LIKE zen-fldlook.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/
CREATE WIDGET-POOL.
{app-paths.i}
&glob KeepRefreshButton
&glob nobuttons
&glob noexitcheck
&glob nochangedcheck
/* ***************************  Definitions  ************************** */

&glob Table-name zen-fldlook
&glob unique-key    {&table-name}TableId
Def Input Param pv-startvalue As Char No-undo.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var ch-cal as com-handle no-undo.
def var lv-str as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-days rs-type Btn-go RECT-1 
&Scoped-Define DISPLAYED-OBJECTS lv-days rs-type 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
def var CtrlFrame AS WIDGET-HANDLE NO-UNDO.
def var chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-go 
     LABEL "Move" 
     SIZE 8.2 BY 1.14 TOOLTIP "Press this button to reposition the calendar.".

def var lv-days as int FORMAT "->>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 TOOLTIP "Enter a negative number to go back  (for example -5 for 5 days ago)."
     BGCOLOR 21  NO-UNDO.

def var rs-type as int 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Days", 1,
"Weeks", 7,
"Months", 30
     SIZE 15 BY 2.62
     BGCOLOR 21  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 4.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-days AT ROW 14.19 COL 16.8 NO-LABEL WIDGET-ID 4
     rs-type AT ROW 13.38 COL 28.2 NO-LABEL WIDGET-ID 8
     Btn-go AT ROW 14.1 COL 46.8 WIDGET-ID 2
     "Today +" VIEW-AS TEXT
          SIZE 10 BY .86 AT ROW 14.24 COL 6.4 WIDGET-ID 12
     "Reposition To:" VIEW-AS TEXT
          SIZE 20.2 BY 1 AT ROW 12.57 COL 4.8 WIDGET-ID 16
     RECT-1 AT ROW 12.19 COL 2.4 WIDGET-ID 14
     SPACE(0.99) SKIP(0.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select a Date".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-fldlook T "?" NO-UNDO schadm zen-fldlook
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lv-days IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 1.29
       COLUMN          = 5.6
       HEIGHT          = 10.19
       WIDTH           = 48.2
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {8E27C92B-1264-101C-8A2F-040224009C02} type: Calendar */
      CtrlFrame:MOVE-BEFORE(lv-days:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select a Date */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-go Dialog-Frame
ON CHOOSE OF Btn-go IN FRAME Dialog-Frame /* Move */
DO:
assign lv-days
       rs-type.
  ch-cal:Value = today + (lv-days * rs-type).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame Dialog-Frame OCX.DblClick
PROCEDURE CtrlFrame.Calendar.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
 lv-str = string(ch-cal:Value,'99/99/9999').
lv-str = lv-str + '{&Delim2}' + lv-str.
    apply 'go' to frame {&frame-name}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


ON ENDKEY, END-ERROR OF FRAME {&FRAME-NAME} ANYWHERE DO:
/*   return string(ch-cal:Value,'99/99/9999'). */
   RUN disable_UI.
END.
                                                   
ON CLOSE OF THIS-PROCEDURE do:
/*   return string(ch-cal:Value,'99/99/9999'). */
   RUN disable_UI.
end.   
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
return lv-str.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
def var UIB_S    as log    NO-UNDO.
def var OCXFile  as char  NO-UNDO.

OCXFile = SEARCH( "calendarsolo.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "calendarsolo.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY lv-days rs-type 
      WITH FRAME Dialog-Frame.
  ENABLE lv-days rs-type Btn-go RECT-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls Dialog-Frame 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 assign
    ch-cal       = chCtrlFrame:Calendar
    ch-cal:Value = today.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

