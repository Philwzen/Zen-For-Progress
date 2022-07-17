&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
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

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEF INPUT PARAMETER ip-type AS CHAR FORMAT "x(5)" NO-UNDO.
DEF INPUT PARAMETER ip-gross AS DEC FORMAT "zz,zzz,zz9.99-" NO-UNDO.
DEF INPUT PARAMETER ip-agcy_comm AS DEC FORMAT "z,zzz,zz9.99-" NO-UNDO.
DEF INPUT PARAMETER ip-lloyds_comm AS DEC FORMAT "z,zzz,zz9.99-" NO-UNDO.
DEF INPUT PARAMETER ip-ipt AS DEC FORMAT "zzz,zz9.99-" NO-UNDO.
DEF INPUT PARAMETER ip-nett AS DEC FORMAT "zz,zzz,zz9.99-" NO-UNDO.
DEF INPUT PARAMETER ip-part_pay AS DEC FORMAT "zz,zzz,zz9.99-" NO-UNDO.
DEF INPUT PARAMETER ip-balance AS DEC FORMAT "zz,zzz,zz9.99-" NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lb-gross lb-agcy_comm lb-lloyds_comm lb-ipt ~
lb-nett lb-balance 
&Scoped-Define DISPLAYED-OBJECTS fi-gross fi-agcy_comm fi-lloyds_comm ~
fi-ipt fi-nett fi-balance lb-gross lb-agcy_comm lb-lloyds_comm lb-ipt ~
lb-nett lb-balance 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
def var fi-agcy_comm as dec FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

def var fi-balance as dec FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

def var fi-gross as dec FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

def var fi-ipt as dec FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

def var fi-lloyds_comm as dec FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

def var fi-nett as dec FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

def var lb-agcy_comm as char FORMAT "X(20)":U INITIAL "Agency Comm" 
      VIEW-AS TEXT 
     SIZE 18 BY .62
     FONT 6 NO-UNDO.

def var lb-balance as char FORMAT "X(20)":U INITIAL "Balance" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

def var lb-gross as char FORMAT "X(20)":U INITIAL "Gross Premium" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

def var lb-ipt as char FORMAT "X(256)":U INITIAL "IPT Amount" 
      VIEW-AS TEXT 
     SIZE 16 BY .62
     FONT 6 NO-UNDO.

def var lb-lloyds_comm as char FORMAT "X(20)":U INITIAL "Lloyd's Comm" 
      VIEW-AS TEXT 
     SIZE 18 BY .62
     FONT 6 NO-UNDO.

def var lb-nett as char FORMAT "X(20)":U INITIAL "Nett Amount" 
      VIEW-AS TEXT 
     SIZE 15 BY .62
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fi-gross AT ROW 3.14 COL 7 COLON-ALIGNED NO-LABEL
     fi-agcy_comm AT ROW 3.14 COL 32 NO-LABEL
     fi-lloyds_comm AT ROW 3.14 COL 55 NO-LABEL
     fi-ipt AT ROW 3.14 COL 78 NO-LABEL
     fi-nett AT ROW 3.14 COL 101 NO-LABEL
     fi-balance AT ROW 3.14 COL 124 NO-LABEL
     lb-gross AT ROW 2.19 COL 7 COLON-ALIGNED NO-LABEL
     lb-agcy_comm AT ROW 2.19 COL 30 COLON-ALIGNED NO-LABEL
     lb-lloyds_comm AT ROW 2.19 COL 53 COLON-ALIGNED NO-LABEL
     lb-ipt AT ROW 2.19 COL 76 COLON-ALIGNED NO-LABEL
     lb-nett AT ROW 2.19 COL 99 COLON-ALIGNED NO-LABEL
     lb-balance AT ROW 2.19 COL 122 COLON-ALIGNED NO-LABEL
     SPACE(15.19) SKIP(2.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Totals".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-agcy_comm IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-balance IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-gross IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ipt IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-lloyds_comm IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-nett IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Totals */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-agcy_comm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-agcy_comm Dialog-Frame
ON RETURN OF fi-agcy_comm IN FRAME Dialog-Frame
DO:
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-balance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-balance Dialog-Frame
ON RETURN OF fi-balance IN FRAME Dialog-Frame
DO:
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-gross
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-gross Dialog-Frame
ON RETURN OF fi-gross IN FRAME Dialog-Frame
DO:
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ipt Dialog-Frame
ON RETURN OF fi-ipt IN FRAME Dialog-Frame
DO:
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-lloyds_comm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-lloyds_comm Dialog-Frame
ON RETURN OF fi-lloyds_comm IN FRAME Dialog-Frame
DO:
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nett Dialog-Frame
ON RETURN OF fi-nett IN FRAME Dialog-Frame
DO:
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  IF ip-type = "sus" THEN
      ASSIGN fi-gross:HIDDEN = TRUE
             lb-gross:HIDDEN = TRUE
             fi-nett:HIDDEN = TRUE
             lb-nett:HIDDEN = TRUE
             fi-balance:HIDDEN = TRUE
             lb-balance:HIDDEN = TRUE
             lb-ipt:SCREEN-VALUE = "Sundry".
  
  ASSIGN
      fi-gross:SCREEN-VALUE = STRING(ip-gross)
      fi-agcy_comm:SCREEN-VALUE = STRING(ip-agcy_comm)
      fi-lloyds_comm:SCREEN-VALUE = STRING(ip-lloyds_comm)
      fi-ipt:SCREEN-VALUE = STRING(ip-ipt)
      fi-nett:SCREEN-VALUE = STRING(ip-nett)
      fi-balance:SCREEN-VALUE = STRING(ip-balance).
    
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY fi-gross fi-agcy_comm fi-lloyds_comm fi-ipt fi-nett fi-balance 
          lb-gross lb-agcy_comm lb-lloyds_comm lb-ipt lb-nett lb-balance 
      WITH FRAME Dialog-Frame.
  ENABLE lb-gross lb-agcy_comm lb-lloyds_comm lb-ipt lb-nett lb-balance 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

