&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME indow-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-Dwidget NO-UNDO LIKE Zen-Dwidget.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS indow-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                                                         */
/*  PROGRAM TITLE   :                                                         */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    :                                                         */
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
/****************************************************************************** 
 this is a semi generic routine you only need to paint the screen widgets
 then edit pc-save for any fields not on the screen ie time stamps etc
 and edit validate-screen for any validation logic you require to allow record 
 to be saved 
********************************************************************************/

CREATE WIDGET-POOL.
&glob title-text Widget Security
&glob table-name zen-dwidget
&glob unique-key    {&table-name}TableId
&glob KeepRefreshButton
/* ***************************  Definitions  ************************** */
{app-paths.i}

/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */

/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
def var lv-pgm as char no-undo format 'x(50)'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frame-maint
&Scoped-define BROWSE-NAME br-maint

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-zen-dwidget

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-dwidget.wid-name ~
t-zen-dwidget.wid-label 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-dwidget NO-LOCK ~
    BY t-zen-dwidget.wid-label
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-dwidget NO-LOCK ~
    BY t-zen-dwidget.wid-label.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-dwidget
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-dwidget


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint btn-properties 
&Scoped-Define DISPLAYED-FIELDS t-Zen-Dwidget.wid-label ~
t-Zen-Dwidget.wid-name t-Zen-Dwidget.widframe t-Zen-Dwidget.x-pos ~
t-Zen-Dwidget.y-pos t-Zen-Dwidget.not-groups t-Zen-Dwidget.not-users ~
t-Zen-Dwidget.help-file t-Zen-Dwidget.help-ref 
&Scoped-define DISPLAYED-TABLES t-Zen-Dwidget
&Scoped-define FIRST-DISPLAYED-TABLE t-Zen-Dwidget


/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-Zen-Dwidget.wid-label t-Zen-Dwidget.wid-name ~
t-Zen-Dwidget.widframe t-Zen-Dwidget.x-pos t-Zen-Dwidget.y-pos ~
t-Zen-Dwidget.not-groups t-Zen-Dwidget.not-users t-Zen-Dwidget.help-file ~
t-Zen-Dwidget.help-ref 
&Scoped-define Edit-List t-Zen-Dwidget.wid-label t-Zen-Dwidget.wid-name ~
t-Zen-Dwidget.widframe t-Zen-Dwidget.x-pos t-Zen-Dwidget.y-pos ~
t-Zen-Dwidget.not-groups t-Zen-Dwidget.not-users t-Zen-Dwidget.help-file ~
t-Zen-Dwidget.help-ref 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR indow-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-properties 
     LABEL "Properties" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-dwidget SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint indow-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-dwidget.wid-name WIDTH 32.4
      t-zen-dwidget.wid-label WIDTH 27.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 66.2 BY 7.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     br-maint AT ROW 1.24 COL 12 HELP
          "Select the record to edit."
     t-Zen-Dwidget.wid-label AT ROW 9.1 COL 30.2 COLON-ALIGNED FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     t-Zen-Dwidget.wid-name AT ROW 10.05 COL 30.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     t-Zen-Dwidget.widframe AT ROW 11 COL 30.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     t-Zen-Dwidget.x-pos AT ROW 12.33 COL 30.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     btn-properties AT ROW 12.67 COL 53.6
     t-Zen-Dwidget.y-pos AT ROW 13.29 COL 30.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     t-Zen-Dwidget.not-groups AT ROW 14.43 COL 30 COLON-ALIGNED FORMAT "X(55)"
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     t-Zen-Dwidget.not-users AT ROW 15.38 COL 30 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     t-Zen-Dwidget.help-file AT ROW 16.38 COL 30 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     t-Zen-Dwidget.help-ref AT ROW 17.33 COL 30 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.14
         SIZE 80.2 BY 18.86
         TITLE "Screen Widgets".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-Zen-Dwidget T "?" NO-UNDO schadm Zen-Dwidget
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW indow-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Browse Maintenance"
         COLUMN             = 19
         ROW                = 7.1
         HEIGHT             = 20.48
         WIDTH              = 80.2
         MAX-HEIGHT         = 45.33
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.33
         VIRTUAL-WIDTH      = 256
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN indow-maint = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW indow-maint
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frame-maint
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint 1 frame-maint */
/* SETTINGS FOR FILL-IN t-Zen-Dwidget.help-file IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Dwidget.help-ref IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Dwidget.not-groups IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-Zen-Dwidget.not-users IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Dwidget.wid-label IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN t-Zen-Dwidget.wid-name IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Dwidget.widframe IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Dwidget.x-pos IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Dwidget.y-pos IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-dwidget"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _OrdList          = "Temp-Tables.t-zen-dwidget.wid-label|yes"
     _FldNameList[1]   > Temp-Tables.t-zen-dwidget.wid-name
"t-zen-dwidget.wid-name" ? ? "character" ? ? ? ? ? ? no ? no no "32.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-dwidget.wid-label
"t-zen-dwidget.wid-label" ? ? "character" ? ? ? ? ? ? no ? no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-properties
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-properties indow-maint
ON CHOOSE OF btn-properties IN FRAME frame-maint /* Properties */
DO:
  
    if avail {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} 
    then runchild("{&core}wid-properties.w",this-procedure).  
    else
       message 'Please Select a Widget First' {&mess-disp-type}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK indow-maint 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/*******************
    DONT FORGET TO CHECK DIR LOCATION FOR BELOW INCLUDE !!!!!!!
********************/
{{&core}commonmaint.i &path = "{&core}{&srv}"
                     &extraparams = "lv-pgm,"}
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-validate indow-maint 
PROCEDURE delete-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    Return "passed".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI indow-maint  _DEFAULT-DISABLE
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
  HIDE FRAME frame-maint.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI indow-maint  _DEFAULT-ENABLE
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
  IF AVAILABLE t-Zen-Dwidget THEN 
    DISPLAY t-Zen-Dwidget.wid-label t-Zen-Dwidget.wid-name t-Zen-Dwidget.widframe 
          t-Zen-Dwidget.x-pos t-Zen-Dwidget.y-pos t-Zen-Dwidget.not-groups 
          t-Zen-Dwidget.not-users t-Zen-Dwidget.help-file t-Zen-Dwidget.help-ref 
      WITH FRAME frame-maint.
  ENABLE br-maint btn-properties 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn indow-maint 
PROCEDURE Local-ChildReturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-from as char no-undo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialise indow-maint 
PROCEDURE Local-Initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-new-trigger indow-maint 
PROCEDURE local-new-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
btn-properties:sensitive in frame {&frame-name} = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-undo-trigger indow-maint 
PROCEDURE local-undo-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
btn-properties:sensitive in frame {&frame-name} = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-child-procedures indow-maint 
PROCEDURE local-update-child-procedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-to as handle no-undo.
case pv-to:name:
    when "{&core}wid-properties.w" then run refresh in pv-to 
            (lv-pgm,t-zen-dwidget.widframe,t-zen-dwidget.wid-name).
end case.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-save indow-maint 
PROCEDURE pre-save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
t-zen-dwidget.pgm = lv-pgm.
btn-properties:sensitive in frame {&frame-name} = true.
   return "passed".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh indow-maint 
PROCEDURE refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-pgm     as char no-undo.

lv-pgm    = pv-pgm.

frame {&frame-name}:title = frame {&frame-name}:title + ' For ' + lv-pgm.

FOR EACH t-{&table-name}:
    DELETE t-{&table-name}.
END.

run openquery.
btn-properties:sensitive = avail t-{&table-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen indow-maint 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  def var v-recid as recid no-undo. 
  def buffer b-{&table-name} for t-{&table-name}.

/*     if can-find(first b-product                                                 */
/*                 where b-product.product-Code =                                  */
/*                                             t-acccentre.product-Code:screen-value */
/*                   and recid(b-product) ne recid(t-acccentre))                     */
/*     then do:                                                                    */
/*         message "Error Product Already Exists!"                                 */
/*         view-as alert-box error.                                                */
/*         return no-apply.                                                        */
/*     end.                                                                        */
/*                                                                                 */

  return 'passed'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

