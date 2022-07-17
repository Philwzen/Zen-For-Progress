&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          schadm           PROGRESS
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-classcode NO-UNDO LIKE zen-classcode.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                                                         */
/*  PROGRAM TITLE   :                                                         */ 
/*                                                                            */
/*  CREATE DATE     :                                                         */
/*                                                                            */
/*  COMPANY NAME    :                                                         */
/*  VERSION NO.     :    1.0                                                  */
/******************************************************************************/
/******************************************************************************/
/*                          PATCH HISTORY                                     */
/*                                                                            */
/*           PATCH  USR                 Job                                   */
/* DATE      NO.    ID                  REF DESCRIPTION                       */
/* -------------------------------------------------------------------------- */
/*                                                                            */  
/******************************************************************************/


CREATE WIDGET-POOL.
&glob title-text    Class Codes
&glob table-name    zen-classCode
&glob unique-key    {&table-name}TableId
&glob noprint
&glob KeepRefreshButton
&Glob ImmediateQuery


/* ***************************  Definitions  ************************** */
{app-paths.i}
/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
def var lv-class  as char no-undo.
def var lv-sysman as  log no-undo.
lv-sysman = systemmanager(GetUserID()).

&glob suppresswindow

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
&Scoped-define INTERNAL-TABLES t-zen-classCode

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-classCode.description ~
t-zen-classCode.code 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-classCode ~
      WHERE (if lv-sysman then true else t-zen-classCode.SysRecord = FALSE) NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-classCode ~
      WHERE (if lv-sysman then true else t-zen-classCode.SysRecord = FALSE) NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-classCode
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-classCode


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS t-zen-classcode.code ~
t-zen-classcode.description 
&Scoped-define ENABLED-TABLES t-zen-classcode
&Scoped-define FIRST-ENABLED-TABLE t-zen-classcode
&Scoped-Define ENABLED-OBJECTS cb-class RECT-7 br-maint 
&Scoped-Define DISPLAYED-FIELDS t-zen-classcode.code ~
t-zen-classcode.description t-zen-classcode.allflag ~
t-zen-classcode.noneflag t-zen-classcode.sysrecord 
&Scoped-define DISPLAYED-TABLES t-zen-classcode
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-classcode
&Scoped-Define DISPLAYED-OBJECTS cb-class fil-desc-label 

/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-zen-classcode.code t-zen-classcode.description ~
t-zen-classcode.allflag t-zen-classcode.noneflag t-zen-classcode.sysrecord 
&Scoped-define Edit-List t-zen-classcode.description ~
t-zen-classcode.allflag t-zen-classcode.noneflag t-zen-classcode.sysrecord 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cb-class AS CHARACTER FORMAT "X(256)":U 
     LABEL "Class" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 53.4 BY 1 NO-UNDO.

DEFINE VARIABLE fil-desc-label AS CHARACTER FORMAT "X(256)":U INITIAL "Description" 
      VIEW-AS TEXT 
     SIZE 13 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 4.52.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-classCode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-classCode.description FORMAT "X(200)":U WIDTH 70
      t-zen-classCode.code FORMAT "X(30)":U WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 89.4 BY 12.48 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     cb-class AT ROW 1.33 COL 16.6 COLON-ALIGNED
     br-maint AT ROW 2.81 COL 18.6 HELP
          "Select the record to edit."
     t-zen-classcode.code AT ROW 15.81 COL 26.8 COLON-ALIGNED DISABLE-AUTO-ZAP  FORMAT "X(255)"
          VIEW-AS FILL-IN 
          SIZE 39.2 BY 1
     t-zen-classcode.description AT ROW 17.19 COL 29 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 83 BY 2.62
     t-zen-classcode.allflag AT ROW 15.91 COL 74.2
          VIEW-AS TOGGLE-BOX
          SIZE 7.2 BY 1 TOOLTIP "All in combo box"
     t-zen-classcode.noneflag AT ROW 15.91 COL 83.2
          VIEW-AS TOGGLE-BOX
          SIZE 10.2 BY 1 TOOLTIP "None in combo box"
     t-zen-classcode.sysrecord AT ROW 15.91 COL 95.2
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY 1
     fil-desc-label AT ROW 17.19 COL 13 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 15.52 COL 13
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115.4 BY 20.33
         TITLE "Class Codes Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-classcode T "?" NO-UNDO schadm zen-classcode
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Class Code Maintenance"
         COLUMN             = 20.4
         ROW                = 8.91
         HEIGHT             = 20.38
         WIDTH              = 115.4
         MAX-HEIGHT         = 54.91
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 54.91
         VIRTUAL-WIDTH      = 320
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
ASSIGN window-maint = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW window-maint
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frame-maint
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB br-maint RECT-7 frame-maint */
/* SETTINGS FOR TOGGLE-BOX t-zen-classcode.allflag IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
ASSIGN 
       br-maint:COLUMN-RESIZABLE IN FRAME frame-maint       = TRUE.

/* SETTINGS FOR FILL-IN t-zen-classcode.code IN FRAME frame-maint
   1 EXP-FORMAT                                                         */
/* SETTINGS FOR EDITOR t-zen-classcode.description IN FRAME frame-maint
   1 2                                                                  */
ASSIGN 
       t-zen-classcode.description:RETURN-INSERTED IN FRAME frame-maint  = TRUE.

/* SETTINGS FOR FILL-IN fil-desc-label IN FRAME frame-maint
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX t-zen-classcode.noneflag IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-zen-classcode.sysrecord IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-classCode"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _Where[1]         = "(if lv-sysman then true else t-zen-classCode.SysRecord = FALSE)"
     _FldNameList[1]   > Temp-Tables.t-zen-classCode.description
"t-zen-classCode.description" ? ? "character" ? ? ? ? ? ? no ? no no "70" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-classCode.code
"t-zen-classCode.code" ? "X(30)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define SELF-NAME cb-class
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-class window-maint
ON VALUE-CHANGED OF cb-class IN FRAME frame-maint /* Class */
DO:
   
  run openquery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*******************
    DONT FORGET TO CHECK DIR LOCATION FOR BELOW INCLUDE !!!!!!!
********************/
{{&core}commonmaint.i &path = "{&core}{&srv}"
                     &extraparams = "lv-class,"}  
                        /* &extraparams = ??? */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-validate window-maint 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI window-maint  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disp-wids window-maint 
PROCEDURE disp-wids :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/         
    assign
        t-{&table-name}.SysRecord:hidden in frame {&frame-name} = not lv-sysman 
        t-zen-classCode.AllFlag:hidden  = t-zen-classCode.class ne "{&class}"
        t-zen-classCode.Noneflag:hidden = t-zen-classCode.class ne "{&class}".   
        
    IF lv-newmode THEN DO:
        t-zen-classCode.description:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

        APPLY "ENTRY" TO t-zen-classCode.code IN FRAME {&FRAME-NAME}.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI window-maint  _DEFAULT-ENABLE
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
  DISPLAY cb-class fil-desc-label 
      WITH FRAME frame-maint.
  IF AVAILABLE t-zen-classcode THEN 
    DISPLAY t-zen-classcode.code t-zen-classcode.description 
          t-zen-classcode.allflag t-zen-classcode.noneflag 
          t-zen-classcode.sysrecord 
      WITH FRAME frame-maint.
  ENABLE cb-class RECT-7 br-maint t-zen-classcode.code 
         t-zen-classcode.description 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extraassign window-maint 
PROCEDURE extraassign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
t-zen-classCode.class = getcombokey(cb-class:handle in frame {&frame-name}).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn window-maint 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise window-maint 
PROCEDURE local-initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

BuildCombo(cb-class:handle in frame {&frame-name},
                  "zen-classcode",
                  "code",
                  "description",
                  'where zen-classcode.class = "class"',
                  'by zen-classcode.description',
                  no,
                  no).
                                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-openquery window-maint 
PROCEDURE Local-openquery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  lv-class = getcombokey(cb-class:handle in frame {&frame-name}).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Update-Child-Procedures window-maint 
PROCEDURE Local-Update-Child-Procedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param pv-to as handle no-undo.

case pv-to:private-data:
/*     when 'order-maint.w' then run refresh in  pv-to (t-customer.cust-num).  */
/*     when 'sr-maint.w'    then run refresh in  pv-to.                        */
end case.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen window-maint 
PROCEDURE validate-screen :
/* ----------------------------------------------------------------
  Purpose:      Checks the zone ref and description are entered
  Parameters:   None
  Notes:        Puts up error messages if invalid and stops 
                processing.
-----------------------------------------------------------------*/
  def var v-recid as recid no-undo. 
  def buffer b-{&table-name} for t-{&table-name}.
                                                                                   
   if index(',',t-zen-classCode.code:screen-value in frame {&frame-name}) ne 0                                                                                   
   then do:
       message msg(39,'save','Code','Contains ,','') view-as alert-box information.
       return string(t-zen-classCode.code:handle).
   end.

   if t-zen-classCode.code:screen-value = '' 
   then do:
    message msg(132,'Code','',',','') view-as alert-box information.
    return string(t-zen-classCode.description:handle).
   end.
   
   if index(',',t-zen-classCode.description:screen-value) ne 0 
   then do:
    message msg(39,'Data','Description','Contains ,','') view-as alert-box information.
    return string(t-zen-classCode.description:handle).
   end.
   
   if t-zen-classCode.description:screen-value = '' 
   then do:
    message msg(132,'Description','',',','') view-as alert-box information.
    return string(t-zen-classCode.description:handle).
   end.
   return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

