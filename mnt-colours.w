&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-colours NO-UNDO LIKE zen-colours.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                                                         */
/*  PROGRAM TITLE   :    mnt-zone.w (formerly - sg210)                        */ 
/*                                                                            */
/*  CREATE DATE     :    22/09/00                                             */
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
/* 22/09/00  v1     Ed Estaugh          00  initial release                   */
/******************************************************************************/


CREATE WIDGET-POOL.
&glob title-text    System Colours
&glob table-name    zen-colours
&glob unique-key    {&table-name}TableId
&Glob ImmediateQuery
&glob KeepRefreshButton

/* ***************************  Definitions  ************************** */
{app-paths.i}

/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */

DEF VAR i             AS INT        NO-UNDO.

def var h-handle AS COM-HANDLE NO-UNDO.

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
&Scoped-define INTERNAL-TABLES t-zen-colours

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-colours.colour_name ~
t-zen-colours.colour_value t-zen-colours.blue_value ~
t-zen-colours.green_value t-zen-colours.red_value 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-colours NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-colours NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-colours
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-colours


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint btnSave btnColour RECT-7 
&Scoped-Define DISPLAYED-FIELDS t-zen-colours.colour_value ~
t-zen-colours.colour_name t-zen-colours.red_value t-zen-colours.green_value ~
t-zen-colours.blue_value 
&Scoped-define DISPLAYED-TABLES t-zen-colours
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-colours


/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-zen-colours.colour_value ~
t-zen-colours.colour_name t-zen-colours.red_value t-zen-colours.green_value ~
t-zen-colours.blue_value 
&Scoped-define Edit-List t-zen-colours.colour_value ~
t-zen-colours.colour_name t-zen-colours.red_value t-zen-colours.green_value ~
t-zen-colours.blue_value 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnColour 
     LABEL "Select Color" 
     SIZE 17.6 BY 1.14.

DEFINE BUTTON btnSave 
     LABEL "Save Colors to Registry" 
     SIZE 31.4 BY 1.14.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 106.8 BY 3.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-colours SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-colours.colour_name WIDTH 51.2
      t-zen-colours.colour_value COLUMN-LABEL "Progress" WIDTH 10.2
      t-zen-colours.blue_value
      t-zen-colours.green_value
      t-zen-colours.red_value
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 105.6 BY 12.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     br-maint AT ROW 1.43 COL 11.4 HELP
          "Select the record to edit."
     btnSave AT ROW 18.14 COL 43.8
     btnColour AT ROW 16 COL 92.4
     t-zen-colours.colour_value AT ROW 15.1 COL 28.2 COLON-ALIGNED
          LABEL "Progress Color"
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     t-zen-colours.colour_name AT ROW 14.1 COL 28.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 35.6 BY 1
     t-zen-colours.red_value AT ROW 14.1 COL 77.8 COLON-ALIGNED
          LABEL "Red"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     t-zen-colours.green_value AT ROW 15.1 COL 77.8 COLON-ALIGNED
          LABEL "Green"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     t-zen-colours.blue_value AT ROW 16.1 COL 77.8 COLON-ALIGNED
          LABEL "Blue"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     "RGB Color" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 14.05 COL 92.4
     RECT-7 AT ROW 13.86 COL 9
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.2 BY 19.81
         TITLE "Colors Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-colours T "?" NO-UNDO schadm zen-colours
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Zone Maintenance"
         COLUMN             = 62.8
         ROW                = 7.43
         HEIGHT             = 22.81
         WIDTH              = 121.8
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
ASSIGN window-maint = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW window-maint
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frame-maint
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB br-maint 1 frame-maint */
/* SETTINGS FOR FILL-IN t-zen-colours.blue_value IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-zen-colours.colour_name IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-zen-colours.colour_value IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-zen-colours.green_value IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN t-zen-colours.red_value IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-colours"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-zen-colours.colour_name
"t-zen-colours.colour_name" ? ? "character" ? ? ? ? ? ? no ? no no "51.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-colours.colour_value
"t-zen-colours.colour_value" "Progress" ? "integer" ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.t-zen-colours.blue_value
     _FldNameList[4]   = Temp-Tables.t-zen-colours.green_value
     _FldNameList[5]   = Temp-Tables.t-zen-colours.red_value
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME frame-maint:HANDLE
       ROW             = 14.81
       COLUMN          = 92.2
       HEIGHT          = .91
       WIDTH           = 17.6
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {93330F00-7CA6-101B-874B-0020AF109266} type: CSComboBox */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME t-zen-colours.blue_value
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-zen-colours.blue_value window-maint
ON LEAVE OF t-zen-colours.blue_value IN FRAME frame-maint /* Blue */
DO:
    RUN display-colour.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnColour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnColour window-maint
ON CHOOSE OF btnColour IN FRAME frame-maint /* Select Color */
DO:
  def var curr-color   as int INITIAL 15.
 

IF NOT COLOR-TABLE:GET-DYNAMIC(curr-color) AND
      NOT COLOR-TABLE:SET-DYNAMIC(curr-color,TRUE)
   THEN MESSAGE "Color must be DYNAMIC to edit.".
   ELSE SYSTEM-DIALOG COLOR curr-color.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave window-maint
ON CHOOSE OF btnSave IN FRAME frame-maint /* Save Colors to Registry */
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do:
   def var lv-colornum as char no-undo.
   def var NewKeyValue as char no-undo.
   def var lv-id as dec no-undo.
   lv-id = t-{&table-name}.{&table-name}tableid.
 
   /* loop through colours, saving to registry file */
for each t-{&table-name}:
    if colour_value le 15 then next.
    NewKeyValue = 
        string(red_value) + "," + 
        string(green_value) + "," + 
        string(blue_value).
  
    lv-colornum = 'color' + string(colour_value).
    put-key-value section 'colors' key lv-colornum value NewKeyValue.
end. /* for each */

 find t-{&table-name} where t-{&table-name}.{&table-name}tableid = lv-id.
 apply "value-changed" to {&browse-name}.
load 'progress'.
use 'progress'.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-zen-colours.green_value
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-zen-colours.green_value window-maint
ON LEAVE OF t-zen-colours.green_value IN FRAME frame-maint /* Green */
DO:
    RUN display-colour.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-zen-colours.red_value
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-zen-colours.red_value window-maint
ON LEAVE OF t-zen-colours.red_value IN FRAME frame-maint /* Red */
DO:
  RUN display-colour.
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
{{&core}commonmaint.i &path = "{&core}{&srv}"}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load window-maint  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "mnt-colours.wrx":U ).
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
ELSE MESSAGE "mnt-colours.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
run display-colour in this-procedure.
run local-after-enable in this-procedure.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-colour window-maint 
PROCEDURE display-colour :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 h-handle:backcolor  =
        RGB-VALUE(INT(t-zen-colours.red_value:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                  INT(t-zen-colours.green_value:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                  INT(t-zen-colours.blue_value:SCREEN-VALUE IN FRAME {&FRAME-NAME})).


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
  RUN control_load.
  IF AVAILABLE t-zen-colours THEN 
    DISPLAY t-zen-colours.colour_value t-zen-colours.colour_name 
          t-zen-colours.red_value t-zen-colours.green_value 
          t-zen-colours.blue_value 
      WITH FRAME frame-maint.
  ENABLE br-maint btnSave btnColour RECT-7 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls window-maint 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    h-handle = chCtrlFrame:CSComboBox.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-after-enable window-maint 
PROCEDURE local-after-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   assign
      btnSave:sensitive in frame {&frame-name} = 
         (not lv-newmode and not lv-editmode)
      btnColour:sensitive in frame {&frame-name} =
         (lv-newmode or lv-editmode).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialise window-maint 
PROCEDURE Local-Initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
 RUN display-colour.

case pv-to:private-data:
/*     when 'order-maint.w' then run refresh in  pv-to (t-customer.cust-num).  */
/*     when 'sr-maint.w'    then run refresh in  pv-to.                        */
end case.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh window-maint 
PROCEDURE refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
message "here"
   view-as alert-box info buttons OK.
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

    if can-find(first b-{&table-name}
                where b-{&table-name}.colour_name = t-{&table-name}.colour_name:screen-value in frame {&frame-name}
                and b-{&table-name}.{&unique-key} ne t-{&table-name}.{&unique-key})then
    do:
        message msg(122,"Colour",t-{&table-name}.colour_name:screen-value,"","") view-as alert-box.
        return string(t-{&table-name}.colour_name:handle).
    end.

    if t-{&table-name}.colour_name:screen-value = "" then
    do:
        message msg(67,"A Colour Name","","","") view-as alert-box.
        return string(t-{&table-name}.colour_name:handle).
    end.

    run display-colour.

    return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

