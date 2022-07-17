&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-Lan_Field NO-UNDO LIKE Zen-Lan_Field.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     : ????????                                                */
/*  PROGRAM TITLE   : ????????                                                */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    : East India Trading LTD                                    */
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


CREATE WIDGET-POOL.
&glob title-text    Field Translations
&glob table-name    Zen-Lan_Field
&glob unique-key    {&table-name}TableId
&glob KeepRefreshButton
&Glob ImmediateQuery

/* ***************************  Definitions  ************************** */
{app-paths.i}

/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
def var lv-id         as int        no-undo.

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
&Scoped-define INTERNAL-TABLES t-Zen-Lan_Field

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-Zen-Lan_Field._Field-Name ~
t-Zen-Lan_Field.lan_altlabel t-Zen-Lan_Field.lan_init-value 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-Zen-Lan_Field ~
      WHERE if rs-by then t-Zen-Lan_Field.lan_altlabel MATCHES lv-string  ~
         else t-Zen-Lan_Field._field-name MATCHES lv-string  NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-Zen-Lan_Field ~
      WHERE if rs-by then t-Zen-Lan_Field.lan_altlabel MATCHES lv-string  ~
         else t-Zen-Lan_Field._field-name MATCHES lv-string  NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-Zen-Lan_Field
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-Zen-Lan_Field


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb-lang RECT-32 RECT-7 lv-string rs-by ~
br-maint cb-from-cntry b-copy rs-copy-opt rs-copy-records cb-to-cntry b-now ~
vl-blank-tran 
&Scoped-Define DISPLAYED-FIELDS t-Zen-Lan_Field._field-name ~
t-Zen-Lan_Field.lan_altlabel 
&Scoped-define DISPLAYED-TABLES t-Zen-Lan_Field
&Scoped-define FIRST-DISPLAYED-TABLE t-Zen-Lan_Field
&Scoped-Define DISPLAYED-OBJECTS cb-lang lv-string rs-by cb-from-cntry ~
rs-copy-opt rs-copy-records cb-to-cntry vl-blank-tran 

/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-Zen-Lan_Field._field-name ~
t-Zen-Lan_Field.lan_altlabel 
&Scoped-define Edit-List t-Zen-Lan_Field._field-name ~
t-Zen-Lan_Field.lan_altlabel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-copy 
     LABEL "Copy >>" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-now 
     LABEL "Now" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cb-from-cntry AS CHARACTER FORMAT "X(256)":U 
     LABEL "From" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cb-lang AS CHARACTER FORMAT "X(256)":U 
     LABEL "Language" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE cb-to-cntry AS CHARACTER FORMAT "X(256)":U 
     LABEL "To" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE lv-string AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Find" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE rs-by AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "English", no,
"Translation", yes
     SIZE 35.8 BY .81 NO-UNDO.

DEFINE VARIABLE rs-copy-opt AS LOGICAL 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Current Translation", no,
"All Translation", yes
     SIZE 23 BY 1.91 NO-UNDO.

DEFINE VARIABLE rs-copy-records AS LOGICAL 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Leave Existing Records", no,
"Overwrite Any", yes
     SIZE 26 BY 1.91 NO-UNDO.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 3.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 2.71.

DEFINE VARIABLE vl-blank-tran AS LOGICAL INITIAL yes 
     LABEL "Copy English Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.4 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-Zen-Lan_Field SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-Zen-Lan_Field._Field-Name COLUMN-LABEL "English" FORMAT "x(60)":U
            WIDTH 45
      t-Zen-Lan_Field.lan_altlabel COLUMN-LABEL "Translation" FORMAT "x(60)":U
            WIDTH 53.2
      t-Zen-Lan_Field.lan_init-value COLUMN-LABEL "Main Screen" FORMAT "X(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 128.8 BY 9.52 ROW-HEIGHT-CHARS .57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     cb-lang AT ROW 1.05 COL 23.2 COLON-ALIGNED
     lv-string AT ROW 2 COL 23.2 COLON-ALIGNED
     rs-by AT ROW 2.19 COL 65.2 NO-LABEL
     br-maint AT ROW 3.14 COL 13.2 HELP
          "Select the record to edit."
     t-Zen-Lan_Field._field-name AT ROW 13.52 COL 39 COLON-ALIGNED
          LABEL "English"
          VIEW-AS FILL-IN 
          SIZE 87 BY 1
     t-Zen-Lan_Field.lan_altlabel AT ROW 14.52 COL 39 COLON-ALIGNED
          LABEL "Alternate"
          VIEW-AS FILL-IN 
          SIZE 87 BY 1
     cb-from-cntry AT ROW 17.14 COL 73.8 COLON-ALIGNED
     b-copy AT ROW 17.19 COL 19
     rs-copy-opt AT ROW 17.19 COL 42.4 NO-LABEL
     rs-copy-records AT ROW 17.19 COL 105.2 NO-LABEL
     cb-to-cntry AT ROW 18.1 COL 73.8 COLON-ALIGNED
     b-now AT ROW 18.38 COL 19
     vl-blank-tran AT ROW 19.24 COL 42.4
     "Copy Translation" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 16.24 COL 19
     "Translation" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 12.86 COL 29
     RECT-32 AT ROW 16.48 COL 17
     RECT-7 AT ROW 13.14 COL 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143.2 BY 19.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: t-Zen-Lan_Field T "?" NO-UNDO schadm Zen-Lan_Field
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
         COLUMN             = 33.6
         ROW                = 6.76
         HEIGHT             = 24.71
         WIDTH              = 148.8
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
         FONT               = 9
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
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint rs-by frame-maint */
/* SETTINGS FOR FILL-IN t-Zen-Lan_Field.lan_altlabel IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
ASSIGN 
       lv-string:PRIVATE-DATA IN FRAME frame-maint     = 
                "{&Delim2}*".

/* SETTINGS FOR FILL-IN t-Zen-Lan_Field._field-name IN FRAME frame-maint
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-Zen-Lan_Field"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _Where[1]         = "if rs-by then Temp-Tables.t-Zen-Lan_Field.lan_altlabel MATCHES lv-string 
         else Temp-Tables.t-Zen-Lan_Field._field-name MATCHES lv-string "
     _FldNameList[1]   > Temp-Tables.t-Zen-Lan_Field._Field-Name
"t-Zen-Lan_Field._Field-Name" "English" "x(60)" "character" ? ? ? ? ? ? no ? no no "45" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-Zen-Lan_Field.lan_altlabel
"t-Zen-Lan_Field.lan_altlabel" "Translation" "x(60)" "character" ? ? ? ? ? ? no ? no no "53.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.t-Zen-Lan_Field.lan_init-value
"t-Zen-Lan_Field.lan_init-value" "Main Screen" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define SELF-NAME b-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-copy window-maint
ON CHOOSE OF b-copy IN FRAME frame-maint /* Copy >> */
DO:

   ASSIGN b-now:SENSITIVE           = YES
          rs-copy-opt:SENSITIVE     = YES
          cb-from-cntry:SENSITIVE   = YES
          cb-to-cntry:SENSITIVE     = YES
          rs-copy-records:SENSITIVE = YES
          vl-blank-tran:SENSITIVE   = YES.
          
   APPLY 'VALUE-CHANGED' TO rs-copy-opt.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-now
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-now window-maint
ON CHOOSE OF b-now IN FRAME frame-maint /* Now */
DO:

   DEF VAR vi-to-country   AS INT NO-UNDO.
   DEF VAR vi-from-country AS INT NO-UNDO.

   ASSIGN cb-from-cntry cb-to-cntry
          rs-copy-opt rs-copy-records
          vl-blank-tran
          vi-from-country = int(getcombokey(cb-from-cntry:HANDLE))
          vi-to-country   = int(getcombokey(cb-to-cntry:HANDLE)).

   IF cb-from-cntry = '' OR cb-to-cntry = '' THEN
   do:
      MESSAGE msg(40,'From and To Country','','','') VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   
   IF vi-from-country = vi-to-country THEN
   DO:
       MESSAGE msg(62,'Country','Country','','') VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
   END.
   
   IF NOT rs-copy-opt AND NOT AVAIL t-{&table-name} THEN
   do:
      MESSAGE msg(40,'translation','','','') VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.

   DEF VAR vl-ok AS LOG NO-UNDO.
   MESSAGE msg(34,'','','','') VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vl-ok.
    
   IF vl-ok THEN
   DO:
      {{&core}run.i &program   = "zen-lan_field.p"
                   &path      = "{&srv}"
                   &Appsrv    = "System"
                   &procedure = "copy-translations"
                   &params    = "(rs-copy-opt,
                                  t-{&table-name}._field-name,
                                  vl-blank-tran,
                                  vi-from-country,
                                  vi-to-country,
                                  rs-copy-records)"}
   END.

   RUN openquery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-lang
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-lang window-maint
ON VALUE-CHANGED OF cb-lang IN FRAME frame-maint /* Language */
DO:
    
  lv-id = int(GetCombokey(self:handle)).
  run openquery. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-string
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-string window-maint
ON ANY-KEY OF lv-string IN FRAME frame-maint /* Find */
DO:
  apply lastkey.
    assign rs-by
           lv-string.
  {&OPEN-QUERY-{&BROWSE-NAME}}         
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-by
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-by window-maint
ON VALUE-CHANGED OF rs-by IN FRAME frame-maint
DO:
  assign rs-by
         lv-string.
  {&OPEN-QUERY-{&BROWSE-NAME}}         
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
{{&core}commonmaint.i &path = "{&core}{&srv}" &extraparams = "lv-id,"}  

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
  APPLY "VALUE-CHANGED":U TO {&BROWSE-NAME} IN FRAME {&frame-name}.
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
    
   ASSIGN b-now:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
          rs-copy-opt:SENSITIVE = NO
          cb-from-cntry:SENSITIVE = NO
          cb-to-cntry:SENSITIVE = NO
          rs-copy-records:SENSITIVE = NO
          vl-blank-tran:SENSITIVE = NO.
    
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
  DISPLAY cb-lang lv-string rs-by cb-from-cntry rs-copy-opt rs-copy-records 
          cb-to-cntry vl-blank-tran 
      WITH FRAME frame-maint.
  IF AVAILABLE t-Zen-Lan_Field THEN 
    DISPLAY t-Zen-Lan_Field._field-name t-Zen-Lan_Field.lan_altlabel 
      WITH FRAME frame-maint.
  ENABLE cb-lang RECT-32 RECT-7 lv-string rs-by br-maint cb-from-cntry b-copy 
         rs-copy-opt rs-copy-records cb-to-cntry b-now vl-blank-tran 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-initialise window-maint 
PROCEDURE Local-initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run pop-combos in this-procedure no-error.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pop-combos window-maint 
PROCEDURE pop-combos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-list as char no-undo.
def var lv-keys as char no-undo.

lv-keys = ClassCodes('{&lang}',output lv-list).
assign cb-lang:private-data IN FRAME {&FRAME-NAME} = lv-keys
       cb-lang:list-items   = lv-list
       /*users language defaulted in here - cb-lang:SCREEN-VALUE IN FRAME {&FRAME-NAME} = setcombovalue('0',cb-language:HANDLE)*/
       cb-from-cntry:private-data IN FRAME {&FRAME-NAME} = lv-keys
       cb-from-cntry:list-items   = lv-list
       cb-to-cntry:private-data IN FRAME {&FRAME-NAME} = lv-keys
       cb-to-cntry:list-items   = lv-list.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-save window-maint 
PROCEDURE pre-save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 t-Zen-Lan_Field.lan_lanid = int(getcombokey(cb-lang:handle in frame {&frame-name})).
   return "passed".
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

   IF NEW t-{&table-name} THEN  
      IF CAN-FIND(FIRST b-{&table-name}
          WHERE b-{&table-name}._Field-Name = t-{&table-name}._Field-Name:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
      DO:   
          MESSAGE msg(122,"Translation",STRING(t-{&table-name}._Field-Name:SCREEN-VALUE),"","") VIEW-AS ALERT-BOX.
          RETURN string(t-{&table-name}._Field-Name:HANDLE IN FRAME {&FRAME-NAME}).
      END.
    if length(t-{&table-name}.lan_altlabel:screen-value) > length(t-{&table-name}._Field-Name:SCREEN-VALUE) then
      DO:   
          MESSAGE msg(998,'','',"","") VIEW-AS ALERT-BOX WARNING.
          /*RETURN string(t-{&table-name}.lan_altlabel:HANDLE IN FRAME {&FRAME-NAME}).
            ak taken out for germany 05/05/01 */
      END.

    
  
   return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

