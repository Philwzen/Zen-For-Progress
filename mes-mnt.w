&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-Mesfil NO-UNDO LIKE Zen-Mesfil.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
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

CREATE WIDGET-POOL.
&glob title-text Message Maintenance
&glob table-name zen-mesfil
&glob unique-key    {&table-name}TableId
&glob KeepRefreshButton
&Glob ImmediateQuery

/* ***************************  Definitions  ************************** */
{app-paths.i}
&glob nosortonsave
&glob NoBrowseSorting
/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
def var lv-lang-id    as int        no-undo.
DEF VAR z-language AS CHAR NO-UNDO.

def var lv-sysman     as  log no-undo.
lv-sysman = systemmanager(GetUserID()).

&glob nohelp
&glob noprint
&glob noaudit

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
&Scoped-define INTERNAL-TABLES t-zen-mesfil

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-mesfil.mesnum ~
t-zen-mesfil.mestxt 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-mesfil ~
      WHERE t-zen-mesfil.mestxt matches vc-search ~
and t-zen-mesfil.mesnum >= vi-msg-no NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-mesfil ~
      WHERE t-zen-mesfil.mestxt matches vc-search ~
and t-zen-mesfil.mesnum >= vi-msg-no NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-mesfil
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-mesfil


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb-lang vc-search vi-msg-no br-maint ~
rs-copy-opt b-copy cb-from-cntry rs-copy-records vi-copy-msg cb-to-cntry ~
b-now 
&Scoped-Define DISPLAYED-FIELDS t-Zen-Mesfil.mesnum t-Zen-Mesfil.sysrecord ~
t-Zen-Mesfil.mestxt 
&Scoped-define DISPLAYED-TABLES t-Zen-Mesfil
&Scoped-define FIRST-DISPLAYED-TABLE t-Zen-Mesfil
&Scoped-Define DISPLAYED-OBJECTS cb-lang vc-search vi-msg-no cb-language ~
rs-copy-opt cb-from-cntry rs-copy-records vi-copy-msg cb-to-cntry 

/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-Zen-Mesfil.mesnum cb-language ~
t-Zen-Mesfil.sysrecord t-Zen-Mesfil.mestxt 
&Scoped-define Edit-List cb-language t-Zen-Mesfil.sysrecord ~
t-Zen-Mesfil.mestxt 

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
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE cb-language AS CHARACTER FORMAT "X(256)":U 
     LABEL "Language" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cb-to-cntry AS CHARACTER FORMAT "X(256)":U 
     LABEL "To" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE vc-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE vi-copy-msg AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE vi-msg-no AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Msg No" 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY 1 NO-UNDO.

DEFINE VARIABLE rs-copy-opt AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Current Message", 1,
"Message Number", 2,
"All Messages", 3
     SIZE 21.6 BY 3 NO-UNDO.

DEFINE VARIABLE rs-copy-records AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Leave Existing", 1,
"Overwrite Any", 2
     SIZE 18.6 BY 2 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-mesfil SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-mesfil.mesnum
      t-zen-mesfil.mestxt
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 97.2 BY 8.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     cb-lang AT ROW 1.1 COL 22 COLON-ALIGNED
     vc-search AT ROW 1.1 COL 60 COLON-ALIGNED
     vi-msg-no AT ROW 1.1 COL 102 COLON-ALIGNED
     br-maint AT ROW 2.19 COL 12 HELP
          "Select the record to edit."
     t-Zen-Mesfil.mesnum AT ROW 10.76 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     cb-language AT ROW 10.76 COL 53 COLON-ALIGNED
     t-Zen-Mesfil.sysrecord AT ROW 10.86 COL 83.4
          VIEW-AS TOGGLE-BOX
          SIZE 18.6 BY 1
     t-Zen-Mesfil.mestxt AT ROW 11.95 COL 29 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 83 BY 3.33
     rs-copy-opt AT ROW 15.29 COL 20.4 NO-LABEL
     b-copy AT ROW 15.76 COL 5
     cb-from-cntry AT ROW 15.76 COL 65 COLON-ALIGNED
     rs-copy-records AT ROW 15.91 COL 92 NO-LABEL
     vi-copy-msg AT ROW 16.24 COL 45 COLON-ALIGNED NO-LABEL
     cb-to-cntry AT ROW 16.71 COL 65 COLON-ALIGNED
     b-now AT ROW 16.95 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 115.8 BY 18.48
         TITLE "Messages Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-Zen-Mesfil T "?" NO-UNDO schadm Zen-Mesfil
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Browse Maintenance"
         COLUMN             = 138.8
         ROW                = 8.33
         HEIGHT             = 27.05
         WIDTH              = 115.8
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
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint vi-msg-no frame-maint */
/* SETTINGS FOR COMBO-BOX cb-language IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Mesfil.mesnum IN FRAME frame-maint
   NO-ENABLE 1                                                          */
/* SETTINGS FOR EDITOR t-Zen-Mesfil.mestxt IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX t-Zen-Mesfil.sysrecord IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-mesfil"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _Where[1]         = "Temp-Tables.t-zen-mesfil.mestxt matches vc-search
and Temp-Tables.t-zen-mesfil.mesnum >= vi-msg-no"
     _FldNameList[1]   = Temp-Tables.t-zen-mesfil.mesnum
     _FldNameList[2]   = Temp-Tables.t-zen-mesfil.mestxt
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
   ASSIGN b-now:SENSITIVE = YES
          rs-copy-opt:SENSITIVE = YES
          cb-from-cntry:SENSITIVE = YES
          cb-to-cntry:SENSITIVE = YES
          rs-copy-records:SENSITIVE = YES.
   APPLY 'value-changed' TO rs-copy-opt IN FRAME {&FRAME-NAME}.

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
          vi-copy-msg
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
   
   DEF VAR vl-ok AS LOG NO-UNDO.
   MESSAGE msg(34,'','','','') VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vl-ok.
    
   IF vl-ok THEN
   DO:
      {{&core}run.i &program   = "zen-mesfil.p"
                   &path      = "{&core}{&srv}"
                   &Appsrv    = "System"
                   &procedure = "copy-messages"
                   &params    = "(rs-copy-opt,vi-copy-msg,
                                  vi-from-country,
                                  vi-to-country,
                                  rs-copy-records)"}
   END.

   RUN openquery.
   RUN disp-wids.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-lang
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-lang window-maint
ON VALUE-CHANGED OF cb-lang IN FRAME frame-maint /* Language */
DO:
    lv-lang-id = int(getcombokey(self:handle)).

    run openquery.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-copy-opt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-copy-opt window-maint
ON VALUE-CHANGED OF rs-copy-opt IN FRAME frame-maint
DO:
   IF rs-copy-opt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '1' THEN
   ASSIGN vi-copy-msg:SCREEN-VALUE = IF AVAIL t-{&table-name} THEN t-{&table-name}.mesnum:SCREEN-VALUE
   ELSE '0'.
   
   IF rs-copy-opt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2' THEN
   ASSIGN vi-copy-msg:SENSITIVE = YES
          vi-copy-msg:SCREEN-VALUE = '0'.
   ELSE ASSIGN vi-copy-msg:SENSITIVE = NO.

   IF rs-copy-opt:SCREEN-VALUE ='3' THEN
   ASSIGN vi-copy-msg:SCREEN-VALUE = '0'.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vc-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vc-search window-maint
ON ANY-KEY OF vc-search IN FRAME frame-maint /* Search */
DO:
   APPLY LASTKEY.
   ASSIGN vc-search = vc-search:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
   RUN queryopen.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi-msg-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi-msg-no window-maint
ON ANY-KEY OF vi-msg-no IN FRAME frame-maint /* Msg No */
DO:
  APPLY LASTKEY.
  ASSIGN vi-msg-no = int(vi-msg-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  RUN queryopen.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*******************
    DONT FORGET TO CHECK DIR LOCATION FOR BELOW INCLUDE !!!!!!!
********************/
{{&core}commonmaint.i &path = "{&core}{&srv}" &extraparams = "lv-lang-id,"}

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
    t-{&table-name}.SysRecord:hidden in frame {&frame-name} = not lv-sysman .
    APPLY 'entry' TO vc-search IN FRAME {&FRAME-NAME}.
    ASSIGN vc-search:SCREEN-VALUE = '*'
           vi-msg-no:SCREEN-VALUE = '0'.
           
    
    ASSIGN cb-language:SCREEN-VALUE IN FRAME {&FRAME-NAME} = setcombovalue(string(t-{&TABLE-name}.lan_lanid),cb-language:HANDLE)
           b-now:SENSITIVE = NO
           rs-copy-opt:SENSITIVE = NO
           vi-copy-msg:SENSITIVE = NO
           cb-from-cntry:SENSITIVE = NO
           cb-to-cntry:SENSITIVE = NO
           rs-copy-records:SENSITIVE = NO.

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
  DISPLAY cb-lang vc-search vi-msg-no cb-language rs-copy-opt cb-from-cntry 
          rs-copy-records vi-copy-msg cb-to-cntry 
      WITH FRAME frame-maint.
  IF AVAILABLE t-Zen-Mesfil THEN 
    DISPLAY t-Zen-Mesfil.mesnum t-Zen-Mesfil.sysrecord t-Zen-Mesfil.mestxt 
      WITH FRAME frame-maint.
  ENABLE cb-lang vc-search vi-msg-no br-maint rs-copy-opt b-copy cb-from-cntry 
         rs-copy-records vi-copy-msg cb-to-cntry b-now 
      WITH FRAME frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
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
    ASSIGN z-language = getsysvar('{&clv}language').

    run pop-combos in this-procedure no-error.
    ASSIGN vc-search = '*'
           cb-lang:SCREEN-VALUE IN FRAME {&FRAME-NAME} = setcombovalue(z-language,cb-lang:HANDLE).
    
    APPLY 'value-changed' TO cb-lang.

  /*  RETURN 'override'.
    */       
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-print-trigger window-maint 
PROCEDURE local-print-trigger :
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
       cb-language:private-data IN FRAME {&FRAME-NAME} = lv-keys
       cb-language:list-items   = lv-list
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

ASSIGN t-{&table-name}.lan_lanid = int(getcombokey(cb-language:HANDLE IN FRAME {&FRAME-NAME})).
   return "passed".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE queryopen window-maint 
PROCEDURE queryopen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {&OPEN-QUERY-br-maint}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen window-maint 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  def var v-recid as recid no-undo. 
  def buffer b-{&table-name} for t-{&table-name}.

  IF NEW t-{&table-name} THEN  
    IF CAN-FIND(FIRST b-{&table-name}
        WHERE b-{&table-name}.mesnum = int(t-{&table-name}.mesnum:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN
    DO:   
        MESSAGE msg(122,"Message",STRING(t-{&table-name}.mesnum:SCREEN-VALUE),"","") VIEW-AS ALERT-BOX.
        RETURN string(t-{&table-name}.mesnum:HANDLE IN FRAME {&FRAME-NAME}).
    END.
    
    return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

