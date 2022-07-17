&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-Zen-Country NO-UNDO LIKE Zen-Country.



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
&glob title-text    Country Maint
&glob table-name    zen-country
&glob unique-key    {&table-name}TableId
&glob KeepRefreshButton
&Glob ImmediateQuery

/* ***************************  Definitions  ************************** */
{app-paths.i}
&glob noimmediatequery
/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */

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
&Scoped-define INTERNAL-TABLES t-Zen-Country

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-Zen-Country.description ~
t-Zen-Country.Country 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-Zen-Country NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-Zen-Country NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-Zen-Country
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-Zen-Country


/* Definitions for FRAME frame-maint                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 br-maint 
&Scoped-Define DISPLAYED-FIELDS t-Zen-Country.country ~
t-Zen-Country.description t-Zen-Country.logo 
&Scoped-define DISPLAYED-TABLES t-Zen-Country
&Scoped-define FIRST-DISPLAYED-TABLE t-Zen-Country
&Scoped-Define DISPLAYED-OBJECTS cb-currency 

/* Custom List Definitions                                              */
/* Add-List,Edit-List,List-3,List-4,List-5,List-6                       */
&Scoped-define Add-List t-Zen-Country.country t-Zen-Country.description ~
t-Zen-Country.logo cb-currency 
&Scoped-define Edit-List t-Zen-Country.description t-Zen-Country.logo ~
cb-currency 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cb-currency AS CHARACTER FORMAT "X(256)":U 
     LABEL "Currency" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 44 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61.8 BY 5.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-Zen-Country SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-Zen-Country.description WIDTH 37
      t-Zen-Country.Country COLUMN-LABEL "Code" FORMAT ">>9":U
            WIDTH 7.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 49.8 BY 13.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     br-maint AT ROW 1.43 COL 13.2 HELP
          "Select the record to edit."
     t-Zen-Country.country AT ROW 15.1 COL 16.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     t-Zen-Country.description AT ROW 16.1 COL 16.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     t-Zen-Country.logo AT ROW 17.1 COL 16.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     cb-currency AT ROW 18.14 COL 16.8 COLON-ALIGNED
     RECT-7 AT ROW 14.57 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 65.4 BY 20.14
         TITLE "Country Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-Zen-Country T "?" NO-UNDO schadm Zen-Country
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
         COLUMN             = 84.8
         ROW                = 6.95
         HEIGHT             = 20.14
         WIDTH              = 65.4
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
/* BROWSE-TAB br-maint RECT-7 frame-maint */
/* SETTINGS FOR COMBO-BOX cb-currency IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Country.country IN FRAME frame-maint
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-Zen-Country.description IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN t-Zen-Country.logo IN FRAME frame-maint
   NO-ENABLE 1 2                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-Zen-Country"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-Zen-Country.description
"t-Zen-Country.description" ? ? "character" ? ? ? ? ? ? no ? no no "37" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-Zen-Country.Country
"t-Zen-Country.Country" "Code" ">>9" "integer" ? ? ? ? ? ? no ? no no "7.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME br-maint

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*******************
    DONT FORGET TO CHECK DIR LOCATION FOR BELOW INCLUDE !!!!!!!
********************/
{{&core}commonmaint.i &path = "{&core}{&srv}"}  
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
IF lv-newmode 
THEN do:
    cb-currency:screen-value in frame {&FRAME-NAME} = entry(1,cb-currency:list-items).
    APPLY "ENTRY" TO t-{&table-name}.Country.
end.
else cb-currency:screen-value = setcombovalue(t-{&table-name}.default-currency,cb-currency:HANDLE).

 
    
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
  DISPLAY cb-currency 
      WITH FRAME frame-maint.
  IF AVAILABLE t-Zen-Country THEN 
    DISPLAY t-Zen-Country.country t-Zen-Country.description t-Zen-Country.logo 
      WITH FRAME frame-maint.
  ENABLE RECT-7 br-maint 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise window-maint 
PROCEDURE local-initialise :
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

ASSIGN lv-keys = ClassCodes('{&currency}',output lv-list).
ASSIGN cb-currency:PRIVATE-DATA IN FRAME {&FRAME-NAME} = lv-keys
       cb-currency:LIST-ITEMS   = lv-list.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-save window-maint 
PROCEDURE pre-save :
t-{&table-name}.default-currency = GetComboKey(cb-currency:handle in frame {&FRAME-NAME}).

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


    if t-{&table-name}.country:screen-value in frame {&frame-name} = "" or
       t-{&table-name}.country:screen-value = "0" 
   then do:
        message msg(67,"A Country Code","","","") view-as alert-box.
        return string(t-{&table-name}.country:handle).
    end.

    if t-{&table-name}.description:screen-value = "" 
    then do:
        message msg(67,"A Description","","","") view-as alert-box.
        return string(t-{&table-name}.description:handle).
    end.

    if cb-currency:screen-value = AltLanguage("{&none-text}") 
    then do:
        message msg(67,"A Currency","","","") view-as alert-box.
        return string(cb-currency:handle).
    end.

    return 'passed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

