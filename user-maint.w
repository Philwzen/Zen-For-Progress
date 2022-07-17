&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-duser NO-UNDO LIKE zen-duser.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/*****************************************************************************/
/*  PROGRAM ID.     : user-maint.w                                           */
/*  PROGRAM TITLE   : User Maintenance                                       */
/*  CREATE DATE     : 01/14/2005                                             */
/*  COMPANY NAME    :                                                        */
/*****************************************************************************/
/*                          PATCH HISTORY                                    */
/*                                                                           */
/* DATE      User ID  DESCRIPTION                                            */
/* ------------------------------------------------------------------------- */
/* 01/14/05  Philw    Initial Release                                        */
/*****************************************************************************/

/*
1.  Add temptable in uib .
2.  Set table-name to the tablename!!!
3.  define query and browse on the temp table.
4.  Drop temptable field on to frames one - six.
5.  Put field in frame one in list-1 frame two in list-2 etc.
6.  Field should be displayed not enabled.
7.  Validation logic is in validate-screen.
8.  See commonmaint.i for event points.
9.  Create a .p with same name as table.
10. Set table-name in .p.
11. Thats it.
*/

create widget-pool.

/* ****************************  PreProcessors  **************************** */
{app-paths.i}
&glob tabs       true
&glob title-text User Maintenance
&glob table-name zen-duser
&glob unique-key {&table-name}TableId
&glob ImmediateQuery
&glob suppresswindow
&glob useDBbuttons
/*
&glob bug               /* turn on debug mesasges */
&glob tree              /* turn on procedure tree listing */
&glob NoChangedCheck    /* disable changed onleave check */
&glob NoExitCheck       /* disble exit question */
&glob NoButtons         /* no buttons displayed */
&glob NoNew             /* no new button */
&glob NoEdit            /* no edit button */
&glob NoSave            /* no save button */
&glob NoDelete          /* no delete button */
&glob NoExport          /* no export button */
&glob NoUndo            /* no undo button */
&glob NoQuery           /* no query button */
&glob NoAudit           /* no audit button */
&glob NoExit            /* no exit button */
&glob NoHelp            /* no help button */
&glob NoPrint           /* no print button */
&glob NoImmediateQuery  /* do not openquery */
&glob ImmediateQuery    /* force open query */
&glob NotFoundMessage   /* no record not found message */
*/

/* ****************************  Definitions  ****************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME a-frame-maint
&Scoped-define BROWSE-NAME br-maint

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-zen-duser

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-duser.duser ~
t-zen-duser.user-name t-zen-duser.u-group t-zen-duser.sys-man 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-duser NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-duser NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-duser
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-duser


/* Definitions for FRAME a-frame-maint                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint btn-prop 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cb-country cb-lang t-zen-duser.duser user-copyFrom ~
t-zen-duser.dpassword cb-group t-zen-duser.sys-man t-zen-duser.expiry ~
t-zen-duser.user-name 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-prop 
     LABEL "Properties":L 
     SIZE 15.8 BY 1.1.

DEFINE VARIABLE cb-country AS CHARACTER FORMAT "X(256)":U 
     LABEL "Country" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE cb-group AS CHARACTER FORMAT "X(256)":U 
     LABEL "User Group" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE cb-lang AS CHARACTER FORMAT "X(256)":U 
     LABEL "Language" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE user-copyFrom AS CHARACTER FORMAT "X(25)":U 
     LABEL "Copy From" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 TOOLTIP "Enter an existing user from which to copy." NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-duser SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-duser.duser
      t-zen-duser.user-name
      t-zen-duser.u-group
      t-zen-duser.sys-man
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 140 BY 8.14 FIT-LAST-COLUMN TOOLTIP "Select a record to edit.".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME a-frame-maint
     br-maint AT ROW 1.43 COL 13 HELP
          "Select the record to edit."
     btn-prop AT ROW 8.14 COL 154 HELP
          "Widgets" WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 177.4 BY 20.33
         TITLE "User Maintenance".

DEFINE FRAME two
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 84 ROW 13
         SIZE 23 BY 6.33.

DEFINE FRAME three
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 81 ROW 13
         SIZE 25 BY 6.81.

DEFINE FRAME four
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 80 ROW 12.95
         SIZE 24 BY 6.86.

DEFINE FRAME five
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 85 ROW 13
         SIZE 20 BY 6.57.

DEFINE FRAME six
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 86 ROW 13.57
         SIZE 15 BY 5.57.

DEFINE FRAME one
     cb-country AT ROW 3.38 COL 99 COLON-ALIGNED WIDGET-ID 16
     cb-lang AT ROW 4.57 COL 99 COLON-ALIGNED WIDGET-ID 18
     t-zen-duser.duser AT ROW 1.19 COL 28 COLON-ALIGNED HELP
          ""
          LABEL "User ID"
          VIEW-AS FILL-IN 
          SIZE 53 BY 1
     user-copyFrom AT ROW 1.19 COL 98.4 COLON-ALIGNED WIDGET-ID 8
     t-zen-duser.dpassword AT ROW 3.38 COL 28 COLON-ALIGNED
          LABEL "Password"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1 TOOLTIP "Enter the user's password; it should be the same as the user's login password."
     cb-group AT ROW 5.86 COL 28 COLON-ALIGNED
     t-zen-duser.sys-man AT ROW 5.86 COL 101
          LABEL "Allow this user to be a system manager"
          VIEW-AS TOGGLE-BOX
          SIZE 53 BY 1 TOOLTIP "Check this box to allow this user to access system-level functions."
     t-zen-duser.expiry AT ROW 4.57 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1 TOOLTIP "Enter the last date the user can log into Rexpert."
     t-zen-duser.user-name AT ROW 2.19 COL 28 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 53 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14 ROW 11.71
         SIZE 160.2 BY 6.86.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-duser T "?" NO-UNDO dive zen-duser
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
         HEIGHT             = 29.62
         WIDTH              = 189.4
         MAX-HEIGHT         = 45.33
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.33
         VIRTUAL-WIDTH      = 256
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
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
/* REPARENT FRAME */
ASSIGN FRAME five:FRAME = FRAME a-frame-maint:HANDLE
       FRAME four:FRAME = FRAME a-frame-maint:HANDLE
       FRAME one:FRAME = FRAME a-frame-maint:HANDLE
       FRAME six:FRAME = FRAME a-frame-maint:HANDLE
       FRAME three:FRAME = FRAME a-frame-maint:HANDLE
       FRAME two:FRAME = FRAME a-frame-maint:HANDLE.

/* SETTINGS FOR FRAME a-frame-maint
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint 1 a-frame-maint */
ASSIGN 
       FRAME a-frame-maint:HIDDEN           = TRUE
       FRAME a-frame-maint:PRIVATE-DATA     = 
                "6".

ASSIGN 
       br-maint:ALLOW-COLUMN-SEARCHING IN FRAME a-frame-maint = TRUE.

/* SETTINGS FOR FRAME five
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME five:HIDDEN           = TRUE
       FRAME five:PRIVATE-DATA     = 
                "5".

/* SETTINGS FOR FRAME four
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME four:HIDDEN           = TRUE
       FRAME four:PRIVATE-DATA     = 
                "4".

/* SETTINGS FOR FRAME one
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME one:HIDDEN           = TRUE
       FRAME one:PRIVATE-DATA     = 
                "1".

/* SETTINGS FOR COMBO-BOX cb-country IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX cb-group IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX cb-lang IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-duser.dpassword IN FRAME one
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN t-zen-duser.duser IN FRAME one
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR FILL-IN t-zen-duser.expiry IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX t-zen-duser.sys-man IN FRAME one
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN user-copyFrom IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN t-zen-duser.user-name IN FRAME one
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FRAME six
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME six:HIDDEN           = TRUE
       FRAME six:PRIVATE-DATA     = 
                "6".

/* SETTINGS FOR FRAME three
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME three:HIDDEN           = TRUE
       FRAME three:PRIVATE-DATA     = 
                "3".

/* SETTINGS FOR FRAME two
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME two:HIDDEN           = TRUE
       FRAME two:PRIVATE-DATA     = 
                "2".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME a-frame-maint
/* Query rebuild information for FRAME a-frame-maint
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME a-frame-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-duser"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   = Temp-Tables.t-zen-duser.duser
     _FldNameList[2]   = Temp-Tables.t-zen-duser.user-name
     _FldNameList[3]   = Temp-Tables.t-zen-duser.u-group
     _FldNameList[4]   = Temp-Tables.t-zen-duser.sys-man
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME five
/* Query rebuild information for FRAME five
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME five */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME four
/* Query rebuild information for FRAME four
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME four */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME one
/* Query rebuild information for FRAME one
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME one */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME six
/* Query rebuild information for FRAME six
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME six */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME three
/* Query rebuild information for FRAME three
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME three */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME two
/* Query rebuild information for FRAME two
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME two */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME a-frame-maint:HANDLE
       ROW             = 9.81
       COLUMN          = 13
       HEIGHT          = 10.24
       WIDTH           = 162
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      CtrlFrame:MOVE-AFTER(btn-prop:HANDLE IN FRAME a-frame-maint).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-prop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-prop window-maint
ON CHOOSE OF btn-prop IN FRAME a-frame-maint /* Properties */
DO:
    if avail {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} 
        then
        runchild("{&core}property-mnt.w",this-procedure).
    else
       message 'Please Select a Program First' {&mess-disp-type}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME one
&Scoped-define SELF-NAME t-zen-duser.dpassword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-zen-duser.dpassword window-maint
ON LEAVE OF t-zen-duser.dpassword IN FRAME one /* Password */
DO:
   /* check password */
   if t-zen-duser.dpassword <> self:screen-value then do:
      /* check the password */
      run {&core}confirm-pass.w.

      /* bad password was entered */
      if self:SCREEN-VALUE <> getsysvar('{&clv}confirmpassword') then do:
         /* tell the user it was a bad password */
         message msg(75,'','','','')
            view-as alert-box error.

         self:screen-value = ''.
         return no-apply.
      end. /* invalid - do not match */
   end. /* password changed */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME user-copyFrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL user-copyFrom window-maint
ON LEAVE OF user-copyFrom IN FRAME one /* Copy From */
DO:
   def var hold-code as char  no-undo.

   define buffer tt-{&table-name} for t-{&table-name}.

   /* don't execute this logic if user hit undo or exit button */
      /* in generallibrary.p */
   if donotfire('btn-exit,btn-undo') then return.
   
   if user-CopyFrom:screen-value ne "" then do:
      /* find record user wants to copy from */
      find tt-{&table-name} where
         tt-{&table-name}.duser = user-CopyFrom:screen-value no-error.
      if not avail tt-{&table-name} then do:
         message  "Enter a valid user to copy from."
            view-as alert-box info buttons OK.
         return no-apply.
      end. /* invalid location */
      else do:
         assign user-CopyFrom.

         find tt-zen-duser where
            tt-zen-duser.duser = tt-{&table-name}.duser no-error.
      end. /* found s-user */

      /* copy from old record everything except main code field and 
         tableID record.  */
      hold-code = t-zen-duser.duser:screen-value in frame one.
      buffer-copy tt-{&table-name} except tt-{&table-name}.duser
         to t-{&table-name}.

      run display-fields. /* let user see copied info */

      assign
         user-CopyFrom:screen-value     = ""
         t-zen-duser.duser:screen-value = hold-code
         user-copyfrom:sensitive        = no
         user-copyfrom:hidden           = yes.

      /* in generallibrary.p */
      SetLkBut(frame one:handle,user-copyFrom:handle,no).
   end. /* user entered something in the field */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME a-frame-maint
&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{{&core}commonmaint.i &path = "{&core}{&srv}"}
                
    /* &extraparams = "'class'," */
                
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BEFORE-OPEN-query window-maint 
PROCEDURE BEFORE-OPEN-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{{&core}run.i &program   = "zen-duser.p"
             &path      = "{&core}{&srv}"
             &Appsrv    = "System"
             &procedure = "open-query"
             &params    = "(output table t-zen-duser)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

OCXFile = SEARCH( "user-maint.wrx":U ).
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
ELSE MESSAGE "user-maint.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-extra-tables window-maint 
PROCEDURE create-extra-tables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
create t-zen-duser.
create tb-zen-duser.
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
  HIDE FRAME a-frame-maint.
  HIDE FRAME five.
  HIDE FRAME four.
  HIDE FRAME one.
  HIDE FRAME six.
  HIDE FRAME three.
  HIDE FRAME two.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disp-wids window-maint 
PROCEDURE disp-wids :
assign
    cb-country:screen-value in frame one = setcombovalue(string(t-zen-duser.country),cb-country:handle)
    cb-lang:screen-value = setcombovalue(string(t-zen-duser.lan_lanid),cb-lang:handle)
    cb-group:screen-value = setcombovalue(t-zen-duser.u-group,cb-group:handle).
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
  ENABLE br-maint btn-prop 
      WITH FRAME a-frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-a-frame-maint}
  DISPLAY cb-country cb-lang user-copyFrom cb-group 
      WITH FRAME one.
  IF AVAILABLE t-zen-duser THEN 
    DISPLAY t-zen-duser.duser t-zen-duser.dpassword t-zen-duser.sys-man 
          t-zen-duser.expiry t-zen-duser.user-name 
      WITH FRAME one.
  {&OPEN-BROWSERS-IN-QUERY-one}
  {&OPEN-BROWSERS-IN-QUERY-four}
  {&OPEN-BROWSERS-IN-QUERY-three}
  {&OPEN-BROWSERS-IN-QUERY-two}
  {&OPEN-BROWSERS-IN-QUERY-five}
  {&OPEN-BROWSERS-IN-QUERY-six}
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
assign
    h-tab   = chctrlframe:tabstrip.

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
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-browse-off-home window-maint 
PROCEDURE local-browse-off-home :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
message program-name(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn window-maint 
PROCEDURE Local-ChildReturn :
/*------------------------------------------------------------------------------
  Purpose:  allow processing after a child screen exits.   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-from as char no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-clear-tables window-maint 
PROCEDURE local-clear-tables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
empty temp-table t-zen-duser.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-edit-trigger window-maint 
PROCEDURE local-edit-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   empty temp-table tb-zen-duser.
   create tb-zen-duser.
   buffer-copy t-zen-duser to tb-zen-duser.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable window-maint 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   if not lv-newmode then assign
      user-CopyFrom:sensitive in frame one = no
      user-CopyFrom:visible   in frame one = no.

   /* show/hide lookup button for "copy from" field */
   SetLkBut(frame one:handle,user-copyFrom:handle,lv-newmode). 
      /* in generallibrary.p */

 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise window-maint 
PROCEDURE local-initialise :
run pop-combos in this-procedure no-error.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Proc-FindRow window-maint 
PROCEDURE Local-Proc-FindRow :
def input param pv-browse as handle no-undo.
/* specialised version of proc-findrow to deal with 2 tables in browse. */

def var v-hqry    as handle no-undo.
def var v-hcolumn as handle no-undo.
def var lv-value  as char   no-undo format 'x(10)'.
def var lv-qry    as char   no-undo.
def var lv-where  as char   no-undo.
def var lv-sort   as char   no-undo.
def var h-db1      as handle no-undo.
def var h-db2      as handle no-undo.
Def Var h-qry     As Handle No-undo.
def var h-qry2    as handle no-undo.
def var x         as int    no-undo.

v-hcolumn = pv-browse:CURRENT-COLUMN.
if v-hcolumn = ? then return.

If Valid-handle(h-QrY2) Then h-QrY2:QUERY-CLOSE().
If Valid-handle(h-QrY2) Then DELETE OBJECT h-QrY2 no-error.
h-qry = pv-browse:query.

/* i know i know only one active wait-for 
   but update is quiclkest and easiest method */
update lv-value label "Search For" at 6 skip
  " Enter the search value or press Esc to cancel." at 5
   go-on (tab)
   with frame upd side-labels three-d  
   view-as dialog-box.
frame upd:title = v-hcolumn:table + " Quick Search".

Create Query h-QrY2.
create buffer h-db1 for table h-qry:get-buffer-handle(1).
h-QrY2:Add-buffer(h-db1).
create buffer h-db2 for table h-qry:get-buffer-handle(2).
h-QrY2:Add-buffer(h-db2).

do x = 1 to h-qry:num-buffers:
   if index(entry(x,h-qry:prepare-string,','),v-hcolumn:table) ne 0
      then do:
         lv-where = entry(x,h-qry:prepare-string,',').
         if index(lv-where,'where') ne 0 
            then lv-sort = ' and ' + v-hcolumn:name + " >= '" + lv-value + "'".
            else lv-sort = ' where ' + v-hcolumn:name + " >= '" + lv-value + "'".
         if index(lv-where,'outer-join') ne 0 
            then lv-where = replace(lv-where,'outer-join',lv-sort + ' outer-join').
            else lv-where = replace(lv-where,'no-lock',lv-sort + ' no-lock').
         lv-qry = lv-qry + lv-where.
      end.
   else lv-qry = lv-qry + entry(x,h-qry:prepare-string,',').

   if x ne h-qry:num-buffers then lv-qry = lv-qry + ','.
end.

h-QRY2:QUERY-PREPARE(lv-qry).
h-QRY2:QUERY-OPEN.
h-qry2:get-first.
h-qry:reposition-to-rowid(h-db1:rowid,h-db2:rowid). 

if pv-browse:name = "{&browse-name}" then
   run br-changed-trigger in this-procedure no-error.

return 'override'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-undo-trigger window-maint 
PROCEDURE local-undo-trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
delete tb-zen-duser.
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
 when "{&core}property-mnt.w"
      then run refresh in pv-to ('User',t-zen-duser.duser:screen-value in frame one).
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
DEF VAR lv-keys AS CHAR NO-UNDO.
DEF VAR lv-list AS CHAR NO-UNDO.


buildcombo(cb-country:handle in frame one,
           'zen-country',
           'country',
           'description',
           '','',no,no).
           
buildcombo(cb-lang:handle in frame one,
           'zen-language',
           'lan_lanid',
           'lan_description',
           '','',no,no).

lv-keys = ClassCodes('{&usergrp}',output lv-list).
assign cb-group:private-data in frame one = lv-keys
       cb-group:list-items   = lv-list.



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
def var i as int    no-undo.

assign
   t-zen-duser.country       = int(getcombokey(cb-country:handle in frame one))
   t-zen-duser.lan_lanid     = int(getcombokey(cb-lang:handle in frame one))
   t-zen-duser.u-group       = getcombokey(cb-group:handle in frame one).

if t-zen-duser.u-group = "" then t-zen-duser.u-group = "system".

   return "passed".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh window-maint 
PROCEDURE Refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    run openquery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen window-maint 
PROCEDURE validate-screen :
return 'passed'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

