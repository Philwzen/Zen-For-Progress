&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME window-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                              */
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
/* 12/19/06 ADIS,LMA   default administrator to root                          */ 
/******************************************************************************/
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{app-paths.i}
&undefine suppresswindow
&glob NoImmediateQuery
&glob nobuttons
&glob login
def var lv-tries as int no-undo.
DEF VAR main-bar-handle AS HANDLE.
def var lv-mainmenuprog as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frame-maint

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-user lv-pass Btn_ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lv-user lv-pass 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckNewVersion window-maint 
FUNCTION CheckNewVersion RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSystemVars window-maint 
FUNCTION SetSystemVars RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "E&xit" 
     SIZE 13 BY 1.29
     BGCOLOR 8 .

DEFINE BUTTON Btn_ok 
     LABEL "&OK" 
     SIZE 13 BY 1.29
     BGCOLOR 8 .

DEFINE VARIABLE lv-pass AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lv-user AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "../zen/grafix/fiji.jpg":U
     SIZE 28 BY 3.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     lv-user AT ROW 1.71 COL 33.6
     lv-pass AT ROW 3 COL 31.6 BLANK 
     Btn_ok AT ROW 4.33 COL 32
     btn-cancel AT ROW 4.33 COL 58
     IMAGE-1 AT ROW 1 COL 1 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 71.8 BY 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Login"
         HEIGHT             = 5.05
         WIDTH              = 71.6
         MAX-HEIGHT         = 54.91
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 54.91
         VIRTUAL-WIDTH      = 384
         SHOW-IN-TASKBAR    = no
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW window-maint
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frame-maint
   FRAME-NAME                                                           */
/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME frame-maint
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-pass IN FRAME frame-maint
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN lv-user IN FRAME frame-maint
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(window-maint)
THEN window-maint:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-maint
/* Query rebuild information for FRAME frame-maint
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel window-maint
ON CHOOSE OF btn-cancel IN FRAME frame-maint /* Exit */
DO:
    CleanSession().   
    quit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ok window-maint
ON CHOOSE OF Btn_ok IN FRAME frame-maint /* OK */
DO:
    def var x as int no-undo.
    def var lv-propath as char no-undo.
    def var lv-deldef as char no-undo init 'dbi,srt,lbi,protrace,procore,*.dct,*.ped,*.qs,*.ab'.
    def var lv-delstring as char no-undo.
    assign
        lv-user = lc(trim(lv-user:screen-value)) 
        lv-pass = lc(trim(lv-pass:screen-value)).
    sysmsg('Validating').
    if not ValidUser(lv-user,lv-pass) then do:
        Lv-tries = lv-tries + 1.
/*         sound("nocando"). */
        message 'Invalid Username or Password' view-as alert-box error.
        if lv-tries > 2 then quit.
        apply 'entry' to lv-user.
        return no-apply.
    end.
    else
do:
        do x = 1 to num-entries(lv-deldef):
            lv-delstring = 'del/q ' + session:temp-directory + entry(x,lv-deldef) + '*'.
            os-command silent value(lv-delstring).
            lv-delstring = 'del/q ' + entry(x,lv-deldef) + '*'.
            os-command silent value(lv-delstring).
        end.
        CheckNewVersion().
        {&window-name}:hidden = true.
        {&window-name}:private-data = entry(1,session:parameter,'^').
        setusrid(lv-user). 
        initlibraries(lv-user).
        sysmsg('Initialising').
        setsysvar("User=" + lv-user +
                  ",Login=" + string(today,'99/99/9999') + '-' + string(time,'hh:mm') + 
                  ",system=" + Getctrl('{&systemname}') +
                  ",language=" + string(UserLanguage(lv-user)) + 
                  ",UserGroup=" + usergroup(lv-user) + 
                  ",country=" + string(UserCountry(lv-user))
                  ,'').        
        lv-mainmenuprog = GetCtrl('{&MainMenuProgram}'). 
        if lv-mainmenuprog = ""  
        then lv-mainmenuprog =  "{&core}mainmenu.w".
        if filenotfound(lv-MainMenuProg)
        then do:
            message 'MainMenu ' lv-mainmenuprog 'not found' skip
                    'Use Default?'
            view-as alert-box buttons yes-no update lv-ok as log.
            {&window-name}:hidden = false.
            if not lv-ok 
                then return no-apply.
                else lv-mainmenuprog =  "{&core}mainmenu.w".
        end.
        sysmsg('Loading Main Screen').
        main-bar-handle = runchild(lv-MainMenuProg,this-procedure). 
        sysmsg('Initialising Main Screen').
        /* setsysvar("{&clv}top-window",string(main-bar-handle)). */

        if Getctrl('{&AuditSystemStatus}') ne 'on' and 
           UserGroup(lv-user) = 'system' 
        then do:
         message 'Auditing is Currently Disabled'
           view-as alert-box information.
        end.
    end.
    session:immediate-display = true.
    sysmsg('off').  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-pass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-pass window-maint
ON RETURN OF lv-pass IN FRAME frame-maint /* Password */
DO:
  apply 'choose' to Btn_ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{{&core}commonmaint.i &path = "{&sys}{&srv}"}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
/*   {{&core}sec-chk.i} */
   run enable_UI.
/*   {{&core}wid-chk.i} */
  {{&core}focus.i}.
session:data-entry-return = true.
    run initialise.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    wait-for close of this-procedure.
END.

cleansession().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(window-maint)
  THEN DELETE WIDGET window-maint.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoTimeclock window-maint 
PROCEDURE DoTimeclock :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-user   as char no-undo.
def input param pv-action as char no-undo.

/*    */
/* {{&core}run.i &program   = "z-checkfile.p" */
/*               &path      = "{&server}{&tables}" */
/*               &Appsrv    = "System" */
/*               &procedure = "SetTimeClock" */
/*               &params    = "(pv-user, */
/*                              pv-action, */
/*                              today, */
/*                              time)"} */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DownloadNewVersion window-maint 
PROCEDURE DownloadNewVersion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-sname as char no-undo.

 return 'passed'.
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
  DISPLAY lv-user lv-pass 
      WITH FRAME frame-maint IN WINDOW window-maint.
  ENABLE lv-user lv-pass Btn_ok btn-cancel 
      WITH FRAME frame-maint IN WINDOW window-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-afterinitialise window-maint 
PROCEDURE local-afterinitialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-childreturn window-maint 
PROCEDURE Local-childreturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-from as char no-undo.

if UserAutoTimeclock(lv-user) then do:
   run DoTimeclock in this-procedure (lv-user,'logout').
end.


    CleanSession().  

LogAction(program-name(1),'','quit').
    quit.

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
/*         lv-user = 'available'. */
/*         setusrid(lv-user). */
/*         setsysvar('user',lv-user). */
/*         initlibraries(). */

    
     
    lv-user = WapiGetUserName().
    
    case lv-user: /* bit of a bodge but ok for dev */
        when 'myaso' then lv-user:screen-value in frame {&frame-name} = 'phil'.
        when "User"  then lv-user:screen-value = "Phil". 
        otherwise    lv-user:screen-value = lv-user.
    end case.
    
    apply "entry" to lv-pass in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-update-child-procedures window-maint 
PROCEDURE Local-update-child-procedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-to as handle no-undo.

case pv-to:private-data:
     when lv-mainmenuprog then run refresh in pv-to (lv-user).
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckNewVersion window-maint 
FUNCTION CheckNewVersion RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

def var lv-sname  as char no-undo.
def var lv-sver   as char no-undo.
def var lv-csver  as char no-undo.
def var lv-zver   as char no-undo.
def var lv-czver  as char no-undo.

  {{&core}run.i &program   = "zen-control.p"
                &path      = "{&core}{&srv}"
                &Appsrv    = "System"  
                &nomess    = 'true'
                &procedure = "getversions"
                &params    = "(output lv-sname,
                               output lv-sver,
                               output lv-zver)"}
assign
    lv-csver = GetIniValue('InstalledVersion',lv-sname)
    lv-czver = GetIniValue('InstalledVersion','Zen').

if lv-sver ne lv-csver or
   lv-zver ne lv-czver 
then do:
    message 'System ' lv-sname ' Installed : ' lv-csver ' Current : ' lv-sver skip
            'Core   ' '     Installed : ' lv-czver ' Current : ' lv-zver skip
            'Download New Version Now?' 
    VIEW-AS ALERT-BOX QUESTION  
    buttons yes-no update lv-ok as log.
    if lv-ok 
    then do:
        run DownloadNewVersion in this-procedure (lv-sname) .
        if return-value = 'passed' then do:
            SetIniValue('InstalledVersion',lv-sname,lv-sver).
            SetIniValue('InstalledVersion','Zen',lv-zver).
            message 'Update Complete' skip
                    'Please Logon again' skip
            view-as alert-box Information.
        end.
        else message 'Problem Updating Youre Software' skip
                      return-value skip
                     'Please Contact Suport' skip
             view-as alert-box error.
        CleanSession().
        Quit.
    end.
end.

  RETURN LV-SVER + ',' +
         LV-CSVER + ':' +  
         lv-zver + ',' +
         lv-czver.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSystemVars window-maint 
FUNCTION SetSystemVars RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
{{&core}run.i &program   = "zen-duser.p"
             &path      = "{&core}{&srv}"
             &Appsrv    = "System"  
             &noper     = true
             &procedure = "SetInitSysVars"
             &params    = "(lv-user)"}
             
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

