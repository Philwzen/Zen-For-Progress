&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME window-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/*****************************************************************************
PROGRAM NAME:   codeclient/login.w
DESCRIPTION:    
NOTES:          
*****************************************************************************/
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
&glob nobuttons
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
&Scoped-Define ENABLED-OBJECTS IMAGE-1 lv-user lv-pass Btn_ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lv-user lv-pass 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "icons/login.bmp":U TRANSPARENT
     SIZE 12 BY 2.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     lv-user AT ROW 1.71 COL 24.2
     lv-pass AT ROW 3 COL 21.8 PASSWORD-FIELD 
     Btn_ok AT ROW 4.76 COL 21.8
     btn-cancel AT ROW 4.76 COL 41
     IMAGE-1 AT ROW 2.43 COL 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 71.8 BY 6.14
         .


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
         TITLE              = "Rexpert Login"
         HEIGHT             = 6.14
         WIDTH              = 61.6
         MAX-HEIGHT         = 34.33
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.33
         VIRTUAL-WIDTH      = 204.8
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
    CleanSession(). /* in zenlibrary.p */  
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

    if not ValidUser(lv-user,lv-pass) then do: /* in zenlibrary.p */
        Lv-tries = lv-tries + 1.
/*         sound("nocando"). */
/*message msg(21,'Username','Password','','') view-as alert-box error .*/
        message 'Invalid Entry' view-as alert-box error .
        if lv-tries > 2 then quit.
        apply 'entry' to lv-user.
        return no-apply.
    end.
    else do:
      do x = 1 to num-entries(lv-deldef):
         lv-delstring = 'del/q ' + session:temp-directory + entry(x,lv-deldef) + '*'.
         os-command silent value(lv-delstring).
         lv-delstring = 'del/q ' + entry(x,lv-deldef) + '*'.
         os-command silent value(lv-delstring).
      end.
      if getctrl('{&AutoKeyGen}') ne 'true'
      then setctrl('{&AutoKeyGen}','true').
        {&window-name}:hidden = true.
        {&window-name}:private-data = entry(1,session:parameter,'^').
       /* initlibraries(). /* in zenlibrary.p */ */
/*         refreshtemptables().  */
        setusrid(lv-user). /* in zenlibrary.p */
        setsysvar('user',lv-user). /* in zenlibrary.p */
                initlibraries(). /* in zenlibrary.p */

/* if stringtolog(getfield(lv-user,zen-duser,sysman)) = true. */ 
      /* in generallibrary.p */
        if Getctrl('{&AuditSystemStatus}') ne 'on' and /* in zenlibrary.p */
           UserGroup(lv-user) = 'system' /* in zenlibrary.p */
        then do:
         message 'Auditing is Currently Disabled'
           view-as alert-box information.
        end.

        lv-propath = Getctrl('{&SystemPropath}'). /* in zenlibrary.p */
        if lv-propath ne ''
        then do:
           if not propath begins lv-propath 
            then propath  = lv-propath + ',' + propath.
        end.
        setsysvar('Login',string(today,'99/99/9999') + '-' + string(time,'hh:mm')).
        setsysvar('system',Getctrl('{&systemname}')). /* in zenlibrary.p */         
        setsysvar('language',string(UserLanguage(lv-user))). /* in zenlibrary.p */
        setsysvar('UserGroup',usergroup(lv-user)).           
        setsysvar('country',string(UserCountry(lv-user))). /* in zenlibrary.p */
       /* main-bar-handle = widget-handle(getsysvar("top-window")). */
         /* in zenlibrary.p */
        lv-mainmenuprog = GetCtrl('{&MainMenuProgram}'). /* in zenlibrary.p */
        if lv-mainmenuprog = ""  then lv-mainmenuprog = 
            "codeclient/mainmenu.w".
        if search(lv-MainMenuProg) = ? 
            then do:
/*message msg(50,'MainMenu',lv-mainmenuprog,' ',' ')*/
                message 'MainMenu not Found ' lv-mainmenuprog
                    view-as alert-box.
                {&window-name}:hidden = false.
                return no-apply.
            end.
          /* make sure a needed value in the registry exists or Word will cause
             an error */
          run {&client}{&account}wordRegUpdate.p.
        main-bar-handle = runchild(lv-MainMenuProg,this-procedure). 
            /* in generallibrary.p */
        SetSysVar("top-window",string(main-bar-handle)). /* in zenlibrary.p */
        if UserAutoTimeclock(lv-user) then do:
            run DoTimeclock in this-procedure (lv-user,'login').
        end.
    end.  
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
  {{&core}wid-chk.i}
  {{&core}focus.i}.
session:data-entry-return = true.
    run initialise.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    wait-for close of this-procedure.
END.

cleansession(). /* in zenlibrary.p */

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


{{&core}run.i &program   = "z-checkfile.p"
              &path      = "{&server}{&tables}"
              &Appsrv    = "System"
              &procedure = "SetTimeClock"
              &params    = "(pv-user,
                             pv-action,
                             today,
                             time)"}

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
  ENABLE IMAGE-1 lv-user lv-pass Btn_ok btn-cancel 
      WITH FRAME frame-maint IN WINDOW window-maint.
  {&OPEN-BROWSERS-IN-QUERY-frame-maint}
  VIEW window-maint.
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


    CleanSession(). /* in zenlibrary.p */ 

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
        DEFINE VARIABLE nr                       AS INTEGER NO-UNDO INITIAL 100.
        DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
        
        
        lv-user = FILL(" ",nr).
        
        RUN GetUserName{&A} (INPUT-OUTPUT lv-user,
                            INPUT-OUTPUT nr,
                            OUTPUT ReturnValue).
        
        case lv-user: /* bit of a bodge but ok for dev */
            when 'guiadmin'      then lv-user:screen-value in frame {&frame-name} 
                                                           = "adisney".
            when 'pwhite'        then lv-user:screen-value = 'phil'.
            when 'gvtpstan'      then lv-user:screen-value = 'gvtddisn'.
            when "administrator" then lv-user:screen-value = "root". 
            when "mpgadmin"      then lv-user:screen-value = "root".
            otherwise                 lv-user:screen-value = lv-user.
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

