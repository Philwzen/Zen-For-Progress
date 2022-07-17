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
&glob NoButtons
&glob AutoStart

def var lv-startprog as char no-undo.
def var lv-email as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frame-maint

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lv-txt 

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
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE VARIABLE lv-txt AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-maint
     btn-cancel AT ROW 5.05 COL 19
     lv-txt AT ROW 2.67 COL 2 NO-LABEL WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 56 BY 5.48.


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
         HEIGHT             = 5.52
         WIDTH              = 56.2
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
/* SETTINGS FOR FILL-IN lv-txt IN FRAME frame-maint
   NO-ENABLE ALIGN-L                                                    */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
output to value("{&logs}" + getFieldWhere("entopt","true","entity-code") +
   "-" + string(today,"99-99-99") + "-autorun.log") append.
put string(time,"HH:MM:SS") " START autorun.w" skip.
output close.

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
/*  {{&core}sec-chk.i} */
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
  DISPLAY lv-txt 
      WITH FRAME frame-maint IN WINDOW window-maint.
  ENABLE btn-cancel 
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

    CheckForBackGroundErrors(widget-handle(pv-from),lv-email).

    CleanSession(). /* in zenlibrary.p */ 
    
    LogAction(program-name(1),'','quit').

output to value("{&logs}" + getFieldWhere("entopt","true","entity-code") +
   "-" + string(today,"99-99-99") + "-autorun.log") append.
put string(time,"HH:MM:SS") " END autorun.w" skip.
output close.

    quit.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise window-maint 
PROCEDURE local-initialise :
def var x as int no-undo.
def var lv-propath as char no-undo.
def var lv-deldef as char no-undo init 'dbi,srt,lbi,protrace,procore,*.dct,*.ped,*.qs,*.ab'.
def var lv-delstring as char no-undo.
def var lv-session      as char no-undo.
def var lv-practice     as char no-undo.
def var lv-user as char no-undo.
def var lv-pass as char no-undo.
def var lv-system    as char             no-undo init '1'.



       current-window:hidden = true.
/* use current windows username */
    lv-user = WapiGetUserName().
setsysvar('user',lv-user).
    case lv-user: /* bit of a bodge but ok for dev */
        when 'guiadmin' then lv-user = "adisney".
        when 'pwhite'   then  lv-user = 'phil'.
        when "administrator" then lv-user = "root". 
        otherwise lv-user = lv-user.
    end case.
    lv-pass = getfield('s-user','signonid',lv-user,'usrpword').

/* or set youre user and password here 
    assign
        lv-user = 'adisney' 
        lv-pass = ''.
*/

      do x = 1 to num-entries(lv-deldef):
         lv-delstring = 'del/q ' + session:temp-directory + entry(x,lv-deldef) + '*'.
         os-command silent value(lv-delstring).
         lv-delstring = 'del/q ' + entry(x,lv-deldef) + '*'.
         os-command silent value(lv-delstring).
      end.

      if getctrl('{&AutoKeyGen}') ne 'true'
      then setctrl('{&AutoKeyGen}','true').

     {&window-name}:private-data = entry(1,session:parameter,'^').
     initlibraries(lv-user). /* in zenlibrary.p */
     setusrid(lv-user). /* in zenlibrary.p */
     setsysvar('user',lv-user). /* in zenlibrary.p */
     setsysvar('top-win',string(current-window)).
     lv-propath = Getctrl('{&SystemPropath}'). /* in zenlibrary.p */
     if lv-propath ne ''
     then do:
        if not propath begins lv-propath 
         then propath  = lv-propath + ',' + propath.
     end.
     RefreshTempTables().
     setsysvar('Login',string(today,'99/99/9999') + '-' + string(time,'hh:mm')).
     setsysvar('system',Getctrl('{&systemname}')). /* in zenlibrary.p */         
     setsysvar('language',string(UserLanguage(lv-user))). /* in zenlibrary.p */
     setsysvar('country',string(UserCountry(lv-user))). /* in zenlibrary.p */

      assign lv-practice = getfield('s-user','signonid',lv-user,'def-practice')
             lv-session = entry(1,session:parameter,'^').

      setSysVar('acct-no',""). /* zenlibrary.p */
      
      /* run youre program here */
      /* if we dont want to use the startprog idea then just do 
       lv-startprog = 'youre program' . */
      /* use local-update-child-=procedures to pass in params */ 
      lv-startprog = getfield('s-user','signonid',lv-user,'ext1[2]').

      if lv-startprog begins '**' then do:
         message 'Program not valid ' skip
                  lv-startprog
         view-as alert-box.
         quit.
      end.
      lv-email = getfield('s-user','signonid',lv-user,'e-mail').
      lv-txt:screen-value in frame {&frame-name} = lv-startprog.
      if lv-startprog ne '' then
         runchild (lv-startprog,this-procedure).
     {&WINDOW-NAME}:hidden = true.
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
     when lv-startprog then run autostart in pv-to /* ('params here') */ no-error.
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

