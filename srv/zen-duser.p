&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
&glob serverprogram true
/* ***************************  Definitions  ************************** */
{app-paths.i}

/* CHANGEABLE BITS HERE change table-name to appropriate db table */
&glob Table-name zen-duser
&glob Unique-key {&table-name}tableid 

{{&core}def-table.i &table-name = {&table-name}}

define query q-{&table-name} FOR {&table-name} scrolling.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 24.67
         WIDTH              = 50.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}mainp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Change-Password) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Change-Password Procedure 
PROCEDURE Change-Password :
def input param pv-user     as char no-undo.
def input param lv-pass-new as char no-undo.

def var lv-user as char no-undo.
lv-user = GetSysVar("user").
find _user where _user._userid = lv-user exclusive-lock no-error.

 _user._password = encode(lv-pass-new).
  /* setsysvar("password",lv-pass-new).*/
if not setuserid(lv-user,lv-pass-new,ldbname(1)) then do:
   release _user.        
end.
else do:
    find zen-duser where zen-duser.duser = pv-user exclusive-lock no-error.
    zen-duser.expiry = today + 30.
end.    
release zen-duser.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckProcSecurity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckProcSecurity Procedure 
PROCEDURE CheckProcSecurity :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-item    as char no-undo.
def output param pv-can-run as log  no-undo init true.

def var lv-user as char no-undo.
lv-user = GetSysVar("user").
        
    find zen-duser where zen-duser.duser = lv-user
                   no-lock no-error.
                                       
    find zen-dpgm where zen-dpgm.pgm = pv-item
                  no-lock no-error.
                  
    if avail zen-dpgm and 
       avail zen-duser then do:
          if can-do(zen-dpgm.not-users,lv-user) or
             can-do(zen-dpgm.not-group,zen-duser.U-GROUP) 
          then pv-can-run = false.
          else if not can-do(zen-dpgm.run-users,lv-user) or
                  not can-do(zen-dpgm.run-groups,zen-duser.U-GROUP)
                  then pv-can-run = false.
   end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkuser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkuser Procedure 
PROCEDURE checkuser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param  pv-user as char no-undo.
def input param  pv-pass as char no-undo.
def output param pv-ok   as log  no-undo.

def var lv-canread  as char no-undo.
def var lv-canwrite as char no-undo.

pv-ok = setuserid(pv-user,pv-pass,ldbname(1)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clear-table) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-table Procedure 
PROCEDURE clear-table :
for each t-{&table-name}:
    delete t-{&table-name}.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copy-users) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-users Procedure 
PROCEDURE copy-users :
def input param vc-userid    AS CHAR NO-UNDO.
def input param vc-password  AS CHAR NO-UNDO.
def input param vl-multiple  AS LOG  NO-UNDO.
def input param vi-number    AS INT  NO-UNDO.
def input param vc-user      AS CHAR NO-UNDO.
def input param TABLE        FOR t-zen-duser.

def buffer b-user    for zen-duser.
DEFINE VAR vd-id  AS DEC NO-UNDO.
DEFINE VAR vi-inc AS INT NO-UNDO.

FIND FIRST t-zen-duser WHERE t-zen-duser.duser = vc-user NO-LOCK NO-ERROR.

IF not vl-multiple then vi-number = 1.

vi-inc = 1.
DO vi-inc = 1 TO vi-number:
    FIND FIRST b-user WHERE b-user.duser = vc-userid + STRING(vi-inc) 
                      NO-LOCK NO-ERROR.
    IF NOT AVAIL b-user 
    THEN DO:                            
        CREATE zen-duser.
        BUFFER-COPY t-zen-duser EXCEPT duser dpassword {&table-name}tableid 
                      TO zen-duser.
    
        ASSIGN zen-duser.duser     = vc-userid + STRING(vi-inc)
               zen-duser.dpassword = vc-password. 
    
        find _user where _user._userid = zen-duser.duser
                   exclusive-lock no-error.
           
        if not avail _user 
        then do:    
            create _user.
            assign _user._userid    = zen-duser.duser
                   _user._password  = encode(zen-duser.dpassword)
                   _user._user-name = t-zen-duser.user-name.
        end.
        ELSE ASSIGN _user._user-name = t-zen-duser.user-name.

        release zen-duser.
        release _user.
    END. /* doesn't exist - can copy */
    ELSE vi-number = vi-number + 1.
END. /* loop for number of users they wish to create */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-record Procedure 
PROCEDURE delete-record :
/* USES MANDATORY PARAMETERS
def input param pv-recid as recid no-undo.
def input-output param table for t-{&table-name}.
*/
{{&core}apsrv-deleterecord.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Delete-Related-Tables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete-Related-Tables Procedure 
PROCEDURE Delete-Related-Tables :
/*------------------------------------------------------------------------------
  Purpose: delete any child tables of this parent
  Parameters:  <none>
  Notes:   leave blank if not required    
------------------------------------------------------------------------------*/
/*   for each useraccess WHERE useraccess.duser = zen-duser.duser exclusive-lock: */
/*     delete useraccess.                                                         */
/*   end.                                                                         */
/*        
                                                                        */

&if defined(no_user) = 0 &then
  find _user where _user._userid = zen-duser.duser
        exclusive-lock no-error.
  if avail _user then 
      delete _user.
&endif
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-validation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-validation Procedure 
PROCEDURE delete-validation :
/*------------------------------------------------------------------------------
  Purpose:     check ok to delete record
  Parameters:  <none>
  Notes:       return 'passed' if it's OK to delete.
------------------------------------------------------------------------------*/
/* example 
    if can-find(first ledger-item of product) then return 'Failed'.
*/
    Return 'passed'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-find-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-record Procedure 
PROCEDURE find-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Def Input Param pv-code As char No-undo.

{{&core}apsrv-findrecord.i 
        &where = "where zen-duser.duser = pv-code"}
eND PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-records) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-records Procedure 
PROCEDURE get-records :
/* USES MANDATORY PARAMETERS YOU NEED NOT WORRY ONLY 
Def Input  Param pv-direction As Char  No-undo.
def Input  param pv-rowid     as recid no-undo.
def output param table for t-{&table-name}.

   can use &whereclause as where clause and &by for sorting
      e.g.  &where = "where {&table-name}.class = pv-class"       
     &by    = "by {&table-name}.name"                                                   
********************************************************/
/*                                            */
/* DEF INPUT PARAM vc-user  AS CHAR NO-UNDO.  */
/* DEF INPUT PARAM vc-group AS CHAR NO-UNDO.  */

{{&core}apsrv-getrecords.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUserCountry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetUserCountry Procedure 
PROCEDURE GetUserCountry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input  param pv-user  as char no-undo.
def output param pv-lanid as CHAR no-undo.

/* z-system = GetSysVar("system").  */
{{&core}find.i &table = "zen-duser"
              &where = "where zen-duser.duser = pv-user"
              &type  = "first"              
              &lock  = "no"}
            
if avail zen-duser 
    then pv-lanid = string(zen-duser.country).
    else pv-lanid = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUserGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetUserGroup Procedure 
PROCEDURE GetUserGroup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-user  as char no-undo.
def output param pv-group as char no-undo.

{{&core}find.i &table = "zen-duser"
            &where = "where zen-duser.duser = pv-user"
            &type  = "first"              
            &lock  = "no"}
            
if avail zen-duser 
    then pv-group = zen-duser.U-GROUP.
    else pv-group = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUserLanguage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetUserLanguage Procedure 
PROCEDURE GetUserLanguage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-user  as char no-undo.
def output param pv-lanid as int no-undo.

{{&core}find.i &table = "zen-duser"
              &where = "where zen-duser.duser = pv-user"
              &type  = "first"              
              &lock  = "no"}
            
if avail zen-duser 
    then pv-lanid = zen-duser.lan_lanid.
    else pv-lanid = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUserName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetUserName Procedure 
PROCEDURE GetUserName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input  param pv-user as char no-undo.
def output param pv-name as CHAR no-undo.

{{&core}find.i &table = "zen-duser"
              &where = "where zen-duser.duser = pv-user"
              &type  = "first"              
              &lock  = "no"}
            
if avail zen-duser 
    then pv-name = zen-duser.user-name.
    else pv-name = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsSystemManager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IsSystemManager Procedure 
PROCEDURE IsSystemManager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-user as char no-undo.
def output param pv-ok   as log  no-undo.

{{&core}find.i &table = "zen-duser"
              &where = "where zen-duser.duser = pv-user"
              &type  = "first"              
              &lock  = "no"}
            
if avail zen-duser 
    then pv-ok = zen-duser.sys-man.
    else pv-ok = no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ListGroups) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ListGroups Procedure 
PROCEDURE ListGroups :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param pv-Groups as char no-undo.

def var lv-group as char no-undo.
for each zen-duser
    no-lock
    break by zen-duser.u-group:
    if first-of(zen-duser.u-group)
        then pv-groups = pv-groups + zen-duser.u-group + '{&combodelim}'.
end.
pv-groups = substring(pv-groups,1,length(pv-groups) - 1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ListUsers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ListUsers Procedure 
PROCEDURE ListUsers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def output param pv-users       as char     init ""     no-undo.


    for each zen-duser no-lock:
        assign pv-users = pv-users + zen-duser.duser + '{&ComboDelim}'.
    end.

    pv-users = substring(pv-users,1,length(pv-users) - 1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Procedure 
PROCEDURE open-query :
/* USES MANDATORY PARAMETERS
     output table  for t-{&table-name}.
      returns ALL records matching the &where clause
   can use &whereclause as where clause and &by for sorting
      e.g.  &where = "where {&table-name}.class = pv-class"       
     &by    = "by {&table-name}.name"   
********************************************************/
{{&core}apsrv-openquery.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create Procedure 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:    just a hook incase we need it
  Parameters:  <none>
  Notes:   called after the buffer copy from temp-table to db.
------------------------------------------------------------------------------*/
&if defined(no_user) = 0 &then
find _user where _user._userid = t-zen-duser.duser
           exclusive-lock no-error.
           
if not avail _user 
    then do:    
        create _user.
        assign _user._userid    = t-zen-duser.duser
               _user._password  = encode(t-zen-duser.dpassword).
    end.
    
_user._user-name = t-zen-duser.user-name .
release _user.
&endif
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-update) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update Procedure 
PROCEDURE post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&if defined(no_user) = 0 &then
find _user where _user._userid = t-zen-duser.duser
               exclusive-lock no-error.

if t-zen-duser.duser ne userid(ldbname(1))
    then delete _user.    

if not avail _user 
    then do:
        create _user.      
        _user._userid    = t-zen-duser.duser.
    End.
assign
       _user._password  = encode(t-zen-duser.dpassword)
       _user._user-name = t-zen-duser.user-name.
       
release _user.
&endif
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pre-save) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pre-save Procedure 
PROCEDURE Pre-save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   return "passed".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-save-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE save-record Procedure 
PROCEDURE save-record :
/* uses MANDATORY PARAMETERS
def input param pv-recid as recid no-undo. 
def input-output param table for t-{&table-name}.
*/
{{&core}apsrv-saverecord.i }
release zen-duser no-error.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetInitSysVars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetInitSysVars Procedure 
PROCEDURE SetInitSysVars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-user as char no-undo.
setsysvar('Login',string(today,'99/99/9999') + '-' + string(time,'hh:mm')).
setsysvar('system',Getctrl('{&systemname}')).
setsysvar('language',string(UserLanguage(pv-user))).
setsysvar('UserGroup',usergroup(pv-user)).
setsysvar('country',string(UserCountry(pv-user))).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

