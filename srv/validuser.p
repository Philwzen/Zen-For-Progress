&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
&glob serverprogram true
/* ***************************  Definitions  ************************** */

{app-paths.i}

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}mainp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CheckUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckUser Procedure 
PROCEDURE CheckUser :
def input param  pv-user as char no-undo.
def input param  pv-pass as char no-undo.
def output param pv-ok   as log  no-undo.

pv-ok = setuserid(pv-user,pv-pass,ldbname(1)).  


  
/* def var lv-session as char no-undo.                                            */
/* lv-session = trim(sessionid()).                                                */
/* if pv-ok                                                                       */
/* then do:                                                                       */
/*     find first Zen-Context where Zen-Context.server-connection-id = lv-session */
/*                              and Zen-Context.type = 'password'                 */
/*                              exclusive-lock no-error.                          */
/*     if not avail Zen-Context                                                   */
/*       then create Zen-Context.                                                 */
/*                                                                                */
/*     Assign Zen-Context.server-connection-id = lv-session                       */
/*            Zen-Context.type  = 'password'                                      */
/*            zen-Context.contextvalue = pv-pass.                                 */
/*     release Zen-Context.                                                       */
/* end.                                                                           */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-no_user) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE no_user Procedure 
PROCEDURE no_user :
def input param  pv-user as char no-undo.
def input param  pv-pass as char no-undo.
def output param pv-ok   as log  no-undo init no.
/* not using _user so use zen-duser instead */
pv-ok = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

