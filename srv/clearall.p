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

/* ***************************  Definitions  ************************** */
def stream log.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AppserverInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AppserverInfo Procedure 
PROCEDURE AppserverInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param pv-sys as char no-undo.

pv-sys = 'Opsys:   ' + opsys + ',' + 
         'Prover:  ' + Proversion + ',' + 
         'DbVer:   ' + Dbversion(1) + ',' + 
         'Num Dbs: ' + string(num-dbs) + ',' +
         'Product: ' + progress + ',' + 
	   'Propath: ' + propath .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearAllProcs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearAllProcs Procedure 
PROCEDURE ClearAllProcs :
def var h as handle no-undo.
h = session:first-procedure.

/*
logmessage(h:name,'','ClearAllProcs').
*/
output stream log to '{&logs}test.log'.
      do while valid-handle(h):   
            if h ne this-procedure and
            not h:private-data begins "LIBRARY"
                then do:
put stream log unformatted h:name skip.
                  delete procedure h.
                end.
            h = h:next-sibling.
        end.
output stream log close.
delete procedure this-procedure.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastServerRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LastServerRunning Procedure 
PROCEDURE LastServerRunning :
def output param pv-last as log    no-undo.
def output param pv-connectstring as char   no-undo.
def var h-appserver      as handle no-undo.
def var lv-service       as char   no-undo.
def var lv-startup       as char   no-undo.
def var x                as int    no-undo.

/* lv-startup = session:startup-parameters.        */
/* do x = 1 to num-entries(lv-startup):            */
/*    if entry(x,lv-startup) begins '-appservice'  */
/*     then do:                                    */
/*       lv-service = entry(x,lv-startup).         */
/*       leave.                                    */
/*    end.                                         */
/* end.                                            */

pv-connectstring = '-H localhost -N tcp -S 5162 ' + lv-service.

CREATE SERVER h-appserver.
IF valid-handle(h-appserver)
THEN DO:
  h-appserver:CONNECT(pv-connectstring,'','',"") No-error.
  If Error-status:error or not h-appserver:CONNECTed()
  Then h-appserver = ?.
End.
ELSE h-appserver = ?.
pv-last = not valid-handle(h-appserver).

if h-appserver:CONNECTed() then h-appserver:disconnect().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SessionParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SessionParams Procedure 
PROCEDURE SessionParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param pv-value as char no-undo.
def var x as int no-undo.

pv-value = session:startup-parameters + ',[Propath],' + propath + ',[End Propath]'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UnloadProcs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnloadProcs Procedure 
PROCEDURE UnloadProcs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-list as char no-undo.


def var h as handle no-undo.
h = session:first-procedure.

output stream log to '{&logs}test.log'.
      do while valid-handle(h):   
            if can-do(pv-list,h:name)
                then do:
                  put stream log unformatted h:name skip.
                  delete procedure h.
                end.
            h = h:next-sibling.
        end.
output stream log close.
delete procedure this-procedure.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WhatsRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WhatsRunning Procedure 
PROCEDURE WhatsRunning :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param pv-list as char no-undo.

def var h as handle no-undo.
h = session:first-procedure.

do while valid-handle(h):   
  pv-list = pv-list + ',' + h:name.
  h = h:next-sibling.
end.
  pv-list = substring(pv-list,2).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

