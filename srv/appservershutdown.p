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
{app-paths.i justvars = true}
/*
{app-paths.i justvars = true}
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-IAmLastServerRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD IAmLastServerRunning Procedure 
FUNCTION IAmLastServerRunning RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NumberOfAppservers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NumberOfAppservers Procedure 
FUNCTION NumberOfAppservers RETURNS INTEGER
  ( pv-by as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

/*    run StopVtxMonitor.  */
    NumberOfAppservers(-1). 


/* def stream op. */
/* output stream op to 'appserveshutdown.log'. */
/* put stream op unformatted string(NumberOfAppservers(0)) skip */
/*     string(IAmLastServerRunning()) skip. */
    
if IAmLastServerRunning()
then do:
/*    run StopVtxMonitor. */
   run stoptaskservers.
   run ClearContext.
end.

/* output stream op close. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ClearContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearContext Procedure 
PROCEDURE ClearContext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each zen-context exclusive-lock:
    delete zen-context.
End.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StopTaskServers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StopTaskServers Procedure 
PROCEDURE StopTaskServers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each zen-tserver exclusive-lock:
   zen-tserver.started = no.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StopVtxMonitor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StopVtxMonitor Procedure 
PROCEDURE StopVtxMonitor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   find zen-control exclusive-lock where 
      zen-control.ctrl-idx = 'VtxMonitor' no-error.
   if avail zen-control then zen-control.ctrl-data = 'down'.
   find zen-control no-lock where 
      zen-control.ctrl-idx = 'VtxMonitor' no-error.

   find first zen-control exclusive-lock where 
      zen-control.ctrl-idx = "RunningAppservers" no-error.
   if avail zen-control then zen-control.ctrl-data = "0".
   find zen-control no-lock where 
      zen-control.ctrl-idx = 'RunningAppServers' no-error.

   output to '{&logs}stop.tag'. 
   put ''. 
   output close.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-IAmLastServerRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION IAmLastServerRunning Procedure 
FUNCTION IAmLastServerRunning RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/* def var lv-connectstring as char   no-undo.                      */
/* def var h-appserver      as handle no-undo.                      */
/* def var lv-good as log no-undo.                                  */
/* def var lv-service as char no-undo.                              */
/* def var lv-startup as char no-undo.                              */
/* def var x as int no-undo.                                        */
/*                                                                  */
/* lv-startup = session:startup-parameters.                         */
/* do x = 1 to num-entries(lv-startup):                             */
/*    if entry(x,lv-startup) begins '-Appservice'                   */
/*     then lv-service = entry(x,lv-startup).                       */
/* end.                                                             */
/*                                                                  */
/* lv-connectstring = '-H localhost -N tcp -S 5162 ' + lv-service.  */
/* lv-system = entry(4,session:parameter,'^').                                                                 */
/* CREATE SERVER h-appserver.                                       */
/* IF valid-handle(h-appserver)                                     */
/* THEN DO:                                                         */
/*   h-appserver:CONNECT(lv-connectstring,'','',"") No-error.       */
/*   If Error-status:error or not h-appserver:CONNECTed()           */
/*   Then h-appserver = ?.                                          */
/* End.                                                             */
/* ELSE h-appserver = ?.                                            */
/* lv-good = valid-handle(h-appserver).                             */
/* if h-appserver:CONNECTed() then h-appserver:disconnect().        */


  find first zen-control where zen-control.ctrl-idx = "RunningAppservers"
                         no-lock.
  RETURN int(zen-control.ctrl-data) le 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NumberOfAppservers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NumberOfAppservers Procedure 
FUNCTION NumberOfAppservers RETURNS INTEGER
  ( pv-by as int ) :

def var lv-num as int no-undo.

if can-find (first zen-control where zen-control.ctrl-idx = "RunningAppservers")
then do:
   find first zen-control where zen-control.ctrl-idx = "RunningAppservers"
                          exclusive-lock.
   lv-num = int(zen-control.ctrl-data).
end.
else do:
   create zen-control.
   zen-control.ctrl-idx = "RunningAppservers".
end.
  lv-num = lv-num + pv-by.
  if lv-num < 0 then lv-num = 0.
  zen-control.ctrl-data = string(lv-num).

  release zen-control.

  RETURN lv-num + pv-by.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

