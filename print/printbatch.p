&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-QueJob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QueJob Procedure 
PROCEDURE QueJob :
def input param pv-path as char no-undo. /* path to .p */
def input param pv-prog as char no-undo. /* .p name */
def input param pv-proc as char no-undo. /* internal procedure name */
def input param pv-printerparams as char   no-undo.
def var lv-task-no as int no-undo.
/* this is the form of pv-printerparams
pv-printerparams = string(t-printer.printertableid) + '{&Delim2}' +
                         t-printer.printer-name + '{&Delim2}' +
                         string(lv-copies) + '{&Delim2}' +
                         string(lv-tray) + '{&Delim2}' +
                         lv-filename + '{&Delim2}' + 
                         string(lv-batch) + '{&Delim2}'.
*/
def input param pv-dpgmparams as char no-undo. /* dpgm table paramesters normally blank */
def output param table-handle lv-printtable.
def input param pv-extras as char no-undo. /* ant extra stuff we might want to pass across */

/* do what ever you have to here to que job on batch system */

{{&core}run.i &program   = "zen-task.p"
             &path      = "{&tsk}{&srv}"
             &Appsrv    = "System"
             &noper     = true
             &procedure = "submit-task"
             &params    = "(pv-path + pv-prog,
                            pv-proc,
                            entry(7,pv-printerparams,'{&Delim2}'), 
                            today,
                            time,
                            '', /* quick schedule */
                            '{&treport}', /* task type */
                            pv-printerparams + '{&ComboDelim}' + pv-dpgmparams + '{&ComboDelim}' + pv-extras, /* parameters */
                            output lv-task-no)"} 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

