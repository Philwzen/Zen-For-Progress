&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : {&core}tools/fixtableids.p
    Purpose     : Program to assign tableid values to all records

    Syntax      : run {&core}tools/fixtableids.p

    Description : Program to assign tableid values to all records

    Author(s)   : 
    Created     :
    Notes       : 04/27/06 CK  Fixed transaction scoping and table lock 
                               overflow problem
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

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

def stream op.
output stream op to 'logs/checktableids.log'.

pause 0 before-hide.

def var x as int no-undo.

do x = 1 to num-dbs:
    create alias dictdb for database value(ldbname(x)).
    run db-qry(ldbname(x)).
end.

output stream op close.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-db-qry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE db-qry Procedure 
PROCEDURE db-qry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def input param pv-db as char no-undo.
    put stream op unformatted '##Processing DB ' pv-db skip.
    Def Var h-qry   As Handle No-undo.
    Def Var b-data  As Handle No-undo.
    Def Var h-buff  As Handle No-undo.
    Def Var h-field As Handle No-undo.
    def var lv-where as char no-undo.
    If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().
    If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
    Create Query h-QrY.
    lv-where = pv-db + '._file'.
    Create Buffer b-data For Table lv-where.
    If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data). 
   
    
    lv-where = "FOR EACH " + pv-db + "._file where " + 
            pv-db + "._file._file-number > 0 and " + 
            pv-db + "._file._file-number < 32000 no-lock".

    h-QRY:QUERY-PREPARE(lv-where).
    h-QRY:QUERY-OPEN.
    h-QRY:GET-NEXT().
    do while not h-qry:query-off-end:
        h-field = b-data:buffer-field('_file-name').
        run table-qry(h-field:buffer-value).
        h-QRY:GET-NEXT().
    end.
    If Valid-handle(h-QrY) Then  
        DELETE OBJECT h-QrY no-error.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-table-qry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE table-qry Procedure 
PROCEDURE table-qry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def input param pv-table as char no-undo.
    Def Var h-qry   As Handle No-undo.
    Def Var b-data  As Handle No-undo.
    Def Var h-buff  As Handle No-undo.
    Def Var h-field As Handle No-undo.
    def var x       as int    no-undo.
    def var y       as int    no-undo.
    If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().
    If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
    Create Query h-QrY.
    Create Buffer b-data For Table pv-table.
    If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data). 
    b-data:disable-load-triggers(false).

    h-QRY:QUERY-PREPARE("FOR EACH dictdb." + pv-table + ' where ' + pv-table + 'tableid = ? no-lock').
    h-QRY:QUERY-OPEN.
    h-qry:get-last().
    if not h-qry:query-off-end then do:
       put stream op unformatted 'Table ' pv-table.
       put stream op  ' bad'.
       put stream op skip.
    end.
    If Valid-handle(h-QrY) Then    
        DELETE OBJECT h-QrY no-error.  
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

