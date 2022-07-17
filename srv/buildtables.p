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


{{&core}zen-temptables.i}

{{&sys}sys-temptables.i}

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
         HEIGHT             = 12.38
         WIDTH              = 40.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
{{&core}mainp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BuildSysTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildSysTables Procedure 
PROCEDURE BuildSysTables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildTables Procedure 
PROCEDURE BuildTables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-user as char no-undo.
def output param table for t-zen-lan_field.
def output param table for t-zen-dpgm.
def output param table for t-zen-dwidget.
def output param table for t-zen-control.
def output param table for t-zen-duser.
def output param table for t-zen-dmenu.
def output param table for t-zen-widlook.
def output param table for t-zen-fldlook.
def output param table for t-zen-colours.
def output param table for t-zen-f-hotkey.
def output param table for t-zen-dfkey.
def output param table for t-zen-fielddefault.
def output param table for t-zen-widgetproperty.
def output param table for t-zen-property.
def output param table for t-zen-mesfil.
def output param table for t-zen-auditdetail.

{{&core}bldtt.i &table = zen-auditdetail
                &where = where active}

def var pv-lanid as int no-undo.
def var pv-country as char no-undo.
find zen-duser where zen-duser.duser = pv-user
               no-lock.
assign pv-lanid = zen-duser.lan_lanid
       pv-country = string(zen-duser.country).
{{&core}bldtt.i &table = zen-duser
                &where = where zen-duser.duser = pv-user}

{{&core}bldtt.i &table = zen-lan_field
                &where = where zen-lan_field.lan_lanid = pv-lanid}

if not session:remote then do:
    {{&core}bldtt.i &table = zen-dpgm
                    &where = where zen-dpgm.type ne 's'}
end.
else do:
    {{&core}bldtt.i &table = zen-dpgm}
end.

{{&core}bldtt.i &table = zen-fielddefault
                &where = where Zen-FieldDefault.duser = pv-user}

{{&core}bldtt.i &table = zen-dwidget}
{{&core}bldtt.i &table = zen-control}
{{&core}bldtt.i &table = zen-dmenu}

empty temp-table t-zen-fldlook.
empty temp-table t-zen-widlook.
for each zen-widlook no-lock:
    create t-zen-widlook.
    buffer-copy zen-widlook to t-zen-widlook.

    if not can-find(t-zen-fldlook where t-zen-fldlook.lookupname = Zen-widlook.LookupName)
    then do:
        find zen-fldlook where zen-fldlook.lookupname = Zen-widlook.LookupName
                          no-lock no-error.
        if avail zen-fldlook then do:
            create t-zen-fldlook.
            buffer-copy zen-fldlook to t-zen-fldlook.
        end.
    end.

end.

for each zen-fldlook where not can-find(t-zen-fldlook where t-zen-fldlook.lookupname = zen-fldlook.lookupname)
                     no-lock:
    create t-zen-fldlook.
    buffer-copy zen-fldlook to t-zen-fldlook.
end.

{{&core}bldtt.i &table = zen-colours}
{{&core}bldtt.i &table = zen-f-hotkey
                &where = where zen-f-hotkey.duser matches pv-user}
{{&core}bldtt.i &table = zen-dfkey}
{{&core}bldtt.i &table = zen-widgetproperty}
{{&core}bldtt.i &table = zen-property}
{{&core}bldtt.i &table = zen-mesfil
                &where = where zen-mesfil.lan_lanid = pv-lanid}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

