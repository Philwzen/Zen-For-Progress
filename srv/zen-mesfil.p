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




/* changeable bits here change table-name to appropriate DB table */
&glob table-name zen-mesfil
&glob unique-key {&table-name}tableid 

{{&core}def-table.i &table-name = {&table-name}}

define query q-{&table-name} for {&table-name} scrolling.

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
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 20.48
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

&IF DEFINED(EXCLUDE-clear-table) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-table Procedure 
procedure clear-table :
for each t-{&table-name}:
    delete t-{&table-name}.
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copy-messages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-messages Procedure 
procedure copy-messages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param vi-copy-opt      as int no-undo. /* 1,2,3
                                                          1 = Current msg (see vi-copy-msg)
                                                          2 = This message (see vi-copy-msg)
                                                          3 = All Messages */

def input param vi-copy-msg      as int no-undo. /* msg no to copy if applicable */
def input param vi-from-language as int no-undo. /* from country code */
def input param vi-to-language   as int no-undo. /* to country code */
def input param vi-copy-records  as int no-undo. /* copy record options 1,2
                                                          1 = Leave Existing
                                                          2 = Overwrite All */

def buffer b-mesfil for zen-mesfil.
define var vd-tableid as dec no-undo.

    /*MESSAGE vi-copy-opt vi-copy-msg vi-from-language vi-to-language vi-copy-records VIEW-AS ALERT-BOX.*/

if vi-copy-opt = 1 or 
   vi-copy-opt = 2 
then do:
    find first zen-mesfil where zen-mesfil.lan_lanid = vi-from-language 
                            and zen-mesfil.mesnum    = vi-copy-msg 
                          no-lock no-error.
    if avail zen-mesfil 
    then do:
       find first b-mesfil where b-mesfil.lan_lanid = vi-to-language 
                             and b-mesfil.mesnum    = vi-copy-msg 
                           exclusive-lock no-error.
       if avail b-mesfil 
       then do:
          if vi-copy-records = 2 
              then b-mesfil.mestxt = zen-mesfil.mestxt.
       end.
       else do:
          create b-mesfil.
          buffer-copy zen-mesfil except {&table-name}tableid lan_lanid mesnum to b-mesfil.
          assign b-mesfil.lan_lanid = vi-to-language
                 b-mesfil.mesnum    = vi-copy-msg.
          release b-mesfil.                 
       end.
    end. /* found current msg - should always find */
 end. /* copy selected */
 else do:
   for each zen-mesfil where zen-mesfil.lan_lanid = vi-from-language 
                       no-lock:
       find first b-mesfil where b-mesfil.lan_lanid = vi-to-language 
                             and b-mesfil.mesnum    = zen-mesfil.mesnum 
                           exclusive-lock no-error.
       if avail b-mesfil 
       then do:
          if vi-copy-records = 2 
          then b-mesfil.mestxt = zen-mesfil.mestxt.
       end.
       else do:
          create b-mesfil.
          buffer-copy zen-mesfil except {&table-name}tableid lan_lanid mesnum to b-mesfil.
          assign b-mesfil.lan_lanid = vi-to-language
                 b-mesfil.mesnum    = zen-mesfil.mesnum.
          release b-mesfil.
       end. /* avail buffer */
    end. /* for each */
 end. /* copy all */
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-record Procedure 
procedure delete-record :
   /* USES MANDATORY PARAMETERS
def input param pv-recid as recid no-undo.
def input-output param table for t-{&table-name}.
*/
   {{&core}apsrv-deleterecord.i}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Delete-Related-Tables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete-Related-Tables Procedure 
procedure Delete-Related-Tables :
/*------------------------------------------------------------------------------
  Purpose:     delete any child tables of this parent
  Parameters:  <none>
  Notes:       leave blank if not required    
------------------------------------------------------------------------------*/
/* EXAMPLE
   find prod-matrix of product exclusive-lock no-error.
   if avail prod-matrix 
      then delete prod-matrix.
   for each prod-curr of product exclusive-lock:
      delete prod-cur.
   end.
   */
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-validation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-validation Procedure 
procedure delete-validation :
/*------------------------------------------------------------------------------
  Purpose:     check ok to delete record
  Parameters:  <none>
  Notes:       return 'passed' if it's OK to delete.
------------------------------------------------------------------------------*/
/* example 
   if can-find(first ledger-item of product) then return 'failed'.
*/
    return 'passed'.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-find-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-record Procedure 
procedure find-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-code as int no-undo.
def var lv-langid as int no-undo.
lv-langid = UserLanguage(GetSysVar("user")).
   {{&core}apsrv-findrecord.i 
        &where = "where zen-mesfil.lan_lanid = lv-langid 
                    and zen-mesfil.mesnum    = pv-code"}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-records) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-records Procedure 
procedure get-records :
   /* USES MANDATORY PARAMETERS YOU NEED NOT WORRY ONLY 
def input param pv-direction as char no-undo.
def input  param pv-rowid     as recid no-undo.
def output param table        for  t-{&table-name}.

   can use &whereclause as where clause and &by for sorting
      e.g.  &where = "where {&table-name}.class = pv-class"       
        &by          = "by {&table-name}.name"                                                   
********************************************************/
def input param pv-lanid as int no-undo.

def input param pv-direction as char no-undo.
def input  param pv-id     like {&table-name}.{&unique-key} no-undo.
def output param table        for  t-{&table-name}.

if not systemmanager(GetSysVar("user")) 
then run normalget(pv-lanid,pv-direction,pv-id,output table t-{&table-name}).
else run sysmanget(pv-lanid,pv-direction,pv-id,output table t-{&table-name}).


end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getselected) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getselected Procedure 
procedure getselected :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def input  param pv-string as char no-undo.
    def input-output param table for t-zen-mesfil.

    for each zen-mesfil where zen-mesfil.mestxt contains(pv-string):
        create t-zen-mesfil.
        buffer-copy zen-mesfil to t-zen-mesfil.
    end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NormalGet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NormalGet Procedure 
procedure NormalGet :
def input param pv-lanid as int no-undo.

   {{&core}apsrv-getrecords.i &whereclause = "where zen-mesfil.lan_lanid = pv-lanid
                                      and zen-mesfil.sysrec = no"}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Procedure 
procedure open-query :
   /* USES MANDATORY PARAMETERS
      output table for t-{&table-name}.
      returns ALL records matching the &where clause
   can use &whereclause as where clause and &by for sorting
      e.g.  &where = "where {&table-name}.class = pv-class"       
        &by          = "by {&table-name}.name"   
********************************************************/

def input param pv-lanid as int no-undo.

{{&core}apsrv-openquery.i &where = "where zen-mesfil.lan_lanid = pv-lanid"}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create Procedure 
procedure post-create :
/*------------------------------------------------------------------------------
  Purpose:     just a hook incase we need it
  Parameters:  <none>
  Notes:   called after the buffer copy from temp-table to db.
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-update) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update Procedure 
procedure post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pre-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create Procedure 
procedure pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     if can-find(first {&table-name} 
        where {&table-name}.lan_lanid = t-{&table-name}.lan_lanid
          and {&table-name}.mesnum = t-{&table-name}.mesnum) then
    do:
        ErrorCreate(122,"Message",string(t-{&table-name}.mesnum),"","").
    end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-save-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE save-record Procedure 
procedure save-record :
   /* USES MANDATORY PARAMETERS
def input param pv-recid as recid no-undo. 
def input-output param table for t-{&table-name}.
*/
   {{&core}apsrv-saverecord.i}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SysManGet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SysManGet Procedure 
procedure SysManGet :
def input param pv-lanid as int no-undo.
   {{&core}apsrv-getrecords.i &whereclause = "where zen-mesfil.lan_lanid = pv-lanid"}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

