&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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
 /* sl-maint.i */
/* procedures for selection list maint */

/* ***************************  Definitions  ************************** */

&if Defined(SLDelim) ne 2 &then
&glob SLDelim {&ComboDelim}
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cut-entries Include 
PROCEDURE cut-entries :
def input  param sl-hand as handle no-undo.
def output param pv-keys as char   no-undo.  
def output param pv-list as char   no-undo.

def var v-cnt     as int  no-undo.
def var v-dellist as char no-undo.

do v-cnt = 1 to sl-hand:num-items:
   if sl-hand:is-selected(v-cnt) then
       assign
           pv-keys   = pv-keys + entry(v-cnt,sl-hand:private-data,"{&SLDelim}") + ','
           pv-list   = pv-list + entry(v-cnt,sl-hand:list-items,"{&SLDelim}") + ','
           v-dellist = v-dellist + string(v-cnt) + ','.
end.

assign pv-keys   = substring(pv-keys,1,length(pv-keys) - 1)
      pv-list   = substring(pv-list,1,length(pv-list) - 1)
      v-dellist = substring(v-dellist,1,length(v-dellist) - 1).

run delete-entries (v-dellist,sl-hand).  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete-Entries Include 
PROCEDURE Delete-Entries :
def input param v-dellist as char   no-undo.
def input param sl-hand   as handle no-undo.

def var v-cnt  as int  no-undo.
def var lv-key as char no-undo. 

v-cnt = num-entries(v-dellist,"{&SLDelim}").

do v-cnt = num-entries(v-dellist,"{&SLDelim}") to 1 by -1:
   lv-key = entry(int(entry(v-cnt,v-dellist,"{&SLDelim}")),sl-hand:private-data,"{&SLDelim}") + "{&SLDelim}".
   
   IF NUM-ENTRIES(sl-hand:PRIVATE-DATA,"{&SLDelim}") = 1 
     THEN lv-key = SUBSTRING(lv-key,1,LENGTH(lv-key) - 1).

   sl-hand:private-data = replace(sl-hand:private-data,lv-key,'').           
   sl-hand:delete(int(entry(v-cnt,v-dellist,"{&SLDelim}"))).
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveEntries Include 
PROCEDURE MoveEntries :
DEF INPUT PARAM PV-SOURCE as handle no-undo.
def input param pv-dest as handle no-undo.

  def var lv-keys  as char no-undo.
  def var lv-items as char no-undo.
  
  run cut-entries (PV-SOURCE,output lv-keys,output lv-items).
  run paste-entries (pv-dest,lv-keys,lv-items).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paste-Entries Include 
PROCEDURE Paste-Entries :
def input param sl-dest as handle no-undo.
  def input param pv-keys as char   no-undo.
  def input param pv-list as char   no-undo.

  if sl-dest:list-items ne ? 
  then assign sl-dest:list-items   = sl-dest:list-items + "{&SLDelim}" + pv-list
              sl-dest:private-data = sl-dest:private-data + "{&SLDelim}" + pv-keys.
  else assign sl-dest:list-items   = pv-list
              sl-dest:private-data = pv-keys.
 
  sl-dest:screen-value = entry(1,sl-dest:list-items,"{&SLDelim}").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Remove-Entries Include 
PROCEDURE Remove-Entries :
def input param sl-hand   as handle no-undo.
  
  DEF VAR no-entries AS INT  NO-UNDO.
  DEF VAR v-cnt      AS INT  NO-UNDO.
  DEF VAR lv-scratch    AS CHAR NO-UNDO.

  ASSIGN
      no-entries =  sl-hand:NUM-ITEMS
      lv-scratch    = "".

  do v-cnt = 1 to no-entries:
      if sl-hand:is-selected(v-cnt) then
          sl-hand:delete(v-cnt).
      ELSE
          lv-scratch = IF lv-scratch = "" 
                     THEN  entry(v-cnt,sl-hand:private-data,"{&SLDelim}") 
                     ELSE  lv-scratch + "{&SLDelim}" +  entry(v-cnt,sl-hand:private-data,"{&SLDelim}").

  end.
  sl-hand:private-data = lv-scratch.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

